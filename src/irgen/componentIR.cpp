#include "irgen.hpp"
#include <llvm/IR/InlineAsm.h>

void IRGenerator::generateRecordStatement(Node *node) {
  auto recordStmt = dynamic_cast<RecordStatement *>(node);
  if (!recordStmt)
    return;

  auto sym = semantics.getSymbolFromMeta(recordStmt);
  if (!sym)
    reportDevBug("Miising record symbol info", recordStmt->recordName.get());

  std::string blockName = recordStmt->recordName->expression.TokenLiteral;

  // Get the struct
  llvm::StructType *structTy = llvmCustomTypes[blockName];
  if (!structTy) {
    // Fallback if not pre-declared
    structTy = llvm::StructType::create(context, blockName);
    llvmCustomTypes[blockName] = structTy;
  }
  recordTypes[blockName] = structTy;

  if (structTy->isOpaque()) {
    std::vector<llvm::Type *> memberTypes;

    // Create a temporary vector to hold types in the correct order
    memberTypes.resize(sym->members.size());

    for (auto &pair : sym->members) {
      std::shared_ptr<MemberInfo> info = pair.second;
      llvm::Type *ty = getLLVMType(info->type);
      memberTypes[info->memberIndex] = ty;
    }
    structTy->setBody(memberTypes);
  }
  sym->codegen().llvmType = structTy;

  logInternal("Defined Type Body for: " + blockName);
}

llvm::Value *IRGenerator::generateInstanceExpression(Node *node) {
  auto instExpr = dynamic_cast<InstanceExpression *>(node);
  if (!instExpr)
    return nullptr;

  std::string instName = instExpr->blockIdent->expression.TokenLiteral;

  llvm::StructType *structTy = llvmCustomTypes[instName];
  if (!structTy)
    reportDevBug("Unknown record type: " + instName,
                 instExpr->blockIdent.get());

  auto typeInfoIt = semantics.customTypesTable.find(instName);
  if (typeInfoIt == semantics.customTypesTable.end())
    reportDevBug("Missing custom type info for '" + instName + "'",
                 instExpr->blockIdent.get());

  auto &typeInfo = typeInfoIt->second;

  // Map user provided field assignments by name
  std::unordered_map<std::string, AssignmentStatement *> userInits;
  for (auto &fieldStmt : instExpr->fields) {
    if (auto assign = dynamic_cast<AssignmentStatement *>(fieldStmt.get()))
      userInits[assign->identifier->expression.TokenLiteral] = assign;
  }

  // Sort the members
  std::vector<std::pair<std::string, std::shared_ptr<MemberInfo>>>
      orderedMembers;
  for (const auto &[name, info] : typeInfo->members) {
    orderedMembers.push_back({name, info});
  }

  std::sort(orderedMembers.begin(), orderedMembers.end(),
            [](const auto &a, const auto &b) {
              return a.second->memberIndex < b.second->memberIndex;
            });

  if (isGlobalScope) {
    std::vector<llvm::Constant *> constantFields;

    for (const auto &[memberName, info] : orderedMembers) {
      llvm::Constant *constField = nullptr;

      auto it = userInits.find(memberName);
      if (it != userInits.end()) {
        llvm::Value *val = generateExpression(it->second->value.get());
        constField = llvm::dyn_cast<llvm::Constant>(val);
        if (!constField) {
          reportDevBug("Field '" + memberName + "' in global record '" +
                           instName + "' must be a compile-time constant",
                       instExpr->blockIdent.get());
        }
      } else {
        auto varDecl = dynamic_cast<VariableDeclaration *>(info->node);
        if (varDecl && varDecl->initializer) {
          llvm::Value *defaultVal =
              generateExpression(varDecl->initializer.get());
          constField = llvm::dyn_cast<llvm::Constant>(defaultVal);
          if (!constField) {
            reportDevBug("Default value for field '" + memberName +
                             "' is not constant",
                         info->node);
          }
        } else {
          constField = llvm::Constant::getNullValue(getLLVMType(info->type));
        }
      }
      constantFields.push_back(constField);
    }

    return llvm::ConstantStruct::get(structTy, constantFields);
  }

  if (!funcBuilder.GetInsertBlock())
    reportDevBug("Cannot create instance for '" + instName +
                     "' outside function scope",
                 instExpr->blockIdent.get());

  llvm::Value *instancePtr =
      funcBuilder.CreateAlloca(structTy, nullptr, instName + "_inst");

  // Store each field
  for (auto const &[memberName, info] : orderedMembers) {
    llvm::Value *memberPtr = funcBuilder.CreateStructGEP(
        structTy, instancePtr, info->memberIndex, memberName);

    llvm::Value *finalVal = nullptr;

    auto it = userInits.find(memberName);
    if (it != userInits.end()) {
      finalVal = generateExpression(it->second->value.get());
    } else {
      auto varDecl = dynamic_cast<VariableDeclaration *>(info->node);
      if (varDecl && varDecl->initializer) {
        finalVal = generateExpression(varDecl->initializer.get());
      } else {
        finalVal = llvm::Constant::getNullValue(getLLVMType(info->type));
      }
    }

    if (!finalVal)
      reportDevBug("Failed to resolve value for field '" + memberName + "'",
                   info->node);

    auto *storeInst = funcBuilder.CreateStore(finalVal, memberPtr);

    auto sym = semantics.getSymbolFromMeta(instExpr);
    bool isInstanceVolatile = sym && sym->storage().isVolatile;
    if (isInstanceVolatile || info->isVolatile)
      storeInst->setVolatile(true);
  }

  // Return the loaded struct value
  auto *finalLoad =
      funcBuilder.CreateLoad(structTy, instancePtr, instName + "_val");

  auto sym = semantics.getSymbolFromMeta(instExpr);
  bool isInstanceVolatile = sym && sym->storage().isVolatile;
  if (isInstanceVolatile)
    finalLoad->setVolatile(true);

  return finalLoad;
}

void IRGenerator::generateInitFunction(Node *node,
                                       ComponentStatement *component) {
  auto *initStmt = dynamic_cast<InitStatement *>(node);
  if (!initStmt || !component)
    return;
  isGlobalScope = false;
  llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();
  const std::string componentName =
      component->component_name->expression.TokenLiteral;

  auto ctIt = componentTypes.find(componentName);
  if (ctIt == componentTypes.end())
    reportDevBug("Component type not registered: " + componentName, initStmt);
  llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(ctIt->second);
  if (!structTy)
    reportDevBug("LLVM type is not a struct for: " + componentName, initStmt);

  // Build parameter types with coercion (first is %this)
  llvm::Type *thisPtrType = structTy->getPointerTo();

  std::vector<llvm::Type *> originalParamTypes = {thisPtrType};
  std::vector<CoercionInfo> paramCoercion;
  std::vector<llvm::Type *> coercedParamTypes = {thisPtrType};

  // this coercion placeholder
  CoercionInfo thisCoercion;
  thisCoercion.isMemory = false;
  thisCoercion.coercedType = thisPtrType;
  paramCoercion.push_back(thisCoercion);

  for (auto &arg : initStmt->constructor_args) {
    auto argSym = semantics.getSymbolFromMeta(arg.get());
    if (!argSym)
      reportDevBug("Missing ctor argument symbol info", arg.get());

    llvm::Type *originalTy = getLLVMType(argSym->type().type);
    originalParamTypes.push_back(originalTy);

    llvm::Type *coercedTy = originalTy;
    CoercionInfo info;
    info.isMemory = false;
    info.coercedType = originalTy;

    if (auto *st = llvm::dyn_cast<llvm::StructType>(originalTy)) {
      info = classifyStruct(st);
      if (info.isMemory) {
        coercedTy = st->getPointerTo();
        info.coercedType = coercedTy;
      } else if (info.coercedType) {
        coercedTy = info.coercedType;
      }
    }

    paramCoercion.push_back(info);
    coercedParamTypes.push_back(coercedTy);
  }

  // Init always returns void — no return coercion needed
  CoercionInfo returnCoercion;
  returnCoercion.isMemory = false;
  returnCoercion.coercedType = llvm::Type::getVoidTy(context);

  auto *funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(context),
                                           coercedParamTypes, false);

  auto *initFunc =
      llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                             componentName + "_init", module.get());

  // Store coercion info
  FunctionCoercion coercion;
  coercion.originalParamTypes = originalParamTypes;
  coercion.coercedParamTypes = coercedParamTypes;
  coercion.paramCoercion = paramCoercion;
  coercion.returnCoercion = returnCoercion;
  coercion.hasSRet = false;
  functionCoercionMap[initFunc] = coercion;

  currentFunction = initFunc;
  auto *entryBlock = llvm::BasicBlock::Create(context, "entry", initFunc);
  funcBuilder.SetInsertPoint(entryBlock);

  // Map %this
  auto argIt = initFunc->arg_begin();
  llvm::Argument *selfArg = &*argIt++;
  selfArg->setName(componentName + ".self_arg");

  llvm::AllocaInst *selfAlloca = funcBuilder.CreateAlloca(
      thisPtrType, nullptr, componentName + ".self_ptr_addr");
  funcBuilder.CreateStore(selfArg, selfAlloca);

  auto sym = semantics.getSymbolFromMeta(component);
  if (!sym)
    reportDevBug("Failed to get component symbol info", component);

  llvm::Value *prevInstance = sym->codegen().llvmValue;
  sym->codegen().llvmValue = selfAlloca;
  currentComponent = component;
  currentComponentInstance = selfAlloca;
  currentFunctionSelfMap[currentFunction] = selfAlloca;

  // Map constructor args with coercion
  size_t paramIdx = 1; // skip this
  for (auto &arg : initStmt->constructor_args) {
    auto argSym = semantics.getSymbolFromMeta(arg.get());
    if (!argSym)
      reportDevBug("Missing ctor argument symbol info", arg.get());

    if (!llvm::isa<llvm::Argument>(&*argIt))
      std::cerr << "[IR WARN] Constructor arg '" << arg->statement.TokenLiteral
                << "' is not a clean Argument!\n";

    llvm::Type *originalTy = originalParamTypes[paramIdx];
    llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(
        originalTy, nullptr, arg->statement.TokenLiteral);

    llvm::Value *paramValue = &(*argIt);

    if (paramCoercion[paramIdx].coercedType &&
        paramCoercion[paramIdx].coercedType != originalTy) {
      paramValue =
          funcBuilder.CreateBitCast(paramValue, originalTy->getPointerTo());
      paramValue = funcBuilder.CreateLoad(originalTy, paramValue);
    }

    funcBuilder.CreateStore(paramValue, alloca);
    argSym->codegen().llvmValue = alloca;

    ++argIt;
    ++paramIdx;
  }

  // Body gen
  if (initStmt->block)
    generateBlockStatement(initStmt->block.get());

  llvm::BasicBlock *currentBlock = funcBuilder.GetInsertBlock();
  if (!currentBlock->getTerminator())
    funcBuilder.CreateRetVoid();

  // Cleanup
  sym->codegen().llvmValue = prevInstance;
  currentComponentInstance = nullptr;
  currentComponent = nullptr;
  isGlobalScope = true;
  currentFunction = nullptr;

  if (oldInsertPoint)
    funcBuilder.SetInsertPoint(oldInsertPoint);
  else
    funcBuilder.ClearInsertionPoint();

  logInternal("Finished generating '" + componentName + "_init'");
}

void IRGenerator::generateComponentFunctionStatement(
    Node *node, const std::string &compName) {
  auto *fnExpr = dynamic_cast<FunctionExpression *>(node);
  if (!fnExpr)
    return;

  if (dynamic_cast<FunctionDeclarationExpression *>(fnExpr))
    reportDevBug("Function declarations are prohibited inside components",
                 fnExpr);

  llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();
  isGlobalScope = false;

  auto funcSym = semantics.getSymbolFromMeta(fnExpr);
  if (!funcSym)
    reportDevBug("Failed to find function symbol info", fnExpr);

  std::string funcName = compName + "_" + fnExpr->func_key.TokenLiteral;

  // Build parameter types with coercion (first param is %this)
  llvm::Type *thisPtrType = llvmCustomTypes[compName]->getPointerTo();

  std::vector<llvm::Type *> originalParamTypes;
  std::vector<CoercionInfo> paramCoercion;
  std::vector<llvm::Type *> coercedParamTypes = {thisPtrType};

  // Add sret coercion placeholder for this
  CoercionInfo thisCoercion;
  thisCoercion.isMemory = false;
  thisCoercion.coercedType = thisPtrType;
  paramCoercion.push_back(thisCoercion);
  originalParamTypes.push_back(thisPtrType);

  for (auto &p : fnExpr->call) {
    auto paramSym = semantics.getSymbolFromMeta(p.get());
    if (!paramSym)
      reportDevBug("Missing parameter symbol info", p.get());

    llvm::Type *originalTy = getLLVMType(paramSym->type().type);
    originalParamTypes.push_back(originalTy);

    llvm::Type *coercedTy = originalTy;
    CoercionInfo info;
    info.isMemory = false;
    info.coercedType = originalTy;

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalTy)) {
      info = classifyStruct(structTy);
      if (info.isMemory) {
        coercedTy = structTy->getPointerTo();
        info.coercedType = coercedTy;
      } else if (info.coercedType) {
        coercedTy = info.coercedType;
      }
    }

    paramCoercion.push_back(info);
    coercedParamTypes.push_back(coercedTy);
  }

  // Return type with coercion
  auto retType = semantics.inferNodeDataType(fnExpr->return_type.get());
  llvm::Type *originalRetTy = getLLVMType(retType);

  CoercionInfo returnCoercion;
  returnCoercion.isMemory = false;
  returnCoercion.coercedType = originalRetTy;
  llvm::Type *coercedRetTy = originalRetTy;
  bool hasSRet = false;

  if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalRetTy)) {
    returnCoercion = classifyStruct(structTy);
    if (returnCoercion.isMemory) {
      coercedRetTy = llvm::Type::getVoidTy(context);
      hasSRet = true;
      coercedParamTypes.insert(coercedParamTypes.begin(),
                               structTy->getPointerTo());
      CoercionInfo sretInfo;
      sretInfo.isMemory = true;
      sretInfo.coercedType = structTy->getPointerTo();
      paramCoercion.insert(paramCoercion.begin(), sretInfo);
    } else if (returnCoercion.coercedType) {
      coercedRetTy = returnCoercion.coercedType;
    }
  }

  llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
  if (funcSym->isExportable)
    linkage = llvm::Function::ExternalLinkage;

  llvm::FunctionType *fnType =
      llvm::FunctionType::get(coercedRetTy, coercedParamTypes, false);

  llvm::Function *fn = module->getFunction(funcName);
  if (!fn) {
    fn = llvm::Function::Create(fnType, linkage, funcName, module.get());
  } else if (fn->getFunctionType() != fnType) {
    reportDevBug(
        "Component function redefinition with different signature for '" +
            funcName + "'",
        fnExpr);
  }

  // Store coercion info
  FunctionCoercion coercion;
  coercion.originalParamTypes = originalParamTypes;
  coercion.coercedParamTypes = coercedParamTypes;
  coercion.paramCoercion = paramCoercion;
  coercion.returnCoercion = returnCoercion;
  coercion.hasSRet = hasSRet;
  functionCoercionMap[fn] = coercion;

  currentFunction = fn;
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
  funcBuilder.SetInsertPoint(entry);

  // Map %this
  auto argIter = fn->arg_begin();
  llvm::Argument &thisArg = *argIter;
  thisArg.setName(compName + ".self");
  llvm::AllocaInst *selfAlloca =
      funcBuilder.CreateAlloca(thisPtrType, nullptr, compName + ".self_ptr");
  funcBuilder.CreateStore(&thisArg, selfAlloca);
  funcSym->codegen().llvmValue = selfAlloca;
  currentFunctionSelfMap[currentFunction] = selfAlloca;
  ++argIter;

  // Map user parameters
  size_t paramIdx = 1; // skip this
  for (auto &p : fnExpr->call) {
    auto paramSym = semantics.getSymbolFromMeta(p.get());
    if (!paramSym)
      reportDevBug("Failed to find param symbol info", p.get());

    llvm::Type *originalTy = originalParamTypes[paramIdx];
    llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(
        originalTy, nullptr, p->statement.TokenLiteral);

    llvm::Value *paramValue = &(*argIter);

    if (paramCoercion[paramIdx].coercedType &&
        paramCoercion[paramIdx].coercedType != originalTy) {
      paramValue =
          funcBuilder.CreateBitCast(paramValue, originalTy->getPointerTo());
      paramValue = funcBuilder.CreateLoad(originalTy, paramValue);
    }

    funcBuilder.CreateStore(paramValue, alloca);
    paramSym->codegen().llvmValue = alloca;

    ++argIter;
    ++paramIdx;
  }

  // Generate body
  generateExpression(fnExpr->block.get());

  llvm::BasicBlock *finalBlock = funcBuilder.GetInsertBlock();
  bool isVoidFunction = funcSym->func().returnType.kind == DataType::VOID;

  if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator())) {
    if (isVoidFunction) {
      funcBuilder.CreateRetVoid();
    } else {
      funcBuilder.CreateUnreachable();
    }
  }

  isGlobalScope = true;
  currentFunction = nullptr;

  if (oldInsertPoint)
    funcBuilder.SetInsertPoint(oldInsertPoint);
  else
    funcBuilder.ClearInsertionPoint();
}

void IRGenerator::generateComponentStatement(Node *node) {
  auto *compStmt = dynamic_cast<ComponentStatement *>(node);
  if (!compStmt)
    return;

  auto sym = semantics.getSymbolFromMeta(compStmt);
  if (!sym)
    reportDevBug("Failed to find component symbol info", compStmt);

  const std::string compName =
      compStmt->component_name->expression.TokenLiteral;
  currentComponent = compStmt;

  llvm::StructType *structTy = llvmCustomTypes[compName];
  if (!structTy) {
    // Fallback for safety, though declareCustomTypes should have handled this
    structTy = llvm::StructType::create(context, compName);
    llvmCustomTypes[compName] = structTy;
  }

  componentTypes[compName] = structTy;
  sym->codegen().llvmType = structTy;

  std::vector<llvm::Type *> memberTypes;
  std::vector<FunctionExpression *> functionExpressions;
  std::unordered_map<std::string, unsigned> memberIndexMap;

  size_t dataMemberCount = 0;
  for (auto const &[name, info] : sym->members) {
    if (!info->isFunction) {
      dataMemberCount++;
    }
  }
  memberTypes.resize(dataMemberCount);

  for (const auto &[memberName, info] : sym->members) {
    logInternal("Member '" + memberName +
                "' isFunction: " + std::to_string(info->isFunction) +
                " index: " + std::to_string(info->memberIndex));
    if (!info->node)
      continue;

    if (auto *funcExpr = dynamic_cast<FunctionExpression *>(info->node)) {
      functionExpressions.push_back(funcExpr);
      continue;
    }

    // Handle data member
    llvm::Type *memberTy = getLLVMType(info->type);
    if (!memberTy)
      reportDevBug("Unknown LLVM type for member '" + memberName + "'",
                   info->node);

    // Use the semantic index to place it correctly in the struct
    memberTypes[info->memberIndex] = memberTy;
  }

  if (structTy->isOpaque()) {
    structTy->setBody(memberTypes);
    std::cout << "[IRGEN] Finalized body for component: " << compName << "\n";
  }

  for (const auto &func : functionExpressions) {
    // Generate functions now since the body is done being set
    generateComponentFunctionStatement(func, compName);
  }
  // INIT HANDLING
  if (compStmt->initConstructor)
    generateInitFunction(compStmt->initConstructor.value().get(), compStmt);

  currentComponent = nullptr;
}

void IRGenerator::generateEnumStatement(Node *node) {
  auto enumStmt = dynamic_cast<EnumStatement *>(node);
  if (!enumStmt)
    return;

  // Getting the enum symbolInfo
  auto enumInfo = semantics.getSymbolFromMeta(enumStmt);
  if (!enumInfo)
    reportDevBug("Fail to find enum symbol info", enumStmt);

  auto enumTypeInfo =
      semantics.customTypesTable[enumInfo->type().type.resolvedName];

  // Creating a struct for book keeping
  llvm::StructType *enumStruct =
      llvm::StructType::create(context, enumInfo->type().type.resolvedName);
  llvmCustomTypes[enumInfo->type().type.resolvedName] = enumStruct;
}

void IRGenerator::generateInstantiateStatement(Node *node) {
  auto instStmt = dynamic_cast<InstantiateStatement *>(node);

  if (!instStmt)
    reportDevBug("Invalid instantiation statement", node);

  auto sym = semantics.getSymbolFromMeta(instStmt);
  if (!sym)
    reportDevBug("Failed to find instantion symbol info", instStmt);

  const auto &instTable = sym->generic().instTable;

  if (instTable.has_value()) {
    auto block =
        dynamic_cast<BlockStatement *>(instTable->instantiatedAST.get());
    for (const auto &stmt : block->statements) {
      generateStatement(stmt.get());
    }
  }
}

void IRGenerator::generateSealStatement(Node *node) {
  auto sealStmt = dynamic_cast<SealStatement *>(node);
  if (!sealStmt)
    return;

  auto sealSym = semantics.getSymbolFromMeta(sealStmt);
  if (!sealSym)
    reportDevBug("Failed to find seal symbol info", sealStmt);

  auto block = dynamic_cast<BlockStatement *>(sealStmt->block.get());
  // Call the generator on the functions themselves
  for (const auto &stmt : block->statements) {
    std::cout << "Looping through seal stmts :" + stmt->toString() << "\n";
    generateStatement(stmt.get());
  }
}

void IRGenerator::generateASMStatement(Node *node) {
  auto asmStmt = dynamic_cast<ASMStatement *>(node);
  if (!asmStmt)
    return;

  std::string asmStr = asmStmt->toASMString();
  std::string constraintStr = "";
  std::vector<llvm::Value *> args;
  std::vector<llvm::Type *> argTypes;

  bool wasInhibited = inhibitCleanUp;
  inhibitCleanUp = true;
  std::vector<Expression *> tofree;

  for (auto &instr : asmStmt->instructions) {
    auto *asmInstr = dynamic_cast<ASMInstruction *>(instr.get());
    if (!asmInstr)
      continue;

    for (auto &constraint : asmInstr->constraints) {
      if (!constraintStr.empty())
        constraintStr += ",";

      if (constraint->direction == "out") {
        constraintStr += "=*m";
        llvm::Value *ptr = generateAddress(constraint->variable.get());
        args.push_back(ptr);
        argTypes.push_back(ptr->getType());
      } else {
        constraintStr += constraint->constraint;
        llvm::Value *val = generateExpression(constraint->variable.get());
        args.push_back(val);
        argTypes.push_back(val->getType());
      }
      tofree.push_back(constraint->variable.get());
    }
  }

  // Build function type based on actual arguments
  llvm::FunctionType *asmFuncType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), argTypes, false);

  auto dialect = llvm::InlineAsm::AD_Intel;
  if (asmStmt->dialect == "att")
    dialect = llvm::InlineAsm::AD_ATT;

  llvm::InlineAsm *inlineAsm = llvm::InlineAsm::get(
      asmFuncType, asmStr, constraintStr, asmStmt->isVolatile, false, dialect);
  inhibitCleanUp = wasInhibited;

  funcBuilder.CreateCall(asmFuncType, inlineAsm, args);
  for (const auto &node : tofree)
    emitCleanup(node);
}
