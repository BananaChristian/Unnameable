#include "irgen.hpp"

void IRGenerator::generateRecordStatement(Node *node) {
  auto recordStmt = dynamic_cast<RecordStatement *>(node);
  if (!recordStmt)
    return;

  auto it = semantics.metaData.find(recordStmt);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Missing record metaData");

  auto &meta = *it->second;
  std::string blockName = meta.type.resolvedName;

  // Get the struct (Should already be created by declareCustomTypes)
  llvm::StructType *structTy = llvmCustomTypes[blockName];
  if (!structTy) {
    // Fallback if not pre-declared
    structTy = llvm::StructType::create(context, blockName);
    llvmCustomTypes[blockName] = structTy;
  }

  if (structTy->isOpaque()) {
    std::vector<llvm::Type *> memberTypes;

    // Create a temporary vector to hold types in the correct order
    memberTypes.resize(meta.members.size());

    for (auto &pair : meta.members) {
      std::shared_ptr<MemberInfo> info = pair.second;
      llvm::Type *ty = info->isHeap ? getLLVMType(info->type)->getPointerTo()
                                    : getLLVMType(info->type);
      memberTypes[info->memberIndex] = ty;
    }
    structTy->setBody(memberTypes);
  }
  meta.llvmType = structTy;

  std::cout << "[IRGEN] Defined Type Body for: " << blockName << "\n";
}

llvm::Value *IRGenerator::generateInstanceExpression(Node *node) {
  auto instExpr = dynamic_cast<InstanceExpression *>(node);
  if (!instExpr)
    return nullptr;

  std::string instName = instExpr->blockIdent->expression.TokenLiteral;
  llvm::StructType *structTy = llvmCustomTypes[instName];
  if (!structTy)
    throw std::runtime_error("Unknown record type: " + instName);

  if (!funcBuilder.GetInsertBlock())
    throw std::runtime_error("Cannot create an instance for '" + instName +
                             "' in the global scope");

  llvm::Value *instancePtr =
      funcBuilder.CreateAlloca(structTy, nullptr, instName + "inst");

  auto typeInfoIt = semantics.customTypesTable.find(instName);
  if (typeInfoIt == semantics.customTypesTable.end())
    throw std::runtime_error("Missing custom type info for " + instName);
  auto &typeInfo = typeInfoIt->second;

  // We create a map of field names to their specific assignment nodes
  std::unordered_map<std::string, AssignmentStatement *> userInits;
  for (auto &fieldStmt : instExpr->fields) {
    if (auto assign = dynamic_cast<AssignmentStatement *>(fieldStmt.get())) {
      userInits[assign->identifier->expression.TokenLiteral] = assign;
    }
  }

  // We iterate through the definition of the struct to ensure total coverage
  for (auto const &[memberName, info] : typeInfo->members) {
    llvm::Value *memberPtr = funcBuilder.CreateStructGEP(
        structTy, instancePtr, info->memberIndex, memberName);
    llvm::Value *finalVal = nullptr;

    // Check if the user provided a value for this field
    auto it = userInits.find(memberName);
    if (it != userInits.end()) {
      // Generate the specific value provided (like 'null' or
      // '7')
      finalVal = generateExpression(it->second->value.get());
    } else {
      // Fallback to the declaration default or a zero/null value
      auto letStmt = dynamic_cast<LetStatement *>(info->node);
      if (letStmt && letStmt->value) {
        finalVal = generateExpression(letStmt->value.get());
      } else {
        if (info->isHeap) {
          finalVal =
              llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(
                  getLLVMType(info->type)->getPointerTo()));
        } else {
          finalVal = llvm::Constant::getNullValue(getLLVMType(info->type));
        }
      }
    }

    if (!finalVal)
      throw std::runtime_error("Failed to resolve value for field: " +
                               memberName);

    // Perform exactly one store per field
    funcBuilder.CreateStore(finalVal, memberPtr);
  }

  // Return the loaded struct value
  return funcBuilder.CreateLoad(structTy, instancePtr, instName + "_val");
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
    throw std::runtime_error("Component type not registered: " + componentName);

  llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(ctIt->second);
  if (!structTy)
    throw std::runtime_error("LLVM type is not a struct for: " + componentName);

  // FUNCTION SETUP
  std::vector<llvm::Type *> llvmParamTypes = {structTy->getPointerTo()}; // self
  for (auto &arg : initStmt->constructor_args)
    llvmParamTypes.push_back(
        getLLVMType(semantics.inferNodeDataType(arg.get())));

  auto *funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(context),
                                           llvmParamTypes, false);
  auto *initFunc =
      llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                             componentName + "_init", module.get());
  currentFunction = initFunc;

  std::cout << "[IR DEBUG] Starting generation for: "
            << initFunc->getName().str() << "\n";

  auto *entryBlock = llvm::BasicBlock::Create(context, "entry", initFunc);
  funcBuilder.SetInsertPoint(entryBlock);

  auto argIt = initFunc->arg_begin();
  llvm::Argument *selfArg = &*argIt++;
  selfArg->setName(componentName + ".self_arg");

  llvm::Type *selfPtrType = structTy->getPointerTo();
  llvm::AllocaInst *selfAlloca = funcBuilder.CreateAlloca(
      selfPtrType, nullptr, componentName + ".self_ptr_addr");

  // SAFETY CHECK: Ensure Alloca is at the beginning
  if (selfAlloca->getParent() != entryBlock ||
      selfAlloca->getNextNonDebugInstruction() != nullptr)
    std::cerr << "[IR WARN] Self Alloca not at start of entry block! This can "
                 "lead to bugs.\n"; // GUARD

  funcBuilder.CreateStore(selfArg, selfAlloca);

  std::cout << "[IR DEBUG] Self Demotion: Arg: " << selfArg
            << " Stored to Alloca: " << selfAlloca << "\n"; // DEBUG

  // METADATA REGISTRATION
  auto compIt = semantics.metaData.find(component);
  if (compIt == semantics.metaData.end())
    throw std::runtime_error("Missing component metaData for: " +
                             componentName);

  // Store the ALLOCA address as the canonical self pointer for this function
  llvm::Value *prevInstance = compIt->second->llvmValue;
  compIt->second->llvmValue =
      selfAlloca; // Use selfAlloca (the ptr* to the self ptr)

  currentComponent = component;
  currentComponentInstance = selfAlloca; // Use selfAlloca
  currentFunctionSelfMap[currentFunction] = selfAlloca;

  // CONSTRUCTOR ARGUMENTS (Store to Alloca)
  for (auto &arg : initStmt->constructor_args) {
    llvm::Value *argVal = &*argIt++;
    auto argName = arg->statement.TokenLiteral;

    // Ensure argVal is not a temporary value being reused in a dangerous way
    if (!llvm::isa<llvm::Argument>(argVal))
      std::cerr << "[IR WARN] Constructor arg '" << argName
                << "' is not a clean Argument!.\n"; // GUARD

    llvm::AllocaInst *alloca =
        funcBuilder.CreateAlloca(argVal->getType(), nullptr, argName);
    funcBuilder.CreateStore(argVal, alloca);

    auto metaIt = semantics.metaData.find(arg.get());
    if (metaIt == semantics.metaData.end())
      throw std::runtime_error("Missing metaData for ctor argument: " +
                               argName);

    metaIt->second->llvmValue = alloca;

    std::cout << "[IR DEBUG] Stored Ctor Arg: " << argName
              << " to Alloca: " << alloca << "\n"; // DEBUG
  }

  // Body Gen
  if (initStmt->block)
    generateBlockStatement(initStmt->block.get());

  llvm::BasicBlock *currentBlock = funcBuilder.GetInsertBlock();

  if (!currentBlock->getTerminator()) {
    llvm::IRBuilder<> terminatorBuilder(currentBlock);
    terminatorBuilder.CreateRetVoid();
  }

  // Cleanup
  compIt->second->llvmValue = prevInstance;
  currentComponentInstance = nullptr;
  currentComponent = nullptr;
  isGlobalScope = true;
  currentFunction = nullptr;
  if (oldInsertPoint)
    funcBuilder.SetInsertPoint(oldInsertPoint);

  std::cout << "[IR INIT] Finished generating " << componentName << "_init\n";
}

void IRGenerator::generateComponentFunctionStatement(
    Node *node, const std::string &compName) {
  auto *fnExpr = dynamic_cast<FunctionExpression *>(node);
  if (!fnExpr)
    return;

  llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();
  std::string funcName;

  if (auto *expr = dynamic_cast<FunctionExpression *>(fnExpr)) {
    auto exprIt = semantics.metaData.find(expr);
    if (exprIt == semantics.metaData.end())
      throw std::runtime_error("Missing metadata for component function");
    if (exprIt->second->hasError)
      throw std::runtime_error("Error in function: " +
                               expr->func_key.TokenLiteral);

    isGlobalScope = false;
    auto funcSym = exprIt->second;
    funcName = compName + "_" + expr->func_key.TokenLiteral;

    llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
    if (funcSym->isExportable)
      linkage = llvm::Function::ExternalLinkage;

    // Collect parameter types (first is %this)
    llvm::Type *thisPtrType = llvmCustomTypes[compName]->getPointerTo();
    std::vector<llvm::Type *> paramTypes = {thisPtrType};

    for (auto &p : expr->call) {
      auto it = semantics.metaData.find(p.get());
      if (it == semantics.metaData.end())
        throw std::runtime_error("Missing parameter metadata for: " +
                                 p->statement.TokenLiteral);
      paramTypes.push_back(getLLVMType(it->second->type));
    }

    // Return type
    ResolvedType retType = semantics.inferNodeDataType(expr->return_type.get());
    llvm::FunctionType *fnType =
        llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

    // Create or fetch function
    llvm::Function *fn = module->getFunction(funcName);
    if (!fn)
      fn = llvm::Function::Create(fnType, linkage, funcName, module.get());

    currentFunction = fn;
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
    funcBuilder.SetInsertPoint(entry);

    // Map %this (first argument)
    llvm::Argument &thisArg = *fn->arg_begin();
    thisArg.setName(compName + ".self");
    llvm::AllocaInst *selfAlloca =
        funcBuilder.CreateAlloca(thisPtrType, nullptr, compName + ".self_ptr");
    funcBuilder.CreateStore(&thisArg, selfAlloca);
    exprIt->second->llvmValue = selfAlloca;
    currentFunctionSelfMap[currentFunction] = selfAlloca;

    // Map user parameters
    auto argIter = std::next(fn->arg_begin()); // skip %this
    for (auto &p : expr->call) {
      auto paramNode = p.get();
      auto pIt = semantics.metaData.find(paramNode);
      if (pIt == semantics.metaData.end())
        throw std::runtime_error("Missing parameter metaData");

      llvm::Type *paramTy = getLLVMType(pIt->second->type);
      llvm::AllocaInst *alloca =
          funcBuilder.CreateAlloca(paramTy, nullptr, p->statement.TokenLiteral);
      funcBuilder.CreateStore(&(*argIter), alloca);
      pIt->second->llvmValue = alloca;

      ++argIter;
    }

    // Generate function body
    generateExpression(expr->block.get());

    llvm::BasicBlock *finalBlock = funcBuilder.GetInsertBlock();

    // If the function is void
    bool isVoidFunction = exprIt->second->returnType.kind == DataType::VOID;

    if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator())) {
      if (isVoidFunction) {
        // Inject 'ret void' for void functions that fell off the end.
        funcBuilder.CreateRetVoid();
        std::cout << "INJECTED missing 'ret void' terminator.\n";
      } else {
        // Non-void function finished without a return.
        // This is a semantic failure, but we terminate for LLVM stability.
        funcBuilder.CreateUnreachable();
      }
    }
    isGlobalScope = true;
    funcBuilder.ClearInsertionPoint();

    currentFunction = nullptr;
  }

  // FunctionDeclarationExpression: declaration only error out incase somehow
  // the semantics allowed them through
  else if (auto *declExpr =
               dynamic_cast<FunctionDeclarationExpression *>(fnExpr)) {
    throw std::runtime_error(
        "Function declarations are prohibited inside components");
  }

  if (oldInsertPoint)
    funcBuilder.SetInsertPoint(oldInsertPoint);
}

llvm::Value *IRGenerator::generateMethodCallExpression(Node *node) {
  auto metCall = dynamic_cast<MethodCallExpression *>(node);
  if (!metCall)
    throw std::runtime_error("Invalid method call expression node");

  if (isGlobalScope)
    throw std::runtime_error("Cannot use calls in global scope");

  // Get the metaData for the whole call
  auto callIt = semantics.metaData.find(metCall);
  if (callIt == semantics.metaData.end()) {
    throw std::runtime_error("Missing method call metaData");
  }

  auto metSym = callIt->second;

  // Check if there are semantic errors
  if (metSym->hasError) {
    throw std::runtime_error(
        "Semantic error detected on component method call");
  }

  // Something like p1.test()
  // Split the call
  auto instance = dynamic_cast<Identifier *>(metCall->instance.get());
  std::string instanceName = instance->identifier.TokenLiteral;
  llvm::Value *result;
  auto sealIt = semantics.sealTable.find(instanceName);

  // Getting the raw function name as stored in the node
  auto call = dynamic_cast<CallExpression *>(metCall->call.get());
  std::string callName = call->function_identifier->expression.TokenLiteral;

  if (sealIt != semantics.sealTable.end()) {
    std::cout << "SEAL PATH\n";
    // Retrive the function we wish to call
    auto sealFnMap = sealIt->second;
    auto sealFnIt = sealFnMap.find(callName);
    if (sealFnIt == sealFnMap.end()) {
      throw std::runtime_error("Function '" + callName +
                               "' does not exist in seal '" + instanceName +
                               "'");
    }

    call->function_identifier->expression.TokenLiteral =
        instanceName + "_" + callName;
    call->function_identifier->token.TokenLiteral =
        instanceName + "_" + callName;

    result = generateCallExpression(call);
  } else {
    std::cout << "METHOD ACCESS PATH\n";
    // Getting the metaData for the instance
    auto instanceIt = semantics.metaData.find(instance);
    if (instanceIt == semantics.metaData.end()) {
      throw std::runtime_error("Could not find instance metaData");
    }

    auto instanceSym = instanceIt->second;

    llvm::Value *objectPtr = nullptr;
    // Get the llvm value for the instance
    if (instanceSym->llvmValue) {
      objectPtr = instanceSym->llvmValue;
    } else {
      objectPtr = generateExpression(instance);
    }

    // Get the component type of the object
    std::string compName = instanceSym->type.resolvedName;

    // Resolve the function name
    std::string funcName = compName + "_" + callName;

    // Fetch the function from the module
    llvm::Function *targetFunc = module->getFunction(funcName);
    if (!targetFunc) {
      throw std::runtime_error("Undefined method '" + callName +
                               "' from component '" + compName + "'");
    }

    // Prepare the arguments
    std::vector<llvm::Value *> args;
    args.push_back(objectPtr); // The implicit self

    // Add user arguments if any were provided
    for (const auto &arg : call->parameters) {
      args.push_back(generateExpression(arg.get()));
    }

    // Call the function
    result = funcBuilder.CreateCall(targetFunc, args);

    if (instanceSym->isHeap && instanceSym->refCount == 0 &&
        instanceSym->lastUseNode == metCall) {
      llvm::FunctionCallee sageFree = module->getOrInsertFunction(
          "sage_free", llvm::Type::getVoidTy(context),
          llvm::Type::getInt64Ty(context));

      llvm::Value *sizeVal = llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(context), instanceSym->componentSize);

      funcBuilder.CreateCall(sageFree, {sizeVal},
                             instance->identifier.TokenLiteral + "_sage_free");
    }
  }

  return result;
}

void IRGenerator::generateComponentStatement(Node *node) {
  auto *compStmt = dynamic_cast<ComponentStatement *>(node);
  if (!compStmt)
    return;

  std::cout << "Generating IR for component: "
            << compStmt->component_name->expression.TokenLiteral << "\n";

  auto it = semantics.metaData.find(compStmt);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Missing component metaData for " +
                             compStmt->component_name->expression.TokenLiteral);

  auto sym = it->second;

  if (sym->hasError) {
    throw std::runtime_error("Semantic error detected in component block");
  }

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
  sym->llvmType = structTy;

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
    if (!info->node)
      continue;

    if (auto *funcExpr = dynamic_cast<FunctionExpression *>(info->node)) {
      functionExpressions.push_back(funcExpr);
      continue;
    }

    // Handle data member
    llvm::Type *memberTy = getLLVMType(info->type);
    if (!memberTy)
      throw std::runtime_error("Unknown LLVM type for member '" + memberName +
                               "'");

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

  // Getting the enum statement name
  const std::string &enumName =
      enumStmt->enum_identifier->expression.TokenLiteral;

  // Retrieve the info from metaData
  auto enumIt = semantics.metaData.find(enumStmt);
  if (enumIt == semantics.metaData.end())
    throw std::runtime_error("No existing metaData for enum class statement");

  // Getting the enum symbolInfo
  auto enumInfo = enumIt->second;
  auto enumTypeInfo = semantics.customTypesTable[enumInfo->type.resolvedName];

  // Creating a struct for book keeping
  llvm::StructType *enumStruct =
      llvm::StructType::create(context, enumInfo->type.resolvedName);
  llvmCustomTypes[enumInfo->type.resolvedName] = enumStruct;

  std::cout << "[IRGEN LOG] Declared enum class " << enumInfo->type.resolvedName
            << " with members:\n";
  for (auto &[memberName, member] : enumTypeInfo->members) {
    // llvm::ConstantInt will be generated  when a member is actually used in an
    // expression
  }
}

void IRGenerator::generateInstantiateStatement(Node *node) {
  auto instStmt = dynamic_cast<InstantiateStatement *>(node);

  if (!instStmt)
    throw std::runtime_error("Invalid instantiation statement");

  auto it = semantics.metaData.find(instStmt);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Failed to find instantiation metaData");
  }

  auto sym = it->second;
  if (sym->hasError)
    throw std::runtime_error("Semantic error detected");

  const auto &instTable = sym->instTable;

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

  auto it = semantics.metaData.find(sealStmt);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Failed to find seal metaData");
  }

  auto sealSym = it->second;
  if (sealSym->hasError)
    throw std::runtime_error("Semantic error detected");

  auto block = dynamic_cast<BlockStatement *>(sealStmt->block.get());
  // Call the generator on the functions themselves
  for (const auto &stmt : block->statements) {
    std::cout << "Looping through seal stmts :" + stmt->toString() << "\n";
    generateStatement(stmt.get());
  }
}

  void IRGenerator::generateQualifyStatement(Node * node) {
    auto qualifyStmt = dynamic_cast<QualifyStatement *>(node);

    if (!qualifyStmt)
      return;

    mainMarker = true;
  }
