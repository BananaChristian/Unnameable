#include "irgen.hpp"
#include <llvm/IR/InlineAsm.h>

void IRGenerator::generateRecordStatement(Node *node) {
  auto recordStmt = dynamic_cast<RecordStatement *>(node);
  if (!recordStmt)
    return;

  auto sym = semantics.getSymbolFromMeta(recordStmt);
  if (!sym)
    reportDevBug("Mising record symbol info", recordStmt->recordName.get());

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
      llvm::Type *ty;
      auto memberSym = info->symbolInfo;
      if (memberSym->storage().isHeap) {
        llvm::Type *baseType = getLLVMType(memberSym->type().type);
        ty = baseType->getPointerTo();
      } else {
        ty = getLLVMType(memberSym->type().type);
      }

      memberTypes[memberSym->type().memberIndex] = ty;
    }
    structTy->setBody(memberTypes, sym->storage().isPacked);
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

  auto typeInfoIt = semantics.payload.customTypesTable.find(instName);
  if (typeInfoIt == semantics.payload.customTypesTable.end())
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
              return a.second->symbolInfo->type().memberIndex <
                     b.second->symbolInfo->type().memberIndex;
            });

  if (isGlobalScope) {
    std::vector<llvm::Constant *> constantFields;

    for (const auto &[memberName, info] : orderedMembers) {
      llvm::Constant *constField = nullptr;
      auto memberSym = info->symbolInfo;

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
          constField =
              llvm::Constant::getNullValue(getLLVMType(memberSym->type().type));
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

  // Initialize heap fields FIRST (before storing normal fields)
  initializeRecordHeapFields(instancePtr, structTy, instName, userInits);

  for (auto const &[memberName, info] : orderedMembers) {
    auto memberSym = info->symbolInfo;

    // Skip heap fields - already handled by initializeRecordHeapFields
    if (memberSym && memberSym->storage().isHeap)
      continue;

    // Handle normal fields...
    llvm::Value *memberPtr = funcBuilder.CreateStructGEP(
        structTy, instancePtr, memberSym->type().memberIndex, memberName);
    llvm::Value *finalVal = nullptr;

    auto it = userInits.find(memberName);
    if (it != userInits.end()) {
      finalVal = generateExpression(it->second->value.get());
    } else {
      auto varDecl = dynamic_cast<VariableDeclaration *>(info->node);
      if (varDecl && varDecl->initializer) {
        finalVal = generateExpression(varDecl->initializer.get());
      } else {
        finalVal =
            llvm::Constant::getNullValue(getLLVMType(memberSym->type().type));
      }
    }

    funcBuilder.CreateStore(finalVal, memberPtr);
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

void IRGenerator::generateMethodsStatement(Node *node) {
  auto metStmt = dynamic_cast<MethodsStatement *>(node);
  if (!metStmt)
    return;

  auto metName =
      semantics.extractIdentifierName(metStmt->method_identifier.get());

  metTracker.insideMethods = true;
  metTracker.typeName = metName;
  currentMethods = metStmt;
  for (const auto &methods : metStmt->functions)
    generateStatement(methods.get());

  metTracker.insideMethods = false;
  metTracker.typeName = "undefined";
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
      semantics.payload.customTypesTable[enumInfo->type().type.resolvedName];

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

void IRGenerator::initializeRecordHeapFields(
    llvm::Value *structPtr, llvm::StructType *structTy,
    const std::string &typeName,
    const std::unordered_map<std::string, AssignmentStatement *> &userInits) {

  auto typeInfo = semantics.payload.customTypesTable[typeName];
  if (!typeInfo) {
    reportDevBug("No type info for record: " + typeName, nullptr);
  }

  // Sort members by index
  std::vector<std::pair<std::string, std::shared_ptr<MemberInfo>>>
      orderedMembers;
  for (const auto &[name, info] : typeInfo->members) {
    orderedMembers.push_back({name, info});
  }
  std::sort(orderedMembers.begin(), orderedMembers.end(),
            [](const auto &a, const auto &b) {
              return a.second->symbolInfo->type().memberIndex <
                     b.second->symbolInfo->type().memberIndex;
            });

  for (const auto &[memberName, memberInfo] : orderedMembers) {
    // Get the member symbol to check if it's a heap field
    auto memberSym = memberInfo->symbolInfo;
    if (!memberSym)
      continue;

    // ONLY handle heap fields
    if (!memberSym->storage().isHeap)
      continue;

    // Get the base type (what the field stores)
    llvm::Type *baseType = getLLVMType(memberInfo->symbolInfo->type().type);
    size_t fieldSize = layout->getTypeAllocSize(baseType);

    // Create a temporary symbol info for allocation
    auto tempSym = std::make_shared<SymbolInfo>();
    tempSym->storage().allocType = memberSym->storage().allocType;
    tempSym->type().type = memberInfo->symbolInfo->type().type;
    tempSym->codegen().componentSize = fieldSize;

    // Allocate heap storage for this field
    llvm::Value *heapPtr = allocateRuntimeHeap(
        tempSym, funcBuilder.getInt64(fieldSize), memberName);

    // Store the heap pointer in the struct slot
    llvm::Value *fieldSlot = funcBuilder.CreateStructGEP(
        structTy, structPtr, memberInfo->symbolInfo->type().memberIndex,
        memberName + "_slot");
    funcBuilder.CreateStore(heapPtr, fieldSlot);

    // Handle default value or user-provided value
    llvm::Value *initValue = nullptr;

    // Check if user provided a value for this field
    auto userIt = userInits.find(memberName);
    if (userIt != userInits.end()) {
      // User provided value evaluate it
      initValue = generateExpression(userIt->second->value.get());
    } else {
      // No user value check for default in declaration
      auto varDecl = dynamic_cast<VariableDeclaration *>(memberInfo->node);
      if (varDecl && varDecl->initializer) {
        initValue = generateExpression(varDecl->initializer.get());
      } else {
        // No default zero initialize
        initValue = llvm::Constant::getNullValue(baseType);
      }
    }

    // Store the initial value to the heap location
    if (initValue) {
      funcBuilder.CreateStore(initValue, heapPtr);
    }
    memberSym->codegen().llvmValue = heapPtr;
  }
}
