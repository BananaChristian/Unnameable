#include "ast.hpp"
#include "irgen.hpp"
#include <string>

// Identifier L-value generator
llvm::Value *IRGenerator::generateIdentifierAddress(Node *node) {
  logInternal("Inside identifier address generation(L VALUE) for " +
              node->toString());
  llvm::Value *address = nullptr;

  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr) {
    reportDevBug("Invalid identifier expression", node->token.line,
                 node->token.column);
  }

  const std::string &identName = identExpr->identifier.TokenLiteral;
  auto metaIt = semantics.metaData.find(identExpr);
  if (metaIt == semantics.metaData.end()) {
    errorHandler.addHint("Semantics did not register the identifier metadata");
    reportDevBug("Could not find identifier metadata",
                 identExpr->identifier.line, identExpr->identifier.column);
  }

  auto sym = metaIt->second;

  llvm::Value *variablePtr = sym->llvmValue;
  if (!variablePtr)
    reportDevBug("No value for '" + identName + "'", identExpr->identifier.line,
                 identExpr->identifier.column);

  if (sym->isRef) {
    llvm::Type *ptrType = llvm::PointerType::get(funcBuilder.getContext(), 0);
    variablePtr =
        funcBuilder.CreateLoad(ptrType, variablePtr, identName + "_ref_addr");
  }

  // Component instance -> pointer to struct
  auto compIt = componentTypes.find(sym->type.resolvedName);
  if (compIt != componentTypes.end()) {
    address = variablePtr;
  } else {
    // scalar/heap -> ensure typed pointer
    if (sym->isSage || sym->isHeap) {
      logInternal("The identifier is heap raised");
      llvm::Type *elemTy = sym->llvmType;
      if (!elemTy) {
        reportDevBug("No type for '" + identName + "'",
                     identExpr->identifier.line, identExpr->identifier.column);
      }

      llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
      if (variablePtr->getType() != expectedPtrTy) {
        variablePtr = funcBuilder.CreateBitCast(variablePtr, expectedPtrTy,
                                                identName + "_ptr_typed");
      }
      address = variablePtr;
    } else {
      if (variablePtr->getType()->isPointerTy()) {
        logInternal("Taken normal pointer path");
        address = variablePtr;
      } else {
        reportDevBug("Identifier '" + identName + "' doesnt have a value",
                     identExpr->identifier.line, identExpr->identifier.column);
      }
    }
  }

  return address;
}

// Infix L-value generator
llvm::Value *IRGenerator::generateInfixAddress(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  llvm::Value *address = generateAddress(infix->left_operand.get());
  if (!address)
    reportDevBug("Failed to generate L-Value",
                 infix->left_operand->expression.line,
                 infix->left_operand->expression.column);

  if (infix->operat.type == TokenType::FULLSTOP) {
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];
    auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());

    if (lhsMeta->type.isPointer || lhsMeta->type.isRef) {
      llvm::Type *ptrTy = llvm::PointerType::get(funcBuilder.getContext(), 0);
      address = funcBuilder.CreateLoad(ptrTy, address, "ptr_deref");
    }

    std::string lookUpName = lhsMeta->type.resolvedName;
    if (lhsMeta->type.isPointer) {
      lookUpName = semantics.stripPtrSuffix(lhsMeta->type.resolvedName);
    } else if (lhsMeta->type.isRef) {
      lookUpName = semantics.stripRefSuffix(lhsMeta->type.resolvedName);
    }

    llvm::StructType *structTy = llvmCustomTypes[lookUpName];

    auto memberInfo = semantics.metaData[infix];
    unsigned memberIndex = memberInfo->memberIndex;

    address = funcBuilder.CreateStructGEP(structTy, address, memberIndex,
                                          rhsIdent->identifier.TokenLiteral);

    return address;
  }

  throw std::runtime_error("Infix operator cannot be treated as an address");
}

// Self L-Value  generator
llvm::Value *IRGenerator::generateSelfAddress(Node *node) {
  auto selfExpr = dynamic_cast<SelfExpression *>(node);
  if (!selfExpr) {
    reportDevBug("Invalid self expression", node->token.line,
                 node->token.column);
  }

  const std::string &compName =
      currentComponent->component_name->expression.TokenLiteral;

  // Lookup LLVM struct for top-level component
  llvm::StructType *currentStructTy = nullptr;
  auto it = componentTypes.find(compName);
  if (it == componentTypes.end()) {
    errorHandler.addHint("Component '" + compName +
                         "' was not added into the componentTypes table");
    reportDevBug("Component '" + compName + "' not found in componentTypes",
                 currentComponent->component_name->expression.line,
                 currentComponent->component_name->expression.column);
  }

  currentStructTy = it->second;

  // --- Load 'self' pointer ---
  llvm::AllocaInst *selfAlloca = currentFunctionSelfMap[currentFunction];
  if (!selfAlloca) {
    reportDevBug("'self' access outside component method",
                 selfExpr->expression.line, selfExpr->expression.column);
  }

  llvm::Value *currentPtr = funcBuilder.CreateLoad(
      currentStructTy->getPointerTo(), selfAlloca, "self_load");

  // --- Semantic chain walk ---
  auto ctIt = semantics.customTypesTable.find(compName);
  if (ctIt == semantics.customTypesTable.end()) {
    errorHandler.addHint(
        "The type was never registered by the semantic analyzer");
    reportDevBug("Component not found in customTypeTable",
                 currentComponent->component_name->expression.line,
                 currentComponent->component_name->expression.column);
  }

  auto currentTypeInfo = ctIt->second;
  std::shared_ptr<MemberInfo> lastMemberInfo = nullptr;

  for (size_t i = 0; i < selfExpr->fields.size(); ++i) {
    auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
    if (!ident) {
      reportDevBug("Invalid node in the self chain", selfExpr->expression.line,
                   selfExpr->expression.column);
    }

    std::string currentTypeName = currentTypeInfo->type.resolvedName;
    if (currentTypeInfo->type.isPointer)
      currentTypeName =
          semantics.stripPtrSuffix(currentTypeInfo->type.resolvedName);
    else if (currentTypeInfo->type.isRef)
      currentTypeName =
          semantics.stripRefSuffix(currentTypeInfo->type.resolvedName);

    std::string fieldName = ident->identifier.TokenLiteral;

    auto memIt = currentTypeInfo->members.find(fieldName);
    if (memIt == currentTypeInfo->members.end()) {
      reportDevBug("Field not found in '" + currentTypeName + "'",
                   selfExpr->expression.line, selfExpr->expression.column);
    }

    lastMemberInfo = memIt->second;

    // --- GEP for this field ---
    auto llvmIt = llvmCustomTypes.find(currentTypeName);
    if (llvmIt == llvmCustomTypes.end()) {
      errorHandler.addHint("Could not find the type '" + currentTypeName +
                           "' in the type table");
      reportDevBug("Unrecognised type '" + currentTypeName + "'",
                   selfExpr->expression.line, selfExpr->expression.column);
    }

    llvm::StructType *structTy = llvmIt->second;

    currentPtr = funcBuilder.CreateStructGEP(
        structTy, currentPtr, lastMemberInfo->memberIndex, fieldName + "_ptr");

    // --- Drill into nested type if needed ---
    if (lastMemberInfo->type.kind == DataType::COMPONENT ||
        lastMemberInfo->type.kind == DataType::RECORD) {
      std::string lookUpName = lastMemberInfo->type.resolvedName;
      if (lastMemberInfo->type.isPointer) {
        lookUpName =
            semantics.stripPtrSuffix(lastMemberInfo->type.resolvedName);
      } else if (lastMemberInfo->type.isRef)
        lookUpName =
            semantics.stripRefSuffix(lastMemberInfo->type.resolvedName);
      auto nestedIt = semantics.customTypesTable.find(lookUpName);
      if (nestedIt == semantics.customTypesTable.end()) {
        errorHandler.addHint("Type '" + lookUpName +
                             "' was not registered by the semantic analyzer");
        reportDevBug("Nested type '" + lookUpName + "' not found",
                     selfExpr->expression.line, selfExpr->expression.column);
      }

      currentTypeInfo = nestedIt->second;
    } else {
      // primitive reached, stop drilling
      currentTypeInfo = nullptr;
    }
  }

  return currentPtr;
}

llvm::Value *IRGenerator::generateArraySubscriptAddress(Node *node) {
  auto arrExpr = dynamic_cast<ArraySubscript *>(node);

  auto arrIt = semantics.metaData.find(arrExpr);
  auto baseSym = arrIt->second;

  llvm::Value *allocaPtr = generateIdentifierAddress(arrExpr->identifier.get());
  llvm::Value *dataPtr =
      funcBuilder.CreateLoad(funcBuilder.getPtrTy(), allocaPtr, "raw_data_ptr");

  const auto &dims = baseSym->sizePerDimensions;

  // 3. Calculate the Flat Linear Offset
  llvm::Value *totalOffset = funcBuilder.getInt64(0);

  for (size_t i = 0; i < arrExpr->index_exprs.size(); ++i) {
    llvm::Value *idx = generateExpression(arrExpr->index_exprs[i].get());
    idx = funcBuilder.CreateIntCast(idx, funcBuilder.getInt64Ty(), false);

    // --- BOUNDS CHECK START ---
    // Only guard if the dimension length is known (> 0)
    if (i < dims.size() && dims[i] > 0) {
      llvm::Value *limit = funcBuilder.getInt64(dims[i]);
      // Unsigned check: catches idx < 0 and idx >= limit in one go
      llvm::Value *isOutOfBounds =
          funcBuilder.CreateICmpUGE(idx, limit, "out_of_bounds");

      llvm::BasicBlock *panicBB =
          llvm::BasicBlock::Create(context, "bounds.panic", currentFunction);
      llvm::BasicBlock *successBB =
          llvm::BasicBlock::Create(context, "bounds.ok", currentFunction);

      funcBuilder.CreateCondBr(isOutOfBounds, panicBB, successBB);

      // Panic Path
      funcBuilder.SetInsertPoint(panicBB);
      auto *trap =
          llvm::Intrinsic::getDeclaration(module.get(), llvm::Intrinsic::trap);
      funcBuilder.CreateCall(trap);
      funcBuilder.CreateUnreachable();

      // Success Path - keep going
      funcBuilder.SetInsertPoint(successBB);
    }

    uint64_t stride = 1;
    for (size_t j = i + 1; j < dims.size(); ++j) {
      stride *= dims[j];
    }

    llvm::Value *scaledIdx = funcBuilder.CreateMul(
        idx, funcBuilder.getInt64(stride), "index_stride");
    totalOffset = funcBuilder.CreateAdd(totalOffset, scaledIdx, "accum_offset");
  }

  // We treat dataPtr as a pointer to the final element type (e.g., i32)
  llvm::Type *elemTy = getLLVMType(baseSym->type);
  return funcBuilder.CreateGEP(elemTy, dataPtr, {totalOffset}, "element_ptr");
}

llvm::Value *IRGenerator::generateDereferenceAddress(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);

  Node *current = node;
  int derefCount = 0;
  while (auto nested = dynamic_cast<DereferenceExpression *>(current)) {
    derefCount++;
    current = nested->identifier.get();
  }

  llvm::Value *addr = generateAddress(current);
  std::vector<llvm::CallInst *> pendingFrees;

  auto identNode = dynamic_cast<Identifier *>(current);

  if (!addr) {
    reportDevBug("Failed to get address to dereference ",
                 derefExpr->expression.line, derefExpr->expression.column);
  }

  auto ptrType = llvm::PointerType::getUnqual(context);

  addr = funcBuilder.CreateLoad(ptrType, addr, "base_lift");

  for (int i = 0; i < derefCount - 1; i++) {
    addr = funcBuilder.CreateLoad(ptrType, addr, "deref_hop_addr");
  }

  for (const auto pendingFree : pendingFrees)
    funcBuilder.Insert(pendingFree);

  return addr;
}
