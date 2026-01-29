#include "ast.hpp"
#include "irgen.hpp"

AddressAndPendingFree IRGenerator::generateIdentifierAddress(Node *node) {
  AddressAndPendingFree out{nullptr, {}};

  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr)
    throw std::runtime_error("Invalid identifier expression: " +
                             node->toString());

  const std::string &identName = identExpr->identifier.TokenLiteral;
  auto metaIt = semantics.metaData.find(identExpr);
  if (metaIt == semantics.metaData.end())
    throw std::runtime_error("Unidentified identifier '" + identName + "'");

  auto sym = metaIt->second;
  if (sym->hasError)
    throw std::runtime_error("Semantic error detected ");

  llvm::Value *variablePtr = sym->llvmValue;
  if (!variablePtr)
    throw std::runtime_error("No llvm value for '" + identName + "'");

  if (sym->isRef) {
    llvm::Type *ptrType = llvm::PointerType::get(funcBuilder.getContext(), 0);
    variablePtr =
        funcBuilder.CreateLoad(ptrType, variablePtr, identName + "_ref_addr");
  }

  // Component instance -> pointer to struct
  auto compIt = componentTypes.find(sym->type.resolvedName);
  if (compIt != componentTypes.end()) {
    out.address = variablePtr; // already a pointer to struct instance
  } else {
    // scalar/heap -> ensure typed pointer
    if (sym->isHeap) {
      std::cout << "The identifier is heap raised\n";
      llvm::Type *elemTy = sym->llvmType;
      if (!elemTy)
        throw std::runtime_error("llvmType null for heap scalar '" + identName +
                                 "'");

      // If sym->llvmValue is a GlobalVariable (the module slot that stores T*),
      // we must load the runtime pointer from it.
      if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(variablePtr)) {
        std::cout << "Inside global heap var path PATH1 \n";
        llvm::PointerType *ptrType = llvm::PointerType::get(
            funcBuilder.getContext(), 0); // Generic ptr type
        llvm::Value *runtimePtr =
            funcBuilder.CreateLoad(ptrType, gv, identName + "_runtime_ptr");
        out.address = runtimePtr; // address usable for subsequent loads/stores
      } else {
        std::cout << "Inside global heap var path PATH2c \n";
        // variablePtr might already be a direct pointer (alloca or
        // bitcast), ensure type matches
        llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
        if (variablePtr->getType() != expectedPtrTy)
          variablePtr = funcBuilder.CreateBitCast(variablePtr, expectedPtrTy,
                                                  identName + "_ptr_typed");
        out.address = variablePtr;
      }
    } else {
      if (variablePtr->getType()->isPointerTy()) {
        std::cout << "Taken raw_ptr path\n";
        out.address = variablePtr;
      } else {
        throw std::runtime_error("Identifier '" + identName +
                                 "' does not have pointer-like llvmValue");
      }
    }
  }

  // Should not free if this needs a post loop free
  if (sym->needsPostLoopFree) {
    return out;
  }
  // Prepare pending free call (create CallInst but don't insert)
  if (sym->isSage) {
    bool shouldFree = identExpr->isKiller ||
                      ((sym->lastUseNode == identExpr) && (sym->refCount == 0));
    if (shouldFree) {
      std::cout << "[IR DEBUG] Freeing '" << identName
                << "' | popCount discovered: " << sym->popCount << "\n";
      llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
          "sage_free", llvm::Type::getVoidTy(context));

      int weight = (sym->popCount > 0) ? sym->popCount : 1;

      for (int i = 0; i < weight; i++) {
        llvm::CallInst *callInst = llvm::CallInst::Create(sageFreeFn);
        callInst->setCallingConv(llvm::CallingConv::C);
        // Stash it in the vector
        out.pendingFrees.push_back(callInst);
      }
    }
  } else if (sym->isHeap) {
    // Determine if we should trigger a free
    bool shouldFree = identExpr->isKiller ||
                      ((sym->lastUseNode == identExpr) && (sym->refCount == 0));

    if (shouldFree) {
      // Get allocator info
      const std::string &allocatorTypeName = sym->allocType;
      auto it = semantics.allocatorMap.find(allocatorTypeName);

      if (it != semantics.allocatorMap.end()) {
        auto handle = it->second;
        llvm::Function *freeFunc = module->getFunction(handle.freeName);

        if (freeFunc) {
          llvm::Value *ptrToFree = sym->llvmValue;

          // Create the single Call Instruction
          llvm::CallInst *callInst =
              llvm::CallInst::Create(freeFunc, {ptrToFree}, "");
          callInst->setCallingConv(llvm::CallingConv::C);

          out.pendingFrees.push_back(callInst);
        }
      }
    }
  }

  return out;
}

// Infix generator
AddressAndPendingFree IRGenerator::generateInfixAddress(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);

  AddressAndPendingFree out;

  if (auto ident = dynamic_cast<Identifier *>(infix->left_operand.get())) {
    out = generateIdentifierAddress(infix->left_operand.get());
  } else {
    out = generateInfixAddress(infix->left_operand.get());
  }

  if (infix->operat.type == TokenType::FULLSTOP) {
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];
    auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());

    if (lhsMeta->type.isPointer || lhsMeta->type.isRef) {
      llvm::Type *ptrTy = llvm::PointerType::get(funcBuilder.getContext(), 0);
      out.address = funcBuilder.CreateLoad(ptrTy, out.address, "ptr_deref");
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

    out.address = funcBuilder.CreateStructGEP(
        structTy, out.address, memberIndex, rhsIdent->identifier.TokenLiteral);

    return out;
  }

  throw std::runtime_error("Infix operator cannot be treated as an address");
}

// Self address generator
llvm::Value *IRGenerator::generateSelfAddress(Node *node) {
  auto selfExpr = dynamic_cast<SelfExpression *>(node);
  if (!selfExpr)
    throw std::runtime_error("Invalid self expression");

  std::cout << "[IR] Generating IR for self address: " << selfExpr->toString()
            << "\n";

  const std::string &compName =
      currentComponent->component_name->expression.TokenLiteral;

  // Lookup LLVM struct for top-level component
  llvm::StructType *currentStructTy = nullptr;
  auto it = componentTypes.find(compName);
  if (it == componentTypes.end())
    throw std::runtime_error("Component '" + compName +
                             "' not found in componentTypes");

  currentStructTy = it->second;

  // --- Load 'self' pointer ---
  llvm::AllocaInst *selfAlloca = currentFunctionSelfMap[currentFunction];
  if (!selfAlloca)
    throw std::runtime_error("'self' access outside component method");

  llvm::Value *currentPtr = funcBuilder.CreateLoad(
      currentStructTy->getPointerTo(), selfAlloca, "self_load");

  // --- Semantic chain walk ---
  auto ctIt = semantics.customTypesTable.find(compName);
  if (ctIt == semantics.customTypesTable.end())
    throw std::runtime_error("Component not found in customTypesTable");

  auto currentTypeInfo = ctIt->second;
  std::shared_ptr<MemberInfo> lastMemberInfo = nullptr;

  for (size_t i = 0; i < selfExpr->fields.size(); ++i) {
    auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
    if (!ident)
      throw std::runtime_error("Expected identifier in self chain");

    std::string currentTypeName = currentTypeInfo->type.resolvedName;
    if (currentTypeInfo->type.isPointer)
      currentTypeName =
          semantics.stripPtrSuffix(currentTypeInfo->type.resolvedName);
    else if (currentTypeInfo->type.isRef)
      currentTypeName =
          semantics.stripRefSuffix(currentTypeInfo->type.resolvedName);

    std::string fieldName = ident->identifier.TokenLiteral;

    auto memIt = currentTypeInfo->members.find(fieldName);
    if (memIt == currentTypeInfo->members.end())
      throw std::runtime_error("Field not found in '" + currentTypeName + "'");

    lastMemberInfo = memIt->second;

    // --- GEP for this field ---
    auto llvmIt = llvmCustomTypes.find(currentTypeName);
    if (llvmIt == llvmCustomTypes.end())
      throw std::runtime_error("LLVM struct missing for type '" +
                               currentTypeName + "'");

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
      if (nestedIt == semantics.customTypesTable.end())
        throw std::runtime_error("Nested type '" + lookUpName + "'not found");

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

  llvm::Value *allocaPtr =
      generateIdentifierAddress(arrExpr->identifier.get()).address;
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

  llvm::Value *addr;
  std::vector<llvm::CallInst *> pendingFrees;

  auto identNode = dynamic_cast<Identifier *>(current);

  if (auto identNode = dynamic_cast<Identifier *>(current)) {
    AddressAndPendingFree out = generateIdentifierAddress(identNode);
    addr = out.address;
    pendingFrees = out.pendingFrees;
  } else {
    addr = generateAddress(current);
  }

  if (!addr) {
    throw std::runtime_error("Failed to get address to dereference");
  }

  auto ptrType = llvm::PointerType::getUnqual(context);

  addr = funcBuilder.CreateLoad(ptrType, addr, "base_lift");

  for (int i = 0; i < derefCount - 1; i++) {
    addr = funcBuilder.CreateLoad(ptrType, addr, "deref_hop_addr");
  }

  for (auto *freeCall : pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  return addr;
}
