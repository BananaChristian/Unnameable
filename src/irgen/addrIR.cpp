#include <string>

#include "ast.hpp"
#include "irgen.hpp"

// Identifier L-value generator
llvm::Value *IRGenerator::generateIdentifierAddress(Node *node) {
    llvm::Value *address = nullptr;

    auto identExpr = dynamic_cast<Identifier *>(node);
    if (!identExpr) {
        reportDevBug("Invalid identifier expression", node);
    }

    const std::string &identName = identExpr->identifier.TokenLiteral;

    auto sym = semantics.getSymbolFromMeta(identExpr);
    if (!sym) {
        errorHandler.addHint("Semantics did not register the identifier symbol info");
        reportDevBug("Could not find identifier metadata", identExpr);
    }

    llvm::Value *variablePtr = sym->codegen().llvmValue;
    if (!variablePtr) reportDevBug("No value for '" + identName + "'", identExpr);

    if (sym->type().isRef) {
        llvm::Type *ptrType = llvm::PointerType::get(funcBuilder.getContext(), 0);
        auto *load = funcBuilder.CreateLoad(ptrType, variablePtr, identName + "_ref_addr");
        if (sym->storage().isVolatile) {
            load->setVolatile(true);
        }
        variablePtr = load;
    }

    // Component instance -> pointer to struct
    auto compIt = componentTypes.find(sym->type().type.resolvedName);
    if (compIt != componentTypes.end()) {
        address = variablePtr;
    } else {
        // scalar/heap -> ensure typed pointer
        if (sym->storage().isHeap) {
            llvm::Type *elemTy = sym->codegen().llvmType;
            if (!elemTy) {
                reportDevBug("No type for '" + identName + "'", identExpr);
            }

            llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
            if (variablePtr->getType() != expectedPtrTy) {
                variablePtr =
                    funcBuilder.CreateBitCast(variablePtr, expectedPtrTy, identName + "_ptr_typed");
            }
            address = variablePtr;
        } else {
            if (variablePtr->getType()->isPointerTy()) {
                address = variablePtr;
            } else {
                reportDevBug("Identifier '" + identName + "' doesnt have a value", identExpr);
            }
        }
    }

    return address;
}

// Infix L-value generator
llvm::Value *IRGenerator::generateInfixAddress(Node *node) {
    auto infix = dynamic_cast<InfixExpression *>(node);
    llvm::Value *address = generateAddress(infix->left_operand.get());
    if (!address) reportDevBug("Failed to generate L-Value", infix->left_operand.get());

    if (infix->operat.type == TokenType::FULLSTOP) {
        auto lhsMeta = semantics.metaData[infix->left_operand.get()];
        auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());

        if (lhsMeta->type().type.isPointer() || lhsMeta->type().type.isRef()) {
            llvm::Type *ptrTy = llvm::PointerType::get(funcBuilder.getContext(), 0);
            auto *load = funcBuilder.CreateLoad(ptrTy, address, "ptr_deref");
            if (lhsMeta->storage().isVolatile) {
                load->setVolatile(true);
            }
            address = load;
        }

        std::string lookUpName = lhsMeta->type().type.resolvedName;
        if (lhsMeta->type().type.isPointer() || lhsMeta->type().type.isRef())
            lookUpName = semantics.getBaseTypeName(lhsMeta->type().type);
        llvm::StructType *structTy = llvmCustomTypes[lookUpName];

        auto memberInfo = semantics.metaData[infix];
        unsigned memberIndex = memberInfo->type().memberIndex;

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
        reportDevBug("Invalid self expression", node);
    }

    const std::string &compName = currentComponent->component_name->expression.TokenLiteral;

    // Lookup LLVM struct for top-level component
    llvm::StructType *currentStructTy = nullptr;
    auto it = componentTypes.find(compName);
    if (it == componentTypes.end()) {
        errorHandler.addHint("Component '" + compName +
                             "' was not added into the componentTypes table");
        reportDevBug("Component '" + compName + "' not found in componentTypes",
                     currentComponent->component_name.get());
    }

    currentStructTy = it->second;

    // Load 'self' pointer
    llvm::AllocaInst *selfAlloca = currentFunctionSelfMap[currentFunction];
    if (!selfAlloca) {
        reportDevBug("'self' access outside component method", selfExpr);
    }

    auto *selfLoad =
        funcBuilder.CreateLoad(currentStructTy->getPointerTo(), selfAlloca, "self_load");
    // Self is never volatile (it's just a pointer to the component)

    llvm::Value *currentPtr = selfLoad;

    // Semantic chain walk
    auto ctIt = semantics.customTypesTable.find(compName);
    if (ctIt == semantics.customTypesTable.end()) {
        errorHandler.addHint("The type was never registered by the semantic analyzer");
        reportDevBug("Component not found in customTypeTable",
                     currentComponent->component_name.get());
    }

    auto currentTypeInfo = ctIt->second;
    std::shared_ptr<MemberInfo> lastMemberInfo = nullptr;

    for (size_t i = 0; i < selfExpr->fields.size(); ++i) {
        auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
        if (!ident) {
            reportDevBug("Invalid node in the self chain", selfExpr);
        }

        std::string currentTypeName = currentTypeInfo->type.resolvedName;
        if (currentTypeInfo->type.isPointer() || currentTypeInfo->type.isRef())
            currentTypeName = semantics.getBaseTypeName(currentTypeInfo->type);

        std::string fieldName = ident->identifier.TokenLiteral;

        auto memIt = currentTypeInfo->members.find(fieldName);
        if (memIt == currentTypeInfo->members.end()) {
            reportDevBug("Field not found in '" + currentTypeName + "'", selfExpr);
        }

        lastMemberInfo = memIt->second;

        // --- GEP for this field ---
        auto llvmIt = llvmCustomTypes.find(currentTypeName);
        if (llvmIt == llvmCustomTypes.end()) {
            errorHandler.addHint("Could not find the type '" + currentTypeName +
                                 "' in the type table");
            reportDevBug("Unrecognised type '" + currentTypeName + "'", selfExpr);
        }

        llvm::StructType *structTy = llvmIt->second;

        currentPtr = funcBuilder.CreateStructGEP(structTy, currentPtr, lastMemberInfo->memberIndex,
                                                 fieldName + "_ptr");

        // --- Drill into nested type if needed ---
        if (lastMemberInfo->type.kind == DataType::COMPONENT ||
            lastMemberInfo->type.kind == DataType::RECORD) {
            std::string lookUpName = lastMemberInfo->type.resolvedName;
            if (lastMemberInfo->type.isPointer() || lastMemberInfo->type.isRef())
                lookUpName = semantics.getBaseTypeName(lastMemberInfo->type);

            auto nestedIt = semantics.customTypesTable.find(lookUpName);
            if (nestedIt == semantics.customTypesTable.end()) {
                errorHandler.addHint("Type '" + lookUpName +
                                     "' was not registered by the semantic analyzer");
                reportDevBug("Nested type '" + lookUpName + "' not found", selfExpr);
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
    if (!arrExpr) reportDevBug("Null subscript expression", node);

    auto baseSym = semantics.getSymbolFromMeta(arrExpr);
    auto identSym = semantics.getSymbolFromMeta(arrExpr->identifier.get());

    if (!baseSym) reportDevBug("Missing array subscript symbol info", arrExpr);
    if (!identSym) reportDevBug("Missing  subscript ident symbol info", arrExpr);

    llvm::Value *allocaPtr = generateIdentifierAddress(arrExpr->identifier.get());
    llvm::Value *dataPtr = allocaPtr;

    if (identSym->type().isPointer) {
        logInternal("  -> Identifier is a raw pointer, loading base address...");
        dataPtr = funcBuilder.CreateLoad(funcBuilder.getPtrTy(), allocaPtr, "ptr_base");
    }

    const auto &dims = identSym->type().sizePerDimensions;
    const auto &dynDims = identSym->type().dynSizePerDimensions;

    logInternal("[SUBSCRIPT] Static Dims: " + std::to_string(dims.size()) +
                " | Dynamic Bag: " + std::to_string(dynDims.size()));

    llvm::Value *totalOffset = funcBuilder.getInt64(0);

    for (size_t i = 0; i < arrExpr->index_exprs.size(); ++i) {
        logInternal("[SUBSCRIPT] Processing Index Level: " + std::to_string(i));

        llvm::Value *idx = generateExpression(arrExpr->index_exprs[i].get());
        idx = funcBuilder.CreateIntCast(idx, funcBuilder.getInt64Ty(), false);

        // Limit Resolution
        llvm::Value *limit = nullptr;
        if (i < dims.size() && dims[i] > 0) {
            logInternal("  -> Found static limit: " + std::to_string(dims[i]));
            limit = funcBuilder.getInt64(dims[i]);
        } else if (i < dynDims.size() && dynDims[i]) {
            logInternal("  -> Found dynamic node in bag. Attempting re-gen...");
            limit = generateExpression(dynDims[i]);
            if (limit) {
                logInternal("  -> Success: Dynamic limit IR generated.");
                limit = funcBuilder.CreateIntCast(limit, funcBuilder.getInt64Ty(), false);
            } else {
                logInternal(
                    "  -> !! FAILURE !!: generateExpression returned NULL for dynamic dim.");
            }
        }
        // Bounds Check
        if (limit) {
            logInternal("  -> Emitting ICMP and Branch for bounds check.");
            llvm::Value *isOutOfBounds = funcBuilder.CreateICmpUGE(idx, limit, "out_of_bounds");
            llvm::BasicBlock *panicBB =
                llvm::BasicBlock::Create(context, "bounds.panic", currentFunction);
            llvm::BasicBlock *successBB =
                llvm::BasicBlock::Create(context, "bounds.ok", currentFunction);

            funcBuilder.CreateCondBr(isOutOfBounds, panicBB, successBB);
            funcBuilder.SetInsertPoint(panicBB);
            funcBuilder.CreateCall(
                llvm::Intrinsic::getDeclaration(module.get(), llvm::Intrinsic::trap));
            funcBuilder.CreateUnreachable();
            funcBuilder.SetInsertPoint(successBB);
        } else {
            logInternal("  -> Skipping bounds check (No limit found for this level).");
        }

        // Stride Calculation
        llvm::Value *currentStride = funcBuilder.getInt64(1);
        size_t maxDims = std::max(dims.size(), dynDims.size());

        for (size_t j = i + 1; j < maxDims; ++j) {
            llvm::Value *dimSize = nullptr;
            if (j < dims.size() && dims[j] > 0) {
                dimSize = funcBuilder.getInt64(dims[j]);
            } else if (j < dynDims.size() && dynDims[j]) {
                logInternal("  -> Stride Calc: Re-gen for dim " + std::to_string(j));
                dimSize = generateExpression(dynDims[j]);
                dimSize = funcBuilder.CreateIntCast(dimSize, funcBuilder.getInt64Ty(), false);
            }

            if (dimSize) {
                currentStride = funcBuilder.CreateMul(currentStride, dimSize, "stride_update");
            }
        }

        llvm::Value *scaledIdx = funcBuilder.CreateMul(idx, currentStride, "index_stride");
        totalOffset = funcBuilder.CreateAdd(totalOffset, scaledIdx, "accum_offset");
    }

    llvm::Type *elemTy = getLLVMType(semantics.getArrayElementType(baseSym->type().type));
    logInternal("[SUBSCRIPT] Final GEP with element type size: " +
                std::to_string(layout->getTypeAllocSize(elemTy)));

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

    if (!addr) {
        reportDevBug("Failed to get address to dereference ", derefExpr);
    }

    auto ptrType = llvm::PointerType::getUnqual(context);

    auto *baseLoad = funcBuilder.CreateLoad(ptrType, addr, "base_lift");
    // Get symbol for the pointer being dereferenced
    auto sym = semantics.getSymbolFromMeta(current);
    if (sym && sym->storage().isVolatile) {
        baseLoad->setVolatile(true);
    }
    addr = baseLoad;

    for (int i = 0; i < derefCount - 1; i++) {
        auto *hopLoad = funcBuilder.CreateLoad(ptrType, addr, "deref_hop_addr");
        // Intermediate loads also need to be volatile if the pointer is volatile
        if (sym && sym->storage().isVolatile) {
            hopLoad->setVolatile(true);
        }
        addr = hopLoad;
    }

    for (const auto pendingFree : pendingFrees) funcBuilder.Insert(pendingFree);

    return addr;
}
