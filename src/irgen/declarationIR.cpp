#include <llvm-18/llvm/IR/Attributes.h>

#include "ast.hpp"
#include "irgen.hpp"

void IRGenerator::generateVariableDeclaration(Node *node) {
    auto declaration = dynamic_cast<VariableDeclaration *>(node);
    if (!declaration) return;

    const std::string &name = declaration->var_name->expression.TokenLiteral;

    auto sym = semantics.getSymbolFromMeta(declaration);
    if (!sym) reportDevBug("Failed to find declaration '" + name + "'", declaration);

    // Global scope, only scalars, no heap
    if (isGlobalScope) {
        if (sym->storage().isHeap) reportDevBug("Cannot heap raise in global scope", declaration);
        generateGlobalScalarLet(sym, name, declaration->initializer.get());
        return;
    }

    llvm::Value *storage = nullptr;

    if (sym->type().isArray)
        storage = generateArrayStorage(declaration, sym, name);
    else if (sym->type().isRef)
        storage = generateReferenceStorage(declaration, sym, name);
    else if (isComponentType(sym->type().type.resolvedName))
        storage = generateComponentStorage(declaration, sym, name);
    else
        storage = generateScalarStorage(declaration, sym, name);

    if (!storage) reportDevBug("No storage allocated for '" + name + "'", declaration);

    sym->codegen().llvmValue = storage;
    sym->codegen().llvmType = getLLVMType(sym->type().type);
    emitDeclarationClean(declaration);
}

llvm::Value *IRGenerator::generateScalarStorage(VariableDeclaration *decl,
                                                std::shared_ptr<SymbolInfo> sym,
                                                const std::string &name) {
    llvm::Value *initVal = nullptr;
    if (decl->initializer) initVal = generateExpression(decl->initializer.get());

    if (sym->storage().isHeap) {
        // heap scalar
        llvm::Value *storage = allocateDynamicHeapStorage(sym, name);
        if (!initVal) initVal = llvm::Constant::getNullValue(getLLVMType(sym->type().type));
        auto *store = funcBuilder.CreateStore(initVal, storage);
        if (sym->storage().isVolatile) store->setVolatile(true);
        sym->type().isAddress = true;
        return storage;
    }

    // stack scalar
    llvm::Type *varTy = getLLVMType(sym->type().type);
    llvm::Align align = layout->getABITypeAlign(varTy);
    auto *storage = funcBuilder.CreateAlloca(varTy, nullptr, name);
    llvm::cast<llvm::AllocaInst>(storage)->setAlignment(align);

    if (!initVal) initVal = llvm::Constant::getNullValue(varTy);

    // Nullable boxing
    logInternal("Is the type nullable: " + std::to_string(sym->type().type.isNull));
    if (sym->type().type.isNull && !llvm::isa<llvm::StructType>(varTy)) {
        llvm::StructType *stTy = llvm::cast<llvm::StructType>(varTy);
        llvm::Value *boxed = llvm::UndefValue::get(stTy);
        boxed = funcBuilder.CreateInsertValue(
            boxed, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1), 0);
        boxed = funcBuilder.CreateInsertValue(boxed, initVal, 1);
        initVal = boxed;
    }

    auto *store = funcBuilder.CreateStore(initVal, storage);
    store->setAlignment(align);
    if (sym->storage().isVolatile) store->setVolatile(true);
    return storage;
}

llvm::Value *IRGenerator::generateArrayStorage(VariableDeclaration *decl,
                                               std::shared_ptr<SymbolInfo> sym,
                                               const std::string &name) {
    auto type_modifier = dynamic_cast<TypeModifier *>(decl->modified_type.get());
    llvm::Type *elemTy = getLLVMType(semantics.getArrayElementType(sym->type().type));


    // Determine Allocation Count (Total Elements)
    llvm::Value *allocationCount = nullptr;
    if (type_modifier && !type_modifier->dimensions.empty()) {
        allocationCount = funcBuilder.getInt64(1);
        TypeModifier *currentModifier = type_modifier;

        while (currentModifier) {
            for (const auto &lenExpr : currentModifier->dimensions) {
                llvm::Value *dim = generateExpression(lenExpr.get());
                dim = funcBuilder.CreateIntCast(dim, funcBuilder.getInt64Ty(), false);
                allocationCount = funcBuilder.CreateMul(allocationCount, dim, "total_elements");
            }
            // Dig into the next layer
            currentModifier = dynamic_cast<TypeModifier *>(currentModifier->inner_modifier.get());
        }

        // Override with Layout info if constant (to ensure consistency)
        if (sym->codegen().componentSize > 0) {
            uint64_t totalElems = sym->codegen().componentSize / layout->getTypeAllocSize(elemTy);
            allocationCount = funcBuilder.getInt64(totalElems);
        }
    } else if (decl->initializer) {
        auto *arrLit = dynamic_cast<ArrayLiteral *>(decl->initializer.get());
        if (arrLit) {
            size_t flat = getFlatCount(arrLit);
            allocationCount = funcBuilder.getInt64(flat);
        } else {
            uint64_t totalElems = sym->codegen().componentSize / layout->getTypeAllocSize(elemTy);
            allocationCount = funcBuilder.getInt64(totalElems);
        }
    } else {
        allocationCount = funcBuilder.getInt64(0);
    }

    // Perform the actual memory allocation (Heap vs Stack)
    llvm::Value *dataPtr = nullptr;
    if (sym->storage().isHeap) {
        llvm::Value *byteSize = funcBuilder.CreateMul(
            allocationCount, funcBuilder.getInt64(layout->getTypeAllocSize(elemTy)));
        dataPtr = allocateRuntimeHeap(sym, byteSize, name);
        sym->codegen().llvmType = elemTy;
    } else {
        dataPtr = funcBuilder.CreateAlloca(elemTy, allocationCount, name + "_data");
    }

    // Wrap in storage handle (Pointer slot or Nullable Box)
    llvm::Value *storage = nullptr;
    if (sym->type().type.isNull) {
        llvm::StructType *boxTy =
            llvm::StructType::get(context, {funcBuilder.getInt1Ty(), funcBuilder.getPtrTy()});
        storage = funcBuilder.CreateAlloca(boxTy, nullptr, name + "_box");

        bool hasContent =
            decl->initializer && !dynamic_cast<NullLiteral *>(decl->initializer.get());

        llvm::Value *boxVal = llvm::UndefValue::get(boxTy);
        boxVal = funcBuilder.CreateInsertValue(boxVal, funcBuilder.getInt1(hasContent), 0);
        boxVal = funcBuilder.CreateInsertValue(boxVal, dataPtr, 1);

        auto *store = funcBuilder.CreateStore(boxVal, storage);
        if (sym->storage().isVolatile) store->setVolatile(true);
    } else {
        storage = dataPtr;
    }

    // Copy initializer content if present
    if (decl->initializer && !dynamic_cast<NullLiteral *>(decl->initializer.get())) {
        llvm::Value *srcData = generateExpression(decl->initializer.get());
        llvm::Align finalAlign = layout->getABITypeAlign(elemTy);

        // Determine byte count for memcpy
        llvm::Value *totalBytes = nullptr;
        if (sym->codegen().componentSize > 0) {
            totalBytes = funcBuilder.getInt64(sym->codegen().componentSize);
        } else {
            totalBytes = funcBuilder.CreateMul(
                allocationCount, funcBuilder.getInt64(layout->getTypeAllocSize(elemTy)));
        }

        funcBuilder.CreateMemCpy(dataPtr, finalAlign, srcData, finalAlign, totalBytes);
    }

    return storage;
}

llvm::Value *IRGenerator::generateReferenceStorage(VariableDeclaration *decl,
                                                   std::shared_ptr<SymbolInfo> sym,
                                                   const std::string &name) {
    auto targetSym = sym->relations().refereeSymbol;
    if (!targetSym) reportDevBug("Reference '" + name + "' has no target symbol", decl);

    // Get the address of the target
    llvm::Value *targetAddr = nullptr;
    
    if (targetSym->codegen().llvmValue) {
        // For heap scalars, llvmValue is already the pointer to the data
        // For stack variables, llvmValue is the alloca pointer
        targetAddr = targetSym->codegen().llvmValue;
    } else {
        // Target not yet materialized, generate address from initializer
        targetAddr = generateAddress(decl->initializer.get());
    }
    
    // Store the target address directly as the reference's value
    // No extra allocation needed - the reference is just an alias
    sym->codegen().llvmValue = targetAddr;

    
    return targetAddr;
}

llvm::Value *IRGenerator::generateComponentStorage(VariableDeclaration *decl,
                                                   std::shared_ptr<SymbolInfo> sym,
                                                   const std::string &name) {
    auto *structTy =
        llvm::dyn_cast<llvm::StructType>(componentTypes[sym->type().type.resolvedName]);
    return generateComponentInit(decl, sym, structTy, sym->storage().isHeap);
}

//_______________________HELPERS______________________________________
void IRGenerator::generateGlobalScalarLet(std::shared_ptr<SymbolInfo> sym,
                                          const std::string &letName, Node *value) {
    llvm::Type *varType = getLLVMType(sym->type().type);

    llvm::Constant *init = nullptr;
    if (value) {
        llvm::Value *val = generateExpression(value);
        init = llvm::dyn_cast<llvm::Constant>(val);

        if (!init) {
            reportDevBug("Global '" + letName + "' must be initialized with a constant expression",
                         value);
        }
    } else if (semantics.customTypesTable.count(sym->type().type.resolvedName)) {
        init = generateGlobalRecordDefaults(sym->type().type.resolvedName);
    } else {
        init = llvm::Constant::getNullValue(varType);
    }

    auto *g = new llvm::GlobalVariable(
        *module, varType,
        sym->storage().isConstant,  // should be true unless semantics has screwed me
        llvm::GlobalValue::InternalLinkage, init, letName);

    sym->codegen().llvmValue = g;
    sym->codegen().llvmType = varType;
}

llvm::Constant *IRGenerator::generateGlobalRecordDefaults(const std::string &typeName) {
    auto *structTy = llvm::cast<llvm::StructType>(llvmCustomTypes[typeName]);
    auto compInfo = semantics.customTypesTable[typeName];

    std::vector<llvm::Constant *> fieldConstants(compInfo->members.size());

    for (const auto &[name, memInfo] : compInfo->members) {
        auto varDecl = dynamic_cast<VariableDeclaration *>(memInfo->node);
        llvm::Constant *fieldInit = nullptr;

        if (varDecl && varDecl->initializer) {
            fieldInit =
                llvm::dyn_cast<llvm::Constant>(generateExpression(varDecl->initializer.get()));
        }

        if (!fieldInit) {
            // No default, zero initialize
            fieldInit = llvm::Constant::getNullValue(getLLVMType(memInfo->type));
        }

        fieldConstants[memInfo->memberIndex] = fieldInit;
    }

    return llvm::ConstantStruct::get(structTy, fieldConstants);
}

llvm::Value *IRGenerator::generateComponentInit(VariableDeclaration *declaration,
                                                std::shared_ptr<SymbolInfo> sym,
                                                llvm::StructType *structTy, bool isHeap) {
    const std::string &name = declaration->var_name->expression.TokenLiteral;

    llvm::Value *instancePtr = nullptr;
    if (isHeap) {
        instancePtr = allocateDynamicHeapStorage(sym, name);
    } else {
        const llvm::DataLayout &DL = module->getDataLayout();
        llvm::Align finalAlign = DL.getPrefTypeAlign(structTy);

        auto *allocaInst = funcBuilder.CreateAlloca(structTy, nullptr, name + ".stack");
        allocaInst->setAlignment(finalAlign);
        instancePtr = allocaInst;

        auto *storeInst =
            funcBuilder.CreateStore(llvm::Constant::getNullValue(structTy), instancePtr);
        storeInst->setAlignment(finalAlign);
    }

    auto compTypeIt = semantics.customTypesTable.find(semantics.getBaseTypeName(sym->type().type));
    if (compTypeIt != semantics.customTypesTable.end()) {
        for (const auto &[fieldName, memInfo] : compTypeIt->second->members) {
            auto varDecl = dynamic_cast<VariableDeclaration *>(memInfo->node);
            if (!varDecl || !varDecl->initializer) continue;

            llvm::Value *initVal = generateExpression(varDecl->initializer.get());
            llvm::Value *fieldPtr = funcBuilder.CreateStructGEP(
                structTy, instancePtr, memInfo->memberIndex, fieldName + "_field");
            funcBuilder.CreateStore(initVal, fieldPtr);
        }
    }

    auto newExpr = declaration->initializer
                       ? dynamic_cast<NewComponentExpression *>(declaration->initializer.get())
                       : nullptr;

    if (newExpr) {
        std::string initFnName = semantics.getBaseTypeName(sym->type().type) + "_init";
        if (llvm::Function *initFn = module->getFunction(initFnName)) {
            std::vector<llvm::Value *> initArgs;
            initArgs.push_back(instancePtr);
            for (auto &arg : newExpr->arguments) initArgs.push_back(generateExpression(arg.get()));
            funcBuilder.CreateCall(initFn, initArgs);
        }
    }

    return instancePtr;
}

// Dynamic heap storage
llvm::Value *IRGenerator::allocateDynamicHeapStorage(std::shared_ptr<SymbolInfo> sym,
                                                     const std::string &varName) {
    const std::string &allocatorTypeName = sym->storage().allocType;

    auto it = semantics.allocatorMap.find(allocatorTypeName);
    if (it == semantics.allocatorMap.end()) {
        reportDevBug("Unknown allocator type '" + allocatorTypeName + "'", nullptr);
    }

    auto handle = it->second;
    auto allocatorName = handle.allocateName;

    // Find the function in the module(it was created in registerAllocators)
    llvm::Function *allocFunc = module->getFunction(allocatorName);
    if (!allocFunc) {
        reportDevBug("Function not found for allocator '" + allocatorName + "'", nullptr);
    }

    size_t allocSize = sym->codegen().componentSize;  // Get the size for the allocation
    llvm::Value *sizeArg = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), allocSize);

    llvm::Value *rawPtr = funcBuilder.CreateCall(allocFunc, {sizeArg}, varName + "_heap_raw");

    llvm::Type *baseType = getLLVMType(sym->type().type);

    return funcBuilder.CreateBitCast(rawPtr, baseType->getPointerTo(), varName + "_ptr");
}

llvm::Value *IRGenerator::allocateRuntimeHeap(std::shared_ptr<SymbolInfo> sym,
                                              llvm::Value *runtimeSize,
                                              const std::string &varName) {
    const std::string &allocatorTypeName = sym->storage().allocType;
    auto it = semantics.allocatorMap.find(allocatorTypeName);
    auto handle = it->second;

    llvm::Function *allocFunc = module->getFunction(handle.allocateName);

    // Pass the RUNTIME calculated size to the custom allocator
    llvm::Value *rawPtr = funcBuilder.CreateCall(allocFunc, {runtimeSize}, varName + "_heap_raw");

    llvm::Type *baseType = getLLVMType(sym->type().type);
    return funcBuilder.CreateBitCast(rawPtr, baseType->getPointerTo(), varName + "_ptr");
}

bool IRGenerator::isComponentType(const std::string &name) { return componentTypes.count(name); }
