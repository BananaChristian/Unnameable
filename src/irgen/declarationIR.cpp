#include "irgen.hpp"
// Dheap statement IR generator
void IRGenerator::generateDheapStatement(Node *node)
{
    auto dheapStmt = dynamic_cast<DheapStatement *>(node);
    if (!dheapStmt)
        return;

    generateStatement(dheapStmt->stmt.get());
}

// Let statement IR generator function
void IRGenerator::generateLetStatement(Node *node)
{
    // VALIDATION AND EXTRACTION
    auto *letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        throw std::runtime_error("Invalid let statement");

    const std::string letName = letStmt->ident_token.TokenLiteral;
    std::cout << "[DEBUG] Generating let statement for variable '" << letName << "'\n";

    auto metaIt = semantics.metaData.find(letStmt);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("No let metadata for '" + letName + "'");

    auto sym = metaIt->second;
    if (sym->hasError)
        throw std::runtime_error("Semantic error detected for '" + letName + "'");

    llvm::StructType *structTy = nullptr;
    bool isHeap = letStmt->isHeap;
    bool isDheap = letStmt->isDheap;
    bool isComponent = false;

    // Detect component type
    auto compIt = componentTypes.find(sym->type.resolvedName);
    if (compIt != componentTypes.end())
    {
        isComponent = true;
        structTy = llvm::dyn_cast<llvm::StructType>(compIt->second); // Populate the struct type
    }

    // If there's no current insert block, handle global(GLOBAL SCOPE)
    if (isGlobalScope)
    {
        std::cout << "[DEBUG] No insert block (global scope) for let '" << letName << "'\n";
        if (isComponent && isHeap)
        {
            generateGlobalComponentHeapInit(letStmt, sym, letName, structTy);
        }
        else if (isHeap)
        {
            generateGlobalHeapLet(letStmt, sym, letName);
        }

        else
        {
            auto valExpr = dynamic_cast<Expression *>(letStmt->value.get());
            generateGlobalScalarLet(sym, letName, valExpr);
        }
        return;
    }

    // LOCAL SCOPE
    llvm::Value *storage = nullptr;

    // For non-components: compute init value (scalar or other) this is for non heap
    llvm::Value *initVal = nullptr;
    if (!isComponent)
    {
        if (letStmt->value)
            initVal = generateExpression(letStmt->value.get());
        else
            initVal = llvm::Constant::getNullValue(getLLVMType(sym->type));
    }

    llvm::Value *constructedPtr = nullptr;
    // If it is a component
    if (isComponent)
    {
        storage = generateComponentInit(letStmt, sym, structTy, isHeap); // It will handle its own heap business
        if (!storage)
        {
            throw std::runtime_error("Component allocation failed for '" + letName + "'");
        }
    }
    else if (isHeap) // Incase the value is heap raised
    {
        // Generate the sage_alloc and allocate the value on the heap
        storage = allocateHeapStorage(sym, letName, nullptr);
        llvm::Value *initVal = letStmt->value ? generateExpression(letStmt->value.get()) : llvm::Constant::getNullValue(getLLVMType(sym->type));

        // Store initial value into the heap-allocated storage
        funcBuilder.CreateStore(initVal, storage);
    }
    else if (isDheap) // If it is dynamic heap raise value
    {
        storage = allocateDynamicHeapStorage(sym, letName);
        llvm::Value *initVal = letStmt->value ? generateExpression(letStmt->value.get()) : llvm::Constant::getNullValue(getLLVMType(sym->type));

        funcBuilder.CreateStore(initVal, storage);
    }
    else // If it isnt a component or a heap raised value
    {

        // scalar / normal type
        llvm::Type *varTy = getLLVMType(sym->type);
        if (varTy->isStructTy() && llvm::cast<llvm::StructType>(varTy)->isOpaque())
        {
            throw std::runtime_error("Logical Error: Trying to allocate Opaque type '" +
                                     sym->type.resolvedName + "'. Did you forget to set the body?");
        }
        printf("[DEBUG] Type Context: %p\n", (void *)&varTy->getContext());
        printf("[DEBUG] Module Context: %p\n", (void *)&module->getContext());
        printf("[DEBUG] Builder Context: %p\n", (void *)&funcBuilder.getContext());

        if (&varTy->getContext() != &module->getContext())
        {
            printf("[CRITICAL] CONTEXT MISMATCH DETECTED!\n");
        }

        varTy->print(llvm::errs());
        llvm::errs() << "\n";

        storage = funcBuilder.CreateAlloca(varTy, nullptr, letName);
        // store initial value if we have one
        if (initVal)
            funcBuilder.CreateStore(initVal, storage);
        else
            funcBuilder.CreateStore(llvm::Constant::getNullValue(varTy), storage);
    }

    if (!storage)
        throw std::runtime_error("No storage allocated for let '" + letName + "'");

    // Update symbol metadata with storage and type
    sym->llvmValue = storage;
    sym->llvmType = (isComponent && structTy) ? structTy : getLLVMType(sym->type);

    // HEAP CLEANUP FOR DEAD LOCALS
    if (letStmt->isHeap)
    {
        Node *lastUse = sym->lastUseNode ? sym->lastUseNode : letStmt;
        if (letStmt == lastUse && sym->refCount == 0)
        {
            freeHeapStorage(sym->componentSize, sym->alignment.value(), letName);
            std::cout << "[DEBUG] Immediately freed dead heap variable '" << letName << "'\n";
        }
    }
    else if (letStmt->isDheap)
    {
        Node *lastUse = sym->lastUseNode ? sym->lastUseNode : letStmt;
        if (letStmt == lastUse && sym->refCount == 0)
        {
            freeDynamicHeapStorage(sym);
            std::cout << "[DEBUG] Immediately freed dead dynamic heap variable '" << letName << "'\n";
        }
    }

    std::cout << "[DEBUG] Local let statement '" << letName << "' fully processed. storage=" << storage << "\n";
}

void IRGenerator::generateGlobalHeapLet(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, const std::string &letName)
{
    std::cout << "Let statement was heap raised\n";

    // If sage_init hasnt been called
    if (!isSageInitCalled)
        generateSageInitCall();

    // Create a global raw pointer slot initialized to null
    llvm::Type *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
    llvm::GlobalVariable *globalPtr = new llvm::GlobalVariable(
        *module,
        i8PtrTy,
        false, // Not constant
        llvm::GlobalValue::InternalLinkage,
        llvm::ConstantPointerNull::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)),
        letName + "_rawptr");

    // Validate and register the global pointer
    if (!llvm::dyn_cast<llvm::GlobalVariable>(globalPtr))
    {
        std::cerr << "FATAL BUG: sym->llvmValue overwritten after GlobalVariable creation for '" << letName << "'.\n";
        throw std::runtime_error("Symbol table corruption.");
    }
    else
    {
        std::cout << "[DEBUG] Global heap symbol '" << letName << "' registered as GlobalVariable.\n";
    }
    sym->llvmValue = globalPtr;

    // Build allocation and initialization in the heap-init function (one-time setup)
    if (!heapInitFnEntry)
    {
        throw std::runtime_error("Global init entry doesnt exist");
    }
    llvm::IRBuilder<> heapBuilder(heapInitFnEntry);

    if (heapInitFnEntry->getTerminator())
    {
        // If the block is already terminated (which it shouldn't be here),
        // we need to insert before the terminator.
        heapBuilder.SetInsertPoint(heapInitFnEntry->getTerminator());
    }
    else
    {
        heapBuilder.SetInsertPoint(heapInitFnEntry);
    }

    // Allocate raw memory via sage_alloc
    llvm::FunctionCallee sageAllocFn = module->getOrInsertFunction(
        "sage_alloc",
        i8PtrTy,
        llvm::Type::getInt64Ty(context));
    llvm::Value *allocSize = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->componentSize);
    llvm::Value *alignment = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->alignment.value());
    llvm::Value *tmpPtr = heapBuilder.CreateCall(sageAllocFn, {allocSize}, letName + "_alloc");

    // Store the raw pointer in the global slot
    heapBuilder.CreateStore(tmpPtr, globalPtr);

    // Cast to typed pointer and store initial value
    llvm::Type *elemTy = getLLVMType(sym->type);
    sym->llvmType = elemTy;
    llvm::Value *typedPtr = heapBuilder.CreateBitCast(tmpPtr, elemTy->getPointerTo(), letName + "_typed");
    llvm::Value *initVal = generateExpression(letStmt->value.get());
    heapBuilder.CreateStore(initVal, typedPtr);

    if (letStmt->isHeap)
    {
        if ((sym->lastUseNode == letStmt) && (sym->refCount == 0))
        {
            llvm::FunctionCallee sageFree = module->getOrInsertFunction(
                "sage_free",
                llvm::Type::getVoidTy(context),
                llvm::Type::getInt64Ty(context));

            heapBuilder.CreateCall(
                sageFree,
                {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->componentSize)},
                letName + "_sage_free");
        }
    }
}

void IRGenerator::generateGlobalScalarLet(std::shared_ptr<SymbolInfo> sym, const std::string &letName, Expression *value)
{
    llvm::Type *varType = getLLVMType(sym->type);
    auto generateConstantLiteral = [&](ResolvedType type) -> llvm::Constant *
    {
        llvm::Value *val = generateExpression(value);
        if (!val)
            throw std::runtime_error("Null value returned from expression during global scalar init");

        switch (type.kind)
        {
        case DataType::I32:
        {
            if (auto constInt = llvm::dyn_cast<llvm::ConstantInt>(val))
            {
                return llvm::ConstantInt::get(varType, constInt->getValue());
            }
            else
            {
                throw std::runtime_error("Expected ConstantInt for INTEGER literal");
            }
        }

        case DataType::STRING:
        {
            if (auto constStr = llvm::dyn_cast<llvm::Constant>(val))
            {
                return constStr;
            }
            else
            {
                throw std::runtime_error("Expected  for STRING literal");
            }
        }

        default:
            throw std::runtime_error("Unsupported literal type in global scalar initialization");
        }
    };

    llvm::Constant *init = nullptr;
    if (sym->isInitialized)
    {
        init = generateConstantLiteral(sym->type);
    }
    else
    {
        init = llvm::Constant::getNullValue(varType);
    }

    bool isConst = sym->isConstant;
    auto *g = new llvm::GlobalVariable(
        *module,
        varType,
        isConst,
        llvm::GlobalValue::ExternalLinkage,
        init,
        letName);
    sym->llvmValue = g;
    sym->llvmType = varType;
    std::cout << "[DEBUG] Created global scalar '" << letName << "' as GlobalVariable\n";
}

void IRGenerator::generateGlobalComponentHeapInit(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, const std::string &letName, llvm::StructType *structType)
{
    if (!isSageInitCalled)
        generateSageInitCall();

    llvm::PointerType *ptrTy = structType->getPointerTo();
    llvm::Constant *initializer = llvm::ConstantPointerNull::get(ptrTy);

    // Create the global variable that will hold the HEAP address
    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        *module,
        ptrTy,
        false,                              // Is not constant
        llvm::GlobalValue::ExternalLinkage, // Use ExternalLinkage for visibility
        initializer,
        letName);

    // Register the global variable in the symbol table
    sym->llvmValue = globalVar;
    sym->llvmType = structType;

    if (!heapInitFnEntry)
    {
        throw std::runtime_error("Global init function entry doesnt exist");
    }

    llvm::IRBuilder<> heapBuilder(heapInitFnEntry);
    if (heapInitFnEntry->getTerminator())
    {
        // If the block is already terminated (which it shouldn't be here),
        // we need to insert before the terminator.
        heapBuilder.SetInsertPoint(heapInitFnEntry->getTerminator());
    }
    else
    {
        // Otherwise, insert at the end.
        heapBuilder.SetInsertPoint(heapInitFnEntry);
    }

    // Check if the initializer is present
    auto newExpr = dynamic_cast<NewComponentExpression *>(letStmt->value.get());
    if (!newExpr)
    {
        std::cout << "[GLOBAL] Warning: Global heap component '" << letName << "' declared without 'new' initializer. Skipping init code.\n";
        return;
    }

    const std::string &compName = sym->type.resolvedName;

    llvm::FunctionCallee sageAllocFn = module->getOrInsertFunction(
        "sage_alloc",
        llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
        llvm::Type::getInt64Ty(context));

    llvm::Value *allocSize = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->componentSize);
    llvm::Value *alignSize = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->alignment.value());

    llvm::Value *i8_ptr = heapBuilder.CreateCall(sageAllocFn, {allocSize}, letName + ".global.ptr.i8");
    llvm::Value *typedPtr = heapBuilder.CreateBitCast(i8_ptr, ptrTy, letName + ".global.ptr.typed");
    typedPtr->setName(letName + ".global.ptr.typed");

    // Constructor call
    if (llvm::Function *initFn = module->getFunction(compName + "_init"))
    {
        llvm::Type *expected = initFn->getFunctionType()->getParamType(0);

        llvm::Value *callPtr = typedPtr;
        if (typedPtr->getType() != expected)
        {
            callPtr = heapBuilder.CreateBitCast(typedPtr, expected, letName + ".for_call_cast");
            callPtr->setName(letName + ".for_call_cast");
            llvm::errs() << "Inserted call-cast: ";
            callPtr->getType()->print(llvm::errs());
            llvm::errs() << "\n";
        }

        std::vector<llvm::Value *> initArgs;
        initArgs.push_back(callPtr); // self pointer

        for (auto &arg : newExpr->arguments)
        {
            llvm::Value *val = generateExpression(arg.get());
            if (!val)
                throw std::runtime_error("Failed to generate argument for new " + compName);
            initArgs.push_back(val);
        }

        heapBuilder.CreateCall(initFn, initArgs, letName + ".ctor_call");
    }

    llvm::Type *globalStoredTy = globalVar->getValueType(); // should be ptrTy, but be explicit
    llvm::Value *toStore = typedPtr;
    if (typedPtr->getType() != globalStoredTy)
    {
        toStore = heapBuilder.CreateBitCast(typedPtr, globalStoredTy, letName + ".store_cast");
        toStore->setName(letName + ".store_cast");
    }
    heapBuilder.CreateStore(toStore, globalVar);

    if (letStmt->isHeap)
    {
        if ((sym->lastUseNode == letStmt) && (sym->refCount == 0))
        {
            llvm::FunctionCallee sageFree = module->getOrInsertFunction(
                "sage_free",
                llvm::Type::getVoidTy(context));

            heapBuilder.CreateCall(
                sageFree
                /*letName + "_sage_free"*/);
        }
    }
}

llvm::Value *IRGenerator::generateComponentInit(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, llvm::StructType *structTy, bool isHeap)
{
    std::string letName = letStmt->ident_token.TokenLiteral;

    // Allocate (Even if no 'new' exists)
    llvm::Value *instancePtr = nullptr;
    if (isHeap)
    {
        if (!isSageInitCalled)
            generateSageInitCall();
        instancePtr = allocateHeapStorage(sym, letName, structTy);
    }
    else
    {
        const llvm::DataLayout &DL = module->getDataLayout();

        // Use Preferred Alignment
        llvm::Align finalAlign = DL.getPrefTypeAlign(structTy);

        // Create the alloca and assign it to instancePtr
        auto *allocaInst = funcBuilder.CreateAlloca(structTy, nullptr, letName + ".stack");
        allocaInst->setAlignment(finalAlign);
        instancePtr = allocaInst; // instancePtr is just the alloca result

        // Use instancePtr for the store
        auto *storeInst = funcBuilder.CreateStore(llvm::Constant::getNullValue(structTy), instancePtr);
        storeInst->setAlignment(finalAlign);
    }

    // Check if the initializer exists
    auto newExpr = letStmt->value ? dynamic_cast<NewComponentExpression *>(letStmt->value.get()) : nullptr;

    // Apply default field initializers(from the component definition)
    auto compTypeIt = semantics.customTypesTable.find(sym->type.resolvedName);
    if (compTypeIt != semantics.customTypesTable.end())
    {
        for (const auto &[name, memInfo] : compTypeIt->second->members)
        {
            auto letNode = dynamic_cast<LetStatement *>(memInfo->node);
            if (!letNode || !letNode->value)
                continue;

            llvm::Value *initVal = generateExpression(letNode->value.get());
            llvm::Value *fieldPtr = funcBuilder.CreateStructGEP(structTy, instancePtr, memInfo->memberIndex, name + "_field");
            funcBuilder.CreateStore(initVal, fieldPtr);
        }
    }

    // Only call init construtor if new was used
    if (newExpr)
    {
        if (llvm::Function *initFn = module->getFunction(sym->type.resolvedName + "_init"))
        {
            std::vector<llvm::Value *> initArgs;
            initArgs.push_back(instancePtr);
            for (auto &arg : newExpr->arguments)
            {
                initArgs.push_back(generateExpression(arg.get()));
            }
            funcBuilder.CreateCall(initFn, initArgs);
        }
    }

    return instancePtr;
}

// Sage Heap storage
llvm::Value *IRGenerator::allocateHeapStorage(std::shared_ptr<SymbolInfo> sym, const std::string &letName, llvm::StructType *structTy)
{
    std::cout << "Let statement was heap raised\n";
    // If sage_init hasnt been called
    if (!isSageInitCalled)
        generateSageInitCall();

    uint64_t allocSize = sym->componentSize;
    uint64_t alignSize = sym->alignment.value();
    llvm::Type *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
    llvm::FunctionCallee sageAlloc = module->getOrInsertFunction(
        "sage_alloc",
        i8PtrTy,
        llvm::Type::getInt64Ty(context),
        llvm::Type::getInt64Ty(context));

    llvm::Value *rawPtr = funcBuilder.CreateCall(
        sageAlloc,
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), allocSize),
         llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), alignSize)},
        letName + "_sage_rawptr");

    llvm::Type *targetPtrTy = structTy ? structTy->getPointerTo() : getLLVMType(sym->type)->getPointerTo();
    return funcBuilder.CreateBitCast(rawPtr, targetPtrTy, letName + "_heap_ptr");
}

void IRGenerator::freeHeapStorage(uint64_t size, uint64_t alignSize, const std::string &letName)
{
    llvm::FunctionCallee sageFree = module->getOrInsertFunction(
        "sage_free",
        llvm::Type::getVoidTy(context),
        llvm::Type::getInt64Ty(context),
        llvm::Type::getInt64Ty(context));

    funcBuilder.CreateCall(
        sageFree,
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size),
         llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), alignSize)},
        letName + "_sage_free");
}

// Dheap storage
llvm::Value *IRGenerator::allocateDynamicHeapStorage(std::shared_ptr<SymbolInfo> sym, const std::string &varName)
{
    std::cout << "Let statement was dynamic heap raised\n";

    const std::string &allocatorTypeName = sym->allocType;

    auto it = semantics.allocatorMap.find(allocatorTypeName);
    if (it == semantics.allocatorMap.end())
        throw std::runtime_error("Unknown allocator type '" + allocatorTypeName + "'");

    auto handle = it->second;
    auto allocatorName = handle.allocateName;

    // Find the function in the module(it was created in registerAllocators)
    llvm::Function *allocFunc = module->getFunction(allocatorName);
    if (!allocFunc)
        throw std::runtime_error("Function not found for allocator: " + allocatorName);

    size_t allocSize = sym->componentSize; // Get the size for the allocation
    llvm::Value *sizeArg = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), allocSize);

    llvm::Value *rawPtr = funcBuilder.CreateCall(allocFunc, {sizeArg}, varName + "_dheap_raw");

    llvm::Type *baseType = getLLVMType(sym->type);

    return funcBuilder.CreateBitCast(rawPtr, baseType->getPointerTo(), varName + "_ptr");
}

void IRGenerator::freeDynamicHeapStorage(std::shared_ptr<SymbolInfo> sym)
{
    const std::string &allocatorTypeName = sym->allocType;
    auto it = semantics.allocatorMap.find(allocatorTypeName);
    if (it == semantics.allocatorMap.end())
        throw std::runtime_error("Unknown allocator type '" + allocatorTypeName + "'");

    auto handle = it->second;
    auto freeName = handle.freeName;

    llvm::Function *freeFunc = module->getFunction(freeName);
    if (!freeFunc)
        throw std::runtime_error("Function not found for free: " + freeName);

    llvm::Value *ptrToFree = sym->llvmValue;
    if (!ptrToFree)
        throw std::runtime_error("No LLVM value stored in symbol for freeing");

    llvm::Type *expectedTy = freeFunc->getFunctionType()->getParamType(0);

    llvm::Value *castPtr = funcBuilder.CreatePointerCast(ptrToFree, expectedTy);

    funcBuilder.CreateCall(freeFunc, {castPtr});
}