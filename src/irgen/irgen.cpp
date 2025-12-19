#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include "llvm/TargetParser/Host.h"

#include <llvm/ADT/STLExtras.h>
#include <llvm/CodeGen/TargetPassConfig.h>
#include <inttypes.h>

#include "irgen.hpp"
#include "ast.hpp"

#include <iostream>
#define CPPREST_FORCE_REBUILD

IRGenerator::IRGenerator(Semantics &semantics, size_t totalHeap)
    : semantics(semantics), totalHeapSize(totalHeap), context(), globalBuilder(context), funcBuilder(context), module(std::make_unique<llvm::Module>("unnameable", context))
{
    registerGeneratorFunctions();
    registerExpressionGeneratorFunctions();
    registerAddressGeneratorFunctions();

    declareCustomTypes();
    declareImportedTypes();
    declareImportedSeals();

    // Declare external allocator functions so IR can call them
    llvm::FunctionType *initType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context),    // returns void
        {llvm::Type::getInt64Ty(context)}, // takes size_t (64-bit int)
        false);

    llvm::PointerType *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);

    llvm::FunctionType *allocType = llvm::FunctionType::get(
        i8PtrTy, // returns void*
        {
            llvm::Type::getInt64Ty(context), // takes size_t for component size
            llvm::Type::getInt64Ty(context)  // takes size_t for alignment
        },
        false);

    llvm::FunctionType *freeType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context), // returns void
        false);

    llvm::FunctionType *destroyType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context), {}, false); // void()

    // Add them to the module
    module->getOrInsertFunction("sage_init", initType);
    module->getOrInsertFunction("sage_alloc", allocType);
    module->getOrInsertFunction("sage_free", freeType);
    module->getOrInsertFunction("sage_destroy", destroyType);

    llvm::Function *globalInitFn = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), false),
        llvm::GlobalValue::InternalLinkage,
        "global_init",
        module.get());

    heapInitFnEntry = llvm::BasicBlock::Create(context, "entry", globalInitFn);
}

// MAIN GENERATOR FUNCTION
void IRGenerator::generate(const std::vector<std::unique_ptr<Node>> &program)
{
    // Generate program body
    for (const auto &node : program)
    {
        generateStatement(node.get());
    }

    // Finish off global heap init if it exists
    if (heapInitFnEntry)
    {
        llvm::IRBuilder<> globalInitBuilder(heapInitFnEntry);
        globalInitBuilder.CreateRetVoid();
    }

    // Look for the main function if the user qualified the function
    llvm::Function *mainFn = module->getFunction("main");
    if (!mainFn)
    {
        if (mainMarker)
        {
            throw std::runtime_error(
                "You marked this file with qualified this file to be main but did not define a main function");
        }
        else
        {
            // No main expected, skip
            return;
        }
    }

    llvm::BasicBlock &entryBlock = mainFn->getEntryBlock();
    llvm::IRBuilder<> tmpBuilder(&entryBlock, entryBlock.begin());

    llvm::Function *globalHeapFn = module->getFunction("global_init");
    tmpBuilder.CreateCall(globalHeapFn);
}

// MAIN GENERATOR FUNCTION FOR EXPRESSION
//  Main Expression generator helper function
llvm::Value *IRGenerator::generateExpression(Node *node)
{
    if (!node)
    {
        std::cout << "[IRGEN] NULL node!\n";
        return nullptr;
    }

    auto exprIt = expressionGeneratorsMap.find(typeid(*node));
    if (exprIt == expressionGeneratorsMap.end())
    {
        throw std::runtime_error("Could not find expression type IR generator: " + node->toString());
    }

    return (this->*exprIt->second)(node);
}

// Main L-value generator helper functions
llvm::Value *IRGenerator::generateAddress(Node *node)
{
    if (!node)
    {
        std::cout << "[IRGEN] Null node! \n";
    }
    auto addrIt = addressGeneratorsMap.find(typeid(*node));
    if (addrIt == addressGeneratorsMap.end())
    {
        throw std::runtime_error("Could not find address generator for: " + node->toString());
    }

    return (this->*addrIt->second)(node);
}

// GENERATOR FUNCTIONS
void IRGenerator::generateStatement(Node *node)
{
    auto generatorIt = generatorFunctionsMap.find(typeid(*node));
    if (generatorIt == generatorFunctionsMap.end())
    {
        std::cout << "Failed to find statement IR generator for : " << node->toString() << "\n";
        return;
    }
    (this->*generatorIt->second)(node);
}
// STATEMENT GENERATOR FUNCTIONS
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
        case DataType::INTEGER:
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

        //Create the alloca and assign it to instancePtr
        auto *allocaInst = funcBuilder.CreateAlloca(structTy, nullptr, letName + ".stack");
        allocaInst->setAlignment(finalAlign);
        instancePtr = allocaInst; //instancePtr is just the alloca result

        //Use instancePtr for the store
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

// Reference statement IR generator
void IRGenerator::generateReferenceStatement(Node *node)
{
    auto refStmt = dynamic_cast<ReferenceStatement *>(node);
    if (!refStmt)
        throw std::runtime_error("Invalid reference statement");

    const auto &refName = refStmt->referer->expression.TokenLiteral;
    const auto &refereeName = semantics.extractIdentifierName(refStmt->referee.get());

    // Lookup metadata
    auto metaIt = semantics.metaData.find(refStmt);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Failed to find reference metaData for '" + refName + "'");

    auto refSym = metaIt->second;
    auto targetSym = refSym->refereeSymbol;
    if (!targetSym)
        throw std::runtime_error("Reference '" + refName + "' has no target symbol");

    if (targetSym->hasError)
        throw std::runtime_error("Semantic error detected on reference target '" + refereeName + "'");

    // Generate the LLVM pointer to the actual value, not the pointer variable
    llvm::Value *targetAddress = nullptr;
    if (targetSym->llvmValue)
    {
        // If the target has already generated LLVM value, ensure we store the **address**
        if (targetSym->llvmValue->getType()->isPointerTy())
        {
            targetAddress = targetSym->llvmValue;
        }
        else
        {
            // For scalars, take their address
            targetAddress = funcBuilder.CreateAlloca(targetSym->llvmValue->getType(), nullptr, refereeName + "_addr");
            funcBuilder.CreateStore(targetSym->llvmValue, targetAddress);
        }
    }
    else
    {
        // If LLVM value not yet generated, compute the address via generateAddress
        targetAddress = generateAddress(refStmt->referee.get());
    }

    if (!targetAddress || !targetAddress->getType()->isPointerTy())
        throw std::runtime_error("Failed to resolve LLVM address for reference '" + refName + "'");

    // The reference itself holds the pointer to the target
    refSym->llvmValue = targetAddress;
}

// Pointer statement IR generator function
void IRGenerator::generatePointerStatement(Node *node)
{
    auto ptrStmt = dynamic_cast<PointerStatement *>(node);
    if (!ptrStmt)
        throw std::runtime_error("Invalid pointer statement");

    auto ptrName = ptrStmt->name->expression.TokenLiteral;

    // Getting the pointer metaData
    auto metaIt = semantics.metaData.find(ptrStmt);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Missing pointer statement metaData for '" + ptrName + "'");

    auto ptrSym = metaIt->second;
    if (!ptrSym)
        throw std::runtime_error("Undefined pointer '" + ptrName + "'");

    if (ptrSym->hasError)
        throw std::runtime_error("Semantic error detected ");

    // Getting the pointer llvm type
    llvm::Type *ptrType = getLLVMType(ptrSym->type);
    if (!ptrType)
        throw std::runtime_error("Failed to get LLVM Type for '" + ptrName + "'");

    llvm::Type *ptrStorageType = ptrType->getPointerTo();

    std::cout << "[IR DEBUG] Pointer type: " << ptrSym->type.resolvedName << "\n";

    llvm::Value *initVal = ptrStmt->value ? generateExpression(ptrStmt->value.get())
                                          : llvm::Constant::getNullValue(ptrType);

    if (!initVal)
        throw std::runtime_error("No init value");

    llvm::Value *storagePtr = nullptr;
    // If in global scope
    if (!funcBuilder.GetInsertBlock())
    {
        llvm::Constant *globalInit = llvm::dyn_cast<llvm::Constant>(initVal);
        if (!globalInit)
        {
            // This happens if the target (&X) is not known at compile time,
            // which should only happen if X is not also global.
            // For now, let me assume global targets are constants.
            throw std::runtime_error("Global pointer initializer must be a constant address.");
        }

        llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
            *module,
            ptrStorageType,
            false,
            llvm::GlobalValue::InternalLinkage,
            globalInit,
            ptrName);

        storagePtr = globalVar;
    }
    else
    {

        llvm::Function *fn = funcBuilder.GetInsertBlock()->getParent();
        llvm::IRBuilder<> entryBuilder(&fn->getEntryBlock(), fn->getEntryBlock().begin());

        storagePtr = entryBuilder.CreateAlloca(ptrStorageType, nullptr, ptrName);
        if (!storagePtr)
            throw std::runtime_error("Failed to allocate local pointer on call stack");

        funcBuilder.CreateStore(initVal, storagePtr);
    }

    ptrSym->llvmValue = storagePtr;
    ptrSym->llvmType = ptrType;

    std::cout << "Exited pointer statement generator\n";
}

void IRGenerator::generateArrayStatement(Node *node)
{
    auto arrStmt = dynamic_cast<ArrayStatement *>(node);
    if (!arrStmt)
        return;

    auto arrIt = semantics.metaData.find(arrStmt);
    if (arrIt == semantics.metaData.end())
        throw std::runtime_error("Failed to find array statement metaData");

    auto arrName = arrStmt->identifier->expression.TokenLiteral;
    auto sym = arrIt->second;
    auto type = sym->type;
    llvm::Type *elemTy = getLLVMType(type);

    llvm::AllocaInst *allocaInst = nullptr;

    // If the array literal has been given
    if (arrStmt->array_content)
    {
        // Get ArrayTypeInfo from semantics
        auto arrInfo = semantics.getArrayTypeInfo(arrStmt->array_content.get());
        const auto &constantSizes = arrInfo.sizePerDimension;

        // Build nested ArrayType
        llvm::Type *arrayTy = elemTy;
        for (auto it = constantSizes.rbegin(); it != constantSizes.rend(); ++it)
            arrayTy = llvm::ArrayType::get(arrayTy, *it);

        // Allocate array on the stack
        allocaInst = funcBuilder.CreateAlloca(arrayTy, nullptr, arrName);

        // Generate the literal as a proper typed value
        llvm::Constant *literalVal = llvm::cast<llvm::Constant>(generateArrayLiteral(arrStmt->array_content.get()));

        // Create a Read-Only Global Variable from the constant literal
        llvm::GlobalVariable *globalInit = createGlobalArrayConstant(literalVal);

        llvm::DataLayout DL(module.get());
        uint64_t SizeInBytes = DL.getTypeAllocSize(allocaInst->getAllocatedType());
        llvm::Value *SizeVal = funcBuilder.getInt64(SizeInBytes);

        llvm::Value *DstPtr = funcBuilder.CreateBitCast(allocaInst, funcBuilder.getPtrTy());
        llvm::Value *SrcPtr = funcBuilder.CreateBitCast(globalInit, funcBuilder.getPtrTy());

        funcBuilder.CreateMemCpy(
            DstPtr,              // Destination (stack array)
            llvm::MaybeAlign(4), // Destination Alignment
            SrcPtr,              // Source (global constant)
            llvm::MaybeAlign(4), // Source Alignment
            SizeVal,             // Size in bytes
            false                // IsVolatile
        );
    }

    // If the array length is missing
    else if (!arrStmt->lengths.empty())
    {
        std::vector<llvm::Value *> sizes;
        for (const auto &lenExpr : arrStmt->lengths)
            sizes.push_back(generateExpression(lenExpr.get()));

        // Compute total size = product of all dimensions
        llvm::Value *totalSize = sizes[0];
        for (size_t i = 1; i < sizes.size(); ++i)
            totalSize = funcBuilder.CreateMul(totalSize, sizes[i]);

        // Allocate 1D buffer on the stack
        allocaInst = funcBuilder.CreateAlloca(elemTy, totalSize, arrName);

        // Leave uninitialized; no literal
    }
    else
    {
        throw std::runtime_error(
            "Must initialize array declaration if did not declare dimensions");
    }

    // Save pointer in the symbol for later use
    sym->llvmValue = allocaInst;
}

// While statement IR generator function
void IRGenerator::generateWhileStatement(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
    {
        throw std::runtime_error("Invalid while statement");
    }

    llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "while.cond", function);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "while.body");
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(context, "while.end");

    std::cerr << "[IR DEBUG] Creating branch to while.cond\n";
    funcBuilder.CreateBr(condBB);

    funcBuilder.SetInsertPoint(condBB);
    std::cerr << "[IR DEBUG] Generating while condition\n";
    llvm::Value *condVal = generateExpression(whileStmt->condition.get());
    if (!condVal)
    {
        throw std::runtime_error("Invalid while condition");
    }

    if (!condVal->getType()->isIntegerTy(1))
    {
        condVal = funcBuilder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "whilecond.bool");
    }

    std::cerr << "[IR DEBUG] Creating conditional branch\n";
    funcBuilder.CreateCondBr(condVal, bodyBB, endBB);

    // Append bodyBB to function
    function->insert(function->end(), bodyBB);
    funcBuilder.SetInsertPoint(bodyBB);
    std::cerr << "[IR DEBUG] Generating while body\n";
    loopBlocksStack.push_back({condBB, endBB});
    generateStatement(whileStmt->loop.get());
    loopBlocksStack.pop_back();
    std::cerr << "[IR DEBUG] Finished generating while body\n";

    if (!funcBuilder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to while.cond\n";
        funcBuilder.CreateBr(condBB);
    }
    else
    {
        std::cerr << "[IR WARNING] While body block already has terminator: " << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // Append endBB to function
    function->insert(function->end(), endBB);
    funcBuilder.SetInsertPoint(endBB);
}

// IR code gen for an if statement
void IRGenerator::generateIfStatement(Node *node)
{
    auto ifStmt = dynamic_cast<ifStatement *>(node);
    if (!ifStmt)
    {
        throw std::runtime_error("Invalid if statement");
    }

    std::cerr << "[IR DEBUG] Generating if statement\n";

    // Generation of condition for the if
    llvm::Value *condVal = generateExpression(ifStmt->condition.get());
    if (!condVal)
    {
        throw std::runtime_error("Invalid if condition");
    }

    if (!condVal->getType()->isIntegerTy(1))
    {
        condVal = funcBuilder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "ifcond.bool");
    }

    // Create basic blocks
    llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", function);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifmerge");

    // Determine the next block (first elif, else, or merge)
    llvm::BasicBlock *nextBB = nullptr;
    if (!ifStmt->elifClauses.empty())
    {
        nextBB = llvm::BasicBlock::Create(context, "elif0");
    }
    else if (ifStmt->else_result.has_value())
    {
        nextBB = llvm::BasicBlock::Create(context, "else");
    }
    else
    {
        nextBB = mergeBB;
    }

    // Conditional branch for if
    std::cerr << "[IR DEBUG] Creating conditional branch for if\n";
    funcBuilder.CreateCondBr(condVal, thenBB, nextBB);

    // Generate then branch
    funcBuilder.SetInsertPoint(thenBB);
    std::cerr << "[IR DEBUG] Generating then branch\n";
    generateStatement(ifStmt->if_result.get());
    if (!funcBuilder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to ifmerge from then\n";
        funcBuilder.CreateBr(mergeBB);
    }
    else
    {
        std::cerr << "[IR DEBUG] Skipping branch to ifmerge from then due to terminator: " << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // Generating elif branches
    for (size_t i = 0; i < ifStmt->elifClauses.size(); ++i)
    {
        function->insert(function->end(), nextBB);
        funcBuilder.SetInsertPoint(nextBB);
        std::cerr << "[IR DEBUG] Generating elif branch " << i << "\n";

        const auto &elifStmt = ifStmt->elifClauses[i];
        auto elif = dynamic_cast<elifStatement *>(elifStmt.get());
        llvm::Value *elifCondVal = generateExpression(elif->elif_condition.get());
        if (!elifCondVal)
        {
            throw std::runtime_error("Invalid elif condition");
        }
        if (!elifCondVal->getType()->isIntegerTy(1))
        {
            elifCondVal = funcBuilder.CreateICmpNE(elifCondVal, llvm::ConstantInt::get(elifCondVal->getType(), 0), "elifcond.bool");
        }

        llvm::BasicBlock *elifBodyBB = llvm::BasicBlock::Create(context, "elif.body" + std::to_string(i), function);
        llvm::BasicBlock *nextElifBB = (i + 1 < ifStmt->elifClauses.size()) ? llvm::BasicBlock::Create(context, "elif" + std::to_string(i + 1))
                                                                            : (ifStmt->else_result.has_value() ? llvm::BasicBlock::Create(context, "else") : mergeBB);

        std::cerr << "[IR DEBUG] Creating conditional branch for elif " << i << "\n";
        funcBuilder.CreateCondBr(elifCondVal, elifBodyBB, nextElifBB);

        funcBuilder.SetInsertPoint(elifBodyBB);
        std::cerr << "[IR DEBUG] Generating elif body " << i << "\n";
        generateStatement(elif->elif_result.get());
        if (!funcBuilder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Adding branch to ifmerge from elif " << i << "\n";
            funcBuilder.CreateBr(mergeBB);
        }
        else
        {
            std::cerr << "[IR DEBUG] Skipping branch " << i << " to ifmerge from elif due to terminator " << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
        }

        nextBB = nextElifBB;
    }

    // Generate else branch if present
    if (ifStmt->else_result.has_value())
    {
        function->insert(function->end(), nextBB);
        funcBuilder.SetInsertPoint(nextBB);
        std::cerr << "[IR DEBUG] Generating else branch\n";
        generateStatement(ifStmt->else_result.value().get());
        if (!funcBuilder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Adding branch to ifmerge from else\n";
            funcBuilder.CreateBr(mergeBB);
        }
        else
        {
            std::cerr << "[IR DEBUG] Skipping branch to ifmerge from else due to terminator: " << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
        }
    }

    // Finalize with merge block
    function->insert(function->end(), mergeBB);
    funcBuilder.SetInsertPoint(mergeBB);
    std::cerr << "[IR DEBUG] Finished generating if statement\n";
}

// IR code gen for a for loop
void IRGenerator::generateForStatement(Node *node)
{
    std::cout << "Generating IR for : " << node->toString();
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        throw std::runtime_error("Invalid for statement");

    llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *origBB = funcBuilder.GetInsertBlock();

    // initializer
    if (forStmt->initializer)
    {
        std::cerr << "[IR DEBUG] Generating initializer\n";
        generateStatement(forStmt->initializer.get());
    }

    // create blocks attached to function
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "loop.cond", function);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "loop.body", function);
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(context, "loop.step", function);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(context, "loop.end", function);

    // branch to condition
    funcBuilder.CreateBr(condBB);

    // condition
    funcBuilder.SetInsertPoint(condBB);
    std::cerr << "[IR DEBUG] Generating condition\n";
    llvm::Value *condVal = generateExpression(forStmt->condition.get());
    if (!condVal)
    {
        std::cerr << "[IR ERROR] For loop has invalid condition\n";
        funcBuilder.SetInsertPoint(origBB);
        return;
    }

    auto it = semantics.metaData.find(forStmt->condition.get());
    if (it == semantics.metaData.end() || it->second->type.kind != DataType::BOOLEAN)
    {
        std::cerr << "[IR ERROR] For loop condition must evaluate to boolean.\n";
        funcBuilder.SetInsertPoint(origBB);
        return;
    }

    // promote if needed
    if (!condVal->getType()->isIntegerTy(1))
    {
        if (condVal->getType()->isIntegerTy(32))
            condVal = funcBuilder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "loopcond.bool");
        else
        {
            std::cerr << "[IR ERROR] For loop condition must be boolean or i32\n";
            funcBuilder.SetInsertPoint(origBB);
            return;
        }
    }

    funcBuilder.CreateCondBr(condVal, bodyBB, endBB);

    // body
    funcBuilder.SetInsertPoint(bodyBB);
    loopBlocksStack.push_back({condBB, stepBB, endBB}); // <-- includes stepBB now
    std::cerr << "[IR DEBUG] Generating loop body\n";
    generateStatement(forStmt->body.get());
    std::cerr << "[IR DEBUG] Finished generating loop body\n";
    loopBlocksStack.pop_back();

    if (!funcBuilder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to loop.step\n";
        funcBuilder.CreateBr(stepBB);
    }
    else
    {
        std::cerr << "[IR WARNING] Loop body block already has terminator: " << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // step
    funcBuilder.SetInsertPoint(stepBB);
    if (forStmt->step)
    {
        std::cerr << "[IR DEBUG] Generating loop step\n";
        generateStatement(forStmt->step.get());
    }
    funcBuilder.CreateBr(condBB);

    // after
    funcBuilder.SetInsertPoint(endBB);
}

void IRGenerator::generateBreakStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Break statement not inside a loop");
    }

    llvm::BasicBlock *afterBB = loopBlocksStack.back().afterBB;
    std::cerr << "[IR DEBUG] Generating break to " << afterBB->getName().str() << "\n";
    funcBuilder.CreateBr(afterBB);
}

void IRGenerator::generateContinueStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Continue statement not inside a loop");
    }

    llvm::BasicBlock *condBB = loopBlocksStack.back().condBB;
    std::cerr << "[IR DEBUG] Generating continue to " << condBB->getName().str() << "\n";
    funcBuilder.CreateBr(condBB);
}

// Expression statement IR generator function
void IRGenerator::generateExpressionStatement(Node *node)
{
    auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
    if (!exprStmt)
    {
        throw std::runtime_error("Invalid expression statement node");
    }
    generateExpression(exprStmt->expression.get());
}

// Assignment statement IR generator function
void IRGenerator::generateAssignmentStatement(Node *node)
{
    auto *assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    if (isGlobalScope)
        throw std::runtime_error("Executable statements are not allowed at global scope");

    llvm::Value *targetPtr = nullptr;
    llvm::Value *initValue = generateExpression(assignStmt->value.get());
    if (!initValue)
        throw std::runtime_error("Failed to generate IR for assignment value");

    // Check if it's a SelfExpression (self.field)
    if (auto *selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
    {
        // Generate pointer to the field in the struct
        targetPtr = generateSelfAddress(selfExpr);
        if (!targetPtr)
            throw std::runtime_error("Failed to get pointer for self field");
    }

    // Checking if it is a dereference expression
    else if (auto derefExpr = dynamic_cast<DereferenceExpression *>(assignStmt->identifier.get()))
    {
        std::cout << "Inside Assignment statement deref branch\n";
        targetPtr = generateDereferenceAddress(derefExpr);
        if (!targetPtr)
            throw std::runtime_error("Failed to get pointer for the dereference expression");
    }
    else if (auto arraySub = dynamic_cast<ArraySubscript *>(assignStmt->identifier.get()))
    {
        std::cout << "Inside array sub branch\n";
        targetPtr = generateArraySubscriptAddress(arraySub);
        if (!targetPtr)
        {
            throw std::runtime_error("Failed to get L-value for array sub");
        }
    }
    else
    { // Regular variable assignment
        const std::string &varName = assignStmt->identifier->expression.TokenLiteral;
        auto metaIt = semantics.metaData.find(assignStmt);
        if (metaIt == semantics.metaData.end())
            throw std::runtime_error("Could not find variable '" + varName + "' metaData");

        auto assignSym = metaIt->second;

        if (!assignSym)
            throw std::runtime_error("Could not find variable '" + varName + "'");

        if (assignSym->hasError)
            throw std::runtime_error("Semantic error detected");

        AddressAndPendingFree addrAndPendingFree = generateIdentifierAddress(assignStmt->identifier.get());
        targetPtr = addrAndPendingFree.address;
        llvm::CallInst *pendingFree = addrAndPendingFree.pendingFree;

        if (!targetPtr)
            throw std::runtime_error("No memory allocated for variable '" + varName + "'");

        if (pendingFree)
        {
            funcBuilder.Insert(pendingFree);
        }
    }
    // Store the value
    funcBuilder.CreateStore(initValue, targetPtr);
    std::cout << "Exited assignment generator\n";
}

void IRGenerator::generateFieldAssignmentStatement(Node *node)
{
    auto *fieldStmt = dynamic_cast<FieldAssignment *>(node);
    if (!fieldStmt)
        return;

    // Generate RHS (value to store)
    llvm::Value *rhs = generateExpression(fieldStmt->value.get());
    if (!rhs)
        throw std::runtime_error("Failed to generate RHS IR");

    // Split scoped name -> "Person::age" -> parent = "Person", child = "age"
    auto [parentVarName, childName] = semantics.splitScopedName(fieldStmt->assignment_token.TokenLiteral);

    // Resolve parent symbol info (the instance)
    auto parentMetaIt = semantics.metaData.find(fieldStmt);
    if (parentMetaIt == semantics.metaData.end())
        throw std::runtime_error("Field assignment is lacking metaData");

    auto parentVarInfo = parentMetaIt->second;
    if (!parentVarInfo)
        throw std::runtime_error("Unidentified variable '" + parentVarName + "'");

    // Getting the base symbol
    auto baseSym = parentVarInfo->baseSymbol;
    if (!baseSym)
        throw std::runtime_error("Unidentified variable '" + parentVarName + "'");

    std::cout << "Parent type name: " << baseSym->type.resolvedName << "\n";

    if (!baseSym->llvmValue)
        throw std::runtime_error("Unresolved parent instance: " + parentVarName);

    // Resolve parent type and member info
    auto parentTypeName = baseSym->type.resolvedName;
    auto parentIt = semantics.customTypesTable.find(parentTypeName);
    if (parentIt == semantics.customTypesTable.end())
        throw std::runtime_error("Type '" + parentTypeName + "' does not exist");

    auto &members = parentIt->second->members;
    auto childIt = members.find(childName);
    if (childIt == members.end())
        throw std::runtime_error("'" + childName + "' is not a member of '" + parentTypeName + "'");

    // Member metadata and types
    auto memberInfo = childIt->second; // whatever structure you use for member metadata
    llvm::StructType *structTy = llvmCustomTypes[parentTypeName];
    unsigned fieldIndex = memberInfo->memberIndex;

    // GEP to the member slot inside this instance (slot type for heap-member is "elem*",
    // so the GEP yields a pointer-to-slot whose type is elem**)
    llvm::Value *fieldPtr = funcBuilder.CreateStructGEP(structTy, baseSym->llvmValue, fieldIndex, childName);
    if (!fieldPtr)
        throw std::runtime_error("Failed to compute GEP for member '" + childName + "'");

    if (memberInfo->isHeap)
    {
        // Element LLVM type (e.g., i32 for int)
        llvm::Type *elemTy = getLLVMType(memberInfo->type);
        if (!elemTy)
            throw std::runtime_error("Failed to get LLVM element type for member '" + childName + "'");

        // ptr-to-elem type (elem*)
        llvm::PointerType *elemPtrTy = elemTy->getPointerTo();

        // Load current heap pointer from the slot: heapPtr = load(elem*, fieldPtr)
        llvm::Value *heapPtr = funcBuilder.CreateLoad(elemPtrTy, fieldPtr, childName + "_heap_load");

        // If heapPtr == null -> allocate; otherwise reuse existing pointer.
        llvm::Value *nullPtr = llvm::ConstantPointerNull::get(elemPtrTy);
        llvm::Value *isNull = funcBuilder.CreateICmpEQ(heapPtr, nullPtr, childName + "_is_null");

        llvm::Function *parentFn = funcBuilder.GetInsertBlock()->getParent();
        llvm::BasicBlock *allocBB = llvm::BasicBlock::Create(context, childName + "_alloc", parentFn);
        llvm::BasicBlock *contBB = llvm::BasicBlock::Create(context, childName + "_cont", parentFn);

        // Cond branch on null
        funcBuilder.CreateCondBr(isNull, allocBB, contBB);

        //  allocBB: call sage_alloc(size), bitcast to elemPtrTy, store into slot, branch to contBB
        funcBuilder.SetInsertPoint(allocBB);

        llvm::PointerType *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
        llvm::FunctionCallee sageAllocFn = module->getOrInsertFunction("sage_alloc", i8PtrTy, llvm::Type::getInt64Ty(context));

        // compute size (use componentSize from semantics if present or DataLayout)
        uint64_t size = module->getDataLayout().getTypeAllocSize(elemTy);

        llvm::Value *sizeArg = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size);

        llvm::Value *rawAlloc = funcBuilder.CreateCall(sageAllocFn, {sizeArg}, childName + "_alloc_i8ptr");
        // bitcast i8* -> elem*
        llvm::Value *newHeapPtr = funcBuilder.CreateBitCast(rawAlloc, elemPtrTy, childName + "_alloc_ptr");

        // store pointer into the struct slot
        funcBuilder.CreateStore(newHeapPtr, fieldPtr);
        // branch to continuation
        funcBuilder.CreateBr(contBB);

        // --- contBB: reload heapPtr from slot (now definitely non-null)
        funcBuilder.SetInsertPoint(contBB);
        llvm::Value *heapPtr2 = funcBuilder.CreateLoad(elemPtrTy, fieldPtr, childName + "_heap");
        // use heapPtr2 for storing rhs
        funcBuilder.CreateStore(rhs, heapPtr2);

        // After the store, if this member's symbol marks this fieldStmt as last use, free it
        if (memberInfo->isHeap && (memberInfo->lastUseNode == fieldStmt || memberInfo->lastUseNode == /* possibly other node pointer */ memberInfo->node))
        {
            llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
                "sage_free", llvm::Type::getVoidTy(context), llvm::Type::getInt64Ty(context));
            funcBuilder.CreateCall(sageFreeFn, {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size)});
        }

        // continue building after contBB (builder is already positioned at contBB end)
    }
    else
    {
        // Non-heap: store RHS directly into the field slot
        funcBuilder.CreateStore(rhs, fieldPtr);
    }
}

void IRGenerator::generateBlockStatement(Node *node)
{
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
    {
        throw std::runtime_error("Invalid block statement");
    }

    std::cerr << "[IR DEBUG] Generating block statement with " << blockStmt->statements.size() << " statements\n";
    for (const auto &stmt : blockStmt->statements)
    {
        std::cerr << "[IR DEBUG] Processing block statement child of type: "
                  << typeid(*stmt).name() << " - " << stmt->toString() << "\n";

        generateStatement(stmt.get());

        auto currentBlock = funcBuilder.GetInsertBlock();
        if (currentBlock)
        {
            if (currentBlock->getTerminator())
            {
                std::cerr << "[IR DEBUG] Terminator found in block statement child: "
                          << currentBlock->getTerminator()->getOpcodeName() << "\n";
                break; // Stop generating further instructions in this block
            }
        }
        else
        {
            std::cerr << "[IR DEBUG] No current insert block after statement, skipping terminator check\n";
        }
    }
}

void IRGenerator::generateShoutStatement(Node *node)
{
    auto shoutStmt = dynamic_cast<ShoutStatement *>(node);

    if (!shoutStmt)
        throw std::runtime_error("Invalid shout statement node");

    if (isGlobalScope)
        throw std::runtime_error("Expected shout to be inside a function");

    // Getting the semantic type of the val
    auto it = semantics.metaData.find(shoutStmt->expr.get());
    if (it == semantics.metaData.end())
        throw std::runtime_error("Missing metaData for shout expression");

    // Getting the symbol
    auto exprSym = it->second;
    if (!exprSym)
        throw std::runtime_error("No symbol info found ");

    // Getting the type
    ResolvedType type = exprSym->type;

    // Call the expression generation n the expression
    auto val = generateExpression(shoutStmt->expr.get());
    if (!val)
        throw std::runtime_error("No llvm value was generated for expression in shout");

    // Call the shoutRuntime this is the one who actually prints
    shoutRuntime(val, type);
}

void IRGenerator::generateFunctionStatement(Node *node)
{
    auto fnStmt = dynamic_cast<FunctionStatement *>(node);
    if (!fnStmt)
        return;

    llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();

    // Checking what the function statement is holding could be a function expression or a function declaration expression
    auto fnExpr = fnStmt->funcExpr.get();
    // Case where it is a full function expression
    if (auto expr = dynamic_cast<FunctionExpression *>(fnExpr))
    {
        generateFunctionExpression(expr);
    }
    // Case where it is a function declaration expression(this is the special case)
    if (auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnExpr))
    {
        /*This is actually supposed to behave like a statement dispatcher
        That is because it doesnt actually produce a value it is a wrapper and doesnt evaluate to anything
        But with the  way I built the AST for my function declaration I have to do it like this kinda shady though
        But it will work*/
        generateFunctionDeclarationExpression(declrExpr);
    }

    if (oldInsertPoint)
        funcBuilder.SetInsertPoint(oldInsertPoint);
}

void IRGenerator::generateFunctionDeclarationExpression(Node *node)
{
    auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(node);
    if (!declrExpr)
        return;

    // This is also just a wrapper for the function declaration expression so let me just call the statement generator
    auto fnDeclr = declrExpr->funcDeclrStmt.get();
    // Call the statement generator for the function declaration statement
    generateFunctionDeclaration(fnDeclr);
}

void IRGenerator::generateFunctionDeclaration(Node *node)
{
    auto fnDeclr = dynamic_cast<FunctionDeclaration *>(node);
    if (!fnDeclr)
        return;
    // Getting the function name
    const std::string &fnName = fnDeclr->function_name->expression.TokenLiteral;

    // Registering the function and its type
    auto declrIt = semantics.metaData.find(fnDeclr);
    if (declrIt == semantics.metaData.end())
    {
        throw std::runtime_error("Missing function declaration meta data");
    }

    auto sym = declrIt->second;
    if (sym->hasError)
        throw std::runtime_error("Semantic error detected");

    std::vector<llvm::Type *> paramTypes;
    for (const auto &param : fnDeclr->parameters)
    {
        auto it = semantics.metaData.find(param.get());
        if (it == semantics.metaData.end())
        {
            throw std::runtime_error("Missing function declaration parameter meta data");
        }
        paramTypes.push_back(getLLVMType(it->second->type));
    }
    auto retType = declrIt->second->returnType;
    llvm::FunctionType *fnType = llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

    llvm::Function *declaredFunc = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, fnName, module.get());
}

void IRGenerator::generateReturnStatement(Node *node)
{
    auto retStmt = dynamic_cast<ReturnStatement *>(node);
    if (!retStmt)
        return;

    // Retrieve the metaData (for error checking)
    auto it = semantics.metaData.find(retStmt);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Missing return statement metaData");
    }

    if (it->second->hasError)
    {
        throw std::runtime_error("Semantic error detected");
    }

    llvm::Value *retVal = nullptr;
    if (retStmt->return_value)
    {
        retVal = generateExpression(retStmt->return_value.get());
    }

    llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
    llvm::Type *retTy = currentFunction->getReturnType();

    // Void return
    if (retTy->isVoidTy())
    {
        if (retVal)
        {
            llvm::errs() << "Warning: returning a value from a void function; value will be ignored\n";
        }
        funcBuilder.CreateRetVoid();
        return;
    }

    // Non-void return: we expect a value
    if (!retVal)
    {
        llvm::errs() << "Return statement missing value for non-void function\n";
        return;
    }

    // If the returned value type doesn't match function return type, try to adapt
    llvm::Type *valTy = retVal->getType();
    if (valTy == retTy)
    {
        funcBuilder.CreateRet(retVal);
        return;
    }

    // If both are pointer types and point to compatible element, bitcast the pointer
    if (valTy->isPointerTy() && retTy->isPointerTy())
    {
        llvm::Value *casted = funcBuilder.CreateBitCast(retVal, retTy);
        funcBuilder.CreateRet(casted);
        return;
    }

    // If returning an aggregate (struct) by value but retVal is a pointer-to-aggregate (unlikely),
    // load and return the aggregate value.
    if (valTy->isPointerTy() && llvm::isa<llvm::StructType>(retTy))
    {
        llvm::Value *loaded = funcBuilder.CreateLoad(retTy, retVal);
        funcBuilder.CreateRet(loaded);
        return;
    }

    // If both are integer types of different widths, extend/truncate.
    if (valTy->isIntegerTy() && retTy->isIntegerTy())
    {
        unsigned vbits = llvm::cast<llvm::IntegerType>(valTy)->getBitWidth();
        unsigned rbits = llvm::cast<llvm::IntegerType>(retTy)->getBitWidth();
        if (vbits < rbits)
            funcBuilder.CreateRet(funcBuilder.CreateSExt(retVal, retTy));
        else if (vbits > rbits)
            funcBuilder.CreateRet(funcBuilder.CreateTrunc(retVal, retTy));
        else
            funcBuilder.CreateRet(retVal);
        return;
    }

    // Fallback types are incompatible — emit an error and try to bitcast if possible.(This wont happen because semantics must have caught it but who knows)
    llvm::errs() << "Return type mismatch: returning '" << *valTy << "' but function expects '" << *retTy << "'\n";

    // Last-chance attempt, try a bitcast if sizes match (risky)
    if (valTy->getPrimitiveSizeInBits() == retTy->getPrimitiveSizeInBits())
    {
        llvm::Value *maybe = funcBuilder.CreateBitCast(retVal, retTy);
        funcBuilder.CreateRet(maybe);
        return;
    }
}

// EXPRESSION GENERATOR
//  Expression generator functions
llvm::Value *IRGenerator::generateInfixExpression(Node *node)
{
    auto infix = dynamic_cast<InfixExpression *>(node);
    if (!infix)
        throw std::runtime_error("Invalid infix expression");

    auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
    auto lhsIdent = dynamic_cast<Identifier *>(infix->left_operand.get());

    if (isGlobalScope && (lhsIdent || rhsIdent))
    {
        throw std::runtime_error(
            "Executable statements are not allowed at global scope: '" +
            infix->operat.TokenLiteral + "' at line " +
            std::to_string(infix->operat.line));
    }

    if (infix->operat.type == TokenType::FULLSTOP)
    {
        // sanity check: member access only inside a function
        if (isGlobalScope)
            throw std::runtime_error("Member access not allowed at global scope");

        llvm::Value *lhsVal = generateExpression(infix->left_operand.get());
        auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
        if (!rhsIdent)
            throw std::runtime_error("Right side of '.' must be an identifier");

        std::string memberName = rhsIdent->expression.TokenLiteral;

        // get metadata for the left operand
        auto lhsMeta = semantics.metaData[infix->left_operand.get()];
        if (!lhsMeta)
            throw std::runtime_error("Missing metadata for struct expression");

        std::string parentTypeName = lhsMeta->type.resolvedName;

        // get struct definition
        auto parentTypeIt = semantics.customTypesTable.find(parentTypeName);
        if (parentTypeIt == semantics.customTypesTable.end())
            throw std::runtime_error("Unknown struct type '" + parentTypeName + "'");

        auto &parentInfo = parentTypeIt->second;
        auto memberIt = parentInfo->members.find(memberName);
        if (memberIt == parentInfo->members.end())
            throw std::runtime_error("No member '" + memberName + "' in type '" + parentTypeName + "'");

        // get member index + type
        unsigned memberIndex = memberIt->second->memberIndex;
        llvm::StructType *structTy = llvmCustomTypes[parentTypeName];
        llvm::Type *memberType = getLLVMType(memberIt->second->type);

        // if lhs is struct by value, we need a pointer to use GEP
        llvm::Value *lhsPtr = lhsVal;
        if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(lhsVal))
        {
            lhsPtr = funcBuilder.CreateLoad(structTy->getPointerTo(), gv, gv->getName() + ".loaded");
        }
        else if (!lhsVal->getType()->isPointerTy())
        {
            llvm::Value *allocaTmp = funcBuilder.CreateAlloca(lhsVal->getType(), nullptr, parentTypeName + "_tmp");
            funcBuilder.CreateStore(lhsVal, allocaTmp);
            lhsPtr = allocaTmp;
        }

        llvm::Value *memberPtr = funcBuilder.CreateStructGEP(structTy, lhsPtr, memberIndex, memberName);

        // now return by-value load, not a pointer
        llvm::Value *memberVal = funcBuilder.CreateLoad(memberType, memberPtr, memberName + "_val");

        // If this is the last use and the component instance was heap raised
        if (lhsMeta->isHeap && lhsMeta->lastUseNode == infix && lhsMeta->refCount == 0)
        {
            llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
                "sage_free",
                llvm::Type::getVoidTy(context),
                llvm::Type::getInt64Ty(context));
            funcBuilder.CreateCall(
                sageFreeFn,
                {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), lhsMeta->componentSize)},
                infix->left_operand->expression.TokenLiteral + "_sage_free");
        }

        return memberVal;
    }

    if (infix->operat.type == TokenType::SCOPE_OPERATOR)
    {
        std::cout << "TRIGGERED SCOPE OPERATOR GUY\n";

        // "::" is only for enum access
        auto enumName = lhsIdent->expression.TokenLiteral;
        auto leftTypeIt = semantics.customTypesTable.find(enumName);
        if (leftTypeIt == semantics.customTypesTable.end())
            throw std::runtime_error("Unknown enum class '" + enumName + "'");

        auto leftType = leftTypeIt->second->type;

        // If the type isn't an enum reject it
        if (leftType.kind != DataType::ENUM)
        {
            throw std::runtime_error("The '::' operator is only valid for enum access");
        }

        auto &enumInfo = leftTypeIt->second;

        // RHS must be an identifier (enum member)
        auto memberIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
        if (!memberIdent)
            throw std::runtime_error("Right-hand of '::' must be an enum member identifier");

        auto memberName = memberIdent->expression.TokenLiteral;

        // Find enum member
        auto memIt = enumInfo->members.find(memberName);
        if (memIt == enumInfo->members.end())
            throw std::runtime_error("Enum '" + enumName + "' has no member named '" + memberName + "'");

        uint64_t memberValue = memIt->second->constantValue;

        // Return constant integer
        llvm::Type *llvmEnumTy = getLLVMType(leftType);
        return llvm::ConstantInt::get(llvmEnumTy, memberValue);
    }

    llvm::Value *left = generateExpression(infix->left_operand.get());
    llvm::Value *right = generateExpression(infix->right_operand.get());

    if (!left || !right)
        throw std::runtime_error("Failed to generate IR for infix expression");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Meta data missing for infix node");

    ResolvedType resultType = it->second->type;
    DataType leftType = semantics.metaData[infix->left_operand.get()]->type.kind;
    DataType rightType = semantics.metaData[infix->right_operand.get()]->type.kind;

    // Helper lambda for integer type promotion
    auto promoteInt = [&](llvm::Value *val, DataType fromType, DataType toType) -> llvm::Value *
    {
        unsigned fromBits = getIntegerBitWidth(fromType);
        unsigned toBits = getIntegerBitWidth(toType);
        if (fromBits == 0 || toBits == 0)
            return val; // Not integer type
        if (fromBits == toBits)
            return val; // Same bit width, no promotion needed

        bool fromSigned = isSignedInteger(fromType);
        if (toBits > fromBits)
        {
            if (fromSigned)
                return funcBuilder.CreateSExt(val, llvm::IntegerType::get(context, toBits), "sexttmp");
            else
                return funcBuilder.CreateZExt(val, llvm::IntegerType::get(context, toBits), "zexttmp");
        }
        else
        {
            // Truncation if needed (rare, probably invalid here)
            return funcBuilder.CreateTrunc(val, llvm::IntegerType::get(context, toBits), "trunctmp");
        }
    };

    // Promote operands to widest integer type among left, right, and result
    if (isIntegerType(resultType.kind))
    {
        unsigned targetBits = getIntegerBitWidth(resultType.kind);
        left = promoteInt(left, leftType, resultType.kind);
        right = promoteInt(right, rightType, resultType.kind);
    }

    // Handle BOOLEAN logical operators (AND, OR)
    if (infix->operat.type == TokenType::AND || infix->operat.type == TokenType::OR)
    {
        if (left->getType() != funcBuilder.getInt1Ty())
            left = funcBuilder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0), "boolcastl");
        if (right->getType() != funcBuilder.getInt1Ty())
            right = funcBuilder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0), "boolcastr");

        if (infix->operat.type == TokenType::AND)
            return funcBuilder.CreateAnd(left, right, "andtmp");
        else
            return funcBuilder.CreateOr(left, right, "ortmp");
    }

    // Handle floating point conversions
    if (resultType.kind == DataType::FLOAT)
    {
        if (isIntegerType(leftType))
            left = funcBuilder.CreateSIToFP(left, llvm::Type::getFloatTy(context), "inttofloat");
        if (isIntegerType(rightType))
            right = funcBuilder.CreateSIToFP(right, llvm::Type::getFloatTy(context), "inttofloat");
    }
    else if (resultType.kind == DataType::DOUBLE)
    {
        if (isIntegerType(leftType))
            left = funcBuilder.CreateSIToFP(left, llvm::Type::getDoubleTy(context), "inttodouble");
        if (isIntegerType(rightType))
            right = funcBuilder.CreateSIToFP(right, llvm::Type::getDoubleTy(context), "inttodouble");
    }

    // Now generate code based on operator and result type
    // Comparison operators - different for signed/unsigned integers
    if (infix->operat.type == TokenType::EQUALS ||
        infix->operat.type == TokenType::NOT_EQUALS ||
        infix->operat.type == TokenType::LESS_THAN ||
        infix->operat.type == TokenType::LT_OR_EQ ||
        infix->operat.type == TokenType::GREATER_THAN ||
        infix->operat.type == TokenType::GT_OR_EQ)
    {
        if (isIntegerType(leftType) || leftType == DataType::ENUM)
        {
            if (isIntegerType(rightType) || rightType == DataType::ENUM)
            {
                bool signedInt = isSignedInteger(resultType.kind);
                switch (infix->operat.type)
                {
                case TokenType::EQUALS:
                    return funcBuilder.CreateICmpEQ(left, right, "cmptmp");
                case TokenType::NOT_EQUALS:
                    return funcBuilder.CreateICmpNE(left, right, "cmptmp");
                case TokenType::LESS_THAN:
                    return signedInt ? funcBuilder.CreateICmpSLT(left, right, "cmptmp")
                                     : funcBuilder.CreateICmpULT(left, right, "cmptmp");
                case TokenType::LT_OR_EQ:
                    return signedInt ? funcBuilder.CreateICmpSLE(left, right, "cmptmp")
                                     : funcBuilder.CreateICmpULE(left, right, "cmptmp");
                case TokenType::GREATER_THAN:
                    return signedInt ? funcBuilder.CreateICmpSGT(left, right, "cmptmp")
                                     : funcBuilder.CreateICmpUGT(left, right, "cmptmp");
                case TokenType::GT_OR_EQ:
                    return signedInt ? funcBuilder.CreateICmpSGE(left, right, "cmptmp")
                                     : funcBuilder.CreateICmpUGE(left, right, "cmptmp");
                default:
                    throw std::runtime_error("Unsupported int comparison operator");
                }
            }
        }
        else if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
        {
            switch (infix->operat.type)
            {
            case TokenType::EQUALS:
                return funcBuilder.CreateFCmpOEQ(left, right, "fcmptmp");
            case TokenType::NOT_EQUALS:
                return funcBuilder.CreateFCmpONE(left, right, "fcmptmp");
            case TokenType::LESS_THAN:
                return funcBuilder.CreateFCmpOLT(left, right, "fcmptmp");
            case TokenType::LT_OR_EQ:
                return funcBuilder.CreateFCmpOLE(left, right, "fcmptmp");
            case TokenType::GREATER_THAN:
                return funcBuilder.CreateFCmpOGT(left, right, "fcmptmp");
            case TokenType::GT_OR_EQ:
                return funcBuilder.CreateFCmpOGE(left, right, "fcmptmp");
            default:
                throw std::runtime_error("Unsupported float comparison operator");
            }
        }
        else
        {
            throw std::runtime_error("Comparison not supported for type '" + resultType.resolvedName + "'");
        }
    }
    // Arithmetic operators
    switch (infix->operat.type)
    {
    case TokenType::PLUS:
    {

        if (isIntegerType(resultType.kind))
            return funcBuilder.CreateAdd(left, right, "addtmp");
        else
            return funcBuilder.CreateFAdd(left, right, "faddtmp");
    }

    case TokenType::MINUS:
    {
        if (isIntegerType(resultType.kind))
            return funcBuilder.CreateSub(left, right, "subtmp");
        else
            return funcBuilder.CreateFSub(left, right, "fsubtmp");
    }

    case TokenType::ASTERISK:
    {
        if (isIntegerType(resultType.kind))
            return funcBuilder.CreateMul(left, right, "multmp");
        else
            return funcBuilder.CreateFMul(left, right, "fmultmp");
    }

    case TokenType::DIVIDE:
    {
        if (isIntegerType(resultType.kind))
            return isSignedInteger(resultType.kind) ? funcBuilder.CreateSDiv(left, right, "divtmp")
                                                    : funcBuilder.CreateUDiv(left, right, "divtmp");
        else
            return funcBuilder.CreateFDiv(left, right, "fdivtmp");
    }

    case TokenType::MODULUS:
    {

        if (isIntegerType(resultType.kind))
            return isSignedInteger(resultType.kind) ? funcBuilder.CreateSRem(left, right, "modtmp")
                                                    : funcBuilder.CreateURem(left, right, "modtmp");
        else
            throw std::runtime_error("Modulus not supported for FLOAT or DOUBLE at line " +
                                     std::to_string(infix->operat.line));
    }

    default:
        throw std::runtime_error("Unsupported infix operator: " + infix->operat.TokenLiteral +
                                 " at line " + std::to_string(infix->operat.line));
    }
}

// Prefix expression generator function
llvm::Value *IRGenerator::generatePrefixExpression(Node *node)
{
    std::cout << "Inside prefix generator\n";
    auto prefix = dynamic_cast<PrefixExpression *>(node);
    if (!prefix)
        throw std::runtime_error("Invalid prefix expression");

    if (isGlobalScope)
        throw std::runtime_error("Executable statements arent allowed in the global scope");

    llvm::Value *operand;

    // If the operand is an identifier
    if (auto identOperand = dynamic_cast<Identifier *>(prefix->operand.get()))
    {
        operand = generateIdentifierAddress(prefix->operand.get()).address;
    }

    // If the operand is a normal literal
    operand = generateExpression(prefix->operand.get());

    if (!operand)
        throw std::runtime_error("Failed to generate IR for prefix operand");

    const std::string &name = prefix->operand->expression.TokenLiteral;
    auto prefixIt = semantics.metaData.find(prefix);
    if (prefixIt == semantics.metaData.end())
    {
        throw std::runtime_error("Unknown variable '" + name + "' in prefix");
    }

    ResolvedType resultType = prefixIt->second->type;

    // Helper to check if integer type
    auto isIntType = [&](DataType dt)
    {
        return isIntegerType(dt);
    };

    // Helper to get LLVM type from DataType
    auto getLLVMType = [&](DataType dt) -> llvm::Type *
    {
        if (dt == DataType::FLOAT)
            return llvm::Type::getFloatTy(context);
        if (dt == DataType::DOUBLE)
            return llvm::Type::getDoubleTy(context);
        if (isIntType(dt))
        {
            unsigned bits = getIntegerBitWidth(dt);
            return llvm::Type::getIntNTy(context, bits);
        }
        return nullptr;
    };

    switch (prefix->operat.type)
    {
    case TokenType::MINUS:
        if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
            return funcBuilder.CreateFNeg(operand, llvm::Twine("fnegtmp"));
        else if (isIntType(resultType.kind))
            return funcBuilder.CreateNeg(operand, llvm::Twine("negtmp"));
        else
            throw std::runtime_error("Unsupported type for unary minus");

    case TokenType::BANG:
        // Boolean NOT
        return funcBuilder.CreateNot(operand, llvm::Twine("nottmp"));

    case TokenType::PLUS_PLUS:
    case TokenType::MINUS_MINUS:
    {
        auto ident = dynamic_cast<Identifier *>(prefix->operand.get());
        if (!ident)
            throw std::runtime_error("Prefix ++/-- must be used on a variable");

        const std::string &name = ident->expression.TokenLiteral;
        auto identIt = semantics.metaData.find(ident);
        if (identIt == semantics.metaData.end())
        {
            throw std::runtime_error("Udefined variable '" + name + "'");
        }
        AddressAndPendingFree addrAndFree = generateIdentifierAddress(ident);
        llvm::Value *varPtr = addrAndFree.address;
        llvm::CallInst *pendingFree = addrAndFree.pendingFree;

        if (!varPtr)
            throw std::runtime_error("Null variable pointer for: " + ident->identifier.TokenLiteral);

        // Get the type from resultType instead of getPointerElementType
        llvm::Type *varType = getLLVMType(resultType.kind);
        if (!varType)
            throw std::runtime_error("Invalid type for variable: " + ident->identifier.TokenLiteral);

        llvm::Value *loaded = funcBuilder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

        llvm::Value *delta = nullptr;
        if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
        {
            delta = llvm::ConstantFP::get(varType, 1.0);
        }
        else if (isIntType(resultType.kind))
        {
            unsigned bits = getIntegerBitWidth(resultType.kind);
            delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
        }
        else
        {
            throw std::runtime_error("Unsupported type for ++/--");
        }

        llvm::Value *updated = nullptr;
        if (prefix->operat.type == TokenType::PLUS_PLUS)
            updated = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                          ? funcBuilder.CreateFAdd(loaded, delta, llvm::Twine("preincfptmp"))
                          : funcBuilder.CreateAdd(loaded, delta, llvm::Twine("preinctmp"));
        else
            updated = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                          ? funcBuilder.CreateFSub(loaded, delta, llvm::Twine("predecfptmp"))
                          : funcBuilder.CreateSub(loaded, delta, llvm::Twine("predectmp"));

        funcBuilder.CreateStore(updated, varPtr);

        if (pendingFree)
        {
            funcBuilder.Insert(pendingFree);
        }
        return updated;
    }

    default:
        throw std::runtime_error("Unsupported prefix operator: " + prefix->operat.TokenLiteral +
                                 " at line " + std::to_string(prefix->operat.line));
    }
}

llvm::Value *IRGenerator::generatePostfixExpression(Node *node)
{
    auto postfix = dynamic_cast<PostfixExpression *>(node);
    if (!postfix)
        throw std::runtime_error("Invalid postfix expression");

    auto identifier = dynamic_cast<Identifier *>(postfix->operand.get());
    if (!identifier)
        throw std::runtime_error("Postfix operand must be a variable");
    auto identName = identifier->identifier.TokenLiteral;
    auto identIt = semantics.metaData.find(identifier);

    AddressAndPendingFree addrAndFree = generateIdentifierAddress(identifier);
    llvm::Value *varPtr = addrAndFree.address;
    llvm::CallInst *pendingFree = addrAndFree.pendingFree; // may be nullptr

    if (!varPtr)
        throw std::runtime_error("Null variable pointer for: " + identifier->identifier.TokenLiteral);

    auto postfixIt = semantics.metaData.find(postfix);
    if (postfixIt == semantics.metaData.end())
    {
        throw std::runtime_error("Variable '" + identName + "' does not exist");
    }

    std::cerr << "[IR-DEBUG] ident lastUse = "
              << (identIt->second->lastUseNode ? typeid(*identIt->second->lastUseNode).name() : "NULL")
              << ", postfix lastUse = "
              << (postfixIt != semantics.metaData.end() && postfixIt->second->lastUseNode ? typeid(*postfixIt->second->lastUseNode).name() : "NULL")
              << "\n";

    if (postfixIt->second->hasError)
        return nullptr;

    ResolvedType resultType = postfixIt->second->type;

    // Helper to check if integer type
    auto isIntType = [&](DataType dt)
    { return isIntegerType(dt); };

    // Helper to get LLVM type from DataType
    auto getLLVMType = [&](DataType dt) -> llvm::Type *
    {
        if (dt == DataType::FLOAT)
            return llvm::Type::getFloatTy(context);
        if (dt == DataType::DOUBLE)
            return llvm::Type::getDoubleTy(context);
        if (isIntType(dt))
        {
            unsigned bits = getIntegerBitWidth(dt);
            return llvm::Type::getIntNTy(context, bits);
        }
        return nullptr;
    };

    // Get the type from resultType for CreateLoad
    llvm::Type *varType = getLLVMType(resultType.kind);
    if (!varType)
        throw std::runtime_error("Invalid type for variable: " + identifier->identifier.TokenLiteral);

    llvm::Value *originalValue = funcBuilder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

    llvm::Value *delta = nullptr;
    if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
    {
        delta = llvm::ConstantFP::get(varType, 1.0);
    }
    else if (isIntType(resultType.kind))
    {
        unsigned bits = getIntegerBitWidth(resultType.kind);
        delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
    }
    else
    {
        throw std::runtime_error("Unsupported type for ++/--");
    }

    llvm::Value *updatedValue = nullptr;

    if (postfix->operator_token.type == TokenType::PLUS_PLUS)
        updatedValue = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                           ? funcBuilder.CreateFAdd(originalValue, delta, llvm::Twine("finc"))
                           : funcBuilder.CreateAdd(originalValue, delta, llvm::Twine("inc"));
    else if (postfix->operator_token.type == TokenType::MINUS_MINUS)
        updatedValue = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                           ? funcBuilder.CreateFSub(originalValue, delta, llvm::Twine("fdec"))
                           : funcBuilder.CreateSub(originalValue, delta, llvm::Twine("dec"));
    else
        throw std::runtime_error("Unsupported postfix operator: " + postfix->operator_token.TokenLiteral +
                                 " at line " + std::to_string(postfix->operator_token.line));

    funcBuilder.CreateStore(updatedValue, varPtr);

    if (pendingFree)
    {
        // Insert the prepared free at the current insertion point
        funcBuilder.Insert(pendingFree); // inserts at current position (after store)
    }

    // Return original value since postfix
    return originalValue;
}

llvm::Value *IRGenerator::generateStringLiteral(Node *node)
{
    std::cout << "INSIDE GENERATE IR FOR STRING\n";
    auto strLit = dynamic_cast<StringLiteral *>(node);
    if (!strLit)
    {
        throw std::runtime_error("Invalid string literal");
    }
    auto it = semantics.metaData.find(strLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("String literal not found in metadata");
    }
    DataType dt = it->second->type.kind;

    if (dt != DataType::STRING)
    {
        throw std::runtime_error("Type error: Expected STRING");
    }
    std::string raw = strLit->string_token.TokenLiteral;
    llvm::Constant *strConst = llvm::ConstantDataArray::getString(context, raw, true);

    auto *globalStr = new llvm::GlobalVariable(
        *module,
        strConst->getType(),
        true,
        llvm::GlobalValue::PrivateLinkage,
        strConst,
        ".str");

    // Pointer to first element
    llvm::Constant *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
    llvm::Constant *indices[] = {zero, zero};
    llvm::Constant *strPtr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        strConst->getType(),
        globalStr,
        indices);

    return strPtr;
}

llvm::Value *IRGenerator::generateCharLiteral(Node *node)
{
    auto charLit = dynamic_cast<CharLiteral *>(node);
    if (!charLit)
    {
        throw std::runtime_error("Invalid char literal");
    }
    auto it = semantics.metaData.find(charLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Char literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::CHAR)
    {
        throw std::runtime_error("Type error: Expected CHAR for CharLiteral");
    }
    std::string tokenLiteral = charLit->char_token.TokenLiteral;

    char c = decodeCharLiteral(tokenLiteral);
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(c), false);
}

llvm::Value *IRGenerator::generateChar16Literal(Node *node)
{
    auto char16Lit = dynamic_cast<Char16Literal *>(node);
    if (!char16Lit)
    {
        throw std::runtime_error("Invalid char 16 literal");
    }
    auto it = semantics.metaData.find(char16Lit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Char16 literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::CHAR16)
    {
        throw std::runtime_error("Type error: Expected CHAR16 for Char16Literal");
    }
    std::string tokenLiteral = char16Lit->char16_token.TokenLiteral;
    uint16_t c = decodeCharLiteral(tokenLiteral);
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), c, false);
}

llvm::Value *IRGenerator::generateChar32Literal(Node *node)
{
    auto char32Lit = dynamic_cast<Char32Literal *>(node);
    if (!char32Lit)
    {
        throw std::runtime_error("Invalid char 32 literal");
    }
    auto it = semantics.metaData.find(char32Lit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Char16 literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::CHAR16)
    {
        throw std::runtime_error("Type error: Expected CHAR16 for Char16Literal");
    }
    std::string tokenLiteral = char32Lit->char32_token.TokenLiteral;
    uint32_t c = decodeChar32Literal(tokenLiteral);
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), c, false);
}

llvm::Value *IRGenerator::generateBooleanLiteral(Node *node)
{
    auto boolLit = dynamic_cast<BooleanLiteral *>(node);
    if (!boolLit)
    {
        throw std::runtime_error("Invalid boolean type");
    }
    auto it = semantics.metaData.find(boolLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Boolean literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::BOOLEAN)
    {
        throw std::runtime_error("Type error: Expected BOOLEAN for BooleanLiteral");
    }

    bool value = (boolLit->boolean_token.TokenLiteral == "true");

    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), value);
}

llvm::Value *IRGenerator::generateShortLiteral(Node *node)
{
    auto shortLit = dynamic_cast<ShortLiteral *>(node);
    if (!shortLit)
        throw std::runtime_error("Invalid short literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Short literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::SHORT_INT)
        throw std::runtime_error("Type error: Expected SHORT_INT");

    int16_t value = static_cast<int16_t>(std::stoi(shortLit->short_token.TokenLiteral));
    return llvm::ConstantInt::get(context, llvm::APInt(16, value, true));
}

llvm::Value *IRGenerator::generateUnsignedShortLiteral(Node *node)
{
    auto ushortLit = dynamic_cast<UnsignedShortLiteral *>(node);
    if (!ushortLit)
        throw std::runtime_error("Invalid ushort literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("UShort literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::USHORT_INT)
        throw std::runtime_error("Type error: Expected USHORT_INT");

    uint16_t value = static_cast<uint16_t>(std::stoul(ushortLit->ushort_token.TokenLiteral));
    return llvm::ConstantInt::get(context, llvm::APInt(16, value, false));
}

llvm::Value *IRGenerator::generateIntegerLiteral(Node *node)
{
    auto intLit = dynamic_cast<IntegerLiteral *>(node);
    if (!intLit)
    {
        throw std::runtime_error("Invalid integer literal");
    }
    auto it = semantics.metaData.find(intLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Integer literal not found in metadata at line:" + std::to_string(intLit->expression.line) + " and column: " + std::to_string(intLit->expression.column));
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::INTEGER)
    {
        throw std::runtime_error("Type error: Expected INTEGER for IntegerLiteral");
    }
    int64_t value = std::stoll(intLit->int_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(32, value, true));
}

llvm::Value *IRGenerator::generateUnsignedIntegerLiteral(Node *node)
{
    auto uintLit = dynamic_cast<UnsignedIntegerLiteral *>(node);
    if (!uintLit)
        throw std::runtime_error("Invalid uint literal");

    auto it = semantics.metaData.find(uintLit);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Uint literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::UINTEGER)
        throw std::runtime_error("Type error: Expected UINTEGER");

    uint32_t value = static_cast<uint32_t>(std::stoul(uintLit->uint_token.TokenLiteral));
    return llvm::ConstantInt::get(context, llvm::APInt(32, value, false));
}

llvm::Value *IRGenerator::generateLongLiteral(Node *node)
{
    auto longLit = dynamic_cast<LongLiteral *>(node);
    if (!longLit)
        throw std::runtime_error("Invalid long literal");

    auto it = semantics.metaData.find(longLit);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Long literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::LONG_INT)
        throw std::runtime_error("Type error: Expected LONG_INT ");

    int64_t value = std::stoll(longLit->long_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(64, value, true));
}

llvm::Value *IRGenerator::generateUnsignedLongLiteral(Node *node)
{
    auto ulongLit = dynamic_cast<UnsignedLongLiteral *>(node);
    if (!ulongLit)
        throw std::runtime_error("Invalid ulong literal");

    auto it = semantics.metaData.find(ulongLit);
    if (it == semantics.metaData.end())
        throw std::runtime_error("ULong literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::ULONG_INT)
        throw std::runtime_error("Type error: Expected ULONG_INT");

    uint64_t value = std::stoull(ulongLit->ulong_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(64, value, false));
}

llvm::Value *IRGenerator::generateExtraLiteral(Node *node)
{
    auto extraLit = dynamic_cast<ExtraLiteral *>(node);
    if (!extraLit)
        throw std::runtime_error("Invalid extra (128-bit) literal");

    auto it = semantics.metaData.find(extraLit);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Extra literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::EXTRA_INT)
        throw std::runtime_error("Type error: Expected EXTRA_INT");

    // Use APInt constructor with string and base 10 for 128-bit
    llvm::APInt value(128, extraLit->extra_token.TokenLiteral, 10);
    return llvm::ConstantInt::get(context, value);
}

llvm::Value *IRGenerator::generateUnsignedExtraLiteral(Node *node)
{
    auto uextraLit = dynamic_cast<UnsignedExtraLiteral *>(node);
    if (!uextraLit)
        throw std::runtime_error("Invalid uextra (128-bit unsigned) literal");

    auto it = semantics.metaData.find(uextraLit);
    if (it == semantics.metaData.end())
        throw std::runtime_error("UExtra literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::UEXTRA_INT)
        throw std::runtime_error("Type error: Expected UEXTRA_INT ");

    llvm::APInt value(128, uextraLit->uextra_token.TokenLiteral, 10);
    return llvm::ConstantInt::get(context, value);
}

llvm::Value *IRGenerator::generateFloatLiteral(Node *node)
{
    auto fltLit = dynamic_cast<FloatLiteral *>(node);
    if (!fltLit)
    {
        throw std::runtime_error("Invalid float literal");
    }
    auto it = semantics.metaData.find(fltLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Float literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::FLOAT)
    {
        throw std::runtime_error("Type error: Expected Float for FloatLiteral ");
    }
    float value = std::stof(fltLit->float_token.TokenLiteral);
    return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
}

llvm::Value *IRGenerator::generateDoubleLiteral(Node *node)
{
    auto dbLit = dynamic_cast<DoubleLiteral *>(node);
    if (!dbLit)
    {
        throw std::runtime_error("Invalid double literal");
    }
    auto it = semantics.metaData.find(dbLit); // Creating an iterator to find specific meta data about the double literal
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Double literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::DOUBLE)
    {
        throw std::runtime_error("Type error: Expected DOUBLE for DoubleLiteral");
    }
    // Checking if we have metaData about the double literal and if so we check to see if the data type is double
    double value = std::stod(dbLit->double_token.TokenLiteral);            // Converting the double literal from a string to a double
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), value); // Returning double value
}

llvm::Value *IRGenerator::generateArrayLiteral(Node *node)
{
    auto arrLit = dynamic_cast<ArrayLiteral *>(node);
    if (!arrLit)
        throw std::runtime_error("Invalid array literal");

    auto it = semantics.metaData.find(arrLit);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Array literal not found in metaData");

    auto sym = it->second;

    // Base element type
    llvm::Type *llvmElemTy = nullptr;

    size_t arraySize = arrLit->array.size();
    // llvm::ArrayType *arrayTy = llvm::ArrayType::get(llvmElemTy, arraySize);

    // Create the vector of element constants
    std::vector<llvm::Constant *> elems;
    elems.reserve(arraySize);

    for (size_t i = 0; i < arraySize; ++i)
    {
        Node *child = arrLit->array[i].get();
        llvm::Value *currentVal = nullptr;

        if (auto nested = dynamic_cast<ArrayLiteral *>(child))
        {
            currentVal = generateArrayLiteral(nested);
            // Recursive constant array
            llvm::Constant *nestedConst = llvm::cast<llvm::Constant>(currentVal);
            elems.push_back(nestedConst);

            if (i == 0)
            {
                llvmElemTy = nestedConst->getType();
            }
        }
        else
        {
            currentVal = generateExpression(child);

            auto *constVal = llvm::dyn_cast<llvm::Constant>(currentVal);
            if (!constVal)
                throw std::runtime_error("Array literal element is not constant");

            elems.push_back(constVal);

            if (i == 0)
            {
                llvmElemTy = constVal->getType();
            }
        }
    }

    if (elems.empty() || !llvmElemTy)
    {
        throw std::runtime_error("Cannot infer element type for array literal.");
    }

    llvm::ArrayType *arrayTy = llvm::ArrayType::get(llvmElemTy, arraySize);

    // Return a constant LLVM array
    return llvm::ConstantArray::get(arrayTy, elems);
}

llvm::Value *IRGenerator::generateNullLiteral(NullLiteral *nullLit, DataType type)
{
    switch (type)
    {
    case DataType::STRING:
        return llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0));

    case DataType::INTEGER:
        // Using minimum signed int as null marker
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, INT32_MIN, true));

    case DataType::SHORT_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, INT16_MIN, true));

    case DataType::USHORT_INT:
        // Using zero as null for unsigned
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, 0));

    case DataType::UINTEGER:
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));

    case DataType::LONG_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, INT64_MIN, true));

    case DataType::ULONG_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0));

    case DataType::EXTRA_INT:
    case DataType::UEXTRA_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt128Ty(context), llvm::APInt(128, 0));

    case DataType::FLOAT:
        return llvm::ConstantFP::getNaN(llvm::Type::getFloatTy(context));

    case DataType::DOUBLE:
        return llvm::ConstantFP::getNaN(llvm::Type::getDoubleTy(context));

    case DataType::BOOLEAN:
        return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), llvm::APInt(1, 0));

    case DataType::CHAR:
        return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0));

    case DataType::CHAR16:
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, 0));

    case DataType::CHAR32:
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));

    default:
        throw std::runtime_error("Unsupported nullable data type in generateNullLiteral");
    }
}

// Generator function for identifier expression
llvm::Value *IRGenerator::generateIdentifierExpression(Node *node)
{
    std::cout << "INSIDE IDENTIFIER GEN\n";
    auto identExpr = dynamic_cast<Identifier *>(node);
    if (!identExpr)
        throw std::runtime_error("Invalid identifier expression :" + node->toString());

    const std::string &identName = identExpr->identifier.TokenLiteral;

    // Lookup symbol
    auto metaIt = semantics.metaData.find(identExpr);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Unidentified identifier '" + identName + "'");

    auto sym = metaIt->second;
    if (sym->hasError)
        throw std::runtime_error("Semantic error detected ");

    // Get address and possible pending free
    AddressAndPendingFree addrInfo = generateIdentifierAddress(identExpr);
    llvm::Value *variableAddr = addrInfo.address;
    if (!variableAddr)
        throw std::runtime_error("No llvm address for '" + identName + "'");

    // If we have a pending free, insert it AFTER we load (we'll insert it here)
    llvm::CallInst *pendingFree = addrInfo.pendingFree;

    // Component instance -> return pointer to the struct instance (address is already correct)
    auto compIt = componentTypes.find(sym->type.resolvedName);
    if (compIt != componentTypes.end())
    {
        return variableAddr;
    }

    // Heap scalar: variableAddr is a T* (runtime pointer). Load T from it.
    if (sym->isHeap)
    {
        llvm::Type *elemTy = sym->llvmType;
        if (!elemTy)
            throw std::runtime_error("llvmType null for heap scalar '" + identName + "'");

        // variableAddr should be T* (address of object). If not, bitcast it.
        llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
        if (variableAddr->getType() != expectedPtrTy)
            variableAddr = funcBuilder.CreateBitCast(variableAddr, expectedPtrTy, identName + "_ptr_typed");

        // Load the value
        llvm::Value *loadedVal = funcBuilder.CreateLoad(elemTy, variableAddr, identName + "_val");

        // If we prepared a pending free, insert it NOW (after load)
        if (pendingFree)
        {
            // Insert the call instruction into current block before next instruction
            funcBuilder.Insert(pendingFree);
        }

        std::cout << "[DEBUG] Returning heap scalar '" << identName << "' (lastUse="
                  << (sym->lastUseNode == identExpr ? "yes" : "no") << ")\n";
        return loadedVal;
    }

    // Non-heap scalar: variableAddr is a pointer to T, just load
    llvm::Type *identType = getLLVMType(sym->type);
    if (!identType)
        throw std::runtime_error("llvmType null for scalar '" + identName + "'");

    // variableAddr should already be a pointer; load from it
    llvm::Value *val = funcBuilder.CreateLoad(identType, variableAddr, identName + "_val");

    std::cout << "ENDED IDENTIFIER GEN\n";
    return val;
}

llvm::Value *IRGenerator::generateAddressExpression(Node *node)
{
    auto addrExpr = dynamic_cast<AddressExpression *>(node);
    if (!addrExpr)
        throw std::runtime_error("Invalid address expression");

    std::cout << "Inside the address expression generator\n";

    const std::string &name = addrExpr->identifier->expression.TokenLiteral;

    auto metaIt = semantics.metaData.find(addrExpr);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Unidentified address identifier '" + name + "'");

    auto sym = metaIt->second;

    if (sym->hasError)
        throw std::runtime_error("Semantic error detected");

    llvm::Value *ptr = generateIdentifierAddress(addrExpr->identifier.get()).address;
    if (!ptr)
        throw std::runtime_error("No llvm value was assigned");

    std::cout << "Exited address expression generator\n";
    sym->llvmValue = ptr;
    sym->llvmType = getLLVMType(sym->type);
    return ptr;
}

llvm::Value *IRGenerator::generateDereferenceExpression(Node *node)
{
    auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
    if (!derefExpr)
        throw std::runtime_error("Invalid dereference expression");

    Node *operandNode = derefExpr->identifier.get();
    if (!operandNode)
        throw std::runtime_error("Dereference has no operand");

    const std::string &name = semantics.extractIdentifierName(operandNode);

    auto metaIt = semantics.metaData.find(derefExpr);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Unidentified identifier '" + name + "'");

    auto derefSym = metaIt->second;
    if (!derefSym)
        throw std::runtime_error("Unidentified dereference operand '" + name + "'");
    if (derefSym->hasError)
        throw std::runtime_error("Semantic error detected");

    // Get the pointer value
    llvm::Value *value = nullptr;
    if (auto identNode = dynamic_cast<Identifier *>(operandNode))
    {
        value = generateIdentifierAddress(identNode).address;
    }
    else
    {
        value = generateAddress(operandNode);
    }

    if (!value || !value->getType()->isPointerTy())
        throw std::runtime_error("Operand is not a pointer");

    // Final element type (type after all derefs)
    llvm::Type *targetType = getLLVMType(derefSym->type);
    if (!targetType)
        throw std::runtime_error("Failed to get element type");

    // Iteratively load until we reach the final type
    llvm::Type *currentTy = getLLVMType(derefSym->derefPtrType); // pointer type
    llvm::Type *targetTy = getLLVMType(derefSym->type);          // final type after all derefs

    int step = 0;
    while (currentTy != targetTy)
    {
        // Load from pointer using LLVM; opaque pointers are fine
        value = funcBuilder.CreateLoad(currentTy, value, name + "_step" + std::to_string(step));

        // Advance the type using metadata for next deref
        currentTy = getLLVMType(derefSym->type);
        ++step;
    }
    return value;
}

llvm::Value *IRGenerator::generateDereferenceAddress(Node *node)
{
    auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
    if (!derefExpr)
        throw std::runtime_error("Invalid dereference expression");

    Node *operandNode = derefExpr->identifier.get();
    if (!operandNode)
        throw std::runtime_error("Dereference has no operand");

    const std::string &name = semantics.extractIdentifierName(operandNode);

    auto metaIt = semantics.metaData.find(derefExpr);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Unidentified identifier '" + name + "' in dereference expression");

    auto derefSym = metaIt->second;
    if (!derefSym)
        throw std::runtime_error("Unidentified dereference operand '" + name + "'");

    if (derefSym->hasError)
        throw std::runtime_error("Semantic error detected");

    llvm::Value *ptrValue = nullptr;

    if (auto identNode = dynamic_cast<Identifier *>(operandNode))
    {
        AddressAndPendingFree addrAndFree = generateIdentifierAddress(identNode);
        ptrValue = addrAndFree.address;
    }
    else
    {
        ptrValue = generateAddress(operandNode);
    }

    if (!ptrValue || !ptrValue->getType()->isPointerTy())
        throw std::runtime_error("Operand is not a pointer");

    llvm::Type *pointerTy = getLLVMType(derefSym->derefPtrType);
    if (!pointerTy || !pointerTy->isPointerTy())
        throw std::runtime_error("Cannot dereference non-pointer type");

    llvm::Value *loadedPointer = funcBuilder.CreateLoad(pointerTy, ptrValue, name + "_addr");
    if (!loadedPointer)
        throw std::runtime_error("Failed to load pointer address");

    return loadedPointer;
}

AddressAndPendingFree IRGenerator::generateIdentifierAddress(Node *node)
{
    AddressAndPendingFree out{nullptr, nullptr};

    auto identExpr = dynamic_cast<Identifier *>(node);
    if (!identExpr)
        throw std::runtime_error("Invalid identifier expression: " + node->toString());

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

    // Component instance -> pointer to struct
    auto compIt = componentTypes.find(sym->type.resolvedName);
    if (compIt != componentTypes.end())
    {
        out.address = variablePtr; // already a pointer to struct instance
    }
    else
    {
        // scalar/heap -> ensure typed pointer
        if (sym->isHeap)
        {
            std::cout << "The identifier is heap raised\n";
            llvm::Type *elemTy = sym->llvmType;
            if (!elemTy)
                throw std::runtime_error("llvmType null for heap scalar '" + identName + "'");

            // If sym->llvmValue is a GlobalVariable (the module slot that stores T*), we must load the runtime pointer from it.
            if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(variablePtr))
            {
                std::cout << "Inside global heap var path PATH1 \n";
                llvm::PointerType *ptrType = llvm::PointerType::get(funcBuilder.getContext(), 0); // Generic ptr type
                llvm::Value *runtimePtr = funcBuilder.CreateLoad(ptrType, gv, identName + "_runtime_ptr");
                out.address = runtimePtr; // address usable for subsequent loads/stores
            }
            else
            {
                std::cout << "Inside global heap var path PATH2c \n";
                // variablePtr might already be a direct pointer (e.g., alloca or bitcast), ensure type matches
                llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
                if (variablePtr->getType() != expectedPtrTy)
                    variablePtr = funcBuilder.CreateBitCast(variablePtr, expectedPtrTy, identName + "_ptr_typed");
                out.address = variablePtr;
            }
        }
        else
        {
            if (variablePtr->getType()->isPointerTy())
            {
                std::cout << "Taken raw_ptr path\n";
                out.address = variablePtr;
            }
            else
            {
                throw std::runtime_error("Identifier '" + identName + "' does not have pointer-like llvmValue");
            }
        }
    }

    // Prepare pending free call (create CallInst but don't insert)
    if (sym->isHeap)
    {
        if ((sym->lastUseNode == identExpr) && (sym->refCount == 0))
        {
            llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
                "sage_free",
                llvm::Type::getVoidTy(context));

            llvm::CallInst *callInst = llvm::CallInst::Create(sageFreeFn);
            callInst->setCallingConv(llvm::CallingConv::C);
            // Not inserted yet — caller will insert before/after as needed
            out.pendingFree = callInst;
        }
    }

    return out;
}

llvm::Value *IRGenerator::generateSelfExpression(Node *node)
{
    auto selfExpr = dynamic_cast<SelfExpression *>(node);
    if (!selfExpr)
        throw std::runtime_error("Invalid self expression");

    std::cout << "[IR] Generating IR for self expression: " << selfExpr->toString() << "\n";

    const std::string &compName = currentComponent->component_name->expression.TokenLiteral;

    // Lookup LLVM struct for top-level component
    llvm::StructType *currentStructTy = nullptr;
    auto it = componentTypes.find(compName);
    if (it == componentTypes.end())
        throw std::runtime_error("Component '" + compName + "' not found in componentTypes");

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

    for (size_t i = 0; i < selfExpr->fields.size(); ++i)
    {
        auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
        if (!ident)
            throw std::runtime_error("Expected identifier in self chain");

        std::string fieldName = ident->identifier.TokenLiteral;
        std::cout << "[IR] Accessing field: " << fieldName
                  << " in type: " << currentTypeInfo->type.resolvedName << "\n";

        auto memIt = currentTypeInfo->members.find(fieldName);
        if (memIt == currentTypeInfo->members.end())
            throw std::runtime_error("Field not found in CustomTypeInfo");

        lastMemberInfo = memIt->second;

        // --- GEP for this field ---
        auto llvmIt = llvmCustomTypes.find(currentTypeInfo->type.resolvedName);
        if (llvmIt == llvmCustomTypes.end())
            throw std::runtime_error("LLVM struct missing for type " + currentTypeInfo->type.resolvedName);

        llvm::StructType *structTy = llvmIt->second;

        currentPtr = funcBuilder.CreateStructGEP(
            structTy,
            currentPtr,
            lastMemberInfo->memberIndex,
            fieldName + "_ptr");

        // --- Drill into nested type if needed ---
        if (lastMemberInfo->type.kind == DataType::COMPONENT ||
            lastMemberInfo->type.kind == DataType::DATABLOCK)
        {
            auto nestedIt = semantics.customTypesTable.find(lastMemberInfo->type.resolvedName);
            if (nestedIt == semantics.customTypesTable.end())
                throw std::runtime_error("Nested type not found in customTypesTable");

            currentTypeInfo = nestedIt->second;
        }
        else
        {
            // primitive reached, stop drilling
            currentTypeInfo = nullptr;
        }
    }

    // --- Final load ---
    llvm::Type *finalTy = getLLVMType(lastMemberInfo->type);
    return funcBuilder.CreateLoad(finalTy, currentPtr, selfExpr->fields.back()->toString() + "_val");
}

llvm::Value *IRGenerator::generateSelfAddress(Node *node)
{
    auto selfExpr = dynamic_cast<SelfExpression *>(node);
    if (!selfExpr)
        throw std::runtime_error("Invalid self expression");

    std::cout << "[IR] Generating IR for self address: " << selfExpr->toString() << "\n";

    const std::string &compName = currentComponent->component_name->expression.TokenLiteral;

    // Lookup LLVM struct for top-level component
    llvm::StructType *currentStructTy = nullptr;
    auto it = componentTypes.find(compName);
    if (it == componentTypes.end())
        throw std::runtime_error("Component '" + compName + "' not found in componentTypes");

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

    for (size_t i = 0; i < selfExpr->fields.size(); ++i)
    {
        auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
        if (!ident)
            throw std::runtime_error("Expected identifier in self chain");

        std::string fieldName = ident->identifier.TokenLiteral;
        std::cout << "[IR] Accessing field: " << fieldName
                  << " in type: " << currentTypeInfo->type.resolvedName << "\n";

        auto memIt = currentTypeInfo->members.find(fieldName);
        if (memIt == currentTypeInfo->members.end())
            throw std::runtime_error("Field not found in CustomTypeInfo");

        lastMemberInfo = memIt->second;

        // --- GEP for this field ---
        auto llvmIt = llvmCustomTypes.find(currentTypeInfo->type.resolvedName);
        if (llvmIt == llvmCustomTypes.end())
            throw std::runtime_error("LLVM struct missing for type " + currentTypeInfo->type.resolvedName);

        llvm::StructType *structTy = llvmIt->second;

        currentPtr = funcBuilder.CreateStructGEP(
            structTy,
            currentPtr,
            lastMemberInfo->memberIndex,
            fieldName + "_ptr");

        // --- Drill into nested type if needed ---
        if (lastMemberInfo->type.kind == DataType::COMPONENT ||
            lastMemberInfo->type.kind == DataType::DATABLOCK)
        {
            auto nestedIt = semantics.customTypesTable.find(lastMemberInfo->type.resolvedName);
            if (nestedIt == semantics.customTypesTable.end())
                throw std::runtime_error("Nested type not found in customTypesTable");

            currentTypeInfo = nestedIt->second;
        }
        else
        {
            // primitive reached, stop drilling
            currentTypeInfo = nullptr;
        }
    }

    return currentPtr;
}

llvm::Value *IRGenerator::generateBlockExpression(Node *node)
{
    auto blockExpr = dynamic_cast<BlockExpression *>(node);
    if (!blockExpr)
        throw std::runtime_error("Invalid block expression");

    for (const auto &stmts : blockExpr->statements)
    {
        // Check if the current block is already terminated by a return or branch
        if (funcBuilder.GetInsertBlock()->getTerminator())
        {
            std::cout << "SKIPPING statement - block terminated\n";
            break;
        }
        generateStatement(stmts.get());
    }

    // A block expression should not return an llvm::Value directly.
    return nullptr;
}

// Generator function for function expression
llvm::Value *IRGenerator::generateFunctionExpression(Node *node)
{
    auto fnExpr = dynamic_cast<FunctionExpression *>(node);
    if (!fnExpr)
        throw std::runtime_error("Invalid function expression");

    // If node has an error we wont generate IR
    auto funcIt = semantics.metaData.find(fnExpr);
    if (funcIt == semantics.metaData.end())
    {
        throw std::runtime_error("Function expression does not exist");
    }
    if (funcIt->second->hasError)
    {
        throw std::runtime_error("Semantic error detected");
    }

    // Getting the function signature
    auto fnName = fnExpr->func_key.TokenLiteral;

    isGlobalScope = false;

    // Building the function type
    std::vector<llvm::Type *> llvmParamTypes;

    for (auto &p : fnExpr->call)
    {
        // Getting the data type to push it into getLLVMType
        auto it = semantics.metaData.find(p.get());
        if (it == semantics.metaData.end())
        {
            throw std::runtime_error("Missing parameter meta data");
        }
        it->second->llvmType = getLLVMType(it->second->type);
        llvmParamTypes.push_back(getLLVMType(it->second->type));
    }

    // Getting the function return type
    auto fnRetType = fnExpr->return_type.get();

    auto retType = semantics.inferNodeDataType(fnRetType);
    llvm::FunctionType *funcType = llvm::FunctionType::get(lowerFunctionType(retType), llvmParamTypes, false);

    // Look up if the function declaration exists
    llvm::Function *fn = module->getFunction(fnName);
    // If the function declaration exists
    if (!fn)
    {
        fn = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, fnName, module.get());
    }
    else
    {
        // If the function was declared checking if the return types match
        if (fn->getFunctionType() != funcType)
        {
            throw std::runtime_error("Function redefinition for '" + fnName + "' with different signature ");
        }
    }

    currentFunction = fn; // Updating the currentFunction pointer
    if (!currentFunction)
    {
        std::cerr << "Current function pointer updated \n";
    }

    // Creating the entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
    funcBuilder.SetInsertPoint(entry);

    // Binding parameters to namedValues
    auto argIter = fn->arg_begin();
    for (auto &p : fnExpr->call)
    {
        // Getting the statement data type
        auto pIt = semantics.metaData.find(p.get());
        if (pIt == semantics.metaData.end())
        {
            throw std::runtime_error("Failed to find paremeter meta data");
        }
        llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(getLLVMType(pIt->second->type), nullptr, p->statement.TokenLiteral);
        funcBuilder.CreateStore(&(*argIter), alloca);
        pIt->second->llvmValue = alloca;

        argIter++;
    }

    llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();

    // This will handle whatever is inside the block including the return value
    generateExpression(fnExpr->block.get());

    llvm::BasicBlock *finalBlock = funcBuilder.GetInsertBlock();

    // If the function is void
    bool isVoidFunction = funcIt->second->returnType.kind == DataType::VOID;

    // CRITICAL CHECK: Does the current block exist and is it not terminated?
    if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator()))
    {
        if (isVoidFunction)
        {
            // Inject 'ret void' for void functions that fell off the end.
            funcBuilder.CreateRetVoid();
            std::cout << "INJECTED missing 'ret void' terminator.\n";
        }
        else
        {
            // Non-void function finished without a return.
            // This is a semantic failure, but we terminate for LLVM stability.
            funcBuilder.CreateUnreachable();
        }
    }

    if (oldInsertPoint)
    {
        funcBuilder.SetInsertPoint(oldInsertPoint);
    }
    else
    {
        funcBuilder.ClearInsertionPoint();
    }

    isGlobalScope = true;      // Reset the flag
    currentFunction = nullptr; // Set it back to a null pointer
    return fn;
}

llvm::Value *IRGenerator::generateCallExpression(Node *node)
{
    auto callExpr = dynamic_cast<CallExpression *>(node);
    if (!callExpr)
    {
        throw std::runtime_error("Invalid call expression");
    }

    if (isGlobalScope)
        throw std::runtime_error("Function calls are not allowed at global scope");

    // Getting the function name
    const std::string &fnName = callExpr->function_identifier->expression.TokenLiteral;

    auto callIt = semantics.metaData.find(callExpr);
    if (callIt == semantics.metaData.end())
    {
        throw std::runtime_error("Call expression does not exist");
    }
    if (callIt->second->hasError)
        throw std::runtime_error("Semantic error detected in '" + fnName + "' call");

    // Getting the function I want to call
    llvm::Function *calledFunc = module->getFunction(fnName);

    if (!calledFunc)
    {
        throw std::runtime_error("Unknown function '" + fnName + "'referenced");
    }

    // Generate IR for each argument
    std::vector<llvm::Value *> argsV;
    for (const auto &arg : callExpr->parameters)
    {
        llvm::Value *argVal = generateExpression(arg.get());
        if (!argVal)
        {
            throw std::runtime_error("Argument codegen failed");
        }
        argsV.push_back(argVal);
    }

    // Emitting the function call itself
    llvm::Value *call = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

    // Check if the function return type is void
    if (calledFunc->getReturnType()->isVoidTy())
    {
        return nullptr;
    }

    // If the return is nullable
    if (callIt->second->returnType.isNull)
    {
        // Extract the success value
        return funcBuilder.CreateExtractValue(call, {0}, "success");
    }
    return call;
}

llvm::Value *IRGenerator::generateArraySubscriptAddress(Node *node)
{
    auto arrExpr = dynamic_cast<ArraySubscript *>(node);
    if (!arrExpr)
        throw std::runtime_error("Invalid array access expression");

    auto arrMetaIt = semantics.metaData.find(arrExpr);
    if (arrMetaIt == semantics.metaData.end())
    {
        throw std::runtime_error("Could not find array subscript metaData");
    }
    auto arrSym = arrMetaIt->second;

    if (arrSym->hasError)
    {
        throw std::runtime_error("Semantic error was detected in array subscript");
    }

    llvm::Value *BaseArrayPtr = generateIdentifierAddress(arrExpr->identifier.get()).address;

    llvm::PointerType *PtrTy = llvm::cast<llvm::PointerType>(BaseArrayPtr->getType());

    llvm::Type *arrayType = nullptr;

    if (llvm::AllocaInst *Alloca = llvm::dyn_cast<llvm::AllocaInst>(BaseArrayPtr))
    {
        arrayType = Alloca->getAllocatedType();
    }
    else if (llvm::PointerType *PtrTy = llvm::dyn_cast<llvm::PointerType>(BaseArrayPtr->getType()))
    {
        arrayType = PtrTy->getContainedType(0);
    }
    else
    {
        throw std::runtime_error("Base array pointer is not a recognized pointer type.");
    }

    // Check if the resulting type is indeed an ArrayType, as expected for GEP.
    if (!llvm::isa<llvm::ArrayType>(arrayType))
    {
        throw std::runtime_error("GEP base must point to an aggregate type (Array or Struct).");
    }

    // i32 constant 0 for the first index
    llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(BaseArrayPtr->getContext()), 0);

    std::vector<llvm::Value *> indices;
    indices.push_back(Zero);

    // Generate llvm value for each index expression
    for (const auto &index_expr : arrExpr->index_exprs)
    {
        llvm::Value *currentIndex = generateExpression(index_expr.get());

        if (!currentIndex->getType()->isIntegerTy())
            throw std::runtime_error("Array index must be an integer");

        indices.push_back(currentIndex);
    }

    llvm::Value *ElementPtr = funcBuilder.CreateGEP(
        arrayType,    // The type of the aggregate being pointed to (e.g., [2 x [3 x i32]])
        BaseArrayPtr, // The base pointer (%my_matrix)
        indices,      // The full list of indices: {0, I1, I2, ...}
        "element_ptr" // Name for the resulting pointer
    );

    return ElementPtr;
}

llvm::Value *IRGenerator::generateArraySubscriptExpression(Node *node)
{
    auto arrExpr = dynamic_cast<ArraySubscript *>(node);
    if (!arrExpr)
        throw std::runtime_error("Invalid array access expression");

    auto arrMetaIt = semantics.metaData.find(arrExpr);
    if (arrMetaIt == semantics.metaData.end())
    {
        throw std::runtime_error("Could not find array subscript metaData");
    }
    auto arrSym = arrMetaIt->second;

    if (arrSym->hasError)
    {
        throw std::runtime_error("Semantic error was detected in array subscript");
    }

    llvm::Value *ptr = generateArraySubscriptAddress(node);

    llvm::Type *elemTy = getLLVMType(arrSym->type);

    return funcBuilder.CreateLoad(elemTy, ptr, "arr_elem_load");
}

llvm::Value *IRGenerator::generateCallAddress(Node *node)
{
    auto callExpr = dynamic_cast<CallExpression *>(node);
    if (!callExpr)
        throw std::runtime_error("Invalid call expression inside address generator");

    if (isGlobalScope)
        throw std::runtime_error("Function calls are not allowed in global scope");

    const std::string &funcName = callExpr->function_identifier->expression.TokenLiteral;

    auto callIt = semantics.metaData.find(callExpr);
    if (callIt == semantics.metaData.end())
        throw std::runtime_error("Call expression metaData not found");

    if (callIt->second->hasError)
        throw std::runtime_error("Semantic error detected in call for '" + funcName + "'");

    llvm::Function *calledFunc = module->getFunction(funcName);
    if (!calledFunc)
        throw std::runtime_error("Unknown function '" + funcName + "' referenced");

    // Generate IR for each argument
    std::vector<llvm::Value *> argsV;
    for (const auto &arg : callExpr->parameters)
    {
        llvm::Value *argVal = generateExpression(arg.get());
        if (!argVal)
            throw std::runtime_error("Argument codegen failed");
        argsV.push_back(argVal);
    }

    llvm::Type *retTy = calledFunc->getReturnType();
    if (retTy->isVoidTy())
        throw std::runtime_error("Attempted to take address of a void call result");

    // Ensure the temporary alloca is created in the entry block of the current function.
    llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&currentFunction->getEntryBlock(),
                                 currentFunction->getEntryBlock().begin());
    llvm::AllocaInst *tmpAlloca = tmpBuilder.CreateAlloca(retTy, nullptr, "calltmp.addr");

    // Emit the call (value returned into caller)
    llvm::Value *callVal = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

    // Store the returned value into the caller-owned temporary and return its address.
    funcBuilder.CreateStore(callVal, tmpAlloca);

    return tmpAlloca; // pointer to caller-allocated storage containing the call result
}

llvm::Value *IRGenerator::generateUnwrapCallExpression(Node *node)
{
    auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node);
    if (!unwrapExpr)
        throw std::runtime_error("Invalid unwrap expression call");

    auto unIt = semantics.metaData.find(unwrapExpr);
    if (unIt == semantics.metaData.end())
        throw std::runtime_error("Missing unwrap metaData");

    if (unIt->second->hasError)
        throw std::runtime_error("Semantic error detected");

    if (!unIt->second->returnType.isNull)
        throw std::runtime_error("Cannot unwrap a non-nullable function return");

    auto call = dynamic_cast<CallExpression *>(unwrapExpr->call.get());
    const std::string &fnName = call->function_identifier->expression.TokenLiteral;

    llvm::Function *calledFn = module->getFunction(fnName);

    if (!calledFn)
    {
        throw std::runtime_error("Unknown function '" + fnName + "' referenced");
    }

    // Generate IR for each argument
    std::vector<llvm::Value *> argsV;
    for (const auto &arg : call->parameters)
    {
        llvm::Value *argVal = generateExpression(arg.get());
        if (!argVal)
        {
            throw std::runtime_error("Argument codegen failed");
        }
        argsV.push_back(argVal);
    }

    llvm::Value *raw = funcBuilder.CreateCall(calledFn, argsV, "unwrap_calltmp");

    // Return the error value
    return funcBuilder.CreateExtractValue(raw, {1}, "error");
}

// HELPER FUNCTIONS
llvm::Type *IRGenerator::getLLVMType(ResolvedType type)
{
    llvm::Type *baseType = nullptr;

    switch (type.kind)
    {
    case DataType::SHORT_INT:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::USHORT_INT:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::INTEGER:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::UINTEGER:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::LONG_INT:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }

    case DataType::ULONG_INT:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }

    case DataType::EXTRA_INT:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }

    case DataType::UEXTRA_INT:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }

    case DataType::BOOLEAN:
    {
        baseType = llvm::Type::getInt1Ty(context);
        break;
    }

    case DataType::CHAR:
    {
        baseType = llvm::Type::getInt8Ty(context);
        break;
    }

    case DataType::CHAR16:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::CHAR32:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::FLOAT:
    {
        baseType = llvm::Type::getFloatTy(context);
        break;
    }

    case DataType::DOUBLE:
    {
        baseType = llvm::Type::getDoubleTy(context);
        break;
    }

    case DataType::STRING:
    {
        baseType = llvm::PointerType::get(context, 0);
        break;
    }

    case DataType::VOID:
    {
        baseType = llvm::Type::getVoidTy(context);
        break;
    }

    case DataType::DATABLOCK:
    case DataType::COMPONENT:
    {
        if (type.resolvedName.empty())
            throw std::runtime_error("Custom type requested but resolvedName is empty");

        auto it = llvmCustomTypes.find(type.resolvedName);
        if (it != llvmCustomTypes.end())
            baseType = it->second;
        else
            throw std::runtime_error("LLVM IR requested for unknown custom type '" + type.resolvedName + "'");
        break;
    }

    case DataType::ENUM:
    {
        auto enumInfo = semantics.customTypesTable[type.resolvedName];
        baseType = getLLVMType({enumInfo->underLyingType, ""});
        break;
    }

    case DataType::ERROR:
    case DataType::GENERIC:
    case DataType::UNKNOWN:
        throw std::runtime_error("Unsupported or unknown data type encountered in getLLVMType");
    }

    // Wrap in a pointer if isPointer is true
    if (type.isPointer)
        return llvm::PointerType::get(baseType, 0);

    // Wrap in a pointer if isRef is true
    if (type.isRef)
        return llvm::PointerType::get(baseType, 0);

    return baseType;
}

llvm::Type *IRGenerator::lowerFunctionType(const ResolvedType &type)
{
    // If the type isnt nullable just use then normal mapper
    if (!type.isNull)
        return getLLVMType(type);

    // If the type is nullable use the hidden struct that holds the success value and error value
    llvm::Type *valueTy = getLLVMType({type.kind, type.resolvedName, /*isPtr*/ false, /*isRef*/ false, /*isNull*/ false});

    // The error value has the same type as the value
    llvm::Type *errorTy = valueTy;

    std::vector<llvm::Type *> members = {valueTy, errorTy};
    return llvm::StructType::get(context, members, false); // not packed
}

// Registering generator functions for statements
void IRGenerator::registerGeneratorFunctions()
{
    generatorFunctionsMap[typeid(LetStatement)] = &IRGenerator::generateLetStatement;
    generatorFunctionsMap[typeid(ReferenceStatement)] = &IRGenerator::generateReferenceStatement;
    generatorFunctionsMap[typeid(PointerStatement)] = &IRGenerator::generatePointerStatement;
    generatorFunctionsMap[typeid(ExpressionStatement)] = &IRGenerator::generateExpressionStatement;
    generatorFunctionsMap[typeid(AssignmentStatement)] = &IRGenerator::generateAssignmentStatement;
    generatorFunctionsMap[typeid(FieldAssignment)] = &IRGenerator::generateFieldAssignmentStatement;
    generatorFunctionsMap[typeid(WhileStatement)] = &IRGenerator::generateWhileStatement;
    generatorFunctionsMap[typeid(ForStatement)] = &IRGenerator::generateForStatement;
    generatorFunctionsMap[typeid(ifStatement)] = &IRGenerator::generateIfStatement;
    generatorFunctionsMap[typeid(BreakStatement)] = &IRGenerator::generateBreakStatement;
    generatorFunctionsMap[typeid(ContinueStatement)] = &IRGenerator::generateContinueStatement;
    generatorFunctionsMap[typeid(BlockStatement)] = &IRGenerator::generateBlockStatement;
    generatorFunctionsMap[typeid(FunctionStatement)] = &IRGenerator::generateFunctionStatement;
    generatorFunctionsMap[typeid(ReturnStatement)] = &IRGenerator::generateReturnStatement;
    generatorFunctionsMap[typeid(FunctionDeclaration)] = &IRGenerator::generateFunctionDeclaration;
    // Special case
    generatorFunctionsMap[typeid(FunctionDeclarationExpression)] = &IRGenerator::generateFunctionDeclarationExpression;
    // Component system
    generatorFunctionsMap[typeid(DataStatement)] = &IRGenerator::generateDataStatement;
    generatorFunctionsMap[typeid(ComponentStatement)] = &IRGenerator::generateComponentStatement;
    generatorFunctionsMap[typeid(EnumClassStatement)] = &IRGenerator::generateEnumClassStatement;

    generatorFunctionsMap[typeid(ArrayStatement)] = &IRGenerator::generateArrayStatement;
    generatorFunctionsMap[typeid(ShoutStatement)] = &IRGenerator::generateShoutStatement;
    generatorFunctionsMap[typeid(QualifyStatement)] = &IRGenerator::generateQualifyStatement;
    generatorFunctionsMap[typeid(InstantiateStatement)] = &IRGenerator::generateInstantiateStatement;
    generatorFunctionsMap[typeid(SealStatement)] = &IRGenerator::generateSealStatement;
}

void IRGenerator::registerAddressGeneratorFunctions()
{
    addressGeneratorsMap[typeid(SelfExpression)] = &IRGenerator::generateSelfAddress;
    addressGeneratorsMap[typeid(CallExpression)] = &IRGenerator::generateCallAddress;
    addressGeneratorsMap[typeid(ArraySubscript)] = &IRGenerator::generateArraySubscriptAddress;
    addressGeneratorsMap[typeid(DereferenceExpression)] = &IRGenerator::generateDereferenceAddress;
}

void IRGenerator::registerExpressionGeneratorFunctions()
{
    expressionGeneratorsMap[typeid(InfixExpression)] = &IRGenerator::generateInfixExpression;
    expressionGeneratorsMap[typeid(PrefixExpression)] = &IRGenerator::generatePrefixExpression;
    expressionGeneratorsMap[typeid(PostfixExpression)] = &IRGenerator::generatePostfixExpression;
    expressionGeneratorsMap[typeid(StringLiteral)] = &IRGenerator::generateStringLiteral;
    expressionGeneratorsMap[typeid(CharLiteral)] = &IRGenerator::generateCharLiteral;
    expressionGeneratorsMap[typeid(Char16Literal)] = &IRGenerator::generateChar16Literal;
    expressionGeneratorsMap[typeid(Char32Literal)] = &IRGenerator::generateChar32Literal;
    expressionGeneratorsMap[typeid(BooleanLiteral)] = &IRGenerator::generateBooleanLiteral;
    expressionGeneratorsMap[typeid(ShortLiteral)] = &IRGenerator::generateShortLiteral;
    expressionGeneratorsMap[typeid(UnsignedShortLiteral)] = &IRGenerator::generateUnsignedShortLiteral;
    expressionGeneratorsMap[typeid(IntegerLiteral)] = &IRGenerator::generateIntegerLiteral;
    expressionGeneratorsMap[typeid(UnsignedIntegerLiteral)] = &IRGenerator::generateUnsignedIntegerLiteral;
    expressionGeneratorsMap[typeid(LongLiteral)] = &IRGenerator::generateLongLiteral;
    expressionGeneratorsMap[typeid(UnsignedLongLiteral)] = &IRGenerator::generateUnsignedLongLiteral;
    expressionGeneratorsMap[typeid(ExtraLiteral)] = &IRGenerator::generateExtraLiteral;
    expressionGeneratorsMap[typeid(UnsignedExtraLiteral)] = &IRGenerator::generateUnsignedExtraLiteral;
    expressionGeneratorsMap[typeid(FloatLiteral)] = &IRGenerator::generateFloatLiteral;
    expressionGeneratorsMap[typeid(DoubleLiteral)] = &IRGenerator::generateDoubleLiteral;
    expressionGeneratorsMap[typeid(ArrayLiteral)] = &IRGenerator::generateArrayLiteral;
    expressionGeneratorsMap[typeid(Identifier)] = &IRGenerator::generateIdentifierExpression;
    expressionGeneratorsMap[typeid(AddressExpression)] = &IRGenerator::generateAddressExpression;
    expressionGeneratorsMap[typeid(DereferenceExpression)] = &IRGenerator::generateDereferenceExpression;
    expressionGeneratorsMap[typeid(BlockExpression)] = &IRGenerator::generateBlockExpression;
    expressionGeneratorsMap[typeid(CallExpression)] = &IRGenerator::generateCallExpression;
    expressionGeneratorsMap[typeid(UnwrapExpression)] = &IRGenerator::generateUnwrapCallExpression;
    expressionGeneratorsMap[typeid(MethodCallExpression)] = &IRGenerator::generateMethodCallExpression;
    expressionGeneratorsMap[typeid(SelfExpression)] = &IRGenerator::generateSelfExpression;
    expressionGeneratorsMap[typeid(InstanceExpression)] = &IRGenerator::generateInstanceExpression;
    expressionGeneratorsMap[typeid(ArraySubscript)] = &IRGenerator::generateArraySubscriptExpression;
}

char IRGenerator::decodeCharLiteral(const std::string &literal)
{
    if (literal.length() == 3 && literal.front() == '\'' && literal.back() == '\'')
    {
        return literal[1];
    }
    else if (literal.length() == 4 && literal.front() == '\'' && literal.back() == '\'' && literal[1] == '\\')
    {
        switch (literal[2])
        {
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case '\\':
            return '\\';
        case '\'':
            return '\'';
        case '\"':
            return '\"';
        case 'r':
            return '\r';
        case '0':
            return '\0';
        default:
            throw std::runtime_error("Unknown escape sequence in char literal: " + literal);
        }
    }
    throw std::runtime_error("Invalid char literal: " + literal);
}

uint16_t IRGenerator::decodeChar16Literal(const std::string &literal)
{
    // Example formats: 'A', '\n', '\u1234' (unicode escape)

    if (literal.length() == 3 && literal.front() == '\'' && literal.back() == '\'')
    {
        return static_cast<uint16_t>(literal[1]);
    }
    else if (literal.length() == 8 && literal.substr(0, 2) == "'\\u" && literal.back() == '\'')
    {
        std::string hex = literal.substr(3, 4); // 4 hex digits
        return static_cast<uint16_t>(std::stoi(hex, nullptr, 16));
    }
    else if (literal.length() == 4 && literal.front() == '\'' && literal.back() == '\'' && literal[1] == '\\')
    {
        // Simple escapes like '\n'
        switch (literal[2])
        {
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case '\\':
            return '\\';
        case '\'':
            return '\'';
        case '\"':
            return '\"';
        case 'r':
            return '\r';
        case '0':
            return '\0';
        default:
            throw std::runtime_error("Unknown escape sequence in char16 literal: " + literal);
        }
    }
    throw std::runtime_error("Invalid char16 literal: " + literal);
}

// Decodes UTF-32 char32 literals (returns uint32_t)
uint32_t IRGenerator::decodeChar32Literal(const std::string &literal)
{
    // Example formats: 'A', '\U0001F600' (unicode escape for emoji)

    if (literal.length() == 3 && literal.front() == '\'' && literal.back() == '\'')
    {
        return static_cast<uint32_t>(literal[1]);
    }
    else if (literal.length() == 12 && literal.substr(0, 2) == "'\\U" && literal.back() == '\'')
    {
        std::string hex = literal.substr(3, 8); // 8 hex digits
        return static_cast<uint32_t>(std::stoul(hex, nullptr, 16));
    }
    else if (literal.length() == 4 && literal.front() == '\'' && literal.back() == '\'' && literal[1] == '\\')
    {
        // Simple escapes like '\n'
        switch (literal[2])
        {
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case '\\':
            return '\\';
        case '\'':
            return '\'';
        case '\"':
            return '\"';
        case 'r':
            return '\r';
        case '0':
            return '\0';
        default:
            throw std::runtime_error("Unknown escape sequence in char32 literal: " + literal);
        }
    }
    throw std::runtime_error("Invalid char32 literal: " + literal);
}

bool IRGenerator::isIntegerType(DataType dt)
{
    switch (dt)
    {
    case DataType::SHORT_INT:
    case DataType::USHORT_INT:
    case DataType::INTEGER:
    case DataType::UINTEGER:
    case DataType::LONG_INT:
    case DataType::ULONG_INT:
    case DataType::EXTRA_INT:
    case DataType::UEXTRA_INT:
        return true;
    default:
        return false;
    }
}

bool IRGenerator::isSignedInteger(DataType dt)
{
    switch (dt)
    {
    case DataType::SHORT_INT:
    case DataType::INTEGER:
    case DataType::LONG_INT:
    case DataType::EXTRA_INT:
        return true;
    default:
        return false;
    }
}

unsigned IRGenerator::getIntegerBitWidth(DataType dt)
{
    switch (dt)
    {
    case DataType::SHORT_INT:
    case DataType::USHORT_INT:
        return 16;
    case DataType::INTEGER:
    case DataType::UINTEGER:
        return 32;
    case DataType::LONG_INT:
    case DataType::ULONG_INT:
        return 64;
    case DataType::EXTRA_INT:
    case DataType::UEXTRA_INT:
        return 128;
    default:
        return 0; // Not an integer type
    }
}

char *IRGenerator::const_unnitoa(__int128 val, char *buf)
{
    char temp[64]; // Enough for 128-bit decimal digits (max ~39 digits).
    int i = 0;
    int neg = 0;

    if (val == 0)
    {
        buf[0] = '0';
        buf[1] = '\0';
        return buf;
    }

    if (val < 0)
    {
        neg = 1;
        val = -val;
    }

    // Extract digits into temp (reversed)
    while (val > 0)
    {
        __int128 digit = val % 10;
        val /= 10;
        temp[i++] = '0' + (int)digit;
    }

    if (neg)
        temp[i++] = '-';

    // Reverse into buf
    int j = 0;
    while (i > 0)
        buf[j++] = temp[--i];

    buf[j] = '\0';
    return buf;
}

void IRGenerator::shoutRuntime(llvm::Value *val, ResolvedType type)
{
    if (!val)
        throw std::runtime_error("shout! called with null value");

    auto &ctx = module->getContext();
    auto i32Ty = llvm::IntegerType::getInt32Ty(ctx);
    auto i8Ty = llvm::IntegerType::getInt8Ty(ctx);
    auto i128Ty = llvm::IntegerType::get(module->getContext(), 128);
    auto i8PtrTy = llvm::PointerType::get(i8Ty, 0);
    auto i64Ty = llvm::IntegerType::getInt64Ty(ctx);

    llvm::Function *unnitoaFn = module->getFunction("unnitoa");
    if (!unnitoaFn)
    {
        llvm::FunctionType *unnitoaTy =
            llvm::FunctionType::get(
                i8PtrTy,           // returns char*
                {i128Ty, i8PtrTy}, // (__int128 value, char* buffer)
                false);

        unnitoaFn = llvm::Function::Create(
            unnitoaTy,
            llvm::GlobalValue::ExternalLinkage,
            "unnitoa",
            *module);
    }

    auto printString = [&](llvm::Value *strVal)
    {
        if (!strVal)
            throw std::runtime_error("printString received null llvm::Value");

        // Ensure 'write' exists
        llvm::Function *writeFn = module->getFunction("write");
        if (!writeFn)
        {
            auto writeType = llvm::FunctionType::get(i64Ty, {i32Ty, i8PtrTy, i64Ty}, false);
            writeFn = llvm::Function::Create(writeType, llvm::Function::ExternalLinkage, "write", module.get());
        }

        // Ensure 'strlen' exists
        llvm::Function *strlenFn = module->getFunction("strlen");
        if (!strlenFn)
        {
            auto strlenType = llvm::FunctionType::get(i64Ty, {i8PtrTy}, false);
            strlenFn = llvm::Function::Create(strlenType, llvm::Function::ExternalLinkage, "strlen", module.get());
        }

        auto fd = llvm::ConstantInt::get(i32Ty, 1); // stdout

        // print main string
        auto len = funcBuilder.CreateCall(strlenFn, {strVal});
        funcBuilder.CreateCall(writeFn, {fd, strVal, len});

        // print newline
        llvm::Value *newline = funcBuilder.CreateGlobalStringPtr("\n");
        auto newLen = funcBuilder.CreateCall(strlenFn, {newline});
        funcBuilder.CreateCall(writeFn, {fd, newline, newLen});
    };

    auto printInt = [&](llvm::Value *intVal)
    {
        char buf[20];

        if (auto constInt = llvm::dyn_cast<llvm::ConstantInt>(intVal))
        {
            // Compile-time constant
            const_unnitoa(static_cast<int>(constInt->getSExtValue()), buf);
            llvm::Value *strVal = funcBuilder.CreateGlobalStringPtr(buf);
            printString(strVal);
        }
        else if (intVal->getType()->isIntegerTy(32))
        {
            std::cout << "Branching to SSA print\n";
            // Promote to i128 for printing

            llvm::Value *int128 = funcBuilder.CreateIntCast(intVal, i128Ty, /*isSigned=*/true);

            // Allocate buffer
            llvm::Value *bufAlloca = funcBuilder.CreateAlloca(
                llvm::ArrayType::get(i8Ty, 64), nullptr, "int_buf");
            llvm::Value *bufPtr = funcBuilder.CreatePointerCast(bufAlloca, i8PtrTy);

            // Call the new universal printer
            funcBuilder.CreateCall(unnitoaFn, {int128, bufPtr});
            printString(bufPtr);
        }

        else
        {
            throw std::runtime_error("Unsupported integer value in shout!");
        }
    };

    auto printPointer = [&](llvm::Value *ptrVal)
    {
        llvm::Value *addrInt = funcBuilder.CreatePtrToInt(ptrVal, i128Ty);

        llvm::Value *bufAlloca = funcBuilder.CreateAlloca(
            llvm::ArrayType::get(i8Ty, 64), nullptr, "ptr_buf");
        llvm::Value *bufPtr = funcBuilder.CreatePointerCast(bufAlloca, i8PtrTy);

        funcBuilder.CreateCall(unnitoaFn, {addrInt, bufPtr});
        printString(bufPtr);
    };

    // If the expression is a pointer
    if (type.isPointer)
    {
        printPointer(val);
        return;
    }

    if (type.kind == DataType::INTEGER)
    {
        printInt(val);
    }
    else if (type.kind == DataType::STRING)
    {
        printString(val);
    }
    else
    {
        throw std::runtime_error("shout! only supports int and string for now");
    }
}

llvm::GlobalVariable *IRGenerator::createGlobalArrayConstant(llvm::Constant *constantArray)
{
    // The type of the constant must be a valid ArrayType (which can be nested)
    llvm::Type *constantType = constantArray->getType();

    llvm::ArrayType *arrayTy = llvm::cast<llvm::ArrayType>(constantArray->getType());

    if (!arrayTy)
    {
        // If the constant is NOT an ArrayType, something is fundamentally wrong
        // with the output of generateArrayLiteral.
        throw std::runtime_error("Attempted to create a global array constant from a non-array type.");
    }

    // 2. Create the Global Variable
    llvm::GlobalVariable *globalArray = new llvm::GlobalVariable(
        *module,                           // The owning module
        arrayTy,                           // The type of the global variable
        true,                              // IsConstant (Read-only)
        llvm::GlobalValue::PrivateLinkage, // Linkage
        constantArray,                     // The initializer constant value
        "array.init"                       // Name
    );
    // Set alignment for safety
    globalArray->setAlignment(llvm::MaybeAlign(arrayTy->getPrimitiveSizeInBits() / 8));
    return globalArray;
}

void IRGenerator::dumpIR()
{
    module->print(llvm::outs(), nullptr);
}

bool IRGenerator::currentBlockIsTerminated()
{
    llvm::BasicBlock *bb = funcBuilder.GetInsertBlock();
    return bb && bb->getTerminator();
}

llvm::Module &IRGenerator::getLLVMModule()
{
    return *module;
}

void IRGenerator::generateSageInitCall()
{
    // Safety check just in case
    if (isSageInitCalled)
        return;

    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(context), false);

    // --- Call sage_init upfront ---
    llvm::Function *initFunc = module->getFunction("sage_init");
    if (!initFunc)
    {
        llvm::FunctionType *initType = llvm::FunctionType::get(
            llvm::Type::getVoidTy(context),
            {llvm::Type::getInt64Ty(context)}, false);

        initFunc = llvm::Function::Create(
            initType, llvm::Function::ExternalLinkage, "sage_init", module.get());
    }

    llvm::IRBuilder<> initBuilder(heapInitFnEntry, heapInitFnEntry->begin());

    initBuilder.CreateCall(initFunc, {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), totalHeapSize)});
    std::cout << "Calling sage init \n";
    isSageInitCalled = true; // Toggle this to true to mark that sage init was called
}

void IRGenerator::generateSageDestroyCall()
{
    llvm::Function *destroyFunc = module->getFunction("sage_destroy");
    if (!destroyFunc)
    {
        llvm::FunctionType *destroyType = llvm::FunctionType::get(
            llvm::Type::getVoidTy(context), {}, false);

        destroyFunc = llvm::Function::Create(
            destroyType, llvm::Function::ExternalLinkage, "sage_destroy", module.get());
    }

    funcBuilder.CreateCall(destroyFunc, {});
    std::cout << "Calling sage destroy\n";
    isSageDestroyCalled = true; // Toggle this to true to mark that sage destroy was called
}

bool IRGenerator::emitObjectFile(const std::string &filename)
{
    // Initialization
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    // Target Setup
    std::string targetTripleStr = llvm::sys::getDefaultTargetTriple();

    module->setTargetTriple(targetTripleStr);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);
    if (!target)
    {
        llvm::errs() << "Failed to find target: " << error << "\n";
        return false;
    }

    llvm::TargetOptions opt;

    std::optional<llvm::Reloc::Model> RM = llvm::Reloc::PIC_;

    auto targetMachine = target->createTargetMachine(
        targetTripleStr,
        "generic", // CPU name
        "",        // Features string
        opt,
        RM);

    if (!targetMachine)
    {
        llvm::errs() << "Failed to create TargetMachine\n";
        return false;
    }

    module->setDataLayout(targetMachine->createDataLayout());

    std::error_code EC;

    const auto FileType = llvm::CodeGenFileType::ObjectFile;

    llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);
    if (EC)
    {
        llvm::errs() << "Could not open file: " << EC.message() << "\n";
        return false;
    }

    llvm::legacy::PassManager pass;

    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType))
    {
        llvm::errs() << "TargetMachine can't emit object file\n";
        return false;
    }

    pass.run(*module);
    dest.flush();

    return true;
}
