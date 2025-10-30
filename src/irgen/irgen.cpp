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

#include "irgen.hpp"
#include "ast.hpp"
#include "allocator/allocator.hpp"

#include <iostream>
#define CPPREST_FORCE_REBUILD

IRGenerator::IRGenerator(Semantics &semantics, size_t totalHeap)
    : semantics(semantics), totalHeapSize(totalHeap), context(), globalBuilder(context), funcBuilder(context), module(std::make_unique<llvm::Module>("unnameable", context))
{
    registerGeneratorFunctions();
    registerExpressionGeneratorFunctions();

    // Declare external allocator functions so IR can call them
    llvm::FunctionType *initType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context),    // returns void
        {llvm::Type::getInt64Ty(context)}, // takes size_t (64-bit int)
        false);

    llvm::PointerType *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);

    llvm::FunctionType *allocType = llvm::FunctionType::get(
        i8PtrTy,                           // returns void*
        {llvm::Type::getInt64Ty(context)}, // takes size_t
        false);

    llvm::FunctionType *freeType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context),    // returns void
        {llvm::Type::getInt64Ty(context)}, // takes size_t
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
        "sage_global_init",
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
    funcBuilder.SetInsertPoint(heapInitFnEntry);
    funcBuilder.CreateRetVoid();

    // Look for the main function
    llvm::Function *mainFn = module->getFunction("main");
    if (!mainFn)
        throw std::runtime_error("User must define main");

    llvm::BasicBlock &entryBlock = mainFn->getEntryBlock();
    llvm::IRBuilder<> tmpBuilder(&entryBlock, entryBlock.begin());

    llvm::Function *globalHeapFn = module->getFunction("sage_global_init");
    tmpBuilder.CreateCall(globalHeapFn);
}

// MAIN GENERATOR FUNCTION FOR EXPRESSION
//  Main Expression generator helper function
llvm::Value *IRGenerator::generateExpression(Node *node)
{
    if (!node)
    {
        std::cout << "[GENEXPR] NULL node!\n";
        return nullptr;
    }

    auto exprIt = expressionGeneratorsMap.find(typeid(*node));
    if (exprIt == expressionGeneratorsMap.end())
    {
        throw std::runtime_error("Could not find expression type IR generator: " + node->toString());
    }

    return (this->*exprIt->second)(node);
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
    //VALIDATION AND EXTRACTION
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

    // If there's no current insert block, handle global
    if (!funcBuilder.GetInsertBlock())
    {
        std::cout << "[DEBUG] No insert block (global scope) for let '" << letName << "'\n";
        if (letStmt->isHeap)
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

    //LOCAL SCOPE 
    llvm::Value *storage = nullptr;
    llvm::StructType *structTy = nullptr;
    bool isComponent = false;

    // Detect component type
    auto compIt = componentTypes.find(sym->type.resolvedName);
    if (compIt != componentTypes.end())
    {
        isComponent = true;
        structTy = llvm::dyn_cast<llvm::StructType>(compIt->second);
    }

    // For non-components: compute init value (scalar or other)
    llvm::Value *initVal = nullptr;
    if (!isComponent)
    {
        if (letStmt->value)
            initVal = generateExpression(letStmt->value.get());
        else
            initVal = llvm::Constant::getNullValue(getLLVMType(sym->type));
    }

    // If component, try to detect `new` expression and obtain the constructed pointer
    bool usedNewExpr = false;
    llvm::Value *constructedPtr = nullptr;
    if (isComponent)
    {
        constructedPtr = generateComponentInit(letStmt, structTy, usedNewExpr);
        // constructedPtr is non-null only when letStmt->value was a NewComponentExpression and generator returned pointer-to-struct
    }

    // Allocate storage
    if (letStmt->isHeap)
    {
        // keep existing heap path (you said to ignore heap raises in earlier messages, but preserving logic)
        storage = allocateHeapStorage(sym, letName, structTy);
        if (!isSageInitCalled)
        {
            generateSageInitCall();
            isSageInitCalled = true;
        }
    }
    else
    {
        if (isComponent)
        {
            if (constructedPtr)
            {
                // Reuse the constructed alloca as the storage for this variable — avoid double alloca or pointer-in-pointer
                storage = constructedPtr;
            }
            else
            {
                // Allocate a fresh struct slot on the stack and zero-init it (predictable defaults)
                storage = funcBuilder.CreateAlloca(structTy, nullptr, letName);
                funcBuilder.CreateStore(llvm::Constant::getNullValue(structTy), storage);
            }
        }
        else
        {
            // scalar / normal type
            llvm::Type *varTy = getLLVMType(sym->type);
            storage = funcBuilder.CreateAlloca(varTy, nullptr, letName);
            // store initial value if we have one
            if (initVal)
                funcBuilder.CreateStore(initVal, storage);
            else
                funcBuilder.CreateStore(llvm::Constant::getNullValue(varTy), storage);
        }
    }

    if (!storage)
        throw std::runtime_error("No storage allocated for let '" + letName + "'");

    // Update symbol metadata with storage and type
    sym->llvmValue = storage;
    sym->llvmType = (isComponent && structTy) ? structTy : getLLVMType(sym->type);


    // COMPONENT-SPECIFIC MEMBER INITIALIZATION 
    if (isComponent)
    {
        initializeComponentMembers(letStmt, sym, letName, storage, structTy, usedNewExpr);
    }

    // HEAP CLEANUP FOR DEAD LOCALS 
    if (letStmt->isHeap)
    {
        Node *lastUse = sym->lastUseNode ? sym->lastUseNode : letStmt;
        if (letStmt == lastUse && sym->refCount == 0)
        {
            freeHeapStorage(sym->componentSize, letName);
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
        // throw std::runtime_error("Symbol table corruption.");  // Uncomment to halt on error
    }
    else
    {
        std::cout << "[DEBUG] Global heap symbol '" << letName << "' registered as GlobalVariable.\n";
    }
    sym->llvmValue = globalPtr;

    // Build allocation and initialization in the heap-init function (one-time setup)
    llvm::IRBuilder<> heapBuilder(heapInitFnEntry);

    // Allocate raw memory via sage_alloc
    llvm::FunctionCallee sageAllocFn = module->getOrInsertFunction(
        "sage_alloc",
        i8PtrTy,
        llvm::Type::getInt64Ty(context));
    llvm::Value *allocSize = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->componentSize);
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

llvm::Value *IRGenerator::generateComponentInit(LetStatement *letStmt, llvm::StructType *structTy, bool &usedNewExpr)
{
    usedNewExpr = false;
    if (!letStmt)
        return nullptr;

    if (letStmt->value)
    {
        if (auto *newExpr = dynamic_cast<NewComponentExpression *>(letStmt->value.get()))
        {
            usedNewExpr = true;
            llvm::Value *constructedPtr = generateNewComponentExpression(newExpr);
            if (!constructedPtr)
                throw std::runtime_error("generateNewComponentExpression returned null for component init");
            // We expect generateNewComponentExpression to return a pointer to the freshly-allocated struct (alloca)
            return constructedPtr;
        }
    }

    // No new expression: nothing constructed for us; caller will allocate stack slot and zero-init / store default.
    return nullptr;
}

llvm::Value *IRGenerator::allocateHeapStorage(std::shared_ptr<SymbolInfo> sym, const std::string &letName, llvm::StructType *structTy)
{
    std::cout << "Let statement was heap raised\n";
    // If sage_init hasnt been called
    if (!isSageInitCalled)
        generateSageInitCall();

    uint64_t allocSize = sym->componentSize;
    llvm::Type *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
    llvm::FunctionCallee sageAlloc = module->getOrInsertFunction(
        "sage_alloc",
        i8PtrTy,
        llvm::Type::getInt64Ty(context));

    llvm::Value *rawPtr = funcBuilder.CreateCall(
        sageAlloc,
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), allocSize)},
        letName + "_sage_rawptr");

    llvm::Type *targetPtrTy = structTy ? structTy->getPointerTo() : getLLVMType(sym->type)->getPointerTo();
    return funcBuilder.CreateBitCast(rawPtr, targetPtrTy, letName + "_heap_ptr");
}

void IRGenerator::initializeComponentMembers(LetStatement *letStmt,
                                             std::shared_ptr<SymbolInfo> sym,
                                             const std::string &letName,
                                             llvm::Value *storagePtr,
                                             llvm::StructType *structTy,
                                             bool usedNewExpr)
{
    if (!storagePtr)
        throw std::runtime_error("initializeComponentMembers called with null storage for " + letName);

    // Retrieve component metadata for members (names + memberIndex should be precomputed by semantics)
    auto compMetaIt = semantics.customTypesTable.find(sym->type.resolvedName);
    if (compMetaIt == semantics.customTypesTable.end())
        return; // nothing to initialize

    // We assume storagePtr is of type `structTy*` (bitcast if needed)
    llvm::Type *expectedPtrTy = structTy->getPointerTo();
    if (storagePtr->getType() != expectedPtrTy)
    {
        if (storagePtr->getType()->isPointerTy())
            storagePtr = funcBuilder.CreateBitCast(storagePtr, expectedPtrTy, (letName + ".cast").c_str());
        else
            throw std::runtime_error("Storage pointer is not pointer-typed for component '" + letName + "'");
    }

    // Walk members by their semantic order (memberIndex must be assigned)
    for (const auto &pair : compMetaIt->second.members)
    {
        const std::string &memberName = pair.first;
        const auto &info = pair.second;

        // skip functions
        if (info->node && dynamic_cast<FunctionExpression *>(info->node))
            continue;

        int memberIndex = info->memberIndex;
        if (memberIndex < 0)
        {
            throw std::runtime_error("Missing memberIndex for member '" + memberName + "' in component '" + sym->type.resolvedName + "'");
        }

        std::cout << "[INIT] Making GEP for member " << memberName << " at index " << memberIndex << "\n";
        llvm::Value *memberPtr = funcBuilder.CreateStructGEP(structTy, storagePtr, static_cast<unsigned>(memberIndex), memberName + ".ptr");
        info->llvmValue = memberPtr;
        info->llvmType = getLLVMType(info->type);

        if (!usedNewExpr)
        {
            // Default-initialize the member according to its llvm type
            llvm::Type *mTy = info->llvmType;
            if (!mTy)
            {
                // If the type is a nested component, mTy may be null here; attempt to get it
                mTy = getLLVMType(info->type);
                info->llvmType = mTy;
            }

            if (auto ptrTy = llvm::dyn_cast<llvm::PointerType>(mTy))
            {
                funcBuilder.CreateStore(llvm::ConstantPointerNull::get(ptrTy), memberPtr);
            }
            else
            {
                funcBuilder.CreateStore(llvm::Constant::getNullValue(mTy), memberPtr);
            }
        }

        // If the member has an AST node, make sure we propagate the pointer into semantic metaData so subsequent lookups work
        if (info->node)
        {
            auto mdIt = semantics.metaData.find(info->node);
            if (mdIt != semantics.metaData.end())
            {
                mdIt->second->llvmValue = memberPtr;
                mdIt->second->llvmType = info->llvmType;
            }
        }
    }

    std::cout << "[DEBUG] Component members for '" << letName << "' initialized "
              << (usedNewExpr ? "(skipped per-new-init)" : "(default initialized)") << "\n";
}

void IRGenerator::freeHeapStorage(uint64_t size, const std::string &letName)
{
    llvm::FunctionCallee sageFree = module->getOrInsertFunction(
        "sage_free",
        llvm::Type::getVoidTy(context),
        llvm::Type::getInt64Ty(context));

    funcBuilder.CreateCall(
        sageFree,
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size)},
        letName + "_sage_free");
}

// Reference statement IR generator
void IRGenerator::generateReferenceStatement(Node *node)
{
    auto refStmt = dynamic_cast<ReferenceStatement *>(node);
    if (!refStmt)
        throw std::runtime_error("Invalid reference statement");

    auto refName = refStmt->referer->expression.TokenLiteral;

    auto metaIt = semantics.metaData.find(refStmt);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Failed to find reference statement metaData for '" + refName + "'");

    auto refSym = metaIt->second;
    // Getting the target symbol
    auto targetSym = refSym->refereeSymbol;

    if (!targetSym)
        throw std::runtime_error("Reference '" + refName + "' has no target symbol");

    if (targetSym->hasError)
        throw std::runtime_error("Semantic error detected");

    if (!targetSym->llvmValue)
        throw std::runtime_error("Reference '" + refName + "' target has no llvmValue");

    // Since references are just an alias system per se I will just give the reference the same llvm value as its target
    refSym->llvmValue = targetSym->llvmValue;
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

    std::cout << "[IR DEBUG] Pointer type: " << ptrSym->type.resolvedName << "\n";

    llvm::Function *fn = funcBuilder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> entryBuilder(&fn->getEntryBlock(), fn->getEntryBlock().begin());

    // Getting the pointer llvm type
    llvm::Type *ptrType = getLLVMType(ptrSym->type);
    llvm::Type *ptrStorageType = ptrType->getPointerTo();

    if (!ptrType)
        throw std::runtime_error("Failed to get LLVM Type for '" + ptrName + "'");

    llvm::Value *initVal = ptrStmt->value ? generateExpression(ptrStmt->value.get())
                                          : llvm::Constant::getNullValue(ptrType);

    if (!initVal)
        throw std::runtime_error("No init value");

    llvm::AllocaInst *alloca = entryBuilder.CreateAlloca(ptrStorageType, nullptr, ptrName);
    if (!alloca)
        throw std::runtime_error("Failed to create alloca");

    funcBuilder.CreateStore(initVal, alloca);
    ptrSym->llvmValue = alloca;
    ptrSym->llvmType = ptrType;

    std::cout << "Exited pointer statement generator\n";
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
        llvm::Value *stepVal = generateExpression(forStmt->step.get());
        if (!stepVal)
        {
            std::cerr << "[IR ERROR] loop step generation returned nullptr!\n";
            funcBuilder.CreateBr(condBB); // keep IR consistent
            funcBuilder.SetInsertPoint(origBB);
            return;
        }
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

    if (!funcBuilder.GetInsertBlock())
        throw std::runtime_error("Executable statements are not allowed at global scope");

    llvm::Value *targetPtr = nullptr;
    llvm::Value *initValue = generateExpression(assignStmt->value.get());
    if (!initValue)
        throw std::runtime_error("Failed to generate IR for assignment value");

    // Check if it's a SelfExpression (self.field)
    if (auto *selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
    {
        // Generate pointer to the field in the struct
        targetPtr = generateSelfExpression(selfExpr);
        if (!targetPtr)
            throw std::runtime_error("Failed to get pointer for self field");
    }

    // Checking if it is a dereference expression
    else if (auto derefExpr = dynamic_cast<DereferenceExpression *>(assignStmt->identifier.get()))
    {
        std::cout << "Inside Assignment statement deref branch\n";
        targetPtr = generateDereferenceExpression(derefExpr);
        if (!targetPtr)
            throw std::runtime_error("Failed to get pointer for the dereference expression");
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

    auto &members = parentIt->second.members;
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

        // --- allocBB: call sage_alloc(size), bitcast to elemPtrTy, store into slot, branch to contBB
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
        std::cerr << "[IR DEBUG] Processing block statement child of type: " << typeid(*stmt).name() << " - " << stmt->toString() << "\n";
        generateStatement(stmt.get());
        if (funcBuilder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Terminator found in block statement child: " << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
            break;
        }
    }
}

void IRGenerator::generateShoutStatement(Node *node)
{
    auto shoutStmt = dynamic_cast<ShoutStatement *>(node);

    if (!shoutStmt)
        throw std::runtime_error("Invalid shout statement node");

    if (!funcBuilder.GetInsertBlock())
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

    llvm::Value *retVal = nullptr;

    // Generating IR for the return value if it exists
    if (retStmt->return_value)
    {
        retVal = generateExpression(retStmt->return_value.get());
    }

    llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
    if (retVal)
    {
        // Ensure the return type matches
        if (retVal->getType() != currentFunction->getReturnType())
        {
            llvm::errs() << "Return type mismatch\n";
        }
        funcBuilder.CreateRet(retVal);
    }
    else
    {
        // For void functions
        if (currentFunction->getReturnType()->isVoidTy())
            funcBuilder.CreateRetVoid();
        else
            llvm::errs() << "Return statement missing value for non-void function\n";
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

    if (!funcBuilder.GetInsertBlock() && (lhsIdent || rhsIdent))
    {
        throw std::runtime_error(
            "Executable statements are not allowed at global scope: '" +
            infix->operat.TokenLiteral + "' at line " +
            std::to_string(infix->operat.line));
    }

    if (infix->operat.type == TokenType::FULLSTOP)
    {
        // sanity check: member access only inside a function
        if (!funcBuilder.GetInsertBlock())
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
        auto memberIt = parentInfo.members.find(memberName);
        if (memberIt == parentInfo.members.end())
            throw std::runtime_error("No member '" + memberName + "' in type '" + parentTypeName + "'");

        // get member index + type
        unsigned memberIndex = memberIt->second->memberIndex;
        llvm::StructType *structTy = llvmCustomTypes[parentTypeName];
        llvm::Type *memberType = getLLVMType(memberIt->second->type);

        // if lhs is struct by value, we need a pointer to use GEP
        llvm::Value *lhsPtr = lhsVal;
        if (!lhsVal->getType()->isPointerTy())
        {
            llvm::Value *allocaTmp = funcBuilder.CreateAlloca(lhsVal->getType(), nullptr, parentTypeName + "_tmp");
            funcBuilder.CreateStore(lhsVal, allocaTmp);
            lhsPtr = allocaTmp;
        }

        llvm::Value *memberPtr = funcBuilder.CreateStructGEP(structTy, lhsPtr, memberIndex, memberName);

        // now return by-value load, not a pointer
        return funcBuilder.CreateLoad(memberType, memberPtr, memberName + "_val");
    }

    if (infix->operat.type == TokenType::SCOPE_OPERATOR)
    {
        std::cout << "TRIGGERED SCOPE OPERATOR GUY\n";
        if (!funcBuilder.GetInsertBlock())
            throw std::runtime_error("Executable statements are not allowed at global scope");

        // Get left-hand object pointer from metadata (guaranteed pointer)
        auto leftMetaIt = semantics.metaData.find(infix->left_operand.get());
        if (leftMetaIt == semantics.metaData.end())
            throw std::runtime_error("Left-hand object metadata missing");

        llvm::Value *objectVal = leftMetaIt->second->llvmValue; // pointer to struct!
        if (!objectVal)
            throw std::runtime_error("Left-hand object llvmValue not set");

        // Get right-hand identifier (member)
        auto memberIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
        if (!memberIdent)
            throw std::runtime_error("Right-hand side of '::' must be a field identifier");

        std::string memberName = memberIdent->expression.TokenLiteral;

        // Resolve member index and type
        auto parentTypeName = leftMetaIt->second->type.resolvedName;
        auto parentIt = semantics.customTypesTable.find(parentTypeName);
        if (parentIt == semantics.customTypesTable.end())
            throw std::runtime_error("Type '" + parentTypeName + "' doesn't exist");

        auto memberIt = parentIt->second.members.find(memberName);
        if (memberIt == parentIt->second.members.end())
            throw std::runtime_error("Member '" + memberName + "' not found in type '" + parentTypeName + "'");

        unsigned memberIndex = memberIt->second->memberIndex;
        llvm::StructType *structTy = llvmCustomTypes[parentTypeName];

        // Compute pointer to the member
        llvm::Value *memberPtr = funcBuilder.CreateStructGEP(structTy, objectVal, memberIndex, memberName);
        if (!memberPtr)
            throw std::runtime_error("No llvm value for '" + memberName + "'");

        llvm::Type *memberType = getLLVMType(memberIt->second->type);
        if (!memberType)
            throw std::runtime_error("Member Type wasn't retrieved");

        // Check if the member is a heap field
        if (memberIt->second->isHeap)
        {
            // ptr-to-element type
            llvm::PointerType *elemPtrTy = memberType->getPointerTo();

            // Load current heap pointer from the struct slot
            llvm::Value *heapPtr = funcBuilder.CreateLoad(elemPtrTy, memberPtr, memberName + "_heap_load");

            // If null, allocate
            llvm::Value *nullPtr = llvm::ConstantPointerNull::get(elemPtrTy);
            llvm::Value *isNull = funcBuilder.CreateICmpEQ(heapPtr, nullPtr, memberName + "_is_null");

            llvm::Function *parentFn = funcBuilder.GetInsertBlock()->getParent();
            llvm::BasicBlock *allocBB = llvm::BasicBlock::Create(context, memberName + "_alloc", parentFn);
            llvm::BasicBlock *contBB = llvm::BasicBlock::Create(context, memberName + "_cont", parentFn);

            funcBuilder.CreateCondBr(isNull, allocBB, contBB);

            // --- allocBB
            funcBuilder.SetInsertPoint(allocBB);
            llvm::PointerType *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
            llvm::FunctionCallee sageAllocFn = module->getOrInsertFunction("sage_alloc", i8PtrTy, llvm::Type::getInt64Ty(context));
            uint64_t size = module->getDataLayout().getTypeAllocSize(memberType);
            llvm::Value *sizeArg = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size);
            llvm::Value *rawAlloc = funcBuilder.CreateCall(sageAllocFn, {sizeArg}, memberName + "_alloc_i8ptr");
            llvm::Value *newHeapPtr = funcBuilder.CreateBitCast(rawAlloc, elemPtrTy, memberName + "_alloc_ptr");
            funcBuilder.CreateStore(newHeapPtr, memberPtr);
            funcBuilder.CreateBr(contBB);

            // --- contBB
            funcBuilder.SetInsertPoint(contBB);
            llvm::Value *heapPtr2 = funcBuilder.CreateLoad(elemPtrTy, memberPtr, memberName + "_heap");
            return heapPtr2; // return pointer to heap memory
        }

        // Non-heap: just load the value
        return funcBuilder.CreateLoad(memberType, memberPtr, memberName + "_val");
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
        if (isIntegerType(leftType) && isIntegerType(rightType))
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

    if (dt != DataType::STRING && dt != DataType::NULLABLE_STR)
    {
        throw std::runtime_error("Type error: Expected STRING or NULLABLE_STR for StringLiteral ");
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
    if (dt != DataType::CHAR && dt != DataType::NULLABLE_CHAR)
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
    if (dt != DataType::CHAR16 && dt != DataType::NULLABLE_CHAR16)
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
    if (dt != DataType::CHAR16 && dt != DataType::NULLABLE_CHAR16)
    {
        throw std::runtime_error("Type error: Expected CHAR32 for Char16Literal");
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
    if (dt != DataType::BOOLEAN && dt != DataType::NULLABLE_BOOLEAN)
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
    if (dt != DataType::SHORT_INT && dt != DataType::NULLABLE_SHORT_INT)
        throw std::runtime_error("Type error: Expected SHORT_INT or NULLABLE_SHORT_INT");

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
    if (dt != DataType::USHORT_INT && dt != DataType::NULLABLE_USHORT_INT)
        throw std::runtime_error("Type error: Expected USHORT_INT or NULLABLE_USHORT_INT");

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
    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Integer literal not found in metadata at line:" + std::to_string(intLit->expression.line) + " and column: " + std::to_string(intLit->expression.column));
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::INTEGER && dt != DataType::NULLABLE_INT)
    {
        throw std::runtime_error("Type error: Expected INTEGER or NULLABLE_INT for IntegerLiteral");
    }
    int64_t value = std::stoll(intLit->int_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(32, value, true));
}

llvm::Value *IRGenerator::generateUnsignedIntegerLiteral(Node *node)
{
    auto uintLit = dynamic_cast<UnsignedIntegerLiteral *>(node);
    if (!uintLit)
        throw std::runtime_error("Invalid uint literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Uint literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::UINTEGER && dt != DataType::NULLABLE_UINT)
        throw std::runtime_error("Type error: Expected UINTEGER or NULLABLE_UINT");

    uint32_t value = static_cast<uint32_t>(std::stoul(uintLit->uint_token.TokenLiteral));
    return llvm::ConstantInt::get(context, llvm::APInt(32, value, false));
}

llvm::Value *IRGenerator::generateLongLiteral(Node *node)
{
    auto longLit = dynamic_cast<LongLiteral *>(node);
    if (!longLit)
        throw std::runtime_error("Invalid long literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Long literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::LONG_INT && dt != DataType::NULLABLE_LONG_INT)
        throw std::runtime_error("Type error: Expected LONG_INT or NULLABLE_LONG_INT");

    int64_t value = std::stoll(longLit->long_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(64, value, true));
}

llvm::Value *IRGenerator::generateUnsignedLongLiteral(Node *node)
{
    auto ulongLit = dynamic_cast<UnsignedLongLiteral *>(node);
    if (!ulongLit)
        throw std::runtime_error("Invalid ulong literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("ULong literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::ULONG_INT && dt != DataType::NULLABLE_ULONG_INT)
        throw std::runtime_error("Type error: Expected ULONG_INT or NULLABLE_ULONG_INT");

    uint64_t value = std::stoull(ulongLit->ulong_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(64, value, false));
}

llvm::Value *IRGenerator::generateExtraLiteral(Node *node)
{
    auto extraLit = dynamic_cast<ExtraLiteral *>(node);
    if (!extraLit)
        throw std::runtime_error("Invalid extra (128-bit) literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Extra literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::EXTRA_INT && dt != DataType::NULLABLE_EXTRA_INT)
        throw std::runtime_error("Type error: Expected EXTRA_INT or NULLABLE_EXTRA_INT");

    // Use APInt constructor with string and base 10 for 128-bit
    llvm::APInt value(128, extraLit->extra_token.TokenLiteral, 10);
    return llvm::ConstantInt::get(context, value);
}

llvm::Value *IRGenerator::generateUnsignedExtraLiteral(Node *node)
{
    auto uextraLit = dynamic_cast<UnsignedExtraLiteral *>(node);
    if (!uextraLit)
        throw std::runtime_error("Invalid uextra (128-bit unsigned) literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("UExtra literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::UEXTRA_INT && dt != DataType::NULLABLE_UEXTRA_INT)
        throw std::runtime_error("Type error: Expected UEXTRA_INT or NULLABLE_UEXTRA_INT");

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
    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Float literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::FLOAT && dt != DataType::NULLABLE_FLT)
    {
        throw std::runtime_error("Type error: Expected Float or NULLABLE_FLT for FloatLiteral ");
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
    auto it = semantics.metaData.find(node); // Creating an iterator to find specific meta data about the double literal
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Double literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::DOUBLE && dt != DataType::NULLABLE_DOUBLE)
    {
        throw std::runtime_error("Type error: Expected DOUBLE for DoubleLiteral");
    }
    // Checking if we have metaData about the double literal and if so we check to see if the data type is double
    double value = std::stod(dbLit->double_token.TokenLiteral);            // Converting the double literal from a string to a double
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), value); // Returning double value
}

llvm::Value *IRGenerator::generateNullLiteral(NullLiteral *nullLit, DataType type)
{
    switch (type)
    {
    case DataType::NULLABLE_STR:
        return llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0));

    case DataType::NULLABLE_INT:
        // Using minimum signed int as null marker
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, INT32_MIN, true));

    case DataType::NULLABLE_SHORT_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, INT16_MIN, true));

    case DataType::NULLABLE_USHORT_INT:
        // Using zero as null for unsigned
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, 0));

    case DataType::NULLABLE_UINT:
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));

    case DataType::NULLABLE_LONG_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, INT64_MIN, true));

    case DataType::NULLABLE_ULONG_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0));

    case DataType::NULLABLE_EXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt128Ty(context), llvm::APInt(128, 0));

    case DataType::NULLABLE_FLT:
        return llvm::ConstantFP::getNaN(llvm::Type::getFloatTy(context));

    case DataType::NULLABLE_DOUBLE:
        return llvm::ConstantFP::getNaN(llvm::Type::getDoubleTy(context));

    case DataType::NULLABLE_BOOLEAN:
        return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), llvm::APInt(1, 0));

    case DataType::NULLABLE_CHAR:
        return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0));

    case DataType::NULLABLE_CHAR16:
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, 0));

    case DataType::NULLABLE_CHAR32:
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

    const std::string &name = derefExpr->identifier->expression.TokenLiteral;

    auto metaIt = semantics.metaData.find(derefExpr);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Unidentified inditifier '" + name + "' in dereference expression");

    auto derefSym = metaIt->second;
    if (!derefSym)
        throw std::runtime_error("Unidentified dereference identifier '" + name + "'");

    // Check if there are any semantic errors
    if (derefSym->hasError)
        throw std::runtime_error("Semantic error detected");

    // Get the llvm value of the identifier(the address of the pointer) it was stored here by the generate pointer statement
    auto derefIdent = dynamic_cast<Identifier *>(derefExpr->identifier.get());
    if (!derefIdent)
        throw std::runtime_error("There is an issue here check the node maybe NODE TYPE IS: '" + derefIdent->toString() + "'");

    std::cout << "DEREF IDENTIFIER NODE IS: " << derefIdent->toString() << "\n";
    AddressAndPendingFree addrAndFree = generateIdentifierAddress(derefIdent);
    auto pointerAddress = addrAndFree.address;
    if (!pointerAddress)
        throw std::runtime_error("Pointer address does not exist");

    // Getting the type of the pointer
    auto pointerType = getLLVMType(derefSym->derefPtrType);
    if (!pointerType)
        throw std::runtime_error("Pointer type doesnt exist");

    if (!pointerType->isPointerTy())
        throw std::runtime_error("Cannot dereference non-pointer type");

    // Loading the actual address stored in the pointer address(Address of the pointee)
    auto loadedPointer = funcBuilder.CreateLoad(pointerType, pointerAddress, name + "_addr");
    if (!loadedPointer)
        throw std::runtime_error("Loaded pointer not created");

    // Loading the actual value stored at the address of the pointee
    auto elementType = getLLVMType(derefSym->type);
    if (!elementType)
        throw std::runtime_error("Failed to generate llvm type for pointee type '" + derefSym->type.resolvedName + "'");

    auto finalValue = funcBuilder.CreateLoad(elementType, loadedPointer, name + "_val");
    if (!finalValue)
        throw std::runtime_error("Failed to generate dereference value");

    std::cout << "Exited dereference expression generator\n";
    return finalValue;
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

    // Component instance -> pointer to struct (unchanged)
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
                llvm::Type::getVoidTy(context),
                llvm::Type::getInt64Ty(context));

            llvm::Value *sizeArg = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->componentSize);

            llvm::CallInst *callInst = llvm::CallInst::Create(sageFreeFn, {sizeArg});
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
    {
        throw std::runtime_error("Invalid self expression");
    }

    const std::string &compName = currentComponent->component_name->expression.TokenLiteral;

    auto ctIt = componentTypes.find(compName);
    if (ctIt == componentTypes.end())
    {
        throw std::runtime_error("Component with name '" + compName + "' not found");
    }

    llvm::Type *compTy = ctIt->second;
    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(compTy);
    if (!structTy)
    {
        throw std::runtime_error("Component llvm type is not a struct for: " + compName);
    }

    const std::string &fieldName = selfExpr->field->expression.TokenLiteral;

    // Component metadata (semantic)
    auto compMetaIt = semantics.metaData.find(currentComponent);
    if (compMetaIt == semantics.metaData.end())
    {
        throw std::runtime_error("Missing component metaData for '" + compName + "'");
    }
    auto compMeta = compMetaIt->second;

    // find member info by name
    auto memIt = compMeta->members.find(fieldName);
    if (memIt == compMeta->members.end())
    {
        throw std::runtime_error("Field '" + fieldName + "' not found in component '" + compName + "'");
    }

    // --- IMPORTANT: use the stored member index (set during semantic walk) ---
    int memberIndex = -1;
    if (memIt->second->memberIndex >= 0)
    {
        memberIndex = memIt->second->memberIndex;
    }
    else
    {
        // Fallback: compute index only if absolutely necessary (and log!)
        std::cerr << "[IR WARN] Member '" << fieldName << "' has no memberIndex; falling back to map iteration (unstable)\n";
        unsigned idx = 0;
        for (const auto &p : compMeta->members)
        {
            if (p.first == fieldName)
                break;
            ++idx;
        }
        memberIndex = static_cast<int>(idx);
    }

    // The component instance (should be a pointer to the struct: %Player*)
    llvm::Value *componentInstance = compMeta->llvmValue;
    // llvm::Value *componentInstance = currentFunctionSelf;

    if (!componentInstance)
    {
        throw std::runtime_error("Component instance llvmValue is null for '" + compName + "' when accessing '" + fieldName + "'");
    }

    // Ensure the pointer is of type "structTy*". If not, try to bitcast it.
    llvm::Type *expectedPtrTy = structTy->getPointerTo();
    if (componentInstance->getType() != expectedPtrTy)
    {
        if (componentInstance->getType()->isPointerTy())
        {
            // bitcast to expected pointer type
            componentInstance = funcBuilder.CreateBitCast(componentInstance, expectedPtrTy, (fieldName + "_instance_cast").c_str());
        }
        else
        {
            throw std::runtime_error("Component instance is not a pointer type for '" + compName + "'");
        }
    }

    // Now get the pointer to the field
    llvm::Value *fieldPtr = funcBuilder.CreateStructGEP(structTy, componentInstance, (unsigned)memberIndex, fieldName + "_ptr");
    if (!fieldPtr)
    {
        throw std::runtime_error("CreateStructGEP failed for field '" + fieldName + "' index " + std::to_string(memberIndex));
    }

    // Debug log
    std::cout << "[IR DEBUG] Generated field pointer for " << compName << "." << fieldName
              << " (index=" << memberIndex << ", ptr=" << fieldPtr << ")\n";

    return fieldPtr;
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
        return nullptr; // If it has an error we just stop IR generation
    }

    // Getting the function signature
    auto fnName = fnExpr->func_key.TokenLiteral;

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
    llvm::FunctionType *funcType = llvm::FunctionType::get(getLLVMType(retType), llvmParamTypes, false);

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

    // This will handle whatever is inside the block including the return value
    generateExpression(fnExpr->block.get());
    funcBuilder.ClearInsertionPoint();
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

    if (!funcBuilder.GetInsertBlock())
        throw std::runtime_error("Function calls are not allowed at global scope");

    auto callIt = semantics.metaData.find(callExpr);
    if (callIt == semantics.metaData.end())
    {
        throw std::runtime_error("Call expression does not exist");
    }
    if (callIt->second->hasError)
        throw std::runtime_error("Semantic error detected in call expression");

    // Getting the function name
    const std::string &fnName = callExpr->function_identifier->expression.TokenLiteral;
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
    return call;
}

// HELPER FUNCTIONS
llvm::Type *IRGenerator::getLLVMType(ResolvedType type)
{
    llvm::Type *baseType = nullptr;

    switch (type.kind)
    {
    case DataType::SHORT_INT:
    case DataType::NULLABLE_SHORT_INT:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::USHORT_INT:
    case DataType::NULLABLE_USHORT_INT:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::UINTEGER:
    case DataType::NULLABLE_UINT:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }

    case DataType::ULONG_INT:
    case DataType::NULLABLE_ULONG_INT:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }

    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }

    case DataType::UEXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }

    case DataType::BOOLEAN:
    case DataType::NULLABLE_BOOLEAN:
    {
        baseType = llvm::Type::getInt1Ty(context);
        break;
    }

    case DataType::CHAR:
    case DataType::NULLABLE_CHAR:
    {
        baseType = llvm::Type::getInt8Ty(context);
        break;
    }

    case DataType::CHAR16:
    case DataType::NULLABLE_CHAR16:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::CHAR32:
    case DataType::NULLABLE_CHAR32:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::FLOAT:
    case DataType::NULLABLE_FLT:
    {
        baseType = llvm::Type::getFloatTy(context);
        break;
    }

    case DataType::DOUBLE:
    case DataType::NULLABLE_DOUBLE:
    {
        baseType = llvm::Type::getDoubleTy(context);
        break;
    }

    case DataType::STRING:
    case DataType::NULLABLE_STR:
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
        baseType = getLLVMType({enumInfo.underLyingType, ""});
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

    return baseType;
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

    generatorFunctionsMap[typeid(ShoutStatement)] = &IRGenerator::generateShoutStatement;
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
    expressionGeneratorsMap[typeid(Identifier)] = &IRGenerator::generateIdentifierExpression;
    expressionGeneratorsMap[typeid(AddressExpression)] = &IRGenerator::generateAddressExpression;
    expressionGeneratorsMap[typeid(DereferenceExpression)] = &IRGenerator::generateDereferenceExpression;
    expressionGeneratorsMap[typeid(BlockExpression)] = &IRGenerator::generateBlockExpression;
    expressionGeneratorsMap[typeid(CallExpression)] = &IRGenerator::generateCallExpression;
    expressionGeneratorsMap[typeid(SelfExpression)] = &IRGenerator::generateSelfExpression;
    expressionGeneratorsMap[typeid(NewComponentExpression)] = &IRGenerator::generateNewComponentExpression;
    expressionGeneratorsMap[typeid(InstanceExpression)] = &IRGenerator::generateInstanceExpression;
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
    case DataType::NULLABLE_SHORT_INT:
    case DataType::USHORT_INT:
    case DataType::NULLABLE_USHORT_INT:
    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
    case DataType::UINTEGER:
    case DataType::NULLABLE_UINT:
    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
    case DataType::ULONG_INT:
    case DataType::NULLABLE_ULONG_INT:
    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
    case DataType::UEXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
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
    case DataType::NULLABLE_SHORT_INT:
    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
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
    case DataType::NULLABLE_SHORT_INT:
    case DataType::USHORT_INT:
    case DataType::NULLABLE_USHORT_INT:
        return 16;
    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
    case DataType::UINTEGER:
    case DataType::NULLABLE_UINT:
        return 32;
    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
    case DataType::ULONG_INT:
    case DataType::NULLABLE_ULONG_INT:
        return 64;
    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
    case DataType::UEXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
        return 128;
    default:
        return 0; // Not an integer type
    }
}

bool IRGenerator::inFunction()
{
    return funcBuilder.GetInsertBlock() != nullptr;
}

char *IRGenerator::unnitoa(int val, char *buf)
{
    char *p = buf;
    if (val < 0)
    {
        *p++ = '-';
        val = -val;
    }

    // Remember start of digits
    char *start = p;

    // Convert digits
    do
    {
        *p++ = '0' + (val % 10);
        val /= 10;
    } while (val);

    // Null terminate
    *p = '\0';

    // Reverse digits in place
    char *end = p - 1;
    while (start < end)
    {
        char tmp = *start;
        *start++ = *end;
        *end-- = tmp;
    }

    return buf;
}

void IRGenerator::shoutRuntime(llvm::Value *val, ResolvedType type)
{
    if (!val)
        throw std::runtime_error("shout! called with null value");

    auto &ctx = module->getContext();
    auto i32Ty = llvm::IntegerType::getInt32Ty(ctx);
    auto i8Ty = llvm::IntegerType::getInt8Ty(ctx);
    auto i8PtrTy = llvm::PointerType::get(i8Ty, 0);
    auto i64Ty = llvm::IntegerType::getInt64Ty(ctx);

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
        llvm::Function *unnitoaFn = module->getFunction("unnitoa");
        if (!unnitoaFn)
        {
            llvm::Type *i32Ty = llvm::Type::getInt32Ty(module->getContext());
            llvm::Type *i8Ty = llvm::Type::getInt8Ty(module->getContext());
            llvm::PointerType *i8PtrTy = llvm::PointerType::get(i8Ty, 0);

            llvm::FunctionType *unnitoaTy =
                llvm::FunctionType::get(i8PtrTy, {i32Ty, i8PtrTy}, false);

            // Note: InternalLinkage means “this function exists here”, not external
            unnitoaFn = llvm::Function::Create(
                unnitoaTy,
                llvm::GlobalValue::ExternalLinkage, // not external to another module, just callable
                "unnitoa",
                *module);
        }

        char buf[20];

        if (auto constInt = llvm::dyn_cast<llvm::ConstantInt>(intVal))
        {
            // Compile-time constant
            unnitoa(static_cast<int>(constInt->getSExtValue()), buf);
            llvm::Value *strVal = funcBuilder.CreateGlobalStringPtr(buf);
            printString(strVal);
        }
        else if (intVal->getType()->isPointerTy())
        {
            std::cout << "Branched to pinter print";
            // Stack/heap integer pointer
            llvm::Type *loadedTy = llvm::cast<llvm::PointerType>(intVal->getType());
            llvm::Value *runtimeInt = funcBuilder.CreateLoad(loadedTy, intVal, "runtime_int");

            // Allocate buffer
            llvm::Type *i8Ty = llvm::Type::getInt8Ty(module->getContext());
            llvm::Value *bufAlloca = funcBuilder.CreateAlloca(llvm::ArrayType::get(i8Ty, 20), nullptr, "int_buf");
            llvm::Value *bufPtr = funcBuilder.CreatePointerCast(bufAlloca, llvm::PointerType::get(i8Ty, 0));
            // Call in-code helper directly

            funcBuilder.CreateCall(unnitoaFn, {runtimeInt, bufPtr});
            printString(bufPtr);
        }
        else if (intVal->getType()->isIntegerTy(32))
        {
            std::cout << "Branching to SSA print\n";
            // Immediate runtime i32 value (not a pointer)
            llvm::Type *i8Ty = llvm::Type::getInt8Ty(module->getContext());
            llvm::Value *bufAlloca = funcBuilder.CreateAlloca(llvm::ArrayType::get(i8Ty, 20), nullptr, "int_buf");
            llvm::Value *bufPtr = funcBuilder.CreatePointerCast(bufAlloca, llvm::PointerType::get(i8Ty, 0));

            // Pass the integer value directly
            funcBuilder.CreateCall(unnitoaFn, {intVal, bufPtr});
            printString(bufPtr);
        }

        else
        {
            throw std::runtime_error("Unsupported integer value in shout!");
        }
    };

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
