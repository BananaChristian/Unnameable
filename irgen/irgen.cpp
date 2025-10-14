#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>

#include <llvm/ADT/STLExtras.h>
#include <llvm/CodeGen/TargetPassConfig.h>

#include "irgen.hpp"
#include "ast.hpp"
#include "allocator/allocator.hpp"

#include <iostream>
#define CPPREST_FORCE_REBUILD
IRGenerator::IRGenerator(Semantics &semantics, size_t totalHeap)
    : semantics(semantics), totalHeapSize(totalHeap), context(), builder(context), module(std::make_unique<llvm::Module>("unnameable", context))
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
}

// MAIN GENERATOR FUNCTION
void IRGenerator::generate(const std::vector<std::unique_ptr<Node>> &program)
{
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(context), false);

    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", module.get());

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);

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

    // Example heap size, later this comes from analyzer
    builder.CreateCall(initFunc, {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), totalHeapSize)});

    // --- Generate program body ---
    for (const auto &node : program)
    {
        generateStatement(node.get());
    }

    // --- Call sage_destroy before returning ---
    llvm::Function *destroyFunc = module->getFunction("sage_destroy");
    if (!destroyFunc)
    {
        llvm::FunctionType *destroyType = llvm::FunctionType::get(
            llvm::Type::getVoidTy(context), {}, false);

        destroyFunc = llvm::Function::Create(
            destroyType, llvm::Function::ExternalLinkage, "sage_destroy", module.get());
    }

    builder.CreateCall(destroyFunc, {});

    // --- Return 0 ---
    builder.CreateRet(llvm::ConstantInt::get(context, llvm::APInt(32, 0)));
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
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        throw std::runtime_error("Invalid let statement");

    const std::string &letName = letStmt->ident_token.TokenLiteral;
    std::cout << "[DEBUG] Generating let statement for variable '" << letName << "'\n";

    auto letIt = semantics.metaData.find(letStmt);
    if (letIt == semantics.metaData.end())
        throw std::runtime_error("No let metadata for '" + letName + "'");

    auto sym = letIt->second;
    std::cout << "[DEBUG] Symbol type: " << sym->type.resolvedName << "\n";

    if (sym->hasError)
        throw std::runtime_error("Semantic error detected");

    llvm::Function *fn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> entryBuilder(&fn->getEntryBlock(), fn->getEntryBlock().begin());

    // === Component type ===
    auto compIt = componentTypes.find(sym->type.resolvedName);
    if (compIt != componentTypes.end())
    {
        llvm::StructType *structTy = compIt->second;
        std::cout << "[DEBUG] Allocating component '" << letName << "' as struct type '"
                  << sym->type.resolvedName << "'\n";

        // --- If initializer is a 'new Component(...)'----
        llvm::Value *instance = nullptr;
        bool usedNewExpr = false;
        if (letStmt->value)
        {
            if (auto newExpr = dynamic_cast<NewComponentExpression *>(letStmt->value.get()))
            {
                usedNewExpr = true;
                instance = generateNewComponentExpression(newExpr);
                if (!instance)
                    throw std::runtime_error("generateNewComponentExpression returned null for '" + letName + "'");

                // Ensure pointer-to-struct type
                llvm::Type *expectedPtrTy = structTy->getPointerTo();
                if (instance->getType() != expectedPtrTy)
                {
                    if (instance->getType()->isPointerTy())
                    {
                        instance = builder.CreateBitCast(instance, expectedPtrTy, letName + "_instance_cast");
                    }
                    else
                    {
                        throw std::runtime_error("New expression produced non-pointer value for component '" + letName + "'");
                    }
                }
                std::cout << "[DEBUG] New expression created component instance at " << instance << "\n";
            }
        }

        // Fallback: allocate the struct itself in entry block if not a 'new' expression
        if (!instance)
        {
            llvm::AllocaInst *alloca = entryBuilder.CreateAlloca(structTy, nullptr, letName);
            instance = alloca;
            std::cout << "[DEBUG] Struct allocated at llvmValue = " << instance << "\n";
        }

        sym->llvmValue = instance;
        sym->llvmType = structTy;
        std::cout << "[DEBUG] Struct allocated at llvmValue = " << instance << "\n";

        // Walk all members: declared + imported
        auto compMetaIt = semantics.customTypesTable.find(sym->type.resolvedName);
        if (compMetaIt != semantics.customTypesTable.end())
        {
            int index = 0;
            for (auto &[memberName, info] : compMetaIt->second.members)
            {
                // Skip functions
                if (info->node && dynamic_cast<FunctionStatement *>(info->node))
                {
                    std::cout << "[DEBUG] Skipping function member '" << memberName << "'\n";
                    index++;
                    continue;
                }

                // Create GEP relative to struct allocation
                llvm::Value *memberPtr = builder.CreateStructGEP(structTy, instance, index, memberName);
                if (!memberPtr)
                    throw std::runtime_error("Failed to allocate llvmValue for member '" + memberName + "'");

                // Assign both llvmValue and llvmType in customTypesTable
                info->llvmValue = memberPtr;
                info->llvmType = getLLVMType(info->type);
                std::cout << "[DEBUG] Member '" << memberName << "' assigned llvmValue = "
                          << memberPtr << " and llvmType = " << info->llvmType
                          << " at struct index " << index << "\n";

                // --- Initialize member slot at function entry ---
                // If this is a heap member, initialize the slot to null (so runtime sees "not allocated yet").
                // Otherwise, zero-initialize simple scalar/pointer fields.
                try
                {
                    llvm::Type *mTy = info->llvmType;
                    if (!mTy)
                    {
                        throw std::runtime_error("Failed to get llvmType while initializing member '" + memberName + "'");
                    }
                    if (!usedNewExpr)
                    {
                        if (info->isHeap)
                        {
                            // Slot type in the struct is a pointer to element (elem*), so store a null of elem*.
                            llvm::PointerType *elemPtrTy = mTy->getPointerTo(); // mTy is element type (i32), slot is elem*
                            llvm::Constant *nullPtr = llvm::ConstantPointerNull::get(elemPtrTy);
                            builder.CreateStore(nullPtr, memberPtr);
                            std::cout << "[DEBUG] Initialized heap member slot '" << memberName << "' to null at entry\n";
                        }
                        else
                        {
                            // For non-heap fields, zero-init simple types (integers, floats, pointers).
                            if (mTy->isPointerTy() || mTy->isIntegerTy() || mTy->isFloatingPointTy())
                            {
                                llvm::Constant *zeroVal = llvm::Constant::getNullValue(mTy);
                                builder.CreateStore(zeroVal, memberPtr);
                                std::cout << "[DEBUG] Zero-initialized member '" << memberName << "' at entry\n";
                            }
                            else
                            {
                                // For complex types, skip automatic init or handle as you prefer.
                                std::cout << "[DEBUG] Skipping zero-init for complex member type for '" << memberName << "'\n";
                            }
                        }
                    }
                    else
                    {
                        std::cout << "[DEBUG] Skipping default init for member '" << memberName << "' because instance came from new()\n";
                    }
                }
                catch (const std::exception &ex)
                {
                    // Defensive: don't crash codegen in the middle; print debug and continue
                    std::cerr << "[WARN] Initialization failed for member '" << memberName << "': " << ex.what() << "\n";
                }

                if (!info->node)
                {
                    throw std::runtime_error("Node not registered for '" + memberName + "'");
                }

                // === Propagate to the metadata node itself ===
                if (info->node)
                {
                    auto metaIt = semantics.metaData.find(info->node);
                    if (metaIt != semantics.metaData.end())
                    {
                        metaIt->second->llvmValue = memberPtr;
                        if (!metaIt->second->llvmValue)
                            throw std::runtime_error("No llvm value given to this guy look here");
                        metaIt->second->llvmType = getLLVMType(info->type);
                        if (!metaIt->second->llvmType)
                        {
                            throw std::runtime_error("No llvm type given to this guy as it failed or some shit");
                        }
                        std::cout << "[DEBUG] Propagated llvmValue :" << metaIt->second->llvmValue << " to member node '"
                                  << memberName << "' at " << info->node << "\n";
                    }
                }

                index++;
            }
        }

        std::cout << "[DEBUG] Component '" << letName << "' fully allocated with all members.\n";
        return;
    }

    // If the let statement is heap allocated
    if (letStmt->isHeap)
    {
        uint64_t size = sym->componentSize;
        // Declare external sage_alloc and sage_free functions

        llvm::PointerType *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);

        llvm::FunctionCallee sageAllocFunction = module->getOrInsertFunction(
            "sage_alloc",
            i8PtrTy,
            llvm::Type::getInt64Ty(context));

        llvm::FunctionCallee sageFreeFunction = module->getOrInsertFunction(
            "sage_free", llvm::Type::getVoidTy(context),
            llvm::Type::getInt64Ty(context));

        // Allocate in SAGE
        llvm::Value *heapPtr = builder.CreateCall(
            sageAllocFunction,
            {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size)},
            letName + "_ptr");

        sym->llvmValue = heapPtr;
        sym->llvmType = getLLVMType(sym->type);

        llvm::Value *initVal = generateExpression(letStmt->value.get());
        builder.CreateStore(initVal, heapPtr);

        Node *lastUse = sym->lastUseNode ? sym->lastUseNode : letStmt;
        if ((letStmt == lastUse) && (sym->refCount == 0))
        {
            builder.CreateCall(
                sageFreeFunction,
                {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size)});
        }

        std::cout << "[DEBUG] Heap variable '" << letName << "' allocated in SAGE, last-use at "
                  << lastUse << "\n";
        return;
    }

    // === Non-component scalar/array/etc ===
    llvm::Type *varType = getLLVMType(sym->type);
    if (!varType)
    {
        throw std::runtime_error("Failed to get LLVM type equivalent for  '" + sym->type.resolvedName + "'");
    }

    llvm::Value *initVal = letStmt->value ? generateExpression(letStmt->value.get())
                                          : llvm::Constant::getNullValue(varType);

    if (!initVal)
    {
        throw std::runtime_error("No init value");
    }

    llvm::AllocaInst *alloca = entryBuilder.CreateAlloca(varType, nullptr, letName);
    if (!alloca)
    {
        throw std::runtime_error("Failed to create alloca");
    }
    builder.CreateStore(initVal, alloca);
    sym->llvmValue = alloca;
    sym->llvmType = varType;
    std::cout << "[DEBUG] Scalar variable '" << letName << "' allocated at llvmValue = "
              << alloca << " with type = " << varType << "\n";
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

    llvm::Function *fn = builder.GetInsertBlock()->getParent();
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

    builder.CreateStore(initVal, alloca);
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

    llvm::Function *function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "while.cond", function);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "while.body");
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(context, "while.end");

    std::cerr << "[IR DEBUG] Creating branch to while.cond\n";
    builder.CreateBr(condBB);

    builder.SetInsertPoint(condBB);
    std::cerr << "[IR DEBUG] Generating while condition\n";
    llvm::Value *condVal = generateExpression(whileStmt->condition.get());
    if (!condVal)
    {
        throw std::runtime_error("Invalid while condition");
    }

    if (!condVal->getType()->isIntegerTy(1))
    {
        condVal = builder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "whilecond.bool");
    }

    std::cerr << "[IR DEBUG] Creating conditional branch\n";
    builder.CreateCondBr(condVal, bodyBB, endBB);

    // Append bodyBB to function
    function->insert(function->end(), bodyBB);
    builder.SetInsertPoint(bodyBB);
    std::cerr << "[IR DEBUG] Generating while body\n";
    loopBlocksStack.push_back({condBB, endBB});
    generateStatement(whileStmt->loop.get());
    loopBlocksStack.pop_back();
    std::cerr << "[IR DEBUG] Finished generating while body\n";

    if (!builder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to while.cond\n";
        builder.CreateBr(condBB);
    }
    else
    {
        std::cerr << "[IR WARNING] While body block already has terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // Append endBB to function
    function->insert(function->end(), endBB);
    builder.SetInsertPoint(endBB);
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
        condVal = builder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "ifcond.bool");
    }

    // Create basic blocks
    llvm::Function *function = builder.GetInsertBlock()->getParent();
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
    builder.CreateCondBr(condVal, thenBB, nextBB);

    // Generate then branch
    builder.SetInsertPoint(thenBB);
    std::cerr << "[IR DEBUG] Generating then branch\n";
    generateStatement(ifStmt->if_result.get());
    if (!builder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to ifmerge from then\n";
        builder.CreateBr(mergeBB);
    }
    else
    {
        std::cerr << "[IR DEBUG] Skipping branch to ifmerge from then due to terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // Generating elif branches
    for (size_t i = 0; i < ifStmt->elifClauses.size(); ++i)
    {
        function->insert(function->end(), nextBB);
        builder.SetInsertPoint(nextBB);
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
            elifCondVal = builder.CreateICmpNE(elifCondVal, llvm::ConstantInt::get(elifCondVal->getType(), 0), "elifcond.bool");
        }

        llvm::BasicBlock *elifBodyBB = llvm::BasicBlock::Create(context, "elif.body" + std::to_string(i), function);
        llvm::BasicBlock *nextElifBB = (i + 1 < ifStmt->elifClauses.size()) ? llvm::BasicBlock::Create(context, "elif" + std::to_string(i + 1))
                                                                            : (ifStmt->else_result.has_value() ? llvm::BasicBlock::Create(context, "else") : mergeBB);

        std::cerr << "[IR DEBUG] Creating conditional branch for elif " << i << "\n";
        builder.CreateCondBr(elifCondVal, elifBodyBB, nextElifBB);

        builder.SetInsertPoint(elifBodyBB);
        std::cerr << "[IR DEBUG] Generating elif body " << i << "\n";
        generateStatement(elif->elif_result.get());
        if (!builder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Adding branch to ifmerge from elif " << i << "\n";
            builder.CreateBr(mergeBB);
        }
        else
        {
            std::cerr << "[IR DEBUG] Skipping branch " << i << " to ifmerge from elif due to terminator " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
        }

        nextBB = nextElifBB;
    }

    // Generate else branch if present
    if (ifStmt->else_result.has_value())
    {
        function->insert(function->end(), nextBB);
        builder.SetInsertPoint(nextBB);
        std::cerr << "[IR DEBUG] Generating else branch\n";
        generateStatement(ifStmt->else_result.value().get());
        if (!builder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Adding branch to ifmerge from else\n";
            builder.CreateBr(mergeBB);
        }
        else
        {
            std::cerr << "[IR DEBUG] Skipping branch to ifmerge from else due to terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
        }
    }

    // Finalize with merge block
    function->insert(function->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    std::cerr << "[IR DEBUG] Finished generating if statement\n";
}

// IR code gen for a for loop
void IRGenerator::generateForStatement(Node *node)
{
    std::cout << "Generating IR for : " << node->toString();
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        throw std::runtime_error("Invalid for statement");

    llvm::Function *function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *origBB = builder.GetInsertBlock();

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
    builder.CreateBr(condBB);

    // condition
    builder.SetInsertPoint(condBB);
    std::cerr << "[IR DEBUG] Generating condition\n";
    llvm::Value *condVal = generateExpression(forStmt->condition.get());
    if (!condVal)
    {
        std::cerr << "[IR ERROR] For loop has invalid condition\n";
        builder.SetInsertPoint(origBB);
        return;
    }

    auto it = semantics.metaData.find(forStmt->condition.get());
    if (it == semantics.metaData.end() || it->second->type.kind != DataType::BOOLEAN)
    {
        std::cerr << "[IR ERROR] For loop condition must evaluate to boolean.\n";
        builder.SetInsertPoint(origBB);
        return;
    }

    // promote if needed
    if (!condVal->getType()->isIntegerTy(1))
    {
        if (condVal->getType()->isIntegerTy(32))
            condVal = builder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "loopcond.bool");
        else
        {
            std::cerr << "[IR ERROR] For loop condition must be boolean or i32\n";
            builder.SetInsertPoint(origBB);
            return;
        }
    }

    builder.CreateCondBr(condVal, bodyBB, endBB);

    // body
    builder.SetInsertPoint(bodyBB);
    loopBlocksStack.push_back({condBB, stepBB, endBB}); // <-- includes stepBB now
    std::cerr << "[IR DEBUG] Generating loop body\n";
    generateStatement(forStmt->body.get());
    std::cerr << "[IR DEBUG] Finished generating loop body\n";
    loopBlocksStack.pop_back();

    if (!builder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to loop.step\n";
        builder.CreateBr(stepBB);
    }
    else
    {
        std::cerr << "[IR WARNING] Loop body block already has terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // step
    builder.SetInsertPoint(stepBB);
    if (forStmt->step)
    {
        std::cerr << "[IR DEBUG] Generating loop step\n";
        llvm::Value *stepVal = generateExpression(forStmt->step.get());
        if (!stepVal)
        {
            std::cerr << "[IR ERROR] loop step generation returned nullptr!\n";
            builder.CreateBr(condBB); // keep IR consistent
            builder.SetInsertPoint(origBB);
            return;
        }
    }
    builder.CreateBr(condBB);

    // after
    builder.SetInsertPoint(endBB);
}

void IRGenerator::generateBreakStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Break statement not inside a loop");
    }

    llvm::BasicBlock *afterBB = loopBlocksStack.back().afterBB;
    std::cerr << "[IR DEBUG] Generating break to " << afterBB->getName().str() << "\n";
    builder.CreateBr(afterBB);
}

void IRGenerator::generateContinueStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Continue statement not inside a loop");
    }

    llvm::BasicBlock *condBB = loopBlocksStack.back().condBB;
    std::cerr << "[IR DEBUG] Generating continue to " << condBB->getName().str() << "\n";
    builder.CreateBr(condBB);
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
            builder.Insert(pendingFree);
        }
    }
    // Store the value
    builder.CreateStore(initValue, targetPtr);
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
    auto parentVarInfo = semantics.resolveSymbolInfo(parentVarName);
    if (!parentVarInfo || !parentVarInfo->llvmValue)
        throw std::runtime_error("Unresolved parent instance: " + parentVarName);

    // Resolve parent type and member info
    auto parentTypeName = parentVarInfo->type.resolvedName;
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
    llvm::Value *fieldPtr = builder.CreateStructGEP(structTy, parentVarInfo->llvmValue, fieldIndex, childName);
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
        llvm::Value *heapPtr = builder.CreateLoad(elemPtrTy, fieldPtr, childName + "_heap_load");

        // If heapPtr == null -> allocate; otherwise reuse existing pointer.
        llvm::Value *nullPtr = llvm::ConstantPointerNull::get(elemPtrTy);
        llvm::Value *isNull = builder.CreateICmpEQ(heapPtr, nullPtr, childName + "_is_null");

        llvm::Function *parentFn = builder.GetInsertBlock()->getParent();
        llvm::BasicBlock *allocBB = llvm::BasicBlock::Create(context, childName + "_alloc", parentFn);
        llvm::BasicBlock *contBB = llvm::BasicBlock::Create(context, childName + "_cont", parentFn);

        // Cond branch on null
        builder.CreateCondBr(isNull, allocBB, contBB);

        // --- allocBB: call sage_alloc(size), bitcast to elemPtrTy, store into slot, branch to contBB
        builder.SetInsertPoint(allocBB);

        llvm::PointerType *i8PtrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
        llvm::FunctionCallee sageAllocFn = module->getOrInsertFunction("sage_alloc", i8PtrTy, llvm::Type::getInt64Ty(context));

        // compute size (use componentSize from semantics if present or DataLayout)
        uint64_t size = module->getDataLayout().getTypeAllocSize(elemTy);

        llvm::Value *sizeArg = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size);

        llvm::Value *rawAlloc = builder.CreateCall(sageAllocFn, {sizeArg}, childName + "_alloc_i8ptr");
        // bitcast i8* -> elem*
        llvm::Value *newHeapPtr = builder.CreateBitCast(rawAlloc, elemPtrTy, childName + "_alloc_ptr");

        // store pointer into the struct slot
        builder.CreateStore(newHeapPtr, fieldPtr);
        // branch to continuation
        builder.CreateBr(contBB);

        // --- contBB: reload heapPtr from slot (now definitely non-null)
        builder.SetInsertPoint(contBB);
        llvm::Value *heapPtr2 = builder.CreateLoad(elemPtrTy, fieldPtr, childName + "_heap");
        // use heapPtr2 for storing rhs
        builder.CreateStore(rhs, heapPtr2);

        // After the store, if this member's symbol marks this fieldStmt as last use, free it
        // (memberInfo->lastUseNode should exist from semantics)
        if (memberInfo->isHeap && (memberInfo->lastUseNode == fieldStmt || memberInfo->lastUseNode == /* possibly other node pointer */ memberInfo->node))
        {
            llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
                "sage_free", llvm::Type::getVoidTy(context), llvm::Type::getInt64Ty(context));
            builder.CreateCall(sageFreeFn, {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size)});
        }

        // continue building after contBB (builder is already positioned at contBB end)
    }
    else
    {
        // Non-heap: store RHS directly into the field slot
        builder.CreateStore(rhs, fieldPtr);
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
        if (builder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Terminator found in block statement child: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
            break;
        }
    }
}

void IRGenerator::generateFunctionStatement(Node *node)
{
    auto fnStmt = dynamic_cast<FunctionStatement *>(node);
    if (!fnStmt)
        return;

    llvm::BasicBlock *oldInsertPoint = builder.GetInsertBlock();

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
        builder.SetInsertPoint(oldInsertPoint);
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

    llvm::Function *currentFunction = builder.GetInsertBlock()->getParent();
    if (retVal)
    {
        // Ensure the return type matches
        if (retVal->getType() != currentFunction->getReturnType())
        {
            llvm::errs() << "Return type mismatch\n";
        }
        builder.CreateRet(retVal);
    }
    else
    {
        // For void functions
        if (currentFunction->getReturnType()->isVoidTy())
            builder.CreateRetVoid();
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

    if (infix->operat.type == TokenType::FULLSTOP)
    {
        std::cout << "TRIGGERED INFIX FULLSTOP GUY\n";
        // Generate the left-hand object (e.g., 'p')
        llvm::Value *objectVal = generateExpression(infix->left_operand.get());

        auto memberIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
        if (!memberIdent)
            throw std::runtime_error("Right-hand side of '.' must be a field identifier");

        std::string memberName = memberIdent->expression.TokenLiteral;

        // Lookup left-hand symbol
        auto leftMetaIt = semantics.metaData.find(infix->left_operand.get());
        if (leftMetaIt == semantics.metaData.end())
            throw std::runtime_error("Left-hand object metadata missing");

        std::string parentTypeName = leftMetaIt->second->type.resolvedName;

        auto parentIt = semantics.customTypesTable.find(parentTypeName);
        if (parentIt == semantics.customTypesTable.end())
            throw std::runtime_error("Type '" + parentTypeName + "' doesn't exist");

        auto memberIt = parentIt->second.members.find(memberName);
        if (memberIt == parentIt->second.members.end())
            throw std::runtime_error("Member '" + memberName + "' not found in type '" + parentTypeName + "'");

        auto &index = llvmStructIndices[parentTypeName];
        unsigned memberIndex = index;
        llvm::StructType *structTy = llvmCustomTypes[parentTypeName];

        // Compute pointer to the member dynamically
        llvm::Value *memberPtr = builder.CreateStructGEP(structTy, objectVal, memberIndex, memberName);
        if (!memberPtr)
            throw std::runtime_error("No llvm value for '" + memberName + "'");

        // Load the value
        llvm::Type *memberType = getLLVMType(memberIt->second->type);
        if (!memberType)
            throw std::runtime_error("Member Type wasnt retrieved");
        return builder.CreateLoad(memberType, memberPtr, memberName + "_val");
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
                return builder.CreateSExt(val, llvm::IntegerType::get(context, toBits), "sexttmp");
            else
                return builder.CreateZExt(val, llvm::IntegerType::get(context, toBits), "zexttmp");
        }
        else
        {
            // Truncation if needed (rare, probably invalid here)
            return builder.CreateTrunc(val, llvm::IntegerType::get(context, toBits), "trunctmp");
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
        if (left->getType() != builder.getInt1Ty())
            left = builder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0), "boolcastl");
        if (right->getType() != builder.getInt1Ty())
            right = builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0), "boolcastr");

        if (infix->operat.type == TokenType::AND)
            return builder.CreateAnd(left, right, "andtmp");
        else
            return builder.CreateOr(left, right, "ortmp");
    }

    // Handle floating point conversions
    if (resultType.kind == DataType::FLOAT)
    {
        if (isIntegerType(leftType))
            left = builder.CreateSIToFP(left, llvm::Type::getFloatTy(context), "inttofloat");
        if (isIntegerType(rightType))
            right = builder.CreateSIToFP(right, llvm::Type::getFloatTy(context), "inttofloat");
    }
    else if (resultType.kind == DataType::DOUBLE)
    {
        if (isIntegerType(leftType))
            left = builder.CreateSIToFP(left, llvm::Type::getDoubleTy(context), "inttodouble");
        if (isIntegerType(rightType))
            right = builder.CreateSIToFP(right, llvm::Type::getDoubleTy(context), "inttodouble");
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
                return builder.CreateICmpEQ(left, right, "cmptmp");
            case TokenType::NOT_EQUALS:
                return builder.CreateICmpNE(left, right, "cmptmp");
            case TokenType::LESS_THAN:
                return signedInt ? builder.CreateICmpSLT(left, right, "cmptmp")
                                 : builder.CreateICmpULT(left, right, "cmptmp");
            case TokenType::LT_OR_EQ:
                return signedInt ? builder.CreateICmpSLE(left, right, "cmptmp")
                                 : builder.CreateICmpULE(left, right, "cmptmp");
            case TokenType::GREATER_THAN:
                return signedInt ? builder.CreateICmpSGT(left, right, "cmptmp")
                                 : builder.CreateICmpUGT(left, right, "cmptmp");
            case TokenType::GT_OR_EQ:
                return signedInt ? builder.CreateICmpSGE(left, right, "cmptmp")
                                 : builder.CreateICmpUGE(left, right, "cmptmp");
            default:
                throw std::runtime_error("Unsupported int comparison operator");
            }
        }
        else if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
        {
            switch (infix->operat.type)
            {
            case TokenType::EQUALS:
                return builder.CreateFCmpOEQ(left, right, "fcmptmp");
            case TokenType::NOT_EQUALS:
                return builder.CreateFCmpONE(left, right, "fcmptmp");
            case TokenType::LESS_THAN:
                return builder.CreateFCmpOLT(left, right, "fcmptmp");
            case TokenType::LT_OR_EQ:
                return builder.CreateFCmpOLE(left, right, "fcmptmp");
            case TokenType::GREATER_THAN:
                return builder.CreateFCmpOGT(left, right, "fcmptmp");
            case TokenType::GT_OR_EQ:
                return builder.CreateFCmpOGE(left, right, "fcmptmp");
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
        if (isIntegerType(resultType.kind))
            return builder.CreateAdd(left, right, "addtmp");
        else
            return builder.CreateFAdd(left, right, "faddtmp");

    case TokenType::MINUS:
        if (isIntegerType(resultType.kind))
            return builder.CreateSub(left, right, "subtmp");
        else
            return builder.CreateFSub(left, right, "fsubtmp");

    case TokenType::ASTERISK:
        if (isIntegerType(resultType.kind))
            return builder.CreateMul(left, right, "multmp");
        else
            return builder.CreateFMul(left, right, "fmultmp");

    case TokenType::DIVIDE:
        if (isIntegerType(resultType.kind))
            return isSignedInteger(resultType.kind) ? builder.CreateSDiv(left, right, "divtmp")
                                                    : builder.CreateUDiv(left, right, "divtmp");
        else
            return builder.CreateFDiv(left, right, "fdivtmp");

    case TokenType::MODULUS:
        if (isIntegerType(resultType.kind))
            return isSignedInteger(resultType.kind) ? builder.CreateSRem(left, right, "modtmp")
                                                    : builder.CreateURem(left, right, "modtmp");
        else
            throw std::runtime_error("Modulus not supported for FLOAT or DOUBLE at line " +
                                     std::to_string(infix->operat.line));

    default:
        throw std::runtime_error("Unsupported infix operator: " + infix->operat.TokenLiteral +
                                 " at line " + std::to_string(infix->operat.line));
    }
}

// Prefix expression generator function
llvm::Value *IRGenerator::generatePrefixExpression(Node *node)
{
    auto prefix = dynamic_cast<PrefixExpression *>(node);
    if (!prefix)
        throw std::runtime_error("Invalid prefix expression");

    llvm::Value *operand = generateIdentifierAddress(prefix->operand.get()).address;
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
            return builder.CreateFNeg(operand, llvm::Twine("fnegtmp"));
        else if (isIntType(resultType.kind))
            return builder.CreateNeg(operand, llvm::Twine("negtmp"));
        else
            throw std::runtime_error("Unsupported type for unary minus");

    case TokenType::BANG:
        // Boolean NOT
        return builder.CreateNot(operand, llvm::Twine("nottmp"));

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

        llvm::Value *loaded = builder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

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
                          ? builder.CreateFAdd(loaded, delta, llvm::Twine("preincfptmp"))
                          : builder.CreateAdd(loaded, delta, llvm::Twine("preinctmp"));
        else
            updated = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                          ? builder.CreateFSub(loaded, delta, llvm::Twine("predecfptmp"))
                          : builder.CreateSub(loaded, delta, llvm::Twine("predectmp"));

        builder.CreateStore(updated, varPtr);

        if (pendingFree)
        {
            builder.Insert(pendingFree);
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

    llvm::Value *originalValue = builder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

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
                           ? builder.CreateFAdd(originalValue, delta, llvm::Twine("finc"))
                           : builder.CreateAdd(originalValue, delta, llvm::Twine("inc"));
    else if (postfix->operator_token.type == TokenType::MINUS_MINUS)
        updatedValue = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                           ? builder.CreateFSub(originalValue, delta, llvm::Twine("fdec"))
                           : builder.CreateSub(originalValue, delta, llvm::Twine("dec"));
    else
        throw std::runtime_error("Unsupported postfix operator: " + postfix->operator_token.TokenLiteral +
                                 " at line " + std::to_string(postfix->operator_token.line));

    builder.CreateStore(updatedValue, varPtr);

    if (pendingFree)
    {
        // Insert the prepared free at the current insertion point
        builder.Insert(pendingFree); // inserts at current position (after store)
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
    llvm::Value *strConst = builder.CreateGlobalStringPtr(raw);
    return strConst;
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
        throw std::runtime_error("Invalid identifier expression");

    const std::string &identName = identExpr->identifier.TokenLiteral;

    // Lookup symbol in metaData
    auto metaIt = semantics.metaData.find(identExpr);
    if (metaIt == semantics.metaData.end())
        throw std::runtime_error("Unidentified identifier '" + identName + "'");

    auto sym = metaIt->second;
    if (sym->hasError)
    {
        throw std::runtime_error("Semantic error detected ");
    }

    llvm::Value *variablePtr = generateIdentifierAddress(identExpr).address;
    if (!variablePtr)
        throw std::runtime_error("No llvm value was assigned for '" + identName + "'");

    // Check if this symbol is a struct/component instance
    auto compIt = componentTypes.find(sym->type.resolvedName);
    if (compIt != componentTypes.end())
    {
        if (!variablePtr)
        {
            std::cerr << "[ERROR] Component instance '" << identName
                      << "' has null llvmValue!" << std::endl;
            throw std::runtime_error("Component llvmValue null");
        }

        // Always return pointer to the struct instance
        return variablePtr;
    }

    if (sym->isHeap)
    {
        // variablePtr should be a pointer to the element type (we stored typed pointer in let)
        llvm::Type *elemTy = sym->llvmType;
        if (!elemTy)
            throw std::runtime_error("llvmType null for heap scalar '" + identName + "'");

        // Ensure pointer type matches expected: if not, bitcast to elemTy*
        llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
        if (variablePtr->getType() != expectedPtrTy)
        {
            variablePtr = builder.CreateBitCast(variablePtr, expectedPtrTy, identName + "_ptr_typed");
        }

        // Load the value first (so we can return it)
        llvm::Value *loadedVal = builder.CreateLoad(elemTy, variablePtr, identName + "_val");

        // If this identifier is the last use, emit sage_free(size) AFTER loading
        if ((sym->lastUseNode == identExpr) && (sym->refCount == 0))
        {
            // get or insert sage_free: void sage_free(i64)
            llvm::FunctionCallee sageFreeFunction = module->getOrInsertFunction(
                "sage_free",
                llvm::Type::getVoidTy(context),
                llvm::Type::getInt64Ty(context));

            // emit call with the known component size
            builder.CreateCall(sageFreeFunction,
                               {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->componentSize)});
        }

        std::cout << "[DEBUG] Returning heap scalar '" << identName << "' (lastUse="
                  << (sym->lastUseNode == identExpr ? "yes" : "no") << ")\n";
        return loadedVal;
    }

    auto identType = getLLVMType(sym->type);
    // Scalar case: pointer -> load
    if (variablePtr->getType()->isPointerTy())
    {
        if (!identType)
            throw std::runtime_error("llvmType null for scalar '" + identName + "'");
        return builder.CreateLoad(identType, variablePtr, identName + "_val");
    }

    if (!variablePtr)
    {
        throw std::runtime_error("Variable ptr is null ");
    }

    std::cout << "ENDED IDENTIFIER GEN\n";
    return variablePtr;
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
    auto loadedPointer = builder.CreateLoad(pointerType, pointerAddress, name + "_addr");
    if (!loadedPointer)
        throw std::runtime_error("Loaded pointer not created");

    // Loading the actual value stored at the address of the pointee
    auto elementType = getLLVMType(derefSym->type);
    if (!elementType)
        throw std::runtime_error("Failed to generate llvm type for pointee type '" + derefSym->type.resolvedName + "'");

    auto finalValue = builder.CreateLoad(elementType, loadedPointer, name + "_val");
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
    {
        std::cout << "Got node " << node->toString() << "\n";
        throw std::runtime_error("Invalid identifier expression");
    }

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
        out.address = variablePtr;
    }
    else
    {
        // scalar/heap -> ensure typed pointer
        if (sym->isHeap)
        {
            llvm::Type *elemTy = sym->llvmType;
            if (!elemTy)
                throw std::runtime_error("llvmType null for heap scalar '" + identName + "'");
            llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
            if (variablePtr->getType() != expectedPtrTy)
            {
                variablePtr = builder.CreateBitCast(variablePtr, expectedPtrTy, identName + "_ptr_typed");
            }
            out.address = variablePtr;
        }
        else
        {
            if (variablePtr->getType()->isPointerTy())
            {
                out.address = variablePtr;
            }
            else
            {
                throw std::runtime_error("Identifier '" + identName + "' does not have pointer-like llvmValue");
            }
        }
    }

    // If this identifier is the last use, prepare (but do NOT insert) the sage_free call.
    // We'll create a CallInst but not insert into any block; the caller will insert it at the right spot.
    if (sym->isHeap)
    {
        if ((sym->lastUseNode == identExpr) && (sym->refCount == 0))
        {
            llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
                "sage_free",
                llvm::Type::getVoidTy(context),
                llvm::Type::getInt64Ty(context));

            // Prepare size constant
            llvm::Value *sizeArg = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), sym->componentSize);

            // Create call instruction WITHOUT inserting it into a block.
            // Use CallInst::Create and do not provide InsertBefore (nullptr) — this yields an instruction that is not attached.
            llvm::CallInst *callInst = llvm::CallInst::Create(sageFreeFn, {sizeArg});
            callInst->setCallingConv(llvm::CallingConv::C);
            // Do NOT insert yet.
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
            componentInstance = builder.CreateBitCast(componentInstance, expectedPtrTy, (fieldName + "_instance_cast").c_str());
        }
        else
        {
            throw std::runtime_error("Component instance is not a pointer type for '" + compName + "'");
        }
    }

    // Now get the pointer to the field
    llvm::Value *fieldPtr = builder.CreateStructGEP(structTy, componentInstance, (unsigned)memberIndex, fieldName + "_ptr");
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
        if (builder.GetInsertBlock()->getTerminator())
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

    // Creating the entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
    builder.SetInsertPoint(entry);

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
        llvm::AllocaInst *alloca = builder.CreateAlloca(getLLVMType(pIt->second->type), nullptr, p->statement.TokenLiteral);
        builder.CreateStore(&(*argIter), alloca);
        pIt->second->llvmValue = alloca;

        argIter++;
    }

    // This will handle whatever is inside the block including the return value
    generateExpression(fnExpr->block.get());
    return fn;
}

llvm::Value *IRGenerator::generateCallExpression(Node *node)
{
    auto callExpr = dynamic_cast<CallExpression *>(node);
    if (!callExpr)
    {
        throw std::runtime_error("Invalid call expression");
    }

    auto callIt = semantics.metaData.find(callExpr);
    if (callIt == semantics.metaData.end())
    {
        throw std::runtime_error("Call expression does not exist");
    }
    if (callIt->second->hasError)
    {
        return nullptr; // If it has an error we just stop IR generation
    }

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
    llvm::Value *call = builder.CreateCall(calledFunc, argsV, "calltmp");

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
    generatorFunctionsMap[typeid(BehaviorStatement)] = &IRGenerator::generateBehaviorStatement;
    generatorFunctionsMap[typeid(ComponentStatement)] = &IRGenerator::generateComponentStatement;
    generatorFunctionsMap[typeid(EnumClassStatement)] = &IRGenerator::generateEnumClassStatement;
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
    return builder.GetInsertBlock() != nullptr;
}

void IRGenerator::dumpIR()
{
    module->print(llvm::outs(), nullptr);
}

bool IRGenerator::currentBlockIsTerminated()
{
    llvm::BasicBlock *bb = builder.GetInsertBlock();
    return bb && bb->getTerminator();
}

llvm::Module &IRGenerator::getLLVMModule()
{
    return *module;
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
    const std::string targetTripleStr = "x86_64-pc-linux-gnu";

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
