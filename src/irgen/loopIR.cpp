#include "irgen.hpp"

// Generate while statament
void IRGenerator::generateWhileStatement(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
    {
        throw std::runtime_error("Invalid while statement");
    }

    if (isGlobalScope)
        throw std::runtime_error("Cannot use a while loop in global scope");

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

    // The resident sweep: Clean up locals before jumping back to condBB
    // This happens AT THE END of the while.body block.
    for (auto *sym : semantics.loopResidentDeathRow)
    {
        // We only free if it's a heap/dheap resident
        if (sym->isDheap)
        {
            const std::string &allocatorTypeName = sym->allocType;
            auto it = semantics.allocatorMap.find(allocatorTypeName);
            if (it != semantics.allocatorMap.end())
            {
                auto handle = it->second;
                llvm::Function *freeFunc = module->getFunction(handle.freeName);
                if (freeFunc)
                {
                    // Force free the local resident at the end of the lap
                    funcBuilder.CreateCall(freeFunc, {sym->llvmValue});
                }
            }
        }
        else if (sym->isHeap)
        {
            // SAGE free for locals
            llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
                "sage_free", llvm::Type::getVoidTy(context));
            funcBuilder.CreateCall(sageFreeFn);
        }
    }
    // Clear the residents so they don't leak into the next loop
    semantics.loopResidentDeathRow.clear();
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

    for (auto *sym : semantics.loopDeathRow)
    {
        if (sym->isHeap)
        {
            if (sym->needsPostLoopFree)
            {
                // SAGE is a stack: we don't pass the pointer!
                llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
                    "sage_free",
                    llvm::Type::getVoidTy(context));

                // Create and insert the call
                funcBuilder.CreateCall(sageFreeFn);

                // if this symbol appears multiple times in the death row
                sym->needsPostLoopFree = false;
            }
        }

        if (sym->isDheap)
        {
            if (sym->needsPostLoopFree)
            {

                // This is a direct copy of your logic from generateIdentifierAddress
                const std::string &allocatorTypeName = sym->allocType;
                auto it = semantics.allocatorMap.find(allocatorTypeName);
                if (it != semantics.allocatorMap.end())
                {
                    auto handle = it->second;
                    llvm::Function *freeFunc = module->getFunction(handle.freeName);
                    if (freeFunc)
                    {
                        // Force the free at the exit gate
                        funcBuilder.CreateCall(freeFunc, {sym->llvmValue});
                    }
                }
                // Clear the flag so it doesn't free again at the end of the function
                sym->needsPostLoopFree = false;
            }
        }
    }
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
    for (auto *sym : semantics.loopResidentDeathRow)
    {
        if (sym->isDheap)
        {
            auto it = semantics.allocatorMap.find(sym->allocType);
            if (it != semantics.allocatorMap.end())
            {
                llvm::Function *freeFunc = module->getFunction(it->second.freeName);
                if (freeFunc)
                    funcBuilder.CreateCall(freeFunc, {sym->llvmValue});
            }
        }
        else if (sym->isHeap)
        {
            llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction("sage_free", llvm::Type::getVoidTy(context));
            funcBuilder.CreateCall(sageFreeFn);
        }
    }
    semantics.loopResidentDeathRow.clear();
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

    for (auto *sym : semantics.loopDeathRow)
    {
        if (sym->needsPostLoopFree)
        {
            if (sym->isDheap)
            {
                auto it = semantics.allocatorMap.find(sym->allocType);
                if (it != semantics.allocatorMap.end())
                {
                    llvm::Function *freeFunc = module->getFunction(it->second.freeName);
                    if (freeFunc)
                        funcBuilder.CreateCall(freeFunc, {sym->llvmValue});
                }
            }
            else if (sym->isHeap)
            {
                llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction("sage_free", llvm::Type::getVoidTy(context));
                funcBuilder.CreateCall(sageFreeFn);
            }
            sym->needsPostLoopFree = false;
        }
    }
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