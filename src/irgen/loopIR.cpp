#include "irgen.hpp"

// Generate while statament
void IRGenerator::generateWhileStatement(Node *node) {
  auto whileStmt = dynamic_cast<WhileStatement *>(node);
  if (!whileStmt)
    throw std::runtime_error("Invalid while statement");
  if (isGlobalScope)
    throw std::runtime_error("Cannot use a while loop in global scope");

  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();

  // Create blocks
  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(context, "while.cond", function);
  llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "while.body");
  llvm::BasicBlock *endBB = llvm::BasicBlock::Create(context, "while.end");

  // Initial jump into the condition check
  std::cerr << "[IR DEBUG] Creating branch to while.cond\n";
  funcBuilder.CreateBr(condBB);

  // --- CONDITION BLOCK ---
  funcBuilder.SetInsertPoint(condBB);
  std::cerr << "[IR DEBUG] Generating while condition\n";
  llvm::Value *condVal = generateExpression(whileStmt->condition.get());

  if (!condVal->getType()->isIntegerTy(1)) {
    condVal = funcBuilder.CreateICmpNE(
        condVal, llvm::ConstantInt::get(condVal->getType(), 0),
        "whilecond.bool");
  }
  funcBuilder.CreateCondBr(condVal, bodyBB, endBB);

  // --- BODY BLOCK ---
  function->insert(function->end(), bodyBB);
  funcBuilder.SetInsertPoint(bodyBB);

  // PUSH: {breakTarget, continueTarget}
  // For 'while', continue goes back to the condition (condBB)
  jumpStack.push_back({endBB, condBB});

  std::cerr << "[IR DEBUG] Generating while body\n";
  generateStatement(whileStmt->loop.get());

  // POP: Leaving body scope
  jumpStack.pop_back();

  // Resident Sweep: Clean up locals at the end of the lap
  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    // REWIRED: Using the helper for consistency
    emitResidentSweep();
    semantics.loopResidentDeathRow.clear();

    std::cerr << "[IR DEBUG] Adding branch to while.cond\n";
    funcBuilder.CreateBr(condBB);
  }

  // --- EXIT BLOCK ---
  function->insert(function->end(), endBB);
  funcBuilder.SetInsertPoint(endBB);

  // Final Death Row: Cleanup variables that only exist for the loop's life
  for (auto *sym : semantics.loopDeathRow) {
    if (sym->needsPostLoopFree) {
      if (sym->isDheap) {
        auto it = semantics.allocatorMap.find(sym->allocType);
        if (it != semantics.allocatorMap.end()) {
          llvm::Function *freeFunc = module->getFunction(it->second.freeName);
          if (freeFunc)
            funcBuilder.CreateCall(freeFunc, {sym->llvmValue});
        }
      } else if (sym->isHeap) {
        llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
            "sage_free", llvm::Type::getVoidTy(context));
        funcBuilder.CreateCall(sageFreeFn);
      }
      sym->needsPostLoopFree = false;
    }
  }
}

// IR code gen for a for loop
void IRGenerator::generateForStatement(Node *node) {
  auto forStmt = dynamic_cast<ForStatement *>(node);
  if (!forStmt)
    throw std::runtime_error("Invalid for statement");

  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();

  // 1. Initializer: Sets up the loop variable (e.g., mut i32 i = 0)
  if (forStmt->initializer) {
    std::cerr << "[IR DEBUG] Generating initializer\n";
    generateStatement(forStmt->initializer.get());
  }

  // 2. Create the 4 core blocks of a 'for' loop
  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(context, "loop.cond", function);
  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(context, "loop.body", function);
  llvm::BasicBlock *stepBB =
      llvm::BasicBlock::Create(context, "loop.step", function);
  llvm::BasicBlock *endBB =
      llvm::BasicBlock::Create(context, "loop.end", function);

  // Initial entry into the loop condition
  funcBuilder.CreateBr(condBB);

  // 3. Condition Block: Check if we should keep looping
  funcBuilder.SetInsertPoint(condBB);
  std::cerr << "[IR DEBUG] Generating condition\n";
  llvm::Value *condVal = generateExpression(forStmt->condition.get());

  // Boolean promotion
  if (!condVal->getType()->isIntegerTy(1)) {
    condVal = funcBuilder.CreateICmpNE(
        condVal, llvm::ConstantInt::get(condVal->getType(), 0),
        "loopcond.bool");
  }
  funcBuilder.CreateCondBr(condVal, bodyBB, endBB);

  // 4. Body Block: Where the code inside the { } lives
  function->insert(function->end(), bodyBB);
  funcBuilder.SetInsertPoint(bodyBB);

  // --- NEW JUMP STACK PUSH ---
  // For 'break', go to endBB. For 'continue', go to stepBB!
  jumpStack.push_back({endBB, stepBB});

  std::cerr << "[IR DEBUG] Generating loop body\n";
  generateStatement(forStmt->body.get());

  // --- NEW JUMP STACK POP ---
  jumpStack.pop_back();

  // Resident Sweep: Free locals at the end of a successful lap
  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    // REWIRED: Using the helper now
    emitResidentSweep();
    semantics.loopResidentDeathRow.clear();

    // After the body and sweep, go to the STEP block
    std::cerr << "[IR DEBUG] Adding branch to loop.step\n";
    funcBuilder.CreateBr(stepBB);
  }

  // 5. Step Block: The increment (e.g., i = i + 1)
  function->insert(function->end(), stepBB);
  funcBuilder.SetInsertPoint(stepBB);
  if (forStmt->step) {
    std::cerr << "[IR DEBUG] Generating loop step\n";
    generateStatement(forStmt->step.get());
  }
  // After stepping, always go back to re-check the condition
  funcBuilder.CreateBr(condBB);

  // 6. End Block: The exit gate
  function->insert(function->end(), endBB);
  funcBuilder.SetInsertPoint(endBB);

  // Final Death Row: Cleanup variables that only exist for the loop's life
  for (auto *sym : semantics.loopDeathRow) {
    if (sym->needsPostLoopFree) {
      // Logic for post-loop cleanup
      if (sym->isDheap) {
        auto it = semantics.allocatorMap.find(sym->allocType);
        if (it != semantics.allocatorMap.end()) {
          llvm::Function *freeFunc = module->getFunction(it->second.freeName);
          if (freeFunc)
            funcBuilder.CreateCall(freeFunc, {sym->llvmValue});
        }
      } else if (sym->isHeap) {
        llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
            "sage_free", llvm::Type::getVoidTy(context));
        funcBuilder.CreateCall(sageFreeFn);
      }
      sym->needsPostLoopFree = false;
    }
  }
}

void IRGenerator::generateBreakStatement(Node *node) {
  if (jumpStack.empty()) {
    throw std::runtime_error("Break statement not inside a loop or switch");
  }

  llvm::BasicBlock *target = jumpStack.back().breakTarget;

  // Debug print (Safer check)
  std::cerr << "[IR DEBUG] Generating break to "
            << (target->hasName() ? target->getName().str() : "unnamed block")
            << "\n";

  funcBuilder.CreateBr(target);

  // Creating a dummy block for any dead code following the break
  llvm::Function *parent = funcBuilder.GetInsertBlock()->getParent();
  llvm::BasicBlock *deadBB =
      llvm::BasicBlock::Create(context, "break.dead", parent);
  funcBuilder.SetInsertPoint(deadBB);
}

void IRGenerator::generateContinueStatement(Node *node) {
  for (auto it = jumpStack.rbegin(); it != jumpStack.rend(); ++it) {
    if (it->continueTarget != nullptr) {
      std::cerr << "[IR DEBUG] Generating continue to loop header\n";
      funcBuilder.CreateBr(it->continueTarget);

      llvm::Function *parent = funcBuilder.GetInsertBlock()->getParent();
      llvm::BasicBlock *deadBB =
          llvm::BasicBlock::Create(context, "cont.dead", parent);
      funcBuilder.SetInsertPoint(deadBB);
      return;
    }
  }

  throw std::runtime_error("Continue statement used outside of a loop!");
}

void IRGenerator::emitResidentSweep() {
  if (semantics.loopResidentDeathRow.empty()) {
    return;
  }

  std::cerr << "[IR DEBUG] Emitting resident memory sweep\n";

  for (auto *sym : semantics.loopResidentDeathRow) {
    if (sym->isDheap) {
      const std::string &allocatorTypeName = sym->allocType;
      auto it = semantics.allocatorMap.find(allocatorTypeName);
      if (it != semantics.allocatorMap.end()) {
        auto handle = it->second;
        llvm::Function *freeFunc = module->getFunction(handle.freeName);
        if (freeFunc) {
          // Force free the local resident (dheap)
          funcBuilder.CreateCall(freeFunc, {sym->llvmValue});
        }
      }
    } else if (sym->isHeap) {
      // SAGE free for locals (heap)
      llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
          "sage_free", llvm::Type::getVoidTy(context));
      funcBuilder.CreateCall(sageFreeFn);
    }
  }
}
