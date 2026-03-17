#include "irgen.hpp"

// Generate while statament
void IRGenerator::generateWhileStatement(Node *node) {
  auto whileStmt = dynamic_cast<WhileStatement *>(node);
  if (!whileStmt) {
    reportDevBug("Invalid while statement ", node->token.line,
                 node->token.line);
  }

  if (isGlobalScope) {
    reportDevBug("Cannot use a while loop in a global scope",
                 whileStmt->statement.line, whileStmt->statement.column);
  }

  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();

  // ---  Create the blocks ---
  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(context, "while.cond", function);
  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(context, "while.body", function);
  llvm::BasicBlock *endBB =
      llvm::BasicBlock::Create(context, "while.end", function);

  // --- Initial jump to condition ---
  funcBuilder.CreateBr(condBB);

  // ---  Condition block ---
  funcBuilder.SetInsertPoint(condBB);
  llvm::Value *condVal = generateExpression(whileStmt->condition.get());

  // Promote to boolean if needed
  if (!condVal->getType()->isIntegerTy(1)) {
    condVal = funcBuilder.CreateICmpNE(
        condVal, llvm::ConstantInt::get(condVal->getType(), 0),
        "whilecond.bool");
  }

  funcBuilder.CreateCondBr(condVal, bodyBB, endBB);

  // ---  Body block ---
  funcBuilder.SetInsertPoint(bodyBB);

  // Push jump targets: break -> endBB, continue -> condBB
  jumpStack.push_back({endBB, condBB, whileStmt->loop.get()});

  generateStatement(whileStmt->loop.get());

  funcBuilder.CreateBr(condBB);

  // Pop after body generation
  jumpStack.pop_back();

  // ---End block ---
  funcBuilder.SetInsertPoint(endBB);
  emitBlockCleanUp(whileStmt->loop.get());
}

// IR code gen for a for loop
void IRGenerator::generateForStatement(Node *node) {
  auto forStmt = dynamic_cast<ForStatement *>(node);
  if (!forStmt) {
    reportDevBug("Invalid for statement", node->token.line, node->token.column);
  }

  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();

  // Initializer
  if (forStmt->initializer) {
    generateStatement(forStmt->initializer.get());
  }

  // Block creation
  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(context, "loop.cond", function);
  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(context, "loop.body", function);
  llvm::BasicBlock *stepBB =
      llvm::BasicBlock::Create(context, "loop.step", function);
  llvm::BasicBlock *endBB =
      llvm::BasicBlock::Create(context, "loop.end", function);

  // Jump into condition first
  funcBuilder.CreateBr(condBB);

  // Condition block
  funcBuilder.SetInsertPoint(condBB);

  llvm::Value *condVal = generateExpression(forStmt->condition.get());

  // Boolean normalize
  if (!condVal->getType()->isIntegerTy(1)) {
    condVal = funcBuilder.CreateICmpNE(
        condVal, llvm::ConstantInt::get(condVal->getType(), 0),
        "loopcond.bool");
  }

  funcBuilder.CreateCondBr(condVal, bodyBB, endBB);

  // Body block
  funcBuilder.SetInsertPoint(bodyBB);

  // Push break / continue targets
  jumpStack.push_back({endBB, stepBB, forStmt->body.get()});

  generateStatement(forStmt->body.get());

  // Pop jump stack
  jumpStack.pop_back();

  // If body didn't already terminate, clean and jump to step
  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    funcBuilder.CreateBr(stepBB);
  }

  // Step block
  funcBuilder.SetInsertPoint(stepBB);

  if (forStmt->step) {
    generateStatement(forStmt->step.get());
  }

  funcBuilder.CreateBr(condBB);

  // End block
  funcBuilder.SetInsertPoint(endBB);
  emitBlockCleanUp(forStmt->body.get());
}

void IRGenerator::generateBreakStatement(Node *node) {
  for (auto it = jumpStack.rbegin(); it != jumpStack.rend(); ++it) {
    if (it->breakTarget) {
      if (it->target) {
        logInternal("[BREAK-CLEANUP] Freeing before jumping out of "
                    "loop: " +
                    it->target->toString());

        freeNatives(it->target);
      }
      funcBuilder.CreateBr(it->breakTarget);

      auto *function = funcBuilder.GetInsertBlock()->getParent();
      auto *deadBB =
          llvm::BasicBlock::Create(context, "unreachable.break", function);
      funcBuilder.SetInsertPoint(deadBB);
      return;
    }
  }

  reportDevBug("Break used outside a loop or switch", node->token.line,
               node->token.column);
}

void IRGenerator::generateContinueStatement(Node *node) {
  for (auto it = jumpStack.rbegin(); it != jumpStack.rend(); ++it) {
    if (it->continueTarget) {
      if (it->target) {
        logInternal("[CONTINUE-CLEANUP] Clearing iteration natives for: " +
                    it->target->toString());

        freeNatives(it->target);
      }
      funcBuilder.CreateBr(it->continueTarget);

      // Guard against unreachable code
      auto *function = funcBuilder.GetInsertBlock()->getParent();
      auto *deadBB =
          llvm::BasicBlock::Create(context, "unreachable.continue", function);
      funcBuilder.SetInsertPoint(deadBB);
      return;
    }
  }
  reportDevBug("Continue used outside a loop", node->token.line,
               node->token.column);
}
