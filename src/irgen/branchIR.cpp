#include "ast.hpp"
#include "irgen.hpp"
#include <llvm-18/llvm/IR/Instructions.h>
//____________If statement__________
void IRGenerator::generateIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  // Setup condition and Blocks
  llvm::Value *rawCond = generateExpression(ifStmt->condition.get());
  llvm::Value *condVal = coerceToBoolean(rawCond, ifStmt->condition.get());
  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();

  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(context, "then", function);
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifmerge");
  llvm::BasicBlock *nextBB = nullptr;

  // Track which blocks need cleanup in merge
  std::vector<Node *> blocksNeedingCleanup;

  // Determine the next block after then
  if (!ifStmt->elifClauses.empty()) {
    nextBB = llvm::BasicBlock::Create(context, "elif0");
  } else if (ifStmt->else_result.has_value()) {
    nextBB = llvm::BasicBlock::Create(context, "else");
  } else {
    nextBB = mergeBB;
  }

  funcBuilder.CreateCondBr(condVal, thenBB, nextBB);

  // generate'then' branch
  funcBuilder.SetInsertPoint(thenBB);
  generateStatement(ifStmt->if_result.get());
  blocksNeedingCleanup.push_back(ifStmt->if_result.get());

  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    funcBuilder.CreateBr(mergeBB);
  }

  // generate 'elif' branches
  for (size_t i = 0; i < ifStmt->elifClauses.size(); ++i) {
    function->insert(function->end(), nextBB);
    funcBuilder.SetInsertPoint(nextBB);

    const auto &elifStmt = ifStmt->elifClauses[i];
    auto elif = dynamic_cast<elifStatement *>(elifStmt.get());

    llvm::Value *elifRawCond = generateExpression(elif->elif_condition.get());
    llvm::Value *elifCondVal =
        coerceToBoolean(elifRawCond, elif->elif_condition.get());
    llvm::BasicBlock *elifBodyBB = llvm::BasicBlock::Create(
        context, "elif.body" + std::to_string(i), function);

    llvm::BasicBlock *nextElifBB = nullptr;
    if (i + 1 < ifStmt->elifClauses.size()) {
      nextElifBB =
          llvm::BasicBlock::Create(context, "elif" + std::to_string(i + 1));
    } else if (ifStmt->else_result.has_value()) {
      nextElifBB = llvm::BasicBlock::Create(context, "else");
    } else {
      nextElifBB = mergeBB;
    }

    funcBuilder.CreateCondBr(elifCondVal, elifBodyBB, nextElifBB);

    funcBuilder.SetInsertPoint(elifBodyBB);
    generateStatement(elif->elif_result.get());
    blocksNeedingCleanup.push_back(elif->elif_result.get());

    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      funcBuilder.CreateBr(mergeBB);
    }
    nextBB = nextElifBB;
  }

  // generate 'else' statement
  if (ifStmt->else_result.has_value()) {
    function->insert(function->end(), nextBB);
    funcBuilder.SetInsertPoint(nextBB);

    generateStatement(ifStmt->else_result.value().get());
    blocksNeedingCleanup.push_back(ifStmt->else_result.value().get());

    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      funcBuilder.CreateBr(mergeBB);
    }
  }

  // finalise merge - NOW do all cleanup
  function->insert(function->end(), mergeBB);
  funcBuilder.SetInsertPoint(mergeBB);

  // Emit all cleanups in the merge block (after all branches)
  for (auto *block : blocksNeedingCleanup) {
    emitBlockCleanUp(block);
  }
}

//___________Switch statement________
void IRGenerator::generateSwitchStatement(Node *node) {
  auto switchStmt = dynamic_cast<SwitchStatement *>(node);
  if (!switchStmt)
    return;

  llvm::Function *parentFunc = funcBuilder.GetInsertBlock()->getParent();

  // 1. Create the destination for 'break' and the default landing pad
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "sw.merge");
  llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(context, "sw.default");

  jumpStack.push_back({mergeBB, nullptr});

  llvm::Value *val = generateExpression(switchStmt->switch_init.get());

  llvm::SwitchInst *sw =
      funcBuilder.CreateSwitch(val, defaultBB, switchStmt->case_clauses.size());

  std::vector<llvm::ConstantInt *> pendingLabels;

  // Build the cases
  for (const auto &clause : switchStmt->case_clauses) {
    auto caseClause = dynamic_cast<CaseClause *>(clause.get());

    // Evaluate the case label (e.g., the '1' in case 1:)
    auto *cond = llvm::cast<llvm::ConstantInt>(
        generateExpression(caseClause->condition.get()));
    pendingLabels.push_back(cond);

    // Only create a block if the case actually has a body
    if (!caseClause->body.empty()) {
      llvm::BasicBlock *caseBodyBB =
          llvm::BasicBlock::Create(context, "sw.case", parentFunc);

      for (auto *label : pendingLabels) {
        sw->addCase(label, caseBodyBB);
      }
      pendingLabels.clear();

      funcBuilder.SetInsertPoint(caseBodyBB);
      for (const auto &stmt : caseClause->body) {
        generateStatement(stmt.get());
      }

      // If the user didn't write an explicit 'break', we auto-jump to merge
      if (!funcBuilder.GetInsertBlock()->getTerminator()) {
        funcBuilder.CreateBr(mergeBB);
      }
    }
  }

  // Handle any labels that were defined but had no body (fall-through to merge)
  for (auto *label : pendingLabels) {
    sw->addCase(label, mergeBB);
  }

  // Generate the Default Block
  parentFunc->insert(parentFunc->end(), defaultBB);
  funcBuilder.SetInsertPoint(defaultBB);
  for (auto &stmt : switchStmt->default_statements) {
    generateStatement(stmt.get());
  }

  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    funcBuilder.CreateBr(mergeBB);
  }

  jumpStack.pop_back();

  parentFunc->insert(parentFunc->end(), mergeBB);
  funcBuilder.SetInsertPoint(mergeBB);
}
