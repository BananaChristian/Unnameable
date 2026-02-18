#include "ast.hpp"
#include "irgen.hpp"
#include <llvm-18/llvm/IR/Instructions.h>
//____________If statement__________
void IRGenerator::generateIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  auto originalPathState = pathLedger;

  // 1. Setup condition and Blocks
  llvm::Value *rawCond = generateExpression(ifStmt->condition.get());
  llvm::Value *condVal = coerceToBoolean(rawCond, ifStmt->condition.get());
  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();

  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(context, "then", function);
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifmerge");
  llvm::BasicBlock *nextBB = nullptr;

  if (!ifStmt->elifClauses.empty()) {
    nextBB = llvm::BasicBlock::Create(context, "elif0");
  } else if (ifStmt->else_result.has_value()) {
    nextBB = llvm::BasicBlock::Create(context, "else");
  } else {
    nextBB = llvm::BasicBlock::Create(context, "implicit.else", function);
  }

  funcBuilder.CreateCondBr(condVal, thenBB, nextBB);

  // 2. GENERATE 'THEN' BRANCH
  funcBuilder.SetInsertPoint(thenBB);
  generateStatement(ifStmt->if_result.get());

  // Clean up both the internal block and the parent IF while still in this
  // block
  emptyLeakedDeputiesBag(ifStmt->if_result.get());
  emptyLeakedDeputiesBag(ifStmt);

  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    funcBuilder.CreateBr(mergeBB);
  }

  // RESET LEDGER for next branch
  pathLedger = originalPathState;

  // 3. GENERATE 'ELIF' BRANCHES
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
      nextElifBB =
          (nextBB->getName() == "implicit.else")
              ? nextBB
              : llvm::BasicBlock::Create(context, "implicit.else", function);
    }

    funcBuilder.CreateCondBr(elifCondVal, elifBodyBB, nextElifBB);

    funcBuilder.SetInsertPoint(elifBodyBB);
    generateStatement(elif->elif_result.get());

    // Clean up elif block and parent IF while still in this block
    emptyLeakedDeputiesBag(elif->elif_result.get());
    emptyLeakedDeputiesBag(ifStmt);

    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      funcBuilder.CreateBr(mergeBB);
    }

    pathLedger = originalPathState;
    nextBB = nextElifBB;
  }

  // 4. GENERATE 'ELSE' OR 'IMPLICIT ELSE'
  if (ifStmt->else_result.has_value()) {
    function->insert(function->end(), nextBB);
    funcBuilder.SetInsertPoint(nextBB);

    generateStatement(ifStmt->else_result.value().get());

    // Clean up else block and parent IF
    emptyLeakedDeputiesBag(ifStmt->else_result.value().get());
    emptyLeakedDeputiesBag(ifStmt);

    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      funcBuilder.CreateBr(mergeBB);
    }
  } else {
    // Implicit Else Path
    if (nextBB->getParent() == nullptr) {
      function->insert(function->end(), nextBB);
    }
    funcBuilder.SetInsertPoint(nextBB);

    // Only clean up the IF (since there is no internal block)
    emptyLeakedDeputiesBag(ifStmt);

    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      funcBuilder.CreateBr(mergeBB);
    }
  }

  // 5. FINALIZE MERGE
  // Restore ledger state for code after the if-else block
  pathLedger = originalPathState;
  function->insert(function->end(), mergeBB);
  funcBuilder.SetInsertPoint(mergeBB);
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
