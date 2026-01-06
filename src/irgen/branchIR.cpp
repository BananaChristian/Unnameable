#include "ast.hpp"
#include "irgen.hpp"
#include <llvm-18/llvm/IR/Instructions.h>
//____________If statement__________
void IRGenerator::generateIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt) {
    throw std::runtime_error("Invalid if statement");
  }

  std::cerr << "[IR DEBUG] Generating if statement\n";

  // Generation of condition for the if
  llvm::Value *condVal = generateExpression(ifStmt->condition.get());
  if (!condVal) {
    throw std::runtime_error("Invalid if condition");
  }

  if (!condVal->getType()->isIntegerTy(1)) {
    condVal = funcBuilder.CreateICmpNE(
        condVal, llvm::ConstantInt::get(condVal->getType(), 0), "ifcond.bool");
  }

  // Create basic blocks
  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(context, "then", function);
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifmerge");

  // Determine the next block (first elif, else, or merge)
  llvm::BasicBlock *nextBB = nullptr;
  if (!ifStmt->elifClauses.empty()) {
    nextBB = llvm::BasicBlock::Create(context, "elif0");
  } else if (ifStmt->else_result.has_value()) {
    nextBB = llvm::BasicBlock::Create(context, "else");
  } else {
    nextBB = mergeBB;
  }

  // Conditional branch for if
  std::cerr << "[IR DEBUG] Creating conditional branch for if\n";
  funcBuilder.CreateCondBr(condVal, thenBB, nextBB);

  // Generate then branch
  funcBuilder.SetInsertPoint(thenBB);
  std::cerr << "[IR DEBUG] Generating then branch\n";
  generateStatement(ifStmt->if_result.get());
  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    std::cerr << "[IR DEBUG] Adding branch to ifmerge from then\n";
    funcBuilder.CreateBr(mergeBB);
  } else {
    std::cerr
        << "[IR DEBUG] Skipping branch to ifmerge from then due to terminator: "
        << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName()
        << "\n";
  }

  // Generating elif branches
  for (size_t i = 0; i < ifStmt->elifClauses.size(); ++i) {
    function->insert(function->end(), nextBB);
    funcBuilder.SetInsertPoint(nextBB);
    std::cerr << "[IR DEBUG] Generating elif branch " << i << "\n";

    const auto &elifStmt = ifStmt->elifClauses[i];
    auto elif = dynamic_cast<elifStatement *>(elifStmt.get());
    llvm::Value *elifCondVal = generateExpression(elif->elif_condition.get());
    if (!elifCondVal) {
      throw std::runtime_error("Invalid elif condition");
    }
    if (!elifCondVal->getType()->isIntegerTy(1)) {
      elifCondVal = funcBuilder.CreateICmpNE(
          elifCondVal, llvm::ConstantInt::get(elifCondVal->getType(), 0),
          "elifcond.bool");
    }

    llvm::BasicBlock *elifBodyBB = llvm::BasicBlock::Create(
        context, "elif.body" + std::to_string(i), function);
    llvm::BasicBlock *nextElifBB =
        (i + 1 < ifStmt->elifClauses.size())
            ? llvm::BasicBlock::Create(context, "elif" + std::to_string(i + 1))
            : (ifStmt->else_result.has_value()
                   ? llvm::BasicBlock::Create(context, "else")
                   : mergeBB);

    std::cerr << "[IR DEBUG] Creating conditional branch for elif " << i
              << "\n";
    funcBuilder.CreateCondBr(elifCondVal, elifBodyBB, nextElifBB);

    funcBuilder.SetInsertPoint(elifBodyBB);
    std::cerr << "[IR DEBUG] Generating elif body " << i << "\n";
    generateStatement(elif->elif_result.get());
    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      std::cerr << "[IR DEBUG] Adding branch to ifmerge from elif " << i
                << "\n";
      funcBuilder.CreateBr(mergeBB);
    } else {
      std::cerr
          << "[IR DEBUG] Skipping branch " << i
          << " to ifmerge from elif due to terminator "
          << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName()
          << "\n";
    }

    nextBB = nextElifBB;
  }

  // Generate else branch if present
  if (ifStmt->else_result.has_value()) {
    function->insert(function->end(), nextBB);
    funcBuilder.SetInsertPoint(nextBB);
    std::cerr << "[IR DEBUG] Generating else branch\n";
    generateStatement(ifStmt->else_result.value().get());
    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      std::cerr << "[IR DEBUG] Adding branch to ifmerge from else\n";
      funcBuilder.CreateBr(mergeBB);
    } else {
      std::cerr
          << "[IR DEBUG] Skipping branch to ifmerge from else due to "
             "terminator: "
          << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName()
          << "\n";
    }
  }

  // Finalize with merge block
  function->insert(function->end(), mergeBB);
  funcBuilder.SetInsertPoint(mergeBB);
  std::cerr << "[IR DEBUG] Finished generating if statement\n";
}

//___________Switch statement________
void IRGenerator::generateSwitchStatement(Node *node) {
  auto switchStmt = dynamic_cast<SwitchStatement *>(node);
  if (!switchStmt)
    return;

  bool hasError = semantics.metaData[switchStmt]->hasError;
  if (hasError)
    throw std::runtime_error("Error deteceted");

  // Get the parent function
  llvm::Function *parentFunc = funcBuilder.GetInsertBlock()->getParent();

  // Create the end point for the default
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "sw.merge");
  llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(context, "sw.default");

  // Generate the init and get the value
  llvm::Value *val = generateExpression(switchStmt->switch_init.get());

  // Start the switch
  llvm::SwitchInst *sw =
      funcBuilder.CreateSwitch(val, defaultBB, switchStmt->case_clauses.size());

  // Build the cases
  for (const auto &clause : switchStmt->case_clauses) {
    llvm::BasicBlock *caseBB =
        llvm::BasicBlock::Create(context, "sw.case", parentFunc);

    auto caseClause = dynamic_cast<CaseClause *>(clause.get());

    auto *cond = llvm::cast<llvm::ConstantInt>(
        generateExpression(caseClause->condition.get()));
    sw->addCase(cond, caseBB);

    funcBuilder.SetInsertPoint(caseBB);
    for (const auto &stmt : caseClause->body) {
      generateStatement(stmt.get());
    }

    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      funcBuilder.CreateBr(mergeBB);
    }
  }

  parentFunc->insert(parentFunc->end(), defaultBB);
  funcBuilder.SetInsertPoint(defaultBB);
  for (auto &stmt : switchStmt->default_statements) {
    generateStatement(stmt.get());
  }
  
  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    funcBuilder.CreateBr(mergeBB);
  }

  // Finish at Merge
  parentFunc->insert(parentFunc->end(), mergeBB);
  funcBuilder.SetInsertPoint(mergeBB);
}
