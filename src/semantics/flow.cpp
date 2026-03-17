#include "ast.hpp"
#include "semantics.hpp"
#include <memory>

void Semantics::walkBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt)
    return;

  auto &stmts = blockStmt->statements;
  activeBlocks.push_back(blockStmt);

  bool gateClosed = false;
  std::string terminatorString;

  for (const auto &stmt : stmts) {
    // Check for unreachable code
    if (gateClosed) {
      errorHandler.addHint(
          "A terminator (like return, break, or continue) "
          "is like a one-way exit. Once the computer takes that exit, it "
          "cannot turn around to see the code that follows. This code is "
          "considered 'dead' because no path of execution can ever reach it.");
      logSemanticErrors("Unreachable code detected after '" + terminatorString +
                            "'.",
                        stmt.get());
      break;
    }

    walker(stmt.get());

    if (isTerminator(stmt.get())) {
      gateClosed = true;
      terminatorString = getTerminatorString(stmt.get());
    }
  }
  activeBlocks.pop_back();
}

void Semantics::walkWhileStatement(Node *node) {
  auto whileStmt = dynamic_cast<WhileStatement *>(node);
  if (!whileStmt)
    return;

  auto whileCondition = whileStmt->condition.get();
  ResolvedType whileCondType = inferNodeDataType(whileCondition);
  if (whileCondType.kind != DataType::BOOLEAN) {
    logSemanticErrors("Expected boolean type but got '" +
                          whileCondType.resolvedName + "'",
                      whileCondition);
  }
  walker(whileCondition);

  loopContext.push_back(true);
  symbolTable.push_back({});
  auto whileLoop = whileStmt->loop.get();
  walker(whileLoop);
  popScope();
  loopContext.pop_back();
}

void Semantics::walkElifStatement(Node *node) {
  auto elifStmt = dynamic_cast<elifStatement *>(node);
  if (!elifStmt)
    return;
  auto elifCondition = elifStmt->elif_condition.get();
  ResolvedType elifConditionType = inferNodeDataType(elifCondition);
  walker(elifCondition);
  // Handling the elif results
  auto elifResults = elifStmt->elif_result.get();
  walker(elifResults);
}

void Semantics::walkIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  auto ifStmtCondition = ifStmt->condition.get();
  ResolvedType ifStmtType = inferNodeDataType(ifStmtCondition);

  walker(ifStmtCondition);
  // Dealing with the if result
  walker(ifStmt->if_result.get());

  // Dealing with the elif clauses
  auto &elifClauses = ifStmt->elifClauses;
  if (!elifClauses.empty()) {
    for (const auto &clause : elifClauses) {
      walkElifStatement(clause.get());
    }
  }

  // Dealing with else statement result
  if (ifStmt->else_result.has_value()) {
    walker(ifStmt->else_result.value().get());
  }
}

void Semantics::walkCaseStatement(Node *node, const ResolvedType &targetType) {
  auto caseStmt = dynamic_cast<CaseClause *>(node);
  if (!caseStmt)
    return;

  bool hasError = false;

  auto caseCondition = caseStmt->condition.get();

  if (!isLiteral(caseCondition)) {
    logSemanticErrors("Case condition must be a constant literal",
                      caseCondition);
    hasError = true;
    return;
  }

  walker(caseCondition);

  // Checking if the case type matches the type of the entire switch
  if (!metaData[caseCondition]) {
    reportDevBug("Could not find case metaData", caseCondition);
    return;
  }

  auto caseType = metaData[caseCondition]->type;
  bool isValid = isInteger(targetType) || isChar(targetType) ||
                 (targetType.kind == DataType::ENUM);
  if (!isValid) {
    logSemanticErrors(
        "Invalid case target expected only integers, chars, and enumarations",
        caseCondition);
    hasError = true;
  }
  if (caseType.kind != targetType.kind) {
    logSemanticErrors("Type mismatch: case literal type '" +
                          caseType.resolvedName +
                          "' doesnt match switch expression type '" +
                          targetType.resolvedName + "'",
                      caseCondition);
    hasError = true;
  }

  symbolTable.push_back({}); // Pushing a new clause scope
  caseContext.push_back(true);
  for (const auto &stmt : caseStmt->body) {
    walker(stmt.get());
  }
  popScope();
  caseContext.pop_back();
  auto caseInfo = std::make_shared<SymbolInfo>();
  caseInfo->hasError = hasError;

  metaData[caseStmt] = caseInfo;
}

void Semantics::walkSwitchStatement(Node *node) {
  auto switchStmt = dynamic_cast<SwitchStatement *>(node);
  if (!switchStmt)
    return;

  bool hasError = false;

  symbolTable.push_back({}); // Push a new scope for the switch

  // The switch's init
  walker(switchStmt->switch_init.get());
  ResolvedType targetType = metaData[switchStmt->switch_init.get()]->type;
  bool isValid = isInteger(targetType) || isChar(targetType) ||
                 (targetType.kind == DataType::ENUM);
  if (!isValid) {
    logSemanticErrors(
        "Invalid switch target expected only integers, chars, and enumarations",
        switchStmt);
    hasError = true;
  }

  for (const auto &caseClause : switchStmt->case_clauses) {
    walkCaseStatement(caseClause.get(), targetType);
    hasError = metaData[caseClause.get()]->hasError;
  }

  // For the default case
  if (!switchStmt->default_statements.empty()) {
    caseContext.push_back(true);
    symbolTable.push_back({}); // Push the scope for the default statements
    for (const auto &stmt : switchStmt->default_statements) {
      walker(stmt.get());
    }
    popScope();
    caseContext.pop_back();
  } else {
    logSemanticErrors("Switch statement is missing default case", switchStmt);
    hasError = true;
  }

  popScope();

  auto switchInfo = std::make_shared<SymbolInfo>();
  switchInfo->hasError = hasError;
  metaData[switchStmt] = switchInfo;
}

void Semantics::walkForStatement(Node *node) {
  auto forStmt = dynamic_cast<ForStatement *>(node);
  if (!forStmt)
    return;

  symbolTable.push_back({});
  // Handling the initializer
  auto initializer = forStmt->initializer.get();
  walker(initializer);
  // Handling the condition
  auto condition = forStmt->condition.get();
  walker(condition);
  // Handling the steps
  auto step = forStmt->step.get();
  walker(step);
  // Handling the block
  loopContext.push_back(true);

  auto block = forStmt->body.get();
  walker(block);
  popScope();
  loopContext.pop_back();
}

void Semantics::walkBreakStatement(Node *node) {
  auto breakStmt = dynamic_cast<BreakStatement *>(node);
  if (!breakStmt)
    return;

  if (loopContext.empty() && caseContext.empty()) {
    logSemanticErrors(" 'break' used outside a loop or switch", breakStmt);
  }
}

void Semantics::walkContinueStatement(Node *node) {
  auto continueStmt = dynamic_cast<ContinueStatement *>(node);
  if (!continueStmt)
    return;

  if (loopContext.empty()) {
    logSemanticErrors("'continue' used outside a loop", continueStmt);
  }
}
