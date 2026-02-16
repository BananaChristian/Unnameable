#include "ast.hpp"
#include "semantics.hpp"
#include <algorithm>
#include <memory>

void Semantics::walkBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt)
    return;

  auto &stmts = blockStmt->statements;
  // keep track of all heap-raised vars declared in this block
  std::vector<std::string> localHeapVars;

  for (const auto &stmt : stmts) {
    std::string name;
    if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get())) {
      if (letStmt->isHeap) {
        // remember this var as heap declared locally
        localHeapVars.push_back(letStmt->ident_token.TokenLiteral);
      }
    }

    if (auto assignStmt = dynamic_cast<AssignmentStatement *>(stmt.get())) {
      logInternal("Triggered self heap check");
      name = extractIdentifierName(assignStmt->identifier.get());
      // Incase it is a self expression just call the walker for now
      if (auto selfExpr =
              dynamic_cast<SelfExpression *>(assignStmt->identifier.get())) {
      } else if (dynamic_cast<DereferenceExpression *>(
                     assignStmt->identifier.get())) {
      } else {

        auto assignSym = resolveSymbolInfo(name);
        if (!assignSym) {
          logSemanticErrors("Undeclared variable '" + name + "'",
                            assignStmt->identifier.get());
          return;
        }
        // only error if it's heap and not declared in this block
        if (assignSym->isHeap &&
            std::find(localHeapVars.begin(), localHeapVars.end(), name) ==
                localHeapVars.end()) {
          logSemanticErrors(
              "Cannot use a variable '" + name +
                  "' that you heap raised externally inside a loop or branch",
              assignStmt->identifier.get());
          return;
        }
      }
    }

    if (auto exprStmt = dynamic_cast<ExpressionStatement *>(stmt.get())) {
      if (auto infix =
              dynamic_cast<InfixExpression *>(exprStmt->expression.get())) {
        auto checkIdent = [&](Identifier *ident) {
          std::string n = ident->identifier.TokenLiteral;
          auto sym = resolveSymbolInfo(n);
          if (!sym) {
            logSemanticErrors("Use of undeclared variable '" + n + "'", ident);
            return false;
          }
          if (sym->isHeap &&
              std::find(localHeapVars.begin(), localHeapVars.end(), n) ==
                  localHeapVars.end()) {
            logSemanticErrors(
                "Cannot use a variable '" + n +
                    "' that you heap raised externally inside a loop",
                ident);
            return false;
          }
          return true;
        };

        if (auto leftIdent =
                dynamic_cast<Identifier *>(infix->left_operand.get()))
          if (!checkIdent(leftIdent))
            return;

        if (auto rightIdent =
                dynamic_cast<Identifier *>(infix->right_operand.get()))
          if (!checkIdent(rightIdent))
            return;
      }
    }

    walker(stmt.get());
  }
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

void Semantics::walkElifStatement(Node *node, LifeTimeTable &baselineTable) {
  auto elifStmt = dynamic_cast<elifStatement *>(node);
  if (!elifStmt)
    return;
  auto elifCondition = elifStmt->elif_condition.get();
  ResolvedType elifConditionType = inferNodeDataType(elifCondition);
  walker(elifCondition);
  // Handling the elif results
  auto elifResults = elifStmt->elif_result.get();
  symbolTable.push_back({});
  phonyTable =
      std::make_unique<LifeTimeTable>(std::move(cloneTable(baselineTable)));
  walker(elifResults);
  takeBranchSnapShot(elifResults, *phonyTable);
  phonyTable.reset();
  popScope();
}

void Semantics::walkIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  auto ifStmtCondition = ifStmt->condition.get();
  ResolvedType ifStmtType = inferNodeDataType(ifStmtCondition);

  walker(ifStmtCondition);
  auto baselineState = cloneTable(
      responsibilityTable); // This gives us a copy of the responsibility table
                            // before we branch anywhere
  // Dealing with the if result
  auto ifResult = ifStmt->if_result.get();
  symbolTable.push_back({});
  phonyTable =
      std::make_unique<LifeTimeTable>(std::move(cloneTable(baselineState)));
  walker(ifResult);
  takeBranchSnapShot(ifResult, *phonyTable);
  phonyTable.reset();
  popScope();

  // Dealing with the elif clauses
  auto &elifClauses = ifStmt->elifClauses;
  if (!elifClauses.empty()) {
    for (const auto &clause : elifClauses) {
      walkElifStatement(clause.get(), baselineState);
    }
  }

  // Dealing with else statement result
  if (ifStmt->else_result.has_value()) {
    auto elseStmt = ifStmt->else_result.value().get();
    symbolTable.push_back({});
    phonyTable =
        std::make_unique<LifeTimeTable>(std::move(cloneTable(baselineState)));
    walker(elseStmt);
    takeBranchSnapShot(elseStmt, *phonyTable);
    phonyTable.reset();
    popScope();
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
  for (const auto &stmt : caseStmt->body) {
    walker(stmt.get());
  }
  popScope();
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
    symbolTable.push_back({}); // Push the scope for the default statements
    for (const auto &stmt : switchStmt->default_statements) {
      walker(stmt.get());
    }
    popScope();
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

  if (loopContext.empty()) {
    logSemanticErrors(" 'break' used outside a loop ", breakStmt);
  }
}

void Semantics::walkContinueStatement(Node *node) {
  auto continueStmt = dynamic_cast<ContinueStatement *>(node);
  if (!continueStmt)
    return;

  if (loopContext.empty() || !loopContext.back()) {
    logSemanticErrors("'continue' used outside a loop", continueStmt);
  }
}
