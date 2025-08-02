#include "semantics.hpp"

void Semantics::walkBlockStatement(Node *node)
{
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing block statement: " << blockStmt->toString() << "\n";
    auto &stmts = blockStmt->statements;
    for (const auto &stmt : stmts)
    {
        walker(stmt.get());
    }
}

void Semantics::walkWhileStatement(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing while statement: " << whileStmt->toString() << "\n";
    auto whileCondition = whileStmt->condition.get();
    DataType whileCondType = inferNodeDataType(whileCondition);
    if (whileCondType != DataType::BOOLEAN)
    {
        std::cerr << "[SEMANTIC ERROR] Expected boolean type but got: " + dataTypetoString(whileCondType) + "\n";
    }
    walker(whileCondition);

    loopContext.push_back(true);
    auto whileLoop = whileStmt->loop.get();
    walker(whileLoop);
    loopContext.pop_back();
}

void Semantics::walkElifStatement(Node *node)
{
    auto elifStmt = dynamic_cast<elifStatement *>(node);
    if (!elifStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing elif statement: " << elifStmt->toString() << "\n";
    auto elifCondition = elifStmt->elif_condition.get();
    DataType elifConditionType = inferNodeDataType(elifCondition);
    if (elifConditionType != DataType::BOOLEAN)
    {
        logSemanticErrors("Expected boolean type but got" + dataTypetoString(elifConditionType), elifStmt);
        return;
    }
    walker(elifCondition);

    // Handling the elif results
    auto elifResults = elifStmt->elif_result.get();
    walker(elifResults);
}

void Semantics::walkIfStatement(Node *node)
{
    auto ifStmt = dynamic_cast<ifStatement *>(node);
    if (!ifStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing if statement: " << ifStmt->toString() << "\n";
    auto ifStmtCondition = ifStmt->condition.get();
    DataType ifStmtType = inferNodeDataType(ifStmtCondition);
    if (ifStmtType != DataType::BOOLEAN)
    {
        logSemanticErrors("Expected boolean type but got: " + dataTypetoString(ifStmtType), node);
        return;
    }
    walker(ifStmtCondition);

    // Dealing with the if result
    auto ifResult = ifStmt->if_result.get();
    walker(ifResult);

    // Dealing with the elif clauses
    auto &elifClauses = ifStmt->elifClauses;
    if (!elifClauses.empty())
    {

        for (const auto &clause : elifClauses)
        {
            walkElifStatement(clause.get());
        }
    }

    // Dealing with else statement result
    if (ifStmt->else_result.has_value())
    {
        auto elseStmt = ifStmt->else_result.value().get();
        walker(elseStmt);
    }
}

void Semantics::walkCaseStatement(Node *node)
{
    auto caseStmt = dynamic_cast<CaseClause *>(node);
    if (!caseStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing case clause: " << caseStmt->toString() << "\n";
    // Dealing with the condition
    auto condition = caseStmt->condition.get();
    walker(condition);

    // Dealing with the body
    auto &body = caseStmt->body;
    for (auto &content : body)
    {
        walker(content.get());
    }
}

void Semantics::walkSwitchStatement(Node *node)
{
    auto switchStmt = dynamic_cast<SwitchStatement *>(node);
    if (!switchStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing switch statement: " << switchStmt->toString() << "\n";

    // Dealing with the switch expression
    auto switchExpr = switchStmt->switch_expr.get();
    auto switchType = inferNodeDataType(switchExpr);
    auto switchExprName = switchStmt->switch_expr->expression.TokenLiteral;
    SymbolInfo *symbolInfo = resolveSymbolInfo(switchExprName);
    bool isConstant = symbolInfo->isConstant;
    bool isMutable = symbolInfo->isMutable;
    bool isNullable = symbolInfo->isNullable;
    bool isInitialized = symbolInfo->isInitialized;
    walker(switchExpr);

    // Dealing with the case clauses
    auto caseType = DataType::UNKNOWN;

    loopContext.push_back(false);
    auto &caseClause = switchStmt->case_clauses;
    for (auto &caseSt : caseClause)
    {
        caseType = inferNodeDataType(caseSt.get());
        if (caseType != switchType)
        {
            logSemanticErrors("Type mismatch in switch case ", node);
        }
        walker(caseSt.get());
    }
    loopContext.pop_back();

    // Dealing with the default statement
    auto &defaults = switchStmt->default_statements;
    for (auto &content : defaults)
    {
        walker(content.get());
    }

    metaData[switchStmt] = {
        .symbolDataType = switchType,
        .isNullable = isNullable,
        .isMutable = isMutable,
        .isConstant = isConstant,
        .isInitialized = isInitialized};
}

void Semantics::walkForStatement(Node *node)
{
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing for statement " << forStmt->toString() << "\n";

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
    loopContext.pop_back();
}

void Semantics::walkEachStatement(Node *node)
{
    auto eachStmt = dynamic_cast<EachStatement *>(node);
    if (!eachStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing each statement " << eachStmt->toString() << "\n";
    // Handling iterator variable
    auto iterVar = eachStmt->iteratorVar.get();
    walker(iterVar);

    // Handling iterable
    auto iter = eachStmt->iterable.get();
    walker(iter);

    // Handling each stmt block
    loopContext.push_back(true);
    auto block = eachStmt->body.get();
    walker(block);
    loopContext.pop_back();
}

void Semantics::walkBreakStatement(Node *node)
{
    auto breakStmt = dynamic_cast<BreakStatement *>(node);
    if (!breakStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing break statement " << breakStmt->toString() << "\n";
    if (loopContext.empty())
    {
        logSemanticErrors(" 'break' used outside a loop ", node);
    }
}

void Semantics::walkContinueStatement(Node *node)
{
    auto continueStmt = dynamic_cast<ContinueStatement *>(node);
    if (!continueStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing continue statement " << continueStmt->toString() << "\n";
    if (loopContext.empty() || !loopContext.back())
    {
        std::cerr << "[SEMANTIC ERROR] 'continue' used outside a loop\n ";
    }
}