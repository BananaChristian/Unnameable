#include "semantics.hpp"

void Semantics::walkBlockStatement(Node *node)
{
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
        return;

    std::cout << "[SEMANTIC LOG]: Analysing block statement: " << blockStmt->toString() << "\n";

    auto &stmts = blockStmt->statements;
    // keep track of all heap-raised vars declared in this block
    std::vector<std::string> localHeapVars;

    for (const auto &stmt : stmts)
    {
        std::string name;
        if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get()))
        {
            if (letStmt->isHeap)
            {
                // remember this var as heap declared locally
                localHeapVars.push_back(letStmt->ident_token.TokenLiteral);
            }
        }

        if (auto assignStmt = dynamic_cast<AssignmentStatement *>(stmt.get()))
        {
            std::cout << "Triggered self heap check\n";
            name = assignStmt->identifier->expression.TokenLiteral;
            int line = assignStmt->identifier->expression.line;
            int col = assignStmt->identifier->expression.column;
            // Incase it is a self expression just call the walker for now
            if (auto selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
            {
            }
            else
            {

                auto assignSym = resolveSymbolInfo(name);
                if (!assignSym)
                {
                    logSemanticErrors("Undeclared variable '" + name + "'", line, col);
                    return;
                }
                // only error if it's heap and not declared in this block
                if (assignSym->isHeap &&
                    std::find(localHeapVars.begin(), localHeapVars.end(), name) == localHeapVars.end())
                {
                    logSemanticErrors("Cannot use a variable '" + name + "' that you heap raised externally inside a loop or branch", line, col);
                    return;
                }
            }
        }

        if (auto exprStmt = dynamic_cast<ExpressionStatement *>(stmt.get()))
        {
            if (auto infix = dynamic_cast<InfixExpression *>(exprStmt->expression.get()))
            {
                std::cout << "Triggered Infix\n";
                auto checkIdent = [&](Identifier *ident)
                {
                    std::string n = ident->identifier.TokenLiteral;
                    int line = ident->expression.line;
                    int col = ident->expression.column;
                    auto sym = resolveSymbolInfo(n);
                    if (!sym)
                    {
                        logSemanticErrors("Use of undeclared variable '" + n + "'", line, col);
                        return false;
                    }
                    if (sym->isHeap &&
                        std::find(localHeapVars.begin(), localHeapVars.end(), n) == localHeapVars.end())
                    {
                        logSemanticErrors("Cannot use a variable '" + n + "' that you heap raised externally inside a loop", line, col);
                        return false;
                    }
                    return true;
                };

                if (auto leftIdent = dynamic_cast<Identifier *>(infix->left_operand.get()))
                    if (!checkIdent(leftIdent))
                        return;

                if (auto rightIdent = dynamic_cast<Identifier *>(infix->right_operand.get()))
                    if (!checkIdent(rightIdent))
                        return;
            }
        }

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
    ResolvedType whileCondType = inferNodeDataType(whileCondition);
    if (whileCondType.kind != DataType::BOOLEAN)
    {
        std::cerr << "[SEMANTIC ERROR] Expected boolean type but got '" + whileCondType.resolvedName + "'\n";
    }
    walker(whileCondition);

    loopContext.push_back(true);
    symbolTable.push_back({});
    auto whileLoop = whileStmt->loop.get();
    walker(whileLoop);
    popScope();
    loopContext.pop_back();
}

void Semantics::walkElifStatement(Node *node)
{
    auto elifStmt = dynamic_cast<elifStatement *>(node);
    if (!elifStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing elif statement: " << elifStmt->toString() << "\n";
    auto elifCondition = elifStmt->elif_condition.get();
    ResolvedType elifConditionType = inferNodeDataType(elifCondition);
    walker(elifCondition);

    currentBranchIdents.clear(); // Empty it from all the pollution of earlier entries
    // Handling the elif results
    auto elifResults = elifStmt->elif_result.get();
    symbolTable.push_back({});
    walker(elifResults);
    for (const auto &ident : currentBranchIdents)
    {
        const std::string &identName = ident->identifier.TokenLiteral;
        auto identLine = ident->identifier.line;
        auto identCol = ident->identifier.column;
        auto identSym = resolveSymbolInfo(identName);
        if (!identSym)
        {
            logSemanticErrors("Could not find the identifier symbol '" + identName + "'", identLine, identCol);
            continue;
        }

        if (identSym->lastUseNode == ident)
        {
            // Trigger the identKiller for the specific identifier node
            ident->isKiller = true;
        }
    }
    popScope();
}

void Semantics::walkIfStatement(Node *node)
{
    auto ifStmt = dynamic_cast<ifStatement *>(node);
    if (!ifStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing if statement: " << ifStmt->toString() << "\n";
    auto ifStmtCondition = ifStmt->condition.get();
    ResolvedType ifStmtType = inferNodeDataType(ifStmtCondition);

    walker(ifStmtCondition);

    currentBranchIdents.clear(); // Empty it from all the pollution of earlier entries
    // Dealing with the if result
    auto ifResult = ifStmt->if_result.get();
    symbolTable.push_back({});
    walker(ifResult);
    for (const auto &ident : currentBranchIdents)
    {
        const std::string &identName = ident->identifier.TokenLiteral;
        auto identLine = ident->identifier.line;
        auto identCol = ident->identifier.column;
        auto identSym = resolveSymbolInfo(identName);
        if (!identSym)
        {
            logSemanticErrors("Could not find the identifier symbol '" + identName + "'", identLine, identCol);
            continue;
        }

        if (identSym->lastUseNode == ident)
        {
            // Trigger the identKiller for the specific identifier node
            ident->isKiller = true;
        }
    }
    popScope();

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
        currentBranchIdents.clear();
        symbolTable.push_back({});
        walker(elseStmt);
        for (const auto &ident : currentBranchIdents)
        {
            const std::string &identName = ident->identifier.TokenLiteral;
            auto identLine = ident->identifier.line;
            auto identCol = ident->identifier.column;
            auto identSym = resolveSymbolInfo(identName);
            if (!identSym)
            {
                logSemanticErrors("Could not find the identifier symbol '" + identName + "'", identLine, identCol);
                continue;
            }

            if ((identSym->lastUseNode == ident) && (identSym->refCount == 0))
            {
                // Trigger the identKiller for the specific identifier node
                ident->isKiller = true;
            }
        }
        popScope();
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
    auto symbolInfo = resolveSymbolInfo(switchExprName);
    bool isConstant = symbolInfo->isConstant;
    bool isMutable = symbolInfo->isMutable;
    bool isNullable = symbolInfo->isNullable;
    bool isInitialized = symbolInfo->isInitialized;
    walker(switchExpr);

    // Dealing with the case clauses
    ResolvedType caseType = ResolvedType{DataType::UNKNOWN, "unknown"};

    loopContext.push_back(false);
    auto &caseClause = switchStmt->case_clauses;
    for (auto &caseSt : caseClause)
    {
        caseType = inferNodeDataType(caseSt.get());
        if (caseType.kind != switchType.kind)
        {
            logSemanticErrors("Type mismatch in switch case ", node->token.line, node->token.column);
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

    auto info = std::make_shared<SymbolInfo>();
    info->type = switchType;
    info->isNullable = isNullable;
    info->isMutable = isMutable;
    info->isConstant = isConstant;
    info->isInitialized = isInitialized;

    metaData[switchStmt] = info;
}

void Semantics::walkForStatement(Node *node)
{
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing for statement " << forStmt->toString() << "\n";

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
        logSemanticErrors(" 'break' used outside a loop ", node->token.line, node->token.column);
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
