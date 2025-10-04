#include "sentinel.hpp"

Sentinel::Sentinel(Semantics &semantics) : semantics(semantics)
{
    registerSentinelFns();
}

void Sentinel::sentinelDriver(Node *node)
{
    auto sentinelIt = sentinelFnsMap.find(typeid(*node));
    if (sentinelIt == sentinelFnsMap.end())
    {
        std::cout << "Sentinel skipping node: " << node->toString() << "\n";
        return;
    }

    (this->*sentinelIt->second)(node);
}

void Sentinel::registerSentinelFns()
{
    sentinelFnsMap[typeid(LetStatement)] = &Sentinel::checkLetStatement;
    sentinelFnsMap[typeid(AssignmentStatement)] = &Sentinel::checkAssignmentStatement;
    sentinelFnsMap[typeid(Identifier)] = &Sentinel::checkIdentifier;
    sentinelFnsMap[typeid(BlockStatement)] = &Sentinel::checkBlockStatement;
    sentinelFnsMap[typeid(ifStatement)] = &Sentinel::checkIfStatement;
    sentinelFnsMap[typeid(elifStatement)] = &Sentinel::checkElifStatement;
    sentinelFnsMap[typeid(WhileStatement)] = &Sentinel::checkWhileStatement;
    sentinelFnsMap[typeid(ForStatement)] = &Sentinel::checkForStatement;
    sentinelFnsMap[typeid(FunctionStatement)] = &Sentinel::checkFunctionStatement;
    sentinelFnsMap[typeid(FunctionExpression)] = &Sentinel::checkFunctionExpression;
    sentinelFnsMap[typeid(BlockExpression)] = &Sentinel::checkBlockExpression;
    sentinelFnsMap[typeid(CallExpression)] = &Sentinel::checkCallExpression;
    sentinelFnsMap[typeid(ExpressionStatement)] = &Sentinel::checkExpressionStatement;
    sentinelFnsMap[typeid(InfixExpression)] = &Sentinel::checkInfixExpression;
}

void Sentinel::checkExpressionStatement(Node *node)
{
    auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
    if (!exprStmt)
        return;

    sentinelDriver(exprStmt->expression.get());
}

void Sentinel::checkInfixExpression(Node *node)
{
    auto infixExpr = dynamic_cast<InfixExpression *>(node);
    if (!infixExpr)
        return;

    auto left = infixExpr->left_operand.get();
    auto right = infixExpr->right_operand.get();

    sentinelDriver(left);
    sentinelDriver(right);
}

void Sentinel::checkIdentifier(Node *node)
{
    auto ident = dynamic_cast<Identifier *>(node);
    if (!ident)
        return;

    auto line = ident->expression.line;
    auto col = ident->identifier.column;
    auto name = ident->identifier.TokenLiteral;

    auto metaIt = semantics.metaData.find(ident);
    if (metaIt == semantics.metaData.end())
    {
        logError("Could not find identifier '" + name + "' metaData", line, col);
        return;
    }

    auto identSym = metaIt->second;
    if (!identSym)
    {
        logError("Unidentified identitfier '" + name + "' ", line, col);
        return;
    }

    if (!identSym->isHeap)
        return;

    if (identSym->lastUseNode == ident)
    {
        if (sentinelStack.back()->alloc_id != identSym->alloc_id)
        {
            logError("Non LIFO free detected, Tried to free '" + name + "' which is not on top of the SAGE stack", line, col);
            identSym->hasError = true;
            return;
        }
        sentinelStack.pop_back();
    }
}

void Sentinel::checkLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;

    auto line = letStmt->ident_token.line;
    auto col = letStmt->ident_token.column;
    const std::string &name = letStmt->ident_token.TokenLiteral;

    // Getting the let statement symbol
    auto metaIt = semantics.metaData.find(letStmt);
    if (metaIt == semantics.metaData.end())
    {
        logError("Could not find let statement metaData", line, col);
        return;
    }
    auto letSym = metaIt->second;
    if (!letSym)
    {
        logError("Invalid let statement '" + name + "'", line, col);
        return;
    }
    // Getting if the let statement is not heap raised and stopping there
    if (!letSym->isHeap)
        return;

    // Assigning the alloc_id
    letSym->alloc_id = nextAllocId++;

    // Pushing the symbol onto the sentinel stack
    sentinelStack.push_back(letSym);

    if (letSym->lastUseNode == letStmt)
    {
        if (sentinelStack.back()->alloc_id != letSym->alloc_id)
        {
            logError("Non LIFO free detected, Tried to free '" + name + "' which is not on top of the SAGE stack", line, col);
            letSym->hasError = true;
            return;
        }
        sentinelStack.pop_back();
    }
}

void Sentinel::checkAssignmentStatement(Node *node)
{
    auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    auto ident = assignStmt->identifier.get();
    auto value = assignStmt->value.get();

    sentinelDriver(ident);
    sentinelDriver(value);
}

void Sentinel::checkBlockStatement(Node *node)
{
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
        return;

    for (const auto &stmt : blockStmt->statements)
    {
        sentinelDriver(stmt.get());
    }
}

void Sentinel::checkIfStatement(Node *node)
{
    auto ifStmt = dynamic_cast<ifStatement *>(node);
    if (!ifStmt)
        return;

    sentinelDriver(ifStmt->if_result.get());

    if (!ifStmt->elifClauses.empty())
    {
        for (const auto &elif : ifStmt->elifClauses)
        {
            sentinelDriver(elif.get());
        }
    }

    if (ifStmt->else_result.has_value())
    {
        sentinelDriver(ifStmt->else_result.value().get());
    }
}

void Sentinel::checkElifStatement(Node *node)
{
    auto elifStmt = dynamic_cast<elifStatement *>(node);
    if (!elifStmt)
        return;

    sentinelDriver(elifStmt->elif_result.get());
}

void Sentinel::checkWhileStatement(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
        return;

    sentinelDriver(whileStmt->loop.get());
}

void Sentinel::checkForStatement(Node *node)
{
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        return;

    sentinelDriver(forStmt->body.get());
}

void Sentinel::checkFunctionStatement(Node *node)
{
    auto funcStmt = dynamic_cast<FunctionStatement *>(node);
    if (!funcStmt)
        return;

    sentinelDriver(funcStmt->funcExpr.get());
}

void Sentinel::checkFunctionExpression(Node *node)
{
    auto funcExpr = dynamic_cast<FunctionExpression *>(node);
    if (!funcExpr)
        return;

    sentinelDriver(funcExpr->block.get());
}

void Sentinel::checkBlockExpression(Node *node)
{
    auto blockExpr = dynamic_cast<BlockExpression *>(node);
    if (!blockExpr)
        return;

    if (!blockExpr->statements.empty())
    {
        for (const auto &stmt : blockExpr->statements)
        {
            sentinelDriver(stmt.get());
        }
    }

    if (blockExpr->finalexpr.has_value())
    {
        sentinelDriver(blockExpr->finalexpr.value().get());
    }
}

void Sentinel::checkCallExpression(Node *node)
{
    auto callExpr = dynamic_cast<CallExpression *>(node);
    if (!callExpr)
        return;

    for (const auto &arg : callExpr->parameters)
    {
        sentinelDriver(arg.get());
    }
}

void Sentinel::logError(const std::string &message, int line, int col)
{
    std::cerr << "[SENTINEL ERROR] " << message << " on line: " << line << ", column: " << col << "\n";
}