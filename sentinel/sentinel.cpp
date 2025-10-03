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
        logError("Could not find identifier metaData", line, col);
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

    auto line = assignStmt->identifier->expression.line;
    auto col = assignStmt->identifier->expression.column;
    const std::string &name = assignStmt->identifier->expression.TokenLiteral;

    auto metaIt = semantics.metaData.find(assignStmt);
    if (metaIt == semantics.metaData.end())
    {
        logError("Could not find assignment statement metaData", line, col);
        return;
    }

    // Getting the assignment symbol
    auto assignSym = metaIt->second;

    if (!assignSym)
    {
        logError("Undeclared variable '" + name + "' ", line, col);
        assignSym->hasError = true;
        return;
    }

    if (!assignSym->isHeap)
        return;

    if (assignSym->lastUseNode = assignStmt)
    {
        if (sentinelStack.back()->alloc_id != assignSym->alloc_id)
        {
            logError("Non LIFO free detected, Tried to free '" + name + "' which is not on top of the SAGE stack", line, col);
            assignSym->hasError = true;
            return;
        }
        sentinelStack.pop_back();
    }
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

void Sentinel::logError(const std::string &message, int line, int col)
{
    std::cerr << "[SENTINEL ERROR] " << message << " on line: " << line << ", column: " << col << "\n";
}