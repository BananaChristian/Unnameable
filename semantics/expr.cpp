#include "semantics.hpp"

void Semantics::walkInfixExpression(Node *node)
{
    auto infixExpr = dynamic_cast<InfixExpression *>(node);
    if (!infixExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing infix expression " + infixExpr->toString() + "\n";

    auto left = infixExpr->left_operand.get();
    walker(left);

    if (infixExpr->operat.type != TokenType::FULLSTOP && infixExpr->operat.type != TokenType::SCOPE_OPERATOR)
    {
        std::cout << "OPERATOR TYPE: " << TokenTypeToLiteral(infixExpr->operat.type) << "\n";
        auto right = infixExpr->right_operand.get();
        walker(right);
    }

    DataType infixType = inferNodeDataType(infixExpr);
    metaData[infixExpr] = {
        .symbolDataType = infixType,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false,
        .isInitialized = false};
}

void Semantics::walkPrefixExpression(Node *node)
{
    auto prefixExpr = dynamic_cast<PrefixExpression *>(node);
    if (!prefixExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing prefix expression " + prefixExpr->toString() + "\n";
    auto prefixExprOperand = prefixExpr->operand.get();
    DataType prefixType = inferNodeDataType(prefixExpr);
    if (prefixExpr->operat.type == TokenType::PLUS_PLUS || prefixExpr->operat.type == TokenType::MINUS_MINUS)
    {
        if (auto ident = dynamic_cast<Identifier *>(prefixExprOperand))
        {
            auto symbol = resolveSymbolInfo(ident->expression.TokenLiteral);
            if (!symbol)
            {
                logSemanticErrors("Undefined variable in prefix expression '" + ident->expression.TokenLiteral + "'", prefixExpr->expression.line, prefixExpr->expression.column);
                return;
            }
            if (!symbol->isMutable)
            {
                logSemanticErrors("Cannot apply '" + prefixExpr->operat.TokenLiteral + "' to immutable variable '" + ident->expression.TokenLiteral + "'", prefixExpr->expression.line, prefixExpr->expression.column);
                return;
            }
            if (!symbol->isInitialized)
            {
                logSemanticErrors("Cannot apply '" + prefixExpr->operat.TokenLiteral + "' to uninitialized variable '" + ident->expression.TokenLiteral + "'", prefixExpr->expression.line, prefixExpr->expression.column);
                return;
            }
        }
        else
        {
            logSemanticErrors("Prefix operator '" + prefixExpr->operat.TokenLiteral + "' can only be applied to identifiers", prefixExpr->expression.line, prefixExpr->expression.column);
            return;
        }
    }
    walker(prefixExprOperand);

    metaData[prefixExpr] = {
        .symbolDataType = prefixType,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false,
        .isInitialized = false};
}

void Semantics::walkPostfixExpression(Node *node)
{
    auto postfixExpr = dynamic_cast<PostfixExpression *>(node);
    if (!postfixExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing postfix expression " + postfixExpr->toString() + "\n";
    DataType postfixType = inferNodeDataType(postfixExpr);
    auto postfixOperand = postfixExpr->operand.get();
    if (postfixExpr->operator_token.type == TokenType::PLUS_PLUS || postfixExpr->operator_token.type == TokenType::MINUS_MINUS)
    {
        if (auto ident = dynamic_cast<Identifier *>(postfixOperand))
        {
            auto symbol = resolveSymbolInfo(ident->expression.TokenLiteral);
            if (!symbol)
            {
                logSemanticErrors("Undefined variable in postfix expression '" + ident->expression.TokenLiteral + "'", postfixExpr->expression.line, postfixExpr->expression.column);
                return;
            }
            if (symbol->isMutable == false)
            {
                logSemanticErrors("Cannot apply '" + postfixExpr->operator_token.TokenLiteral + "' to immutable variable '" + ident->expression.TokenLiteral + "'", postfixExpr->expression.line, postfixExpr->expression.column);
                return;
            }
            if (!symbol->isInitialized)
            {
                logSemanticErrors("Cannot apply '" + postfixExpr->operator_token.TokenLiteral + "' to uninitialized variable '" + ident->expression.TokenLiteral + "'", postfixExpr->expression.line, postfixExpr->expression.column);
                return;
            }
        }
        else
        {
            logSemanticErrors("Postfix operator '" + postfixExpr->operator_token.TokenLiteral + "' can only be applied to identifiers", postfixExpr->expression.line, postfixExpr->expression.column);
            return;
        }
    }
    walker(postfixOperand);

    metaData[postfixExpr] = {
        .symbolDataType = postfixType,
        .isNullable = false,
        .isMutable = false,
        .isInitialized = false};
}

void Semantics::walkExpressionStatement(Node *node)
{
    auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
    if (!exprStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing the expression statement " << exprStmt->toString() << "\n";
    walker(exprStmt->expression.get());
}

void Semantics::walkErrorExpression(Node *node)
{
    auto errExpr = dynamic_cast<ErrorExpression *>(node);
    if (!errExpr)
        return;

    std::cout << "[SEMANTIC LOG] Analysing the error expression " << errExpr->toString() << "\n";

    auto errType = inferNodeDataType(errExpr);
    auto errMessage = errExpr->err_message.get();

    walker(errMessage);
    DataType msgType = inferNodeDataType(errMessage);

    if (msgType == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid error message type '" + dataTypetoString(msgType) + "'", errExpr->error_token.line, errExpr->error_token.column);
    }
    metaData[errExpr] = {
        .symbolDataType = errType,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false,
        .isInitialized = false};
}

void Semantics::walkErrorStatement(Node *node)
{
    auto errStmt = dynamic_cast<ErrorStatement *>(node);
    if (!errStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing the error statement" << errStmt->toString() << "\n";
    auto errExpr = errStmt->errorExpr.get();
    walker(errExpr);
}