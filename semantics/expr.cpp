#include "semantics.hpp"

void Semantics::walkInfixExpression(Node *node)
{
    auto infixExpr = dynamic_cast<InfixExpression *>(node);
    if (!infixExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing infix expression " + infixExpr->toString() + "\n";

    auto left = infixExpr->left_operand.get();
    walker(left);
    auto right = infixExpr->right_operand.get();
    walker(right);

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
    walker(prefixExprOperand);

    metaData[prefixExpr] = {
        .symbolDataType = prefixType,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false,
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