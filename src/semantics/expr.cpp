#include "semantics.hpp"

void Semantics::walkInfixExpression(Node *node)
{
    auto infixExpr = dynamic_cast<InfixExpression *>(node);
    if (!infixExpr)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing infix expression: " << infixExpr->toString() << "\n";
    bool hasError = false;

    // Resolve left-hand side
    auto left = infixExpr->left_operand.get();
    walker(left);

    // Special-case: dot (.) operator
    if (infixExpr->operat.type == TokenType::FULLSTOP)
    {
        // Get the lhs name
        auto lhsIdent = dynamic_cast<Identifier *>(infixExpr->left_operand.get());

        auto lhsName = lhsIdent->identifier.TokenLiteral;
        auto line = lhsIdent->identifier.line;
        auto col = lhsIdent->identifier.column;

        // Retrieve the symbol info of the left side
        auto leftSym = resolveSymbolInfo(lhsName);
        if (!leftSym)
        {
            logSemanticErrors("Unidentified variable '" + lhsName + "' in infix left side", line, col);
            hasError = true;
        }

        auto lhsType = leftSym->type;

        if (leftSym->isHeap)
        {
            leftSym->lastUseNode = infixExpr;
            if (leftSym->refCount > 0)
            {
                leftSym->refCount--;
            }

            std::cout << "[SEMANTIC LOG] Heap variable '" << lhsName
                      << "' accessed, refCount=" << leftSym->refCount
                      << ", lastUseNode set\n";
        }

        auto rhsIdent = dynamic_cast<Identifier *>(infixExpr->right_operand.get());
        if (!rhsIdent)
        {
            logSemanticErrors("Right-hand side of '.' must be an identifier",
                              infixExpr->right_operand->expression.line,
                              infixExpr->right_operand->expression.column);
            return;
        }

        // Resolve member in component or behavior
        auto resolved = resultOfScopeOrDot(TokenType::FULLSTOP,
                                           lhsType.resolvedName,
                                           rhsIdent->identifier.TokenLiteral,
                                           infixExpr);

        // Store metadata for RHS identifier
        auto rhsInfo = std::make_shared<SymbolInfo>();
        rhsInfo->type = resolved;
        rhsInfo->isNullable = false;
        rhsInfo->isConstant = false;
        rhsInfo->isInitialized = true;
        metaData[rhsIdent] = rhsInfo;

        // Store metadata for full infix expression
        auto infixInfo = std::make_shared<SymbolInfo>();
        infixInfo->type = resolved;
        infixInfo->isNullable = false;
        infixInfo->isConstant = false;
        infixInfo->isInitialized = true;
        metaData[infixExpr] = infixInfo;

        return; // done handling dot
    }

    // Special-case: scope (::) operator
    if (infixExpr->operat.type == TokenType::SCOPE_OPERATOR)
    {
        auto lhsType = metaData[left]->type;

        auto rhsIdent = dynamic_cast<Identifier *>(infixExpr->right_operand.get());
        if (!rhsIdent)
        {
            logSemanticErrors("Right-hand side of '::' must be an identifier",
                              infixExpr->right_operand->expression.line,
                              infixExpr->right_operand->expression.column);
            return;
        }

        // Resolve member in type / datablock / enum
        auto resolved = resultOfScopeOrDot(TokenType::SCOPE_OPERATOR,
                                           lhsType.resolvedName,
                                           rhsIdent->identifier.TokenLiteral,
                                           infixExpr);

        auto rhsInfo = std::make_shared<SymbolInfo>();
        rhsInfo->type = resolved;
        rhsInfo->isNullable = false;
        rhsInfo->isConstant = false;
        rhsInfo->isInitialized = true;
        metaData[rhsIdent] = rhsInfo;

        auto infixInfo = std::make_shared<SymbolInfo>();
        infixInfo->type = resolved;
        infixInfo->isNullable = false;
        infixInfo->isConstant = false;
        infixInfo->isInitialized = true;
        metaData[infixExpr] = infixInfo;

        return; // done handling scope operator
    }

    // Generic infix handling (+, -, *, /, etc.)
    auto right = infixExpr->right_operand.get();
    walker(right);

    // Infer type for normal infix
    ResolvedType infixType = inferNodeDataType(infixExpr);

    auto info = std::make_shared<SymbolInfo>();
    info->type = infixType;
    info->isNullable = false;
    info->isConstant = false;
    info->isInitialized = false;

    metaData[infixExpr] = info;
}

void Semantics::walkPrefixExpression(Node *node)
{
    auto prefixExpr = dynamic_cast<PrefixExpression *>(node);
    if (!prefixExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing prefix expression " + prefixExpr->toString() + "\n";
    auto prefixExprOperand = prefixExpr->operand.get();
    ResolvedType prefixType = inferNodeDataType(prefixExpr);
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
    auto info = std::make_shared<SymbolInfo>();
    info->type = prefixType;
    info->isNullable = false;
    info->isConstant = false;
    info->isInitialized = false;

    metaData[prefixExpr] = info;
}

void Semantics::walkPostfixExpression(Node *node)
{
    auto postfixExpr = dynamic_cast<PostfixExpression *>(node);
    if (!postfixExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing postfix expression " + postfixExpr->toString() + "\n";
    ResolvedType postfixType = inferNodeDataType(postfixExpr);
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
    auto info = std::make_shared<SymbolInfo>();
    info->type = postfixType;
    info->isNullable = false;
    info->isMutable = false;
    info->isInitialized = false;

    metaData[postfixExpr] = info;
}

void Semantics::walkExpressionStatement(Node *node)
{
    auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
    if (!exprStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing the expression statement " << exprStmt->toString() << "\n";
    walker(exprStmt->expression.get());
}

void Semantics::walkErrorStatement(Node *node)
{
    auto errStmt = dynamic_cast<ErrorStatement *>(node);
    if (!errStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing the error statement" << errStmt->toString() << "\n";

    bool hasError = false;

    auto errExpr = errStmt->errorExpr.get();

    if (!dynamic_cast<StringLiteral *>(errExpr) && !dynamic_cast<IntegerLiteral *>(errExpr) && !dynamic_cast<FloatLiteral *>(errExpr) && !dynamic_cast<BooleanLiteral *>(errExpr))
    {
        logSemanticErrors("Only literal values are allowed in error! expressions",
                          errExpr->expression.line,
                          errExpr->expression.column);
        hasError = true;
    }
    walker(errExpr);

    auto it = metaData.find(errExpr);
    if (it == metaData.end())
    {
        std::cout << "Could not find error expression metaData\n";
        return;
    }

    auto errInfo = it->second;
    errInfo->hasError = hasError;
    metaData[errStmt] = errInfo; // Giving the error expression info to the overall statement
}

void Semantics::walkBasicType(Node *node)
{
    auto basicType = dynamic_cast<BasicType *>(node);
    if (!basicType)
        return;
    std::cout << "[SEMANTIC LOG] Analysing the basic type expression" << basicType->toString() << "\n";
}

void Semantics::walkArrayType(Node *node)
{
    auto arrayType = dynamic_cast<ArrayType *>(node);
    if (!arrayType)
        return;
    std::cout << "[SEMANTIC LOG] Analysing the array type expression" << arrayType->toString() << "\n";
}