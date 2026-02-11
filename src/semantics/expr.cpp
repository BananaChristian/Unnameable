#include "ast.hpp"
#include "semantics.hpp"

void Semantics::walkInfixExpression(Node *node) {
  auto infixExpr = dynamic_cast<InfixExpression *>(node);
  if (!infixExpr)
    return;

  bool hasError = false;

  auto left = infixExpr->left_operand.get();
  walker(left);

  auto leftSym = metaData[left];

  // Retrieve the symbol info of the left side
  if (!leftSym) {
    logSemanticErrors("Unresolved expression in infix left side", left);
    hasError = true;
    return;
  }

  auto lhsType = leftSym->type;

  // Access operators
  if (infixExpr->operat.type == TokenType::FULLSTOP ||
      infixExpr->operat.type == TokenType::SCOPE_OPERATOR) {

    auto rhsIdent = dynamic_cast<Identifier *>(infixExpr->right_operand.get());
    if (!rhsIdent) {
      logSemanticErrors("Right-hand side of '.' must be an identifier",
                        infixExpr->right_operand.get());
      return;
    }
    logInternal("Lhs Infix Type: " + lhsType.resolvedName);

    // Resolve member
    auto memberInfo =
        resultOfScopeOrDot(infixExpr->operat.type, lhsType,
                           rhsIdent->identifier.TokenLiteral, infixExpr);

    if (!memberInfo)
      return;

    logInternal("Infix Type for member access :" +
                memberInfo->type.resolvedName);

    // Store metadata for RHS identifier
    metaData[rhsIdent] = memberInfo;
    metaData[infixExpr] = memberInfo;

    return;
  }

  // Infer type for normal infix
  walker(infixExpr->right_operand.get());
  ResolvedType infixType = inferNodeDataType(infixExpr);
  if (infixType.kind == DataType::UNKNOWN) {
    logSemanticErrors("Infix cannot be of unknown type",
                      infixExpr->left_operand.get());
    hasError = true;
  }

  auto info = std::make_shared<SymbolInfo>();
  info->type = infixType;
  info->isNullable = false;
  info->isConstant = false;
  info->hasError = hasError;
  info->isInitialized = false;

  metaData[infixExpr] = info;
}

void Semantics::walkPrefixExpression(Node *node) {
  auto prefixExpr = dynamic_cast<PrefixExpression *>(node);
  if (!prefixExpr)
    return;

  auto prefixExprOperand = prefixExpr->operand.get();
  ResolvedType prefixType = inferNodeDataType(prefixExpr);
  if (prefixExpr->operat.type == TokenType::PLUS_PLUS ||
      prefixExpr->operat.type == TokenType::MINUS_MINUS) {
    if (auto ident = dynamic_cast<Identifier *>(prefixExprOperand)) {
      auto symbol = resolveSymbolInfo(ident->expression.TokenLiteral);
      if (!symbol) {
        logSemanticErrors("Undefined variable in prefix expression '" +
                              ident->expression.TokenLiteral + "'",
                          prefixExpr);
        return;
      }
      if (!symbol->isMutable) {
        logSemanticErrors("Cannot apply '" + prefixExpr->operat.TokenLiteral +
                              "' to immutable variable '" +
                              ident->expression.TokenLiteral + "'",
                          prefixExpr);
        return;
      }
      if (!symbol->isInitialized) {
        logSemanticErrors("Cannot apply '" + prefixExpr->operat.TokenLiteral +
                              "' to uninitialized variable '" +
                              ident->expression.TokenLiteral + "'",
                          prefixExpr);
        return;
      }
    } else {
      logSemanticErrors("Prefix operator '" + prefixExpr->operat.TokenLiteral +
                            "' can only be applied to identifiers",
                        prefixExpr);
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

void Semantics::walkPostfixExpression(Node *node) {
  auto postfixExpr = dynamic_cast<PostfixExpression *>(node);
  if (!postfixExpr)
    return;

  ResolvedType postfixType = inferNodeDataType(postfixExpr);
  auto postfixOperand = postfixExpr->operand.get();
  if (postfixExpr->operator_token.type == TokenType::PLUS_PLUS ||
      postfixExpr->operator_token.type == TokenType::MINUS_MINUS) {
    if (auto ident = dynamic_cast<Identifier *>(postfixOperand)) {
      auto symbol = resolveSymbolInfo(ident->expression.TokenLiteral);
      if (!symbol) {
        logSemanticErrors("Undefined variable in postfix expression '" +
                              ident->expression.TokenLiteral + "'",
                          postfixExpr);
        return;
      }
      if (symbol->isMutable == false) {
        logSemanticErrors("Cannot apply '" +
                              postfixExpr->operator_token.TokenLiteral +
                              "' to immutable variable '" +
                              ident->expression.TokenLiteral + "'",
                          postfixExpr);
        return;
      }
      if (!symbol->isInitialized) {
        logSemanticErrors("Cannot apply '" +
                              postfixExpr->operator_token.TokenLiteral +
                              "' to uninitialized variable '" +
                              ident->expression.TokenLiteral + "'",
                          postfixExpr);
        return;
      }
    } else {
      logSemanticErrors("Postfix operator '" +
                            postfixExpr->operator_token.TokenLiteral +
                            "' can only be applied to identifiers",
                        postfixExpr);
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

void Semantics::walkUnwrapExpression(Node *node) {
  auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node);
  if (!unwrapExpr)
    return;

  bool hasError = false;

  auto expr = dynamic_cast<Expression *>(unwrapExpr->expr.get());
  if (!expr) {
    reportDevBug("Invalid unwrap expression", unwrapExpr->expr.get());
    return;
  }
  int exprLine = expr->expression.line;
  int exprCol = expr->expression.column;

  auto exprName = extractIdentifierName(unwrapExpr->expr.get());

  walker(expr);
  auto exprSym = metaData[expr];

  if (!exprSym) {
    logSemanticErrors("Unwrapped unknown identifier '" + exprName + "'",
                      unwrapExpr);
    return;
  }

  ResolvedType exprType = exprSym->type;

  if (!exprType.isNull) {
    logSemanticErrors("Cannot unwrap a concrete type '" +
                          exprType.resolvedName + "'",
                      unwrapExpr);
    hasError = true;
  }

  if (exprType.kind == DataType::VOID || exprType.kind == DataType::UNKNOWN) {
    logSemanticErrors("Cannot unwrap type '" + exprType.resolvedName + "'",
                      unwrapExpr);
    hasError = true;
  }

  logInternal("Unwrap type before strip :" + exprType.resolvedName);

  // Stripping the type
  exprType.isNull = false;
  auto strippedName = stripOptionalSuffix(exprType.resolvedName);
  exprType.resolvedName = strippedName;

  auto unwrapSym = std::make_shared<SymbolInfo>();
  unwrapSym->type = exprType;
  unwrapSym->hasError = hasError;
  unwrapSym->sizePerDimensions = exprSym->sizePerDimensions;
  unwrapSym->componentSize = exprSym->componentSize;

  logInternal("Unwrapped Type: " + exprType.resolvedName);

  metaData[unwrapExpr] = unwrapSym;
}

void Semantics::walkExpressionStatement(Node *node) {
  auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
  if (!exprStmt)
    return;
  walker(exprStmt->expression.get());
}

void Semantics::walkBasicType(Node *node) {
  auto basicType = dynamic_cast<BasicType *>(node);
  if (!basicType)
    return;
  ;
}

void Semantics::walkArrayType(Node *node) {
  auto arrayType = dynamic_cast<ArrayType *>(node);
  if (!arrayType)
    return;
  ;
}

void Semantics::walkCastExpression(Node *node) {
  auto castExpr = dynamic_cast<CastExpression *>(node);
  if (!castExpr)
    return;

  bool hasError = false;

  // Resolve Destination Type (The type in the < >)
  auto type = castExpr->type.get();
  if (!type)
    return;

  ResolvedType destinationType = inferNodeDataType(type);
  if (destinationType.isNull) {
    logSemanticErrors("Cannot cast to a nullable type '" +
                          destinationType.resolvedName + "'",
                      type);
    hasError = true;
  }
  // Check if the destination type is a scalar otherwise block it
  bool isDestinationValid = isInteger(destinationType) ||
                            isBoolean(destinationType) ||
                            isFloat(destinationType);
  if (!isDestinationValid) {
    logSemanticErrors("Invalid cast destination type '" +
                          destinationType.resolvedName + "'",
                      type);
    hasError = true;
  }

  // Resolve Source Type (The expression in the ( ))
  auto source = castExpr->expr.get();
  if (!source)
    return;

  walker(source);

  // Retrieve the metaData and check if the source type is valid
  auto sym = metaData[source];
  ResolvedType sourceType = sym->type;
  if (sourceType.isNull) {
    logSemanticErrors("Cannot cast from a nullable type '" +
                          sourceType.resolvedName + "'",
                      source);
    hasError = true;
  }

  if (sourceType.isPointer) {
    logSemanticErrors("Cannot cast from a pointer type '" +
                          sourceType.resolvedName + "' try using a bitcast ",
                      source);
    hasError = true;
  }

  if (sourceType.isRef) {
    logSemanticErrors("Cannot cast from a reference type '" +
                          sourceType.resolvedName + "'",
                      source);
    hasError = true;
  }

  bool isSourceValid =
      isInteger(sourceType) || isFloat(sourceType) || isBoolean(sourceType);

  if (!isSourceValid) {
    logSemanticErrors(
        "Invalid source cast type '" + sourceType.resolvedName + "'", source);
    hasError = true;
  }

  auto castInfo = std::make_shared<SymbolInfo>();

  castInfo->hasError = hasError;
  castInfo->type = destinationType;

  metaData[castExpr] = castInfo;
}

void Semantics::walkBitcastExpression(Node *node) {
  auto bitcastExpr = dynamic_cast<BitcastExpression *>(node);
  if (!bitcastExpr)
    return;

  bool hasError = false;

  // Resolve Destination Type (The type inside < >)
  auto destTypeNode = bitcastExpr->type.get();
  if (!destTypeNode)
    return;
  ResolvedType destinationType = inferNodeDataType(destTypeNode);

  // Resolve Source Type (The expression inside ( ))
  auto sourceExpr = bitcastExpr->expr.get();
  if (!sourceExpr)
    return;

  walker(sourceExpr); // Analyze the source expression first
  auto sourceSym = metaData[sourceExpr];

  if (!sourceSym) {
    reportDevBug("Could not resolve source expression for bitcast", sourceExpr);
    return;
  }
  ResolvedType sourceType = sourceSym->type;

  // Bitcastable is any type that fits in a general-purpose
  //  register.
  auto isBitcastable = [](ResolvedType type) {
    bool isPtr = type.isPointer;
    auto kind = type.kind;
    if (isPtr)
      return true;
    switch (kind) {
    case DataType::I8:
    case DataType::U8:
    case DataType::I16:
    case DataType::U16:
    case DataType::I32:
    case DataType::U32:
    case DataType::I64:
    case DataType::U64:
    case DataType::I128:
    case DataType::U128:
    case DataType::USIZE:
    case DataType::ISIZE:
    case DataType::F32:
    case DataType::F64:
    case DataType::OPAQUE:
      return true;
    default:
      return false;
    }
  };

  if (!isBitcastable(sourceType)) {
    logSemanticErrors(
        "bitcast source must be a pointer or integer type, got '" +
            sourceType.resolvedName + "'",
        sourceExpr);
    hasError = true;
  }

  if (!isBitcastable(destinationType)) {
    logSemanticErrors(
        "bitcast destination must be a pointer or integer type, got '" +
            destinationType.resolvedName + "'",
        destTypeNode);
    hasError = true;
  }

  // Build the Resulting Symbol
  auto bitcastInfo = std::make_shared<SymbolInfo>();
  bitcastInfo->type = destinationType;
  bitcastInfo->hasError = hasError;
  bitcastInfo->isPointer = destinationType.isPointer;
  bitcastInfo->isNullable = destinationType.isNull;
  metaData[bitcastExpr] = bitcastInfo;

  logInternal("Bitcast validated: " + sourceType.resolvedName + " -> " +
              destinationType.resolvedName);
}
