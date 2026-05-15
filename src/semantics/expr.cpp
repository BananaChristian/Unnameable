#include "ast.hpp"
#include "semantics.hpp"

void Semantics::walkInfixExpression(Node *node) {
  auto infixExpr = dynamic_cast<InfixExpression *>(node);
  if (!infixExpr)
    return;

  auto left = infixExpr->left_operand.get();
  walker(left);

  auto leftSym = getSymbolFromMeta(left);

  // Retrieve the symbol info of the left side
  if (!leftSym) {
    reportDevBug("Unresolved expression in infix left side", left);
  }

  auto lhsType = leftSym->type().type;

  // Access operators
  if (infixExpr->operat.type == TokenType::FULLSTOP ||
      infixExpr->operat.type == TokenType::SCOPE_OPERATOR) {

    auto rhsIdent = dynamic_cast<Identifier *>(infixExpr->right_operand.get());
    auto rhsCall =
        dynamic_cast<CallExpression *>(infixExpr->right_operand.get());

    if (!rhsIdent && !rhsCall) {
      logSemanticErrors(ErrorCode::InvalidInfix,
                        infixExpr->right_operand.get());
      return;
    }

    logInternal("Lhs Infix Type: " + lhsType.resolvedName);

    if (rhsCall) {
      lhsNode = infixExpr->left_operand.get();
      walkFunctionCallExpression(rhsCall);
      lhsNode = nullptr; // Reset for the next round
      insertMetaData(infixExpr, getSymbolFromMeta(rhsCall));
      return;
    }

    // Resolve member
    auto memberInfo =
        resultOfScopeOrDot(infixExpr->operat.type, lhsType,
                           rhsIdent->identifier.TokenLiteral, infixExpr);

    if (!memberInfo)
      return;

    logInternal("Member ID: " + memberInfo->codegen().ID);

    logInternal("Infix Type for member access :" +
                memberInfo->type().type.resolvedName);

    // Store metadata for RHS identifier
    insertMetaData(rhsIdent, memberInfo);
    insertMetaData(infixExpr, memberInfo);

    return;
  }

  // Infer type for normal infix
  walker(infixExpr->right_operand.get());
  ResolvedType infixType = inferNodeDataType(infixExpr);
  if (infixType.kind == DataType::UNKNOWN) {
    logSemanticErrors(ErrorCode::FailedToInfer, infixExpr->left_operand.get());
  }

  auto info = std::make_shared<SymbolInfo>();
  info->type().type = infixType;
  info->type().isNullable = false;
  info->storage().isConstant = false;
  info->hasError = hasError;
  info->storage().isInitialized = false;

  insertMetaData(infixExpr, info);
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
      auto name = extractIdentifierName(ident);
      auto symbol = resolveSymbolInfo(name);
      if (!symbol) {
        logSemanticErrors(ErrorCode::UndefinedVariable, prefixExpr, {name});
        return;
      }
      if (!symbol->storage().isMutable) {
        logSemanticErrors(ErrorCode::InvalidImmutUse, prefixExpr,
                          {prefixExpr->operat.TokenLiteral, name});
        return;
      }
      if (!symbol->storage().isInitialized) {
        logSemanticErrors(ErrorCode::InvalidImmutUse, prefixExpr,
                          {prefixExpr->operat.TokenLiteral, name});
        return;
      }
    } else {
      return;
    }
  }
  walker(prefixExprOperand);
  auto info = std::make_shared<SymbolInfo>();
  info->type().type = prefixType;
  info->type().isNullable = false;
  info->storage().isConstant = false;
  info->storage().isInitialized = false;

  insertMetaData(prefixExpr, info);
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
      auto name = extractIdentifierName(ident);
      auto symbol = resolveSymbolInfo(name);
      if (!symbol) {
        logSemanticErrors(ErrorCode::UndefinedVariable, postfixExpr, {name});
        return;
      }
      if (symbol->storage().isMutable == false) {
        logSemanticErrors(ErrorCode::InvalidImmutUse, postfixExpr, {name});
        return;
      }
      if (!symbol->storage().isInitialized) {
        logSemanticErrors(ErrorCode::InvalidUninitUse, postfixExpr,
                          {postfixExpr->operator_token.TokenLiteral, name});
        return;
      }
    } else {
      return;
    }
  }
  walker(postfixOperand);
  auto info = std::make_shared<SymbolInfo>();
  info->type().type = postfixType;
  info->type().isNullable = false;
  info->storage().isMutable = false;
  info->storage().isInitialized = false;

  insertMetaData(postfixExpr, info);
}

void Semantics::walkUnwrapExpression(Node *node) {
  auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node);
  if (!unwrapExpr)
    return;

  auto expr = dynamic_cast<Expression *>(unwrapExpr->expr.get());
  if (!expr) {
    reportDevBug("Invalid unwrap expression", unwrapExpr->expr.get());
    return;
  }

  auto exprName = extractIdentifierName(unwrapExpr->expr.get());

  walker(expr);
  auto exprSym = metaData[expr];

  if (!exprSym) {
    logSemanticErrors(ErrorCode::UndefinedVariable, unwrapExpr, {exprName});
    return;
  }

  ResolvedType exprType = exprSym->type().type;

  if (!exprType.isNull) {
    logSemanticErrors(ErrorCode::UnwrappableType, unwrapExpr,
                      {exprType.resolvedName});
    hasError = true;
  }

  if (exprType.kind == DataType::VOID || exprType.kind == DataType::UNKNOWN) {
    logSemanticErrors(ErrorCode::UnwrappableType, unwrapExpr,
                      {exprType.resolvedName});
    hasError = true;
  }

  logInternal("Unwrap type before strip :" + exprType.resolvedName);

  // Stripping the type
  exprType.isNull = false;
  auto strippedName = getBaseTypeName(exprType);
  exprType.resolvedName = strippedName;

  auto unwrapSym = std::make_shared<SymbolInfo>();
  unwrapSym->type().type = exprType;
  unwrapSym->hasError = hasError;
  unwrapSym->type().sizePerDimensions = exprSym->type().sizePerDimensions;
  unwrapSym->codegen().componentSize = exprSym->codegen().componentSize;

  logInternal("Unwrapped Type: " + exprType.resolvedName);

  insertMetaData(unwrapExpr, unwrapSym);
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
}

void Semantics::walkCastExpression(Node *node) {
  auto castExpr = dynamic_cast<CastExpression *>(node);
  if (!castExpr)
    return;

  auto destNode = castExpr->type.get();
  if (!destNode)
    return;

  ResolvedType destinationType = inferNodeDataType(destNode);

  // Cast destination must be a plain scalar, no modifiers
  if (!destinationType.isBase()) {
    logSemanticErrors(ErrorCode::NoneCastableType, destNode,
                      {destinationType.resolvedName});
  }

  if (destinationType.isNull) {
    logSemanticErrors(ErrorCode::NoneCastableType, destNode,
                      {destinationType.resolvedName});
  }

  auto isCastable = [](const ResolvedType &t) -> bool {
    if (!t.isBase())
      return false; // no pointers, refs, arrays
    switch (t.base().kind) {
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
    case DataType::ISIZE:
    case DataType::USIZE:
    case DataType::F32:
    case DataType::F64:
    case DataType::BOOLEAN:
      return true;
    default:
      return false;
    }
  };

  if (!isCastable(destinationType)) {
    logSemanticErrors(ErrorCode::NoneCastableType, destNode,
                      {destinationType.resolvedName});
  }

  auto sourceExpr = castExpr->expr.get();
  if (!sourceExpr)
    return;

  walker(sourceExpr);
  auto sym = getSymbolFromMeta(sourceExpr);
  if (!sym)
    return;

  ResolvedType sourceType = sym->type().type;

  if (sourceType.isNull) {
    logSemanticErrors(ErrorCode::NoneCastableType, sourceExpr,
                      {sourceType.resolvedName});
  }

  if (sourceType.isPointer()) {
    logSemanticErrors(ErrorCode::NoneCastableType, sourceExpr,
                      {sourceType.resolvedName});
  }

  if (sourceType.isRef()) {
    logSemanticErrors(ErrorCode::NoneCastableType, sourceExpr,
                      {sourceType.resolvedName});
  }

  if (sourceType.isArray()) {
    logSemanticErrors(ErrorCode::NoneCastableType, sourceExpr,
                      {sourceType.resolvedName});
  }

  if (!isCastable(sourceType)) {
    logSemanticErrors(ErrorCode::NoneCastableType, sourceExpr,
                      {sourceType.resolvedName});
  }

  auto castInfo = std::make_shared<SymbolInfo>();
  castInfo->hasError = hasError;
  castInfo->type().type = destinationType;
  insertMetaData(castExpr, castInfo);
}

void Semantics::walkBitcastExpression(Node *node) {
  auto bitcastExpr = dynamic_cast<BitcastExpression *>(node);
  if (!bitcastExpr)
    return;

  auto destNode = bitcastExpr->type.get();
  if (!destNode)
    return;

  ResolvedType destinationType = inferNodeDataType(destNode);

  auto sourceExpr = bitcastExpr->expr.get();
  if (!sourceExpr)
    reportDevBug("Failed to get bitcast source", bitcastExpr);

  walker(sourceExpr);
  auto sourceSym = getSymbolFromMeta(sourceExpr);
  if (!sourceSym)
    reportDevBug("Could not get source symbol for bitcast", sourceExpr);

  ResolvedType sourceType = sourceSym->type().type;

  // Bitcastable — pointer (any inner type) or scalar numeric
  // Blocks: refs, arrays, bare custom types, nullable
  auto isBitcastable = [](const ResolvedType &t) -> bool {
    // Nullable is never bitcastable — unwrap first
    if (t.isNull)
      return false;

    // References are aliases — no bit representation
    if (t.isRef())
      return false;

    // Arrays are memory regions not values
    if (t.isArray())
      return false;

    if (t.isPointer())
      return true;

    if (!t.isBase())
      return false;
    switch (t.base().kind) {
    case DataType::COMPONENT:
    case DataType::RECORD:
    case DataType::ENUM:
    case DataType::VOID:
    case DataType::ERROR:
    case DataType::UNKNOWN:
    case DataType::STRING:
      return false;
    default:
      return true; // all scalars and opaque
    }
  };

  if (destinationType.isNull) {
    logSemanticErrors(ErrorCode::NoneBitcastableType, destNode,
                      {destinationType.resolvedName});
  }

  if (!isBitcastable(sourceType)) {
    logSemanticErrors(ErrorCode::NoneBitcastableType, sourceExpr,
                      {sourceType.resolvedName});
  }

  if (!isBitcastable(destinationType)) {
    logSemanticErrors(ErrorCode::NoneBitcastableType, destNode,
                      {destinationType.resolvedName});
    return;
  }

  auto bitcastInfo = std::make_shared<SymbolInfo>();
  bitcastInfo->type().type = destinationType;
  bitcastInfo->hasError = hasError;
  insertMetaData(bitcastExpr, bitcastInfo);

  logInternal("Bitcast validated: " + sourceType.resolvedName + " -> " +
              destinationType.resolvedName);
}

// Walking array subscript expression
void Semantics::walkArraySubscriptExpression(Node *node) {
  auto arrExpr = dynamic_cast<ArraySubscript *>(node);
  if (!arrExpr)
    return;

  auto arrName = arrExpr->identifier->expression.TokenLiteral;

  auto arrSymbol = resolveSymbolInfo(arrName);
  if (!arrSymbol) {
    logSemanticErrors(ErrorCode::UndefinedVariable, arrExpr, {arrName});
    return;
  }

  if (arrSymbol->hasError)
    return;

  walker(arrExpr->identifier.get());
  for (const auto &expr : arrExpr->index_exprs)
    walker(expr.get());

  // Get the full array type
  ResolvedType arrType = arrSymbol->type().type;
  logInternal("Index Type '" + arrType.resolvedName + "'");

  if (!arrType.isArray() && !arrType.isPointer()) {
    errorHandler.addHint(
        "Subscripting is only permmited on arrays and pointers");
    logSemanticErrors(ErrorCode::NoneIndexableType, arrExpr,
                      {arrType.resolvedName});
  }

  if (arrType.isNull) {
    logSemanticErrors(ErrorCode::NoneIndexableType, arrExpr,
                      {arrType.resolvedName});
    return;
  }

  // Get the element type as when we index we want the element type itself not a
  // full array type
  auto elementType = arrType.innerType;
  for (size_t i = 1; i < arrExpr->index_exprs.size(); ++i) {
    if (!elementType || !elementType->innerType)
      break;
    elementType = elementType->innerType;
  }

  // Optionally store info for further analysis
  auto arrAccessInfo = std::make_shared<SymbolInfo>();
  arrAccessInfo->type().type = *elementType;
  arrAccessInfo->type().isPointer = elementType->isPointer();
  arrAccessInfo->type().isRef = elementType->isRef();
  arrAccessInfo->type().isArray = elementType->isArray();
  arrAccessInfo->hasError = hasError;
  arrAccessInfo->storage().isHeap = arrSymbol->storage().isHeap;
  arrAccessInfo->type().sizePerDimensions = arrSymbol->type().sizePerDimensions;
  arrAccessInfo->type().dynSizePerDimensions =
      arrSymbol->type().dynSizePerDimensions;

  insertMetaData(arrExpr, arrAccessInfo);
  if (arrSymbol->storage().isHeap) {
    arrAccessInfo->codegen().ID = arrSymbol->codegen().ID;
    transferBaton(arrExpr, arrSymbol->codegen().ID);
  }
}

// Walking the identifier expression
void Semantics::walkIdentifierExpression(Node *node) {
  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr)
    return;
  auto identName = identExpr->identifier.TokenLiteral;
  auto symbolInfo = resolveSymbolInfo(identName);

  if (!symbolInfo) {
    logSemanticErrors(ErrorCode::UndefinedVariable, identExpr, {identName});
    return;
  }

  if (symbolInfo->isFunction) {
    auto fnSym = std::make_shared<SymbolInfo>();
    fnSym->type().type =
        makeFnPtrType(symbolInfo->func().returnType, symbolInfo);
    fnSym->type().isFnPtr = true;
    fnSym->isFunction = true;
    insertMetaData(identExpr, fnSym);
    return;
  }

  insertMetaData(identExpr, symbolInfo);
  if (symbolInfo->storage().isHeap) {
    transferBaton(identExpr, symbolInfo->codegen().ID);
  }
}

// Walking address expression
void Semantics::walkAddressExpression(Node *node) {
  auto addrExpr = dynamic_cast<AddressExpression *>(node);
  if (!addrExpr)
    return;

  auto innerExpr = addrExpr->identifier.get();
  if (!innerExpr)
    reportDevBug("Failed to get addr operand", addrExpr);

  auto addrName = extractIdentifierName(innerExpr);

  auto call = dynamic_cast<CallExpression *>(innerExpr);

  if (auto infix = dynamic_cast<InfixExpression *>(innerExpr)) {
    call = dynamic_cast<CallExpression *>(infix->right_operand.get());
    // Check the operator
    if (infix->operat.type != TokenType::FULLSTOP &&
        infix->operat.type != TokenType::SCOPE_OPERATOR) {
      logSemanticErrors(ErrorCode::InvalidAddrOperand, infix, {addrName});
      return;
    }
  }

  if (call) {
    logSemanticErrors(ErrorCode::InvalidAddrOperand, innerExpr, {addrName});
    return;
  }

  walker(innerExpr);
  auto symbolInfo = getSymbolFromMeta(innerExpr);
  if (!symbolInfo) {
    logSemanticErrors(ErrorCode::UndefinedVariable, innerExpr, {addrName});
    return;
  }

  bool isHeap = symbolInfo->storage().isHeap;

  auto addrInfo = std::make_shared<SymbolInfo>();
  addrInfo->type().isPointer = true;
  addrInfo->storage().isHeap = isHeap;
  addrInfo->storage().allocType = symbolInfo->storage().allocType;
  addrInfo->relations().targetSymbol = symbolInfo;
  addrInfo->storage().isVolatile = symbolInfo->storage().isVolatile;
  addrInfo->type().type = inferNodeDataType(addrExpr);

  // Ths syncs the pointerCount with that of the original identifier
  symbolInfo->storage().pointerCount = addrInfo->storage().pointerCount;

  if (isHeap) {
    addrInfo->codegen().ID = symbolInfo->codegen().ID;
    transferBaton(addrExpr, addrInfo->codegen().ID);
  }
  insertMetaData(addrExpr, addrInfo);
}

void Semantics::walkDereferenceExpression(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
  if (!derefExpr)
    return;

  auto innerNode = derefExpr->identifier.get();
  std::string derefName = extractIdentifierName(innerNode);

  walker(innerNode);

  auto derefSym = getSymbolFromMeta(innerNode);
  if (!derefSym) {
    logSemanticErrors(ErrorCode::UndefinedVariable, derefExpr, {derefName});
    return;
  }

  if (derefSym->hasError)
    return;

  // Must be a pointer to dereference
  if (!derefSym->type().type.isPointer()) {
    logSemanticErrors(ErrorCode::NoneDereferencableType, derefExpr,
                      {derefSym->type().type.resolvedName});
    return;
  }

  // Block nullable, must unwrap first
  if (derefSym->type().type.isNull) {
    logSemanticErrors(ErrorCode::NoneDereferencableType, derefExpr,
                      {derefSym->type().type.resolvedName});
    return;
  }

  // Block opaque pointer,no type info to dereference into
  // With chains opaque lives in innerType not at the top level
  if (!derefSym->type().type.innerType) {
    logSemanticErrors(ErrorCode::NoneDereferencableType, derefExpr,
                      {derefSym->type().type.resolvedName});
    return;
  }

  if (derefSym->type().type.innerType->base().kind == DataType::OPAQUE) {
    logSemanticErrors(ErrorCode::NoneDereferencableType, derefExpr,
                      {derefSym->type().type.resolvedName});
    return;
  }

  // Peel one pointer level, just return what it points to
  ResolvedType finalType = *derefSym->type().type.innerType;

  auto derefInfo = std::make_shared<SymbolInfo>();
  derefInfo->type().type = finalType;
  derefInfo->storage().isMutable = derefSym->storage().isMutable;
  derefInfo->storage().isHeap = derefSym->storage().isHeap;
  derefInfo->storage().isVolatile = derefSym->storage().isVolatile;
  derefInfo->storage().allocType = derefSym->storage().allocType;
  derefInfo->relations().targetSymbol = derefSym->relations().targetSymbol;

  // Transfer baton if heap
  if (derefSym->storage().isHeap) {
    derefInfo->codegen().ID = derefSym->codegen().ID;
    transferBaton(derefExpr, derefInfo->codegen().ID);
  }

  insertMetaData(derefExpr, derefInfo);
}

void Semantics::walkSizeOfExpression(Node *node) {
  auto sizeExpr = dynamic_cast<SizeOfExpression *>(node);
  if (!sizeExpr)
    return;

  Expression *typeNode = sizeExpr->type.get();
  if (!typeNode)
    return;

  ResolvedType type = inferNodeDataType(typeNode);
  bool isInbuilt = isInteger(type) || isBoolean(type) || isChar(type) ||
                   isString(type) || isFloat(type) ||
                   type.kind == DataType::OPAQUE;

  auto typeName = sizeExpr->type->expression.TokenLiteral;

  // Check if the type written is legal that is if its not inbuilt
  if (!isInbuilt) {
    auto typeIt = customTypesTable.find(typeName);
    if (typeIt == customTypesTable.end()) {
      logSemanticErrors(ErrorCode::UndefinedVariable, typeNode, {typeName});
    }
  }

  auto sizeInfo = std::make_shared<SymbolInfo>();
  sizeInfo->type().type = ResolvedType::makeBase(DataType::USIZE, "usize");
  sizeInfo->hasError = hasError;

  insertMetaData(sizeExpr, sizeInfo);
}
