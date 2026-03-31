#include "ast.hpp"
#include "semantics.hpp"

void Semantics::walkInfixExpression(Node *node) {
  auto infixExpr = dynamic_cast<InfixExpression *>(node);
  if (!infixExpr)
    return;

  bool hasError = false;

  auto left = infixExpr->left_operand.get();
  walker(left);

  auto leftSym = getSymbolFromMeta(left);

  // Retrieve the symbol info of the left side
  if (!leftSym) {
    logSemanticErrors("Unresolved expression in infix left side", left);
    return;
  }

  auto lhsType = leftSym->type().type;

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

    logInternal("Member ID: " + memberInfo->codegen().ID);

    logInternal("Infix Type for member access :" +
                memberInfo->type().type.resolvedName);

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
  }

  auto info = std::make_shared<SymbolInfo>();
  info->type().type = infixType;
  info->type().isNullable = false;
  info->storage().isConstant = false;
  info->hasError = hasError;
  info->storage().isInitialized = false;

  hasError = false;
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
      if (!symbol->storage().isMutable) {
        logSemanticErrors("Cannot apply '" + prefixExpr->operat.TokenLiteral +
                              "' to immutable variable '" +
                              ident->expression.TokenLiteral + "'",
                          prefixExpr);
        return;
      }
      if (!symbol->storage().isInitialized) {
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
  info->type().type = prefixType;
  info->type().isNullable = false;
  info->storage().isConstant = false;
  info->storage().isInitialized = false;

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
      if (symbol->storage().isMutable == false) {
        logSemanticErrors("Cannot apply '" +
                              postfixExpr->operator_token.TokenLiteral +
                              "' to immutable variable '" +
                              ident->expression.TokenLiteral + "'",
                          postfixExpr);
        return;
      }
      if (!symbol->storage().isInitialized) {
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
  info->type().type = postfixType;
  info->type().isNullable = false;
  info->storage().isMutable = false;
  info->storage().isInitialized = false;

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

  auto exprName = extractIdentifierName(unwrapExpr->expr.get());

  walker(expr);
  auto exprSym = metaData[expr];

  if (!exprSym) {
    logSemanticErrors("Unwrapped unknown identifier '" + exprName + "'",
                      unwrapExpr);
    return;
  }

  ResolvedType exprType = exprSym->type().type;

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
  auto strippedName = getBaseTypeName(exprType);
  exprType.resolvedName = strippedName;

  auto unwrapSym = std::make_shared<SymbolInfo>();
  unwrapSym->type().type = exprType;
  unwrapSym->hasError = hasError;
  unwrapSym->type().sizePerDimensions = exprSym->type().sizePerDimensions;
  unwrapSym->codegen().componentSize = exprSym->codegen().componentSize;

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
}

void Semantics::walkCastExpression(Node *node) {
  auto castExpr = dynamic_cast<CastExpression *>(node);
  if (!castExpr)
    return;

  bool hasError = false;

  auto destNode = castExpr->type.get();
  if (!destNode)
    return;

  ResolvedType destinationType = inferNodeDataType(destNode);

  // Cast destination must be a plain scalar, no modifiers
  if (!destinationType.isBase()) {
    errorHandler.addHint("'cast' only converts between scalar numeric types")
        .addHint("For pointer conversions use 'bitcast'")
        .addHint("Example: cast<i32>(myFloat)");
    logSemanticErrors("Cast destination '" + destinationType.resolvedName +
                          "' must be a scalar type",
                      destNode);
  }

  if (destinationType.isNull) {
    errorHandler.addHint("Unwrap the nullable before casting")
        .addHint("Use '?''?' or 'unwrap' to get the non-nullable value first");
    logSemanticErrors("Cannot cast to nullable type '" +
                          destinationType.resolvedName + "'",
                      destNode);
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
    errorHandler.addHint("Valid cast destinations: integers, floats, bool")
        .addHint("For pointer conversions use 'bitcast'")
        .addHint("For custom types consider implementing a conversion method");
    logSemanticErrors("Invalid cast destination type '" +
                          destinationType.resolvedName + "'",
                      destNode);
    hasError = true;
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
    errorHandler.addHint("Unwrap the nullable before casting")
        .addHint("Use '?''?' or 'unwrap' to get the non-nullable value first");
    logSemanticErrors("Cannot cast from nullable type '" +
                          sourceType.resolvedName + "'",
                      sourceExpr);
  }

  if (sourceType.isPointer()) {
    errorHandler.addHint("Pointers cannot be cast — use 'bitcast' instead")
        .addHint(
            "Example: bitcast<usize>(myPtr) to get the address as integer");
    logSemanticErrors("Cannot cast from pointer type '" +
                          sourceType.resolvedName + "'",
                      sourceExpr);
    hasError = true;
  }

  if (sourceType.isRef()) {
    errorHandler
        .addHint("References are aliases, cast the target variable directly")
        .addHint(
            "Example: cast<i32>(targetVar) instead of casting the reference");
    logSemanticErrors("Cannot cast from reference type '" +
                          sourceType.resolvedName + "'",
                      sourceExpr);
    hasError = true;
  }

  if (sourceType.isArray()) {
    errorHandler
        .addHint("Arrays cannot be cast, cast individual elements instead")
        .addHint("Example: cast<i32>(myArr[0])");
    logSemanticErrors("Cannot cast from array type '" +
                          sourceType.resolvedName + "'",
                      sourceExpr);
    hasError = true;
  }

  if (!isCastable(sourceType)) {
    errorHandler.addHint("Valid cast sources: integers, floats, bool")
        .addHint(
            "Custom types cannot be cast, use a conversion method instead");
    logSemanticErrors("Invalid cast source type '" + sourceType.resolvedName +
                          "'",
                      sourceExpr);
  }

  auto castInfo = std::make_shared<SymbolInfo>();
  castInfo->hasError = hasError;
  castInfo->type().type = destinationType;
  metaData[castExpr] = castInfo;
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
    return;

  walker(sourceExpr);
  auto sourceSym = getSymbolFromMeta(sourceExpr);
  if (!sourceSym) {
    reportDevBug("Could not resolve source for bitcast", sourceExpr);
    return;
  }

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
    errorHandler.addHint("Bitcast destination cannot be nullable")
        .addHint("Remove the '?' from the destination type");
    logSemanticErrors("Cannot bitcast to nullable type '" +
                          destinationType.resolvedName + "'",
                      destNode);
  }

  if (!isBitcastable(sourceType)) {
    errorHandler
        .addHint("Bitcastable types: pointers, integers, floats, opaque")
        .addHint("References and arrays cannot be bitcast")
        .addHint("Bare custom types cannot be bitcast, use ptr MyType instead");
    logSemanticErrors("Cannot bitcast from type '" + sourceType.resolvedName +
                          "'",
                      sourceExpr);
  }

  if (!isBitcastable(destinationType)) {
    errorHandler
        .addHint("Bitcastable destinations: pointers, integers, floats, opaque")
        .addHint("References and arrays cannot be bitcast targets")
        .addHint("Bare custom types cannot be bitcast, use ptr MyType instead");
    logSemanticErrors("Cannot bitcast to type '" +
                          destinationType.resolvedName + "'",
                      destNode);
  }

  auto bitcastInfo = std::make_shared<SymbolInfo>();
  bitcastInfo->type().type = destinationType;
  bitcastInfo->hasError = hasError;
  hasError = false;
  metaData[bitcastExpr] = bitcastInfo;

  logInternal("Bitcast validated: " + sourceType.resolvedName + " -> " +
              destinationType.resolvedName);
}

// Walking array subscript expression
void Semantics::walkArraySubscriptExpression(Node *node) {
  auto arrExpr = dynamic_cast<ArraySubscript *>(node);
  if (!arrExpr)
    return;

  bool hasError = false;
  auto arrName = arrExpr->identifier->expression.TokenLiteral;

  auto arrSymbol = resolveSymbolInfo(arrName);
  if (!arrSymbol) {
    logSemanticErrors("Unidentified variable '" + arrName + "'", arrExpr);
    return;
  }

  if (arrSymbol->hasError)
    return;

  walker(arrExpr->identifier.get());

  // Get the full array type
  ResolvedType arrType = arrSymbol->type().type;
  logInternal("Array Type '" + arrType.resolvedName + "'");

  if (!arrType.isArray()) {
    logSemanticErrors("Cannot index a non array '" + arrName + "of type '" +
                          arrType.resolvedName + "'",
                      arrExpr);
    hasError = true;
  }

  if (arrType.isNull) {
    logSemanticErrors("Cannot index a nullable array '" + arrName +
                          "' of type '" + arrType.resolvedName +
                          "' without unwrapping it ",
                      arrExpr);
    hasError = true;
  }

  // Get the element type as when we index we want the element type itself not a
  // full array type
  auto elementType = getArrayElementType(arrType);

  // Optionally store info for further analysis
  auto arrAccessInfo = std::make_shared<SymbolInfo>();
  arrAccessInfo->type().type = elementType;
  arrAccessInfo->hasError = hasError;
  arrAccessInfo->storage().isHeap = arrSymbol->storage().isHeap;
  arrAccessInfo->type().sizePerDimensions = arrSymbol->type().sizePerDimensions;

  metaData[arrExpr] = arrAccessInfo;
}

// Walking the identifier expression
void Semantics::walkIdentifierExpression(Node *node) {
  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr)
    return;
  auto identName = identExpr->identifier.TokenLiteral;
  auto symbolInfo = resolveSymbolInfo(identName);

  if (!symbolInfo) {
    logSemanticErrors(" Use of undeclared identifer '" + identName + "'",
                      identExpr);
    return;
  }

  metaData[identExpr] = symbolInfo;
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
    return;

  auto addrName = extractIdentifierName(innerExpr);

  auto call = dynamic_cast<CallExpression *>(innerExpr);
  auto metCall = dynamic_cast<MethodCallExpression *>(innerExpr);

  if (auto infix = dynamic_cast<InfixExpression *>(innerExpr)) {
    // Check the operator
    if (infix->operat.type != TokenType::FULLSTOP &&
        infix->operat.type != TokenType::SCOPE_OPERATOR) {
      errorHandler
          .addHint(
              "When you use the 'addr' operator you want to get the address "
              "of that expression")
          .addHint("The 'addr' operator can only work on an expression with a "
                   "permanent "
                   "location an arithetic operation of any sorts does not have "
                   "this");
      logSemanticErrors(
          "Cannot take address of an arithmetic expression or comparison",
          infix);
      return;
    }
  }

  if (call || metCall) {

    errorHandler
        .addHint("When you use the 'addr' operator you want to get the address "
                 "of that expression")
        .addHint("The 'addr' operator can only work on an expression "
                 "with a permanent location, function calls are "
                 "temporary in nature and have no location ");
    logSemanticErrors("Cannot get the address to a temporary value '" +
                          addrName + "'",
                      innerExpr);
    return;
  }

  walker(innerExpr);
  auto symbolInfo = metaData[innerExpr];

  if (!symbolInfo) {
    logSemanticErrors("Unidentified variable '" + addrName + "'", innerExpr);
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
  metaData[addrExpr] = addrInfo;
}

void Semantics::walkMoveExpression(Node *node) {
  auto moveExpr = dynamic_cast<MoveExpression *>(node);
  if (!moveExpr)
    return;

  auto innerExpr = moveExpr->expr.get();
  if (!innerExpr)
    return;
  auto exprName = extractIdentifierName(innerExpr);

  auto call = dynamic_cast<CallExpression *>(innerExpr);
  auto metCall = dynamic_cast<MethodCallExpression *>(innerExpr);

  if (auto infix = dynamic_cast<InfixExpression *>(innerExpr)) {
    // Check the operator
    if (infix->operat.type != TokenType::FULLSTOP &&
        infix->operat.type != TokenType::SCOPE_OPERATOR) {
      errorHandler
          .addHint("When you use the 'move' operator you want to transfer "
                   "ownership of memory from one variable to another")
          .addHint("The move operator can only work on an expression with a "
                   "permanent "
                   "location an arithetic operation of any sorts does not have "
                   "this");
      logSemanticErrors("Cannot move of an arithmetic expression or comparison",
                        infix);
      return;
    }
  }

  if (call || metCall) {
    errorHandler
        .addHint("When you use the 'move' operator you want to transfer "
                 "ownership of memory from one variable to another")
        .addHint("The move operator can only work on an expression "
                 "with a permanent location, function calls are "
                 "temoprary in nature and have no location ");
    logSemanticErrors("Cannot move a temporary value '" + exprName + "'",
                      innerExpr);
    return;
  }

  walker(innerExpr);
  auto symbolInfo = metaData[innerExpr];

  if (!symbolInfo) {
    logSemanticErrors("Unidentified variable '" + exprName + "'", innerExpr);
    return;
  }

  if (!symbolInfo->storage().isHeap) {
    logSemanticErrors("Cannot move variable '" + exprName +
                          "' as it is stored on the stack",
                      innerExpr);
  }

  if (symbolInfo->type().type.isPointer() || symbolInfo->type().type.isRef()) {
    logSemanticErrors("Cannot move variable '" + exprName +
                          "' as it is of a type '" +
                          symbolInfo->type().type.resolvedName + "'",
                      innerExpr);
  }

  if (symbolInfo->storage().isInvalid) {
    logSemanticErrors("Variable '" + exprName + "' was already moved",
                      innerExpr);
  }

  metaData[moveExpr] = symbolInfo;
  hasError = false;
}

void Semantics::walkDereferenceExpression(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
  if (!derefExpr)
    return;

  auto innerNode = derefExpr->identifier.get();
  std::string derefName = extractIdentifierName(innerNode);

  walker(innerNode);

  auto derefSym = resolveSymbolInfo(derefName);
  if (!derefSym) {
    errorHandler.addHint("Check spelling or declaration order")
        .addHint("Variables must be declared before use");
    logSemanticErrors("Use of undeclared identifier '" + derefName + "'",
                      derefExpr);
    return;
  }

  if (derefSym->hasError)
    return;

  // Must be a pointer to dereference
  if (!derefSym->type().type.isPointer()) {
    errorHandler.addHint("Only pointer types can be dereferenced")
        .addHint("Declare as: ptr i32 " + derefName + " -> addr target")
        .addHint("Then dereference with: deref " + derefName);
    logSemanticErrors("Cannot dereference non-pointer type '" +
                          derefSym->type().type.resolvedName + "'",
                      derefExpr);
    return;
  }

  // Block nullable, must unwrap first
  if (derefSym->type().type.isNull) {
    errorHandler.addHint("Unwrap the nullable pointer before dereferencing")
        .addHint("Use '?''?' or 'unwrap' to get the non-nullable pointer first")
        .addHint("Example: deref (myPtr ?? defaultPtr)");
    logSemanticErrors("Cannot dereference nullable pointer '" + derefName + "'",
                      derefExpr);
    hasError = false;
    return;
  }

  // Block opaque pointer,no type info to dereference into
  // With chains opaque lives in innerType not at the top level
  if (!derefSym->type().type.innerType) {
    errorHandler.addHint("Pointer has no inner type information")
        .addHint("Use bitcast to convert to a typed pointer first")
        .addHint("Example: bitcast<ptr i32>(myOpaquePtr)");
    logSemanticErrors("Cannot dereference untyped pointer '" + derefName + "'",
                      derefExpr);
    return;
  }

  if (derefSym->type().type.innerType->base().kind == DataType::OPAQUE) {
    errorHandler
        .addHint("Opaque pointers have no concrete type to dereference into")
        .addHint("Use bitcast to convert to a typed pointer first")
        .addHint("Example: bitcast<ptr i32>(myOpaquePtr)");
    logSemanticErrors("Cannot dereference opaque pointer '" + derefName + "'",
                      derefExpr);
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

  metaData[derefExpr] = derefInfo;
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
                   isString(type) || isFloat(type);

  auto typeName = sizeExpr->type->expression.TokenLiteral;

  // Check if the type written is legal that is if its not inbuilt
  if (!isInbuilt) {
    auto typeIt = customTypesTable.find(typeName);
    if (typeIt == customTypesTable.end()) {
      logSemanticErrors("Unknown type '" + typeName + "' in sizeof", typeNode);
    }
  }

  auto sizeInfo = std::make_shared<SymbolInfo>();
  sizeInfo->type().type = ResolvedType::makeBase(DataType::USIZE, "usize");
  sizeInfo->hasError = hasError;

  hasError = false;

  metaData[sizeExpr] = sizeInfo;
}
