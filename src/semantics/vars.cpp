#include "ast.hpp"
#include "semantics.hpp"
#include "token.hpp"

// Walking the data type literals
void Semantics::walkBooleanLiteral(Node *node) {
  auto boolLiteral = dynamic_cast<BooleanLiteral *>(node);
  if (!boolLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::BOOLEAN, "bool"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[boolLiteral] = info;
}

void Semantics::walkStringLiteral(Node *node) {
  auto strLiteral = dynamic_cast<StringLiteral *>(node);
  if (!strLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::STRING, "string"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[strLiteral] = info;
}

void Semantics::walkChar8Literal(Node *node) {
  auto char8Literal = dynamic_cast<Char8Literal *>(node);
  if (!char8Literal)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::CHAR8, "char8"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[char8Literal] = info;
}

void Semantics::walkChar16Literal(Node *node) {
  auto lit = dynamic_cast<Char16Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::CHAR16, "char16"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkChar32Literal(Node *node) {
  auto lit = dynamic_cast<Char32Literal *>(node);
  if (!lit)
    return;
  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::CHAR32, "char32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI8Literal(Node *node) {
  auto lit = dynamic_cast<I8Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I8, "i8"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU8Literal(Node *node) {
  auto lit = dynamic_cast<U8Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U8, "u8"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI16Literal(Node *node) {
  auto lit = dynamic_cast<I16Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I16, "i16"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU16Literal(Node *node) {
  auto lit = dynamic_cast<U16Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U16, "u16"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI32Literal(Node *node) {
  auto intLiteral = dynamic_cast<I32Literal *>(node);
  if (!intLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I32, "i32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[intLiteral] = info;
}

void Semantics::walkU32Literal(Node *node) {
  auto intLiteral = dynamic_cast<U32Literal *>(node);
  if (!intLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U32, "u32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[intLiteral] = info;
}

void Semantics::walkI64Literal(Node *node) {
  auto lit = dynamic_cast<I64Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I64, "i64"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU64Literal(Node *node) {
  auto lit = dynamic_cast<U64Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U64, "u64"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI128Literal(Node *node) {
  auto lit = dynamic_cast<I128Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I128, "i128"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU128Literal(Node *node) {
  auto lit = dynamic_cast<U128Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U128, "u128"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkISIZELiteral(Node *node) {
  auto lit = dynamic_cast<ISIZELiteral *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::ISIZE, "isize"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkUSIZELiteral(Node *node) {
  auto lit = dynamic_cast<USIZELiteral *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::USIZE, "usize"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkF32Literal(Node *node) {
  auto f32Literal = dynamic_cast<F32Literal *>(node);
  if (!f32Literal)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::F32, "f32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[f32Literal] = info;
}

void Semantics::walkF64Literal(Node *node) {
  auto f64Literal = dynamic_cast<F64Literal *>(node);
  if (!f64Literal)
    return;
  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::F64, "f64"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[f64Literal] = info;
}

// Walking the null literal
void Semantics::walkNullLiteral(Node *node) {
  auto nullLit = dynamic_cast<NullLiteral *>(node);
  if (!nullLit)
    return;

  auto symbol = std::make_shared<SymbolInfo>();
  symbol->type =
      ResolvedType{DataType::UNKNOWN, "null"}; // Unknown data type for now
  metaData[nullLit] = symbol;
}

void Semantics::walkSizeOfExpression(Node *node) {
  auto sizeExpr = dynamic_cast<SizeOfExpression *>(node);
  if (!sizeExpr)
    return;

  bool hasError = false;

  Expression *typeNode = sizeExpr->type.get();
  if (!typeNode)
    return;

  int line = typeNode->expression.line;
  int col = typeNode->expression.column;

  ResolvedType type = inferNodeDataType(typeNode);
  bool isInbuilt = isInteger(type) || isBoolean(type) || isChar(type) ||
                   isString(type) || isFloat(type);

  auto typeName = sizeExpr->type->expression.TokenLiteral;

  // Check if the type written is legal that is if its not inbuilt
  if (!isInbuilt) {
    auto typeIt = customTypesTable.find(typeName);
    if (typeIt == customTypesTable.end()) {
      logSemanticErrors("Unknown type '" + typeName + "' in sizeof", line, col);
      hasError = true;
    }
  }

  auto sizeInfo = std::make_shared<SymbolInfo>();
  sizeInfo->type = ResolvedType{DataType::USIZE, "usize"};
  sizeInfo->hasError = hasError;

  metaData[sizeExpr] = sizeInfo;
}

// Walking the array literal
void Semantics::walkArrayLiteral(Node *node) {
  auto arrLit = dynamic_cast<ArrayLiteral *>(node);
  if (!arrLit)
    return;

  // Positional info for error logging
  auto line = arrLit->expression.line;
  auto col = arrLit->expression.column;

  bool hasError = false;

  ResolvedType arrType = inferNodeDataType(arrLit);
  for (const auto &item : arrLit->array) {
    walker(item.get()); // Calling their walkers
  }

  auto sizePerDims = getSizePerDimesion(arrLit);

  // Storing metaData about the array
  auto arrInfo = std::make_shared<SymbolInfo>();
  arrInfo->type = arrType;
  arrInfo->isNullable = false;
  arrInfo->isMutable = false;
  arrInfo->isConstant = false;
  arrInfo->sizePerDimensions = sizePerDims;

  metaData[arrLit] = arrInfo;
}

// Walking array subscript expression
void Semantics::walkArraySubscriptExpression(Node *node) {
  auto arrExpr = dynamic_cast<ArraySubscript *>(node);
  if (!arrExpr)
    return;

  bool hasError = false;
  auto arrName = arrExpr->identifier->expression.TokenLiteral;
  auto line = arrExpr->expression.line;
  auto col = arrExpr->expression.column;

  auto arrSymbol = resolveSymbolInfo(arrName);
  if (!arrSymbol) {
    logSemanticErrors("Unidentified variable '" + arrName + "'", line, col);
    return;
  }

  if (arrSymbol->hasError)
    return;

  walker(arrExpr->identifier.get());

  // Get the full array type
  ResolvedType arrType = arrSymbol->type;
  logInternal("Array Type '" + arrType.resolvedName + "'");

  if (!arrType.isArray) {
    logSemanticErrors("Cannot index a non array '" + arrName + "of type '" +
                          arrType.resolvedName + "'",
                      line, col);
    hasError = true;
  }

  if (arrType.isNull) {
    logSemanticErrors("Cannot index a nullable array '" + arrName +
                          "' of type '" + arrType.resolvedName +
                          "' without unwrapping it ",
                      line, col);
    hasError = true;
  }

  // Get the element type as when we index we want the element type itself not a
  // full array type
  auto elementType = getArrayElementType(arrType);

  // Optionally store info for further analysis
  auto arrAccessInfo = std::make_shared<SymbolInfo>();
  arrAccessInfo->type = elementType;
  arrAccessInfo->hasError = hasError;
  arrAccessInfo->isHeap = arrSymbol->isHeap;
  arrAccessInfo->isDheap = arrSymbol->isDheap;

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
                      identExpr->expression.line, identExpr->expression.column);
    return;
  }

  if (symbolInfo->isHeap || symbolInfo->isDheap) {
    bool inLoop = !loopContext.empty() && loopContext.back();
    if (inLoop && !symbolInfo->bornInLoop) {
      // ELDERS
      symbolInfo->needsPostLoopFree = true;
      loopDeathRow.insert(symbolInfo.get());
    } else if (inLoop && symbolInfo->bornInLoop) {
      // RESIDENTS
      // Track them so we can ensure they die before the loop repeats
      loopResidentDeathRow.insert(symbolInfo.get());
      symbolInfo->lastUseNode = identExpr;
    } else {
      // Normal linear path
      currentBranchIdents.push_back(identExpr);
      symbolInfo->lastUseNode = identExpr;
    }
  }

  metaData[identExpr] = symbolInfo;
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
  auto line = innerExpr->expression.line;
  auto col = innerExpr->expression.column;

  auto call = dynamic_cast<CallExpression *>(innerExpr);
  auto metCall = dynamic_cast<MethodCallExpression *>(innerExpr);

  if (auto infix = dynamic_cast<InfixExpression *>(innerExpr)) {
    // Check the operator
    if (infix->operat.type != TokenType::FULLSTOP &&
        infix->operat.type != TokenType::SCOPE_OPERATOR) {

      logSemanticErrors(
          "Cannot take address of an arithmetic expression or comparison",
          infix->expression.line, infix->expression.column);
      return;
    }
  }

  if (call || metCall) {
    logSemanticErrors("Cannot get the address to a temporary value '" +
                          addrName + "'",
                      line, col);
    return;
  }

  walker(innerExpr);
  auto symbolInfo = metaData[innerExpr];

  if (!symbolInfo) {
    logSemanticErrors("Unidentified variable '" + addrName + "'", line, col);
    return;
  }

  bool hasError = false;
  bool isHeap = symbolInfo->isHeap;
  bool isDheap = symbolInfo->isDheap;

  if (symbolInfo->isHeap || symbolInfo->isDheap)
    symbolInfo->lastUseNode = addrExpr;

  auto addrInfo = std::make_shared<SymbolInfo>();
  addrInfo->isPointer = true;
  addrInfo->isHeap = isHeap;
  addrInfo->isDheap = isDheap;
  addrInfo->allocType = symbolInfo->allocType;
  addrInfo->targetSymbol = symbolInfo;
  addrInfo->type = inferNodeDataType(addrExpr);

  metaData[addrExpr] = addrInfo;
}

void Semantics::walkDereferenceExpression(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
  if (!derefExpr)
    return;

  auto innerNode = derefExpr->identifier.get();
  std::string derefName = extractIdentifierName(innerNode);

  auto line = derefExpr->identifier->expression.line;
  auto col = derefExpr->identifier->expression.column;

  walker(innerNode);

  auto derefSym = resolveSymbolInfo(derefName);
  if (!derefSym) {
    logSemanticErrors("Use of undeclared identifier '" + derefName + "'", line,
                      col);
    return;
  }

  if (derefSym->hasError) {
    logSemanticErrors(
        "Cannot dereference erronious pointer '" + derefName + "'", line, col);
    return;
  }

  // Block dereferencing non pointer
  if (!derefSym->type.isPointer) {
    logSemanticErrors("Cannot dereference non pointer type '" +
                          derefSym->type.resolvedName + "'",
                      line, col);
    return; // Dont bother
  }

  // Block dereferencing opaque pointer
  if (derefSym->type.kind == DataType::OPAQUE) {
    logSemanticErrors(
        "Cannot dereference an opaque pointer '" + derefName + "'", line, col);
    return;
  }

  // Block dereferencing nullable types
  if (derefSym->type.isNull) {
    logSemanticErrors("Cannot dereference nullable pointer type '" +
                          derefSym->type.resolvedName + "'",
                      line, col);
    return;
  }

  derefSym->lastUseNode = derefExpr;

  ResolvedType peeledType = derefSym->type;
  peeledType.isPointer = false;
  ResolvedType finalType = isPointerType(peeledType);

  auto derefInfo = std::make_shared<SymbolInfo>();

  derefInfo->isPointer = finalType.isPointer;
  derefInfo->type = finalType; // Setting the final 'i32' type here
  derefInfo->isMutable = derefSym->isMutable;
  derefInfo->isInitialized = derefSym->isInitialized;

  if (derefSym->targetSymbol != nullptr) {
    derefInfo->targetSymbol = derefSym->targetSymbol;
    derefSym->targetSymbol->lastUseNode = derefExpr;
  } else {
    derefInfo->targetSymbol = nullptr;
  }

  metaData[derefExpr] = derefInfo;
}

void Semantics::walkAssignStatement(Node *node) {
  auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
  if (!assignStmt)
    return;

  std::shared_ptr<SymbolInfo> symbol = nullptr;
  std::string assignName;
  bool hasError = false;

  // --- Handle self.field assignments ---
  if (auto *selfExpr =
          dynamic_cast<SelfExpression *>(assignStmt->identifier.get())) {
    logInternal("self assignment triggered");
    if (currentTypeStack.empty() ||
        currentTypeStack.back().type.kind != DataType::COMPONENT) {
      logSemanticErrors("'self' cannot be used outside a component",
                        selfExpr->expression.line, selfExpr->expression.column);
      hasError = true;
      return;
    }

    // Start from the current component
    auto &compScope = currentTypeStack.back();
    auto currentTypeName = compScope.typeName;

    std::shared_ptr<MemberInfo> fieldInfo;

    // Walk the full chain of fields
    for (const auto &field : selfExpr->fields) {
      auto ident = dynamic_cast<Identifier *>(field.get());
      if (!ident) {
        logSemanticErrors("Expected identifier in self expression chain",
                          selfExpr->expression.line,
                          selfExpr->expression.column);
        hasError = true;
        return;
      }

      const std::string fieldName = ident->identifier.TokenLiteral;

      // Look up current type
      auto compTypeIt = customTypesTable.find(currentTypeName);
      if (compTypeIt == customTypesTable.end()) {
        logSemanticErrors("Component '" + currentTypeName + "' does not exist",
                          selfExpr->expression.line,
                          selfExpr->expression.column);
        hasError = true;
        return;
      }

      auto &members = compTypeIt->second->members;
      auto memIt = members.find(fieldName);
      if (memIt == members.end()) {
        logSemanticErrors("Field '" + fieldName + "' not found in component '" +
                              currentTypeName + "'",
                          ident->identifier.line, ident->identifier.column);
        hasError = true;
        return;
      }

      fieldInfo = memIt->second;
      currentTypeName = fieldInfo->type.resolvedName; // move deeper
    }

    // Wrap final field info into SymbolInfo
    symbol = std::make_shared<SymbolInfo>();
    symbol->type = fieldInfo->type;
    symbol->isNullable = fieldInfo->isNullable;
    symbol->isMutable = fieldInfo->isMutable;
    symbol->isConstant = fieldInfo->isConstant;
    symbol->isInitialized = fieldInfo->isInitialised;
    symbol->memberIndex = fieldInfo->memberIndex;
    symbol->hasError = hasError;
  }

  // --- Handle plain identifier assignments ---
  else if (auto ident =
               dynamic_cast<Identifier *>(assignStmt->identifier.get())) {
    assignName = ident->identifier.TokenLiteral;
    symbol = resolveSymbolInfo(assignName);

    if (!symbol) {
      logSemanticErrors("Variable '" + assignName + "' is not declared",
                        ident->identifier.line, ident->identifier.column);
      hasError = true;
      return;
    }
    walker(ident);
  }
  //---Handle dereference identifiers
  else if (auto derefExpr = dynamic_cast<DereferenceExpression *>(
               assignStmt->identifier.get())) {
    walker(derefExpr);
    assignName = extractIdentifierName(derefExpr->identifier.get());
    auto derefMeta = metaData.find(derefExpr);
    if (derefMeta == metaData.end()) {
      logSemanticErrors(
          "Failed to resolve dereference metadata for '" + assignName + "'",
          derefExpr->expression.line, derefExpr->expression.column);
      hasError = true;
      return;
    }

    // THIS is the actual pointee’s symbol info (the target of x)
    symbol = derefMeta->second;

    // Optional sanity check
    if (symbol->isPointer) {
      reportDevBug("Dereference did not unwrap pointer correctly for '" +
                   assignName + "'");
      hasError = true;
    }
  } else if (auto arrAccess =
                 dynamic_cast<ArraySubscript *>(assignStmt->identifier.get())) {
    assignName = arrAccess->identifier->expression.TokenLiteral;
    auto line = arrAccess->expression.line;
    auto col = arrAccess->expression.column;
    symbol = resolveSymbolInfo(assignName);
    if (!symbol) {
      logSemanticErrors("Unidentifed variable '" + assignName + "' ", line,
                        col);
      hasError = true;
      return;
    }
    walker(arrAccess);
    auto accessMeta = metaData.find(arrAccess);
    if (accessMeta == metaData.end()) {
      reportDevBug("Failed to get array access metaData");
      hasError = true;
      return;
    }
    symbol = accessMeta->second;
  } else {
    logSemanticErrors("Invalid assignment target",
                      assignStmt->identifier->expression.line,
                      assignStmt->identifier->expression.column);
    hasError = true;
    return;
  }

  // --- Handle RHS null literal ---
  bool rhsIsNull = false;
  bool isDefinitelyNull = false;
  if (assignStmt->value) {
    auto ident = dynamic_cast<Identifier *>(assignStmt->value.get());

    // Check to prevent assignment of null identifiers
    if (ident) {
      auto identName = ident->identifier.TokenLiteral;
      auto identSym = resolveSymbolInfo(identName);
      if (!identSym) {
        logSemanticErrors("Cannot assign non existant identifier '" +
                              identName + "' to variable '" + assignName + "'",
                          ident->expression.line, ident->expression.column);
        hasError = true;
      }

      if (!identSym->isInitialized) {
        logSemanticErrors("Cannot assign non initialized identifier '" +
                              identName + "' to variable '" + assignName + "'",
                          ident->expression.line, ident->expression.column);
        hasError = true;
      }
      walker(ident);
    } else if (auto arrLit =
                   dynamic_cast<ArrayLiteral *>(assignStmt->value.get())) {
      // Get the array literal size per dimension vector
      auto litSizePerDim = getSizePerDimesion(arrLit);
      auto arrSymbol = resolveSymbolInfo(assignName);
      if (!arrSymbol) {
        logSemanticErrors("Could not find variable '" + assignName + "'",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
      }
      auto arrSizePerDim = arrSymbol->sizePerDimensions;

      if (arrSizePerDim.size() != litSizePerDim.size()) {
        logSemanticErrors("Array variable '" + assignName + "' dimensions " +
                              std::to_string(arrSizePerDim.size()) +
                              " does not match array literal dimensions " +
                              std::to_string(arrSizePerDim.size()),
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
      }
    }
  }

  // Enforcing the assignment usage guard
  bool isAssign = (assignStmt->op.type == TokenType::ASSIGN);
  bool isArrow = (assignStmt->op.type == TokenType::ARROW);

  if (isArrow && !symbol->isPointer) {
    logSemanticErrors("Cannot use '->' for non-pointer variable '" +
                          assignName + "'. Use '=' instead.",
                      assignStmt->op.line, assignStmt->op.column);
    hasError = true;
  }

  if (isAssign && symbol->isPointer) {
    // Special case: If we are assigning to a pointer, we enforce the arrow for
    // clarity
    logSemanticErrors("Cannot use '=' for pointer variable '" + assignName +
                          "'. Use '->' to repoint.",
                      assignStmt->op.line, assignStmt->op.column);
    hasError = true;
  }

  // Check if the value is null and if do give it context
  if (auto nullVal = dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
    rhsIsNull = true;
    isDefinitelyNull = true;

    if (!symbol->isNullable) {
      logSemanticErrors("Cannot assign 'null' to non-nullable variable '" +
                            assignName + "'",
                        assignStmt->identifier->expression.line,
                        assignStmt->identifier->expression.column);

      hasError = true;
      return;
    }

    walker(nullVal);

    metaData[nullVal]->type = symbol->type;

    symbol->isInitialized = true;
    symbol->hasError = hasError;
    symbol->isDefinitelyNull = isDefinitelyNull;
    metaData[assignStmt] = symbol;

    // Nothing else to check
    return;
  }

  // --- Infer RHS type if not null ---
  ResolvedType rhsType = inferNodeDataType(assignStmt->value.get());
  auto lhsType = symbol->type;
  if (symbol->isRef) // If the symbol is a reference strip it
  {
    lhsType = peelRef(lhsType);
  }

  bool typeCheckPassed = false;
  if (lhsType.kind == DataType::OPAQUE) {
    if (rhsIsNull) {
      logSemanticErrors("Cannot assign 'null' to an opaque pointer '" +
                            assignName + "'",
                        assignStmt->op.line, assignStmt->op.column);
      hasError = true;
    } else if (!rhsType.isPointer) {
      logSemanticErrors("Cannot assign non-pointer type '" +
                            rhsType.resolvedName + "' to opaque pointer '" +
                            assignName + "'",
                        assignStmt->op.line, assignStmt->op.column);
      hasError = true;
    }
    typeCheckPassed = true; // We've handled the "Opaque Sink" logic
  }

  if (!typeCheckPassed && !isTypeCompatible(lhsType, rhsType)) {
    logSemanticErrors("Type mismatch: expected '" + symbol->type.resolvedName +
                          "' but got '" + rhsType.resolvedName + "'",
                      assignStmt->identifier->expression.line,
                      assignStmt->identifier->expression.column);
    hasError = true;
  }

  // --- Mutability / const checks ---
  if (symbol->isConstant) {
    logSemanticErrors("Cannot reassign to constant variable '" + assignName +
                          "'",
                      assignStmt->identifier->expression.line,
                      assignStmt->identifier->expression.column);
    hasError = true;
  }

  if (!symbol->isMutable && symbol->isInitialized) {
    logSemanticErrors("Cannot reassign to immutable variable '" + assignName +
                          "'",
                      assignStmt->identifier->expression.line,
                      assignStmt->identifier->expression.column);
    hasError = true;
  }

  // --- Prevent assigning a nullable value to a non-nullable variable ---
  if (!symbol->isNullable) // LHS is non-nullable
  {
    bool rhsDefinitelyNull = false;

    if (assignStmt->value) {
      if (auto nullVal = dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
        rhsDefinitelyNull = true; // literal null
      } else if (auto ident =
                     dynamic_cast<Identifier *>(assignStmt->value.get())) {
        auto identSym = resolveSymbolInfo(ident->identifier.TokenLiteral);
        if (identSym && identSym->isDefinitelyNull) {
          rhsDefinitelyNull = true; // identifier that is definitely null
        }
      }
    }

    if (rhsDefinitelyNull) {
      logSemanticErrors(
          "Cannot assign a null value to to a non-nullable variable '" +
              assignName + "'",
          assignStmt->identifier->expression.line,
          assignStmt->identifier->expression.column);
      hasError = true;
    }
  }

  // If the symbol is a pointer
  if (symbol->isPointer) {
    if (auto ident = dynamic_cast<Identifier *>(assignStmt->value.get())) {
      auto identName = ident->identifier.TokenLiteral;
      auto identSym = resolveSymbolInfo(identName);
      // Pointer checks
      if (!identSym->isPointer) // The identifier itself must be a pointer
      {
        logSemanticErrors("Cannot reassign a non pointer '" + identName +
                              "' to pointer variable '" + assignName + "'",
                          ident->expression.line, ident->expression.column);
        hasError = true;
      }
      // Get the target symbol
      auto targetSym = identSym->targetSymbol;
      if (!targetSym) {
        logSemanticErrors("No target symbol for '" + identName +
                              "' being reassigned to variable '" + assignName +
                              "'",
                          ident->expression.line, ident->expression.column);
        hasError = true;
        return; // This is a critical error leaving it will cause derefencing of
                // nullptrs
      }

      // Check the target symbol scope and block it if it is local
      if (targetSym->storage == StorageType::STACK) {
        // The compiler should complain
        logSemanticErrors("Cannot reassign local pointer '" + identName +
                              "' to pointer variable '" + assignName + "'",
                          ident->expression.line, ident->expression.column);
        hasError = true;
      }
    } else if (auto addr =
                   dynamic_cast<AddressExpression *>(assignStmt->value.get())) {
      // Get the address name
      auto addrName = addr->identifier->expression.TokenLiteral;
      auto line = addr->expression.line;
      auto col = addr->expression.column;
      // Get the symbolInfo
      auto addrSym = resolveSymbolInfo(addrName);
      if (!addrSym) {
        logSemanticErrors("Unidentified address pointer '" + addrName +
                              "' cannot reassign to pointer variable '" +
                              assignName + "'",
                          line, col);
        hasError = true;
      }
      // Get the storage type
      if (addrSym->storage == StorageType::STACK) {
        logSemanticErrors("Cannot reassign local address pointer '" + addrName +
                              "' to pointer variable '" + assignName + "'",
                          line, col); // Complain
        hasError = true;
      }
    } else if (auto call =
                   dynamic_cast<CallExpression *>(assignStmt->value.get())) {
      // Get the function name
      auto funcName = call->function_identifier->expression.TokenLiteral;
      auto line = call->function_identifier->expression.line;
      auto col = call->function_identifier->expression.column;

      // Get the symbol info
      auto callSym = resolveSymbolInfo(funcName);
      if (!callSym) {
        hasError = true;
      }
      // Ensure it is a pointer
      if (!callSym->returnType.isPointer) {
        logSemanticErrors("Cannot reassign a non pointer call '" + funcName +
                              "' to pointer variable '" + assignName + "'",
                          line, col);
        hasError = true;
      }
    } else {
      logSemanticErrors(
          "Must only reassign address to pointer or a pointer to '" +
              assignName + "'",
          assignStmt->identifier->expression.line,
          assignStmt->identifier->expression.column);
      hasError = true;
    }
  }

  // --- Mark variable initialized ---
  symbol->isInitialized = true;
  symbol->isDefinitelyNull = isDefinitelyNull;
  symbol->hasError = hasError;

  // --- Walk the RHS expression ---
  if (assignStmt->value)
    walker(assignStmt->value.get());

  // --- Store metadata for later stages ---
  metaData[assignStmt] = symbol;
}

void Semantics::walkFieldAssignmentStatement(Node *node) {
  auto fieldAssignStmt = dynamic_cast<FieldAssignment *>(node);
  if (!fieldAssignStmt)
    return;

  auto name = extractIdentifierName(fieldAssignStmt->lhs_chain.get());
  auto line = fieldAssignStmt->statement.line;
  auto column = fieldAssignStmt->statement.column;

  walker(fieldAssignStmt->lhs_chain.get());

  auto lhsInfo = metaData[fieldAssignStmt->lhs_chain.get()];
  if (!lhsInfo || lhsInfo->hasError) {
    return;
  }

  if (lhsInfo->isConstant) {
    logSemanticErrors("Cannot reassign to constant field '" + name + "'", line,
                      column);
    return;
  }

  if (!lhsInfo->isMutable && lhsInfo->isInitialized) {
    logSemanticErrors("Cannot reassign to immutable field '" + name + "'", line,
                      column);
    return;
  }

  // Enforce the assignment usage or arrow usage
  bool isAssign = (fieldAssignStmt->op.type == TokenType::ASSIGN);
  bool isArrow = (fieldAssignStmt->op.type == TokenType::ARROW);

  if (isArrow && !lhsInfo->isPointer) {
    logSemanticErrors("Cannot use '->' for non-pointer variable '" + name +
                          "'. Use '=' instead.",
                      fieldAssignStmt->op.line, fieldAssignStmt->op.column);
    return;
  }

  if (isAssign && lhsInfo->isPointer) {
    // Special case: If we are assigning to a pointer, we enforce the arrow for
    // clarity
    logSemanticErrors("Cannot use '=' for pointer variable '" + name +
                          "'. Use '->' to repoint.",
                      fieldAssignStmt->op.line, fieldAssignStmt->op.column);
    return;
  }

  if (fieldAssignStmt->value) {
    walker(fieldAssignStmt->value.get());
    auto rhsInfo = metaData[fieldAssignStmt->value.get()];

    if (rhsInfo) {
      if (lhsInfo->type.kind == DataType::OPAQUE) {
        if (!rhsInfo->type.isPointer) {
          logSemanticErrors("Cannot reassign non pointer type '" +
                                rhsInfo->type.resolvedName +
                                "' to an opaque pointer",
                            line, column);
        }
      } else if (!isTypeCompatible(lhsInfo->type, rhsInfo->type)) {
        logSemanticErrors("Type mismatch in assignment", line, column);
      }
    }
  }

  metaData[fieldAssignStmt] = lhsInfo;
}
