#include "ast.hpp"
#include "semantics.hpp"

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

  // Recursively getting the data type of the contents inside the array and also
  // calling the walkers
  std::vector<ResolvedType> itemTypes;

  ArrayTypeInfo arrayTypeInfo;
  int dimensionCount = 1; // The array literal is already a dimension

  for (const auto &item : arrLit->array) {
    if (auto memArr = dynamic_cast<ArrayLiteral *>(item.get())) {
      dimensionCount++;
    }
    walker(item.get()); // Calling their walkers
    auto itemType = inferNodeDataType(item.get());
    arrayTypeInfo.underLyingType = itemType;
    arrayTypeInfo.dimensions = dimensionCount;
    itemTypes.push_back(itemType);
  }

  ResolvedType arrType = inferNodeDataType(arrLit);

  // Storing metaData about the array
  auto arrInfo = std::make_shared<SymbolInfo>();

  arrInfo->type = arrType;
  arrInfo->isNullable = false;
  arrInfo->isMutable = false;
  arrInfo->isConstant = false;
  arrInfo->arrayTyInfo = arrayTypeInfo;

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

  walker(arrExpr->identifier.get());

  // Get the full array type
  ResolvedType arrType = arrSymbol->type;
  std::cout << "Array type: " << arrType.resolvedName << "\n";

  // Check indices
  int indexLevel = 0;
  for (auto &idxNode : arrExpr->index_exprs) {
    walker(idxNode.get());
    ResolvedType idxType = inferNodeDataType(idxNode.get());
    if (idxType.kind != DataType::I32) {
      logSemanticErrors("Array index must be of type i32",
                        idxNode->expression.line, idxNode->expression.column);
      hasError = true;
    }

    if (!arrType.isArray) {
      logSemanticErrors("Too many indices for array '" + arrName + "'",
                        idxNode->expression.line, idxNode->expression.column);
      hasError = true;
      break;
    }

    if (!arrType.innerType) {
      logSemanticErrors("Array type missing inner type.",
                        idxNode->expression.line, idxNode->expression.column);
      hasError = true;
      break;
    }

    arrType = *arrType.innerType; // Peel one layer
    indexLevel++;
  }

  // Optionally store info for further analysis
  auto arrAccessInfo = std::make_shared<SymbolInfo>();
  arrAccessInfo->type = arrType; // type after peeling
  arrAccessInfo->hasError = hasError;
  arrAccessInfo->isHeap = arrSymbol->isHeap;
  arrAccessInfo->isDheap = arrSymbol->isDheap;
  arrAccessInfo->arrayTyInfo = arrSymbol->arrayTyInfo;

  metaData[arrExpr] = arrAccessInfo;
}

// Walking the identifier expression
void Semantics::walkIdentifierExpression(Node *node) {
  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr)
    return;
  std::cout << "[SEMANTIC LOG] Analyzing identifier node: "
            << identExpr->toString() << "\n";
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

  auto addrName = addrExpr->identifier->expression.TokenLiteral;
  auto line = addrExpr->identifier->expression.line;
  auto col = addrExpr->identifier->expression.column;

  walker(addrExpr->identifier.get());
  auto symbolInfo = resolveSymbolInfo(addrName);

  if (!symbolInfo) {
    logSemanticErrors("Unidentified variable '" + addrName + "'", line, col);
    return;
  }

  bool hasError = false;
  bool isHeap = symbolInfo->isHeap;
  bool isDheap = symbolInfo->isDheap;

  if (!symbolInfo) {
    logSemanticErrors("Use of undeclared identifier '" + addrName + "'", line,
                      col);
    return;
  }

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

  auto derefInfo = std::make_shared<SymbolInfo>();

  if (dynamic_cast<DereferenceExpression *>(innerNode)) {
    derefInfo->type = derefSym->type;
  } else {
    derefInfo->type = derefSym->targetSymbol->type;
    derefInfo->lastUseNode = derefExpr;
  }

  if (derefSym->isHeap || derefSym->isDheap)
    derefSym->lastUseNode = derefExpr;

  auto derefTargetSym = derefSym->targetSymbol;
  derefTargetSym->lastUseNode = derefExpr;

  ResolvedType peeledType = derefSym->type;
  peeledType.isPointer = false;
  ResolvedType finalType = isPointerType(peeledType);

  derefInfo->isPointer = false; // Just a sanity measure
  derefInfo->isMutable = derefSym->isMutable;
  derefInfo->isConstant = derefSym->isConstant;
  derefInfo->isInitialized = derefSym->isInitialized;
  derefInfo->derefPtrType = derefSym->type;
  derefInfo->type = finalType;
  derefInfo->targetSymbol = derefTargetSym;
  derefInfo->isHeap = derefSym->isHeap;
  derefInfo->isDheap = derefSym->isDheap;

  std::cout << "DEREF PTR TYPE: " << derefSym->type.resolvedName << "\n";

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
    std::cout << "SELF assignment has been triggered\n";

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
    assignName = derefExpr->identifier->expression.TokenLiteral;
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
      logSemanticErrors("Dereference did not unwrap pointer correctly for '" +
                            assignName + "'",
                        derefExpr->expression.line,
                        derefExpr->expression.column);
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
      logSemanticErrors("Failed to get array access metaData", line, col);
      hasError = true;
      return;
    }
    symbol = accessMeta->second;
  }

  else {
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
    } else if (auto nullVal =
                   dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
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
      std::cout << "[SEMANTIC TAG] Tagging Null at: "
                << static_cast<void *>(nullVal)
                << " with type: " << symbol->type.resolvedName << "\n";
      metaData[nullVal]->type = symbol->type;

      symbol->isInitialized = true;
      symbol->hasError = hasError;
      symbol->isDefinitelyNull = isDefinitelyNull;
      metaData[assignStmt] = symbol;

      // Nothing else to check
      return;
    } else if (auto arrLit =
                   dynamic_cast<ArrayLiteral *>(assignStmt->value.get())) {
      // Getting the length of the array literal
      auto arrLitTypeInfo = getArrayTypeInfo(arrLit);
      // Getting the arrayMeta for the symbol
      auto arrSymbol = resolveSymbolInfo(assignName);
      if (!arrSymbol) {
        logSemanticErrors("Could not find variable '" + assignName + "'",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
      }

      auto assignMeta = arrSymbol->arrayTyInfo;
      // Comapring the lengths
      if (arrLitTypeInfo.dimensions != assignMeta.dimensions) {
        logSemanticErrors("Array variable '" + assignName + "' dimensions " +
                              std::to_string(assignMeta.dimensions) +
                              " does not match array literal dimensions " +
                              std::to_string(arrLitTypeInfo.dimensions),
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
      }
    }
  }

  // --- Infer RHS type if not null ---
  ResolvedType rhsType = inferNodeDataType(assignStmt->value.get());
  auto lhsType = symbol->type;
  if (symbol->isRef) // If the symbol is a reference strip it
  {
    lhsType = peelRef(lhsType);
  }

  if (!isTypeCompatible(lhsType, rhsType)) {
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

  auto fieldName = fieldAssignStmt->assignment_token.TokenLiteral;
  auto line = fieldAssignStmt->statement.line;
  auto column = fieldAssignStmt->statement.column;
  bool hasError = false;

  // Split into "parent.child"
  auto [parentName, childName] = splitScopedName(fieldName);
  std::cout << "PARENT NAME:" << parentName << "\n";
  std::cout << "CHILD NAME:" << childName << "\n";

  // Resolve the parent symbol (must be a variable in scope)
  auto parentSymbol = resolveSymbolInfo(parentName);
  if (!parentSymbol) {
    logSemanticErrors("Variable '" + parentName + "' does not exist", line,
                      column);
    hasError = true;
    return;
  }

  // Get the parent's type
  std::string parentType = parentSymbol->type.resolvedName;
  std::string lookUpName =
      stripPtrSuffix(parentType); // Strip the _ptr suffix so that the semantics
                                  // can search with the original name
  std::cout << "SEMANTIC LOG: Parent type: " << lookUpName << "\n";

  // Look up that type in the custom types table
  auto parentIt = customTypesTable.find(lookUpName);
  if (parentIt == customTypesTable.end()) {
    logSemanticErrors("Type '" + lookUpName + "' does not exist", line, column);
    hasError = true;
    return;
  }

  // Check if the childName exists in that type’s members
  auto members = parentIt->second->members;
  auto memberIt = members.find(childName);
  if (memberIt == members.end()) {
    logSemanticErrors("Variable '" + childName + "' does not exist in type '" +
                          parentType + "'",
                      line, column);
    hasError = true;
    return;
  }

  // Grab field properties
  ResolvedType type = memberIt->second->type;
  bool isNullable = memberIt->second->isNullable;
  bool isMutable = memberIt->second->isMutable;
  bool isConstant = memberIt->second->isConstant;
  bool isHeap = memberIt->second->isHeap;
  bool isInitialized = memberIt->second->isInitialised;

  // Constant/immutability checks
  if (isConstant) {
    logSemanticErrors("Cannot reassign to constant variable '" + fieldName +
                          "'",
                      line, column);
    hasError = true;
    return;
  }
  if (!isMutable && isInitialized) {
    logSemanticErrors("Cannot reassign to immutable variable '" + fieldName +
                          "'",
                      line, column);
    hasError = true;
    return;
  }

  // Mark initialized (since this is assignment)
  isInitialized = true;

  // Analyse the value if present
  if (fieldAssignStmt->value) {
    walker(fieldAssignStmt->value.get());
  }

  if (memberIt->second->isHeap) {
    memberIt->second->lastUseNode = fieldAssignStmt;
  }

  // Symbol info for the field
  auto fieldInfo = std::make_shared<SymbolInfo>();
  fieldInfo->type = type;
  fieldInfo->isNullable = isNullable;
  fieldInfo->isConstant = isConstant;
  fieldInfo->isInitialized = isInitialized;
  fieldInfo->hasError = hasError;
  fieldInfo->isHeap = isHeap;

  // Build metadata for this assignment node
  auto info = std::make_shared<SymbolInfo>();
  info->type = type; // The whole node still evaluates to the member type
  info->baseSymbol = parentSymbol;
  info->fieldSymbol = fieldInfo;
  info->isNullable = isNullable;
  info->isConstant = isConstant;
  info->isInitialized = isInitialized;
  info->hasError = hasError;

  metaData[fieldAssignStmt] = info;
}
