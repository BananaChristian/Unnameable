#include "ast.hpp"
#include "semantics.hpp"

//_____________Array Statement__________________
void Semantics::walkArrayStatement(Node *node) {
  auto arrStmt = dynamic_cast<ArrayStatement *>(node);
  if (!arrStmt)
    return;

  // --- Mutability flags ---
  bool isMutable = arrStmt->mutability == Mutability::MUTABLE;
  bool isConstant = arrStmt->mutability == Mutability::CONSTANT;

  const auto &arrayName = arrStmt->identifier->expression.TokenLiteral;
  int arrNameLine = arrStmt->identifier->expression.line;
  int arrNameCol = arrStmt->identifier->expression.column;

  if (resolveSymbolInfo(arrayName)) {
    logSemanticErrors("Array name '" + arrayName + "' already exists",
                      arrNameLine, arrNameCol);
    return;
  }

  bool hasError = false;
  bool isInitialized = false;
  bool isHeap = arrStmt->isHeap;
  bool isDheap = arrStmt->isDheap;
  ArrayTypeInfo arrTypeInfo;

  // --- Base type inference ---
  ResolvedType baseType = inferNodeDataType(arrStmt->arrayType.get());
  ResolvedType arrayType;
  // Get the array declaration dimensions
  int arrStmtDimensions = 0;
  // If the array dimesions are empty infer from the array literal but only if
  // the array literal exists
  if (arrStmt->lengths.empty()) {
    if (!arrStmt->array_content) {
      logSemanticErrors("Cannot infer dimension count if you do not initialize "
                        "array declaration '" +
                            arrayName + "'",
                        arrNameLine, arrNameCol);
      return;
    } else {
      auto literalType = inferNodeDataType(arrStmt->array_content.get());
      arrStmtDimensions =
          getArrayTypeInfo(arrStmt->array_content.get()).dimensions;
      arrayType = literalType;
    }
  } else {
    arrStmtDimensions = arrStmt->lengths.size();
    arrayType = makeArrayType(baseType, arrStmtDimensions);
  }

  std::cout << "BASE TYPE: " << baseType.resolvedName << "\n";
  std::cout << "ARRAY TYPE: " << arrayType.resolvedName << "\n";

  arrTypeInfo.underLyingType = baseType;

  // --- Walk length expressions ---
  for (const auto &len : arrStmt->lengths) {
    walker(len.get());

    int64_t evaluatedLen = evaluateArrayLengthConstant(len.get());
    if (!isDheap && evaluatedLen <= 0) {
      logSemanticErrors("Array dimension length must be greater than 0",
                        len->expression.line, len->expression.column);
      hasError = true;
    }

    arrTypeInfo.sizePerDimension.push_back(evaluatedLen);
  }

  // --- Handle initializer ---
  if (arrStmt->array_content) {
    walker(arrStmt->array_content.get());
    isInitialized = true;

    ArrayLiteral *arrLit =
        dynamic_cast<ArrayLiteral *>(arrStmt->array_content.get());
    if (!arrLit) {
      std::cerr << "Only use array literals for array statements\n";
      hasError = true;
      return;
    }

    ResolvedType literalType = inferNodeDataType(arrLit);

    // Retrieve the array literal dimensions
    int arrLitDimensions = getArrayTypeInfo(arrLit).dimensions;

    // Dimension check
    if (arrStmtDimensions != arrLitDimensions) {
      logSemanticErrors("Dimensions mismatch in array declaration '" +
                            arrayName + "' expected '" +
                            std::to_string(arrStmtDimensions) + "' but got '" +
                            std::to_string(arrLitDimensions) + "'",
                        arrNameLine, arrNameCol);
      hasError = true;
    }

    if (!isTypeCompatible(arrayType, literalType)) {
      logSemanticErrors("Declared type '" + arrayType.resolvedName +
                            "' does not match the initialized type '" +
                            literalType.resolvedName + "'",
                        arrLit->expression.line, arrLit->expression.column);
      hasError = true;
    }

    // Use a recursive lambda to check every nested dimension
    if (!isDheap) {
      std::function<void(ArrayLiteral *, int)> verifyRecursive;
      verifyRecursive = [&](ArrayLiteral *lit, int dimIdx) {
        if (dimIdx >= arrTypeInfo.sizePerDimension.size())
          return;

        // Use sizePerDimension vector to check against the current literal
        // elements
        size_t expected =
            static_cast<size_t>(arrTypeInfo.sizePerDimension[dimIdx]);
        size_t actual = lit->array.size();

        if (expected != actual) {
          logSemanticErrors("Dimension length mismatch, expected " +
                                std::to_string(expected) +
                                " elements but got " + std::to_string(actual),
                            lit->expression.line, lit->expression.column);
          hasError = true;
          return;
        }

        // If there is a dimension below this one, we must keep checking
        if (dimIdx + 1 < arrTypeInfo.sizePerDimension.size()) {
          for (auto &element : lit->array) {
            auto subLit = dynamic_cast<ArrayLiteral *>(element.get());
            if (subLit) {
              verifyRecursive(subLit, dimIdx + 1);
            } else {
              logSemanticErrors("Expected nested array literal for dimension " +
                                    std::to_string(dimIdx + 1),
                                element->expression.line,
                                element->expression.column);
              hasError = true;
              return;
            }
          }
        }
      };

      // Start the check at the top level (index 0)
      verifyRecursive(arrLit, 0);
    }
  } else if (isConstant) {
    logSemanticErrors("Constant array '" + arrayName + "' must be initialized",
                      arrStmt->statement.line, arrStmt->statement.column);
    hasError = true;
  } else if (arrStmt->lengths.empty()) {
    logSemanticErrors("Uninitialized array '" + arrayName +
                          "' is missing length declarations",
                      arrStmt->statement.line, arrStmt->statement.column);
    hasError = true;
  }

  arrTypeInfo.dimensions = arrStmtDimensions;

  if (isHeap) {
    for (int64_t len : arrTypeInfo.sizePerDimension) {
      if (len <= 0) {
        logSemanticErrors("heap arrays must have constant literal lengths > 0.",
                          arrNameLine, arrNameCol);
        hasError = true;
        break;
      }
    }
  }

  // --- Store symbol info ---
  auto arrInfo = std::make_shared<SymbolInfo>();
  arrInfo->isMutable = isMutable;
  arrInfo->isConstant = isConstant;
  arrInfo->arrayTyInfo = arrTypeInfo;
  arrInfo->hasError = hasError;
  arrInfo->isInitialized = isInitialized;
  arrInfo->isHeap = isHeap;
  arrInfo->isDheap = isDheap;
  arrInfo->lastUseNode = arrStmt;
  arrInfo->type = arrayType; // store fully wrapped type
  arrInfo->arrayTyInfo = arrTypeInfo;
  if (!loopContext.empty() && loopContext.back()) {
    arrInfo->needsPostLoopFree = true;
    arrInfo->bornInLoop = true;
  }

  metaData[arrStmt] = arrInfo;
  symbolTable.back()[arrayName] = arrInfo;

  std::cout << "FINAL ARRAY STATEMENT TYPE '" << arrayType.resolvedName
            << "'\n";
}

//_________________Pointer Statement___________________
void Semantics::walkPointerStatement(Node *node) {
  auto ptrStmt = dynamic_cast<PointerStatement *>(node);

  if (!ptrStmt)
    return;

  auto ptrName = ptrStmt->name->expression.TokenLiteral;
  auto line = ptrStmt->name->expression.line;
  auto col = ptrStmt->name->expression.column;
  bool hasError = false;
  bool isMutable = false;
  bool isConstant = false;
  bool isHeap = ptrStmt->isHeap;
  bool isDheap = ptrStmt->isDheap;

  ResolvedType ptrType = ResolvedType{DataType::UNKNOWN, "unknown"};

  // If we dont have the value(This is only allowed inside records and components for now)
  if (!ptrStmt->value) {
    if (insideRecord||insideComponent) {
      if (ptrStmt->type) {
        ptrType = inferNodeDataType(ptrStmt);
      } else {
        logSemanticErrors("Cannot get pointer type if you did not provide it "
                          "and did not provide a value for inference",
                          line, col);
      }
      auto ptrInfo = std::make_shared<SymbolInfo>();
      ptrInfo->type = ptrType;
      ptrInfo->isPointer = true;
      ptrInfo->isInitialized = false;

      metaData[ptrStmt] = ptrInfo;
      symbolTable.back()[ptrName] = ptrInfo;
      return;
    } else {
      logSemanticErrors("Must initialize the pointer '" + ptrName + "'", line,
                        col);
      return;
    }
  }

  // Infer the type of the pointer value
  ResolvedType targetType = inferNodeDataType(ptrStmt->value.get());

  // Check if the target is a pointer
  if (!targetType.isPointer) {
    logSemanticErrors("Must initialize the pointer '" + ptrName +
                          "' with a pointer value",
                      line, col);
    hasError = true;
    return;
  }

  // Dealing with what is being pointed to
  std::string targetName = extractIdentifierName(ptrStmt->value.get());

  // Walking the target
  walker(ptrStmt->value.get());

  auto targetSymbol = metaData[ptrStmt->value.get()];
  if (!targetSymbol) {
    logSemanticErrors("Pointer '" + ptrName +
                          "'is pointing to an undeclared variable '" +
                          targetName + "'",
                      line, col);
    hasError = true;
    return;
  }

  // What if the user didnt include the type (We infer for them)
  if (!ptrStmt->type)
    ptrType = targetType;
  else // If the user actually included a type we verify it
  {
    ptrType = inferNodeDataType(ptrStmt);
    // Check if the types are compatible
    // I am comparing with the resolved name since it is sure to either be
    // type_ptr comparing with datatypes purely can allow bugs in the type
    // system
    if (ptrType.resolvedName != targetType.resolvedName) {
      logSemanticErrors("Type mismatch pointer '" + ptrName + "' of type '" +
                            ptrType.resolvedName + "' does not match '" +
                            targetName + "' of type '" +
                            targetType.resolvedName + "'",
                        line, col);
      hasError = true;
    }
  }

  // Guard against local pointing
  auto targetStorage = targetSymbol->storage;
  if (auto ptrValue = dynamic_cast<AddressExpression *>(ptrStmt->value.get())) {
    if (targetStorage == StorageType::STACK) {

      logSemanticErrors("Pointer '" + ptrName + "' cannot point to '" +
                            targetName + "' because it is local",
                        line, col);
      hasError = true;
    }
  } else if (auto ptrValue = dynamic_cast<Identifier *>(ptrStmt->value.get())) {
    auto pointeeTargetSym = targetSymbol->targetSymbol;
    if (!pointeeTargetSym) {
      logSemanticErrors("Pointer '" + ptrName + "'s target '" + targetName +
                            "' lacks targetInfo",
                        line, col);
      return;
    }
    if (pointeeTargetSym->storage == StorageType::STACK) {
      logSemanticErrors("Pointer '" + ptrName + "' cannot point to '" +
                            targetName +
                            "' because it also points to local variable",
                        line, col);
      hasError = true;
    }
  }

  /*
  Checking for mutability (It should be noted that by default the mutability
  is... well immutable That means if the user didnt add it we are dealing with
  an immutable pointer
  */

  if (ptrStmt->mutability == Mutability::MUTABLE) {
    std::cout << "POINTER IS MUTABLE\n";
    isMutable = true;
  } else if (ptrStmt->mutability == Mutability::CONSTANT) {
    std::cout << "POINTER IS CONSTANT\n";
    isConstant = true;
  }

  std::cout << "POINTER TYPE: " << ptrType.resolvedName << "\n";
  int ptrPopCount = 0;
  // Pointer's storage info (The pointer itself not the target)
  StorageType pointerStorage;
  if (isGlobalScope()) {
    pointerStorage = StorageType::GLOBAL;
  } else if (isHeap) {
    pointerStorage = StorageType::HEAP;
    ptrPopCount = targetSymbol->popCount + 1;
    std::cout << "[SEMANTICS DEBUG] Pointer '" << ptrName << "' points to '"
              << targetName
              << "' | target->popCount: " << targetSymbol->popCount
              << " | new ptr->popCount: " << ptrPopCount << "\n";
  } else if (isDheap) {
    pointerStorage = StorageType::HEAP;
  } else {
    pointerStorage = StorageType::STACK;
  }

  auto ptrInfo = std::make_shared<SymbolInfo>();
  ptrInfo->isHeap = isHeap;
  ptrInfo->isDheap = isDheap;
  ptrInfo->lastUseNode = ptrStmt;
  ptrInfo->type = ptrType;
  ptrInfo->hasError = hasError;
  ptrInfo->isPointer = true;
  ptrInfo->targetSymbol = targetSymbol;
  ptrInfo->isMutable = isMutable;
  ptrInfo->isConstant = isConstant;
  ptrInfo->storage = pointerStorage;
  ptrInfo->popCount = ptrPopCount;
  ptrInfo->isInitialized = true;

  metaData[ptrStmt] = ptrInfo;
  symbolTable.back()[ptrName] = ptrInfo;
}

//____________Let statement walker______________
void Semantics::walkLetStatement(Node *node) {
  auto letStmt = dynamic_cast<LetStatement *>(node);
  if (!letStmt)
    return;

  std::cout << "[SEMANTIC LOG]: Analyzing let statement node\n";

  auto type = dynamic_cast<BasicType *>(letStmt->type.get());

  // --- Initial flags ---
  bool isNullable = type->isNullable;
  bool isHeap = letStmt->isHeap;
  bool isDheap = letStmt->isDheap;
  bool isDefinitelyNull = false;
  bool isInitialized = false;
  bool hasError = false;
  bool isExportable = letStmt->isExportable;
  int popCount = 0;

  // Checking if the name is being reused
  auto letName = letStmt->ident_token.TokenLiteral;

  auto existingSym = resolveSymbolInfo(letName);
  auto localSym = lookUpInCurrentScope(letName);

  if (localSym) {
    logSemanticErrors("Variable with name '" + letName +
                          "' already exists in the same scope",
                      letStmt->ident_token.line, letStmt->ident_token.column);
    hasError = true;
    return;
  }

  if (existingSym) {
    if (existingSym->isFunction) {
      logSemanticErrors("Local variable '" + letName +
                            "' shadows existing global function",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
      return;
    } else if (existingSym->isComponent) {
      logSemanticErrors("Local variable '" + letName +
                            "' shadows existing component",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
      return;
    } else if (existingSym->isBehavior) {
      logSemanticErrors("Local variable '" + letName +
                            "' shadows existing  behavior block",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
      return;
    } else if (existingSym->isDataBlock) {
      logSemanticErrors("Local variable '" + letName +
                            "' shadows existing data block",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
      return;
    }
  }

  auto letStmtValue = letStmt->value.get();

  // --- Resolve type from token ---
  ResolvedType expectedType = inferNodeDataType(letStmt->type.get());
  ResolvedType declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};

  // --- Mutability & constants ---
  bool isMutable = (letStmt->mutability == Mutability::MUTABLE);
  bool isConstant = (letStmt->mutability == Mutability::CONSTANT);

  // Export check
  if (isExportable && !isGlobalScope()) {
    logSemanticErrors("The 'export' keyword can only be used on global "
                      "constant declarations. "
                      "Local variable '" +
                          letName + "' cannot be exported.",
                      letStmt->ident_token.line, letStmt->ident_token.column);
    hasError = true;
  }

  if (isExportable && !isConstant) {
    logSemanticErrors("The 'export' keyword can only be used on global "
                      "constant declarations. "
                      "Non const variable '" +
                          letName + "' cannot be exported.",
                      letStmt->ident_token.line, letStmt->ident_token.column);
    hasError = true;
  }

  // Constant declaration checks
  if (isConstant && !letStmtValue) {
    logSemanticErrors("Need to assign a value to a constant variable '" +
                          letName + "'",
                      letStmt->ident_token.line, letStmt->ident_token.column);
    hasError = true;
  }

  if (isConstant && !isConstLiteral(letStmtValue)) {
    logSemanticErrors("Constant variable'" + letName +
                          "' must be initialized with a raw literal, not an "
                          "runtime expression",
                      letStmtValue->expression.line,
                      letStmtValue->expression.column);
    hasError = true;
  }

  if (isConstant && isNullable) {
    logSemanticErrors("Constant variable '" + letName +
                          "' cannot be nullable. " +
                          "Constants must hold a concrete literal value.",
                      letStmt->ident_token.line, letStmt->ident_token.column);
    hasError = true;
  }

  if (isGlobalScope()) {
    // Force Const for stack variables
    if (!isConstant && !isDheap && !isHeap) {
      logSemanticErrors("Global stack variable '" + letName +
                            "' must be declared as 'const'",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
    }

    // Strict Literal Check for Const
    if (isConstant && !isConstLiteral(letStmtValue)) {
      logSemanticErrors("Global constant '" + letName +
                            "' must be initialized with a raw literal, not a "
                            " runtime expression",
                        letStmtValue->expression.line,
                        letStmtValue->expression.column);
      hasError = true;
    }
  }

  // --- Walk value & infer type ---
  if (letStmtValue) {
    declaredType = expectedType;

    walker(letStmtValue);
    isInitialized = true;

    if (auto nullVal = dynamic_cast<NullLiteral *>(letStmtValue)) {
      isDefinitelyNull = true;
      if (!isNullable) {
        logSemanticErrors("Cannot assign 'null' to non-nullable variable '" +
                              letStmt->ident_token.TokenLiteral + "'",
                          type->data_token.line, type->data_token.column);
        hasError = true;
        declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};
      } else {
        declaredType = expectedType;
        metaData[nullVal]->type = expectedType;
      }
    } else if (auto ident = dynamic_cast<Identifier *>(letStmtValue)) {
      auto identSym = resolveSymbolInfo(ident->identifier.TokenLiteral);
      if (!identSym) {
        logSemanticErrors("Cannot assign non-existent identifier '" +
                              ident->identifier.TokenLiteral + "'",
                          ident->expression.line, ident->expression.column);
        hasError = true;
      } else if (!identSym->isInitialized) {
        logSemanticErrors("Cannot assign non-initialized identifier '" +
                              ident->identifier.TokenLiteral + "'",
                          ident->expression.line, ident->expression.column);
        hasError = true;
      }
    } else {
      ResolvedType valueType = inferNodeDataType(letStmtValue);
      if (type->data_token.type == TokenType::AUTO) {
        // If it's auto, the expected type BECOMES the value type
        expectedType = valueType;
        declaredType = valueType;
      } else {
        declaredType = expectedType;
        if (!isTypeCompatible(declaredType, valueType)) {
          logSemanticErrors("Type mismatch: cannot assign " +
                                valueType.resolvedName + " to " +
                                declaredType.resolvedName,
                            type->data_token.line, type->data_token.column);
          hasError = true;
        }
      }
    }
  } else {
    declaredType = expectedType;
  }

  // --- Type mismatch checks ---
  if (type->data_token.type != TokenType::AUTO) {
    if (!isTypeCompatible(expectedType, declaredType)) {
      logSemanticErrors("Type mismatch in variable declaration statement '" +
                            letName + "' expected '" +
                            expectedType.resolvedName + "' but got '" +
                            declaredType.resolvedName + "'",
                        type->data_token.line, type->data_token.column);
      hasError = true;
    }

    if (!isNullable && isDefinitelyNull) {
      logSemanticErrors("Cannot assign a nullable (possibly null) value to "
                        "non-nullable variable '" +
                            letStmt->ident_token.TokenLiteral + "'",
                        type->data_token.line, type->data_token.column);
      hasError = true;
      declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};
    }
  }

  // --- Constant + nullable/heap checks ---
  if (isConstant && (isNullable || isDefinitelyNull)) {
    logSemanticErrors("Cannot use null on a constant variable '" +
                          letStmt->ident_token.TokenLiteral + "'",
                      letStmt->ident_token.line, letStmt->ident_token.column);
    hasError = true;
  }

  // Heap and Dheap checks
  if (isHeap || isDheap) {
    if (!isInitialized) {
      logSemanticErrors("Cannot promote uninitialized variable '" +
                            letStmt->ident_token.TokenLiteral + "' to the heap",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
    }

    if (isNullable || isDefinitelyNull) {
      logSemanticErrors("Cannot promote nullable variable '" +
                            letStmt->ident_token.TokenLiteral + "' to the heap",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
    }

    if (type->data_token.type == TokenType::AUTO) {
      logSemanticErrors("Cannot promote auto variable '" +
                            letStmt->ident_token.TokenLiteral +
                            "' to the heap, please explicitly use its type",
                        letStmt->ident_token.line, letStmt->ident_token.column);
      hasError = true;
    }
  }

  StorageType letStorageType;
  // Storing the scope information
  if (isGlobalScope()) {
    letStorageType = StorageType::GLOBAL;
  } else if (isHeap) {
    letStorageType = StorageType::HEAP;
    popCount = 1;
  } else if (isDheap) {
    letStorageType = StorageType::HEAP;
  } else {
    letStorageType = StorageType::STACK;
  }

  // --- Metadata & symbol registration ---
  auto letInfo = std::make_shared<SymbolInfo>();
  letInfo->type = declaredType;
  letInfo->isNullable = isNullable;
  letInfo->isMutable = isMutable;
  letInfo->isConstant = isConstant;
  letInfo->isInitialized = isInitialized;
  letInfo->isDefinitelyNull = isDefinitelyNull;
  letInfo->isHeap = isHeap;
  letInfo->isDheap = isDheap;
  letInfo->popCount = popCount;
  letInfo->lastUseNode = letStmt;
  letInfo->storage = letStorageType;
  letInfo->isExportable = isExportable;
  letInfo->hasError = hasError;
  if (!loopContext.empty() && loopContext.back()) {
    letInfo->needsPostLoopFree = true;
    letInfo->bornInLoop = true;
  }

  metaData[letStmt] = letInfo;
  symbolTable.back()[letStmt->ident_token.TokenLiteral] = letInfo;

  std::cout << "LET STATEMENT DATA TYPE: " << expectedType.resolvedName << "\n";
}

//__________________Reference Statement___________________
void Semantics::walkReferenceStatement(Node *node) {
  auto refStmt = dynamic_cast<ReferenceStatement *>(node);
  if (!refStmt)
    return;

  std::cout << "[SEMANTIC LOG]: Analysing reference statement\n";

  auto refName = refStmt->name->expression.TokenLiteral;
  auto line = refStmt->statement.line;
  auto column = refStmt->statement.column;
  ResolvedType refType = ResolvedType{DataType::UNKNOWN, "unknown"};
  bool hasError = false;

  // Check if the reference variable name already exists
  auto existingSym = resolveSymbolInfo(refName);
  if (existingSym) {
    logSemanticErrors("Reference name '" + refName + "' already in use", line,
                      column);
    hasError = true;
  }

  // Check if the reference is pointing to something
  if (!refStmt->value) {
    if (insideRecord) {
      if (refStmt->type) {
        refType = inferNodeDataType(refStmt);
      } else {
        logSemanticErrors("Cannot get reference type if you did not provide it "
                          "and did not provide a value for inference",
                          line, column);
      }
      refType = inferNodeDataType(refStmt->type.get());
      auto refInfo = std::make_shared<SymbolInfo>();
      refInfo->type = refType;
      refInfo->isInitialized = false;
      refInfo->isRef = true;

      metaData[refStmt] = refInfo;
      symbolTable.back()[refName] = refInfo;
      return;
    } else {
      logSemanticErrors("Reference'" + refName + "' must reference something",
                        line, column);
      return;
    }
  }

  // Checking the type of the referee
  auto refereeName = extractIdentifierName(refStmt->value.get());
  walker(refStmt->value.get());
  auto refereeSymbol = resolveSymbolInfo(refereeName);
  if (!refereeSymbol) {
    logSemanticErrors("Reference '" + refName +
                          "'is referencing an undeclared variable '" +
                          refereeName + "'",
                      line, column);
    hasError = true;
    return;
  }

  ResolvedType refereeType = refereeSymbol->type;
  // Check if the referee type is a pointer and block it
  if (refereeType.isPointer) {
    logSemanticErrors("Cannot create references to pointers", line, column);
    hasError = true;
    return;
  } else {
    // If the referee type isnt a pointer toggle the isRef flag to true
    // It is like type elevation after all we want to create a reference to
    // something
    auto tempType = refereeSymbol->type;
    tempType.isRef = true;
    refereeType = isRefType(refereeType); // Convert the name and store it
  }

  // If the reference statement has no type we just infer the type
  if (!refStmt->type) {
    refType = refereeType;
  } else if (refStmt->type) {
    refType = inferNodeDataType(
        refStmt); // Update the data type with the type that was declared

    if (refType.isNull) {
      logSemanticErrors("Cannot have a nullable reference '" + refName + "'",
                        line, column);
      hasError = true;
    }

    // Compare the two types
    if (!isTypeCompatible(refType, refereeType)) {
      logSemanticErrors("Type mismatch reference '" + refName + "' of type '" +
                            refType.resolvedName +
                            "' does not match variable '" + refereeName +
                            "' being refered with type '" +
                            refereeType.resolvedName + "'",
                        line, column);
      hasError = true;
    }
  }

  // Checking if we are refering to a heap raised or global variable if not
  // complain
  auto refereeStorage = refereeSymbol->storage;
  if (refereeStorage == StorageType::STACK) {
    logSemanticErrors("Cannot create a reference '" + refName +
                          "' to a local variable '" + refereeName + "'",
                      line, column);
    hasError = true;
  }

  // Checking the mutability
  bool isMutable = false;
  bool isConstant = false;
  if (refStmt->mutability == Mutability::MUTABLE)
    isMutable = true;

  if (refStmt->mutability == Mutability::CONSTANT)
    isConstant = true;

  // Check if the symbol is also mutable
  if (!refereeSymbol->isMutable && isMutable) {
    logSemanticErrors("Cannot create a mutable reference '" + refName +
                          "' to an immutable variable '" + refereeName + "'",
                      line, column);
    hasError = true;
  }

  if (refereeSymbol->isNullable) {
    logSemanticErrors("Cannot create a reference to a nullable variable '" +
                          refereeName + "'",
                      line, column);
    hasError = true;
  }

  // Updating the reference count of the symbol being referenced
  refereeSymbol->refCount += 1;
  std::cout << "[DEBUG] Incremented refCount for target -> "
            << refereeSymbol->refCount << "\n";

  // Updating the storage type for references
  StorageType refStorage;
  if (isGlobalScope()) {
    refStorage = StorageType::GLOBAL;
  } else {
    refStorage = StorageType::STACK;
  }

  auto refInfo = std::make_shared<SymbolInfo>();
  refInfo->type = refType;
  refInfo->isInitialized = true;
  refInfo->isMutable = isMutable;
  refInfo->isConstant = isConstant;
  refInfo->refereeSymbol = refereeSymbol;
  refInfo->hasError = hasError;
  refInfo->isRef = true;
  refInfo->storage = refStorage;

  metaData[refStmt] = refInfo;
  symbolTable.back()[refName] = refInfo;
}
