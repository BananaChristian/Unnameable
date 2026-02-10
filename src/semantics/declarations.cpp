#include "ast.hpp"
#include "semantics.hpp"

#include <string>

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
  bool isSage = arrStmt->isSage;
  bool isHeap = arrStmt->isHeap;

  ResolvedType arrayType = inferNodeDataType(arrStmt->arrayType.get());
  logInternal("Array Type: " + arrayType.resolvedName);

  // Enforce initialization rules and constance rules
  if (isConstant) {
    logSemanticErrors("Constant array '" + arrayName + "' must be initialized",
                      arrStmt->statement.line, arrStmt->statement.column);
    hasError = true;
  }

  if (!arrStmt->array_content && arrStmt->lengths.empty()) {
    logSemanticErrors("Uninitialized array '" + arrayName +
                          "' is missing length declaration",
                      arrStmt->statement.line, arrStmt->statement.column);
    hasError = true;
  }

  std::vector<uint64_t> declSizePerDim;
  if (!arrStmt->lengths.empty()) {
    for (const auto &len : arrStmt->lengths) {
      walker(len.get());
      if (isIntegerConstant(len.get())) {
        auto size = getIntegerConstant(len.get());
        declSizePerDim.push_back(size);
      } else {
        // Push a place holder to shut the counter up
        declSizePerDim.push_back(0);
        logInternal("Detected dynamic dimension for '" + arrayName + "'");
      }
      // Look into a dynamic
    }
  }

  // Nullability rules and literals
  if (arrStmt->array_content) {
    isInitialized = true;
    walker(arrStmt->array_content.get());

    if (!metaData[arrStmt->array_content.get()]) {
      reportDevBug("Could not find array literal metaData");
      return;
    }

    // If it is a null literal give it context
    if (auto nullLit =
            dynamic_cast<NullLiteral *>(arrStmt->array_content.get())) {
      if (!arrayType.isNull) {
        logSemanticErrors("Cannot assign null to non nullable array '" +
                              arrayName + "' of type '" +
                              arrayType.resolvedName + "'",
                          arrNameLine, arrNameCol);
        hasError = true;
      }

      if (arrStmt->lengths.empty()) {
        logSemanticErrors("Cannot assign null to an array'" + arrayName +
                              "' whose dimensions you have not specified",
                          arrNameLine, arrNameCol);
        hasError = true;
      }

      metaData[nullLit]->type = arrayType;

      auto nullArray = std::make_shared<SymbolInfo>();
      nullArray->type = arrayType;
      nullArray->isMutable = isMutable;
      nullArray->isConstant = isConstant;
      nullArray->hasError = hasError;
      nullArray->isInitialized = isInitialized;
      nullArray->isSage = isSage;
      nullArray->isHeap = isHeap;
      nullArray->sizePerDimensions = declSizePerDim;
      // nullArray->isResponsibl = arrStmt;

      if (!loopContext.empty() && loopContext.back()) {
        nullArray->needsPostLoopFree = true;
        nullArray->bornInLoop = true;
      }

      metaData[arrStmt] = nullArray;
      symbolTable.back()[arrayName] = nullArray;

      logInternal("Dimension size '" + std::to_string(declSizePerDim.size()) +
                  "'");
      logInternal("Final Array statement Type '" + arrayType.resolvedName +
                  "'");
      return;
    }
    // Check dimension match
    auto literalDimensionSizePerDim =
        metaData[arrStmt->array_content.get()]->sizePerDimensions;
    // Only check if the user provided lengths
    if (!arrStmt->lengths.empty()) {
      if (declSizePerDim.size() != literalDimensionSizePerDim.size()) {
        logSemanticErrors(
            "Dimension count misatch expected '" +
                std::to_string(declSizePerDim.size()) +
                "' dimensions but got '" +
                std::to_string(literalDimensionSizePerDim.size()) + "'",
            arrStmt->array_content->expression.line,
            arrStmt->array_content->expression.column);
        hasError = true;
      }
    } else {
      // If the length wasnt given use the one of the literal
      declSizePerDim = literalDimensionSizePerDim;
    }

    // Check type compatibility
    ResolvedType literalType = metaData[arrStmt->array_content.get()]->type;
    if (!isTypeCompatible(arrayType, literalType)) {
      logSemanticErrors("Declared type '" + arrayType.resolvedName +
                            "' does not match the initialized type '" +
                            literalType.resolvedName + "'",
                        arrStmt->array_content->expression.line,
                        arrStmt->array_content->expression.column);
      hasError = true;
    }
  }

  auto arrayInfo = std::make_shared<SymbolInfo>();
  arrayInfo->type = arrayType;
  arrayInfo->isConstant = isConstant;
  arrayInfo->isMutable = isMutable;
  arrayInfo->hasError = hasError;
  arrayInfo->isSage = isSage;
  arrayInfo->isHeap = isHeap;
  arrayInfo->sizePerDimensions = declSizePerDim;
  // arrayInfo->isResponsible = arrStmt;
  arrayInfo->isInitialized = isInitialized;

  if (!loopContext.empty() && loopContext.back()) {
    arrayInfo->needsPostLoopFree = true;
    arrayInfo->bornInLoop = true;
  }

  metaData[arrStmt] = arrayInfo;
  symbolTable.back()[arrayName] = arrayInfo;

  logInternal("Final Array statement Type '" + arrayType.resolvedName + "'");
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
  bool isSage = ptrStmt->isSage;
  bool isHeap = ptrStmt->isHeap;
  bool isNullable = false;

  ResolvedType ptrType = ResolvedType{DataType::UNKNOWN, "unknown"};
  // Pointer's storage info (The pointer itself not the target)
  StorageType pointerStorage;
  if (isGlobalScope()) {
    pointerStorage = StorageType::GLOBAL;
  } else if (isSage) {
    pointerStorage = StorageType::SAGE;
  } else if (isHeap) {
    pointerStorage = StorageType::HEAP;
  } else {
    pointerStorage = StorageType::STACK;
  }

  // If we dont have the value(This is only allowed inside records and
  // components for now)
  if (!ptrStmt->value) {
    logSemanticErrors("Must initialize the pointer '" + ptrName + "'", line,
                      col);
    return;
  }

  if (ptrStmt->mutability == Mutability::MUTABLE) {
    logInternal("Pointer is mutable");
    isMutable = true;
  } else if (ptrStmt->mutability == Mutability::CONSTANT) {
    logInternal("Pointer is Constant");
    isConstant = true;
  }

  auto ptrVal = ptrStmt->value.get();

  walker(ptrVal);

  // Handle a null literal target
  if (auto nullLit = dynamic_cast<NullLiteral *>(ptrVal)) {
    // The user must provide the pointer type the compiler cannot infer from a
    // null

    if (!ptrStmt->type) {
      logSemanticErrors("Must provide pointer type for null inference to work",
                        line, col);
      return;
    }

    ptrType = inferNodeDataType(ptrStmt);
    isNullable = ptrType.isNull; // Set the null flag based of the type
    logInternal("Nullable Pointer Type '" + ptrType.resolvedName + "'");

    if (!ptrType.isPointer) {
      logSemanticErrors("Cannot point to a non pointer type'" +
                            ptrType.resolvedName + "'",
                        line, col);
      return;
    }

    // First of all block such if the pointer type isnt nullable
    if (!ptrType.isNull) {
      logSemanticErrors("Cannot use 'null' on a non nullable pointer type '" +
                            ptrType.resolvedName + "'",
                        line, col);
      return;
    }

    // Now you can give the null context
    metaData[nullLit]->type = ptrType;

    // Exit
    auto ptrInfo = std::make_shared<SymbolInfo>();
    ptrInfo->isSage = isSage;
    ptrInfo->isHeap = isHeap;
    ptrInfo->type = ptrType;
    ptrInfo->hasError = hasError;
    ptrInfo->isPointer = true;
    ptrInfo->isMutable = isMutable;
    ptrInfo->isConstant = isConstant;
    ptrInfo->isInitialized = true;
    ptrInfo->storage = pointerStorage;
    ptrInfo->isNullable = isNullable;

    metaData[ptrStmt] = ptrInfo;
    symbolTable.back()[ptrName] = ptrInfo;
    return;
  }

  // Dealing with what is being pointed to
  std::string targetName = extractIdentifierName(ptrVal);

  auto targetSymbol = metaData[ptrVal];
  if (!targetSymbol) {
    logSemanticErrors("Pointer '" + ptrName +
                          "'is pointing to an undeclared variable '" +
                          targetName + "'",
                      line, col);
    hasError = true;
    return;
  }

  ResolvedType targetType = targetSymbol->type;
  // Check if the target is a pointer
  if (!targetType.isPointer) {
    logSemanticErrors("Must initialize the pointer '" + ptrName +
                          "' with a pointer value",
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

    // Intercept a case where the pointer is opaque
    // If it is an opaque pointer any targetType is allowed as long as it a
    // pointer of course
    if (ptrType.kind == DataType::OPAQUE) {
      if (!targetType.isPointer) {
        logSemanticErrors(
            "Cannot initialize an opaque pointer with a non pointer type '" +
                targetType.resolvedName + "'",
            line, col);
      }
    } else if (!isTypeCompatible(ptrType, targetType)) {
      logSemanticErrors("Type mismatch pointer '" + ptrName + "' of type '" +
                            ptrType.resolvedName + "' does not match '" +
                            targetName + "' of type '" +
                            targetType.resolvedName + "'",
                        line, col);
      hasError = true;
    }
  }

  isNullable = ptrType.isNull; // Set the nullability based of the type

  // Guard against lattice violation pointing and update pointer count
  auto targetStorage = targetSymbol->storage;
  if (!validateLatticeMove(pointerStorage, targetStorage)) {
    errorHandler.addHint("Lattice Rule Violation: A pointer cannot be more "
                         "stable than the data it points to. "
                         "If '" +
                         ptrName + "' (" + storageStr(pointerStorage) +
                         ") outlives '" + targetName + "' (" +
                         storageStr(targetStorage) +
                         "), it will become a dangling pointer "
                         "once '" +
                         targetName + "' is freed.");
    logSemanticErrors("Pointer '" + ptrName + "' of memory tier '" +
                          storageStr(pointerStorage) +
                          "' cannot point to a target '" + targetName +
                          "'of type '" + storageStr(targetStorage) + "'",
                      line, col);
  } else {
    targetSymbol->pointerCount++;
  }

  /*
  Checking for mutability (It should be noted that by default the mutability
  is... well immutable That means if the user didnt add it we are dealing with
  an immutable pointer
  */

  logInternal("Pointer Type: " + ptrType.resolvedName);

  auto ptrInfo = std::make_shared<SymbolInfo>();
  ptrInfo->isSage = isSage;
  ptrInfo->isHeap = isHeap;
  ptrInfo->type = ptrType;
  ptrInfo->hasError = hasError;
  ptrInfo->isPointer = true;
  ptrInfo->targetSymbol = targetSymbol;
  ptrInfo->isMutable = isMutable;
  ptrInfo->isConstant = isConstant;
  ptrInfo->storage = pointerStorage;
  ptrInfo->isInitialized = true;
  ptrInfo->isNullable = isNullable;

  // Get the target baton
  LifeTime *targetBaton = responsibilityTable[ptrStmt->value.get()].get();

  auto lifetime = createLifeTimeTracker(ptrStmt, targetBaton, ptrInfo);
  ptrInfo->ID = lifetime->ID;

  responsibilityTable[ptrStmt] = std::move(lifetime);
  metaData[ptrStmt] = ptrInfo;
  symbolTable.back()[ptrName] = ptrInfo;
}

//____________Let statement walker______________
void Semantics::walkLetStatement(Node *node) {
  auto letStmt = dynamic_cast<LetStatement *>(node);
  if (!letStmt)
    return;
  auto type = dynamic_cast<BasicType *>(letStmt->type.get());

  // --- Initial flags ---
  bool isNullable = type->isNullable;
  bool isSage = letStmt->isSage;
  bool isHeap = letStmt->isHeap;
  bool isDefinitelyNull = false;
  bool isInitialized = false;
  bool hasError = false;

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
  if (expectedType.kind == DataType::OPAQUE) {
    logSemanticErrors(
        "Cannot use an opaque pointer type on a scalar variable declaration",
        letStmt->ident_token.line, letStmt->ident_token.column);
  }
  ResolvedType declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};

  // --- Mutability & constants ---
  bool isMutable = (letStmt->mutability == Mutability::MUTABLE);
  bool isConstant = (letStmt->mutability == Mutability::CONSTANT);

  // Constant declaration checks
  if (isConstant && !letStmtValue) {
    logSemanticErrors("Need to assign a value to a constant variable '" +
                          letName + "'",
                      letStmt->ident_token.line, letStmt->ident_token.column);
    hasError = true;
    return;
  }

  if (isConstant && !isConstLiteral(letStmtValue)) {
    logSemanticErrors("Constant variable'" + letName +
                          "' must be initialized with a raw literal, not a "
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
    } else if (auto moveVal = dynamic_cast<MoveExpression *>(letStmtValue)) {
      auto moveSym = metaData[moveVal];
      if (!moveSym)
        return;

      if (isHeap && moveSym->storage != StorageType::HEAP) {
        errorHandler.addHint(
            "This variable was "
            "explicitly declared as 'heap', "
            "imposing a strict memory policy. Moving a non heap "
            "value into it would force the "
            "compiler to break that contract and switch to a "
            "storage type you didn't request. "
            "If you need this assignment to work, the source "
            "must also be a 'heap' allocation.");
        logSemanticErrors(
            "Cannot move a non heap variable into a heap variable",
            moveVal->expr->expression.line, moveVal->expr->expression.line);
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
  // TODO: Revise this rule
  if (isSage || isHeap) {
    // REVISE THESE LAWS TOO
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
  } else if (isSage) {
    letStorageType = StorageType::SAGE;
  } else if (isHeap) {
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
  letInfo->isSage = isSage;
  letInfo->isHeap = isHeap;
  letInfo->pointerCount = 0;

  letInfo->storage = letStorageType;
  letInfo->hasError = hasError;
  if (!loopContext.empty() && loopContext.back()) {
    letInfo->needsPostLoopFree = true;
    letInfo->bornInLoop = true;
  }

  // Since this is a normal let statement it has no target baton as its target
  // is independent to it(The compiler uses a deep copy first style so these are
  // independent)
  auto lifetime = createLifeTimeTracker(letStmt, nullptr, letInfo);
  letInfo->ID = lifetime->ID;
  responsibilityTable[letStmt] = std::move(lifetime);
  metaData[letStmt] = letInfo;
  symbolTable.back()[letStmt->ident_token.TokenLiteral] = letInfo;

  logInternal("Variable Declaration Data Type: " + expectedType.resolvedName);
}

//__________________Reference Statement___________________
void Semantics::walkReferenceStatement(Node *node) {
  auto refStmt = dynamic_cast<ReferenceStatement *>(node);
  if (!refStmt)
    return;

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
  logInternal("Incremented refCount for target -> " +
              std::to_string(refereeSymbol->refCount));

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
