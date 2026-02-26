#include "ast.hpp"
#include "semantics.hpp"
#include "token.hpp"
#include <memory>
#include <string>

void Semantics::walkSelfAssignment(AssignmentStatement *assignStmt) {
  bool hasError = true;
  auto selfSymbol = std::make_shared<SymbolInfo>();
  std::string assignName;
  // Handle the LHS
  if (auto selfExpr =
          dynamic_cast<SelfExpression *>(assignStmt->identifier.get())) {
    if (currentTypeStack.empty() ||
        currentTypeStack.back().type.kind != DataType::COMPONENT) {
      logSemanticErrors("'self' cannot be used outside a component", selfExpr);
      return;
    }

    auto &componentScope = currentTypeStack.back();
    auto currentTypeName = componentScope.typeName;
    assignName = extractIdentifierName(selfExpr);

    std::shared_ptr<MemberInfo> fieldInfo;

    for (const auto &field : selfExpr->fields) {
      auto ident = dynamic_cast<Identifier *>(field.get());
      if (!ident) {
        logSemanticErrors("Expected identifier in self expression", ident);
        return;
      }

      const std::string &fieldName = extractIdentifierName(ident);
      // Look up current type
      auto compTypeIt = customTypesTable.find(currentTypeName);
      if (compTypeIt == customTypesTable.end()) {
        logSemanticErrors("Component '" + currentTypeName + "' does not exist",
                          selfExpr);
        hasError = true;
        return;
      }

      auto &members = compTypeIt->second->members;
      auto memIt = members.find(fieldName);
      if (memIt == members.end()) {
        logSemanticErrors("Field '" + fieldName + "' not found in component '" +
                              currentTypeName + "'",
                          ident);
        hasError = true;
        return;
      }

      fieldInfo = memIt->second;
      currentTypeName = fieldInfo->type.resolvedName; // move deeper
    }
    selfSymbol->type = fieldInfo->type;
    selfSymbol->isNullable = fieldInfo->isNullable;
    selfSymbol->isMutable = fieldInfo->isMutable;
    selfSymbol->isConstant = fieldInfo->isConstant;
    selfSymbol->isPointer = fieldInfo->isPointer;
    selfSymbol->isInitialized = fieldInfo->isInitialised;
    selfSymbol->memberIndex = fieldInfo->memberIndex;
  }

  bool isPointing = (assignStmt->op.type == TokenType::ARROW);
  bool isAssigning = (assignStmt->op.type == TokenType::ASSIGN);

  // Enforce assignment style
  if (isPointing && !selfSymbol->isPointer) {
    logSemanticErrors("Cannot use '->' for non-pointer variable '" +
                          assignName + "'. Use '=' instead.",
                      assignStmt);
  }

  if (isAssigning && selfSymbol->isPointer) {
    // Special case: If we are assigning to a pointer, we enforce the arrow for
    // clarity
    logSemanticErrors("Cannot use '=' for pointer variable '" + assignName +
                          "'. Use '->' to repoint.",
                      assignStmt);
    hasError = true;
  }

  bool rhsIsNull = true;

  // Handle the RHS
  if (assignStmt->value) {
    walker(assignStmt->value.get());
    // If it is a null literal it is a special case give it context
    if (auto nullLiteral =
            dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
      auto nullSym = getSymbolFromMeta(nullLiteral);
      rhsIsNull = true;
      if (!selfSymbol->isNullable) {
        logSemanticErrors("Cannot assign 'null' to non nullable variable",
                          assignStmt);
        return;
      }
      // Give it context
      nullSym->type = selfSymbol->type;

      selfSymbol->isInitialized = true;
      selfSymbol->hasError = hasError;
      metaData[assignStmt] = selfSymbol;
      return;
    }

    auto valSym = getSymbolFromMeta(assignStmt->value.get());
    ResolvedType rhsType = valSym->type;
    ResolvedType lhsType = selfSymbol->type;
    // First strip the symbol if it is reference
    if (selfSymbol->isRef) {
      lhsType = peelRef(lhsType);
    }

    // Handle the special case for the opaque pointer
    bool passedTypeCheck = false;
    if (lhsType.kind == DataType::OPAQUE) {
      if (rhsIsNull) {
        logSemanticErrors("Cannot assign 'null' to an opaque pointer '" +
                              assignName + "'",
                          assignStmt);
        hasError = true;
      } else if (!rhsType.isPointer) {
        logSemanticErrors("Cannot assign non-pointer type '" +
                              rhsType.resolvedName + "' to opaque pointer '" +
                              assignName + "'",
                          assignStmt);
        hasError = true;
      }
      passedTypeCheck = true;
    }

    if (!passedTypeCheck && !isTypeCompatible(lhsType, rhsType)) {
      logSemanticErrors("Type mismatch: expected '" +
                            selfSymbol->type.resolvedName + "' but got '" +
                            rhsType.resolvedName + "'",
                        assignStmt->identifier.get());
      hasError = true;
    }

    // Mutability and const checks
    if (selfSymbol->isConstant) {
      logSemanticErrors("Cannot reassign to constant variable '" + assignName +
                            "'",
                        assignStmt->identifier.get());
      hasError = true;
    }

    if (!selfSymbol->isMutable && selfSymbol->isInitialized) {
      logSemanticErrors("Cannot reassign to immutable variable '" + assignName +
                            "'",
                        assignStmt->identifier.get());
      hasError = true;
    }

    selfSymbol->isInitialized = true;
  }
  metaData[assignStmt] = selfSymbol;
}

void Semantics::walkAssignStatement(Node *node) {
  auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
  if (dynamic_cast<SelfExpression *>(assignStmt->identifier.get())) {
    walkSelfExpression(assignStmt);
    return;
  }
  bool hasError = false;
  std::string name = extractIdentifierName(assignStmt->identifier.get());

  walker(assignStmt->identifier.get());
  auto lhsSym = getSymbolFromMeta(assignStmt->identifier.get());
  if (!lhsSym)
    return;

  if (lhsSym->hasError)
    return;

  bool isAssign = (assignStmt->op.type == TokenType::ASSIGN);
  bool isArrow = (assignStmt->op.type == TokenType::ARROW);
  if (isArrow && !lhsSym->isPointer) {
    logSemanticErrors("Cannot use '->' for non-pointer variable '" + name +
                          "'. Use '=' instead.",
                      assignStmt);
    hasError = true;
  }

  if (isAssign && lhsSym->isPointer) {
    // Special case: If we are assigning to a pointer, we enforce the arrow for
    // clarity
    logSemanticErrors("Cannot use '=' for pointer variable '" + name +
                          "'. Use '->' to repoint.",
                      assignStmt);
    hasError = true;
  }

  walker(assignStmt->value.get());
  bool rhsIsNull = false;
  bool rhsDefinitelyNull = false;
  // Handling the null literal and giving it context
  if (auto nullVal = dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
    rhsIsNull = true;
    auto nullSym = getSymbolFromMeta(nullVal);
    nullSym->type = lhsSym->type;
    nullSym->hasError = hasError;

    metaData[assignStmt] = lhsSym;
    return;
  }

  auto rhsSym = getSymbolFromMeta(assignStmt->value.get());
  if (rhsSym->hasError)
    return;

  ResolvedType rhsType = rhsSym->type;
  ResolvedType lhsType = lhsSym->type;

  if (!lhsSym->isNullable) {
    if (dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
      rhsDefinitelyNull = true; // literal null
    } else {
      if (rhsSym->isDefinitelyNull) {
        rhsDefinitelyNull = true;
      }
    }

    if (rhsDefinitelyNull) {
      logSemanticErrors(
          "Cannot assign a null value to to a non-nullable variable '" + name +
              "'",
          assignStmt->identifier.get());
      hasError = true;
    }
  }

  if (lhsSym->isRef) {
    lhsType = peelRef(lhsType);
  }

  bool passedTypeCheck = false;
  if (lhsType.kind == DataType::OPAQUE) {
    if (rhsIsNull) {
      logSemanticErrors("Cannot assign 'null' to an opaque pointer '" + name +
                            "'",
                        assignStmt);
      hasError = true;
    } else if (!rhsType.isPointer) {
      logSemanticErrors("Cannot assign non-pointer type '" +
                            rhsType.resolvedName + "' to opaque pointer '" +
                            name + "'",
                        assignStmt);
      hasError = true;
    }
    passedTypeCheck = true;
  }

  if (!passedTypeCheck && !isTypeCompatible(lhsType, rhsType)) {
    logSemanticErrors("Type mismatch: expected '" + lhsSym->type.resolvedName +
                          "' but got '" + rhsType.resolvedName + "'",
                      assignStmt->identifier.get());
    hasError = true;
  }

  // Mutability  / const checks
  if (lhsSym->isConstant) {
    logSemanticErrors("Cannot reassign to constant variable '" + name + "'",
                      assignStmt->identifier.get());
    hasError = true;
  }

  if (!lhsSym->isMutable && lhsSym->isInitialized) {
    logSemanticErrors("Cannot reassign to immutable variable '" + name + "'",
                      assignStmt->identifier.get());
    hasError = true;
  }

  // Specific special case handling
  if (auto arrLit = dynamic_cast<ArrayLiteral *>(assignStmt->value.get())) {
    // Get the array literal size per dimension vector
    auto litSizePerDim = getSizePerDimesion(arrLit);
    auto arrSizePerDim = rhsSym->sizePerDimensions;

    if (arrSizePerDim.size() != litSizePerDim.size()) {
      logSemanticErrors("Array variable '" + name + "' dimensions " +
                            std::to_string(arrSizePerDim.size()) +
                            " does not match array literal dimensions " +
                            std::to_string(arrSizePerDim.size()),
                        assignStmt->identifier.get());
      hasError = true;
    }
  } else if (auto moveVal =
                 dynamic_cast<MoveExpression *>(assignStmt->value.get())) {

    if (!rhsSym->isHeap && lhsSym->isHeap) {
      errorHandler.addHint("This variable was "
                           "explicitly declared as 'heap', "
                           "imposing a strict memory policy. Moving a non heap "
                           "value into it would force the "
                           "compiler to break that contract and switch to a "
                           "storage type you didn't request. "
                           "If you need this assignment to work, the source "
                           "must also be a 'heap' allocation.");
      logSemanticErrors("Cannot move a non heap variable into a heap variable",
                        moveVal->expr.get());
    }

    // symbol->isOwner = true; // Transfer ownership to say y
    // symbol->storage = moveSym->storage;
    rhsSym->isInvalid = true; // Invalidate the symbol for say move x
  }

  lhsSym->isInitialized = true;

  if (lhsSym->isHeap && rhsSym->isHeap) {
    transferBaton(assignStmt, lhsSym->ID);
  }
  metaData[assignStmt] = lhsSym;
}

void Semantics::walkFieldAssignmentStatement(Node *node) {
  auto fieldAssignStmt = dynamic_cast<FieldAssignment *>(node);
  if (!fieldAssignStmt)
    return;

  auto name = extractIdentifierName(fieldAssignStmt->lhs_chain.get());

  walker(fieldAssignStmt->lhs_chain.get());

  auto lhsInfo = metaData[fieldAssignStmt->lhs_chain.get()];
  if (!lhsInfo || lhsInfo->hasError) {
    return;
  }

  if (lhsInfo->isConstant) {
    logSemanticErrors("Cannot reassign to constant field '" + name + "'",
                      fieldAssignStmt);
    return;
  }

  if (!lhsInfo->isMutable && lhsInfo->isInitialized) {
    logSemanticErrors("Cannot reassign to immutable field '" + name + "'",
                      fieldAssignStmt);
    return;
  }

  // Enforce the assignment usage or arrow usage
  bool isAssign = (fieldAssignStmt->op.type == TokenType::ASSIGN);
  bool isArrow = (fieldAssignStmt->op.type == TokenType::ARROW);

  if (isArrow && !lhsInfo->isPointer) {
    logSemanticErrors("Cannot use '->' for non-pointer variable '" + name +
                          "'. Use '=' instead.",
                      fieldAssignStmt);
    return;
  }

  if (isAssign && lhsInfo->isPointer) {
    // Special case: If we are assigning to a pointer, we enforce the arrow
    // for clarity
    logSemanticErrors("Cannot use '=' for pointer variable '" + name +
                          "'. Use '->' to repoint.",
                      fieldAssignStmt);
    return;
  }

  if (fieldAssignStmt->value) {
    walker(fieldAssignStmt->value.get());
    auto rhsInfo = metaData[fieldAssignStmt->value.get()];
    if (dynamic_cast<NullLiteral *>(fieldAssignStmt->value.get())) {
      if (!lhsInfo->type.isNull) {
        logSemanticErrors("Cannot assign 'null' to non nullable variable '" +
                              name + "'",
                          fieldAssignStmt);
      }
      rhsInfo->type = lhsInfo->type;
      rhsInfo->isInitialized = true;
      rhsInfo->isDefinitelyNull = true;

      metaData[fieldAssignStmt] = lhsInfo;
    }

    if (rhsInfo) {
      if (lhsInfo->type.kind == DataType::OPAQUE) {
        if (!rhsInfo->type.isPointer) {
          logSemanticErrors("Cannot reassign non pointer type '" +
                                rhsInfo->type.resolvedName +
                                "' to an opaque pointer",
                            fieldAssignStmt);
        }
      } else if (!isTypeCompatible(lhsInfo->type, rhsInfo->type)) {
        logSemanticErrors("Type mismatch in assignment expected '" +
                              lhsInfo->type.resolvedName + "' but got '" +
                              rhsInfo->type.resolvedName + "'",
                          fieldAssignStmt);
      }
    }
  }

  metaData[fieldAssignStmt] = lhsInfo;
}
