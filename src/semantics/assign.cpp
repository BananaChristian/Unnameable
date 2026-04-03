#include "ast.hpp"
#include "semantics.hpp"
#include "token.hpp"
#include <memory>
#include <string>

void Semantics::checkOperatorStyle(TokenType op, bool isPointer,
                                   const std::string &name, Node *site) {
  if (op == TokenType::ARROW && !isPointer) {
    logSemanticErrors("Cannot use '->' on '" + name +
                          "' because it is not a pointer. "
                          "Use '=' to assign a value directly.",
                      site);
  } else if (op == TokenType::ASSIGN && isPointer) {
    logSemanticErrors("Cannot use '=' on '" + name +
                          "' because it is a pointer. "
                          "Use '->' to repoint it to a new target.",
                      site);
  }
}

/// Enforces that a variable is not constant or already-initialised immutable.
void Semantics::checkMutability(const SymbolInfo &sym, const std::string &name,
                                Node *site) {
  if (sym.storage().isConstant) {
    logSemanticErrors(
        "'" + name +
            "' is declared as 'const' and cannot be reassigned. "
            "Declare it as 'let' or 'var' if you need a mutable binding.",
        site);
    return;
  }
  if (!sym.storage().isMutable && sym.storage().isInitialized) {
    logSemanticErrors("'" + name +
                          "' is immutable and has already been initialised. "
                          "Declare it with 'var' if you intend to reassign it.",
                      site);
  }
}

bool Semantics::checkTypeCompatible(ResolvedType lhs, ResolvedType rhs,
                                    bool rhsIsNull, const std::string &name,
                                    Node *site) {
  if (lhs.kind == DataType::OPAQUE) {
    if (rhsIsNull) {
      logSemanticErrors(
          "Cannot assign 'null' to opaque pointer '" + name +
              "'. "
              "Opaque pointers must always point to a valid allocation.",
          site);
    } else if (!rhs.isPointer()) {
      logSemanticErrors("Type mismatch: opaque pointer '" + name +
                            "' requires a pointer "
                            "value, but got '" +
                            rhs.resolvedName + "'.",
                        site);
    }
    return true; // opaque check is self-contained
  }

  if (!isTypeCompatible(lhs, rhs)) {
    logSemanticErrors(
        "Type mismatch: '" + name + "' expects '" + lhs.resolvedName +
            "' but the right-hand side has type '" + rhs.resolvedName + "'.",
        site);
    hasError = false;
    return false;
  }
  return true;
}

bool Semantics::handleNullRhs(NullLiteral *nullLit,
                              const std::shared_ptr<SymbolInfo> &lhsSym,
                              const std::string &name,
                              AssignmentStatement *assignStmt) {
  if (!nullLit)
    return false;

  if (!lhsSym->type().isNullable) {
    logSemanticErrors(
        "Cannot assign 'null' to '" + name +
            "' because it is not nullable. "
            "Declare the variable with a '?' suffix (e.g. 'Type?') "
            "if it should be allowed to hold null.",
        assignStmt);
    hasError = false;
    return true; // error logged, caller should bail
  }

  // Give the null literal the LHS type so downstream passes have context.
  auto nullSym = getSymbolFromMeta(nullLit);
  if (nullSym)
    nullSym->type().type = lhsSym->type().type;

  lhsSym->storage().isInitialized = true;
  metaData[assignStmt] = lhsSym;
  hasError = false;
  return true;
}

void Semantics::walkSelfAssignment(AssignmentStatement *assignStmt) {
  auto *selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get());
  if (!selfExpr)
    return;

  // 'self' is only valid inside a component body.
  if (currentTypeStack.empty() ||
      currentTypeStack.back().type.kind != DataType::COMPONENT) {
    logSemanticErrors("'self' can only be used inside a component. "
                      "Move this assignment into a component method.",
                      selfExpr);
    return;
  }

  auto &componentScope = currentTypeStack.back();
  std::string currentTypeName = componentScope.typeName;
  std::string assignName = extractIdentifierName(selfExpr);

  std::shared_ptr<MemberInfo> fieldInfo;

  for (const auto &field : selfExpr->fields) {
    auto *ident = dynamic_cast<Identifier *>(field.get());
    if (!ident) {
      logSemanticErrors("Expected a plain identifier in 'self' field chain, "
                        "but found a more complex expression.",
                        field.get());
      return;
    }

    const std::string &fieldName = extractIdentifierName(ident);

    auto compTypeIt = customTypesTable.find(currentTypeName);
    if (compTypeIt == customTypesTable.end()) {
      logSemanticErrors("Component '" + currentTypeName +
                            "' is not defined. "
                            "Make sure it has been declared before this use.",
                        selfExpr);
      return;
    }

    auto &members = compTypeIt->second->members;
    auto memIt = members.find(fieldName);
    if (memIt == members.end()) {
      logSemanticErrors("Component '" + currentTypeName +
                            "' has no field named '" + fieldName +
                            "'. Check the component declaration for a typo.",
                        ident);
      return;
    }

    fieldInfo = memIt->second;
    currentTypeName = fieldInfo->type.resolvedName;
  }

  // Build a SymbolInfo from the resolved field.
  auto selfSymbol = std::make_shared<SymbolInfo>();
  selfSymbol->type().type = fieldInfo->type;
  selfSymbol->type().isNullable = fieldInfo->isNullable;
  selfSymbol->storage().isMutable = fieldInfo->isMutable;
  selfSymbol->storage().isConstant = fieldInfo->isConstant;
  selfSymbol->type().isPointer = fieldInfo->isPointer;
  selfSymbol->storage().isInitialized = fieldInfo->isInitialised;
  selfSymbol->type().memberIndex = fieldInfo->memberIndex;

  checkOperatorStyle(assignStmt->op.type, selfSymbol->type().isPointer, assignName,
                     assignStmt);

  if (!assignStmt->value)
    return;

  walker(assignStmt->value.get());
  auto valSym = getSymbolFromMeta(assignStmt->value.get());
  if (!valSym) {
    logSemanticErrors(
        "The right-hand side of this assignment refers to an undefined "
        "symbol '" +
            extractIdentifierName(assignStmt->value.get()) + "'.",
        assignStmt->value.get());
    return;
  }

  if (auto *nullLit = dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
    handleNullRhs(nullLit, selfSymbol, assignName, assignStmt);
    return;
  }

  giveGenericLiteralContext(assignStmt->value.get(), selfSymbol, valSym);

  ResolvedType lhsType =
      selfSymbol->type().isRef ? peelRef(selfSymbol->type().type) : selfSymbol->type().type;

  checkTypeCompatible(lhsType, valSym->type().type, /*rhsIsNull=*/false, assignName,
                      assignStmt->identifier.get());

  checkMutability(*selfSymbol, assignName, assignStmt->identifier.get());

  selfSymbol->storage().isInitialized = true;
  selfSymbol->hasError = false;
  metaData[assignStmt] = selfSymbol;
}

void Semantics::walkAssignStatement(Node *node) {
  auto *assignStmt = dynamic_cast<AssignmentStatement *>(node);
  if (!assignStmt)
    return;

  // Delegate 'self.x = …' to the self-specific walker.
  if (dynamic_cast<SelfExpression *>(assignStmt->identifier.get())) {
    walkSelfAssignment(assignStmt);
    return;
  }

  std::string name = extractIdentifierName(assignStmt->identifier.get());

  walker(assignStmt->identifier.get());
  auto lhsSym = getSymbolFromMeta(assignStmt->identifier.get());
  if (!lhsSym || lhsSym->hasError)
    return;

  checkOperatorStyle(assignStmt->op.type, lhsSym->type().isPointer, name, assignStmt);

  walker(assignStmt->value.get());
  auto rhsSym = getSymbolFromMeta(assignStmt->value.get());

  // Null literal,give it context and bail early.
  if (dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
    if (!lhsSym->type().isNullable) {
      logSemanticErrors(
          "Cannot assign 'null' to '" + name +
              "' because it is not nullable. "
              "Declare the variable with a '?' suffix (e.g. 'Type?') "
              "if it should be allowed to hold null.",
          assignStmt->identifier.get());
      return;
    }
    rhsSym->type().type = lhsSym->type().type;
    hasError = false;
    metaData[assignStmt] = lhsSym;
    return;
  }

  giveGenericLiteralContext(assignStmt->value.get(), lhsSym, rhsSym);

  if (rhsSym->hasError)
    return;

  if (!lhsSym->type().isNullable && rhsSym->type().isDefinitelyNull) {
    logSemanticErrors(
        "Cannot assign a null value to '" + name +
            "' because it is not "
            "nullable. Ensure the right-hand side is always initialised, "
            "or declare '" +
            name + "' as nullable with '?'.",
        assignStmt->identifier.get());
  }

  ResolvedType lhsType = lhsSym->type().isRef ? peelRef(lhsSym->type().type) : lhsSym->type().type;

  checkTypeCompatible(lhsType, rhsSym->type().type, /*rhsIsNull=*/false, name,
                      assignStmt->identifier.get());

  checkMutability(*lhsSym, name, assignStmt->identifier.get());

  if (auto *arrLit = dynamic_cast<ArrayLiteral *>(assignStmt->value.get())) {
    auto litDims = getSizePerDimesion(arrLit);
    auto varDims = rhsSym->type().sizePerDimensions;
    if (varDims.size() != litDims.size()) {
      logSemanticErrors("Array shape mismatch: '" + name +
                            "' was declared with " +
                            std::to_string(varDims.size()) +
                            " dimension(s), but the literal "
                            "has " +
                            std::to_string(litDims.size()) + " dimension(s).",
                        assignStmt->identifier.get());
    }
  } else if (auto *moveExpr =
                 dynamic_cast<MoveExpression *>(assignStmt->value.get())) {
    if (!rhsSym->storage().isHeap && lhsSym->storage().isHeap) {
      errorHandler.addHint("'" + name +
                           "' was declared as 'heap', which enforces a strict "
                           "ownership contract: only heap-allocated values may "
                           "be moved into it. "
                           "Either remove the 'heap' qualifier from '" +
                           name +
                           "', or allocate "
                           "the source value on the heap before moving.");
      logSemanticErrors(
          "Cannot move a non-heap value into the heap variable '" + name + "'.",
          moveExpr->expr.get());
    }
    rhsSym->storage().isInvalid = true; // source variable is consumed by the move
  }

  lhsSym->storage().isInitialized = true;

  if (lhsSym->storage().isHeap && rhsSym->storage().isHeap)
    transferBaton(assignStmt, lhsSym->codegen().ID);

  hasError = false;
  metaData[assignStmt] = lhsSym;
}

void Semantics::walkFieldAssignmentStatement(Node *node) {
  auto *fieldAssignStmt = dynamic_cast<FieldAssignment *>(node);
  if (!fieldAssignStmt)
    return;

  std::string name = extractIdentifierName(fieldAssignStmt->lhs_chain.get());

  walker(fieldAssignStmt->lhs_chain.get());
  auto lhsInfo = getSymbolFromMeta(fieldAssignStmt->lhs_chain.get());
  if (!lhsInfo) {
    reportDevBug("Failed to resolve LHS symbol in field assignment",
                 fieldAssignStmt->lhs_chain.get());
    return;
  }

  if (lhsInfo->hasError)
    return;

  checkMutability(*lhsInfo, name, fieldAssignStmt);

  checkOperatorStyle(fieldAssignStmt->op.type, lhsInfo->type().isPointer, name,
                     fieldAssignStmt);

  if (!fieldAssignStmt->value) {
    hasError = false;
    metaData[fieldAssignStmt] = lhsInfo;
    return;
  }

  walker(fieldAssignStmt->value.get());
  auto rhsInfo = getSymbolFromMeta(fieldAssignStmt->value.get());
  if (!rhsInfo) {
    logSemanticErrors("The right-hand side of the assignment to '" + name +
                          "' refers to an undefined symbol.",
                      fieldAssignStmt->value.get());
    hasError = false;
    return;
  }

  // Null literal, validate nullability and bail early.
  if (dynamic_cast<NullLiteral *>(fieldAssignStmt->value.get())) {
    if (!lhsInfo->type().type.isNull) {
      logSemanticErrors(
          "Cannot assign 'null' to field '" + name +
              "' because it is not "
              "nullable. Add a '?' to the field's type declaration if null "
              "is a valid state for it.",
          fieldAssignStmt);
      hasError = false;
      return;
    }
    rhsInfo->type().type = lhsInfo->type().type;
    rhsInfo->storage().isInitialized = true;
    rhsInfo->type().isDefinitelyNull = true;
    metaData[fieldAssignStmt] = lhsInfo;
    hasError = false;
    return;
  }

  if (dynamic_cast<INTLiteral *>(fieldAssignStmt->value.get())) {
    if (isInteger(lhsInfo->type().type))
      rhsInfo->type().type = lhsInfo->type().type;
  }
  giveGenericLiteralContext(fieldAssignStmt->value.get(), lhsInfo, rhsInfo);

  checkTypeCompatible(lhsInfo->type().type, rhsInfo->type().type, /*rhsIsNull=*/false, name,
                      fieldAssignStmt);

  hasError = false;
  metaData[fieldAssignStmt] = lhsInfo;
}
