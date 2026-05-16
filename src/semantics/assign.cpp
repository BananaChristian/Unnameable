#include <memory>
#include <string>

#include "ast.hpp"
#include "semantics.hpp"
#include "token.hpp"

void Semantics::checkOperatorStyle(TokenType op, bool isPointer,
                                   const std::string &name, Node *site) {
  if (op == TokenType::ARROW && !isPointer) {
    logSemanticErrors(ErrorCode::InvalidOperatorStyle, site,
                      {"->", name, "pointer"});
  } else if (op == TokenType::ASSIGN && isPointer) {
    logSemanticErrors(ErrorCode::InvalidOperatorStyle, site,
                      {"=", name, "pointer"});
  }
}

/// Enforces that a variable is not constant or already-initialised immutable.
void Semantics::checkMutability(const SymbolInfo &sym, const std::string &name,
                                Node *site) {
  if (sym.storage().isConstant) {
    logSemanticErrors(ErrorCode::InvalidConstUse, site, {name});
    return;
  }
  if (!sym.storage().isMutable && sym.storage().isInitialized) {
    logSemanticErrors(ErrorCode::CantReassignImmut, site, {name});
  }
}

bool Semantics::checkTypeCompatible(ResolvedType lhs, ResolvedType rhs,
                                    bool rhsIsNull, const std::string &name,
                                    Node *site) {
  if (lhs.kind == DataType::OPAQUE) {
    if (rhsIsNull) {
      logSemanticErrors(ErrorCode::OpaqueNullAssignment, site, {name});
    } else if (!rhs.isPointer()) {
      logSemanticErrors(ErrorCode::OpaqueTypeMismatch, site,
                        {name, rhs.resolvedName});
    }
    return true; // opaque check is self-contained
  }

  if (!isTypeCompatible(lhs, rhs)) {
    logSemanticErrors(ErrorCode::TypeMismatch, site,
                      {lhs.resolvedName, rhs.resolvedName});
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
    logSemanticErrors(ErrorCode::NullAssignmentToNonNullable, assignStmt,
                      {name});
    return true; // error logged, caller should bail
  }

  // Give the null literal the LHS type so downstream passes have context.
  auto nullSym = getSymbolFromMeta(nullLit);
  if (nullSym)
    nullSym->type().type = lhsSym->type().type;

  lhsSym->storage().isInitialized = true;
  insertMetaData(assignStmt, lhsSym);
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
    logSemanticErrors(ErrorCode::SelfOnlyInComponent, selfExpr);
    return;
  }

  auto &componentScope = currentTypeStack.back();
  std::string currentTypeName = componentScope.typeName;
  std::string assignName = extractIdentifierName(selfExpr);

  std::shared_ptr<MemberInfo> fieldInfo;

  for (const auto &field : selfExpr->fields) {
    auto *ident = dynamic_cast<Identifier *>(field.get());
    if (!ident) {
      logSemanticErrors(ErrorCode::ExpectedIdentifierInSelf, field.get());
      return;
    }

    const std::string &fieldName = extractIdentifierName(ident);

    auto compTypeIt = customTypesTable.find(currentTypeName);
    if (compTypeIt == customTypesTable.end()) {
      logSemanticErrors(ErrorCode::UndefinedVariable, selfExpr,
                        {currentTypeName});
      return;
    }

    auto &members = compTypeIt->second->members;
    auto memIt = members.find(fieldName);
    if (memIt == members.end()) {
      logSemanticErrors(ErrorCode::UndefinedVariable, ident, {fieldName});
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

  checkOperatorStyle(assignStmt->op.type, selfSymbol->type().isPointer,
                     assignName, assignStmt);

  if (!assignStmt->value)
    return;

  walker(assignStmt->value.get());
  auto valSym = getSymbolFromMeta(assignStmt->value.get());
  if (!valSym) {
    logSemanticErrors(ErrorCode::UndefinedVariable, assignStmt->value.get(),
                      {extractIdentifierName(assignStmt->value.get())});
    return;
  }

  if (auto *nullLit = dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
    handleNullRhs(nullLit, selfSymbol, assignName, assignStmt);
    return;
  }

  giveGenericLiteralContext(assignStmt->value.get(), selfSymbol->type().type,
                            valSym);

  ResolvedType lhsType = selfSymbol->type().isRef
                             ? peelRef(selfSymbol->type().type)
                             : selfSymbol->type().type;

  checkTypeCompatible(lhsType, valSym->type().type, /*rhsIsNull=*/false,
                      assignName, assignStmt->identifier.get());

  checkMutability(*selfSymbol, assignName, assignStmt->identifier.get());

  selfSymbol->storage().isInitialized = true;
  selfSymbol->hasError = hasError;
  insertMetaData(assignStmt, selfSymbol);
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

  checkOperatorStyle(assignStmt->op.type, lhsSym->type().isPointer, name,
                     assignStmt);

  walker(assignStmt->value.get());
  auto rhsSym = getSymbolFromMeta(assignStmt->value.get());

  // Null literal,give it context and bail early.
  if (dynamic_cast<NullLiteral *>(assignStmt->value.get())) {
    if (!lhsSym->type().isNullable) {
      logSemanticErrors(ErrorCode::NullAssignmentToNonNullable,
                        assignStmt->identifier.get(), {name});
      return;
    }
    rhsSym->type().type = lhsSym->type().type;
    insertMetaData(assignStmt, lhsSym);
    return;
  }

  giveGenericLiteralContext(assignStmt->value.get(), lhsSym->type().type,
                            rhsSym);

  if (rhsSym->hasError)
    return;

  if (!lhsSym->type().isNullable && rhsSym->type().isDefinitelyNull) {
    logSemanticErrors(ErrorCode::NullAssignmentToNonNullable,
                      assignStmt->identifier.get(), {name});
  }

  ResolvedType lhsType =
      lhsSym->type().isRef ? peelRef(lhsSym->type().type) : lhsSym->type().type;

  checkTypeCompatible(lhsType, rhsSym->type().type, /*rhsIsNull=*/false, name,
                      assignStmt->identifier.get());

  checkMutability(*lhsSym, name, assignStmt->identifier.get());

  if (auto *arrLit = dynamic_cast<ArrayLiteral *>(assignStmt->value.get())) {
    auto litDims = getSizePerDimesion(arrLit);
    auto varDims = rhsSym->type().sizePerDimensions;
    if (varDims.size() != litDims.size()) {
      logSemanticErrors(ErrorCode::ArrayShapeMismatch,
                        assignStmt->identifier.get(),
                        {name, std::to_string(varDims.size()),
                         std::to_string(litDims.size())});
    }
  }

  lhsSym->storage().isInitialized = true;

  if (lhsSym->storage().isHeap && rhsIsHeap(assignStmt->value.get()))
    transferBaton(assignStmt, lhsSym->codegen().ID);

  insertMetaData(assignStmt, lhsSym);
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

  if (lhsInfo->hasError) {
    logSemanticErrors(ErrorCode::LhsHasError, fieldAssignStmt->lhs_chain.get());
    return;
  }

  checkMutability(*lhsInfo, name, fieldAssignStmt);

  checkOperatorStyle(fieldAssignStmt->op.type, lhsInfo->type().isPointer, name,
                     fieldAssignStmt);

  if (!fieldAssignStmt->value) {
    insertMetaData(fieldAssignStmt, lhsInfo);
    return;
  }

  walker(fieldAssignStmt->value.get());
  auto rhsInfo = getSymbolFromMeta(fieldAssignStmt->value.get());
  if (!rhsInfo) {
    auto valName = extractIdentifierName(fieldAssignStmt->value.get());
    logSemanticErrors(ErrorCode::UndefinedVariable,
                      fieldAssignStmt->value.get(), {valName});
    return;
  }

  // Null literal, validate nullability and bail early.
  if (dynamic_cast<NullLiteral *>(fieldAssignStmt->value.get())) {
    if (!lhsInfo->type().type.isNull) {
      logSemanticErrors(ErrorCode::NullAssignmentToField, fieldAssignStmt,
                        {name});
      return;
    }
    rhsInfo->type().type = lhsInfo->type().type;
    rhsInfo->storage().isInitialized = true;
    rhsInfo->type().isDefinitelyNull = true;
    insertMetaData(fieldAssignStmt, lhsInfo);
    return;
  }

  giveGenericLiteralContext(fieldAssignStmt->value.get(), lhsInfo->type().type,
                            rhsInfo);

  checkTypeCompatible(lhsInfo->type().type, rhsInfo->type().type,
                      /*rhsIsNull=*/false, name, fieldAssignStmt);

  insertMetaData(fieldAssignStmt, lhsInfo);
}
