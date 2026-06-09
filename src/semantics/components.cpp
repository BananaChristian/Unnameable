#include <memory>
#include <string>

#include "ast.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include "token.hpp"

void Semantics::mapSymbolInfoToMemberInfo(
    const std::shared_ptr<SymbolInfo> &sym,
    std::shared_ptr<MemberInfo> &memInfo, int memberIndex) {
  memInfo->type = sym->type().type;
  memInfo->isMutable = sym->storage().isMutable;
  memInfo->isConstant = sym->storage().isConstant;
  memInfo->isInitialised = sym->storage().isInitialized;
  memInfo->isPointer = sym->type().isPointer;
  memInfo->isRef = sym->type().isRef;
  memInfo->isNullable = sym->type().isNullable;
  memInfo->isVolatile = sym->storage().isVolatile;
  memInfo->isFnPtr = sym->type().isFnPtr;
  memInfo->isHeap = sym->storage().isHeap;
  memInfo->ID = sym->codegen().ID;
  memInfo->allocType = sym->storage().allocType;
  memInfo->isVolatile = sym->storage().isVolatile;
  memInfo->memberIndex = memberIndex;

  if (memInfo->isFunction) {
    memInfo->paramTypes = sym->func().paramTypes;
    memInfo->isReturnHeap = sym->storage().isHeap;
    memInfo->retFamilyID = sym->codegen().ID;
    memInfo->returnType = sym->func().returnType;
    memInfo->isExportable = sym->isExportable;
    memInfo->isDefined = true;
    memInfo->isDeclared = true;
  }
}

void Semantics::walkEnumStatement(Node *node) {
  auto enumStmt = dynamic_cast<EnumStatement *>(node);
  if (!enumStmt)
    return;

  std::string enumStmtName =
      extractIdentifierName(enumStmt->enum_identifier.get());
  bool isExportable = enumStmt->isExportable;

  // Check duplicate type name
  if (resolveSymbolInfo(enumStmtName)) {
    logSemanticErrors(ErrorCode::DuplicateName, enumStmt, {enumStmtName});
    insertErrorMetaData(enumStmt);
    return;
  }

  // Set underlying type (default = i32)
  ResolvedType underLyingType = ResolvedType::makeBase(DataType::I32, "i32");
  if (enumStmt->int_type.has_value()) {
    underLyingType =
        tokenTypeToResolvedType(enumStmt->int_type.value(), "void", false);
    if (!isInteger(underLyingType)) {
      logSemanticErrors(ErrorCode::InvalidEnumLitType, enumStmt);
      insertErrorMetaData(enumStmt);
      return;
    }
  }

  // Determine if underlying type is unsigned
  bool underlyingIsUnsigned = isUnsignedIntegerType(underLyingType);

  // Push temporary scope
  payload.symbolTable.push_back({});
  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  int64_t currentValue = 0;
  auto enumInfo = std::make_shared<SymbolInfo>();

  for (const auto &enumMember : enumStmt->enum_content) {
    if (!enumMember)
      reportDevBug("Invalid enum member", node);

    logInternal("Analysing enum member ...");
    std::string memberName = enumMember->enumMember;

    // Check for duplicate member name
    if (members.count(memberName) || resolveSymbolInfo(memberName)) {
      logSemanticErrors(ErrorCode::DuplicateName, enumMember.get(),
                        {memberName});
      insertErrorMetaData(enumStmt);
      payload.symbolTable.pop_back();
      return;
    }

    std::int64_t memberValue = 0;
    if (enumMember->value) {
      if (!isIntegerConstant(enumMember->value.get())) {
        logSemanticErrors(ErrorCode::InvalidEnumMemberVal, enumMember.get());
        insertErrorMetaData(enumStmt);
        payload.symbolTable.pop_back();
        return;
      }

      walker(enumMember->value.get());
      auto litSym = getSymbolFromMeta(enumMember->value.get());
      if (!litSym)
        reportDevBug("Failed to analyze enum member value",
                     enumMember->value.get());

      auto literalType = litSym->type().type;
      if (!isInteger(literalType)) {
        logSemanticErrors(ErrorCode::InvalidEnumLitType, enumMember.get());
        insertErrorMetaData(enumStmt);
        payload.symbolTable.pop_back();
        return;
      }

      // Give the enum member's value literal context
      if (enumMember->value.get()) {
        giveGenericLiteralContext(enumMember->value.get(), underLyingType,
                                  litSym);
      }

      if (!isTypeCompatible(underLyingType, literalType)) {
        logSemanticErrors(
            ErrorCode::TypeMismatch, enumMember->value.get(),
            {underLyingType.resolvedName, literalType.resolvedName});
        insertErrorMetaData(enumStmt);
        payload.symbolTable.pop_back();
        return;
      }

      std::uint64_t parsedValue =
          std::stoull(enumMember->value.get()->expression.TokenLiteral);
      memberValue = static_cast<std::int64_t>(parsedValue);
    } else {
      memberValue = currentValue;
      if (underlyingIsUnsigned && memberValue < 0) {
        logSemanticErrors(ErrorCode::NegativeMember, enumMember.get(),
                          {std::to_string(memberValue)});
        insertErrorMetaData(enumStmt);
        payload.symbolTable.pop_back();
        return;
      }
    }

    // Store member info
    auto info = std::make_shared<MemberInfo>();
    info->memberName = memberName;
    info->type = underLyingType;
    info->isConstant = true;
    info->isInitialised = true;
    info->isExportable = isExportable;
    info->node = enumMember.get();
    info->constantValue = static_cast<int64_t>(memberValue);
    info->parentType = ResolvedType::makeBase(DataType::ENUM, enumStmtName);
    members[memberName] = info;

    // Add it to local scope
    auto memberInfo = std::make_shared<SymbolInfo>();
    memberInfo->type().type = underLyingType;
    memberInfo->storage().isConstant = true;
    memberInfo->storage().isInitialized = true;
    memberInfo->isExportable = isExportable;
    payload.symbolTable.back()[memberName] = memberInfo;

    currentValue = memberValue + 1;
  }

  // Store enum type info
  auto typeInfo = std::make_shared<CustomTypeInfo>();

  typeInfo->typeName = enumStmtName;
  typeInfo->type = ResolvedType::makeBase(DataType::ENUM, enumStmtName);
  typeInfo->underLyingType = underLyingType.kind;
  typeInfo->members = members;
  typeInfo->isExportable = isExportable;
  payload.customTypesTable[enumStmtName] = typeInfo;

  // Add enum type to parent scope
  auto generalInfo = std::make_shared<SymbolInfo>();
  generalInfo->type().type =
      ResolvedType::makeBase(DataType::ENUM, enumStmtName);
  generalInfo->storage().isConstant = false;
  generalInfo->storage().isInitialized = true;
  generalInfo->isExportable = isExportable;
  payload.symbolTable[payload.symbolTable.size() - 2][enumStmtName] =
      generalInfo;

  insertMetaData(enumStmt, generalInfo);

  // Pop temporary scope
  payload.symbolTable.pop_back();
}

void Semantics::walkRecordStatement(Node *node) {
  auto recordStmt = dynamic_cast<RecordStatement *>(node);
  if (!recordStmt)
    return;

  // Get block name
  std::string recordName = extractIdentifierName(recordStmt->recordName.get());
  bool isExportable = recordStmt->isExportable;

  // Ensure name not already used
  if (resolveSymbolInfo(recordName)) {
    logSemanticErrors(DuplicateName, recordStmt, {recordName});
    insertErrorMetaData(recordStmt);
    return;
  }

  // Setup mutability
  bool isBlockMutable = (recordStmt->mutability == Mutability::MUTABLE);
  bool isBlockConstant = (recordStmt->mutability == Mutability::CONSTANT);
  bool isBlockVolatile = recordStmt->isVolatile;

  // Build the record symbol
  auto recordSym = std::make_shared<SymbolInfo>();
  recordSym->isExportable = isExportable;
  recordSym->storage().isMutable = isBlockMutable;
  recordSym->storage().isConstant = isBlockConstant;
  recordSym->isExportable = isExportable;
  recordSym->type().type = ResolvedType::makeBase(DataType::RECORD, recordName);
  recordSym->isRecord = true;

  // Build customtype info
  auto typeInfo = std::make_shared<CustomTypeInfo>();
  typeInfo->typeName = recordName;
  typeInfo->type = ResolvedType::makeBase(DataType::RECORD, recordName),
  typeInfo->isExportable = isExportable;
  typeInfo->typeNode = recordStmt;

  if (auto recordModifier =
          dynamic_cast<StructureModifier *>(recordStmt->modifiers.get())) {
    recordSym->storage().isPacked = recordModifier->isPacked;
    if (recordModifier->_align.get()) {
      typeInfo->isExplicitAligned = true;
      typeInfo->alignmentBytes =
          parseAlignmentBytes(recordModifier->_align.get());
    }
  }

  // Early registration
  payload.symbolTable[0][recordName] = recordSym;
  payload.customTypesTable[recordName] = typeInfo;
  insertMetaData(recordStmt, recordSym);

  // Create new local scope for analysis
  insideRecord = true;
  payload.symbolTable.push_back({});

  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> recordMembers;

  int currentMemberIndex = 0;
  // Analyze each field
  for (const auto &field : recordStmt->fields) {
    auto declaration = dynamic_cast<VariableDeclaration *>(field.get());

    // Double check incase the parser messed up and leaked wrong statements
    if (!declaration) {
      logSemanticErrors(ErrorCode::IllegalStmtInRecord, field.get(),
                        {recordName});
      continue; // skip bad field but keep going
    }

    // Apply block mutability if set
    if (isBlockMutable)
      declaration->mutability = Mutability::MUTABLE;
    else if (isBlockConstant)
      declaration->mutability = Mutability::CONSTANT;

    // Apply volatility to the entire block if the user marked it
    if (isBlockVolatile)
      declaration->isVolatile = true;

    // Walk the let statement to register it in the scope
    walker(field.get());

    // Now retrieve its symbol
    auto fieldName = extractDeclarationName(field.get());
    auto fieldSymbol = getSymbolFromMeta(field.get());
    if (!fieldSymbol) {
      reportDevBug("Declaration statement '" + fieldName +
                       "' was not analyzed properly",
                   field.get());
    }
    fieldSymbol->type().memberIndex = currentMemberIndex;

    // If the variable analyzed is heap then register it as capture candidate
    if (fieldSymbol->storage().isHeap)
      typeInfo->captureCandidates.push_back(fieldSymbol->codegen().ID);

    // Build member info
    auto memInfo = std::make_shared<MemberInfo>();
    memInfo->memberName = fieldName;
    mapSymbolInfoToMemberInfo(fieldSymbol, memInfo, currentMemberIndex);
    memInfo->typeNode = recordStmt;
    memInfo->node = field.get();
    currentMemberIndex++;

    // Insert into members map
    recordMembers[fieldName] = memInfo;

    logInternal("Added field '" + fieldName + "' to record '" + recordName +
                "'");
  }

  // Build symbol info for the block
  recordSym->members = recordMembers;
  recordSym->hasError = hasError;

  typeInfo->members = recordMembers;

  payload.customTypesTable[recordName] = typeInfo;
  // Pop local scope
  insideRecord = false;
  payload.symbolTable.pop_back();
}

void Semantics::walkInstanceExpression(Node *node) {
  auto instExpr = dynamic_cast<InstanceExpression *>(node);
  if (!instExpr)
    return;

  auto instName = extractIdentifierName(instExpr->blockIdent.get());

  auto instSym = resolveSymbolInfo(instName);

  if (!instSym) {
    logSemanticErrors(ErrorCode::UndefinedVariable, instExpr->blockIdent.get(),
                      {instName});
    insertErrorMetaData(instExpr);
    return;
  }

  if (instSym->type().type.kind != DataType::RECORD)
    logSemanticErrors(ErrorCode::InstNotaRecord, instExpr->blockIdent.get(),
                      {instName});

  // Dealing with arguments if they exist
  if (!instExpr->fields.empty()) {
    auto members = instSym->members;
    for (const auto &field : instExpr->fields) {
      auto fieldNode = dynamic_cast<AssignmentStatement *>(field.get());
      auto fieldName = fieldNode->identifier->expression.TokenLiteral;

      auto it = members.find(fieldName);
      if (it == members.end()) {
        logSemanticErrors(ErrorCode::NotaMemberOf, instExpr->blockIdent.get(),
                          {instName, fieldName});
        continue;
      }

      auto expectedFieldType = it->second->type;

      walker(fieldNode->value.get());
      auto litSym = getSymbolFromMeta(fieldNode->value.get());
      if (!litSym)
        reportDevBug("Failed to get field value symbol info",
                     fieldNode->value.get());

      if (auto *nullLit = dynamic_cast<NullLiteral *>(fieldNode->value.get())) {
        litSym->type().type = expectedFieldType;
        litSym->type().isDefinitelyNull = true;

        metaData[nullLit] = litSym;
      } else {
        giveGenericLiteralContext(fieldNode->value.get(), expectedFieldType,
                                  litSym);
      }
    }
  }

  ResolvedType instType = instSym->type().type;
  logInternal("Instance Type '" + instType.resolvedName + "'");

  auto instInfo = std::make_shared<SymbolInfo>();
  instInfo->type().type = instType;
  instInfo->hasError = hasError;

  insertMetaData(instExpr, instInfo);
}

void Semantics::walkMethodsStatement(Node *node) {
  auto metStmt = dynamic_cast<MethodsStatement *>(node);
  if (!metStmt)
    return;

  std::string name = extractIdentifierName(metStmt->method_identifier.get());
  auto typeInfo = getCustomTypeInfo(name);
  if (!typeInfo) {
    logSemanticErrors(ErrorCode::UndefinedVariable,
                      metStmt->method_identifier.get());
    insertErrorMetaData(metStmt);
    return;
  }

  auto metSym = std::make_shared<SymbolInfo>();
  metSym->type().type = typeInfo->type;

  insideMethods = true;

  // Restore a type stack so that self can see the members in the record
  payload.currentTypeStack.push_back(
      {.type = ResolvedType::makeBase(DataType::RECORD, name),
       .typeName = name,
       .members = typeInfo->members});

  for (const auto &stmt : metStmt->functions) {
    auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
    // Only function definitions
    auto funcExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
    if (!funcExpr) {
      logSemanticErrors(ErrorCode::IllegalStmtInMethods, fnStmt);
      continue;
    }
    walker(stmt.get());
    auto fnSym = getSymbolFromMeta(funcExpr);
    if (!fnSym)
      reportDevBug("Failed to get function symbol info", funcExpr);

    auto memInfo = std::make_shared<MemberInfo>();
    memInfo->isFunction = true;
    // These are functions so I dont care about memberIndex
    mapSymbolInfoToMemberInfo(fnSym, memInfo, -1);
    auto fnName = extractIdentifierName(funcExpr->func_identifier.get());
    memInfo->node = funcExpr;
    memInfo->typeNode = typeInfo->typeNode;
    memInfo->memberName = fnName;
    typeInfo->members[fnName] = memInfo;
  }

  insideMethods = false;
  metSym->members = typeInfo->members;

  insertMetaData(metStmt, metSym);
  payload.currentTypeStack.pop_back();
}

// Resolves a self expression chain like self.pos.x.y
// Returns the ResolvedType of the final field, nullptr on error
ResolvedType *Semantics::resolveSelfChain(SelfExpression *selfExpr,
                                          const std::string &typeName) {
  // Look up the type's type info
  auto ctIt = payload.customTypesTable.find(typeName);
  if (ctIt == payload.customTypesTable.end()) {
    logSemanticErrors(ErrorCode::UndefinedVariable, selfExpr, {typeName});
    return nullptr;
  }

  std::shared_ptr<CustomTypeInfo> currentTypeInfo = ctIt->second;
  ResolvedType *currentResolvedType = &currentTypeInfo->type;

  for (const auto &fieldNode : selfExpr->fields) {
    const std::string &fieldName = extractIdentifierName(fieldNode.get());
    logInternal("Name received from self expression '" + fieldName + "'");

    // Look in the current type's members
    auto memIt = currentTypeInfo->members.find(fieldName);
    if (memIt == currentTypeInfo->members.end()) {
      logSemanticErrors(ErrorCode::NotaMemberOf, fieldNode.get(),
                        {fieldName, currentTypeInfo->type.resolvedName});
      return nullptr;
    }

    // Drill into this member's type for the next field
    currentResolvedType = &memIt->second->type;

    // If this member is a custom type, get its CustomTypeInfo for next
    // iteration
    bool isCustom = currentResolvedType->kind == DataType::RECORD ||
                    currentResolvedType->kind == DataType::ENUM;
    std::string lookUpName = getBaseTypeName(currentTypeInfo->type);

    if (isCustom) {
      auto ctiIt = payload.customTypesTable.find(lookUpName);
      if (ctiIt == payload.customTypesTable.end()) {
        logSemanticErrors(ErrorCode::UndefinedVariable, fieldNode.get(),
                          {lookUpName});
        return nullptr;
      }
      currentTypeInfo = ctiIt->second;
    } else {
      // For primitive types, stop drilling (next fields would be an error)
      currentTypeInfo = nullptr;
    }
  }

  return currentResolvedType;
}

void Semantics::walkSelfExpression(Node *node) {
  auto selfExpr = dynamic_cast<SelfExpression *>(node);
  if (!selfExpr)
    return;
  // Must be inside a component
  if (payload.currentTypeStack.empty() ||
      payload.currentTypeStack.back().type.kind != DataType::RECORD) {
    logSemanticErrors(ErrorCode::SelfOnlyInComponent, selfExpr);
    insertErrorMetaData(selfExpr);
    return;
  }

  const std::string &typeName = payload.currentTypeStack.back().typeName;

  ResolvedType *finalType = resolveSelfChain(selfExpr, typeName);
  if (!finalType)
    return;

  auto info = std::make_shared<SymbolInfo>();
  info->type().type = *finalType; // Copy resolved type
  info->type().isNullable = finalType->isNull;

  // Mutability etc only available if the last member is a custom member
  auto lastNode = selfExpr->fields.back().get();
  if (lastNode) {
    auto ctIt = payload.customTypesTable.find(typeName);
    if (ctIt != payload.customTypesTable.end()) {
      auto &members = ctIt->second->members;
      auto memIt = members.find(extractIdentifierName(lastNode));
      if (memIt != members.end()) {
        auto m = memIt->second;
        info->storage().isMutable = m->isMutable;
        info->storage().isConstant = m->isConstant;
        info->storage().isInitialized = m->isInitialised;
        info->type().memberIndex = m->memberIndex;
      }
    }
  }

  insertMetaData(selfExpr, info);
}

void Semantics::walkASMStatement(Node *node) {
  auto asmStmt = dynamic_cast<ASMStatement *>(node);
  if (!asmStmt)
    return;

  for (const auto &instruction : asmStmt->instructions) {
    auto asmInst = dynamic_cast<ASMInstruction *>(instruction.get());
    if (!asmInst)
      reportDevBug("Expected an assembly instruction ", instruction.get());

    for (const auto &constraint : asmInst->constraints) {
      auto asmConstraint = dynamic_cast<ASMConstraint *>(constraint.get());
      if (!asmConstraint)
        reportDevBug("Expected an assembly constraint", constraint.get());

      walker(asmConstraint->variable.get());
    }
  }
}
