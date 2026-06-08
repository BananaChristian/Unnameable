#include <algorithm>
#include <string>

#include "ast.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include "token.hpp"

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
      logSemanticErrors(ErrorCode::IllegalStmtInRecordOrComponent, field.get(),
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

    // If the variable analyzed is heap then register it as capture candidate
    if (fieldSymbol->storage().isHeap)
      typeInfo->captureCandidates.push_back(fieldSymbol->codegen().ID);

    // Build member info
    auto memInfo = std::make_shared<MemberInfo>();
    memInfo->memberName = fieldName;
    memInfo->type = fieldSymbol->type().type;
    memInfo->isMutable = fieldSymbol->storage().isMutable;
    memInfo->isConstant = fieldSymbol->storage().isConstant;
    memInfo->isInitialised = fieldSymbol->storage().isInitialized;
    memInfo->isPointer = fieldSymbol->type().isPointer;
    memInfo->isRef = fieldSymbol->type().isRef;
    memInfo->isNullable = fieldSymbol->type().isNullable;
    memInfo->isVolatile = fieldSymbol->storage().isVolatile;
    memInfo->isFnPtr = fieldSymbol->type().isFnPtr;
    memInfo->isHeap = fieldSymbol->storage().isHeap;
    memInfo->ID = fieldSymbol->codegen().ID;
    memInfo->typeNode = recordStmt;
    memInfo->node = field.get();
    memInfo->memberIndex = currentMemberIndex++;

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

void Semantics::walkInitConstructor(Node *node) {
  auto initStmt = dynamic_cast<InitStatement *>(node);
  if (!initStmt)
    return;

  // Must be inside a component
  if (payload.currentTypeStack.empty() ||
      payload.currentTypeStack.back().type.kind != DataType::COMPONENT) {
    logSemanticErrors(FloatingInit, initStmt);
    insertErrorMetaData(initStmt);
    return;
  }

  auto &currentComponent = payload.currentTypeStack.back();

  // Only one init constructor per component
  if (currentComponent.hasInitConstructor) {
    logSemanticErrors(ErrorCode::DuplicateInit, initStmt);
    insertErrorMetaData(initStmt);
    return;
  }
  currentComponent.hasInitConstructor = true;

  // init constructor is always void-returning
  auto initInfo = std::make_shared<SymbolInfo>();
  initInfo->type().type = ResolvedType::makeBase(DataType::VOID, "void");
  initInfo->func().returnType = ResolvedType::makeBase(DataType::VOID, "void");
  initInfo->func().isDeclaration = true;
  initInfo->func().isDefined = true;

  auto currentComponentName = currentComponent.typeName;
  std::vector<ResolvedType> initArgs;
  // Process constructor parameters
  payload.symbolTable.push_back({});
  for (const auto &arg : initStmt->constructor_args) {
    walkFunctionParameters(arg.get());
    // Storing the init args
    initArgs.push_back(inferNodeDataType(arg.get()));
  }
  initInfo->func().initArgs = initArgs;
  payload.componentInitArgs[currentComponentName] = initArgs;

  // Walk the constructor body
  if (initStmt->block) {
    walkBlockStatement(initStmt->block.get());
  }
  popScope();

  // Attach metadata
  insertMetaData(initStmt, initInfo);
}

// Resolves a self expression chain like self.pos.x.y
// Returns the ResolvedType of the final field, nullptr on error
ResolvedType *Semantics::resolveSelfChain(SelfExpression *selfExpr,
                                          const std::string &componentName) {
  // Look up the component's type info
  auto ctIt = payload.customTypesTable.find(componentName);
  if (ctIt == payload.customTypesTable.end()) {
    logSemanticErrors(ErrorCode::UndefinedVariable, selfExpr, {componentName});
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
                    currentResolvedType->kind == DataType::COMPONENT ||
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
      payload.currentTypeStack.back().type.kind != DataType::COMPONENT) {
    logSemanticErrors(ErrorCode::SelfOnlyInComponent, selfExpr);
    insertErrorMetaData(selfExpr);
    return;
  }

  const std::string &componentName = payload.currentTypeStack.back().typeName;

  ResolvedType *finalType = resolveSelfChain(selfExpr, componentName);
  if (!finalType)
    return;

  auto info = std::make_shared<SymbolInfo>();
  info->type().type = *finalType; // Copy resolved type
  info->type().isNullable = finalType->isNull;

  // Mutability etc only available if the last member is a custom member
  auto lastNode = selfExpr->fields.back().get();
  if (lastNode) {
    auto ctIt = payload.customTypesTable.find(componentName);
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

void Semantics::walkComponentStatement(Node *node) {
  auto componentStmt = dynamic_cast<ComponentStatement *>(node);
  if (!componentStmt)
    return;

  auto componentName = componentStmt->component_name->expression.TokenLiteral;
  bool isExportable = componentStmt->isExportable;

  insideComponent = true;

  if (payload.symbolTable[0].find(componentName) !=
      payload.symbolTable[0].end()) {
    logSemanticErrors(ErrorCode::DuplicateName, componentStmt, {componentName});
    insertErrorMetaData(componentStmt);
    return;
  }

  auto componentSymbol = std::make_shared<SymbolInfo>();
  componentSymbol->isComponent = true;
  auto componentTypeInfo = std::make_shared<CustomTypeInfo>();

  componentSymbol->type().type =
      ResolvedType::makeBase(DataType::COMPONENT, componentName);
  componentTypeInfo->type =
      ResolvedType::makeBase(DataType::COMPONENT, componentName);

  payload.symbolTable[0][componentName] = componentSymbol;
  payload.customTypesTable[componentName] = componentTypeInfo;
  insertMetaData(componentStmt, componentSymbol);

  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  int currentMemberIndex = 0;

  // Enter a new component scope
  payload.symbolTable.push_back({});
  payload.currentTypeStack.push_back(
      {.type = ResolvedType::makeBase(DataType::COMPONENT, componentName),
       .typeName = componentName,
       .hasInitConstructor = false,
       .members = members,
       .node = componentStmt});

  // Walk record injections
  for (const auto &injectedField : componentStmt->injectedFields) {
    if (!injectedField) {
      reportDevBug("Invalid inject record node ", nullptr);
    }

    auto injectStmt = dynamic_cast<InjectStatement *>(injectedField.get());
    if (!injectStmt) {
      reportDevBug("Invalid inject statement", injectedField.get());
    }

    // Mass inject case
    auto ident = dynamic_cast<Identifier *>(injectStmt->expr.get());
    if (ident) {
      auto identName = ident->expression.TokenLiteral;
      logInternal("Injecting fields from '" + identName + "' into '" +
                  componentName + "'");

      auto typeIt = payload.customTypesTable.find(identName);
      if (typeIt == payload.customTypesTable.end()) {
        logSemanticErrors(ErrorCode::UndefinedVariable, ident, {identName});
        return;
      }

      auto &importedMembers = typeIt->second->members;
      auto &currentScope = payload.symbolTable.back();

      std::vector<std::pair<std::string, std::shared_ptr<MemberInfo>>>
          sortedMembers(importedMembers.begin(), importedMembers.end());

      std::sort(sortedMembers.begin(), sortedMembers.end(),
                [](const auto &a, const auto &b) {
                  return a.second->memberIndex < b.second->memberIndex;
                });

      for (auto &[name, info] : sortedMembers) {
        // CHECK FOR COLLISION FIRST!
        if (members.find(name) != members.end()) {
          logSemanticErrors(ErrorCode::InjectionCollision, ident,
                            {name, identName});
          return;
        }

        // Also check in current scope (fields already defined in component)
        if (payload.symbolTable.back().find(name) !=
            payload.symbolTable.back().end()) {
          logSemanticErrors(ErrorCode::InjectionCollision, ident,
                            {name, identName});
          insertErrorMetaData(componentStmt);
          return;
        }

        auto memberCopy = std::make_shared<MemberInfo>();
        memberCopy->memberName = info->memberName;
        memberCopy->type = info->type;
        memberCopy->isNullable = info->isNullable;
        memberCopy->isMutable = info->isMutable;
        memberCopy->isConstant = info->isConstant;
        memberCopy->isVolatile = info->isVolatile;
        memberCopy->isInitialised = info->isInitialised;
        memberCopy->isFnPtr = info->isFnPtr;
        memberCopy->isRef = info->isRef;
        memberCopy->isPointer = info->isPointer;

        memberCopy->node = info->node;
        memberCopy->memberIndex = currentMemberIndex++;
        members[name] = memberCopy;

        componentTypeInfo->members = members;

        auto memSym = std::make_shared<SymbolInfo>();
        memSym->type().type = info->type;
        memSym->type().isNullable = info->isNullable;
        memSym->storage().isMutable = info->isMutable;
        memSym->storage().isConstant = info->isConstant;
        memSym->storage().isVolatile = info->isVolatile;
        memSym->storage().isInitialized = info->isInitialised;
        memSym->type().isFnPtr = info->isFnPtr;
        memSym->type().isPointer = info->isPointer;
        memSym->type().isArray = info->type.isArray();
        memSym->type().isRef = info->isRef;
        memSym->type().memberIndex = memberCopy->memberIndex;
        currentScope[name] = memSym;

        if (memberCopy->node) {
          metaData[memberCopy->node] = memSym;
          logInternal("Mapped member node '" + memberCopy->node->toString() +
                      "' symbol for '" + name + "'");
        } else {
          // If the node wasnt populated
          reportDevBug("Injected member '" + name + "' has no node", nullptr);
        }
      }
    }

    // Specific import case
    auto access = dynamic_cast<ComponentAccess *>(injectStmt->expr.get());
    if (access) {
      auto parent = dynamic_cast<Identifier *>(access->parent.get());
      auto child = dynamic_cast<Identifier *>(access->child.get());
      if (!parent || !child)
        return;

      auto recordName = extractIdentifierName(parent);
      auto memberName = extractIdentifierName(child);

      auto importedTypeIt = payload.customTypesTable.find(recordName);
      if (importedTypeIt != payload.customTypesTable.end()) {
        auto &importedMembers = importedTypeIt->second->members;
        auto memIt = importedMembers.find(memberName);
        if (memIt != importedMembers.end()) {
          if (members.find(memberName) != members.end()) {
            logSemanticErrors(ErrorCode::InjectionCollision, ident,
                              {memberName, recordName});
            insertErrorMetaData(componentStmt);
            return;
          }

          if (payload.symbolTable.back().find(memberName) !=
              payload.symbolTable.back().end()) {
            logSemanticErrors(ErrorCode::InjectionCollision, ident,
                              {memberName, recordName});
            insertErrorMetaData(componentStmt);
            return;
          }

          // Copy only the requested member
          std::shared_ptr<MemberInfo> memberCopy = memIt->second;
          memberCopy->memberIndex = currentMemberIndex++;
          members[memberName] = memberCopy;

          auto memSym = std::make_shared<SymbolInfo>();
          memSym->type().type = memIt->second->type;
          memSym->type().isNullable = memIt->second->isNullable;
          memSym->storage().isMutable = memIt->second->isMutable;
          memSym->storage().isConstant = memIt->second->isConstant;
          memSym->storage().isInitialized = memIt->second->isInitialised;
          memSym->storage().isVolatile = memIt->second->isVolatile;
          memSym->type().memberIndex = memberCopy->memberIndex;
          memSym->type().isFnPtr = memberCopy->isFnPtr;
          memSym->type().isRef = memberCopy->isRef;
          memSym->type().isPointer = memberCopy->isPointer;
          memSym->type().isArray = memberCopy->type.isArray();
          payload.symbolTable.back()[memberName] = memSym;

          componentTypeInfo->members = members;
        }
      }
    }
  }

  for (const auto &data : componentStmt->fields) {
    auto declaration = dynamic_cast<VariableDeclaration *>(data.get());
    std::string declName = extractDeclarationName(data.get());

    if (declaration) {
      walker(data.get());
      std::shared_ptr<SymbolInfo> declSym = resolveSymbolInfo(declName);
      if (!declSym) {
        reportDevBug("Failed to resolve member '" + declName + "'", data.get());
      }
      auto memInfo = std::make_shared<MemberInfo>();
      memInfo->memberName = declName;
      memInfo->type = declSym->type().type;
      memInfo->isNullable = declSym->type().isNullable;
      memInfo->isMutable = declSym->storage().isMutable;
      memInfo->isConstant = declSym->storage().isConstant;
      memInfo->isInitialised = declSym->storage().isInitialized;
      memInfo->node = data.get();
      memInfo->typeNode = componentStmt;
      memInfo->isVolatile = declSym->storage().isVolatile;
      memInfo->isFnPtr = declSym->type().isFnPtr;
      memInfo->isPointer = declSym->type().isPointer;
      memInfo->isRef = declSym->type().isRef;
      memInfo->memberIndex = currentMemberIndex++;

      members[declName] = memInfo;
      declSym->type().memberIndex = memInfo->memberIndex;

      metaData[declaration] = declSym;
      componentTypeInfo->members = members;
    } else {
      logSemanticErrors(ErrorCode::IllegalStmtInRecordOrComponent, data.get(),
                        {componentName});
    }
  }

  for (const auto &method : componentStmt->methods) {
    auto funcStmt = dynamic_cast<FunctionStatement *>(method.get());
    if (!funcStmt)
      continue;

    auto funcExpr =
        dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get());
    std::string funcName = "unknown";

    if (funcExpr) {
      funcName = extractIdentifierName(funcExpr);
      walker(funcExpr);
      auto metSym = resolveSymbolInfo(funcName);
      logInternal("Trying to insert '" + funcName + "'");
      if (metSym) {
        logInternal("Inserting component function expression ....");
        auto memInfo = std::make_shared<MemberInfo>();
        memInfo->memberName = funcName;
        memInfo->type = metSym->type().type;
        memInfo->isNullable = metSym->type().isNullable;
        memInfo->isMutable = metSym->storage().isMutable;
        memInfo->isExportable = metSym->isExportable;
        memInfo->returnType = metSym->func().returnType;
        memInfo->retFamilyID = metSym->codegen().ID;
        memInfo->isReturnHeap = metSym->storage().isHeap;
        memInfo->allocType = metSym->storage().allocType;
        memInfo->isVolatile = metSym->storage().isVolatile;
        memInfo->isFunction = true;
        memInfo->isDefined = true;
        memInfo->isDeclared = true;
        memInfo->paramTypes = metSym->func().paramTypes;
        memInfo->node = funcExpr;
        memInfo->typeNode = componentStmt;
        members[funcName] = memInfo;
      }
      componentTypeInfo->members =
          members; // Update the custom type table members
    } else {
      auto funcDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(
          funcStmt->funcExpr.get());
      if (funcDeclrExpr && funcDeclrExpr->funcDeclrStmt) {
        auto funcDeclrStmt = dynamic_cast<FunctionDeclaration *>(
            funcDeclrExpr->funcDeclrStmt.get());
        if (funcDeclrStmt) {
          funcName = funcDeclrStmt->function_name->expression.TokenLiteral;
        }
      }
      logSemanticErrors(ErrorCode::IllegalFunctionDeclaration, funcDeclrExpr);
    }
  }

  // Register component as a symbol
  componentSymbol->hasError = hasError;
  componentSymbol->members = members;

  componentTypeInfo->members = members;
  componentTypeInfo->typeName = componentName;
  componentTypeInfo->isExportable = isExportable;
  componentTypeInfo->type =
      ResolvedType::makeBase(DataType::COMPONENT, componentName);

  payload.customTypesTable[componentName] = componentTypeInfo;

  if (componentStmt->initConstructor.has_value())
    walkInitConstructor(componentStmt->initConstructor.value().get());

  if (members.empty()) {
    logInternal("Component members are empty at runtime");
  }

  // Exit component scope
  payload.currentTypeStack.pop_back();
  insideComponent = false;
  payload.symbolTable.pop_back();
}

void Semantics::walkNewComponentExpression(Node *node) {
  auto newExpr = dynamic_cast<NewComponentExpression *>(node);
  if (!newExpr)
    return;

  auto componentName = newExpr->component_name.TokenLiteral;

  if (isGlobalScope()) {
    logSemanticErrors(ErrorCode::GlobalInstantiation, newExpr);
    insertErrorMetaData(newExpr);
    return;
  }
  // We determine the type once and use it everywhere.
  ResolvedType componentType;
  bool componentExists = false;

  if (payload.customTypesTable.count(componentName)) {
    componentType = payload.customTypesTable[componentName]->type;
    componentExists = true;
  } else if (payload.ImportedComponentTable.count(componentName)) {
    componentType = payload.ImportedComponentTable[componentName]->type;
    componentExists = true;
  }

  if (!componentExists) {
    logSemanticErrors(ErrorCode::UndefinedVariable, newExpr, {componentName});
    insertErrorMetaData(newExpr);
    return;
  }

  // UNIFIED INIT LOOKUP
  // Bridge the gap between local and imported constructor signatures.
  std::vector<ResolvedType> expectedArgs;
  bool hasInitConstructor = false;

  if (payload.importedInits.count(componentName)) {
    hasInitConstructor = true;
    expectedArgs = payload.importedInits[componentName]->func().initArgs;
  } else if (payload.componentInitArgs.count(componentName)) {
    hasInitConstructor = true;
    expectedArgs = payload.componentInitArgs[componentName];
  }

  const auto &givenArgs = newExpr->arguments;

  // FLATTENED VALIDATION LOGIC
  if (!hasInitConstructor) {
    if (!givenArgs.empty()) {
      logSemanticErrors(ErrorCode::NoNeedForInit, newExpr, {componentName});
    }
  } else {
    // We have a constructor, check the count
    if (expectedArgs.size() != givenArgs.size()) {
      logSemanticErrors(ErrorCode::ArgumentSizeMismatch, newExpr,
                        {componentName, std::to_string(expectedArgs.size()),
                         std::to_string(givenArgs.size())});
    }

    // PAIRWISE TYPE CHECKING
    size_t checkCount = std::min(expectedArgs.size(), givenArgs.size());
    for (size_t i = 0; i < checkCount; ++i) {
      walker(givenArgs[i].get()); // Analyze the argument expression
      ResolvedType argType = inferNodeDataType(givenArgs[i].get());
      ResolvedType expectedType = expectedArgs[i];

      if (argType.kind != expectedType.kind) {
        logSemanticErrors(ErrorCode::ArgumentTypeMismatch, newExpr,
                          {std::to_string(i + 1), expectedType.resolvedName,
                           argType.resolvedName});
      }
    }
  }

  // Fetch the actual symbol for the IR Generator
  auto componentSym = resolveSymbolInfo(componentName);

  auto info = std::make_shared<SymbolInfo>();
  info->type().type = componentType;
  info->hasError = hasError;
  info->relations().componentSymbol = componentSym;

  // This map link is critical for the IR Generator to know what it's looking at
  insertMetaData(newExpr, info);
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
