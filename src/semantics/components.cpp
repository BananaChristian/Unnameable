#include "ast.hpp"
#include "semantics.hpp"
#include <algorithm>
#include <limits>

void Semantics::walkEnumStatement(Node *node) {
  auto enumStmt = dynamic_cast<EnumStatement *>(node);
  if (!enumStmt)
    return;

  std::cout << "[SEMANTIC LOG] Analysing enum class statement\n";

  std::string enumStmtName = enumStmt->enum_identifier->expression.TokenLiteral;
  bool isExportable = enumStmt->isExportable;

  // Check duplicate type name
  if (resolveSymbolInfo(enumStmtName)) {
    logSemanticErrors("Already used the name '" + enumStmtName + "'",
                      enumStmt->statement.line, enumStmt->statement.column);
    return;
  }

  // Set underlying type (default = i32)
  ResolvedType underLyingType{DataType::I32, "i32"};
  if (enumStmt->int_type.has_value()) {
    switch (enumStmt->int_type.value().type) {
    case TokenType::I8_KEYWORD:
      underLyingType = ResolvedType{DataType::I8, "i8"};
    case TokenType::U8_KEYWORD:
      underLyingType = ResolvedType{DataType::U8, "u8"};
    case TokenType::I16_KEYWORD:
      underLyingType = ResolvedType{DataType::I16, "i16"};
      break;
    case TokenType::U16_KEYWORD:
      underLyingType = ResolvedType{DataType::U16, "u16"};
      break;
    case TokenType::I32_KEYWORD:
      underLyingType = ResolvedType{DataType::I32, "i32"};
      break;
    case TokenType::U32_KEYWORD:
      underLyingType = ResolvedType{DataType::U32, "u32"};
      break;
    case TokenType::I64_KEYWORD:
      underLyingType = ResolvedType{DataType::I64, "i64"};
      break;
    case TokenType::U64_KEYWORD:
      underLyingType = ResolvedType{DataType::U64, "u64"};
      break;
    case TokenType::I128_KEYWORD:
      underLyingType = ResolvedType{DataType::I128, "i128"};
      break;
    case TokenType::U128_KEYWORD:
      underLyingType = ResolvedType{DataType::U128, "u128"};
      break;
    case TokenType::USIZE_KEYWORD:
      underLyingType = ResolvedType{DataType::USIZE, "usize"};
      break;
    default:
      underLyingType = ResolvedType{DataType::I32, "i32"};
      break;
    }
  }

  // Determine if underlying type is unsigned
  bool underlyingIsUnsigned = (underLyingType.kind == DataType::U8 ||
                               underLyingType.kind == DataType::U16 ||
                               underLyingType.kind == DataType::U32 ||
                               underLyingType.kind == DataType::U64 ||
                               underLyingType.kind == DataType::U128);

  // Push temporary scope
  symbolTable.push_back({});
  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  std::int64_t currentValue = 0;
  auto enumInfo = std::make_shared<SymbolInfo>();

  for (const auto &enumMember : enumStmt->enum_content) {
    if (!enumMember) {
      logSemanticErrors("Invalid enum member", enumStmt->statement.line,
                        enumStmt->statement.column);
      symbolTable.pop_back();
      return;
    }

    std::cout << "[SEMANTIC LOG] Analysing enum member node\n";
    std::string memberName = enumMember->enumMember;

    // Check for duplicate member name
    if (members.count(memberName) || resolveSymbolInfo(memberName)) {
      logSemanticErrors("Enum member name '" + memberName + "' already exists",
                        enumMember->token.line, enumMember->token.column);
      symbolTable.pop_back();
      return;
    }

    std::int64_t memberValue = 0;
    if (enumMember->value) {
      // Check all literal types
      Expression *literal = nullptr;
      TokenType literalType = TokenType::ILLEGAL; // Default invalid type
      std::string literalStr;
      if (auto i8Lit = dynamic_cast<I8Literal *>(enumMember->value.get())) {
        literal = i8Lit;
        literalType = TokenType::INT8;
        literalStr = i8Lit->expression.TokenLiteral;
      } else if (auto u8Lit =
                     dynamic_cast<U8Literal *>(enumMember->value.get())) {
        literal = u8Lit;
        literalType = TokenType::UINT8;
        literalStr = u8Lit->expression.TokenLiteral;
      } else if (auto i16Lit =
                     dynamic_cast<I16Literal *>(enumMember->value.get())) {
        literal = i16Lit;
        literalType = TokenType::INT16;
        literalStr = i16Lit->expression.TokenLiteral;
      } else if (auto u16Lit =
                     dynamic_cast<U16Literal *>(enumMember->value.get())) {
        literal = u16Lit;
        literalType = TokenType::UINT16;
        literalStr = u16Lit->expression.TokenLiteral;
      } else if (auto i32Lit =
                     dynamic_cast<I32Literal *>(enumMember->value.get())) {
        literal = i32Lit;
        literalType = TokenType::INT32;
        literalStr = i32Lit->expression.TokenLiteral;
      } else if (auto u32Lit =
                     dynamic_cast<U32Literal *>(enumMember->value.get())) {
        literal = u32Lit;
        literalType = TokenType::UINT32;
        literalStr = u32Lit->expression.TokenLiteral;
      } else if (auto i64Lit =
                     dynamic_cast<I64Literal *>(enumMember->value.get())) {
        literal = i64Lit;
        literalType = TokenType::INT64;
        literalStr = i64Lit->expression.TokenLiteral;
      } else if (auto u64Lit =
                     dynamic_cast<U64Literal *>(enumMember->value.get())) {
        literal = u64Lit;
        literalType = TokenType::UINT64;
        literalStr = u64Lit->expression.TokenLiteral;
      } else if (auto i128Lit =
                     dynamic_cast<I128Literal *>(enumMember->value.get())) {
        literal = i128Lit;
        literalType = TokenType::INT128;
        literalStr = i128Lit->expression.TokenLiteral;
      } else if (auto u128Lit =
                     dynamic_cast<U128Literal *>(enumMember->value.get())) {
        literal = u128Lit;
        literalType = TokenType::UINT128;
        literalStr = u128Lit->expression.TokenLiteral;
      } else if (auto isizeLit =
                     dynamic_cast<ISIZELiteral *>(enumMember->value.get())) {
        literal = isizeLit;
        literalType = TokenType::INTSIZE;
        literalStr = isizeLit->expression.TokenLiteral;
      } else if (auto usizeLit =
                     dynamic_cast<USIZELiteral *>(enumMember->value.get())) {
        literal = usizeLit;
        literalType = TokenType::UINTSIZE;
        literalStr = usizeLit->expression.TokenLiteral;
      } else {
        logSemanticErrors("Enum member value must be a i8,u8, i16, u16, i32, "
                          "u32, i64, u63, i128, u128 or isize, usize literal",
                          enumMember->token.line, enumMember->token.column);
        symbolTable.pop_back();
        return;
      }

      // Map literal type to DataType
      bool isUnsignedLiteral;
      switch (literalType) {
      case TokenType::INT8:
        isUnsignedLiteral = false;
        break;
      case TokenType::UINT8:
        isUnsignedLiteral = false;
        break;
      case TokenType::INT16:
        isUnsignedLiteral = false;
        break;
      case TokenType::UINT16:
        isUnsignedLiteral = true;
        break;
      case TokenType::INT32:
        isUnsignedLiteral = false;
        break;
      case TokenType::UINT32:
        isUnsignedLiteral = true;
        break;
      case TokenType::INT64:
        isUnsignedLiteral = false;
        break;
      case TokenType::UINT64:
        isUnsignedLiteral = true;
        break;
      case TokenType::INT128:
        isUnsignedLiteral = false;
        break;
      case TokenType::UINT128:
        isUnsignedLiteral = true;
        break;
      case TokenType::INTSIZE:
        isUnsignedLiteral = false;
        break;
      case TokenType::UINTSIZE:
        isUnsignedLiteral = true;
        break;
      default:
        logSemanticErrors("Invalid literal type for enum member",
                          enumMember->token.line, enumMember->token.column);
        symbolTable.pop_back();
        return;
      }

      // Validate signedness
      if (isUnsignedLiteral && !underlyingIsUnsigned) {
        logSemanticErrors("Unsigned literal '" + literalStr +
                              "' incompatible with signed underlying type",
                          enumMember->token.line, enumMember->token.column);
        symbolTable.pop_back();
        return;
      }
      if (!isUnsignedLiteral && underlyingIsUnsigned) {
        logSemanticErrors("Signed literal '" + literalStr +
                              "' incompatible with unsigned underlying type",
                          enumMember->token.line, enumMember->token.column);
        symbolTable.pop_back();
        return;
      }

      // Parse and validate range
      try {
        if (isUnsignedLiteral) {
          std::uint64_t parsedValue = std::stoull(literalStr);
          memberValue = static_cast<std::int64_t>(parsedValue);
          if (underLyingType.kind == DataType::U8 &&
              parsedValue > std::numeric_limits<std::uint8_t>::max())
            throw std::out_of_range("Value out of range for u8");
          if (underLyingType.kind == DataType::U16 &&
              parsedValue > std::numeric_limits<std::uint16_t>::max())
            throw std::out_of_range("Value out of range for u16");
          if (underLyingType.kind == DataType::U32 &&
              parsedValue > std::numeric_limits<std::uint32_t>::max())
            throw std::out_of_range("Value out of range for u32");
          // No range check for U64, U128, USIZE yet
        } else {
          memberValue = std::stoll(literalStr);
          if (underLyingType.kind == DataType::I8 &&
              (memberValue < std::numeric_limits<std::int8_t>::min() ||
               memberValue > std::numeric_limits<std::int8_t>::max()))
            throw std::out_of_range("Value out of range for i8");
          if (underLyingType.kind == DataType::I16 &&
              (memberValue < std::numeric_limits<std::int16_t>::min() ||
               memberValue > std::numeric_limits<std::int16_t>::max()))
            throw std::out_of_range("Value out of range for i16");
          if (underLyingType.kind == DataType::I32 &&
              (memberValue < std::numeric_limits<std::int32_t>::min() ||
               memberValue > std::numeric_limits<std::int32_t>::max()))
            throw std::out_of_range("Value out of range for i32");
          // No range check for I64, I128, ISIZE YET
        }
      } catch (const std::invalid_argument &) {
        logSemanticErrors("Invalid integer literal '" + literalStr + "'",
                          enumMember->token.line, enumMember->token.column);
        symbolTable.pop_back();
        return;
      } catch (const std::out_of_range &e) {
        logSemanticErrors(
            "Value '" + literalStr +
                "' out of range for underlying type: " + std::string(e.what()),
            enumMember->token.line, enumMember->token.column);
        symbolTable.pop_back();
        return;
      }
    } else {
      memberValue = currentValue;
      if (underlyingIsUnsigned && memberValue < 0) {
        logSemanticErrors("Negative auto-incremented value '" +
                              std::to_string(memberValue) +
                              "' invalid for unsigned underlying type",
                          enumMember->token.line, enumMember->token.column);
        symbolTable.pop_back();
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
    info->constantValue = static_cast<int>(memberValue);
    info->parentType = ResolvedType{DataType::ENUM, enumStmtName};
    members[memberName] = info;

    // Add member to local scope
    enumInfo->type = underLyingType;
    enumInfo->isConstant = true;
    enumInfo->isInitialized = true;
    enumInfo->isExportable = isExportable;
    symbolTable.back()[memberName] = enumInfo;

    currentValue = memberValue + 1;
  }

  // Store enum type info
  auto typeInfo = std::make_shared<CustomTypeInfo>();

  typeInfo->typeName = enumStmtName;
  typeInfo->type = ResolvedType{DataType::ENUM, enumStmtName};
  typeInfo->underLyingType = underLyingType.kind;
  typeInfo->members = members;
  typeInfo->isExportable = isExportable;
  customTypesTable[enumStmtName] = typeInfo;

  // Add enum type to parent scope
  auto generalInfo = std::make_shared<SymbolInfo>();
  generalInfo->type = ResolvedType{DataType::ENUM, enumStmtName};
  generalInfo->isConstant = false;
  generalInfo->isInitialized = true;
  generalInfo->isExportable = isExportable;
  symbolTable[symbolTable.size() - 2][enumStmtName] = generalInfo;

  metaData[enumStmt] = generalInfo;

  // Pop temporary scope
  symbolTable.pop_back();
}

void Semantics::walkRecordStatement(Node *node) {
  auto recordStmt = dynamic_cast<RecordStatement *>(node);
  if (!recordStmt)
    return;

  std::cout << "[SEMANTIC LOG] Analyzing data statement: "
            << recordStmt->toString() << "\n";

  // Get block name
  std::string recordName = recordStmt->recordName->expression.TokenLiteral;
  bool hasError=false;
  bool isExportable = recordStmt->isExportable;

  // Ensure name not already used
  if (resolveSymbolInfo(recordName)) {
    logSemanticErrors("Already used the name '" + recordName + "'",
                      recordStmt->statement.line, recordStmt->statement.column);
    return;
  }

  // Setup mutability
  bool isBlockMutable = (recordStmt->mutability == Mutability::MUTABLE);
  bool isBlockConstant = (recordStmt->mutability == Mutability::CONSTANT);

  // Create new local scope for analysis
  insideRecord = true;
  symbolTable.push_back({});

  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> recordMembers;

  // Analyze each field
  for (const auto &field : recordStmt->fields) {
    auto letStmt = dynamic_cast<LetStatement *>(field.get());
    auto ptrStmt = dynamic_cast<PointerStatement *>(field.get());
    auto refStmt = dynamic_cast<ReferenceStatement *>(field.get());
    auto arrStmt = dynamic_cast<ArrayStatement *>(field.get());

    // Special error handle in case a user tries to use a reference inside a
    // record
    if (refStmt) {
      logSemanticErrors("Cannot use references inside record '" + recordName +
                            "'",
                        field->statement.line, field->statement.column);
      hasError=true;
      continue;
    }

    bool isDecl = letStmt || ptrStmt || arrStmt;

    // Double check incase the parser messed up and leaked wrong statements
    if (!isDecl) {
      logSemanticErrors("Invalid statement inside record '" + recordName + "'",
                        field->statement.line, field->statement.column);
      hasError=true;
      continue; // skip bad field but keep going
    }

    // Apply block mutability if set
    if (isBlockMutable) {
      if (letStmt)
        letStmt->mutability = Mutability::MUTABLE;
      else if (ptrStmt)
        ptrStmt->mutability = Mutability::MUTABLE;
      else if (refStmt)
        refStmt->mutability = Mutability::MUTABLE;
      else if (arrStmt)
        arrStmt->mutability = Mutability::MUTABLE;
    } else if (isBlockConstant) {
      if (letStmt)
        letStmt->mutability = Mutability::CONSTANT;
      else if (ptrStmt)
        ptrStmt->mutability = Mutability::CONSTANT;
      else if (refStmt)
        refStmt->mutability = Mutability::CONSTANT;
      else if (arrStmt)
        arrStmt->mutability = Mutability::CONSTANT;
    }

    // Walk the let statement to register it in the scope
    walker(field.get());

    // Now retrieve its symbol
    auto fieldName = extractDeclarationName(field.get());
    auto fieldSymbol = resolveSymbolInfo(fieldName);
    if (!fieldSymbol) {
      logSemanticErrors("Declaration statement '" + fieldName +
                            "' was not analyzed properly",
                        field->statement.line, field->statement.column);
      hasError=true;
      continue;
    }

    bool isHeap = fieldSymbol->isHeap;
    bool isDheap = fieldSymbol->isDheap;
    StorageType memberStorage;
    if (isHeap || isDheap) {
      memberStorage = StorageType::HEAP;
    } else {
      memberStorage = StorageType::STACK;
    }

    // Build member info
    auto memInfo = std::make_shared<MemberInfo>();
    memInfo->memberName = fieldName;
    memInfo->type = fieldSymbol->type;
    memInfo->isMutable = fieldSymbol->isMutable;
    memInfo->isConstant = fieldSymbol->isConstant;
    memInfo->isInitialised = fieldSymbol->isInitialized;
    memInfo->isHeap = isHeap;
    memInfo->storage = memberStorage;
    memInfo->typeNode = recordStmt;
    memInfo->node = field.get();

    // Insert into members map
    recordMembers[fieldName] = memInfo;

    std::cout << "[SEMANTIC LOG] Added field '" << fieldName
              << "' to data block '" << recordName << "'\n";
  }

  int currentMemberIndex = 0;
  for (auto &kv : recordMembers) {
    kv.second->memberIndex = currentMemberIndex++;
  }

  // Build symbol info for the whole block
  auto recordInfo = std::make_shared<SymbolInfo>();
  recordInfo->type = ResolvedType{DataType::RECORD, recordName};
  recordInfo->isMutable = isBlockMutable;
  recordInfo->isConstant = isBlockConstant;
  recordInfo->isExportable = isExportable;
  recordInfo->members = recordMembers;
  recordInfo->hasError=hasError;
  recordInfo->isDataBlock = true;

  for (auto &kv : recordInfo->members) {
    auto it = recordMembers.find(kv.first);
    if (it != recordMembers.end())
      kv.second->memberIndex = it->second->memberIndex;
  }

  // Build customtype info
  auto typeInfo = std::make_shared<CustomTypeInfo>();

  typeInfo->typeName = recordName;
  typeInfo->type = ResolvedType{DataType::RECORD, recordName},
  typeInfo->isExportable = isExportable;
  typeInfo->members = recordMembers;

  // Propagate indices to customTypesTable
  for (auto &kv : typeInfo->members) {
    auto it = recordMembers.find(kv.first);
    if (it != recordMembers.end())
      kv.second->memberIndex = it->second->memberIndex;
  }

  // Store results
  metaData[recordStmt] = recordInfo;
  symbolTable[0][recordName] = recordInfo;
  customTypesTable[recordName] = typeInfo;

  // Pop local scope
  insideRecord = false;
  popScope();
}

void Semantics::walkInstanceExpression(Node *node) {
  auto instExpr = dynamic_cast<InstanceExpression *>(node);
  if (!instExpr)
    return;

  auto instName = instExpr->blockIdent->expression.TokenLiteral;
  auto line = instExpr->blockIdent->expression.line;
  auto col = instExpr->blockIdent->expression.column;
  bool hasError = false;

  auto instSym = resolveSymbolInfo(instName);

  if (!instSym) {
    logSemanticErrors("Undefined identifier '" + instName + "'", line, col);
    hasError = true;
    return;
  }

  if (instSym->type.kind != DataType::RECORD) {
    logSemanticErrors("'" + instName + "' is not a record ", line, col);
    hasError = true;
  }

  // Dealing with arguments if they exist
  if (!instExpr->fields.empty()) {
    auto members = instSym->members;
    for (const auto &field : instExpr->fields) {
      auto fieldNode = dynamic_cast<AssignmentStatement *>(field.get());
      auto fieldName = fieldNode->identifier->expression.TokenLiteral;

      auto it = members.find(fieldName);
      if (it == members.end()) {
        logSemanticErrors("Field '" + fieldName + "' does not exist in '" +
                              instName + "'",
                          line, col);
        hasError = true;
        continue;
      }

      auto expectedFieldType = it->second->type;

      if (auto *nullLit = dynamic_cast<NullLiteral *>(fieldNode->value.get())) {
        auto nullSym = std::make_shared<SymbolInfo>();
        nullSym->type = expectedFieldType;
        nullSym->isDefinitelyNull = true;

        metaData[nullLit] = nullSym;
        std::cout << "[SEMANTIC] Tagged Null at " << (void *)nullLit << " as "
                  << expectedFieldType.resolvedName << "\n";
      } else {
        walker(fieldNode->value.get());
      }
    }
  }

  ResolvedType instType = instSym->type;
  std::cout << "Instance type: " << instType.resolvedName << "\n";

  auto instInfo = std::make_shared<SymbolInfo>();
  instInfo->type = instType;
  instInfo->hasError = hasError;

  metaData[instExpr] = instInfo;
}

void Semantics::walkBehaviorStatement(Node *node) {
  auto behaviorStmt = dynamic_cast<BehaviorStatement *>(node);
  if (!behaviorStmt)
    return;

  std::cout << "[SEMANTIC LOG] Analyzing behavior statement: "
            << behaviorStmt->toString() << "\n";

  // Get behavior block name
  std::string behaviorName =
      behaviorStmt->behaviorBlockName->expression.TokenLiteral;
  bool isExportable = behaviorStmt->isExportable;

  bool hasError = false;
  insideBehavior = true;

  // Ensure name not already used
  if (resolveSymbolInfo(behaviorName)) {
    logSemanticErrors("Already used the name '" + behaviorName + "'",
                      behaviorStmt->behaviorBlockName->expression.line,
                      behaviorStmt->behaviorBlockName->expression.column);
    return;
  }

  // Push a new scope for analysis
  symbolTable.push_back({});

  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> behaviorMembers;

  // Analyze each function inside the behavior
  for (const auto &func : behaviorStmt->functions) {
    auto funcStmt = dynamic_cast<FunctionStatement *>(func.get());
    if (!funcStmt) {
      logSemanticErrors(
          "Only function statements are allowed inside a behavior block",
          func->statement.line, func->statement.column);
      continue; // keep checking the rest
    }

    // If we get a function expression this isnt allowed inside behavior blocks
    if (auto funcExpr =
            dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get())) {
      auto exprSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
      if (!exprSym) {
        logSemanticErrors(
            "Function definition '" + funcExpr->func_key.TokenLiteral +
                "' prohibited in behavior blocks",
            funcExpr->expression.line, funcExpr->expression.column);
        hasError = true;
      }
    }

    // Let the walker handle function analysis
    auto funcDeclExpr =
        dynamic_cast<FunctionDeclarationExpression *>(funcStmt->funcExpr.get());
    walker(funcDeclExpr);

    auto funcInfo = std::make_shared<MemberInfo>();

    if (funcDeclExpr) {
      auto funcDecl = dynamic_cast<FunctionDeclaration *>(
          funcDeclExpr->funcDeclrStmt.get());
      if (!funcDecl) {
        logSemanticErrors("Invalid function declaration inside behavior block",
                          funcDeclExpr->expression.line,
                          funcDeclExpr->expression.column);
        continue;
      }

      auto declSym =
          resolveSymbolInfo(funcDecl->function_name->expression.TokenLiteral);
      if (!declSym) {
        logSemanticErrors("Function declaration '" +
                              funcDecl->function_name->expression.TokenLiteral +
                              "' was not analyzed properly",
                          funcDecl->statement.line, funcDecl->statement.column);
        continue;
      }

      funcInfo->memberName = funcDecl->function_name->expression.TokenLiteral;
      funcInfo->type = declSym->type;
      funcInfo->paramTypes = declSym->paramTypes;
      funcInfo->returnType = declSym->returnType;
      funcInfo->isExportable = declSym->isExportable;
      funcInfo->isFunction = true;
      funcInfo->isDeclared = true;
      funcInfo->node = funcStmt;

      behaviorMembers[funcDecl->function_name->expression.TokenLiteral] =
          funcInfo;

      std::cout << "[SEMANTIC LOG] Added function declaration '"
                << funcDecl->function_name->expression.TokenLiteral
                << "' to behavior '" << behaviorName << "'\n";
    }
  }

  // Build symbol info
  auto sym = std::make_shared<SymbolInfo>();
  sym->isBehavior = true;
  sym->isExportable = isExportable;
  sym->members = behaviorMembers;

  // Store results
  symbolTable[0][behaviorName] =
      sym; // All behavior blocks are hoisted to global scope
  metaData[behaviorStmt] = sym;

  insideBehavior = false;
  // Pop scope
  popScope();
}

void Semantics::walkInitConstructor(Node *node) {
  auto initStmt = dynamic_cast<InitStatement *>(node);
  if (!initStmt)
    return;

  std::cout << "[SEMANTIC LOG] Analyzing init constructor\n";

  // Must be inside a component

  if (currentTypeStack.empty() ||
      currentTypeStack.back().type.kind != DataType::COMPONENT) {
    std::cout << "TYPE STACK IS EMPTY\n";
    logSemanticErrors("`init` constructor must be declared inside a component",
                      initStmt->statement.line, initStmt->statement.column);
    return;
  }

  auto &currentComponent = currentTypeStack.back();

  // Only one init constructor per component
  if (currentComponent.hasInitConstructor) {
    logSemanticErrors("Component '" + currentComponent.typeName +
                          "' already has an init constructor",
                      initStmt->token.line, initStmt->token.column);
    return;
  }
  currentComponent.hasInitConstructor = true;

  // init constructor is always void-returning
  auto initInfo = std::make_shared<SymbolInfo>();
  initInfo->type = ResolvedType{DataType::VOID, "void"};
  initInfo->returnType = ResolvedType{DataType::VOID, "void"};
  initInfo->isDeclaration = true;
  initInfo->isDefined = true;

  auto currentComponentName = currentComponent.typeName;
  std::vector<ResolvedType> initArgs;
  // Process constructor parameters
  symbolTable.push_back({});
  for (const auto &arg : initStmt->constructor_args) {
    walker(arg.get());
    // Storing the init args
    initArgs.push_back(inferNodeDataType(arg.get()));
  }
  initInfo->initArgs = initArgs;
  componentInitArgs[currentComponentName] = initArgs;

  // Walk the constructor body
  if (initStmt->block) {
    walkBlockStatement(initStmt->block.get());
  }
  popScope();

  // Attach metadata
  metaData[initStmt] = initInfo;

  std::cout << "[SEMANTIC LOG] Init constructor added to component '"
            << currentComponent.typeName << "'\n";
}

// Resolves a self expression chain like self.pos.x.y
// Returns the ResolvedType of the final field, nullptr on error
ResolvedType *Semantics::resolveSelfChain(SelfExpression *selfExpr,
                                          const std::string &componentName) {
  // Look up the component's type info
  auto ctIt = customTypesTable.find(componentName);
  if (ctIt == customTypesTable.end()) {
    logSemanticErrors("Component '" + componentName + "' not found",
                      selfExpr->expression.line, selfExpr->expression.column);
    return nullptr;
  }

  std::shared_ptr<CustomTypeInfo> currentTypeInfo = ctIt->second;
  ResolvedType *currentResolvedType = &currentTypeInfo->type;

  for (const auto &fieldNode : selfExpr->fields) {
    auto ident = dynamic_cast<Identifier *>(fieldNode.get());
    if (!ident) {
      logSemanticErrors("Expected identifier in 'self' field chain",
                        selfExpr->expression.line, selfExpr->expression.column);
      return nullptr;
    }

    const std::string &fieldName = ident->identifier.TokenLiteral;
    std::cout << "NAME BEING GOTTEN FROM SELF EXPRESSION: " << fieldName
              << "\n";

    // Look in the current type's members
    auto memIt = currentTypeInfo->members.find(fieldName);
    if (memIt == currentTypeInfo->members.end()) {
      logSemanticErrors("'" + fieldName + "' does not exist in type '" +
                            currentTypeInfo->type.resolvedName + "'",
                        ident->identifier.line, ident->identifier.column);
      return nullptr;
    }

    // Drill into this member's type for the next field
    currentResolvedType = &memIt->second->type;

    // If this member is a custom type, get its CustomTypeInfo for next
    // iteration
    bool isCustom = currentResolvedType->kind == DataType::RECORD ||
                    currentResolvedType->kind == DataType::COMPONENT ||
                    currentResolvedType->kind == DataType::ENUM;
    if (isCustom) {
      auto ctiIt = customTypesTable.find(currentResolvedType->resolvedName);
      if (ctiIt == customTypesTable.end()) {
        logSemanticErrors("Type info for '" +
                              currentResolvedType->resolvedName + "' not found",
                          ident->identifier.line, ident->identifier.column);
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

  std::cout << "[SEMANTIC LOG] Analysing field access expression: "
            << selfExpr->toString() << "\n";

  // Must be inside a component
  if (currentTypeStack.empty() ||
      currentTypeStack.back().type.kind != DataType::COMPONENT) {
    logSemanticErrors("'self' cannot be used outside a component",
                      selfExpr->expression.line, selfExpr->expression.column);
    return;
  }

  const std::string &componentName = currentTypeStack.back().typeName;

  ResolvedType *finalType = resolveSelfChain(selfExpr, componentName);
  if (!finalType)
    return;

  auto info = std::make_shared<SymbolInfo>();
  info->type = *finalType; // Copy resolved type
  info->isNullable = finalType->isNull;

  // Mutability etc only available if the last member is a custom member
  auto lastIdent = dynamic_cast<Identifier *>(selfExpr->fields.back().get());
  if (lastIdent) {
    auto ctIt = customTypesTable.find(componentName);
    if (ctIt != customTypesTable.end()) {
      auto &members = ctIt->second->members;
      auto memIt = members.find(lastIdent->identifier.TokenLiteral);
      if (memIt != members.end()) {
        auto m = memIt->second;
        info->isMutable = m->isMutable;
        info->isConstant = m->isConstant;
        info->isInitialized = m->isInitialised;
        info->memberIndex = m->memberIndex;
      }
    }
  }

  metaData[selfExpr] = info;
}

void Semantics::walkComponentStatement(Node *node) {
  auto componentStmt = dynamic_cast<ComponentStatement *>(node);
  if (!componentStmt)
    return;

  auto componentName = componentStmt->component_name->expression.TokenLiteral;
  bool isExportable = componentStmt->isExportable;

  bool hasError = false;
  insideComponent = true;

  if (symbolTable[0].find(componentName) != symbolTable[0].end()) {
    logSemanticErrors("Component name'" + componentName + "' already in use",
                      componentStmt->statement.line,
                      componentStmt->statement.column);
    return;
  }

  auto componentSymbol = std::make_shared<SymbolInfo>();
  componentSymbol->isComponent = true;
  auto componentTypeInfo = std::make_shared<CustomTypeInfo>();

  componentSymbol->type = ResolvedType{DataType::COMPONENT, componentName};
  symbolTable[0][componentName] = componentSymbol;

  customTypesTable[componentName] = componentTypeInfo;

  metaData[componentStmt] = componentSymbol;

  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  int currentMemberIndex = 0;

  // Enter a new component scope
  symbolTable.push_back({});
  currentTypeStack.push_back(
      {.type = ResolvedType{DataType::COMPONENT, componentName},
       .typeName = componentName,
       .hasInitConstructor = false,
       .members = members,
       .node = componentStmt});

  // Walk data/behavior imports
  for (const auto &usedData : componentStmt->usedDataBlocks) {
    if (!usedData) {
      std::cout << "Invalid used record node\n";
      return;
    }

    auto useStmt = dynamic_cast<UseStatement *>(usedData.get());
    if (!useStmt) {
      std::cout << "Invalid use statement \n";
      continue;
    }

    // Mass import case
    auto ident = dynamic_cast<Identifier *>(useStmt->blockNameOrCall.get());
    if (ident) {
      auto identName = ident->expression.TokenLiteral;
      std::cout << "IMPORTING DATA FROM: " << identName << "\n";

      auto typeIt = customTypesTable.find(identName);
      if (typeIt == customTypesTable.end()) {
        logSemanticErrors("Record with name '" + identName + "' does not exist",
                          ident->expression.line, ident->expression.column);
        hasError = true;
        return;
      }

      auto &importedMembers = typeIt->second->members;
      auto &currentScope = symbolTable.back();
      for (auto &[name, info] : importedMembers) {
        auto memberCopy = std::make_shared<MemberInfo>();
        memberCopy->memberName = info->memberName;
        memberCopy->type = info->type;
        memberCopy->isNullable = info->isNullable;
        memberCopy->isMutable = info->isMutable;
        memberCopy->isConstant = info->isConstant;
        memberCopy->storage = StorageType::STACK;
        memberCopy->isInitialised = info->isInitialised;

        memberCopy->node = info->node;
        memberCopy->memberIndex = currentMemberIndex++;
        members[name] = memberCopy;

        componentTypeInfo->members = members;

        auto memSym = std::make_shared<SymbolInfo>();
        memSym->type = info->type;
        memSym->isNullable = info->isNullable;
        memSym->isMutable = info->isMutable;
        memSym->isConstant = info->isConstant;
        memSym->isInitialized = info->isInitialised;
        memSym->storage = StorageType::STACK;
        memSym->memberIndex = memberCopy->memberIndex;
        currentScope[name] = memSym;

        if (memberCopy->node) {
          metaData[memberCopy->node] = memSym;
          std::cout << "[SEMANTIC LOG] mapped member node " << memberCopy->node
                    << " -> symbol for '" << name << "'\n";
        } else {
          // If the node wasnt populated
          std::cout << "[SEMANTIC WARN] imported member '" << name
                    << "' has no node\n";
        }
      }
    }

    // Specific import case
    auto infixExpr =
        dynamic_cast<InfixExpression *>(useStmt->blockNameOrCall.get());
    if (infixExpr) {
      auto leftIdent =
          dynamic_cast<Identifier *>(infixExpr->left_operand.get());
      auto rightIdent =
          dynamic_cast<Identifier *>(infixExpr->right_operand.get());
      if (!leftIdent || !rightIdent)
        return;

      TokenType op = infixExpr->operat.type;

      if (op != TokenType::AT) {
        logSemanticErrors("Invalid operator '" +
                              infixExpr->operat.TokenLiteral +
                              "' for explicit use statement import",
                          infixExpr->operat.line, infixExpr->operat.column);
        hasError = true;
      }

      auto dataName = leftIdent->expression.TokenLiteral;
      auto memberName = rightIdent->expression.TokenLiteral;

      auto importedTypeIt = customTypesTable.find(dataName);
      if (importedTypeIt != customTypesTable.end()) {
        auto &importedMembers = importedTypeIt->second->members;
        auto memIt = importedMembers.find(memberName);
        if (memIt != importedMembers.end()) {
          // Copy only the requested member
          std::shared_ptr<MemberInfo> memberCopy = memIt->second;
          memberCopy->memberIndex = currentMemberIndex++;
          members[memberName] = memberCopy;

          auto memSym = std::make_shared<SymbolInfo>();
          memSym->type = memIt->second->type;
          memSym->isNullable = memIt->second->isNullable;
          memSym->isMutable = memIt->second->isMutable;
          memSym->isConstant = memIt->second->isConstant;
          memSym->isInitialized = memIt->second->isInitialised;
          memSym->storage = StorageType::STACK;
          memSym->memberIndex = memberCopy->memberIndex;
          symbolTable.back()[memberName] = memSym;

          componentTypeInfo->members = members;
        }
      }
    }
  }

  for (const auto &usedBehavior : componentStmt->usedBehaviorBlocks) {
    if (!usedBehavior) {
      std::cout << "Invalid used behavior node\n";
      return;
    }

    auto useStmt = dynamic_cast<UseStatement *>(usedBehavior.get());
    if (!useStmt) {
      std::cout << "Invalid use statement\n";
      continue;
    }

    // Mass import case
    auto ident = dynamic_cast<Identifier *>(useStmt->blockNameOrCall.get());
    if (ident) {
      auto identName = ident->expression.TokenLiteral;
      std::cout << "IMPORTING DATA FROM: " << identName << "\n";

      auto typeIt = customTypesTable.find(identName);
      if (typeIt == customTypesTable.end()) {
        logSemanticErrors("Behavior block with name '" + identName +
                              "' does not exist",
                          ident->expression.line, ident->expression.column);
        hasError = true;
        return;
      }

      auto &importedMembers = typeIt->second->members;
      auto &currentScope = symbolTable.back();
      for (auto &[name, info] : importedMembers) {
        auto memberCopy = std::make_shared<MemberInfo>();
        memberCopy->memberName = info->memberName;
        memberCopy->type = info->type;
        memberCopy->isNullable = info->isNullable;
        memberCopy->isMutable = info->isMutable;
        memberCopy->isConstant = info->isConstant;
        memberCopy->isInitialised = info->isInitialised;
        memberCopy->isFunction = info->isFunction;
        memberCopy->paramTypes = info->paramTypes;
        memberCopy->returnType = info->returnType;
        memberCopy->isDeclared = info->isDeclared;
        memberCopy->isDefined = true;

        memberCopy->node = info->node;
        memberCopy->memberIndex = currentMemberIndex++;
        members[name] = memberCopy;

        auto memSym = std::make_shared<SymbolInfo>();
        memSym->type = info->type;
        memSym->isNullable = info->isNullable;
        memSym->isMutable = info->isMutable;
        memSym->isConstant = info->isConstant;
        memSym->isInitialized = info->isInitialised;
        memSym->memberIndex = memberCopy->memberIndex;
        memSym->paramTypes = info->paramTypes;
        memSym->returnType = info->returnType;
        memSym->isDeclaration = info->isDeclared;
        memSym->isDefined = true;
        currentScope[name] = memSym;

        componentTypeInfo->members =
            members; // Update the typeInfo member storage

        if (memberCopy->node) {
          metaData[memberCopy->node] = memSym;
          std::cout << "[SEMANTIC LOG] mapped member node " << memberCopy->node
                    << " -> symbol for '" << name << "'\n";
        } else {
          // If the node wasnt populated
          std::cout << "[SEMANTIC WARN] imported member '" << name
                    << "' has no node\n";
        }
      }
    }

    // Specific import case
    auto infixExpr =
        dynamic_cast<InfixExpression *>(useStmt->blockNameOrCall.get());
    if (infixExpr) {
      auto leftIdent =
          dynamic_cast<Identifier *>(infixExpr->left_operand.get());
      auto rightIdent =
          dynamic_cast<Identifier *>(infixExpr->right_operand.get());
      if (!leftIdent || !rightIdent)
        return;

      TokenType op = infixExpr->operat.type;

      if (op != TokenType::AT) {
        logSemanticErrors("Invalid operator '" +
                              infixExpr->operat.TokenLiteral +
                              "' for explicit use statement import",
                          infixExpr->operat.line, infixExpr->operat.column);
        hasError = true;
      }

      auto methodName = leftIdent->expression.TokenLiteral;
      auto memberName = rightIdent->expression.TokenLiteral;

      auto importedTypeIt = customTypesTable.find(methodName);
      if (importedTypeIt != customTypesTable.end()) {
        auto &importedMembers = importedTypeIt->second->members;
        auto memIt = importedMembers.find(memberName);
        if (memIt != importedMembers.end()) {
          // Copy only the requested member
          std::shared_ptr<MemberInfo> memberCopy = memIt->second;
          memberCopy->memberIndex = currentMemberIndex++;
          memberCopy->isFunction = memIt->second->isFunction;
          memberCopy->returnType = memIt->second->returnType;
          memberCopy->isDeclared = memIt->second->isDeclared;
          members[memberName] = memberCopy;

          auto memSym = std::make_shared<SymbolInfo>();
          memSym->type = memIt->second->type;
          memSym->isNullable = memIt->second->isNullable;
          memSym->isMutable = memIt->second->isMutable;
          memSym->isConstant = memIt->second->isConstant;
          memSym->isInitialized = memIt->second->isInitialised;
          memSym->memberIndex = memberCopy->memberIndex;
          memSym->returnType = memberCopy->returnType;
          memSym->paramTypes = memberCopy->paramTypes;
          memSym->isDefined = true;
          symbolTable.back()[memberName] = memSym;
          componentTypeInfo->members = members; // Update the typeInfo members
        }
      }
    }
  }

  for (const auto &data : componentStmt->privateData) {
    auto letStmt = dynamic_cast<LetStatement *>(data.get());

    if (letStmt) {
      walker(letStmt);
      auto letSym = resolveSymbolInfo(letStmt->ident_token.TokenLiteral);
      if (!letSym) {
        logSemanticErrors("Failed to resolve private data '" +
                              letStmt->ident_token.TokenLiteral + "'",
                          letStmt->ident_token.line,
                          letStmt->ident_token.column);
        hasError = true;
        continue;
      }
      auto memInfo = std::make_shared<MemberInfo>();
      memInfo->memberName = letStmt->ident_token.TokenLiteral;
      memInfo->type = letSym->type;
      memInfo->isNullable = letSym->isNullable;
      memInfo->isMutable = letSym->isMutable;
      memInfo->isConstant = letSym->isConstant;
      memInfo->isInitialised = letSym->isInitialized;
      memInfo->storage = letSym->storage;
      memInfo->node = letStmt;
      memInfo->typeNode = componentStmt;
      memInfo->memberIndex = currentMemberIndex++;

      members[letStmt->ident_token.TokenLiteral] = memInfo;
      letSym->memberIndex = memInfo->memberIndex;

      metaData[letStmt] = letSym;
      componentTypeInfo->members = members;
    }

    else {
      if (data) {
        logSemanticErrors("Executable statement found in component '" +
                              componentName + "' definition scope",
                          data->statement.line, data->statement.column);
        hasError = true;
      }
    }
  }

  for (const auto &method : componentStmt->privateMethods) {
    auto funcStmt = dynamic_cast<FunctionStatement *>(method.get());
    if (!funcStmt)
      continue;

    auto funcExpr =
        dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get());
    std::string funcName = "unknown";

    if (funcExpr) {
      funcName = funcExpr->func_key.TokenLiteral;
      // Assume funcExpr is the current component method being processed
      auto &currentMembers = currentTypeStack.back().members;

      // Check if this method exists in the imported behavior
      auto behaviorIt = currentMembers.find(funcName);
      if (behaviorIt != currentMembers.end()) {
        auto &behaviorMethod = behaviorIt->second;

        // Compare signatures directly
        bool matches =
            signaturesMatchBehaviorDeclaration(behaviorMethod, funcExpr);
        if (!matches) {
          logSemanticErrors(
              "Component function '" + funcExpr->func_key.TokenLiteral +
                  "' does not match imported behavior declaration",
              funcExpr->func_key.line, funcExpr->func_key.column);
          continue; // skip registration, it's private and mismatched
        }
      }

      walker(funcExpr);
      auto metSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
      std::cout << "TRYING TO INSERT: " << funcExpr->func_key.TokenLiteral
                << "\n";
      if (metSym) {
        std::cout << "INSERTING COMPONENT FUNCTION EXPRESSION\n";
        auto memInfo = std::make_shared<MemberInfo>();
        memInfo->memberName = funcExpr->func_key.TokenLiteral;
        memInfo->type = metSym->type;
        memInfo->isNullable = metSym->isNullable;
        memInfo->isMutable = metSym->isMutable;
        memInfo->isExportable = metSym->isExportable;
        memInfo->returnType = metSym->returnType;
        memInfo->isFunction = true;
        memInfo->isDefined = true;
        memInfo->isDeclared = true;
        memInfo->paramTypes = metSym->paramTypes;
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
      logSemanticErrors(
          "Cannot use function declarations inside a component '" +
              componentName + "'",
          funcDeclrExpr->expression.line, funcDeclrExpr->expression.column);
      hasError = true;
    }
  }

  // Register component as a symbol
  componentSymbol->members = members;

  componentTypeInfo->members = members;
  componentTypeInfo->typeName = componentName;
  componentTypeInfo->isExportable = isExportable;
  componentTypeInfo->type = ResolvedType{DataType::COMPONENT, componentName};

  customTypesTable[componentName] = componentTypeInfo;

  if (componentStmt->initConstructor.has_value())
    walkInitConstructor(componentStmt->initConstructor.value().get());

  if (members.empty()) {
    std::cout << "COMPONENT MEMBERS ARE EMPTY AT RUNTIME\n";
  }

  // Exit component scope
  currentTypeStack.pop_back();
  insideComponent = false;
  popScope();
}

void Semantics::walkNewComponentExpression(Node *node) {
  auto newExpr = dynamic_cast<NewComponentExpression *>(node);
  if (!newExpr)
    return;

  auto line = newExpr->expression.line;
  auto column = newExpr->expression.column;
  auto componentName = newExpr->component_name.TokenLiteral;
  bool hasError = false;

  // UNIFIED TYPE LOOKUP
  // We determine the type once and use it everywhere.
  ResolvedType componentType;
  bool componentExists = false;

  if (customTypesTable.count(componentName)) {
    componentType = customTypesTable[componentName]->type;
    componentExists = true;
  } else if (ImportedComponentTable.count(componentName)) {
    componentType = ImportedComponentTable[componentName]->type;
    componentExists = true;
  }

  if (!componentExists) {
    logSemanticErrors("Component '" + componentName + "' does not exist", line,
                      column);
    return;
  }

  // UNIFIED INIT LOOKUP
  // Bridge the gap between local and imported constructor signatures.
  std::vector<ResolvedType> expectedArgs;
  bool hasInitConstructor = false;

  if (importedInits.count(componentName)) {
    hasInitConstructor = true;
    expectedArgs = importedInits[componentName]->initArgs;
  } else if (componentInitArgs.count(componentName)) {
    hasInitConstructor = true;
    expectedArgs = componentInitArgs[componentName];
  }

  const auto &givenArgs = newExpr->arguments;

  // FLATTENED VALIDATION LOGIC
  if (!hasInitConstructor) {
    if (!givenArgs.empty()) {
      logSemanticErrors("Component '" + componentName +
                            "' has no init constructor, arguments not allowed.",
                        line, column);
      hasError = true;
    }
  } else {
    // We have a constructor, check the count
    if (expectedArgs.size() != givenArgs.size()) {
      std::string msg =
          "Constructor for '" + componentName + "' expects " +
          std::to_string(expectedArgs.size()) + " arguments but got " +
          (givenArgs.empty() ? "none" : std::to_string(givenArgs.size())) + ".";
      logSemanticErrors(msg, line, column);
      hasError = true;
    }

    // PAIRWISE TYPE CHECKING
    size_t checkCount = std::min(expectedArgs.size(), givenArgs.size());
    for (size_t i = 0; i < checkCount; ++i) {
      walker(givenArgs[i].get()); // Analyze the argument expression
      ResolvedType argType = inferNodeDataType(givenArgs[i].get());
      ResolvedType expectedType = expectedArgs[i];

      if (argType.kind != expectedType.kind) {
        logSemanticErrors("Type mismatch in argument " + std::to_string(i + 1) +
                              ": expected '" + expectedType.resolvedName +
                              "', got '" + argType.resolvedName + "'.",
                          line, column);
        hasError = true;
      }
    }
  }

  // Fetch the actual symbol for the IR Generator
  auto componentSym = resolveSymbolInfo(componentName);

  auto info = std::make_shared<SymbolInfo>();
  info->type = componentType;
  info->hasError = hasError;
  info->componentSymbol = componentSym;

  // This map link is critical for the IR Generator to know what it's looking at
  metaData[newExpr] = info;
}

void Semantics::walkMethodCallExpression(Node *node) {
  auto metCall = dynamic_cast<MethodCallExpression *>(node);
  if (!metCall) {
    std::cout << "Invalid method call expression node\n";
    return;
  }

  std::cout << "Analysing method call expression\n";
  bool hasError = false;

  auto instanceIdent = dynamic_cast<Identifier *>(metCall->instance.get());
  if (!instanceIdent) {
    logSemanticErrors("Method call instance must be an identifier",
                      metCall->instance->expression.line,
                      metCall->instance->expression.column);
    return;
  }

  const std::string instanceName = instanceIdent->identifier.TokenLiteral;
  const int line = instanceIdent->expression.line;
  const int col = instanceIdent->expression.column;

  // Get the call expression and function name
  auto funcCall = dynamic_cast<CallExpression *>(metCall->call.get());
  if (!funcCall) {
    logSemanticErrors("Invalid call expression in method call",
                      metCall->call->expression.line,
                      metCall->call->expression.column);
    return;
  }
  const std::string funcName =
      funcCall->function_identifier->expression.TokenLiteral;
  const int funcLine = funcCall->function_identifier->expression.line;
  const int funcCol = funcCall->function_identifier->expression.column;

  // check if this LHS is a seal name
  auto sealIt = sealTable.find(instanceName);
  if (sealIt != sealTable.end()) {
    // SEAL PATH
    auto &sealMap = sealIt->second;
    auto sealfnIt = sealMap.find(funcName);
    if (sealfnIt == sealMap.end()) {
      logSemanticErrors("Function '" + funcName + "' does not exist in seal '" +
                            instanceName + "'",
                        funcLine, funcCol);
      hasError = true;
      return;
    }

    // Walk the seal call
    walkSealCallExpression(funcCall, instanceName);

    // Retrieve the function call metaData
    auto fnIt = metaData.find(funcCall);
    if (fnIt == metaData.end()) {
      logSemanticErrors("Function '" + funcName + "' metaData does not exist ",
                        funcLine, funcCol);
      return;
    }

    auto fnInfo = fnIt->second;

    // Fill the method-call metaData for the whole expression
    auto fnCallSym = std::make_shared<SymbolInfo>();
    fnCallSym->hasError = hasError;
    fnCallSym->type = fnInfo->returnType;
    fnCallSym->isNullable = fnInfo->isNullable;

    metaData[metCall] = fnCallSym;
    return;
  }

  // Walk the instance first to populate metaData for it
  walker(metCall->instance.get());

  auto instanceSym = resolveSymbolInfo(instanceName);
  if (!instanceSym) {
    logSemanticErrors("Unidentified instance name '" + instanceName + "'", line,
                      col);
    return;
  }

  auto type = instanceSym->type;
  auto typeIt = customTypesTable.find(type.resolvedName);
  if (typeIt == customTypesTable.end()) {
    logSemanticErrors("Unknown type '" + type.resolvedName +
                          "' for instance '" + instanceName + "'",
                      line, col);
    return;
  }

  // Check if the instance is the type itself and block such as it is leading to
  // issues in LLVM
  if (type.resolvedName == instanceName) {
    logSemanticErrors("Cannot use the actual type as the instance itself", line,
                      col);
    hasError = true;
  }

  // Now check members on the component type
  auto &members = typeIt->second->members;
  std::cout << "[SEMANTIC DEBUG] Searching for '" << funcName << "' in type '"
            << type.resolvedName << "'\n";
  std::cout << "[SEMANTIC DEBUG] Current members in " << type.resolvedName
            << ": ";
  for (auto const &[name, info] : members) {
    std::cout << name << " (isFunc: " << (info->isFunction ? "Y" : "N") << ") ";
  }
  std::cout << "\n";
  auto memIt = members.find(funcName);
  if (memIt == members.end()) {
    logSemanticErrors("'Function " + funcName + "' does not exist in type '" +
                          type.resolvedName + "'",
                      funcLine, funcCol);
    return;
  }

  auto memInfo = memIt->second;

  // Declaration check
  if (!memInfo->isDeclared) {
    logSemanticErrors("'" + funcName + "' was not declared anywhere", funcLine,
                      funcCol);
    hasError = true;
  }

  // Compatibility check (parameters/return)
  if (!isMethodCallCompatible(*memInfo, funcCall)) {
    hasError = true;
  }

  // Walk the arguments (populate metaData for them)
  for (const auto &arg : funcCall->parameters) {
    walker(arg.get());
  }

  // Update the instance symbol metaData usage info
  instanceSym->lastUseNode = metCall;
  if (instanceSym->refCount) {
    instanceSym->refCount--;
  }

  // Create result metaData for this method call expression
  auto metCallSym = std::make_shared<SymbolInfo>();
  metCallSym->hasError = hasError;
  metCallSym->type = memInfo->returnType;
  metCallSym->isNullable = memInfo->isNullable;

  metaData[metCall] = metCallSym;
}
