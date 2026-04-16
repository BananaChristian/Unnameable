#include <algorithm>
#include <limits>
#include <string>

#include "ast.hpp"
#include "semantics.hpp"

void Semantics::walkEnumStatement(Node* node) {
    auto enumStmt = dynamic_cast<EnumStatement*>(node);
    if (!enumStmt) return;

    std::string enumStmtName = enumStmt->enum_identifier->expression.TokenLiteral;
    bool isExportable = enumStmt->isExportable;

    // Check duplicate type name
    if (resolveSymbolInfo(enumStmtName)) {
        logSemanticErrors("Already used the name '" + enumStmtName + "'", enumStmt);
        return;
    }

    // Set underlying type (default = usize)
    ResolvedType underLyingType = ResolvedType::makeBase(DataType::I32, "i32");
    if (enumStmt->int_type.has_value()) {
        switch (enumStmt->int_type.value().type) {
            case TokenType::I8_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::I8, "i8");
                break;
            case TokenType::U8_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::U8, "u8");
                break;
            case TokenType::I16_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::I16, "i16");
                break;
            case TokenType::U16_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::U16, "u16");
                break;
            case TokenType::I32_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::I32, "i32");
                break;
            case TokenType::U32_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::U32, "u32");
                break;
            case TokenType::I64_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::I64, "i64");
                break;
            case TokenType::U64_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::U64, "u64");
                break;
            case TokenType::I128_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::I128, "i128");
                break;
            case TokenType::U128_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::U128, "u128");
                break;
            case TokenType::USIZE_KEYWORD:
                underLyingType = ResolvedType::makeBase(DataType::ISIZE, "isize");
                break;
            default:
                underLyingType = ResolvedType::makeBase(DataType::USIZE, "usize");
                break;
        }
    }

    // Determine if underlying type is unsigned
    bool underlyingIsUnsigned =
        (underLyingType.kind == DataType::U8 || underLyingType.kind == DataType::U16 ||
         underLyingType.kind == DataType::U32 || underLyingType.kind == DataType::U64 ||
         underLyingType.kind == DataType::U128);

    // Push temporary scope
    symbolTable.push_back({});
    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
    std::int64_t currentValue = 0;
    auto enumInfo = std::make_shared<SymbolInfo>();

    for (const auto& enumMember : enumStmt->enum_content) {
        if (!enumMember) {
            reportDevBug("Invalid enum member", node);
            symbolTable.pop_back();
            return;
        }
        logInternal("Analysing enum member ...");
        std::string memberName = enumMember->enumMember;

        // Check for duplicate member name
        if (members.count(memberName) || resolveSymbolInfo(memberName)) {
            logSemanticErrors("Enum member name '" + memberName + "' already exists",
                              enumMember.get());
            symbolTable.pop_back();
            return;
        }

        std::int64_t memberValue = 0;
        if (enumMember->value) {
            // Check all literal types
            TokenType literalType = TokenType::ILLEGAL;  // Default invalid type
            std::string literalStr;
            if (auto i8Lit = dynamic_cast<I8Literal*>(enumMember->value.get())) {
                literalType = TokenType::INT8;
                literalStr = i8Lit->expression.TokenLiteral;
            } else if (auto u8Lit = dynamic_cast<U8Literal*>(enumMember->value.get())) {
                literalType = TokenType::UINT8;
                literalStr = u8Lit->expression.TokenLiteral;
            } else if (auto i16Lit = dynamic_cast<I16Literal*>(enumMember->value.get())) {
                literalType = TokenType::INT16;
                literalStr = i16Lit->expression.TokenLiteral;
            } else if (auto u16Lit = dynamic_cast<U16Literal*>(enumMember->value.get())) {
                literalType = TokenType::UINT16;
                literalStr = u16Lit->expression.TokenLiteral;
            } else if (auto i32Lit = dynamic_cast<I32Literal*>(enumMember->value.get())) {
                literalType = TokenType::INT32;
                literalStr = i32Lit->expression.TokenLiteral;
            } else if (auto u32Lit = dynamic_cast<U32Literal*>(enumMember->value.get())) {
                literalType = TokenType::UINT32;
                literalStr = u32Lit->expression.TokenLiteral;
            } else if (auto i64Lit = dynamic_cast<I64Literal*>(enumMember->value.get())) {
                literalType = TokenType::INT64;
                literalStr = i64Lit->expression.TokenLiteral;
            } else if (auto u64Lit = dynamic_cast<U64Literal*>(enumMember->value.get())) {
                literalType = TokenType::UINT64;
                literalStr = u64Lit->expression.TokenLiteral;
            } else if (auto i128Lit = dynamic_cast<I128Literal*>(enumMember->value.get())) {
                literalType = TokenType::INT128;
                literalStr = i128Lit->expression.TokenLiteral;
            } else if (auto u128Lit = dynamic_cast<U128Literal*>(enumMember->value.get())) {
                literalType = TokenType::UINT128;
                literalStr = u128Lit->expression.TokenLiteral;
            } else if (auto isizeLit = dynamic_cast<ISIZELiteral*>(enumMember->value.get())) {
                literalType = TokenType::INTSIZE;
                literalStr = isizeLit->expression.TokenLiteral;
            } else if (auto usizeLit = dynamic_cast<USIZELiteral*>(enumMember->value.get())) {
                literalType = TokenType::UINTSIZE;
                literalStr = usizeLit->expression.TokenLiteral;
            } else if(auto intLit=dynamic_cast<INTLiteral*>(enumMember->value.get())){
                literalType=TokenType::INT;
                literalStr=intLit->expression.TokenLiteral;
            }else {
                logSemanticErrors(
                    "Enum member value must be a i8,u8, i16, u16, i32, "
                    "u32, i64, u63, i128, u128 or isize, usize literal",
                    enumMember.get());
                symbolTable.pop_back();
                return;
            }

            // Map literal type to DataType
            bool isUnsignedLiteral;
            switch (literalType) {
                case TokenType::INT8:
                case TokenType::INT16:
                case TokenType::INT32:
                case TokenType::INT64:
                case TokenType::INT128:
                case TokenType::INTSIZE:
                case TokenType::INT://Generic int is i32
                    isUnsignedLiteral = false;
                    break;
                case TokenType::UINT8:
                case TokenType::UINT16:
                case TokenType::UINT32:
                case TokenType::UINT64:
                case TokenType::UINT128:
                case TokenType::UINTSIZE:
                    isUnsignedLiteral = true;
                    break;
                default:
                    logSemanticErrors("Invalid literal type for enum member", enumMember.get());
                    symbolTable.pop_back();
                    return;
            }

            // Validate signedness
            if (isUnsignedLiteral && !underlyingIsUnsigned) {
                logSemanticErrors("Unsigned literal '" + literalStr +
                                      "' incompatible with signed underlying type",
                                  enumMember.get());
                symbolTable.pop_back();
                return;
            }
            if (!isUnsignedLiteral && underlyingIsUnsigned) {
                logSemanticErrors("Signed literal '" + literalStr +
                                      "' incompatible with unsigned underlying type",
                                  enumMember.get());
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
            } catch (const std::invalid_argument&) {
                logSemanticErrors("Invalid integer literal '" + literalStr + "'", enumMember.get());
                symbolTable.pop_back();
                return;
            } catch (const std::out_of_range& e) {
                logSemanticErrors("Value '" + literalStr + "' out of range for underlying type: " +
                                      std::string(e.what()),
                                  enumMember.get());
                symbolTable.pop_back();
                return;
            }
        } else {
            memberValue = currentValue;
            if (underlyingIsUnsigned && memberValue < 0) {
                logSemanticErrors("Negative auto-incremented value '" +
                                      std::to_string(memberValue) +
                                      "' invalid for unsigned underlying type",
                                  enumMember.get());
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
        info->constantValue = static_cast<int64_t>(memberValue);
        info->parentType = ResolvedType::makeBase(DataType::ENUM, enumStmtName);
        members[memberName] = info;

        //Add it to local scope
        auto memberInfo = std::make_shared<SymbolInfo>();
        memberInfo->type().type = underLyingType;
        memberInfo->storage().isConstant = true;
        memberInfo->storage().isInitialized = true;
        memberInfo->isExportable = isExportable;
        symbolTable.back()[memberName] = memberInfo;  

        currentValue = memberValue + 1;
    }

    // Store enum type info
    auto typeInfo = std::make_shared<CustomTypeInfo>();

    typeInfo->typeName = enumStmtName;
    typeInfo->type = ResolvedType::makeBase(DataType::ENUM, enumStmtName);
    typeInfo->underLyingType = underLyingType.kind;
    typeInfo->members = members;
    typeInfo->isExportable = isExportable;
    customTypesTable[enumStmtName] = typeInfo;

    // Add enum type to parent scope
    auto generalInfo = std::make_shared<SymbolInfo>();
    generalInfo->type().type = ResolvedType::makeBase(DataType::ENUM, enumStmtName);
    generalInfo->storage().isConstant = false;
    generalInfo->storage().isInitialized = true;
    generalInfo->isExportable = isExportable;
    symbolTable[symbolTable.size() - 2][enumStmtName] = generalInfo;

    metaData[enumStmt] = generalInfo;

    // Pop temporary scope
    symbolTable.pop_back();
}

void Semantics::walkRecordStatement(Node* node) {
    auto recordStmt = dynamic_cast<RecordStatement*>(node);
    if (!recordStmt) return;
    hasError = false;

    // Get block name
    std::string recordName = recordStmt->recordName->expression.TokenLiteral;
    bool isExportable = recordStmt->isExportable;

    // Ensure name not already used
    if (resolveSymbolInfo(recordName)) {
        logSemanticErrors("Already used the name '" + recordName + "'", recordStmt);
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

    // Early registration
    symbolTable[0][recordName] = recordSym;
    customTypesTable[recordName] = typeInfo;
    metaData[recordStmt] = recordSym;

    // Create new local scope for analysis
    insideRecord = true;
    symbolTable.push_back({});

    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> recordMembers;
    

    int currentMemberIndex = 0;
    // Analyze each field
    for (const auto& field : recordStmt->fields) {
        auto declaration = dynamic_cast<VariableDeclaration*>(field.get());

        // Double check incase the parser messed up and leaked wrong statements
        if (!declaration) {
            logSemanticErrors("Invalid statement inside record '" + recordName + "'", field.get());
            continue;  // skip bad field but keep going
        }

        // Apply block mutability if set
        if (isBlockMutable)
            declaration->mutability = Mutability::MUTABLE;
        else if (isBlockConstant)
            declaration->mutability = Mutability::CONSTANT;

        // Apply volatility to the entire block if the user marked it
        if (isBlockVolatile) {
            declaration->isVolatile = true;
        }

        // Walk the let statement to register it in the scope
        walker(field.get());

        // Now retrieve its symbol
        auto fieldName = extractDeclarationName(field.get());
        auto fieldSymbol = getSymbolFromMeta(field.get());
        if (!fieldSymbol) {
            reportDevBug("Declaration statement '" + fieldName + "' was not analyzed properly",
                         field.get());
        }

        // Build member info
        auto memInfo = std::make_shared<MemberInfo>();
        memInfo->memberName = fieldName;
        memInfo->type = fieldSymbol->type().type;
        memInfo->isMutable = fieldSymbol->storage().isMutable;
        memInfo->isConstant = fieldSymbol->storage().isConstant;
        memInfo->isInitialised = fieldSymbol->storage().isInitialized;
        memInfo->isPointer = fieldSymbol->type().isPointer;
        memInfo->isNullable = fieldSymbol->type().isNullable;
        memInfo->isVolatile = fieldSymbol->storage().isVolatile;
        memInfo->typeNode = recordStmt;
        memInfo->node = field.get();
        memInfo->memberIndex=currentMemberIndex++;

        // Insert into members map
        recordMembers[fieldName] = memInfo;

        logInternal("Added field '" + fieldName + "' to record '" + recordName + "'");
    }


    // Build symbol info for the block
    recordSym->members = recordMembers;
    recordSym->hasError = hasError;

    typeInfo->members = recordMembers;
    
    customTypesTable[recordName] = typeInfo;
    // Pop local scope
    insideRecord = false;
    symbolTable.pop_back();
}

void Semantics::walkInstanceExpression(Node* node) {
    auto instExpr = dynamic_cast<InstanceExpression*>(node);
    if (!instExpr) return;

    auto instName = instExpr->blockIdent->expression.TokenLiteral;

    bool hasError = false;

    auto instSym = resolveSymbolInfo(instName);

    if (isGlobalScope()) {
        logSemanticErrors("Cannot create an instance in the global scope",
                          instExpr->blockIdent.get());
        return;
    }

    if (!instSym) {
        logSemanticErrors("Undefined identifier '" + instName + "'", instExpr->blockIdent.get());
        return;
    }

    if (instSym->type().type.kind != DataType::RECORD)
        logSemanticErrors("'" + instName + "' is not a record ", instExpr->blockIdent.get());

    // Dealing with arguments if they exist
    if (!instExpr->fields.empty()) {
        auto members = instSym->members;
        for (const auto& field : instExpr->fields) {
            auto fieldNode = dynamic_cast<AssignmentStatement*>(field.get());
            auto fieldName = fieldNode->identifier->expression.TokenLiteral;

            auto it = members.find(fieldName);
            if (it == members.end()) {
                logSemanticErrors("Field '" + fieldName + "' does not exist in '" + instName + "'",
                                  instExpr->blockIdent.get());
                hasError = true;
                continue;
            }

            auto expectedFieldType = it->second->type;

            if (auto* nullLit = dynamic_cast<NullLiteral*>(fieldNode->value.get())) {
                auto nullSym = std::make_shared<SymbolInfo>();
                nullSym->type().type = expectedFieldType;
                nullSym->type().isDefinitelyNull = true;

                metaData[nullLit] = nullSym;
            } else {
                walker(fieldNode->value.get());
            }
        }
    }

    ResolvedType instType = instSym->type().type;
    logInternal("Instance Type '" + instType.resolvedName + "'");

    auto instInfo = std::make_shared<SymbolInfo>();
    instInfo->type().type = instType;
    instInfo->hasError = hasError;

    metaData[instExpr] = instInfo;
}

void Semantics::walkInitConstructor(Node* node) {
    auto initStmt = dynamic_cast<InitStatement*>(node);
    if (!initStmt) return;
    hasError = false;

    // Must be inside a component
    if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT) {
        logSemanticErrors("`init` constructor must be declared inside a component", initStmt);
        return;
    }

    auto& currentComponent = currentTypeStack.back();

    // Only one init constructor per component
    if (currentComponent.hasInitConstructor) {
        logSemanticErrors(
            "Component '" + currentComponent.typeName + "' already has an init constructor",
            initStmt);
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
    symbolTable.push_back({});
    for (const auto& arg : initStmt->constructor_args) {
        walkFunctionParameters(arg.get());
        // Storing the init args
        initArgs.push_back(inferNodeDataType(arg.get()));
    }
    initInfo->func().initArgs = initArgs;
    componentInitArgs[currentComponentName] = initArgs;

    // Walk the constructor body
    if (initStmt->block) {
        walkBlockStatement(initStmt->block.get());
    }
    popScope();

    // Attach metadata
    metaData[initStmt] = initInfo;
}

// Resolves a self expression chain like self.pos.x.y
// Returns the ResolvedType of the final field, nullptr on error
ResolvedType* Semantics::resolveSelfChain(SelfExpression* selfExpr,
                                          const std::string& componentName) {
    // Look up the component's type info
    auto ctIt = customTypesTable.find(componentName);
    if (ctIt == customTypesTable.end()) {
        logSemanticErrors("Component '" + componentName + "' not found", selfExpr);
        return nullptr;
    }

    std::shared_ptr<CustomTypeInfo> currentTypeInfo = ctIt->second;
    ResolvedType* currentResolvedType = &currentTypeInfo->type;

    for (const auto& fieldNode : selfExpr->fields) {
        auto ident = dynamic_cast<Identifier*>(fieldNode.get());
        if (!ident) {
            logSemanticErrors("Expected identifier in 'self' field chain", selfExpr);
            return nullptr;
        }

        const std::string& fieldName = ident->identifier.TokenLiteral;
        logInternal("Name received from self expression '" + fieldName + "'");

        // Look in the current type's members
        auto memIt = currentTypeInfo->members.find(fieldName);
        if (memIt == currentTypeInfo->members.end()) {
            logSemanticErrors("'" + fieldName + "' does not exist in type '" +
                                  currentTypeInfo->type.resolvedName + "'",
                              ident);
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
            auto ctiIt = customTypesTable.find(lookUpName);
            if (ctiIt == customTypesTable.end()) {
                logSemanticErrors("Type info for '" + lookUpName + "' not found", ident);
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

void Semantics::walkSelfExpression(Node* node) {
    auto selfExpr = dynamic_cast<SelfExpression*>(node);
    if (!selfExpr) return;
    hasError = false;

    // Must be inside a component
    if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT) {
        logSemanticErrors("'self' cannot be used outside a component", selfExpr);
        return;
    }

    const std::string& componentName = currentTypeStack.back().typeName;

    ResolvedType* finalType = resolveSelfChain(selfExpr, componentName);
    if (!finalType) return;

    auto info = std::make_shared<SymbolInfo>();
    info->type().type = *finalType;  // Copy resolved type
    info->type().isNullable = finalType->isNull;

    // Mutability etc only available if the last member is a custom member
    auto lastIdent = dynamic_cast<Identifier*>(selfExpr->fields.back().get());
    if (lastIdent) {
        auto ctIt = customTypesTable.find(componentName);
        if (ctIt != customTypesTable.end()) {
            auto& members = ctIt->second->members;
            auto memIt = members.find(lastIdent->identifier.TokenLiteral);
            if (memIt != members.end()) {
                auto m = memIt->second;
                info->storage().isMutable = m->isMutable;
                info->storage().isConstant = m->isConstant;
                info->storage().isInitialized = m->isInitialised;
                info->type().memberIndex = m->memberIndex;
            }
        }
    }

    metaData[selfExpr] = info;
}

void Semantics::walkComponentStatement(Node* node) {
    auto componentStmt = dynamic_cast<ComponentStatement*>(node);
    if (!componentStmt) return;
    hasError = false;

    auto componentName = componentStmt->component_name->expression.TokenLiteral;
    bool isExportable = componentStmt->isExportable;

    hasError = false;
    insideComponent = true;

    if (symbolTable[0].find(componentName) != symbolTable[0].end()) {
        logSemanticErrors("Component name'" + componentName + "' already in use", componentStmt);
        return;
    }

    auto componentSymbol = std::make_shared<SymbolInfo>();
    componentSymbol->isComponent = true;
    auto componentTypeInfo = std::make_shared<CustomTypeInfo>();

    componentSymbol->type().type = ResolvedType::makeBase(DataType::COMPONENT, componentName);
    componentTypeInfo->type = ResolvedType::makeBase(DataType::COMPONENT, componentName);

    symbolTable[0][componentName] = componentSymbol;
    customTypesTable[componentName] = componentTypeInfo;
    metaData[componentStmt] = componentSymbol;

    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
    int currentMemberIndex = 0;

    // Enter a new component scope
    symbolTable.push_back({});
    currentTypeStack.push_back({.type = ResolvedType::makeBase(DataType::COMPONENT, componentName),
                                .typeName = componentName,
                                .hasInitConstructor = false,
                                .members = members,
                                .node = componentStmt});

    // Walk record imports
    for (const auto& injectedField : componentStmt->injectedFields) {
        if (!injectedField) {
            reportDevBug("Invalid inject record node ", nullptr);
            return;
        }

        auto injectStmt = dynamic_cast<InjectStatement*>(injectedField.get());
        if (!injectStmt) {
            reportDevBug("Invalid inject statement", injectedField.get());
            continue;
        }

        // Mass import case
        auto ident = dynamic_cast<Identifier*>(injectStmt->expr.get());
        if (ident) {
            auto identName = ident->expression.TokenLiteral;
            logInternal("Injecting fields from '" + identName + "' into '" + componentName + "'");

            auto typeIt = customTypesTable.find(identName);
            if (typeIt == customTypesTable.end()) {
                logSemanticErrors("Record with name '" + identName + "' does not exist", ident);
                hasError = true;
                return;
            }

            auto& importedMembers = typeIt->second->members;
            auto& currentScope = symbolTable.back();

            std::vector<std::pair<std::string, std::shared_ptr<MemberInfo>>> sortedMembers(
    importedMembers.begin(), importedMembers.end());

    std::sort(sortedMembers.begin(), sortedMembers.end(),
    [](const auto &a, const auto &b) {
        return a.second->memberIndex < b.second->memberIndex;
    });


            for (auto& [name, info] : sortedMembers) {
                // CHECK FOR COLLISION FIRST!
                if (members.find(name) != members.end()) {
                    logSemanticErrors("Field '" + name + "' from record '" + identName + 
                                      "' collides with existing field in component '" + componentName + "'",
                                      ident);
                    return;  
                }
                
                // Also check in current scope (fields already defined in component)
                if (symbolTable.back().find(name) != symbolTable.back().end()) {
                    logSemanticErrors("Field '" + name + "' from record '" + identName + 
                                      "' collides with existing member in component '" + componentName + "'",
                                      ident);
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
        auto infixExpr = dynamic_cast<InfixExpression*>(injectStmt->expr.get());
        if (infixExpr) {
            auto leftIdent = dynamic_cast<Identifier*>(infixExpr->left_operand.get());
            auto rightIdent = dynamic_cast<Identifier*>(infixExpr->right_operand.get());
            if (!leftIdent || !rightIdent) return;

            TokenType op = infixExpr->operat.type;

            if (op != TokenType::AT) {
                logSemanticErrors("Invalid operator '" + infixExpr->operat.TokenLiteral +
                                      "' for explicit injection",
                                  infixExpr);
            }

            auto dataName = leftIdent->expression.TokenLiteral;
            auto memberName = rightIdent->expression.TokenLiteral;

            auto importedTypeIt = customTypesTable.find(dataName);
            if (importedTypeIt != customTypesTable.end()) {
                auto& importedMembers = importedTypeIt->second->members;
                auto memIt = importedMembers.find(memberName);
                if (memIt != importedMembers.end()) {
                    if (members.find(memberName) != members.end()) {
                        logSemanticErrors("Explicit injection of field '" + memberName + 
                                          "' from record '" + dataName + 
                                          "' collides with existing field in component '" + componentName + "'",
                                          infixExpr);
                        hasError = true;
                        return;
                    }
                    
                    if (symbolTable.back().find(memberName) != symbolTable.back().end()) {
                        logSemanticErrors("Explicit injection of field '" + memberName + 
                                          "' collides with existing member in component '" + componentName + "'",
                                          infixExpr);
                        hasError = true;
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
                    symbolTable.back()[memberName] = memSym;

                    componentTypeInfo->members = members;
                }
            }
        }
    }

    for (const auto& data : componentStmt->fields) {
        auto declaration = dynamic_cast<VariableDeclaration*>(data.get());
        std::string declName = extractDeclarationName(data.get());

        if (declaration) {
            walker(data.get());
            std::shared_ptr<SymbolInfo> declSym = resolveSymbolInfo(declName);
            if (!declSym) {
                reportDevBug("Failed to resolve member '" + declName + "'", data.get());
                hasError = true;
                continue;
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
            memInfo->memberIndex = currentMemberIndex++;

            members[declName] = memInfo;
            declSym->type().memberIndex = memInfo->memberIndex;

            metaData[declaration] = declSym;
            componentTypeInfo->members = members;
        } else {
            logSemanticErrors(
                "Invalid statement found in component '" + componentName + "' definition scope",
                data.get());
        }
    }

    for (const auto& method : componentStmt->methods) {
        auto funcStmt = dynamic_cast<FunctionStatement*>(method.get());
        if (!funcStmt) continue;

        auto funcExpr = dynamic_cast<FunctionExpression*>(funcStmt->funcExpr.get());
        std::string funcName = "unknown";

        if (funcExpr) {
            funcName = funcExpr->func_key.TokenLiteral;
            // Assume funcExpr is the current component method being processed
            auto& currentMembers = currentTypeStack.back().members;

            // Check if this method exists in the imported behavior
            auto behaviorIt = currentMembers.find(funcName);
            if (behaviorIt != currentMembers.end()) {
                auto& behaviorMethod = behaviorIt->second;

                // Compare signatures directly
                bool matches = signaturesMatchBehaviorDeclaration(behaviorMethod, funcExpr);
                if (!matches) {
                    logSemanticErrors("Component function '" + funcExpr->func_key.TokenLiteral +
                                          "' does not match imported behavior declaration",
                                      funcExpr);
                    continue;  // skip registration, it's private and mismatched
                }
            }

            walker(funcExpr);
            auto metSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
            logInternal("Trying to insert '" + funcExpr->func_key.TokenLiteral + "'");
            if (metSym) {
                logInternal("Inserting component function expression ....");
                auto memInfo = std::make_shared<MemberInfo>();
                memInfo->memberName = funcExpr->func_key.TokenLiteral;
                memInfo->type = metSym->type().type;
                memInfo->isNullable = metSym->type().isNullable;
                memInfo->isMutable = metSym->storage().isMutable;
                memInfo->isExportable = metSym->isExportable;
                memInfo->returnType = metSym->func().returnType;
                memInfo->isVolatile = metSym->storage().isVolatile;
                memInfo->isFunction = true;
                memInfo->isDefined = true;
                memInfo->isDeclared = true;
                memInfo->paramTypes = metSym->func().paramTypes;
                memInfo->node = funcExpr;
                memInfo->typeNode = componentStmt;
                members[funcName] = memInfo;
            }
            componentTypeInfo->members = members;  // Update the custom type table members
        } else {
            auto funcDeclrExpr =
                dynamic_cast<FunctionDeclarationExpression*>(funcStmt->funcExpr.get());
            if (funcDeclrExpr && funcDeclrExpr->funcDeclrStmt) {
                auto funcDeclrStmt =
                    dynamic_cast<FunctionDeclaration*>(funcDeclrExpr->funcDeclrStmt.get());
                if (funcDeclrStmt) {
                    funcName = funcDeclrStmt->function_name->expression.TokenLiteral;
                }
            }
            logSemanticErrors(
                "Cannot use function declarations inside a component '" + componentName + "'",
                funcDeclrExpr);
        }
    }

    // Register component as a symbol
    componentSymbol->hasError = hasError;
    componentSymbol->members = members;

    componentTypeInfo->members = members;
    componentTypeInfo->typeName = componentName;
    componentTypeInfo->isExportable = isExportable;
    componentTypeInfo->type = ResolvedType::makeBase(DataType::COMPONENT, componentName);

    customTypesTable[componentName] = componentTypeInfo;

    if (componentStmt->initConstructor.has_value())
        walkInitConstructor(componentStmt->initConstructor.value().get());

    if (members.empty()) {
        logInternal("Component members are empty at runtime");
    }

    // Exit component scope
    currentTypeStack.pop_back();
    insideComponent = false;
    symbolTable.pop_back();
}

void Semantics::walkNewComponentExpression(Node* node) {
    auto newExpr = dynamic_cast<NewComponentExpression*>(node);
    if (!newExpr) return;
    hasError = false;

    auto componentName = newExpr->component_name.TokenLiteral;

    if (isGlobalScope()) {
        logSemanticErrors("Cannot instiate a component in global scope", newExpr);
        return;
    }
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
        logSemanticErrors("Component '" + componentName + "' does not exist", newExpr);
        return;
    }

    // UNIFIED INIT LOOKUP
    // Bridge the gap between local and imported constructor signatures.
    std::vector<ResolvedType> expectedArgs;
    bool hasInitConstructor = false;

    if (importedInits.count(componentName)) {
        hasInitConstructor = true;
        expectedArgs = importedInits[componentName]->func().initArgs;
    } else if (componentInitArgs.count(componentName)) {
        hasInitConstructor = true;
        expectedArgs = componentInitArgs[componentName];
    }

    const auto& givenArgs = newExpr->arguments;

    // FLATTENED VALIDATION LOGIC
    if (!hasInitConstructor) {
        if (!givenArgs.empty()) {
            logSemanticErrors(
                "Component '" + componentName + "' has no init constructor, arguments not allowed.",
                newExpr);
        }
    } else {
        // We have a constructor, check the count
        if (expectedArgs.size() != givenArgs.size()) {
            std::string msg = "Constructor for '" + componentName + "' expects " +
                              std::to_string(expectedArgs.size()) + " arguments but got " +
                              (givenArgs.empty() ? "none" : std::to_string(givenArgs.size())) + ".";
            logSemanticErrors(msg, newExpr);
        }

        // PAIRWISE TYPE CHECKING
        size_t checkCount = std::min(expectedArgs.size(), givenArgs.size());
        for (size_t i = 0; i < checkCount; ++i) {
            walker(givenArgs[i].get());  // Analyze the argument expression
            ResolvedType argType = inferNodeDataType(givenArgs[i].get());
            ResolvedType expectedType = expectedArgs[i];

            if (argType.kind != expectedType.kind) {
                logSemanticErrors("Type mismatch in argument " + std::to_string(i + 1) +
                                      ": expected '" + expectedType.resolvedName + "', got '" +
                                      argType.resolvedName + "'.",
                                  newExpr);
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
    metaData[newExpr] = info;
}

void Semantics::walkMethodCallExpression(Node* node) {
    auto metCall = dynamic_cast<MethodCallExpression*>(node);
    if (!metCall) reportDevBug("Invalid method call expression node", node);

    hasError = false;

    auto instanceIdent = dynamic_cast<Identifier*>(metCall->instance.get());
    if (!instanceIdent) {
        logSemanticErrors("Method call instance must be an identifier", metCall);
        return;
    }

    const std::string instanceName = instanceIdent->identifier.TokenLiteral;

    // Get the call expression and function name
    auto funcCall = dynamic_cast<CallExpression*>(metCall->call.get());
    if (!funcCall) {
        logSemanticErrors("Invalid call expression in method call", metCall);
        return;
    }

    const std::string funcName = funcCall->function_identifier->expression.TokenLiteral;

    // check if this LHS is a seal name
    auto sealIt = sealTable.find(instanceName);
    if (sealIt != sealTable.end()) {
        // SEAL PATH
        auto& sealMap = sealIt->second;
        auto sealfnIt = sealMap.find(funcName);
        if (sealfnIt == sealMap.end()) {
            logSemanticErrors(
                "Function '" + funcName + "' does not exist in seal '" + instanceName + "'",
                funcCall->function_identifier.get());
            return;
        }

        // Walk the seal call
        walkSealCallExpression(funcCall, instanceName);

        auto fnInfo = getSymbolFromMeta(funcCall);

        // Fill the method-call metaData for the whole expression
        auto fnCallSym = std::make_shared<SymbolInfo>();
        fnCallSym->hasError = hasError;
        fnCallSym->type().type = fnInfo->type().type;
        fnCallSym->type().isNullable = fnInfo->type().isNullable;

        metaData[metCall] = fnCallSym;
        return;
    }

    // Walk the instance first to populate metaData for it
    walker(metCall->instance.get());

    auto instanceSym = resolveSymbolInfo(instanceName);
    if (!instanceSym) {
        logSemanticErrors("Unidentified instance name '" + instanceName + "'",
                          funcCall->function_identifier.get());
        return;
    }

    auto type = instanceSym->type().type;
    auto typeIt = customTypesTable.find(type.resolvedName);
    if (typeIt == customTypesTable.end()) {
        logSemanticErrors(
            "Unknown type '" + type.resolvedName + "' for instance '" + instanceName + "'",
            funcCall->function_identifier.get());
        return;
    }

    // Check if the instance is the type itself and block such as it is leading to
    // issues in LLVM
    if (type.resolvedName == instanceName) 
        logSemanticErrors("Cannot use the actual type as the instance itself",
                          funcCall->function_identifier.get());
    

    // Now check members on the component type
    auto& members = typeIt->second->members;
    std::cout << "[SEMANTIC DEBUG] Searching for '" << funcName << "' in type '"
              << type.resolvedName << "'\n";
    std::cout << "[SEMANTIC DEBUG] Current members in " << type.resolvedName << ": ";
    for (auto const& [name, info] : members) {
        std::cout << name << " (isFunc: " << (info->isFunction ? "Y" : "N") << ") ";
    }
    std::cout << "\n";
    auto memIt = members.find(funcName);
    if (memIt == members.end()) {
        logSemanticErrors(
            "'Function " + funcName + "' does not exist in type '" + type.resolvedName + "'",
            funcCall->function_identifier.get());
        return;
    }

    auto memInfo = memIt->second;

    // Declaration check
    if (!memInfo->isDeclared) {
        logSemanticErrors("'" + funcName + "' was not declared anywhere",
                          funcCall->function_identifier.get());
    }

    // Walk the arguments (populate metaData for them)
    for (size_t i = 0; i < funcCall->parameters.size(); ++i) {
        const auto& arg = funcCall->parameters[i];
        walker(arg.get());
        auto argSym=getSymbolFromMeta(arg.get());
        giveGenericLiteralContext(arg.get(),memInfo->paramTypes[i].first,argSym);

        if (auto nullLit = dynamic_cast<NullLiteral*>(arg.get())) {
            if (i < memInfo->paramTypes.size()) {
                auto expected = memInfo->paramTypes[i].first;
                if (expected.isNull) {
                    metaData[nullLit]->type().type = expected;
                } else {
                    logSemanticErrors("Cannot pass null to non-nullable parameter", arg.get());
                    continue;
                }
            }
        }
    }

    // Compatibility check (parameters/return)
    if (!isMethodCallCompatible(*memInfo, funcCall)) {
        hasError = true;
    }

    // Update the instance symbol metaData usage info
    // instanceSym->lastUseNode = metCall;
    if (instanceSym->storage().refCount) {
        instanceSym->storage().refCount--;
    }

    // Create result metaData for this method call expression
    auto metCallSym = std::make_shared<SymbolInfo>();
    metCallSym->hasError = hasError;
    metCallSym->type().type = memInfo->returnType;
    metCallSym->type().isNullable = memInfo->isNullable;

    metaData[metCall] = metCallSym;
}
