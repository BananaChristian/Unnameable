#include "semantics.hpp"
#include <algorithm>
#include <limits>

void Semantics::walkEnumClassStatement(Node *node)
{
    auto enumStmt = dynamic_cast<EnumClassStatement *>(node);
    if (!enumStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analysing enum class statement\n";

    std::string enumStmtName = enumStmt->enum_identifier->expression.TokenLiteral;

    // Check duplicate type name
    if (resolveSymbolInfo(enumStmtName))
    {
        logSemanticErrors("Already used the name '" + enumStmtName + "'",
                          enumStmt->statement.line, enumStmt->statement.column);
        return;
    }

    // Set underlying type (default = int)
    ResolvedType underLyingType{DataType::INTEGER, "int"};
    if (enumStmt->int_type.has_value())
    {
        switch (enumStmt->int_type.value().type)
        {
        case TokenType::SHORT_KEYWORD:
            underLyingType = ResolvedType{DataType::SHORT_INT, "short"};
            break;
        case TokenType::USHORT_KEYWORD:
            underLyingType = ResolvedType{DataType::USHORT_INT, "ushort"};
            break;
        case TokenType::INTEGER_KEYWORD:
            underLyingType = ResolvedType{DataType::INTEGER, "int"};
            break;
        case TokenType::UINT_KEYWORD:
            underLyingType = ResolvedType{DataType::UINTEGER, "uint"};
            break;
        case TokenType::LONG_KEYWORD:
            underLyingType = ResolvedType{DataType::LONG_INT, "long"};
            break;
        case TokenType::ULONG_KEYWORD:
            underLyingType = ResolvedType{DataType::ULONG_INT, "ulong"};
            break;
        case TokenType::EXTRA_KEYWORD:
            underLyingType = ResolvedType{DataType::EXTRA_INT, "extra"};
            break;
        case TokenType::UEXTRA_KEYWORD:
            underLyingType = ResolvedType{DataType::UEXTRA_INT, "uextra"};
            break;
        default:
            underLyingType = ResolvedType{DataType::INTEGER, "int"};
            break;
        }
    }

    // Determine if underlying type is unsigned
    bool underlyingIsUnsigned = (underLyingType.kind == DataType::USHORT_INT ||
                                 underLyingType.kind == DataType::UINTEGER ||
                                 underLyingType.kind == DataType::ULONG_INT ||
                                 underLyingType.kind == DataType::UEXTRA_INT);

    // Push temporary scope
    symbolTable.push_back({});
    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
    std::int64_t currentValue = 0;
    auto enumInfo = std::make_shared<SymbolInfo>();

    for (const auto &enumMember : enumStmt->enum_content)
    {
        if (!enumMember)
        {
            logSemanticErrors("Invalid enum member", enumStmt->statement.line, enumStmt->statement.column);
            symbolTable.pop_back();
            return;
        }

        std::cout << "[SEMANTIC LOG] Analysing enum member node\n";
        std::string memberName = enumMember->enumMember;

        // Check for duplicate member name
        if (members.count(memberName) || resolveSymbolInfo(memberName))
        {
            logSemanticErrors("Enum member name '" + memberName + "' already exists",
                              enumMember->token.line, enumMember->token.column);
            symbolTable.pop_back();
            return;
        }

        std::int64_t memberValue = 0;
        if (enumMember->value)
        {
            // Check all literal types
            Expression *literal = nullptr;
            TokenType literalType = TokenType::ILLEGAL; // Default invalid type
            std::string literalStr;

            if (auto shortLit = dynamic_cast<ShortLiteral *>(enumMember->value.get()))
            {
                literal = shortLit;
                literalType = TokenType::SHORT;
                literalStr = shortLit->expression.TokenLiteral;
            }
            else if (auto ushortLit = dynamic_cast<UnsignedShortLiteral *>(enumMember->value.get()))
            {
                literal = ushortLit;
                literalType = TokenType::USHORT;
                literalStr = ushortLit->expression.TokenLiteral;
            }
            else if (auto intLit = dynamic_cast<IntegerLiteral *>(enumMember->value.get()))
            {
                literal = intLit;
                literalType = TokenType::INT;
                literalStr = intLit->expression.TokenLiteral;
            }
            else if (auto uintLit = dynamic_cast<UnsignedIntegerLiteral *>(enumMember->value.get()))
            {
                literal = uintLit;
                literalType = TokenType::UINT;
                literalStr = uintLit->expression.TokenLiteral;
            }
            else if (auto longLit = dynamic_cast<LongLiteral *>(enumMember->value.get()))
            {
                literal = longLit;
                literalType = TokenType::LONG;
                literalStr = longLit->expression.TokenLiteral;
            }
            else if (auto ulongLit = dynamic_cast<UnsignedLongLiteral *>(enumMember->value.get()))
            {
                literal = ulongLit;
                literalType = TokenType::ULONG;
                literalStr = ulongLit->expression.TokenLiteral;
            }
            else if (auto extraLit = dynamic_cast<ExtraLiteral *>(enumMember->value.get()))
            {
                literal = extraLit;
                literalType = TokenType::EXTRA;
                literalStr = extraLit->expression.TokenLiteral;
            }
            else if (auto uextraLit = dynamic_cast<UnsignedExtraLiteral *>(enumMember->value.get()))
            {
                literal = uextraLit;
                literalType = TokenType::UEXTRA;
                literalStr = uextraLit->expression.TokenLiteral;
            }
            else
            {
                logSemanticErrors("Enum member value must be a short, ushort, int, uint, long, ulong, extra, or uextra literal",
                                  enumMember->token.line, enumMember->token.column);
                symbolTable.pop_back();
                return;
            }

            // Map literal type to DataType
            bool isUnsignedLiteral;
            switch (literalType)
            {
            case TokenType::SHORT:
                isUnsignedLiteral = false;
                break;
            case TokenType::USHORT:
                isUnsignedLiteral = true;
                break;
            case TokenType::INT:
                isUnsignedLiteral = false;
                break;
            case TokenType::UINT:
                isUnsignedLiteral = true;
                break;
            case TokenType::LONG:
                isUnsignedLiteral = false;
                break;
            case TokenType::ULONG:
                isUnsignedLiteral = true;
                break;
            case TokenType::EXTRA:
                isUnsignedLiteral = false;
                break;
            case TokenType::UEXTRA:
                isUnsignedLiteral = true;
                break;
            default:
                logSemanticErrors("Invalid literal type for enum member",
                                  enumMember->token.line, enumMember->token.column);
                symbolTable.pop_back();
                return;
            }

            // Validate signedness
            if (isUnsignedLiteral && !underlyingIsUnsigned)
            {
                logSemanticErrors("Unsigned literal '" + literalStr + "' incompatible with signed underlying type",
                                  enumMember->token.line, enumMember->token.column);
                symbolTable.pop_back();
                return;
            }
            if (!isUnsignedLiteral && underlyingIsUnsigned)
            {
                logSemanticErrors("Signed literal '" + literalStr + "' incompatible with unsigned underlying type",
                                  enumMember->token.line, enumMember->token.column);
                symbolTable.pop_back();
                return;
            }

            // Parse and validate range
            try
            {
                if (isUnsignedLiteral)
                {
                    std::uint64_t parsedValue = std::stoull(literalStr);
                    memberValue = static_cast<std::int64_t>(parsedValue);
                    if (underLyingType.kind == DataType::USHORT_INT && parsedValue > std::numeric_limits<std::uint16_t>::max())
                        throw std::out_of_range("Value out of range for ushort (16-bit)");
                    if (underLyingType.kind == DataType::UINTEGER && parsedValue > std::numeric_limits<std::uint32_t>::max())
                        throw std::out_of_range("Value out of range for uint (32-bit)");
                    // No range check for ULONG_INT, UEXTRA_INT
                }
                else
                {
                    memberValue = std::stoll(literalStr);
                    if (underLyingType.kind == DataType::SHORT_INT &&
                        (memberValue < std::numeric_limits<std::int16_t>::min() || memberValue > std::numeric_limits<std::int16_t>::max()))
                        throw std::out_of_range("Value out of range for short (16-bit)");
                    if (underLyingType.kind == DataType::INTEGER &&
                        (memberValue < std::numeric_limits<std::int32_t>::min() || memberValue > std::numeric_limits<std::int32_t>::max()))
                        throw std::out_of_range("Value out of range for int (32-bit)");
                    // No range check for LONG_INT, EXTRA_INT
                }
            }
            catch (const std::invalid_argument &)
            {
                logSemanticErrors("Invalid integer literal '" + literalStr + "'",
                                  enumMember->token.line, enumMember->token.column);
                symbolTable.pop_back();
                return;
            }
            catch (const std::out_of_range &e)
            {
                logSemanticErrors("Value '" + literalStr + "' out of range for underlying type: " + std::string(e.what()),
                                  enumMember->token.line, enumMember->token.column);
                symbolTable.pop_back();
                return;
            }
        }
        else
        {
            memberValue = currentValue;
            if (underlyingIsUnsigned && memberValue < 0)
            {
                logSemanticErrors("Negative auto-incremented value '" + std::to_string(memberValue) + "' invalid for unsigned underlying type",
                                  enumMember->token.line, enumMember->token.column);
                symbolTable.pop_back();
                return;
            }
        }

        // Store member info
        // Store member info as shared_ptr
        auto info = std::make_shared<MemberInfo>();
        info->memberName = memberName;
        info->type = underLyingType;
        info->isConstant = true;
        info->isInitialised = true;
        info->node = enumMember.get();
        info->constantValue = static_cast<int>(memberValue);
        info->parentType = ResolvedType{DataType::ENUM, enumStmtName};
        members[memberName] = info;

        // Add member to local scope
        enumInfo->type = underLyingType;
        enumInfo->isConstant = true;
        enumInfo->isInitialized = true;
        symbolTable.back()[memberName] = enumInfo;

        currentValue = memberValue + 1;
    }

    // Store enum type info
    auto typeInfo = std::make_shared<CustomTypeInfo>();

    typeInfo->typeName = enumStmtName;
    typeInfo->type = ResolvedType{DataType::ENUM, enumStmtName};
    typeInfo->underLyingType = underLyingType.kind;
    typeInfo->members = members;
    customTypesTable[enumStmtName] = typeInfo;

    // Add enum type to parent scope
    auto generalInfo = std::make_shared<SymbolInfo>();
    generalInfo->type = ResolvedType{DataType::ENUM, enumStmtName};
    generalInfo->isConstant = false;
    generalInfo->isInitialized = true;
    symbolTable[symbolTable.size() - 2][enumStmtName] = generalInfo;

    metaData[enumStmt] = generalInfo;

    // Pop temporary scope
    symbolTable.pop_back();
}

void Semantics::walkDataStatement(Node *node)
{
    auto dataBlockStmt = dynamic_cast<DataStatement *>(node);
    if (!dataBlockStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing data statement: "
              << dataBlockStmt->toString() << "\n";

    // Get block name
    std::string dataBlockName = dataBlockStmt->dataBlockName->expression.TokenLiteral;

    // Ensure name not already used
    if (resolveSymbolInfo(dataBlockName))
    {
        logSemanticErrors(
            "Already used the name '" + dataBlockName + "'",
            dataBlockStmt->statement.line,
            dataBlockStmt->statement.column);
        return;
    }

    // Setup mutability
    bool isBlockMutable = (dataBlockStmt->mutability == Mutability::MUTABLE);
    bool isBlockConstant = (dataBlockStmt->mutability == Mutability::CONSTANT);

    // Create new local scope for analysis
    symbolTable.push_back({});

    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> dataBlockMembers;

    // Analyze each field
    for (const auto &field : dataBlockStmt->fields)
    {
        auto letStmt = dynamic_cast<LetStatement *>(field.get());
        if (!letStmt)
        {
            logSemanticErrors(
                "Invalid statement inside a data block",
                field->statement.line,
                field->statement.column);
            continue; // skip bad field but keep going
        }

        // Apply block mutability if set
        if (isBlockMutable)
            letStmt->mutability = Mutability::MUTABLE;
        else if (isBlockConstant)
            letStmt->mutability = Mutability::CONSTANT;

        // Walk the let statement to register it in the scope
        walker(field.get());

        // Now retrieve its symbol
        auto letSymbol = resolveSymbolInfo(letStmt->ident_token.TokenLiteral);
        if (!letSymbol)
        {
            logSemanticErrors(
                "Let statement '" + letStmt->ident_token.TokenLiteral +
                    "' was not analyzed properly",
                letStmt->statement.line,
                letStmt->statement.column);
            continue;
        }

        bool isHeap = letSymbol->isHeap;
        StorageType memberStorage;
        if (isHeap)
        {
            memberStorage = StorageType::HEAP;
        }
        else
        {
            memberStorage = StorageType::STACK;
        }

        // Build member info
        auto memInfo = std::make_shared<MemberInfo>();
        memInfo->memberName = letStmt->ident_token.TokenLiteral;
        memInfo->type = letSymbol->type;
        memInfo->isMutable = letSymbol->isMutable;
        memInfo->isConstant = letSymbol->isConstant;
        memInfo->isInitialised = letSymbol->isInitialized;
        memInfo->isHeap = isHeap;
        memInfo->storage = memberStorage;
        memInfo->node = field.get();

        // Insert into members map
        dataBlockMembers[letStmt->ident_token.TokenLiteral] = memInfo;

        std::cout << "[SEMANTIC LOG] Added field '"
                  << letStmt->ident_token.TokenLiteral
                  << "' to data block '" << dataBlockName << "'\n";
    }

    int currentMemberIndex = 0;
    for (auto &kv : dataBlockMembers)
    {
        kv.second->memberIndex = currentMemberIndex++;
    }

    // Build symbol info for the whole block
    auto dataSymbolInfo = std::make_shared<SymbolInfo>();
    dataSymbolInfo->type = ResolvedType{DataType::DATABLOCK, dataBlockName};
    dataSymbolInfo->isMutable = isBlockMutable;
    dataSymbolInfo->isConstant = isBlockConstant;
    dataSymbolInfo->members = dataBlockMembers;
    dataSymbolInfo->isDataBlock = true;

    for (auto &kv : dataSymbolInfo->members)
    {
        auto it = dataBlockMembers.find(kv.first);
        if (it != dataBlockMembers.end())
            kv.second->memberIndex = it->second->memberIndex;
    }

    // Build customtype info
    auto typeInfo = std::make_shared<CustomTypeInfo>();

    typeInfo->typeName = dataBlockName;
    typeInfo->type = ResolvedType{DataType::DATABLOCK, dataBlockName},
    typeInfo->members = dataBlockMembers;

    // Propagate indices to customTypesTable
    for (auto &kv : typeInfo->members)
    {
        auto it = dataBlockMembers.find(kv.first);
        if (it != dataBlockMembers.end())
            kv.second->memberIndex = it->second->memberIndex;
    }

    // Store results
    metaData[dataBlockStmt] = dataSymbolInfo;
    symbolTable[0][dataBlockName] = dataSymbolInfo;
    customTypesTable[dataBlockName] = typeInfo;

    // Pop local scope
    popScope();
}

void Semantics::walkInstanceExpression(Node *node)
{
    auto instExpr = dynamic_cast<InstanceExpression *>(node);
    if (!instExpr)
        return;

    auto instName = instExpr->blockIdent->expression.TokenLiteral;
    auto line = instExpr->blockIdent->expression.line;
    auto col = instExpr->blockIdent->expression.column;
    bool hasError = false;

    auto instSym = resolveSymbolInfo(instName);

    if (!instSym)
    {
        logSemanticErrors("Undefined identifier '" + instName + "'", line, col);
        hasError = true;
    }

    if (instSym->type.kind != DataType::DATABLOCK)
    {
        logSemanticErrors("'" + instName + "' is not a data block", line, col);
        hasError = true;
    }

    // Dealing with arguments if they exist
    if (!instExpr->fields.empty())
    {
        auto members = instSym->members;
        for (const auto &field : instExpr->fields)
        {
            auto fieldNode = dynamic_cast<AssignmentStatement *>(field.get());
            auto fieldName = fieldNode->identifier->expression.TokenLiteral;
            // Check if the member exists in the parent type
            auto it = members.find(fieldName);
            if (it == members.end())
            {
                logSemanticErrors("Field '" + fieldName + "' does not exist in '" + instName + "'", line, col);
                hasError = true;
            }

            walker(fieldNode->value.get());
        }
    }

    ResolvedType instType = instSym->type;
    std::cout << "Instance type: " << instType.resolvedName << "\n";

    auto instInfo = std::make_shared<SymbolInfo>();
    instInfo->type = instType;
    instInfo->hasError = hasError;

    metaData[instExpr] = instInfo;
}

void Semantics::walkBehaviorStatement(Node *node)
{
    auto behaviorStmt = dynamic_cast<BehaviorStatement *>(node);
    if (!behaviorStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing behavior statement: "
              << behaviorStmt->toString() << "\n";

    // Get behavior block name
    std::string behaviorName = behaviorStmt->behaviorBlockName->expression.TokenLiteral;

    bool hasError = false;
    insideBehavior = true;

    // Ensure name not already used
    if (resolveSymbolInfo(behaviorName))
    {
        logSemanticErrors(
            "Already used the name '" + behaviorName + "'",
            behaviorStmt->behaviorBlockName->expression.line,
            behaviorStmt->behaviorBlockName->expression.column);
        return;
    }

    // Push a new scope for analysis
    symbolTable.push_back({});

    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> behaviorMembers;

    // Analyze each function inside the behavior
    for (const auto &func : behaviorStmt->functions)
    {
        auto funcStmt = dynamic_cast<FunctionStatement *>(func.get());
        if (!funcStmt)
        {
            logSemanticErrors(
                "Only function statements are allowed inside a behavior block",
                func->statement.line,
                func->statement.column);
            continue; // keep checking the rest
        }

        // If we get a function expression this isnt allowed inside behavior blocks
        if (auto funcExpr = dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get()))
        {
            auto exprSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
            if (!exprSym)
            {
                logSemanticErrors(
                    "Function definition '" + funcExpr->func_key.TokenLiteral + "' prohibited in behavior blocks",
                    funcExpr->expression.line,
                    funcExpr->expression.column);
                hasError = true;
            }
        }

        // Let the walker handle function analysis
        auto funcDeclExpr = dynamic_cast<FunctionDeclarationExpression *>(funcStmt->funcExpr.get());
        walker(funcDeclExpr);

        auto funcInfo = std::make_shared<MemberInfo>();

        if (funcDeclExpr)
        {
            auto funcDecl = dynamic_cast<FunctionDeclaration *>(funcDeclExpr->funcDeclrStmt.get());
            if (!funcDecl)
            {
                logSemanticErrors(
                    "Invalid function declaration inside behavior block",
                    funcDeclExpr->expression.line,
                    funcDeclExpr->expression.column);
                continue;
            }

            auto declSym = resolveSymbolInfo(funcDecl->function_name->expression.TokenLiteral);
            if (!declSym)
            {
                logSemanticErrors(
                    "Function declaration '" + funcDecl->function_name->expression.TokenLiteral + "' was not analyzed properly",
                    funcDecl->statement.line,
                    funcDecl->statement.column);
                continue;
            }

            funcInfo->memberName = funcDecl->function_name->expression.TokenLiteral;
            funcInfo->type = declSym->type;
            funcInfo->paramTypes = declSym->paramTypes;
            funcInfo->returnType = declSym->returnType;
            funcInfo->isDeclared = true;
            funcInfo->node = funcStmt;

            behaviorMembers[funcDecl->function_name->expression.TokenLiteral] = funcInfo;

            std::cout << "[SEMANTIC LOG] Added function declaration '"
                      << funcDecl->function_name->expression.TokenLiteral
                      << "' to behavior '" << behaviorName << "'\n";
        }
    }

    // Build symbol info
    auto sym = std::make_shared<SymbolInfo>();
    sym->type = ResolvedType{DataType::BEHAVIORBLOCK, behaviorName};
    sym->isBehavior = true;
    sym->members = behaviorMembers;

    // Build custom type info
    auto typeInfo = std::make_shared<CustomTypeInfo>();

    typeInfo->typeName = behaviorName;
    typeInfo->type = ResolvedType{DataType::BEHAVIORBLOCK, behaviorName};
    typeInfo->members = behaviorMembers;

    // Store results
    symbolTable[0][behaviorName] = sym; // All behavior blocks are hoisted to global scope
    metaData[behaviorStmt] = sym;
    customTypesTable[behaviorName] = typeInfo;

    insideBehavior = false;
    // Pop scope
    popScope();
}

void Semantics::walkInitConstructor(Node *node)
{
    auto initStmt = dynamic_cast<InitStatement *>(node);
    if (!initStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing init constructor\n";

    // Must be inside a component

    if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
    {
        std::cout << "TYPE STACK IS EMPTY\n";
        logSemanticErrors("`init` constructor must be declared inside a component",
                          initStmt->statement.line, initStmt->statement.column);
        return;
    }

    auto &currentComponent = currentTypeStack.back();

    // Only one init constructor per component
    if (currentComponent.hasInitConstructor)
    {
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
    initInfo->isDefined = true;

    auto currentComponentName = currentComponent.typeName;
    std::vector<ResolvedType> initArgs;
    // Process constructor parameters
    symbolTable.push_back({});
    for (const auto &arg : initStmt->constructor_args)
    {
        walker(arg.get());
        // Storing the init args
        initArgs.push_back(inferNodeDataType(arg.get()));
    }
    componentInitArgs[currentComponentName] = initArgs;

    // Walk the constructor body
    if (initStmt->block)
    {
        walkBlockStatement(initStmt->block.get());
    }
    popScope();

    // Attach metadata
    metaData[node] = initInfo;

    std::cout << "[SEMANTIC LOG] Init constructor added to component '"
              << currentComponent.typeName << "'\n";
}

void Semantics::walkSelfExpression(Node *node)
{
    auto selfExpr = dynamic_cast<SelfExpression *>(node);
    if (!selfExpr)
        return;

    std::cout << "[SEMANTIC LOG] Analysing field access expression: "
              << selfExpr->toString() << "\n";

    // Must be inside a component
    if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
    {
        logSemanticErrors("'self' cannot be used outside a component",
                          selfExpr->expression.line, selfExpr->expression.column);
        return;
    }

    // The self must be inside a method or

    const std::string &componentName = currentTypeStack.back().typeName;
    auto ctIt = customTypesTable.find(componentName);
    if (ctIt == customTypesTable.end())
    {
        // Shouldn’t happen if walkComponentStatement registered it
        logSemanticErrors("Internal error: unknown component '" + componentName + "'",
                          selfExpr->expression.line, selfExpr->expression.column);
        return;
    }

    // Calling the walker on the actual field expression
    walker(selfExpr->field.get());

    const std::string fieldName = selfExpr->field->expression.TokenLiteral;
    std::cout << "NAME BEING GOTTEN FROM SELF EXPRESSION: " << fieldName << "\n";
    const auto &members = ctIt->second->members;
    auto memIt = members.find(fieldName);
    if (memIt == members.end())
    {
        logSemanticErrors("'" + fieldName + "' does not exist in component '" + componentName + "'",
                          selfExpr->expression.line, selfExpr->expression.column);
        return;
    }

    std::shared_ptr<MemberInfo> m = memIt->second;

    // Attach type info for this field access
    auto info = std::make_shared<SymbolInfo>();
    info->type = m->type;
    info->isNullable = m->isNullable;
    info->isMutable = m->isMutable;
    info->isConstant = m->isConstant;
    info->isInitialized = m->isInitialised;
    info->memberIndex = m->memberIndex;

    metaData[selfExpr] = info;

    std::cout << "[SEMANTIC LOG] Field access 'self." << fieldName
              << "' resolved in component '" << componentName << "'\n";
}

void Semantics::walkComponentStatement(Node *node)
{
    auto componentStmt = dynamic_cast<ComponentStatement *>(node);
    if (!componentStmt)
        return;

    auto componentName = componentStmt->component_name->expression.TokenLiteral;
    std::cout << "[SEMANTIC LOG] Analyzing component '" << componentName << "'\n";
    bool hasError = false;
    insideComponent = true;

    if (symbolTable[0].find(componentName) != symbolTable[0].end())
    {
        logSemanticErrors("Component '" + componentName + "' already exists", componentStmt->statement.line, componentStmt->statement.column);
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
    currentTypeStack.push_back({.type = ResolvedType{DataType::COMPONENT, componentName},
                                .typeName = componentName,
                                .hasInitConstructor = false,
                                .members = members,
                                .node = componentStmt});

    // Walk data/behavior imports
    for (const auto &usedData : componentStmt->usedDataBlocks)
    {
        if (!usedData)
        {
            std::cout << "Invalid used data node\n";
            return;
        }

        auto useStmt = dynamic_cast<UseStatement *>(usedData.get());
        if (!useStmt)
        {
            std::cout << "Invalid use statement \n";
            continue;
        }

        // Mass import case
        auto ident = dynamic_cast<Identifier *>(useStmt->blockNameOrCall.get());
        if (ident)
        {
            auto identName = ident->expression.TokenLiteral;
            std::cout << "IMPORTING DATA FROM: " << identName << "\n";

            auto typeIt = customTypesTable.find(identName);
            if (typeIt == customTypesTable.end())
            {
                logSemanticErrors("Data block with name '" + identName + "' does not exist", ident->expression.line, ident->expression.column);
                hasError = true;
            }
            auto &importedMembers = typeIt->second->members;
            auto &currentScope = symbolTable.back();
            for (auto &[name, info] : importedMembers)
            {
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

                if (memberCopy->node)
                {
                    metaData[memberCopy->node] = memSym;
                    std::cout << "[SEMANTIC LOG] mapped member node " << memberCopy->node
                              << " -> symbol for '" << name << "'\n";
                }
                else
                {
                    // If the node wasnt populated
                    std::cout << "[SEMANTIC WARN] imported member '" << name << "' has no node\n";
                }
            }
        }

        // Specific import case
        auto infixExpr = dynamic_cast<InfixExpression *>(useStmt->blockNameOrCall.get());
        if (infixExpr)
        {
            auto leftIdent = dynamic_cast<Identifier *>(infixExpr->left_operand.get());
            auto rightIdent = dynamic_cast<Identifier *>(infixExpr->right_operand.get());
            if (!leftIdent || !rightIdent)
                return;

            TokenType op = infixExpr->operat.type;

            if (op != TokenType::AT)
            {
                logSemanticErrors("Invalid operator '" + infixExpr->operat.TokenLiteral + "' for explicit use statement import",
                                  infixExpr->operat.line, infixExpr->operat.column);
                hasError = true;
            }

            auto dataName = leftIdent->expression.TokenLiteral;
            auto memberName = rightIdent->expression.TokenLiteral;

            auto importedTypeIt = customTypesTable.find(dataName);
            if (importedTypeIt != customTypesTable.end())
            {
                auto &importedMembers = importedTypeIt->second->members;
                auto memIt = importedMembers.find(memberName);
                if (memIt != importedMembers.end())
                {
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

    for (const auto &usedBehavior : componentStmt->usedBehaviorBlocks)
    {
        if (!usedBehavior)
        {
            std::cout << "Invalid used behavior node\n";
            return;
        }

        auto useStmt = dynamic_cast<UseStatement *>(usedBehavior.get());
        if (!useStmt)
        {
            std::cout << "Invalid use statement\n";
            continue;
        }

        // Mass import case
        auto ident = dynamic_cast<Identifier *>(useStmt->blockNameOrCall.get());
        if (ident)
        {
            auto identName = ident->expression.TokenLiteral;
            std::cout << "IMPORTING DATA FROM: " << identName << "\n";

            auto typeIt = customTypesTable.find(identName);
            if (typeIt == customTypesTable.end())
            {
                logSemanticErrors("Method block with name '" + identName + "' does not exist", ident->expression.line, ident->expression.column);
                hasError = true;
            }
            auto &importedMembers = typeIt->second->members;
            auto &currentScope = symbolTable.back();
            for (auto &[name, info] : importedMembers)
            {
                auto memberCopy = std::make_shared<MemberInfo>();
                memberCopy->memberName = info->memberName;
                memberCopy->type = info->type;
                memberCopy->isNullable = info->isNullable;
                memberCopy->isMutable = info->isMutable;
                memberCopy->isConstant = info->isConstant;
                memberCopy->isInitialised = info->isInitialised;
                memberCopy->paramTypes = info->paramTypes;
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
                memSym->isDefined = true;
                currentScope[name] = memSym;

                componentTypeInfo->members = members; // Update the typeInfo member storage

                if (memberCopy->node)
                {
                    metaData[memberCopy->node] = memSym;
                    std::cout << "[SEMANTIC LOG] mapped member node " << memberCopy->node
                              << " -> symbol for '" << name << "'\n";
                }
                else
                {
                    // If the node wasnt populated
                    std::cout << "[SEMANTIC WARN] imported member '" << name << "' has no node\n";
                }
            }
        }

        // Specific import case
        auto infixExpr = dynamic_cast<InfixExpression *>(useStmt->blockNameOrCall.get());
        if (infixExpr)
        {
            auto leftIdent = dynamic_cast<Identifier *>(infixExpr->left_operand.get());
            auto rightIdent = dynamic_cast<Identifier *>(infixExpr->right_operand.get());
            if (!leftIdent || !rightIdent)
                return;

            TokenType op = infixExpr->operat.type;

            if (op != TokenType::AT)
            {
                logSemanticErrors("Invalid operator '" + infixExpr->operat.TokenLiteral + "' for explicit use statement import",
                                  infixExpr->operat.line, infixExpr->operat.column);
                hasError = true;
            }

            auto methodName = leftIdent->expression.TokenLiteral;
            auto memberName = rightIdent->expression.TokenLiteral;

            auto importedTypeIt = customTypesTable.find(methodName);
            if (importedTypeIt != customTypesTable.end())
            {
                auto &importedMembers = importedTypeIt->second->members;
                auto memIt = importedMembers.find(memberName);
                if (memIt != importedMembers.end())
                {
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
                    memSym->memberIndex = memberCopy->memberIndex;
                    memSym->paramTypes = memberCopy->paramTypes;
                    memSym->isDefined = true;
                    symbolTable.back()[memberName] = memSym;
                    componentTypeInfo->members = members; // Update the typeInfo members
                }
            }
        }
    }

    for (const auto &data : componentStmt->privateData)
    {
        auto letStmt = dynamic_cast<LetStatement *>(data.get());

        if (letStmt)
        {
            walker(letStmt);
            auto letSym = resolveSymbolInfo(letStmt->ident_token.TokenLiteral);
            if (!letSym)
            {
                logSemanticErrors("Failed to resolve private data '" + letStmt->ident_token.TokenLiteral + "'",
                                  letStmt->ident_token.line, letStmt->ident_token.column);
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
            memInfo->memberIndex = currentMemberIndex++;

            members[letStmt->ident_token.TokenLiteral] = memInfo;
            letSym->memberIndex = memInfo->memberIndex;

            metaData[letStmt] = letSym;
            componentTypeInfo->members = members;
        }

        else
        {
            logSemanticErrors("Executable statement found in component '" + componentName + "' definition scope", data->statement.line, data->statement.column);
            hasError = true;
        }
    }

    for (const auto &method : componentStmt->privateMethods)
    {
        auto funcStmt = dynamic_cast<FunctionStatement *>(method.get());
        if (!funcStmt)
            continue;

        auto funcExpr = dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get());
        std::string funcName = "unknown";

        if (funcExpr)
        {
            funcName = funcExpr->func_key.TokenLiteral;
            // Assume funcExpr is the current component method being processed
            auto &currentMembers = currentTypeStack.back().members;

            // Check if this method exists in the imported behavior
            auto behaviorIt = currentMembers.find(funcName);
            if (behaviorIt != currentMembers.end())
            {
                auto &behaviorMethod = behaviorIt->second;

                // Compare signatures directly
                bool matches = signaturesMatchBehaviorDeclaration(behaviorMethod, funcExpr);
                if (!matches)
                {
                    logSemanticErrors(
                        "Component function '" + funcExpr->func_key.TokenLiteral +
                            "' does not match imported behavior declaration",
                        funcExpr->func_key.line, funcExpr->func_key.column);
                    continue; // skip registration, it's private and mismatched
                }
            }

            walker(funcExpr);
            auto metSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
            std::cout << "TRYING TO INSERT: " << funcExpr->func_key.TokenLiteral << "\n";
            if (metSym)
            {
                std::cout << "INSERTING COMPONENT FUNCTION EXPRESSION\n";
                auto memInfo = std::make_shared<MemberInfo>();
                memInfo->memberName = funcExpr->func_key.TokenLiteral;
                memInfo->type = metSym->type;
                memInfo->isNullable = metSym->isNullable;
                memInfo->isMutable = metSym->isMutable;
                memInfo->isDefined = true;
                memInfo->paramTypes = metSym->paramTypes;
                memInfo->node = funcExpr;
                members[funcName] = memInfo;
            }
            componentTypeInfo->members = members; // Update the custom type table members
        }
        else
        {
            auto funcDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(funcStmt->funcExpr.get());
            if (funcDeclrExpr && funcDeclrExpr->funcDeclrStmt)
            {
                auto funcDeclrStmt = dynamic_cast<FunctionDeclaration *>(funcDeclrExpr->funcDeclrStmt.get());
                if (funcDeclrStmt)
                {
                    funcName = funcDeclrStmt->function_name->expression.TokenLiteral;
                }
            }
            logSemanticErrors("Cannot use function declarations inside a component '" + componentName + "'", funcDeclrExpr->expression.line, funcDeclrExpr->expression.column);
            hasError = true;
        }
    }

    // Register component as a symbol
    componentSymbol->members = members;
    componentTypeInfo->members = members;
    componentTypeInfo->typeName = componentName;
    componentTypeInfo->type = ResolvedType{DataType::COMPONENT, componentName};

    customTypesTable[componentName] = componentTypeInfo;

    if (componentStmt->initConstructor.has_value())
        walkInitConstructor(componentStmt->initConstructor.value().get());

    if (members.empty())
    {
        std::cout << "COMPONENT MEMBERS ARE EMPTY AT RUNTIME\n";
    }

    // Exit component scope
    currentTypeStack.pop_back();
    insideComponent = false;
    popScope();
}

void Semantics::walkNewComponentExpression(Node *node)
{
    auto newExpr = dynamic_cast<NewComponentExpression *>(node);
    if (!newExpr)
        return;

    auto line = newExpr->expression.line;
    auto column = newExpr->expression.column;
    bool hasError = false;

    // Get component name
    auto componentName = newExpr->component_name.TokenLiteral;

    // Look up component type
    auto componentIt = customTypesTable.find(componentName);
    if (componentIt == customTypesTable.end())
    {
        logSemanticErrors("Component '" + componentName + "' does not exist", line, column);
        return;
    }

    // Look up init constructor info
    auto it = componentInitArgs.find(componentName);
    bool hasInitConstructor = (it != componentInitArgs.end());
    const auto &givenArgs = newExpr->arguments;

    // Case 1: Component has NO init constructor
    if (!hasInitConstructor)
    {
        if (!givenArgs.empty())
        {
            logSemanticErrors("Component '" + componentName +
                                  "' has no init constructor, arguments not allowed.",
                              line, column);
            hasError = true;
        }
        // store metadata and exit early
        auto info = std::make_shared<SymbolInfo>();
        info->type = componentIt->second->type;
        info->hasError = hasError;
        metaData[newExpr] = info;
        return;
    }

    const auto &expectedArgs = it->second;

    // Case 2: Init constructor exists but is EMPTY
    if (expectedArgs.empty())
    {
        if (!givenArgs.empty())
        {
            logSemanticErrors("Init constructor for '" + componentName +
                                  "' takes no arguments.",
                              line, column);
            hasError = true;
        }
        auto info = std::make_shared<SymbolInfo>();
        info->type = componentIt->second->type;
        info->hasError = hasError;
        metaData[newExpr] = info;
        return;
    }

    // Case 3: Init constructor expects arguments but user provided NONE
    if (givenArgs.empty())
    {
        logSemanticErrors("Constructor for component '" + componentName +
                              "' expects " + std::to_string(expectedArgs.size()) +
                              " arguments but got none.",
                          line, column);
        hasError = true;

        auto info = std::make_shared<SymbolInfo>();
        info->type = componentIt->second->type;
        info->hasError = hasError;
        metaData[newExpr] = info;
        return;
    }

    // Case 4: Argument count mismatch
    if (expectedArgs.size() != givenArgs.size())
    {
        logSemanticErrors("Constructor for component '" + componentName +
                              "' expects " + std::to_string(expectedArgs.size()) +
                              " arguments but got " + std::to_string(givenArgs.size()) + ".",
                          line, column);
        hasError = true;
    }

    // Case 5: Type checks (pairwise, safe even if sizes differ)
    for (size_t i = 0; i < std::min(expectedArgs.size(), givenArgs.size()); ++i)
    {
        walker(givenArgs[i].get());
        ResolvedType argType = inferNodeDataType(givenArgs[i].get());
        ResolvedType expectedType = expectedArgs[i];

        if (argType.kind != expectedType.kind)
        {
            logSemanticErrors("Type mismatch in argument " + std::to_string(i + 1) +
                                  " of constructor for '" + componentName +
                                  "'. Expected '" + expectedType.resolvedName +
                                  "' but got '" + argType.resolvedName + "'.",
                              line, column);
            hasError = true;
        }
    }

    // Special case (I am going to store the component symbol info so that I can retrieve it later I need it in IRGEN)
    // Get the component symbol info and store it in the instance symbol table

    auto componentSym = resolveSymbolInfo(componentName); // I will use the scope resolver as all components are always in globals scope
    if (!componentSym)                                    // Chances of this happening are low but who knows
    {
        logSemanticErrors("Component '" + componentName + "' doesnt not exist '", line, column);
        hasError = true;
    }

    // === Store metadata ===
    auto info = std::make_shared<SymbolInfo>();
    info->type = componentIt->second->type;
    info->hasError = hasError;
    info->componentSymbol = componentSym;
    metaData[newExpr] = info;
}

void Semantics::walkMethodCallExpression(Node *node)
{
    auto metCall = dynamic_cast<MethodCallExpression *>(node);
    if (!metCall)
    {
        std::cout << "Invalid method call expression node";
        return;
    }

    std::cout << "Analysing method call expression\n";
    bool hasError = false;
    // Get the instance name
    auto instanceName = metCall->instance->expression.TokenLiteral;
    auto line = metCall->instance->expression.line;
    auto col = metCall->instance->expression.column;
    // Check the symbol table for the instance name
    auto instanceSym = resolveSymbolInfo(instanceName);
    if (!instanceSym)
    {
        logSemanticErrors("Unidentified instance name '" + instanceName + "'", line, col);
        return;
    }

    walker(metCall->instance.get()); // Walk the instance as I need its metaData in the IR

    // Get the type
    auto type = instanceSym->type;
    auto typeIt = customTypesTable.find(type.resolvedName);
    if (typeIt == customTypesTable.end())
    {
        logSemanticErrors("Unknown type '" + type.resolvedName + "'", line, col);
        hasError = true;
        return;
    }

    // If the symbol actually exists we retrieve the members and check if the function we are calling is among them
    auto members = typeIt->second->members;
    // Retrieve the function name itself
    auto funcCall = dynamic_cast<CallExpression *>(metCall->call.get());
    auto funcName = funcCall->function_identifier->expression.TokenLiteral;
    auto funcLine = funcCall->function_identifier->expression.line;
    auto funcCol = funcCall->function_identifier->expression.column;

    // Check if the function is in the members
    auto memIt = members.find(funcName);
    if (memIt == members.end())
    {
        logSemanticErrors("'Function " + funcName + "' does not exist in type '" + type.resolvedName + "'", funcLine, funcCol);
        return;
    }

    auto memInfo = memIt->second;

    // If the member exists carry out some checks
    // Definition check
    if (!memInfo->isDefined)
    {
        logSemanticErrors("'" + funcName + "' was not defined anywhere", funcLine, funcCol);
        hasError = true;
    }

    // Compatibility check
    if (!isMethodCallCompatible(*memInfo, funcCall))
    {
        hasError = true;
    }

    // Walking the arguments
    for (const auto &arg : funcCall->parameters)
    {
        walker(arg.get());
    }

    // Update the instance symbol metaData
    instanceSym->lastUseNode = metCall;
    if (instanceSym->refCount)
    {
        instanceSym->refCount--;
    }

    // Store the metaData
    auto metCallSym = std::make_shared<SymbolInfo>();
    metCallSym->hasError = hasError;
    metCallSym->type = memInfo->returnType;
    metCallSym->isNullable = memInfo->isNullable;

    metaData[metCall] = metCallSym;
}

void Semantics::walkArrayStatement(Node *node)
{
    auto arrStmt = dynamic_cast<ArrayStatement *>(node);
    if (!arrStmt)
        return;

    // --- Mutability flags ---
    bool isMutable = arrStmt->mutability == Mutability::MUTABLE;
    bool isConstant = arrStmt->mutability == Mutability::CONSTANT;

    const auto &arrayName = arrStmt->identifier->expression.TokenLiteral;
    int arrNameLine = arrStmt->identifier->expression.line;
    int arrNameCol = arrStmt->identifier->expression.column;

    if (resolveSymbolInfo(arrayName))
    {
        logSemanticErrors("Array name '" + arrayName + "' already exists", arrNameLine, arrNameCol);
        return;
    }

    bool hasError = false;
    bool isInitialized = false;
    bool isHeap = arrStmt->isHeap;
    ArrayTypeInfo arrTypeInfo;

    // --- Base type inference ---
    ResolvedType baseType = inferNodeDataType(arrStmt->arrayType.get());
    ResolvedType arrayType;
    // Get the array declaration dimensions
    int arrStmtDimensions = 0;
    // If the array dimesions are empty infer from the array literal but only if the array literal exists
    if (arrStmt->lengths.empty())
    {
        if (!arrStmt->array_content)
        {
            logSemanticErrors("Cannot infer dimension count if you do not initialize array declaration '" + arrayName + "'", arrNameLine, arrNameCol);
            return;
        }
        else
        {
            auto literalType = inferNodeDataType(arrStmt->array_content.get());
            arrStmtDimensions = getArrayTypeInfo(arrStmt->array_content.get()).dimensions;
            arrayType = literalType;
        }
    }
    else
    {
        arrStmtDimensions = arrStmt->lengths.size();
        arrayType = makeArrayType(baseType, arrStmtDimensions);
    }

    std::cout
        << "BASE TYPE: " << baseType.resolvedName << "\n";
    std::cout << "ARRAY TYPE: " << arrayType.resolvedName << "\n";

    arrTypeInfo.underLyingType = baseType;

    // --- Walk length expressions ---
    for (const auto &len : arrStmt->lengths)
        walker(len.get());

    // --- Handle initializer ---
    if (arrStmt->array_content)
    {
        walker(arrStmt->array_content.get());
        isInitialized = true;

        ArrayLiteral *arrLit = dynamic_cast<ArrayLiteral *>(arrStmt->array_content.get());
        if (!arrLit)
        {
            std::cerr << "Only use array literals for array statements\n";
            hasError = true;
            return;
        }

        ResolvedType literalType = inferNodeDataType(arrLit);

        // Retrieve the array literal dimensions
        int arrLitDimensions = getArrayTypeInfo(arrLit).dimensions;

        // Dimension check
        if (arrStmtDimensions != arrLitDimensions)
        {
            logSemanticErrors("Dimensions mismatch in array declaration '" + arrayName + "' expected '" + std::to_string(arrStmtDimensions) + "' but got '" + std::to_string(arrLitDimensions) + "'", arrNameLine, arrNameCol);
            hasError = true;
        }

        if (!isTypeCompatible(arrayType, literalType))
        {
            logSemanticErrors("Declared type '" + arrayType.resolvedName +
                                  "' does not match the initialized type '" + literalType.resolvedName + "'",
                              arrLit->expression.line, arrLit->expression.column);
            hasError = true;
        }
    }
    else if (isConstant)
    {
        logSemanticErrors("Constant array '" + arrayName + "' must be initialized",
                          arrStmt->statement.line, arrStmt->statement.column);
        hasError = true;
    }
    else if (arrStmt->lengths.empty())
    {
        logSemanticErrors("Uninitialized array '" + arrayName + "' is missing length declarations",
                          arrStmt->statement.line, arrStmt->statement.column);
        hasError = true;
    }

    arrTypeInfo.dimensions = arrStmtDimensions;

    // --- Store symbol info ---
    auto arrInfo = std::make_shared<SymbolInfo>();
    arrInfo->isMutable = isMutable;
    arrInfo->isConstant = isConstant;
    arrInfo->arrayTyInfo = arrTypeInfo;
    arrInfo->hasError = hasError;
    arrInfo->isInitialized = isInitialized;
    arrInfo->isHeap = isHeap;
    arrInfo->lastUseNode = arrStmt;
    arrInfo->type = arrayType; // store fully wrapped type
    arrInfo->arrayTyInfo = arrTypeInfo;

    metaData[arrStmt] = arrInfo;
    symbolTable.back()[arrayName] = arrInfo;

    std::cout << "FINAL ARRAY STATEMENT TYPE '" << arrayType.resolvedName << "'\n";
}
