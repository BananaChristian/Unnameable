#include "semantics.hpp"
#include <algorithm>

void Semantics::walkEnumClassStatement(Node *node)
{
    auto enumStmt = dynamic_cast<EnumClassStatement *>(node);
    if (!enumStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analysing enum class statement\n";
    SymbolInfo enumInfo;

    // Getting the Enum name
    std::string enumStmtName = enumStmt->enum_identifier->expression.TokenLiteral;
    enumInfo.enumName = enumStmtName;

    // Checking for duplicate enum name
    if (resolveSymbolInfo(enumStmtName))
    {
        logSemanticErrors("Already used the name '" + enumStmtName + "'", enumStmt->statement.line, enumStmt->statement.column);
        return;
    }

    // Determining underlying type if none we default to INTEGER
    if (enumStmt->int_type.has_value())
    {
        switch (enumStmt->int_type.value().type)
        {
        case TokenType::SHORT_KEYWORD:
            enumInfo.enumIntType = DataType::SHORT_INT;
            break;
        case TokenType::USHORT_KEYWORD:
            enumInfo.enumIntType = DataType::USHORT_INT;
            break;
        case TokenType::INTEGER_KEYWORD:
            enumInfo.enumIntType = DataType::INTEGER;
            break;
        case TokenType::UINT_KEYWORD:
            enumInfo.enumIntType = DataType::UINTEGER;
            break;
        case TokenType::LONG_KEYWORD:
            enumInfo.enumIntType = DataType::LONG_INT;
            break;
        case TokenType::ULONG_KEYWORD:
            enumInfo.enumIntType = DataType::ULONG_INT;
            break;
        case TokenType::EXTRA_KEYWORD:
            enumInfo.enumIntType = DataType::EXTRA_INT;
            break;
        case TokenType::UEXTRA_KEYWORD:
            enumInfo.enumIntType = DataType::UEXTRA_INT;
            break;
        default:
            enumInfo.enumIntType = DataType::INTEGER;
            break;
        }
    }
    else
    {
        enumInfo.enumIntType = DataType::INTEGER;
    }

    // Push new local scope for enum members
    symbolTable.push_back({});
    int currentValue = 0;

    auto matchesSuffix = [](const std::string &lit, const std::string &suffix)
    {
        return lit.size() > suffix.size() &&
               lit.compare(lit.size() - suffix.size(), suffix.size(), suffix) == 0;
    };

    auto stripSuffix = [](const std::string &lit, const std::string &suffix)
    {
        return lit.substr(0, lit.size() - suffix.size());
    };

    for (const auto &content : enumStmt->enum_content)
    {
        std::string memberName;
        int memberValue = 0;
        std::string literalValue;
        std::string detectedSuffix;

        if (auto infixExpr = dynamic_cast<InfixExpression *>(content.get()))
        {
            // Left side = identifier
            if (auto leftIdent = dynamic_cast<Identifier *>(infixExpr->left_operand.get()))
            {
                memberName = leftIdent->expression.TokenLiteral;
            }
            else
            {
                logSemanticErrors("Invalid enum member syntax (left side must be identifier)",
                                  content->expression.line, content->expression.column);
                symbolTable.pop_back();
                return;
            }

            // Right side = integer literal (we only accept literal here)
            if (auto rightVal = dynamic_cast<IntegerLiteral *>(infixExpr->right_operand.get()))
            {
                literalValue = rightVal->expression.TokenLiteral;

                // Detect suffix (check longer suffixes first)
                if (matchesSuffix(literalValue, "us"))
                    detectedSuffix = "us";
                else if (matchesSuffix(literalValue, "ue"))
                    detectedSuffix = "ue";
                else if (matchesSuffix(literalValue, "ul"))
                    detectedSuffix = "ul";
                else if (matchesSuffix(literalValue, "s"))
                    detectedSuffix = "s";
                else if (matchesSuffix(literalValue, "l"))
                    detectedSuffix = "l";
                else if (matchesSuffix(literalValue, "e"))
                    detectedSuffix = "e";
                else if (matchesSuffix(literalValue, "u"))
                    detectedSuffix = "u";

                // numeric part = literal without suffix
                std::string numericPart = detectedSuffix.empty() ? literalValue : stripSuffix(literalValue, detectedSuffix);

                try
                {
                    long long parsed = std::stoll(numericPart); // parse into wide type
                    // range checking could be added here if you want
                    memberValue = static_cast<int>(parsed); // stored as int per your struct
                }
                catch (...)
                {
                    logSemanticErrors("Invalid numeric value '" + literalValue + "' for enum member",
                                      content->expression.line, content->expression.column);
                    symbolTable.pop_back();
                    return;
                }

                // Enforce correct suffix per enum underlying type
                bool suffixOk = false;
                switch (enumInfo.enumIntType)
                {
                case DataType::SHORT_INT:
                    suffixOk = (detectedSuffix == "s" || detectedSuffix.empty());
                    break;
                case DataType::USHORT_INT:
                    suffixOk = (detectedSuffix == "us" || detectedSuffix == "u");
                    break;
                case DataType::INTEGER:
                    suffixOk = (detectedSuffix.empty());
                    break;
                case DataType::UINTEGER:
                    suffixOk = (detectedSuffix == "u");
                    break;
                case DataType::LONG_INT:
                    suffixOk = (detectedSuffix == "l" || detectedSuffix.empty());
                    break;
                case DataType::ULONG_INT:
                    suffixOk = (detectedSuffix == "ul" || detectedSuffix == "u");
                    break;
                case DataType::EXTRA_INT:
                    suffixOk = (detectedSuffix == "e" || detectedSuffix.empty());
                    break;
                case DataType::UEXTRA_INT:
                    suffixOk = (detectedSuffix == "ue" || detectedSuffix == "u");
                    break;
                default:
                    suffixOk = (detectedSuffix.empty());
                    break;
                }

                if (!suffixOk)
                {
                    logSemanticErrors("Enum member '" + memberName + "' value '" + literalValue +
                                          "' does not match required suffix for enum type",
                                      content->expression.line, content->expression.column);
                    symbolTable.pop_back();
                    return;
                }

                // Unsigned value cannot be negative
                if ((enumInfo.enumIntType == DataType::UINTEGER ||
                     enumInfo.enumIntType == DataType::USHORT_INT ||
                     enumInfo.enumIntType == DataType::ULONG_INT ||
                     enumInfo.enumIntType == DataType::UEXTRA_INT) &&
                    memberValue < 0)
                {
                    logSemanticErrors("Enum member value '" + literalValue + "' cannot be negative for unsigned enum type",
                                      content->expression.line, content->expression.column);
                    symbolTable.pop_back();
                    return;
                }
            }
            else
            {
                logSemanticErrors("Enum member value must be an integer literal",
                                  content->expression.line, content->expression.column);
                symbolTable.pop_back();
                return;
            }
        }
        else if (auto identExpr = dynamic_cast<Identifier *>(content.get()))
        {
            memberName = identExpr->expression.TokenLiteral;
            memberValue = currentValue;
        }
        else
        {
            logSemanticErrors("Invalid enum member expression",
                              content->expression.line, content->expression.column);
            symbolTable.pop_back();
            return;
        }

        // Duplicate check
        if (std::find(enumInfo.enumContent.begin(), enumInfo.enumContent.end(), memberName) != enumInfo.enumContent.end())
        {
            logSemanticErrors("Cannot reuse existing enum class member '" + memberName + "'",
                              content->expression.line, content->expression.column);
            symbolTable.pop_back();
            return;
        }

        // Add member (store integer value only to match SymbolInfo)
        enumInfo.enumContent.push_back(memberName);
        enumInfo.enumMembers.push_back({memberName, memberValue});

        // Add member symbol to current scope
        symbolTable.back()[memberName] = SymbolInfo{
            .symbolDataType = enumInfo.enumIntType,
            .genericName = memberName,
            .isConstant = true,
            .isInitialized = true,
            .constantValue = memberValue,
        };

        currentValue = memberValue + 1;
    }

    // Add enum type symbol to outer scope
    SymbolInfo symbol = {
        .symbolDataType = DataType::ENUM,
        .enumName = enumInfo.enumName,
        .enumContent = enumInfo.enumContent,
        .enumMembers = enumInfo.enumMembers,
        .enumIntType = enumInfo.enumIntType,
    };

    CustomTypeInfo typeInfo = {
        .typeName = enumInfo.enumName,
        .kind = DataType::ENUM,
        .members = {}};

    symbolTable[symbolTable.size() - 2][enumStmtName] = symbol;
    metaData[enumStmt] = symbol;
    symbolTable.pop_back();
}

void Semantics::walkDataStatement(Node *node)
{
    auto dataBlockStmt = dynamic_cast<DataStatement *>(node);
    if (!dataBlockStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing data statement: "
              << dataBlockStmt->toString() << "\n";

    // 1. Get block name
    std::string dataBlockName = dataBlockStmt->dataBlockName->expression.TokenLiteral;

    // 2. Ensure name not already used
    if (resolveSymbolInfo(dataBlockName))
    {
        logSemanticErrors(
            "Already used the name '" + dataBlockName + "'",
            dataBlockStmt->statement.line,
            dataBlockStmt->statement.column);
        return;
    }

    // 3. Setup mutability
    bool isBlockMutable = (dataBlockStmt->mutability == Mutability::MUTABLE);
    bool isBlockConstant = (dataBlockStmt->mutability == Mutability::CONSTANT);

    // 4. Create new local scope for analysis
    symbolTable.push_back({});

    std::unordered_map<std::string, MemberInfo> dataBlockMembers;

    // 5. Analyze each field
    for (const auto &field : dataBlockStmt->fields)
    {
        auto letStmt = dynamic_cast<LetStatement *>(field.get());
        if (!letStmt)
        {
            logSemanticErrors(
                "Only let statements are allowed inside a data block",
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
        walkLetStatement(letStmt);

        // Now retrieve its symbol
        SymbolInfo *letSymbol = resolveSymbolInfo(letStmt->ident_token.TokenLiteral);
        if (!letSymbol)
        {
            logSemanticErrors(
                "Let statement '" + letStmt->ident_token.TokenLiteral +
                    "' was not analyzed properly",
                letStmt->statement.line,
                letStmt->statement.column);
            continue;
        }

        // Build member info
        MemberInfo memberInfo = {
            .memberName = letStmt->ident_token.TokenLiteral,
            .type = letSymbol->symbolDataType,
            .isMutable = letSymbol->isMutable,
            .isConstant = letSymbol->isConstant,
            .isInitialised = letSymbol->isInitialized};

        // Insert into members map
        dataBlockMembers[letStmt->ident_token.TokenLiteral] = memberInfo;

        std::cout << "[SEMANTIC LOG] Added field '"
                  << letStmt->ident_token.TokenLiteral
                  << "' to data block '" << dataBlockName << "'\n";
    }

    // Build symbol info for the whole block
    SymbolInfo dataSymbolInfo = {
        .symbolDataType = DataType::DATABLOCK,
        .isMutable = isBlockMutable,
        .isConstant = isBlockConstant,
        .members = dataBlockMembers};

    // Build custom type info
    CustomTypeInfo typeInfo = {
        .typeName = dataBlockName,
        .kind = DataType::DATABLOCK,
        .members = dataBlockMembers};

    // Store results
    metaData[dataBlockStmt] = dataSymbolInfo;
    symbolTable[0][dataBlockName] = dataSymbolInfo;
    customTypesTable[dataBlockName] = typeInfo;

    // Pop local scope
    symbolTable.pop_back();
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

    std::unordered_map<std::string, MemberInfo> behaviorMembers;

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

        // Let the walker handle function analysis
        walker(funcStmt);

        MemberInfo funcInfo;

        // Case A: function expression
        if (auto funcExpr = dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get()))
        {
            SymbolInfo *exprSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
            if (!exprSym)
            {
                logSemanticErrors(
                    "Function expression '" + funcExpr->func_key.TokenLiteral + "' was not analyzed properly",
                    funcExpr->expression.line,
                    funcExpr->expression.column);
                continue;
            }

            funcInfo = {
                .memberName = funcExpr->func_key.TokenLiteral,
                .type = exprSym->symbolDataType};

            behaviorMembers[funcExpr->func_key.TokenLiteral] = funcInfo;

            std::cout << "[SEMANTIC LOG] Added function expression '"
                      << funcExpr->func_key.TokenLiteral
                      << "' to behavior '" << behaviorName << "'\n";
        }

        // Case B: function declaration
        else if (auto funcDeclExpr = dynamic_cast<FunctionDeclarationExpression *>(funcStmt->funcExpr.get()))
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

            SymbolInfo *declSym = resolveSymbolInfo(funcDecl->function_name->expression.TokenLiteral);
            if (!declSym)
            {
                logSemanticErrors(
                    "Function declaration '" + funcDecl->function_name->expression.TokenLiteral + "' was not analyzed properly",
                    funcDecl->statement.line,
                    funcDecl->statement.column);
                continue;
            }

            funcInfo = {
                .memberName = funcDecl->function_name->expression.TokenLiteral,
                .type = declSym->symbolDataType};

            behaviorMembers[funcDecl->function_name->expression.TokenLiteral] = funcInfo;

            std::cout << "[SEMANTIC LOG] Added function declaration '"
                      << funcDecl->function_name->expression.TokenLiteral
                      << "' to behavior '" << behaviorName << "'\n";
        }
    }

    // Build symbol info
    SymbolInfo sym = {
        .symbolDataType = DataType::BEHAVIORBLOCK,
        .members = behaviorMembers};

    // Build custom type info
    CustomTypeInfo typeInfo = {
        .typeName = behaviorName,
        .kind = DataType::BEHAVIORBLOCK,
        .members = behaviorMembers};

    // Store results
    symbolTable[0][behaviorName] = sym;
    metaData[behaviorStmt] = sym;
    customTypesTable[behaviorName] = typeInfo;

    // Pop scope
    symbolTable.pop_back();
}

void Semantics::walkUseStatement(Node *node)
{
    auto useStmt = dynamic_cast<UseStatement *>(node);
    if (!useStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing use statement " << node->toString() << "\n";

    // We'll produce a SymbolInfo to stash on this node via metaData.
    // That SymbolInfo will carry the imported members (all or selected).
    SymbolInfo resultSym{};
    resultSym.isNullable = false;
    resultSym.isMutable = false;
    resultSym.isConstant = false;
    resultSym.isInitialized = false;

    // 1) Determine "kind" requested by the 'use' keyword (data/behavior)
    TokenType requestedKind = useStmt->kind_token.type;

    // 2) Inspect the expression after 'use ...'
    //    Cases:
    //      A) Identifier           -> full import of that block
    //      B) InfixExpression '.'  -> behavior.member (selective import)
    //      C) InfixExpression '::' -> data.member     (selective import)
    auto expr = useStmt->blockNameOrCall.get(); // The node we want to import from

    // Helper lambdas
    // This helps to get search for the type we are importing from in the customTypesTable
    auto findType = [&](const std::string &name) -> CustomTypeInfo *
    {
        auto it = customTypesTable.find(name);
        if (it == customTypesTable.end())
            return nullptr;
        return &it->second;
    };

    auto rejectKind = [&](const std::string &name, DataType kind, const char *where, int line, int col)
    {
        std::string got = dataTypetoString(kind);
        std::string want = (requestedKind == TokenType::DATA) ? "DATABLOCK" : "BEHAVIORBLOCK";
        logSemanticErrors("Cannot use '" + name + "' here: expected " + want + " but got " + got, line, col);
    };

    // Case A: simple identifier
    if (auto ident = dynamic_cast<Identifier *>(expr))
    {
        std::string targetName = ident->identifier.TokenLiteral; // The name we want to import from
        CustomTypeInfo *target = findType(targetName);           // Calling our lambda to search for that name in the customTypesTable
        if (!target)
        {
            logSemanticErrors("Unknown block '" + targetName + "' in use statement",
                              ident->expression.line, ident->expression.column);
            return;
        }
        // If we imported from a data block
        if (requestedKind == TokenType::DATA)
        {
            if (target->kind != DataType::DATABLOCK)
            {
                rejectKind(targetName, target->kind, "use data", ident->expression.line, ident->expression.column);
                return;
            }
            resultSym.symbolDataType = DataType::DATABLOCK;
            resultSym.members = target->members; // full import of all data members
        }
        // If we imported from a behavior block
        else if (requestedKind == TokenType::BEHAVIOR)
        {
            if (target->kind != DataType::BEHAVIORBLOCK)
            {
                rejectKind(targetName, target->kind, "use behavior", ident->expression.line, ident->expression.column);
                return;
            }
            resultSym.symbolDataType = DataType::BEHAVIORBLOCK;
            resultSym.members = target->members; // full import of all behavior members
        }
        else
        {
            logSemanticErrors("Expected either 'data' or 'behavior' after 'use'",
                              useStmt->statement.line, useStmt->statement.column);
            return;
        }

        // Stash the resolved import on this node
        metaData[useStmt] = resultSym;
        std::cout << "[SEMANTIC LOG] use "
                  << ((requestedKind == TokenType::DATA) ? "data " : "behavior ")
                  << targetName << " imported " << resultSym.members.size() << " member(s)\n";
        return;
    }

    // Dotted or scope-qualified selection(Importing specifics from the blocks)
    if (auto infix = dynamic_cast<InfixExpression *>(expr))
    {
        TokenType op = infix->operat.type;

        // We only accept . for behavior blocks and :: for data blocks
        bool expectBehaviorDot = (requestedKind == TokenType::BEHAVIOR && op == TokenType::FULLSTOP);
        bool expectDataScope = (requestedKind == TokenType::DATA && op == TokenType::SCOPE_OPERATOR);

        if (!expectBehaviorDot && !expectDataScope)
        {
            std::string opStr = (op == TokenType::FULLSTOP) ? "." : (op == TokenType::SCOPE_OPERATOR) ? "::"
                                                                                                      : "<op>";
            logSemanticErrors("Invalid operator '" + opStr + "' for this 'use' context",
                              infix->operat.line, infix->operat.column);
            return;
        }

        // Extract parent and child names (both are identifiers)
        auto leftId = dynamic_cast<Identifier *>(infix->left_operand.get());
        auto rightId = dynamic_cast<Identifier *>(infix->right_operand.get());
        if (!leftId || !rightId)
        {
            logSemanticErrors("Invalid qualified name in use statement",
                              useStmt->statement.line, useStmt->statement.column);
            return;
        }

        std::string parentName = leftId->identifier.TokenLiteral;
        std::string childName = rightId->identifier.TokenLiteral;

        // Resolve parent type
        CustomTypeInfo *parent = findType(parentName); // Check if the parent exists in the custom types
        if (!parent)
        {
            logSemanticErrors("Unknown block '" + parentName + "' in use statement",
                              leftId->expression.line, leftId->expression.column);
            return;
        }

        // Validate kind per operator/context
        if (expectBehaviorDot)
        {
            if (parent->kind != DataType::BEHAVIORBLOCK)
            {
                rejectKind(parentName, parent->kind, "use behavior <name>.<member>",
                           leftId->expression.line, leftId->expression.column);
                return;
            }
            resultSym.symbolDataType = DataType::BEHAVIORBLOCK;
        }
        else // expectDataScope
        {
            if (parent->kind != DataType::DATABLOCK)
            {
                rejectKind(parentName, parent->kind, "use data <name>::<member>",
                           leftId->expression.line, leftId->expression.column);
                return;
            }
            resultSym.symbolDataType = DataType::DATABLOCK;
        }

        // Select a single member
        auto memIt = parent->members.find(childName);
        if (memIt == parent->members.end())
        {
            logSemanticErrors("Type '" + parentName + "' has no member '" + childName + "'",
                              rightId->expression.line, rightId->expression.column);
            return;
        }

        // Keep only the chosen member
        resultSym.members.clear();
        resultSym.members.emplace(childName, memIt->second);

        // Attach to metaData for the component walker to merge later
        metaData[useStmt] = resultSym;

        // I think here I will insert the imported  data directly into the caller component's members
        auto componentMembers = currentTypeStack.back().members;

        componentMembers = resultSym.members;
        if (componentMembers.empty())
        {
            std::cout << "NOTHING WAS INSERTED INTO COMPONENT MEMBERS\n";
        }
        else
        {
            // Here let us investigate if the currentType stack actually has a component
            DataType &currentType = currentTypeStack.back().kind;
            std::cout << "COMPONENT IS OF TYPE  " << dataTypetoString(currentType) << "\n";
            std::cout << "COMPONENT MEMBERS ARE NOT EMPTY\n";
        }

        std::cout << "[SEMANTIC LOG] use "
                  << ((requestedKind == TokenType::DATA) ? "data " : "behavior ")
                  << parentName
                  << ((op == TokenType::FULLSTOP) ? "." : "::")
                  << childName << " imported 1 member\n";
        return;
    }

    // If we get here, we got a shape we don't support (e.g., call expression)
    logSemanticErrors("Invalid use target", useStmt->statement.line, useStmt->statement.column);
}

void Semantics::walkInitConstructor(Node *node)
{
    auto initStmt = dynamic_cast<InitStatement *>(node);
    if (!initStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing init constructor\n";

    // Must be inside a component

    if (currentTypeStack.empty() || currentTypeStack.back().kind != DataType::COMPONENT)
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
    SymbolInfo initInfo;
    initInfo.symbolDataType = DataType::VOID;
    initInfo.returnType = DataType::VOID;
    initInfo.isDefined = true;

    // Process constructor parameters
    for (const auto &arg : initStmt->constructor_args)
    {
        walkFunctionParameterLetStatement(arg.get());
    }

    // Walk the constructor body
    if (initStmt->block)
    {
        walkBlockStatement(initStmt->block.get());
    }

    // Attach metadata
    metaData[node] = initInfo;

    std::cout << "[SEMANTIC LOG] Init constructor added to component '"
              << currentComponent.typeName << "'\n";
}

void Semantics::walkFieldAccessExpression(Node *node)
{
    auto fieldExpr = dynamic_cast<FieldAccessExpression *>(node);
    if (!fieldExpr)
        return;

    std::cout << "[SEMANTIC LOG] Analysing field access expression: "
              << fieldExpr->toString() << "\n";

    // Walk base (e.g., "self")
    walker(fieldExpr->base.get());

    // Base must be `self`
    auto baseIdent = dynamic_cast<Identifier *>(fieldExpr->base.get());
    if (!baseIdent || baseIdent->identifier.TokenLiteral != "self")
    {
        logSemanticErrors("Field access base must be 'self' (for now)",
                          fieldExpr->expression.line, fieldExpr->expression.column);
        return;
    }

    // Must be inside a component
    if (currentTypeStack.empty() || currentTypeStack.back().kind != DataType::COMPONENT)
    {
        logSemanticErrors("'self' cannot be used outside a component",
                          fieldExpr->expression.line, fieldExpr->expression.column);
        return;
    }

    const std::string &componentName = currentTypeStack.back().typeName;
    auto ctIt = customTypesTable.find(componentName);
    if (ctIt == customTypesTable.end())
    {
        // Shouldn’t happen if walkComponentStatement registered it
        logSemanticErrors("Internal error: unknown component '" + componentName + "'",
                          fieldExpr->expression.line, fieldExpr->expression.column);
        return;
    }

    const std::string fieldName = fieldExpr->field.TokenLiteral;
    const auto &members = ctIt->second.members;
    auto memIt = members.find(fieldName);
    if (memIt == members.end())
    {
        logSemanticErrors("'" + fieldName + "' does not exist in component '" + componentName + "'",
                          fieldExpr->expression.line, fieldExpr->expression.column);
        return;
    }

    const MemberInfo &m = memIt->second;

    // Attach type info for this field access
    metaData[fieldExpr] = {
        .symbolDataType = m.type,
        .isNullable = m.isNullable,
        .isMutable = m.isMutable,
        .isConstant = m.isConstant,
        .isInitialized = m.isInitialised};

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

    // Check if this component name already exists
    if (resolveSymbolInfo(componentName))
    {
        logSemanticErrors(
            "Component name '" + componentName + "' is already defined",
            componentStmt->statement.line,
            componentStmt->statement.column);
        return;
    }

    std::unordered_map<std::string, MemberInfo> members;

    // Enter a new component scope
    symbolTable.push_back({});
    currentTypeStack.push_back({.kind = DataType::COMPONENT,
                                .typeName = componentName,
                                .hasInitConstructor = false,
                                .members = members});

    // Walk data/behavior imports
    for (const auto &usedData : componentStmt->usedDataBlocks)
    {
        if (!usedData)
        {
            std::cout << "INVALID USED DATA NODE\n";
            return;
        }

        auto useStmt = dynamic_cast<UseStatement *>(usedData.get());
        if (!useStmt)
        {
            std::cout << "INVALID SHIT\n";
            continue;
        }

        auto infixExpr = dynamic_cast<InfixExpression *>(useStmt->blockNameOrCall.get());
        if (!infixExpr)
        {
            std::cout << "Unexpected use expression, not infix\n";
            continue;
        }

        auto leftIdent = dynamic_cast<Identifier *>(infixExpr->left_operand.get());
        if (!leftIdent)
        {
            std::cout << "Left side of infix is not identifier\n";
            continue;
        }

        auto dataName = leftIdent->expression.TokenLiteral;

        std::cout << "IMPORTED DATA NAME: " << dataName << "\n";
        auto importedTypeIt = customTypesTable.find(dataName);
        if (importedTypeIt != customTypesTable.end())
        {
            auto &importedMembers = importedTypeIt->second.members;

            auto &currentScope = symbolTable.back();
            for (auto &[name, info] : importedMembers)
            {
                // Add to component members blueprint
                members[name] = info;

                // Also add to local symbol table so assignments resolve
                currentScope[name] = SymbolInfo{
                    .symbolDataType = info.type,
                    .isNullable = info.isNullable,
                    .isMutable = info.isMutable,
                    .isConstant = info.isConstant,
                    .isInitialized = info.isInitialised};
            }
        }
    }

    for (const auto &usedBehavior : componentStmt->usedBehaviorBlocks)
        walkUseStatement(usedBehavior.get());

    // Walk init constructor (if any)
    if (componentStmt->initConstructor.has_value())
        walkInitConstructor(componentStmt->initConstructor.value().get());

    // Walk private data members
    for (const auto &data : componentStmt->privateData)
    {
        auto letStmt = dynamic_cast<LetStatement *>(data.get());
        auto assignStmt = dynamic_cast<AssignmentStatement *>(data.get());
        if (letStmt)
        {
            walker(letStmt);
            auto letSym = resolveSymbolInfo(letStmt->ident_token.TokenLiteral);
            if (!letSym)
            {
                logSemanticErrors(
                    "Failed to resolve private data '" + letStmt->ident_token.TokenLiteral + "'",
                    letStmt->ident_token.line,
                    letStmt->ident_token.column);
                continue;
            }

            members[letStmt->ident_token.TokenLiteral] = {
                .memberName = letStmt->ident_token.TokenLiteral,
                .type = letSym->symbolDataType,
                .isNullable = letSym->isNullable,
                .isMutable = letSym->isMutable,
                .isConstant = letSym->isConstant,
                .isInitialised = letSym->isInitialized};
        }
        if (assignStmt)
        {
            walker(assignStmt);
            auto assignSym = resolveSymbolInfo(assignStmt->identifier->expression.TokenLiteral);
            if (!assignSym)
            {
                logSemanticErrors(
                    "Failed to resolve private data '" + assignStmt->identifier->expression.TokenLiteral + "'",
                    assignStmt->identifier->expression.line,
                    assignStmt->identifier->expression.column);
                continue;
            }

            members[assignStmt->identifier->expression.TokenLiteral] = {
                .memberName = assignStmt->identifier->expression.TokenLiteral,
                .type = assignSym->symbolDataType,
                .isNullable = assignSym->isNullable,
                .isMutable = assignSym->isMutable,
                .isConstant = assignSym->isConstant,
                .isInitialised = assignSym->isInitialized};
        }
    }

    // Walk private methods
    for (const auto &method : componentStmt->privateMethods)
    {
        auto funcStmt = dynamic_cast<FunctionStatement *>(method.get());
        if (!funcStmt)
            continue;

        auto funcExpr = dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get());
        auto funcDeclrExpr = dynamic_cast<FunctionDeclaration *>(funcStmt->funcExpr.get());

        if (funcExpr)
        {
            walker(funcExpr);
            auto metSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
            if (metSym)
            {
                members[funcExpr->func_key.TokenLiteral] = {
                    .memberName = funcExpr->func_key.TokenLiteral,
                    .type = metSym->symbolDataType,
                    .isNullable = metSym->isNullable,
                    .isMutable = metSym->isMutable};
            }
        }
        else if (funcDeclrExpr)
        {
            walker(funcDeclrExpr);
            auto metSym = resolveSymbolInfo(funcDeclrExpr->function_name->expression.TokenLiteral);
            if (metSym)
            {
                members[funcDeclrExpr->function_name->expression.TokenLiteral] = {
                    .memberName = funcDeclrExpr->function_name->expression.TokenLiteral,
                    .type = metSym->symbolDataType,
                    .isNullable = metSym->isNullable,
                    .isMutable = metSym->isMutable};
            }
        }
    }

    // Register component as a symbol
    SymbolInfo componentSymbol = {
        .symbolDataType = DataType::COMPONENT,
    };

    CustomTypeInfo typeInfo = {
        .typeName = componentName,
        .kind = DataType::COMPONENT,
        .members = members};

    metaData[componentStmt] = componentSymbol;
    customTypesTable[componentName] = typeInfo;

    // I want to investigate what is inside the local scope
    if (members.empty())
    {
        std::cout << "COMPONENT MEMBERS ARE EMPTY AT RUNTIME\n";
    }

    // Exit component scope
    currentTypeStack.pop_back();
    symbolTable.pop_back();
}
