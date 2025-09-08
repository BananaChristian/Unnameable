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
    std::unordered_map<std::string, MemberInfo> members;
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
        MemberInfo info{
            .memberName = memberName,
            .type = underLyingType,
            .isConstant = true,
            .isInitialised = true,
            .constantValue = static_cast<int>(memberValue)};
        members[memberName] = info;

        // Add member to local scope
        enumInfo->type = underLyingType;
        enumInfo->isConstant = true;
        enumInfo->isInitialized = true;
        symbolTable.back()[memberName] = enumInfo;

        currentValue = memberValue + 1;
    }

    // Store enum type info
    CustomTypeInfo typeInfo{
        .typeName = enumStmtName,
        .type = ResolvedType{DataType::ENUM, enumStmtName},
        .underLyingType = underLyingType.kind,
        .members = members};
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

    std::unordered_map<std::string, MemberInfo> dataBlockMembers;

    // Analyze each field
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

        // Build member info
        MemberInfo memberInfo = {
            .memberName = letStmt->ident_token.TokenLiteral,
            .type = letSymbol->type,
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
    auto dataSymbolInfo = std::make_shared<SymbolInfo>();
    dataSymbolInfo->type = ResolvedType{DataType::DATABLOCK, dataBlockName};
    dataSymbolInfo->isMutable = isBlockMutable;
    dataSymbolInfo->isConstant = isBlockConstant;
    dataSymbolInfo->members = dataBlockMembers;

    // Build customtype info
    CustomTypeInfo typeInfo = {
        .typeName = dataBlockName,
        .type = ResolvedType{DataType::DATABLOCK, dataBlockName},
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
            auto exprSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
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
                .type = exprSym->type,
                .node = funcStmt};

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

            auto declSym = resolveSymbolInfo(funcDecl->function_name->expression.TokenLiteral);
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
                .type = declSym->type,
                .node = funcStmt};

            behaviorMembers[funcDecl->function_name->expression.TokenLiteral] = funcInfo;

            std::cout << "[SEMANTIC LOG] Added function declaration '"
                      << funcDecl->function_name->expression.TokenLiteral
                      << "' to behavior '" << behaviorName << "'\n";
        }
    }

    // Build symbol info
    auto sym = std::make_shared<SymbolInfo>();
    sym->type = ResolvedType{DataType::BEHAVIORBLOCK, behaviorName};
    sym->members = behaviorMembers;

    // Build custom type info
    CustomTypeInfo typeInfo = {
        .typeName = behaviorName,
        .type = ResolvedType{DataType::BEHAVIORBLOCK, behaviorName},
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

    auto resultSym = std::make_shared<SymbolInfo>();
    // Initializing safe defaults
    resultSym->isNullable = false;
    resultSym->isMutable = false;
    resultSym->isConstant = false;
    resultSym->isInitialized = false;
    resultSym->members.clear();
    resultSym->type = ResolvedType{DataType::UNKNOWN, ""};

    // Determining requested kind (data/behavior)
    TokenType requestedKind = useStmt->kind_token.type;

    // Expression after 'use'
    Node *expr = useStmt->blockNameOrCall.get();
    if (!expr)
    {
        logSemanticErrors("Empty target for use", useStmt->statement.line, useStmt->statement.column);
        return;
    }

    // helper to find custom type by name in customTypesTable
    auto findType = [&](const std::string &name) -> CustomTypeInfo *
    {
        auto it = customTypesTable.find(name);
        if (it == customTypesTable.end())
            return nullptr;
        return &it->second;
    };

    // ---------- Case A: simple identifier -> full import ----------
    if (auto ident = dynamic_cast<Identifier *>(expr))
    {
        std::string targetName = ident->expression.TokenLiteral;
        CustomTypeInfo *target = findType(targetName);
        if (!target)
        {
            logSemanticErrors("Unknown block '" + targetName + "' in use statement",
                              ident->expression.line, ident->expression.column);
            return;
        }

        if (requestedKind == TokenType::DATA)
        {
            if (target->type.kind != DataType::DATABLOCK)
            {
                logSemanticErrors("Cannot use '" + targetName + "' here: expected DATABLOCK",
                                  ident->expression.line, ident->expression.column);
                return;
            }
            resultSym->type.kind = DataType::DATABLOCK;
            resultSym->members = target->members; // copying members
        }
        else if (requestedKind == TokenType::BEHAVIOR)
        {
            if (target->type.kind != DataType::BEHAVIORBLOCK)
            {
                logSemanticErrors("Cannot use '" + targetName + "' here: expected BEHAVIORBLOCK",
                                  ident->expression.line, ident->expression.column);
                return;
            }
            resultSym->type.kind = DataType::BEHAVIORBLOCK;
            resultSym->members = target->members;
        }
        else
        {
            logSemanticErrors("Expected either 'data' or 'behavior' after 'use'",
                              useStmt->statement.line, useStmt->statement.column);
            return;
        }

        // Attaching result to useStmt node
        metaData[useStmt] = resultSym;
        std::cout << "[SEMANTIC LOG] use " << ((requestedKind == TokenType::DATA) ? "data " : "behavior ")
                  << targetName << " imported " << resultSym->members.size() << " member(s)\n";

        // Merging into current component scope if a component is active
        if (!currentTypeStack.empty())
        {
            // merging into the member map
            auto &currentMembers = currentTypeStack.back().members;
            for (auto &kv : resultSym->members)
            {
                currentMembers[kv.first] = kv.second;
                auto memSym = std::make_shared<SymbolInfo>();
                memSym->type = kv.second.type;
                memSym->isNullable = kv.second.isNullable;
                memSym->isMutable = kv.second.isMutable;
                memSym->isConstant = kv.second.isConstant;
                memSym->isInitialized = kv.second.isInitialised;
                symbolTable.back()[kv.first] = memSym;
            }
        }
        else
        {
            std::cout << "[SEMANTIC LOG] No currentTypeStack to merge 'use' members into\n";
        }

        return;
    }

    // ---------- Case B: qualified import (infix) ----------
    if (auto infix = dynamic_cast<InfixExpression *>(expr))
    {
        TokenType op = infix->operat.type;
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

        auto leftId = dynamic_cast<Identifier *>(infix->left_operand.get());
        auto rightId = dynamic_cast<Identifier *>(infix->right_operand.get());
        if (!leftId || !rightId)
        {
            logSemanticErrors("Invalid qualified name in use statement",
                              useStmt->statement.line, useStmt->statement.column);
            return;
        }

        std::string parentName = leftId->expression.TokenLiteral;
        std::string childName = rightId->expression.TokenLiteral;

        CustomTypeInfo *parent = findType(parentName);
        if (!parent)
        {
            logSemanticErrors("Unknown block '" + parentName + "' in use statement",
                              leftId->expression.line, leftId->expression.column);
            return;
        }

        if (expectBehaviorDot)
        {
            if (parent->type.kind != DataType::BEHAVIORBLOCK)
            {
                logSemanticErrors("Cannot use behavior '" + parentName + "': not a behavior block",
                                  leftId->expression.line, leftId->expression.column);
                return;
            }
            resultSym->type.kind = DataType::BEHAVIORBLOCK;
        }
        else
        {
            if (parent->type.kind != DataType::DATABLOCK)
            {
                logSemanticErrors("Cannot use data '" + parentName + "': not a data block",
                                  leftId->expression.line, leftId->expression.column);
                return;
            }
            resultSym->type.kind = DataType::DATABLOCK;
        }

        auto memIt = parent->members.find(childName);
        if (memIt == parent->members.end())
        {
            logSemanticErrors("Type '" + parentName + "' has no member '" + childName + "'",
                              rightId->expression.line, rightId->expression.column);
            return;
        }

        // keep only the selected member
        resultSym->members.clear();
        resultSym->members.emplace(childName, memIt->second);
        metaData[useStmt] = resultSym;

        // merge into the active component type if present
        if (!currentTypeStack.empty())
        {
            auto &currentMembers = currentTypeStack.back().members;
            currentMembers[childName] = memIt->second;

            auto memSym = std::make_shared<SymbolInfo>();
            memSym->type = memIt->second.type;
            memSym->isNullable = memIt->second.isNullable;
            memSym->isMutable = memIt->second.isMutable;
            memSym->isConstant = memIt->second.isConstant;
            memSym->isInitialized = memIt->second.isInitialised;
            symbolTable.back()[childName] = memSym;
        }
        else
        {
            std::cout << "[SEMANTIC LOG] No currentTypeStack to merge single-use member into\n";
        }

        std::cout << "[SEMANTIC LOG] use "
                  << ((requestedKind == TokenType::DATA) ? "data " : "behavior ")
                  << parentName
                  << ((op == TokenType::FULLSTOP) ? "." : "::")
                  << childName << " imported 1 member\n";
        return;
    }

    // Unsupported shape
    logSemanticErrors("Invalid use target", useStmt->statement.line, useStmt->statement.column);
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
    for (const auto &arg : initStmt->constructor_args)
    {
        walkFunctionParameterLetStatement(arg.get());
        // Storing the init args
        initArgs.push_back(inferNodeDataType(arg.get()));
    }
    componentInitArgs[currentComponentName] = initArgs;

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
    if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
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
    auto info = std::make_shared<SymbolInfo>();
    info->type = m.type;
    info->isNullable = m.isNullable;
    info->isMutable = m.isMutable;
    info->isConstant = m.isConstant;
    info->isInitialized = m.isInitialised;

    metaData[fieldExpr] = info;

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

    if (symbolTable[0].find(componentName) != symbolTable[0].end())
    {
        logSemanticErrors("Component '" + componentName + "' already exists", componentStmt->statement.line, componentStmt->statement.column);
        return;
    }

    auto componentSymbol = std::make_shared<SymbolInfo>();
    componentSymbol->type = ResolvedType{DataType::COMPONENT, componentName};
    symbolTable[0][componentName] = componentSymbol;

    std::unordered_map<std::string, MemberInfo> members;

    // Enter a new component scope
    symbolTable.push_back({});
    currentTypeStack.push_back({.type = ResolvedType{DataType::COMPONENT, componentName},
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
                return;
            }
            auto &importedMembers = typeIt->second.members;
            auto &currentScope = symbolTable.back();
            for (auto &[name, info] : importedMembers)
            {
                members[name] = info;
                auto memSym = std::make_shared<SymbolInfo>();
                memSym->type = info.type;
                memSym->isNullable = info.isNullable;
                memSym->isMutable = info.isMutable;
                memSym->isConstant = info.isConstant;
                memSym->isInitialized = info.isInitialised;
                currentScope[name] = memSym;
            }
        }

        // Specific import case
        auto infixExpr = dynamic_cast<InfixExpression *>(useStmt->blockNameOrCall.get());
        if (infixExpr)
        {
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
                    auto memInfo = std::make_shared<SymbolInfo>();
                    memInfo->type = info.type;
                    memInfo->isNullable = info.isNullable;
                    memInfo->isMutable = info.isMutable;
                    memInfo->isConstant = info.isConstant;
                    memInfo->isInitialized = info.isInitialised;

                    currentScope[name] = memInfo;
                }
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
                .type = letSym->type,
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
                .type = assignSym->type,
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
        auto funcDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(funcStmt->funcExpr.get());

        if (funcExpr)
        {
            walker(funcExpr);
            auto metSym = resolveSymbolInfo(funcExpr->func_key.TokenLiteral);
            std::cout << "TRYING TO INSERT: " << funcExpr->func_key.TokenLiteral << "\n";
            if (metSym)
            {
                std::cout << "INSERTING COMPONENT FUNCTION EXPRESSION\n";
                members[funcExpr->func_key.TokenLiteral] = {
                    .memberName = funcExpr->func_key.TokenLiteral,
                    .type = metSym->type,
                    .isNullable = metSym->isNullable,
                    .isMutable = metSym->isMutable,
                    .node = funcStmt};
            }
        }
        else if (funcDeclrExpr)
        {
            auto funcDeclr = dynamic_cast<FunctionDeclaration *>(funcDeclrExpr->funcDeclrStmt.get());
            if (!funcDeclr)
            {
                std::cout << "[SEMANTIC LOG]: Function declaration statement is null\n";
                return;
            }
            walker(funcDeclr);
            auto metSym = resolveSymbolInfo(funcDeclr->function_name->expression.TokenLiteral);
            if (metSym)
            {
                std::cout << "INSERTING COMPONENT FUNCTION DECLARATION EXPRESSION\n";
                members[funcDeclr->function_name->expression.TokenLiteral] = {
                    .memberName = funcDeclr->function_name->expression.TokenLiteral,
                    .type = metSym->type,
                    .isNullable = metSym->isNullable,
                    .isMutable = metSym->isMutable,
                    .node = funcStmt};
            }
        }
    }

    // Register component as a symbol
    componentSymbol->members = members;

    CustomTypeInfo typeInfo = {
        .typeName = componentName,
        .type = ResolvedType{DataType::COMPONENT, componentName},
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

void Semantics::walkNewComponentExpression(Node *node)
{
    auto newExpr = dynamic_cast<NewComponentExpression *>(node);
    if (!newExpr)
        return;
    auto line = newExpr->expression.line;
    auto column = newExpr->expression.column;
    // Getting the component name we shall search in the component types table and not the scope
    auto componentName = newExpr->component_name.TokenLiteral;
    auto componentIt = customTypesTable.find(componentName);
    if (componentIt == customTypesTable.end())
    {
        logSemanticErrors("Component '" + componentName + "' does not exist", line, column);
        return;
    }

    // If the name exists we check the args and see if they match the same arguments in the init constructor of that component
    for (const auto &arg : newExpr->arguments)
    {
        // Getting the type of the argument
        ResolvedType argType = inferNodeDataType(arg.get());
        auto it = componentInitArgs.find(componentName);
        if (it != componentInitArgs.end())
        {
            const auto &expectedArgs = it->second;
            const auto &givenArgs = newExpr->arguments;

            // Checking the argument count
            if (expectedArgs.size() != givenArgs.size())
            {
                logSemanticErrors("Constructor for component '" + componentName +
                                      "' expects " + std::to_string(expectedArgs.size()) +
                                      " arguments but got " + std::to_string(givenArgs.size()),
                                  line, column);
                return;
            }

            // Checking the type check pairwise
            for (size_t i = 0; i < givenArgs.size(); ++i)
            {
                ResolvedType argType = inferNodeDataType(givenArgs[i].get());
                ResolvedType expectedType = expectedArgs[i];

                if (argType.kind != expectedType.kind)
                {
                    {
                        logSemanticErrors("Type mismatch in argument " + std::to_string(i + 1) +
                                              " of constructor for component '" + componentName +
                                              "'. Expected '" + expectedType.resolvedName +
                                              "' but got '" + argType.resolvedName + "'",
                                          line, column);
                    }
                }
            }
        }
    }

    // Storing meta data
    auto info = std::make_shared<SymbolInfo>();
    info->type = componentIt->second.type;
    metaData[newExpr] = info;
}
