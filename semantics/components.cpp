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

    symbolTable[symbolTable.size() - 2][enumStmtName] = symbol;
    metaData[enumStmt] = symbol;
    customTypesTable[enumStmtName] = enumInfo.enumContent;
    symbolTable.pop_back();
}

void Semantics::walkDataStatement(Node *node)
{
    auto dataBlockStmt = dynamic_cast<DataStatement *>(node);

    if (!dataBlockStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analysing data statement " + dataBlockStmt->toString() + "\n";
    // Checking if the data block name has been taken
    // First we get the data block name
    std::string dataBlockName = dataBlockStmt->dataBlockName->expression.TokenLiteral;
    auto dataSymbol = resolveSymbolInfo(dataBlockName); // Retrieving the symbol
    // Now we check if the symbol already exists
    if (dataSymbol)
    {
        logSemanticErrors("Already used the name '" + dataBlockName + "'", dataBlockStmt->statement.line, dataBlockStmt->statement.column);
        return;
    }
    // Creating the local scope
    symbolTable.push_back({});
    // Retreiving the fields
    bool isBlockMutable = (dataBlockStmt->mutability == Mutability::MUTABLE);
    bool isBlockConstant = (dataBlockStmt->mutability == Mutability::CONSTANT);

    std::vector<std::string> dataBlockFields;

    for (const auto &field : dataBlockStmt->fields)
    {
        // Ensure only let statements are inside data blocks
        auto letStmt = dynamic_cast<LetStatement *>(field.get());
        if (!letStmt)
        {
            logSemanticErrors(
                "Only let statements are allowed inside data block",
                field->statement.line,
                field->statement.column);
            continue;
        }

        // Apply block-level mutability if set
        if (isBlockMutable)
        {
            letStmt->mutability = Mutability::MUTABLE;
        }

        else if (isBlockConstant)
        {
            letStmt->mutability = Mutability::CONSTANT;
        }

        // Otherwise, let the let statement enforce its own default (immutable)

        // Walk the let statement normally
        walkLetStatement(letStmt);
        // Add the let statement to the block members
        dataBlockFields.push_back(letStmt->ident_token.TokenLiteral);
        std::cout << "DATA NAME STORED IN DATA BLOCK FIELD: " << letStmt->ident_token.TokenLiteral << "\n";
    }

    SymbolInfo dataSymbolInfo = {
        .symbolDataType = DataType::DATABLOCK,
        .isMutable = isBlockMutable,
        .isConstant = isBlockConstant,
        .dataBlockName = dataBlockName,
        .dataBlockMembers = dataBlockFields,
    };
    metaData[dataBlockStmt] = dataSymbolInfo; // Storing the metadata
    // Pushing the data block to the global scope
    symbolTable[0][dataBlockName] = dataSymbolInfo;
    // Storing in the custom types table
    customTypesTable[dataBlockName] = dataBlockFields;
    // Exiting the current local scope
    symbolTable.pop_back();
}

void Semantics::walkBehaviorStatement(Node *node)
{
    auto behaviorStmt = dynamic_cast<BehaviorStatement *>(node);
    if (!behaviorStmt)
    {
        return;
    }

    // Getting the behavior blocks name
    std::string behaviorName = behaviorStmt->behaviorBlockName->expression.TokenLiteral;
    // Checking if the name was already used somewhere
    auto behaviorSym = resolveSymbolInfo(behaviorName);
    if (behaviorSym)
    {
        logSemanticErrors("Already used the name '" + behaviorName + "'", behaviorStmt->behaviorBlockName->expression.line, behaviorStmt->behaviorBlockName->expression.column);
        return;
    }
    // Creating a local scope
    symbolTable.push_back({});
    // Dealing with the functions inside the block
    std::vector<std::string> behaviorFuncs;
    for (const auto &func : behaviorStmt->functions)
    {
        // Enforcing only function statements as these nest the expressions, generics, and declarations
        auto funcStmt = dynamic_cast<FunctionStatement *>(func.get());
        if (funcStmt)
        {
            walker(funcStmt); // Leaving the rest to the analyzer
            // Storing the info of the funcs
            auto funcExpr = dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get());
            auto funcDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(funcStmt->funcExpr.get());
            if (funcExpr)
            {
                behaviorFuncs.push_back(funcExpr->func_key.TokenLiteral);
                std::cout << "FUNCTION NAME GETTING STORED IN BEHAVIOR BLOCK FOR FUNC EXPRESSION: " << funcExpr->func_key.TokenLiteral << "\n";
            }
            if (funcDeclrExpr)
            {
                if (auto funcDeclr = dynamic_cast<FunctionDeclaration *>(funcDeclrExpr->funcDeclrStmt.get()))
                {
                    behaviorFuncs.push_back(funcDeclr->function_name->expression.TokenLiteral);
                    std::cout << "FUNCTION NAME GETTING STORED IN BEHAVIOR BLOCK FOR FUNC DECLARATIONS: " << funcDeclr->function_name->expression.TokenLiteral << "\n";
                }
            }
        }
        else
        {
            logSemanticErrors("Expected only function statements inside behavior block ", func->statement.line, func->statement.column);
            return;
        }
    }

    SymbolInfo sym = {
        .symbolDataType = DataType::BEHAVIORBLOCK,
        .behaviorBlockName = behaviorName,
        .behaviorBlockFuncs = behaviorFuncs,
    };

    symbolTable[0][behaviorName] = sym;
    metaData[behaviorStmt] = sym;
    customTypesTable[behaviorName] = behaviorFuncs;
    symbolTable.pop_back();
}

void Semantics::walkUseStatement(Node *node)
{
    auto useStmt = dynamic_cast<UseStatement *>(node);
    if (!useStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analysing use statement " << node->toString() << "\n";
    // Just walk it
    walker(useStmt->blockNameOrCall.get());
}