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
        logSemanticErrors("Already used the name '" + enumStmtName + "' for an existing enum class",
                          enumStmt->statement.line, enumStmt->statement.column);
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
    symbolTable.pop_back();
}

void Semantics::walkDataStatement(Node *node)
{
    auto dataBlockStmt = dynamic_cast<DataStatement *>(node);

    if (!dataBlockStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analysing data statement " + dataBlockStmt->toString() + "\n";
    // Checking is the data blocks name is already take by something else
    const std::string &dataBlockName = dataBlockStmt->dataBlockName->expression.TokenLiteral;

    // Check if the data block name already exists
    auto dataBlockSymbolInfo = resolveSymbolInfo(dataBlockName);
    if (dataBlockSymbolInfo)
    {
        logSemanticErrors("Cannot reuse name '" + dataBlockName + "' for data block", node->token.line, node->token.column);
        return;
    }

    // Dealing with the information inside the data block first we push a local scope
    symbolTable.emplace_back(); // Pushing a local scope
    std::vector<SymbolInfo> fieldSymbols;
    bool Constant = false;
    bool Mutable = false;
    auto &dataBlockFields = dataBlockStmt->fields;
    for (const auto &field : dataBlockFields)
    {
        auto letStmt = dynamic_cast<LetStatement *>(field.get());
        walker(field.get());
        if (letStmt)
        {
            if (letStmt->mutability == Mutability::MUTABLE)
            {
                Mutable = true;
            }
            else if (letStmt->mutability == Mutability::CONSTANT)
            {
                Constant = false;
            }

            fieldSymbols.push_back(
                {.symbolDataType = inferNodeDataType(letStmt),
                 .isNullable = letStmt->isNullable,
                 .isMutable = Mutable,
                 .isConstant = Constant,
                 .isInitialized = false});
        }
    }
    sharedDataBlocks[dataBlockName] = std::move(fieldSymbols);
    symbolTable.pop_back();
}
