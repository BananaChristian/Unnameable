#include "semantics.hpp"

// Walking the data type literals
void Semantics::walkBooleanLiteral(Node *node)
{
    auto boolLiteral = dynamic_cast<BooleanLiteral *>(node);
    if (!boolLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the boolean literal \n";
    metaData[boolLiteral] = {
        .symbolDataType = DataType::BOOLEAN,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkStringLiteral(Node *node)
{
    auto strLiteral = dynamic_cast<StringLiteral *>(node);
    if (!strLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the string literal \n";
    metaData[strLiteral] = {
        .symbolDataType = DataType::STRING,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkCharLiteral(Node *node)
{
    auto charLiteral = dynamic_cast<CharLiteral *>(node);
    if (!charLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the char literal \n";
    metaData[charLiteral] = {
        .symbolDataType = DataType::CHAR,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkIntegerLiteral(Node *node)
{
    auto intLiteral = dynamic_cast<IntegerLiteral *>(node);
    if (!intLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the integer literal \n";
    metaData[intLiteral] = {
        .symbolDataType = DataType::INTEGER,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkFloatLiteral(Node *node)
{
    auto fltLiteral = dynamic_cast<FloatLiteral *>(node);
    if (!fltLiteral)
        return;

    std::cout << "[SEMANTIC LOG]: Analyzing the float literal \n";
    metaData[fltLiteral] = {
        .symbolDataType = DataType::FLOAT,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkDoubleLiteral(Node *node)
{
    auto dbLiteral = dynamic_cast<DoubleLiteral *>(node);
    if (!dbLiteral)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing the double literal \n";
    metaData[dbLiteral] = {
        .symbolDataType = DataType::DOUBLE,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

// Walking the identifier expression
void Semantics::walkIdentifierExpression(Node *node)
{
    auto identExpr = dynamic_cast<Identifier *>(node);
    if (!identExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing identifier node: " << identExpr->toString() << "\n";
    auto identName = identExpr->identifier.TokenLiteral;
    if (identName == "self")
    {
        return;
    }
    auto symbolInfo = resolveSymbolInfo(identName);
    if (!symbolInfo)
    {
        std::cerr << "[SEMANTIC ERROR] Use of undeclared identifer " << identName << "\n";
        metaData[identExpr] = {
            .symbolDataType = DataType::UNKNOWN,
            .isNullable = false,
            .isMutable = false,
            .isConstant = false,
            .isInitialized = false};
        return;
    }
    auto identExprType = symbolInfo->symbolDataType;
    metaData[identExpr] = {
        .symbolDataType = identExprType,
        .isNullable = symbolInfo->isNullable,
        .isMutable = symbolInfo->isMutable,
        .isConstant = symbolInfo->isConstant,
        .isInitialized = symbolInfo->isInitialized};
}

// Walking let statement
void Semantics::walkLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing let statement node\n";

    bool isNullable = letStmt->isNullable;
    bool isInitialized = false;

    auto letStmtValue = letStmt->value.get();

    DataType declaredType = DataType::UNKNOWN; // Defaulting to the normal data type check
    if (letStmtValue)
    {
        walker(letStmtValue);
        isInitialized = true;
        auto nullVal = dynamic_cast<NullLiteral *>(letStmtValue);
        if (nullVal)
        {
            if (!isNullable)
            {
                logSemanticErrors("Cannot assign 'null' to a non-nullable value " + letStmt->ident_token.TokenLiteral, letStmt);
                declaredType = DataType::UNKNOWN;
            }
            else
            {
                switch (letStmt->data_type_token.type)
                {
                case TokenType::INT:
                    declaredType = DataType::NULLABLE_INT;
                    break;
                case TokenType::FLOAT_KEYWORD:
                    declaredType = DataType::NULLABLE_FLT;
                    break;
                case TokenType::DOUBLE_KEYWORD:
                    declaredType = DataType::NULLABLE_DOUBLE;
                    break;
                case TokenType::STRING_KEYWORD:
                    declaredType = DataType::NULLABLE_STR;
                    break;
                case TokenType::CHAR_KEYWORD:
                    declaredType = DataType::NULLABLE_CHAR;
                    break;
                case TokenType::BOOL_KEYWORD:
                    declaredType = DataType::NULLABLE_BOOLEAN;
                    break;
                default:
                    declaredType = DataType::UNKNOWN;
                    break;
                }
            }
        }
        else
        {
            declaredType = inferNodeDataType(letStmtValue);
        }
    }
    else
    {
        declaredType = inferNodeDataType(letStmt);
    }

    //  Check for type mismatch if type is explicitly declared (not inferred via auto)
    if (letStmt->data_type_token.type != TokenType::AUTO)
    {
        DataType expectedType = tokenTypeToDataType(letStmt->data_type_token.type, isNullable);
        if (expectedType != declaredType)
        {
            logSemanticErrors("Type mismatch in 'let' statement. Expected '" + dataTypetoString(expectedType) + "' but got '" + dataTypetoString(declaredType) + "'", letStmt);
            return;
        }
    }

    std::cout << "LET STATEMENT DATA TYPE: " << dataTypetoString(declaredType) << "\n";

    // Checking for mutability and constance
    bool isMutable = false;
    bool isConstant = false;
    if (letStmt->mutability == Mutability::MUTABLE)
    {
        isMutable = true;
    }
    else if (letStmt->mutability == Mutability::CONSTANT)
    {
        isConstant = true;
    }

    // Creating metadata about the let statement node
    SymbolInfo symbol = {
        .symbolDataType = declaredType,
        .isNullable = isNullable,
        .isMutable = isMutable,
        .isConstant = isConstant,
        .isInitialized = isInitialized};
    metaData[letStmt] = symbol;

    // Pushing the let statement to current scope
    symbolTable.back()[letStmt->ident_token.TokenLiteral] = symbol;
}

void Semantics::walkAssignStatement(Node *node)
{
    auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    std::string assignName = assignStmt->ident_token.TokenLiteral;
    std::cout << "[SEMANTIC LOG]: Analyzing assignment to '" << assignName << "'\n";

    SymbolInfo *symbol = resolveSymbolInfo(assignName);
    if (!symbol)
    {
        std::cerr << "[SEMANTIC ERROR]: Variable '" << assignName << "' is not declared\n";
        return;
    }

    // Null safety check — allow assigning null only if symbol is nullable
    if (assignStmt->value && assignStmt->value->token.type == TokenType::NULLABLE)
    {
        if (!symbol->isNullable)
        {
            logSemanticErrors("Cannot assign null to non-nullable variable '" + assignName + "'", assignStmt);
            return;
        }
    }
    else
    {
        // If not null, check type compatibility explicitly
        DataType valueType = inferNodeDataType(assignStmt->value.get());
        if (!isTypeCompatible(symbol->symbolDataType, valueType))
        {
            logSemanticErrors("Type mismatch expected '" + dataTypetoString(symbol->symbolDataType) + "' but got '" + dataTypetoString(valueType) + "'", assignStmt);
            return;
        }
    }

    // Constant check
    if (symbol->isConstant)
    {
        std::cerr << "[SEMANTIC ERROR]: Cannot reassign to constant variable '" << assignName << "'\n";
        return;
    }

    // Immutability check
    if (!symbol->isMutable && symbol->isInitialized)
    {
        std::cerr << "[SEMANTIC ERROR]: Cannot reassign to immutable variable '" << assignName << "'\n";
        return;
    }

    // Passed all checks — mark as initialized now
    symbol->isInitialized = true;

    // Run walker on value
    if (assignStmt->value)
        walker(assignStmt->value.get());

    // Infer type (mostly for metadata/debug)
    DataType valueType = inferNodeDataType(assignStmt);

    metaData[assignStmt] = {
        .symbolDataType = valueType,
        .isNullable = symbol->isNullable,
        .isMutable = symbol->isMutable,
        .isConstant = symbol->isConstant,
        .isInitialized = true};
}
