#include "semantics_test.hpp"

// Walking the data type literals
void Semantics::walkNullLiteral(Node *node)
{
    auto nullLiteral = dynamic_cast<NullLiteral *>(node);
    if (!nullLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the null literal \n";
    metaData[nullLiteral] = {
        .symbolDataType = DataType::NULLABLE,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

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
    std::cout << "[SEMANTIC LOG]: Analyzing the double literal \n";
    metaData[dbLiteral] = {
        .symbolDataType = DataType::DOUBLE,
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

// Walking the let statement and assign statement
void Semantics::walkLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;

    std::cout << "[SEMANTIC LOG]: Analyzing the let statement\n";

    auto letStmtName = letStmt->ident_token.TokenLiteral;
    auto letStmtLiteral = letStmt->value.get();
    bool isNullable = letStmt->isNullable;

    // Mutability setup
    bool isMutable = false;
    bool isConstant = false;

    switch (letStmt->mutability)
    {
    case Mutability::MUTABLE:
        isMutable = true;
        break;
    case Mutability::CONSTANT:
        isConstant = true;
        break;
    case Mutability::IMMUTABLE:
    default:
        break; // defaults already false
    }

    bool isInitialized = (letStmtLiteral != nullptr);

    if (!isNullable && letStmtLiteral && letStmtLiteral->expression.TokenLiteral == "null")
    {
        std::cerr << "[SEMANTIC ERROR]: Cannot assign null to a non-nullable variable '" << letStmtName << "'\n";
    }

    if (!letStmtLiteral && isConstant == true)
    {
        std::cerr << "[SEMANTIC ERROR]: Constant variable '" << letStmtName << "needs a value assigned to it" << "'\n";
    }

    if (letStmtLiteral)
    {
        walker(letStmtLiteral); // Analyze the value
    }

    DataType declaredType = inferNodeDataType(letStmt);

    SymbolInfo symbol{
        .symbolDataType = declaredType,
        .isNullable = isNullable,
        .isMutable = isMutable,
        .isConstant = isConstant,
        .isInitialized = isInitialized};

    // Attach meta info to node
    metaData[letStmt] = symbol;

    // Register in current scope
    symbolTable.back()[letStmtName] = symbol;
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

    // Null safety check
    if (!symbol->isNullable && assignStmt->value && assignStmt->value->token.type == TokenType::NULLABLE)
    {
        std::cerr << "[SEMANTIC ERROR]: Cannot assign null to non-nullable variable '" << assignName << "'\n";
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
