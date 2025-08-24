#include "semantics.hpp"
#include <algorithm>

// Walking the data type literals
void Semantics::walkBooleanLiteral(Node *node)
{
    auto boolLiteral = dynamic_cast<BooleanLiteral *>(node);
    if (!boolLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the boolean literal \n";
    metaData[boolLiteral] = {
        .type = ResolvedType{DataType::BOOLEAN, "bool"},
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
        .type = ResolvedType{DataType::STRING, "string"},
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
        .type = ResolvedType{DataType::CHAR, "char"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkChar16Literal(Node *node)
{
    auto lit = dynamic_cast<Char16Literal *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the char16 literal\n";
    metaData[lit] = {
        .type = ResolvedType{DataType::CHAR16, "char16"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkChar32Literal(Node *node)
{
    auto lit = dynamic_cast<Char32Literal *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the char32 literal\n";
    metaData[lit] = {
        .type = ResolvedType{DataType::CHAR16, "char16"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkShortLiteral(Node *node)
{
    auto lit = dynamic_cast<ShortLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the short int literal\n";
    metaData[lit] = {
        .type = ResolvedType{DataType::SHORT_INT, "short"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkUnsignedShortLiteral(Node *node)
{
    auto lit = dynamic_cast<UnsignedShortLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the unsigned short int literal\n";
    metaData[lit] = {
        .type = ResolvedType{DataType::USHORT_INT, "ushort"},
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
        .type = ResolvedType{DataType::INTEGER, "int"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkUnsignedIntegerLiteral(Node *node)
{
    auto intLiteral = dynamic_cast<UnsignedIntegerLiteral *>(node);
    if (!intLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the Unsigned integer literal \n";
    metaData[intLiteral] = {
        .type = ResolvedType{DataType::UINTEGER, "uint"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkLongLiteral(Node *node)
{
    auto lit = dynamic_cast<LongLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the long int literal\n";
    metaData[lit] = {
        .type = ResolvedType{DataType::LONG_INT, "long"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkUnsignedLongLiteral(Node *node)
{
    auto lit = dynamic_cast<UnsignedLongLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the unsigned long int literal\n";
    metaData[lit] = {
        .type = ResolvedType{DataType::ULONG_INT, "ulong"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkExtraLiteral(Node *node)
{
    auto lit = dynamic_cast<ExtraLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the extra (128-bit) int literal\n";
    metaData[lit] = {
        .type = ResolvedType{DataType::EXTRA_INT, "extra"},
        .isNullable = false,
        .isMutable = false,
        .isConstant = false};
}

void Semantics::walkUnsignedExtraLiteral(Node *node)
{
    auto lit = dynamic_cast<UnsignedExtraLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the unsigned extra (128-bit) int literal\n";
    metaData[lit] = {
        .type = {DataType::UEXTRA_INT, "uextra"},
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
        .type = ResolvedType{DataType::FLOAT, "float"},
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
        .type = ResolvedType{DataType::DOUBLE, "double"},
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
        logSemanticErrors(" Use of undeclared identifer '" + identName + "'", identExpr->expression.line, identExpr->expression.column);
        metaData[identExpr] = {
            .type = ResolvedType{DataType::UNKNOWN, "unknown"},
            .isNullable = false,
            .isMutable = false,
            .isConstant = false,
            .isInitialized = false};
        return;
    }
    auto identExprType = symbolInfo->type;
    metaData[identExpr] = {
        .type = identExprType,
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

    auto existing = metaData.find(node);
    if (existing != metaData.end() && existing->second.type.kind == DataType::GENERIC)
    {
        std::cout << "[SEMANTIC LOG]: Skipping already analyzed generic parameter: " << letStmt->ident_token.TokenLiteral << "\n";
        return;
    }

    bool isNullable = letStmt->isNullable;
    bool isInitialized = false;

    auto letStmtValue = letStmt->value.get();

    ResolvedType declaredType = ResolvedType{DataType::UNKNOWN, "unknown"}; // Defaulting to the unknown data type

    if (letStmtValue)
    {
        walker(letStmtValue);
        isInitialized = true;
        auto nullVal = dynamic_cast<NullLiteral *>(letStmtValue);
        // This will be triggered if the value itself is null
        if (nullVal)
        {
            // Will be triggered if the let statement is not nullable but the literal is null
            if (!isNullable)
            {
                logSemanticErrors("Cannot assign 'null' to a non-nullable value '" + letStmt->ident_token.TokenLiteral + "'", letStmt->data_type_token.line, letStmt->data_type_token.column);
                declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};
            }
            else
            {
                declaredType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
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

    // If we dont have a value(A variable declaration)
    if (!letStmtValue)
    {
        ResolvedType expectedType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
        declaredType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
    }

    //  Check for type mismatch if type is explicitly declared (not inferred via auto)
    if (letStmt->data_type_token.type != TokenType::AUTO)
    {
        ResolvedType expectedType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
        if (!isTypeCompatible(expectedType, declaredType))
        {
            logSemanticErrors("Type mismatch in 'let' statement. Expected '" + expectedType.resolvedName + "' but got '" + declaredType.resolvedName + "'", letStmt->data_type_token.line, letStmt->data_type_token.column);
            return;
        }
    }

    std::cout << "LET STATEMENT DATA TYPE: " << declaredType.resolvedName << "\n";

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
        .type = declaredType,
        .isNullable = isNullable,
        .isMutable = isMutable,
        .isConstant = isConstant,
        .isInitialized = isInitialized};
    metaData[letStmt] = symbol;

    // Pushing the let statement to current scope
    symbolTable.back()[letStmt->ident_token.TokenLiteral] = symbol;
}

// walking generic let statements
void Semantics::walkFunctionParameterLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
    {
        logSemanticErrors("Invalid parameter let statement", node->token.line, node->token.column);
        return;
    }
    std::cout << "[SEMANTIC LOG]: Analyzing function parameter let statement " << letStmt->toString() << "\n";

    // Debug mutability
    std::string mutabilityStr = (letStmt->mutability == Mutability::MUTABLE) ? "MUTABLE" : (letStmt->mutability == Mutability::CONSTANT) ? "CONSTANT"
                                                                                                                                         : "IMMUTABLE";
    std::cout << "[SEMANTIC LOG]: Parameter '" << letStmt->ident_token.TokenLiteral
              << "' mutability: " << mutabilityStr << "\n";

    bool isNullable = letStmt->isNullable;
    bool isMutable = false;
    bool isConstant = false;
    if (letStmt->mutability == Mutability::MUTABLE)
    {
        isMutable = true;
    }

    if (letStmt->mutability == Mutability::CONSTANT)
    {
        isConstant = true;
    }

    ResolvedType declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};
    std::string genericName;

    if (letStmt->data_type_token.type == TokenType::IDENTIFIER)
    {
        if (!currentFunction)
        {
            logSemanticErrors("Generic type '" + letStmt->data_type_token.TokenLiteral + "' used outside function scope", letStmt->data_type_token.line, letStmt->data_type_token.column);
            return;
        }
        if (letStmt->value)
        {
            logSemanticErrors("Cannot explicitly assign a value to a generic consider using 'auto' ", letStmt->data_type_token.line, letStmt->data_type_token.column);
            return;
        }
        genericName = letStmt->data_type_token.TokenLiteral;
        if (std::find(currentFunction->genericParams.begin(), currentFunction->genericParams.end(),
                      genericName) == currentFunction->genericParams.end())
        {
            logSemanticErrors("Undefined generic type '" + genericName + "' in function parameter", node->token.line, node->token.column);
            return;
        }
        declaredType = ResolvedType{DataType::GENERIC, "generic"};
    }
    else
    {
        declaredType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
        if (declaredType.kind == DataType::UNKNOWN)
        {
            logSemanticErrors("Invalid parameter type: " + letStmt->data_type_token.TokenLiteral,
                              letStmt->data_type_token.line, letStmt->data_type_token.column);
            return;
        }
    }

    std::cout << "FUNCTION PARAMETER DATA TYPE: " << declaredType.resolvedName << "\n";

    SymbolInfo symbol = {
        .type = declaredType,
        .genericName = genericName,
        .isNullable = isNullable,
        .isMutable = isMutable,
        .isConstant = isConstant,
        .isInitialized = true // Parameters are not initialized
    };

    metaData[letStmt] = symbol;
    symbolTable.back()[letStmt->ident_token.TokenLiteral] = symbol;
    std::cout << "[SEMANTIC LOG] Parameter '" << letStmt->ident_token.TokenLiteral
              << "' stored with type: " << declaredType.resolvedName
              << ", mutable: " << (isMutable ? "true" : "false") << "\n";
}

void Semantics::walkAssignStatement(Node *node)
{
    auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    SymbolInfo *symbol = nullptr;
    std::string assignName;

    // Dealing with self.field
    if (auto fieldAccess = dynamic_cast<FieldAccessExpression *>(assignStmt->identifier.get()))
    {
        auto baseIdent = dynamic_cast<Identifier *>(fieldAccess->base.get());
        if (!baseIdent || baseIdent->identifier.TokenLiteral != "self")
        {
            logSemanticErrors("Left-hand side of assignment must be 'self.<field>' or an identifier",
                              assignStmt->identifier->expression.line,
                              assignStmt->identifier->expression.column);
            return;
        }

        // Ensure we’re actually in a component scope
        if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
        {
            logSemanticErrors("'self' cannot be used outside a component",
                              assignStmt->identifier->expression.line,
                              assignStmt->identifier->expression.column);
            return;
        }

        assignName = fieldAccess->field.TokenLiteral;
        symbol = resolveSymbolInfo(assignName); // normal lookup inside the component’s scope

        if (!symbol)
        {
            logSemanticErrors("Field '" + assignName + "' does not exist in this component",
                              assignStmt->identifier->expression.line,
                              assignStmt->identifier->expression.column);
            return;
        }

        std::cout << "[SEMANTIC LOG]: Analyzing assignment to field 'self."
                  << assignName << "'\n";
    }
    // Plain identifier assignment handling
    else if (auto ident = dynamic_cast<Identifier *>(assignStmt->identifier.get()))
    {
        assignName = ident->identifier.TokenLiteral;
        symbol = resolveSymbolInfo(assignName);

        if (!symbol)
        {
            std::cerr << "[SEMANTIC ERROR]: Variable '" << assignName << "' is not declared\n";
            return;
        }

        std::cout << "[SEMANTIC LOG]: Analyzing assignment to variable '" << assignName << "'\n";
    }
    else
    {
        std::cerr << "[SEMANTIC ERROR]: Invalid assignment target\n";
        return;
    }

    // ---- Type & mutability checks ----
    if (assignStmt->value && assignStmt->value->token.type == TokenType::NULLABLE)
    {
        if (!symbol->isNullable)
        {
            logSemanticErrors("Cannot assign null to non-nullable variable '" + assignName + "'",
                              assignStmt->identifier->expression.line,
                              assignStmt->identifier->expression.column);
            return;
        }
    }
    else
    {
        ResolvedType valueType = inferNodeDataType(assignStmt->value.get());
        if (!isTypeCompatible(symbol->type, valueType))
        {
            logSemanticErrors("Type mismatch: expected '" +
                                  symbol->type.resolvedName + "' but got '" +
                                  valueType.resolvedName + "'",
                              assignStmt->identifier->expression.line,
                              assignStmt->identifier->expression.column);
            return;
        }
    }

    if (symbol->isConstant)
    {
        std::cerr << "[SEMANTIC ERROR]: Cannot reassign to constant variable '" << assignName << "'\n";
        return;
    }

    if (!symbol->isMutable && symbol->isInitialized)
    {
        std::cerr << "[SEMANTIC ERROR]: Cannot reassign to immutable variable '" << assignName << "'\n";
        return;
    }

    // Mark as initialized
    symbol->isInitialized = true;

    // Walk the value expression
    if (assignStmt->value)
        walker(assignStmt->value.get());

    // Store metadata for later stages
    ResolvedType valueType = inferNodeDataType(assignStmt);
    metaData[assignStmt] = {
        .type = valueType,
        .isNullable = symbol->isNullable,
        .isMutable = symbol->isMutable,
        .isConstant = symbol->isConstant,
        .isInitialized = true};
}

void Semantics::walkFieldAssignmentStatement(Node *node)
{
    auto fieldAssignStmt = dynamic_cast<FieldAssignment *>(node);
    if (!fieldAssignStmt)
        return;

    // Let us get the name of the field assignment it is something like ident::ident
    auto fieldName = fieldAssignStmt->assignment_token.TokenLiteral;

    auto line = fieldAssignStmt->statement.line;
    auto column = fieldAssignStmt->statement.column;

    // This is a special case so we are gonna have to resolve the names from the customTypes table
    auto [parentName, childName] = splitScopedName(fieldName); // Splitting the name
    auto parentIt = customTypesTable.find(parentName);
    if (parentIt == customTypesTable.end())
    {
        logSemanticErrors("Type '" + parentName + "' does not exist", line, column);
        return;
    }
    auto members = parentIt->second.members;
    auto memberIt = members.find(childName);
    if (memberIt == members.end())
    {
        logSemanticErrors("Variable '" + childName + "' does not exist in '" + parentName + "'", line, column);
        return;
    }

    // Getting the info I need for metaData construction and some checks
    ResolvedType type = memberIt->second.type;
    bool isNullable = memberIt->second.isNullable;
    bool isMutable = memberIt->second.isMutable;
    bool isConstant = memberIt->second.isConstant;
    bool isInitialized = memberIt->second.isInitialised;

    // Mutability and constant checks
    if (isConstant)
    {
        logSemanticErrors("Cannot reassign to constant variable '" + fieldName + "'", line, column);
        return;
    }

    if (!isMutable && isInitialized)
    {
        logSemanticErrors("Cannot reassign to immutable variable '" + fieldName + "'", line, column);
        return;
    }

    // We have to force initialisation since this is an assignment
    isInitialized = true;

    // Analysing the value if it exists
    if (fieldAssignStmt->value)
    {
        walker(fieldAssignStmt->value.get());
    }

    SymbolInfo info = {
        .type = type,
        .isNullable = isNullable,
        .isMutable = isMutable,
        .isConstant = isConstant,
        .isInitialized = isInitialized};

    // Meta data construction
    metaData[fieldAssignStmt] = info;
}
