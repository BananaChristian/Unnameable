#include "semantics.hpp"
#include <algorithm>

// Walking the data type literals
void Semantics::walkBooleanLiteral(Node *node)
{
    auto boolLiteral = dynamic_cast<BooleanLiteral *>(node);
    if (!boolLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the boolean literal \n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::BOOLEAN, "bool"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[boolLiteral] = info;
}

void Semantics::walkStringLiteral(Node *node)
{
    auto strLiteral = dynamic_cast<StringLiteral *>(node);
    if (!strLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the string literal \n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::STRING, "string"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[strLiteral] = info;
}

void Semantics::walkCharLiteral(Node *node)
{
    auto charLiteral = dynamic_cast<CharLiteral *>(node);
    if (!charLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the char literal \n";

    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::CHAR, "char"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[charLiteral] = info;
}

void Semantics::walkChar16Literal(Node *node)
{
    auto lit = dynamic_cast<Char16Literal *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the char16 literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::CHAR16, "char16"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkChar32Literal(Node *node)
{
    auto lit = dynamic_cast<Char32Literal *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the char32 literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::CHAR32, "char32"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkShortLiteral(Node *node)
{
    auto lit = dynamic_cast<ShortLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the short int literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::SHORT_INT, "short"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkUnsignedShortLiteral(Node *node)
{
    auto lit = dynamic_cast<UnsignedShortLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the unsigned short int literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::USHORT_INT, "ushort"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkIntegerLiteral(Node *node)
{
    auto intLiteral = dynamic_cast<IntegerLiteral *>(node);
    if (!intLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the integer literal \n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::INTEGER, "int"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[intLiteral] = info;
}

void Semantics::walkUnsignedIntegerLiteral(Node *node)
{
    auto intLiteral = dynamic_cast<UnsignedIntegerLiteral *>(node);
    if (!intLiteral)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the Unsigned integer literal \n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::UINTEGER, "uint"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[intLiteral] = info;
}

void Semantics::walkLongLiteral(Node *node)
{
    auto lit = dynamic_cast<LongLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the long int literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::LONG_INT, "long"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkUnsignedLongLiteral(Node *node)
{
    auto lit = dynamic_cast<UnsignedLongLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the unsigned long int literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::ULONG_INT, "ulong"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkExtraLiteral(Node *node)
{
    auto lit = dynamic_cast<ExtraLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the extra (128-bit) int literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::EXTRA_INT, "extra"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkUnsignedExtraLiteral(Node *node)
{
    auto lit = dynamic_cast<UnsignedExtraLiteral *>(node);
    if (!lit)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing the unsigned extra (128-bit) int literal\n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::UEXTRA_INT, "uextra"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[lit] = info;
}

void Semantics::walkFloatLiteral(Node *node)
{
    auto fltLiteral = dynamic_cast<FloatLiteral *>(node);
    if (!fltLiteral)
        return;

    std::cout << "[SEMANTIC LOG]: Analyzing the float literal \n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::FLOAT, "float"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[fltLiteral] = info;
}

void Semantics::walkDoubleLiteral(Node *node)
{
    auto dbLiteral = dynamic_cast<DoubleLiteral *>(node);
    if (!dbLiteral)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing the double literal \n";
    auto info = std::make_shared<SymbolInfo>();

    info->type = ResolvedType{DataType::DOUBLE, "double"};
    info->isNullable = false;
    info->isMutable = false;
    info->isConstant = false;
    metaData[dbLiteral] = info;
}

// Walking the identifier expression
void Semantics::walkIdentifierExpression(Node *node)
{
    auto identExpr = dynamic_cast<Identifier *>(node);
    if (!identExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing identifier node: " << identExpr->toString() << "\n";
    auto identName = identExpr->identifier.TokenLiteral;
    auto symbolInfo = resolveSymbolInfo(identName);

    if (!symbolInfo)
    {
        logSemanticErrors(" Use of undeclared identifer '" + identName + "'", identExpr->expression.line, identExpr->expression.column);
        auto errorInfo = std::make_shared<SymbolInfo>();
        errorInfo->type = ResolvedType{DataType::UNKNOWN, "unknown"};
        errorInfo->isNullable = false;
        errorInfo->isMutable = false;
        errorInfo->isConstant = false;
        errorInfo->isInitialized = false;

        metaData[identExpr] = errorInfo;
        return;
    }

    metaData[identExpr] = symbolInfo;
}

// Walking let statement
void Semantics::walkLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing let statement node\n";

    auto existing = metaData.find(node);
    if (existing != metaData.end() && existing->second->type.kind == DataType::GENERIC)
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
        declaredType = inferNodeDataType(letStmtValue);
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
    auto letInfo = std::make_shared<SymbolInfo>();

    letInfo->type = declaredType;
    letInfo->isNullable = isNullable;
    letInfo->isMutable = isMutable;
    letInfo->isConstant = isConstant;
    letInfo->isInitialized = isInitialized;

    metaData[letStmt] = letInfo;

    // Pushing the let statement to current scope
    symbolTable.back()[letStmt->ident_token.TokenLiteral] = letInfo;
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
    std::string typeName;

    if (letStmt->data_type_token.type == TokenType::IDENTIFIER)
    {
        typeName = letStmt->data_type_token.TokenLiteral;

        // First: check if it's a generic (only valid in function scope)
        if (currentFunction &&
            std::find(currentFunction.value()->genericParams.begin(),
                      currentFunction.value()->genericParams.end(),
                      typeName) != currentFunction.value()->genericParams.end())
        {
            if (letStmt->value)
            {
                logSemanticErrors("Cannot explicitly assign a value to a generic; consider using 'auto'",
                                  letStmt->data_type_token.line, letStmt->data_type_token.column);
                return;
            }

            declaredType = ResolvedType{DataType::GENERIC, typeName};
            return;
        }

        // Second: check if it's a custom type
        auto [parentName, childName] = splitScopedName(typeName);
        auto parentIt = customTypesTable.find(parentName);
        std::cout << "[DEBUG] customTypesTable contains:\n";
        for (auto &p : customTypesTable)
        {
            std::cout << "   " << p.first << "\n";
        }

        if (parentIt != customTypesTable.end())
        {
            if (!childName.empty())
            {
                auto members = parentIt->second.members;
                auto memberIt = members.find(childName);
                declaredType = memberIt->second.type;
            }
            declaredType = parentIt->second.type;
        }
        else
        {
            // Neither generic nor custom
            logSemanticErrors("Undefined type '" + typeName + "'",
                              letStmt->data_type_token.line, letStmt->data_type_token.column);
            return;
        }
    }
    else
    {
        // Normal built-in type (int, float, etc.)
        declaredType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
        if (declaredType.kind == DataType::UNKNOWN)
        {
            logSemanticErrors("Invalid parameter type: " + letStmt->data_type_token.TokenLiteral,
                              letStmt->data_type_token.line, letStmt->data_type_token.column);
            return;
        }
    }

    std::cout << "FUNCTION PARAMETER DATA TYPE: " << declaredType.resolvedName << "\n";
    auto info = std::make_shared<SymbolInfo>();
    info->type = declaredType;
    info->genericName = typeName;
    info->isNullable = isNullable;
    info->isMutable = isMutable;
    info->isInitialized = true; // WATCHING OUT FOR THIS

    metaData[letStmt] = info;
    symbolTable.back()[letStmt->ident_token.TokenLiteral] = info;
    std::cout << "[SEMANTIC LOG] Parameter '" << letStmt->ident_token.TokenLiteral
              << "' stored with type: " << declaredType.resolvedName
              << ", mutable: " << (isMutable ? "true" : "false") << "\n";
}

void Semantics::walkAssignStatement(Node *node)
{
    auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    std::shared_ptr<SymbolInfo> symbol = nullptr;
    std::string assignName;

    // Dealing with self.field
    if (auto *selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
    {
        std::cout << "INSIDE SPECIAL SELF ASSIGNMENT\n";
        // Must be inside a component
        if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
        {
            logSemanticErrors("'self' cannot be used outside a component",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            return;
        }

        assignName = selfExpr->field->expression.TokenLiteral;
        std::cout << "NAME BEING RECEIVED : " << assignName << "\n";

        // Look up the current component's metadata
        auto &compScope = currentTypeStack.back();
        auto compMetaIt = metaData.find(compScope.node);
        if (compMetaIt == metaData.end())
        {
            logSemanticErrors("Component metadata not found",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            return;
        }

        auto compMeta = compMetaIt->second;

        // Lookup the field inside the component
        auto memIt = compMeta->members.find(assignName);
        if (memIt == compMeta->members.end())
        {
            logSemanticErrors("Field '" + assignName + "' not found in component",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            return;
        }

        const MemberInfo &fieldInfo = memIt->second;

        // Wrap field info into a SymbolInfo for semantic tracking
        symbol = std::make_shared<SymbolInfo>();
        symbol->type = fieldInfo.type;
        symbol->isNullable = fieldInfo.isNullable;
        symbol->isMutable = fieldInfo.isMutable;
        symbol->isConstant = fieldInfo.isConstant;
        symbol->isInitialized = fieldInfo.isInitialised;
        symbol->memberIndex = fieldInfo.memberIndex;

        std::cout << "[SEMANTIC LOG]: Analyzing assignment to field 'self." << assignName << "'\n";
    }
    // Plain identifier assignment handling
    else if (auto ident = dynamic_cast<Identifier *>(assignStmt->identifier.get()))
    {
        assignName = ident->identifier.TokenLiteral;
        std::cout << "NORMAL ASSIGN NAME: " << assignName << "\n";
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

    metaData[assignStmt] = symbol;
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
    auto info = std::make_shared<SymbolInfo>();
    info->type = type;
    info->isNullable = isNullable;
    info->isConstant = isConstant;
    info->isInitialized = isInitialized;

    // Meta data construction
    metaData[fieldAssignStmt] = info;
}
