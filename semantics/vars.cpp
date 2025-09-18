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

// Walking the null literal
void Semantics::walkNullLiteral(Node *node)
{
    auto nullLit = dynamic_cast<NullLiteral *>(node);
    if (!nullLit)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing null literal\n";

    auto symbol = std::make_shared<SymbolInfo>();
    symbol->type = ResolvedType{DataType::UNKNOWN, "null"}; // Unknown data type for now
    metaData[nullLit] = symbol;
}

// Walking the array literal
void Semantics::walkArrayLiteral(Node *node)
{
    auto arrLit = dynamic_cast<ArrayLiteral *>(node);
    if (!arrLit)
        return;

    // Positional info for error logging
    auto line = arrLit->expression.line;
    auto col = arrLit->expression.column;

    bool hasError = false;

    // Recursively getting the data type of the contents inside the array and also calling the walkers
    std::vector<ResolvedType> itemTypes;

    ArrayMeta arrMeta; // This is where the array meta is stored
    for (const auto &item : arrLit->array)
    {
        if (auto memArr = dynamic_cast<ArrayLiteral *>(item.get()))
        {
            logSemanticErrors("Arrays not allowed inside arrays only use flat arrays", line, col);
            hasError = true;
            return;
        }
        walker(item.get()); // Calling their walkers
        auto itemType = inferNodeDataType(item.get());
        arrMeta.underLyingType = itemType;
        itemTypes.push_back(itemType);
    }

    arrMeta.arrLen = arrLit->array.size();

    ResolvedType arrType = inferNodeDataType(arrLit);

    // Storing metaData about the array
    auto arrInfo = std::make_shared<SymbolInfo>();

    arrInfo->type = arrType;
    arrInfo->isNullable = false;
    arrInfo->isMutable = false;
    arrInfo->isConstant = false;
    arrInfo->arrayMeta = arrMeta;

    metaData[arrLit] = arrInfo;
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

    bool isNullable = letStmt->isNullable;
    bool isDefinitelyNull = false;
    bool isInitialized = false;
    bool hasError = false;

    auto letStmtValue = letStmt->value.get();

    ResolvedType declaredType = ResolvedType{DataType::UNKNOWN, "unknown"}; // Defaulting to the unknown data type

    if (letStmtValue)
    {
        declaredType = inferNodeDataType(letStmtValue);
        walker(letStmtValue);
        isInitialized = true;

        auto nullVal = dynamic_cast<NullLiteral *>(letStmtValue);
        auto ident = dynamic_cast<Identifier *>(letStmtValue);
        // Check to prevent assignment of null identifiers
        if (ident)
        {
            auto identName = ident->identifier.TokenLiteral;
            auto identSym = resolveSymbolInfo(identName);
            if (!identSym)
            {
                logSemanticErrors("Cannot assign non existant identifier '" + identName + "' to let statement '", ident->expression.line, ident->expression.column);
                hasError = true;
            }

            if (!identSym->isInitialized)
            {
                logSemanticErrors("Cannot assign non initialized identifier '" + identName + "' to let statement '", ident->expression.line, ident->expression.column);
                hasError = true;
            }
        }

        // This will be triggered if the value itself is null
        if (nullVal)
        {
            isDefinitelyNull = true;
            if (!isNullable)
            {
                logSemanticErrors("Cannot assign 'null' to non-nullable variable '" + letStmt->ident_token.TokenLiteral + "'",
                                  letStmt->data_type_token.line, letStmt->data_type_token.column);
                hasError = true;
                declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};
            }
            else
            {
                // Null literal adopts LHS type context
                declaredType = tokenTypeToResolvedType(letStmt->data_type_token, /*isNullable=*/true);
                declaredType.resolvedName = "null";
            }
            isInitialized = true; // consider it initialized to null
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
            hasError = true;
            return;
        }
        // --- Prevent assigning a nullable value to a non-nullable variable ---
        if (!isNullable) // LHS is non-nullable
        {
            bool rhsDefinitelyNull = false;

            if (letStmtValue)
            {
                if (auto nullVal = dynamic_cast<NullLiteral *>(letStmtValue))
                {
                    rhsDefinitelyNull = true; // literal null
                }
                else if (auto ident = dynamic_cast<Identifier *>(letStmtValue))
                {
                    auto identSym = resolveSymbolInfo(ident->identifier.TokenLiteral);
                    if (identSym && identSym->isDefinitelyNull)
                    {
                        rhsDefinitelyNull = true; // identifier that is definitely null
                    }
                }
            }

            if (rhsDefinitelyNull)
            {
                logSemanticErrors(
                    "Cannot assign a nullable (possibly null) value to non-nullable variable '" + letStmt->ident_token.TokenLiteral + "'",
                    letStmt->data_type_token.line, letStmt->data_type_token.column);
                hasError = true;
                declaredType = ResolvedType{DataType::UNKNOWN, "unknown"}; // mark LHS type invalid
            }
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
    letInfo->isDefinitelyNull = isDefinitelyNull;
    letInfo->hasError = hasError;

    metaData[letStmt] = letInfo;

    // Pushing the let statement to current scope
    symbolTable.back()[letStmt->ident_token.TokenLiteral] = letInfo;
}

void Semantics::walkAssignStatement(Node *node)
{
    auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    std::shared_ptr<SymbolInfo> symbol = nullptr;
    std::string assignName;
    bool hasError = false;

    // --- Handle self.field assignments ---
    if (auto *selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
    {
        if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
        {
            logSemanticErrors("'self' cannot be used outside a component",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            hasError = true;
            return;
        }

        assignName = selfExpr->field->expression.TokenLiteral;

        auto &compScope = currentTypeStack.back();
        auto compMetaIt = metaData.find(compScope.node);
        if (compMetaIt == metaData.end())
        {
            logSemanticErrors("Component metadata not found",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            hasError = true;
            return;
        }

        auto compMeta = compMetaIt->second;
        auto memIt = compMeta->members.find(assignName);
        if (memIt == compMeta->members.end())
        {
            logSemanticErrors("Field '" + assignName + "' not found in component",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            hasError = true;
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
        symbol->hasError = hasError;
    }
    // --- Handle plain identifier assignments ---
    else if (auto ident = dynamic_cast<Identifier *>(assignStmt->identifier.get()))
    {
        assignName = ident->identifier.TokenLiteral;
        symbol = resolveSymbolInfo(assignName);

        if (!symbol)
        {
            logSemanticErrors("Variable '" + assignName + "' is not declared",
                              ident->identifier.line,
                              ident->identifier.column);
            hasError = true;
            return;
        }
    }
    else
    {
        logSemanticErrors("Invalid assignment target",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
        return;
    }

    // --- Handle RHS null literal ---
    bool rhsIsNull = false;
    bool isDefinitelyNull = false;
    if (assignStmt->value)
    {
        auto ident = dynamic_cast<Identifier *>(assignStmt->value.get());
        // Check to prevent assignment of null identifiers
        if (ident)
        {
            auto identName = ident->identifier.TokenLiteral;
            auto identSym = resolveSymbolInfo(identName);
            if (!identSym)
            {
                logSemanticErrors("Cannot assign non existant identifier '" + identName + "' to variable '" + assignName + "'", ident->expression.line, ident->expression.column);
                hasError = true;
            }

            if (!identSym->isInitialized)
            {
                logSemanticErrors("Cannot assign non initialized identifier '" + identName + "' to variable '" + assignName + "'", ident->expression.line, ident->expression.column);
                hasError = true;
            }
        }
        if (auto nullVal = dynamic_cast<NullLiteral *>(assignStmt->value.get()))
        {
            rhsIsNull = true;
            isDefinitelyNull = true;

            if (!symbol->isNullable)
            {
                logSemanticErrors("Cannot assign 'null' to non-nullable variable '" + assignName + "'",
                                  assignStmt->identifier->expression.line,
                                  assignStmt->identifier->expression.column);
                hasError = true;
                return;
            }

            // Null adopts LHS type
            symbol->type = symbol->type;
            symbol->isInitialized = true;
            symbol->hasError = hasError;
            symbol->isDefinitelyNull = isDefinitelyNull;
            metaData[assignStmt] = symbol;

            // Nothing else to check
            return;
        }
        else if (auto arrLit = dynamic_cast<ArrayLiteral *>(assignStmt->value.get()))
        {
            // Getting the length of the array literal
            auto arrLitMeta = getArrayMeta(arrLit);
            // Getting the arrayMeta for the symbol
            auto arrSymbol = resolveSymbolInfo(assignName);
            if (!arrSymbol)
            {
                logSemanticErrors("Could not find variable '" + assignName + "'", assignStmt->identifier->expression.line,
                                  assignStmt->identifier->expression.column);
                hasError = true;
            }

            auto assignMeta = arrSymbol->arrayMeta;
            // Comapring the lengths
            if (arrLitMeta.arrLen != assignMeta.arrLen)
            {
                logSemanticErrors("Array variable '" + assignName + "' length [" + std::to_string(assignMeta.arrLen) + "] does not match array literal length [" + std::to_string(arrLitMeta.arrLen) + "]", assignStmt->identifier->expression.line, assignStmt->identifier->expression.column);
                hasError = true;
            }
        }
    }

    // --- Infer RHS type if not null ---
    ResolvedType rhsType = inferNodeDataType(assignStmt->value.get());

    if (!isTypeCompatible(symbol->type, rhsType))
    {
        logSemanticErrors("Type mismatch: expected '" +
                              symbol->type.resolvedName + "' but got '" +
                              rhsType.resolvedName + "'",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
        return;
    }

    // --- Mutability / const checks ---
    if (symbol->isConstant)
    {
        logSemanticErrors("Cannot reassign to constant variable '" + assignName + "'",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
        return;
    }

    if (!symbol->isMutable && symbol->isInitialized)
    {
        logSemanticErrors("Cannot reassign to immutable variable '" + assignName + "'",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
        return;
    }

    // --- Prevent assigning a nullable value to a non-nullable variable ---
    if (!symbol->isNullable) // LHS is non-nullable
    {
        bool rhsDefinitelyNull = false;

        if (assignStmt->value)
        {
            if (auto nullVal = dynamic_cast<NullLiteral *>(assignStmt->value.get()))
            {
                rhsDefinitelyNull = true; // literal null
            }
            else if (auto ident = dynamic_cast<Identifier *>(assignStmt->value.get()))
            {
                auto identSym = resolveSymbolInfo(ident->identifier.TokenLiteral);
                if (identSym && identSym->isDefinitelyNull)
                {
                    rhsDefinitelyNull = true; // identifier that is definitely null
                }
            }
        }

        if (rhsDefinitelyNull)
        {
            logSemanticErrors("Cannot assign a null value to to a non-nullable variable '" + assignName + "'",
                              assignStmt->identifier->expression.line,
                              assignStmt->identifier->expression.column);
            hasError = true;
        }
    }

    // --- Mark variable initialized ---
    symbol->isInitialized = true;
    symbol->isDefinitelyNull = isDefinitelyNull;
    symbol->hasError = hasError;

    // --- Walk the RHS expression ---
    if (assignStmt->value)
        walker(assignStmt->value.get());

    // --- Store metadata for later stages ---
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
    bool hasError = false;

    // This is a special case so we are gonna have to resolve the names from the customTypes table
    auto [parentName, childName] = splitScopedName(fieldName); // Splitting the name
    auto parentIt = customTypesTable.find(parentName);
    if (parentIt == customTypesTable.end())
    {
        logSemanticErrors("Type '" + parentName + "' does not exist", line, column);
        hasError = true;
        return;
    }
    auto members = parentIt->second.members;
    auto memberIt = members.find(childName);
    if (memberIt == members.end())
    {
        logSemanticErrors("Variable '" + childName + "' does not exist in '" + parentName + "'", line, column);
        hasError = true;
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
        hasError = true;
        return;
    }

    if (!isMutable && isInitialized)
    {
        logSemanticErrors("Cannot reassign to immutable variable '" + fieldName + "'", line, column);
        hasError = true;
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
    info->hasError = hasError;

    // Meta data construction
    metaData[fieldAssignStmt] = info;
}
