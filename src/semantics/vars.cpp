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

// Walking array subscript expression
void Semantics::walkArraySubscriptExpression(Node *node)
{
    auto arrExpr = dynamic_cast<ArraySubscript *>(node);
    if (!arrExpr)
        return;

    // Semantic error flag
    bool hasError = false;

    // Getting the variable name
    auto arrName = arrExpr->identifier->expression.TokenLiteral;
    auto line = arrExpr->expression.line;
    auto col = arrExpr->expression.column;

    // Checking if the variable exists
    auto arrSym = resolveSymbolInfo(arrName);
    if (!arrSym)
    {
        logSemanticErrors("Variable '" + arrName + "' does not exist", line, col);
        auto arrError = std::make_shared<SymbolInfo>();
        arrError->type = ResolvedType{DataType::UNKNOWN, "unknown"};
        arrError->hasError = true;
    }

    // If the symbol exists we get for the arrayMeta
    auto arrMeta = arrSym->arrayMeta;
    // We compare the length from the array meta to the index being used (To avoid out of bounds usage)
    auto arrLength = arrMeta.arrLen;
    // Get the index being used
    auto index = arrExpr->index_expr.get();

    // TODO: Will use the position returned from index verifier to fill in this position, will do this if I return to coding on this project(Really exhausted so I am gonna stop here till I return)
    auto pos = SubscriptIndexVerifier(index, arrLength);

    metaData[arrExpr] = arrSym;
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
        return;
    }

    if (symbolInfo->isHeap)
    {
        symbolInfo->lastUseNode = identExpr;
    }

    metaData[identExpr] = symbolInfo;
}

// Walking address expression
void Semantics::walkAddressExpression(Node *node)
{
    auto addrExpr = dynamic_cast<AddressExpression *>(node);
    if (!addrExpr)
        return;

    auto addrName = addrExpr->identifier->expression.TokenLiteral;
    auto line = addrExpr->identifier->expression.line;
    auto col = addrExpr->identifier->expression.column;

    walker(addrExpr->identifier.get());
    auto symbolInfo = resolveSymbolInfo(addrName);
    bool hasError = false;

    if (!symbolInfo)
    {
        logSemanticErrors("Use of undeclared identifier '" + addrName + "'", line, col);
        return;
    }

    if (symbolInfo->isHeap)
        symbolInfo->lastUseNode = addrExpr;

    auto addrInfo = std::make_shared<SymbolInfo>(*symbolInfo);
    addrInfo->isPointer = true;
    addrInfo->type = inferNodeDataType(addrExpr);

    metaData[addrExpr] = addrInfo;
}

void Semantics::walkDereferenceExpression(Node *node)
{
    auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
    if (!derefExpr)
        return;

    auto derefName = derefExpr->identifier->expression.TokenLiteral;
    auto line = derefExpr->identifier->expression.line;
    auto col = derefExpr->identifier->expression.column;

    walker(derefExpr->identifier.get());

    auto derefSym = resolveSymbolInfo(derefName);
    if (!derefSym)
    {
        logSemanticErrors("Use of undeclared identifier '" + derefName + "'", line, col);
        return;
    }

    if (derefSym->isHeap)
        derefSym->lastUseNode = derefExpr;

    auto derefInfo = std::make_shared<SymbolInfo>(*derefSym->targetSymbol);
    derefInfo->isPointer = false; // Just a sanity measure
    derefInfo->isMutable = derefSym->isMutable;
    derefInfo->isConstant = derefSym->isConstant;
    derefInfo->isInitialized = derefSym->isInitialized;
    derefInfo->derefPtrType = derefSym->type;

    std::cout << "DEREF PTR TYPE: " << derefSym->type.resolvedName << "\n";
    std::cout << "DEREF TYPE: " << derefSym->targetSymbol->type.resolvedName << "\n";

    metaData[derefExpr] = derefInfo;
}

// Walking let statement
void Semantics::walkLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;

    std::cout << "[SEMANTIC LOG]: Analyzing let statement node\n";

    // --- Initial flags ---
    bool isNullable = letStmt->isNullable;
    bool isHeap = letStmt->isHeap;
    bool isDefinitelyNull = false;
    bool isInitialized = false;
    bool hasError = false;

    // Checking if the name is being reused
    auto letName = letStmt->ident_token.TokenLiteral;

    auto existingSym = resolveSymbolInfo(letName);
    auto localSym = lookUpInCurrentScope(letName);

    if (localSym)
    {
        logSemanticErrors("Local variable with name '" + letName + "' already exists ", letStmt->ident_token.line, letStmt->ident_token.column);
        hasError = true;
        return;
    }

    if (existingSym)
    {
        if (existingSym->isFunction)
        {
            logSemanticErrors("Local variable '" + letName + "' shadows existing global function", letStmt->ident_token.line, letStmt->ident_token.column);
            hasError = true;
            return;
        }
        else if (existingSym->isComponent)
        {
            logSemanticErrors("Local variable '" + letName + "' shadows existing component", letStmt->ident_token.line, letStmt->ident_token.column);
            hasError = true;
            return;
        }
        else if (existingSym->isBehavior)
        {
            logSemanticErrors("Local variable '" + letName + "' shadows existing  behavior block", letStmt->ident_token.line, letStmt->ident_token.column);
            hasError = true;
            return;
        }
        else if (existingSym->isDataBlock)
        {
            logSemanticErrors("Local variable '" + letName + "' shadows existing data block", letStmt->ident_token.line, letStmt->ident_token.column);
            hasError = true;
            return;
        }
    }

    auto letStmtValue = letStmt->value.get();

    // --- Resolve type from token ---
    ResolvedType expectedType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
    ResolvedType declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};

    // --- Mutability & constants ---
    bool isMutable = (letStmt->mutability == Mutability::MUTABLE);
    bool isConstant = (letStmt->mutability == Mutability::CONSTANT);

    if (isConstant && !letStmtValue)
    {
        logSemanticErrors(
            "Need to assign a value to a constant variable '" + letStmt->ident_token.TokenLiteral + "'",
            letStmt->ident_token.line, letStmt->ident_token.column);
        hasError = true;
    }

    // --- Resolve constant value if present ---
    int64_t constInt = 0;
    if (isConstant && letStmtValue)
    {
        if (auto intLit = dynamic_cast<IntegerLiteral *>(letStmtValue))
        {
            constInt = std::stoll(intLit->int_token.TokenLiteral);
        }
        else if (auto ident = dynamic_cast<Identifier *>(letStmtValue))
        {
            auto identSym = resolveSymbolInfo(ident->identifier.TokenLiteral);
            if (!identSym)
            {
                logSemanticErrors(
                    "Use of undeclared variable '" + ident->identifier.TokenLiteral + "' in constant let statement",
                    ident->expression.line, ident->expression.column);
                hasError = true;
            }
            else if (!identSym->isConstant)
            {
                logSemanticErrors(
                    "Cannot use non-constant variable '" + ident->identifier.TokenLiteral + "' in constant let statement",
                    ident->expression.line, ident->expression.column);
                hasError = true;
            }
            else
            {
                constInt = identSym->constIntVal;
            }
        }
    }

    // --- Walk value & infer type ---
    if (letStmtValue)
    {
        declaredType = inferNodeDataType(letStmtValue);
        walker(letStmtValue);
        isInitialized = true;

        if (auto nullVal = dynamic_cast<NullLiteral *>(letStmtValue))
        {
            isDefinitelyNull = true;
            if (!isNullable)
            {
                logSemanticErrors(
                    "Cannot assign 'null' to non-nullable variable '" + letStmt->ident_token.TokenLiteral + "'",
                    letStmt->data_type_token.line, letStmt->data_type_token.column);
                hasError = true;
                declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};
            }
            else
            {
                declaredType = tokenTypeToResolvedType(letStmt->data_type_token, true);
                declaredType.resolvedName = "null";
            }
        }
        else if (auto ident = dynamic_cast<Identifier *>(letStmtValue))
        {
            auto identSym = resolveSymbolInfo(ident->identifier.TokenLiteral);
            if (!identSym)
            {
                logSemanticErrors(
                    "Cannot assign non-existent identifier '" + ident->identifier.TokenLiteral + "'",
                    ident->expression.line, ident->expression.column);
                hasError = true;
            }
            else if (!identSym->isInitialized)
            {
                logSemanticErrors(
                    "Cannot assign non-initialized identifier '" + ident->identifier.TokenLiteral + "'",
                    ident->expression.line, ident->expression.column);
                hasError = true;
            }
        }
    }
    else
    {
        declaredType = tokenTypeToResolvedType(letStmt->data_type_token, isNullable);
    }

    // --- Type mismatch checks ---
    if (letStmt->data_type_token.type != TokenType::AUTO)
    {
        if (!isTypeCompatible(expectedType, declaredType))
        {
            logSemanticErrors(
                "Type mismatch in 'let' statement. Expected '" + expectedType.resolvedName + "' but got '" +
                    declaredType.resolvedName + "'",
                letStmt->data_type_token.line, letStmt->data_type_token.column);
            hasError = true;
        }

        if (!isNullable && isDefinitelyNull)
        {
            logSemanticErrors(
                "Cannot assign a nullable (possibly null) value to non-nullable variable '" +
                    letStmt->ident_token.TokenLiteral + "'",
                letStmt->data_type_token.line, letStmt->data_type_token.column);
            hasError = true;
            declaredType = ResolvedType{DataType::UNKNOWN, "unknown"};
        }
    }

    // --- Constant + nullable/heap checks ---
    if (isConstant && (isNullable || isDefinitelyNull))
    {
        logSemanticErrors("Cannot use null on a constant variable '" + letStmt->ident_token.TokenLiteral + "'",
                          letStmt->ident_token.line, letStmt->ident_token.column);
        hasError = true;
    }

    if (isHeap)
    {
        if (!isInitialized)
        {
            logSemanticErrors("Cannot promote uninitialized variable '" + letStmt->ident_token.TokenLiteral + "' to the heap",
                              letStmt->ident_token.line, letStmt->ident_token.column);
            hasError = true;
        }

        if (isNullable || isDefinitelyNull)
        {
            logSemanticErrors("Cannot promote nullable variable '" + letStmt->ident_token.TokenLiteral + "' to the heap",
                              letStmt->ident_token.line, letStmt->ident_token.column);
            hasError = true;
        }

        if (letStmt->data_type_token.type == TokenType::AUTO)
        {
            logSemanticErrors(
                "Cannot promote auto variable '" + letStmt->ident_token.TokenLiteral + "' to the heap, please explicitly use its type",
                letStmt->ident_token.line, letStmt->ident_token.column);
            hasError = true;
        }
    }

    // --- Metadata & symbol registration ---
    auto letInfo = std::make_shared<SymbolInfo>();
    letInfo->type = declaredType;
    letInfo->isNullable = isNullable;
    letInfo->isMutable = isMutable;
    letInfo->isConstant = isConstant;
    letInfo->constIntVal = constInt;
    letInfo->isInitialized = isInitialized;
    letInfo->isDefinitelyNull = isDefinitelyNull;
    letInfo->isHeap = isHeap;
    letInfo->lastUseNode = isHeap ? letStmt : nullptr;
    letInfo->hasError = hasError;

    metaData[letStmt] = letInfo;
    symbolTable.back()[letStmt->ident_token.TokenLiteral] = letInfo;

    std::cout << "LET STATEMENT DATA TYPE: " << declaredType.resolvedName << "\n";
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
        std::cout << "SELF assignment has been triggered\n";
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
        auto typeName = compScope.typeName;

        auto compTypeIt = customTypesTable.find(typeName);
        if (compTypeIt == customTypesTable.end())
        {
            logSemanticErrors("Component '" + typeName + "' does not exist",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            hasError = true;
            return;
        }

        auto compInfo = compTypeIt->second;
        auto memIt = compInfo->members.find(assignName);
        if (memIt == compInfo->members.end())
        {
            logSemanticErrors("Field '" + assignName + "' not found in component",
                              selfExpr->expression.line,
                              selfExpr->expression.column);
            hasError = true;
            return;
        }

        std::shared_ptr<MemberInfo> fieldInfo = memIt->second;

        // Wrap field info into a SymbolInfo for semantic tracking
        symbol = std::make_shared<SymbolInfo>();
        symbol->type = fieldInfo->type;
        symbol->isNullable = fieldInfo->isNullable;
        symbol->isMutable = fieldInfo->isMutable;
        symbol->isConstant = fieldInfo->isConstant;
        symbol->isInitialized = fieldInfo->isInitialised;
        symbol->memberIndex = fieldInfo->memberIndex;
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
        walker(ident);
    }
    //---Handle dereference identifiers
    else if (auto derefExpr = dynamic_cast<DereferenceExpression *>(assignStmt->identifier.get()))
    {
        walker(derefExpr);
        assignName = derefExpr->identifier->expression.TokenLiteral;
        auto derefMeta = metaData.find(derefExpr);
        if (derefMeta == metaData.end())
        {
            logSemanticErrors("Failed to resolve dereference metadata for '" + assignName + "'",
                              derefExpr->expression.line,
                              derefExpr->expression.column);
            hasError = true;
            return;
        }

        // THIS is the actual pointee’s symbol info (the target of x)
        symbol = derefMeta->second;

        // Optional sanity check
        if (symbol->isPointer)
        {
            logSemanticErrors("Dereference did not unwrap pointer correctly for '" + assignName + "'",
                              derefExpr->expression.line,
                              derefExpr->expression.column);
            hasError = true;
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
            walker(ident);
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
    }

    // --- Mutability / const checks ---
    if (symbol->isConstant)
    {
        logSemanticErrors("Cannot reassign to constant variable '" + assignName + "'",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
    }

    if (!symbol->isMutable && symbol->isInitialized)
    {
        logSemanticErrors("Cannot reassign to immutable variable '" + assignName + "'",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        hasError = true;
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

    // Special check if the symbol is a reference
    if (symbol->isRef)
    {
        if (auto ident = dynamic_cast<AddressExpression *>(assignStmt->value.get()))
        {
            logSemanticErrors("Cannot reassign an address to a reference '" + assignName + "'",
                              assignStmt->identifier->expression.line,
                              assignStmt->identifier->expression.column);
            hasError = true;
        }
    }

    // If the symbol is a pointer
    if (symbol->isPointer)
    {
        auto addr = dynamic_cast<AddressExpression *>(assignStmt->value.get());
        if (!addr)
        {
            logSemanticErrors("Must only reassign address to pointer '" + assignName + "'", assignStmt->identifier->expression.line,
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

    auto fieldName = fieldAssignStmt->assignment_token.TokenLiteral;
    auto line = fieldAssignStmt->statement.line;
    auto column = fieldAssignStmt->statement.column;
    bool hasError = false;

    // Split into "parent.child"
    auto [parentName, childName] = splitScopedName(fieldName);
    std::cout << "PARENT NAME:" << parentName << "\n";
    std::cout << "CHILD NAME:" << childName << "\n";

    // Resolve the parent symbol (must be a variable in scope)
    auto parentSymbol = resolveSymbolInfo(parentName);
    if (!parentSymbol)
    {
        logSemanticErrors("Variable '" + parentName + "' does not exist", line, column);
        hasError = true;
        return;
    }

    // Get the parent's type
    std::string parentType = parentSymbol->type.resolvedName;
    std::cout << "SEMANTIC LOG: Parent type: " << parentType << "\n";

    // Look up that type in the custom types table
    auto parentIt = customTypesTable.find(parentType);
    if (parentIt == customTypesTable.end())
    {
        logSemanticErrors("Type '" + parentType + "' does not exist", line, column);
        hasError = true;
        return;
    }

    // Check if the childName exists in that type’s members
    auto members = parentIt->second->members;
    auto memberIt = members.find(childName);
    if (memberIt == members.end())
    {
        logSemanticErrors("Variable '" + childName + "' does not exist in type '" + parentType + "'", line, column);
        hasError = true;
        return;
    }

    // Grab field properties
    ResolvedType type = memberIt->second->type;
    bool isNullable = memberIt->second->isNullable;
    bool isMutable = memberIt->second->isMutable;
    bool isConstant = memberIt->second->isConstant;
    bool isHeap = memberIt->second->isHeap;
    bool isInitialized = memberIt->second->isInitialised;

    // Constant/immutability checks
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

    // Mark initialized (since this is assignment)
    isInitialized = true;

    // Analyse the value if present
    if (fieldAssignStmt->value)
    {
        walker(fieldAssignStmt->value.get());
    }

    if (memberIt->second->isHeap)
    {
        memberIt->second->lastUseNode = fieldAssignStmt;
    }

    // Symbol info for the field
    auto fieldInfo = std::make_shared<SymbolInfo>();
    fieldInfo->type = type;
    fieldInfo->isNullable = isNullable;
    fieldInfo->isConstant = isConstant;
    fieldInfo->isInitialized = isInitialized;
    fieldInfo->hasError = hasError;
    fieldInfo->isHeap = isHeap;

    // Build metadata for this assignment node
    auto info = std::make_shared<SymbolInfo>();
    info->type = type; // The whole node still evaluates to the member type
    info->baseSymbol = parentSymbol;
    info->fieldSymbol = fieldInfo;
    info->isNullable = isNullable;
    info->isConstant = isConstant;
    info->isInitialized = isInitialized;
    info->hasError = hasError;

    metaData[fieldAssignStmt] = info;
}

void Semantics::walkReferenceStatement(Node *node)
{
    auto refStmt = dynamic_cast<ReferenceStatement *>(node);
    if (!refStmt)
        return;

    std::cout << "[SEMANTIC LOG]: Analysing reference statement\n";

    auto refName = refStmt->referer->expression.TokenLiteral;
    auto line = refStmt->statement.line;
    auto column = refStmt->statement.column;
    ResolvedType refType = ResolvedType{DataType::UNKNOWN, "unknown"};
    bool hasError = false;

    // Check if the reference variable name already exists
    auto existingSym = resolveSymbolInfo(refName);
    if (existingSym)
    {
        logSemanticErrors("Reference name '" + refName + "' already in use", line, column);
        hasError = true;
    }

    // Check if the reference is pointing to something
    if (!refStmt->referee)
    {
        logSemanticErrors("Reference'" + refName + "' must reference something", line, column);
        hasError = true;
    }

    // Checking the type of the referee
    auto refereeIdent = dynamic_cast<AddressExpression *>(refStmt->referee.get());
    if (!refereeIdent)
    {
        logSemanticErrors("Invalid reference usage '" + refName + "' must only point to an address expression", line, column);
        hasError = true;
        return;
    }
    auto refereeName = refereeIdent->identifier->expression.TokenLiteral;
    auto refereeSymbol = resolveSymbolInfo(refereeName);
    if (!refereeSymbol)
    {
        logSemanticErrors("Reference '" + refName + "'is referencing an undeclared variable '" + refereeName + "'", line, column);
        hasError = true;
        return;
    }

    ResolvedType refereeType = refereeSymbol->type;
    // If the reference statement has no type we just infer the type
    if (!refStmt->type)
    {
        refType = refereeType;
    }
    else if (refStmt->type)
    {
        auto refTypeNode = dynamic_cast<ReturnType *>(refStmt->type.get());
        refType = inferNodeDataType(refTypeNode); // Update the data type with the type that was declared

        if (isNullable(refType))
        {
            logSemanticErrors("Cannot have a nullable reference '" + refName + "'", line, column);
            hasError = true;
        }

        // Compare the two types
        if (refType.kind != refereeType.kind)
        {
            logSemanticErrors("Type mismatch reference '" + refName + "' of type '" + refType.resolvedName + "' does not match variable '" + refereeName + "' being refered with type '" + refereeType.resolvedName + "'", line, column);
            hasError = true;
        }
    }

    // Checking if we are refering to a heap raised variable
    if (!refereeSymbol->isHeap)
    {
        logSemanticErrors("Cannot create a reference '" + refName + "' to a non heap raised variable '" + refereeName + "'", line, column);
        hasError = true;
    }

    // Checking the mutability
    bool isMutable = false;
    bool isConstant = false;
    if (refStmt->mutability == Mutability::MUTABLE)
        isMutable = true;

    if (refStmt->mutability == Mutability::CONSTANT)
        isConstant = true;

    // Check if the symbol is also mutable
    if (!refereeSymbol->isMutable && isMutable)
    {
        logSemanticErrors("Cannot create a mutable reference '" + refName + "' to an immutable variable '" + refereeName + "'", line, column);
        hasError = true;
    }

    if (refereeSymbol->isNullable)
    {
        logSemanticErrors("Cannot create a reference to a nullable variable '" + refereeName + "'", line, column);
        hasError = true;
    }

    // Updating the reference count of the symbol being referenced
    refereeSymbol->refCount += 1;
    std::cout << "[DEBUG] Incremented refCount for target -> "
              << refereeSymbol->refCount << "\n";

    auto refInfo = std::make_shared<SymbolInfo>();
    refInfo->type = refType;
    refInfo->isInitialized = true;
    refInfo->isMutable = isMutable;
    refInfo->isConstant = isConstant;
    refInfo->refereeSymbol = refereeSymbol;
    refInfo->hasError = hasError;
    refInfo->isRef = true;

    metaData[refStmt] = refInfo;
    symbolTable.back()[refName] = refInfo;
}

void Semantics::walkPointerStatement(Node *node)
{
    auto ptrStmt = dynamic_cast<PointerStatement *>(node);

    if (!ptrStmt)
        return;

    auto ptrName = ptrStmt->name->expression.TokenLiteral;
    auto line = ptrStmt->name->expression.line;
    auto col = ptrStmt->name->expression.column;
    bool hasError = false;
    bool isMutable = false;
    bool isConstant = false;
    ResolvedType ptrType = ResolvedType{DataType::UNKNOWN, "unknown"};

    // Dealing with what is being pointed to
    auto ptrValue = dynamic_cast<AddressExpression *>(ptrStmt->value.get());
    if (!ptrValue)
    {
        logSemanticErrors("Must initialize the pointer '" + ptrName + "' with an address like(&<target>)", line, col);
        hasError = true;
        return;
    }
    // Walking the address expression
    walker(ptrValue);

    auto varName = ptrValue->identifier->expression.TokenLiteral;
    auto ptSymbol = resolveSymbolInfo(varName);
    if (!ptSymbol)
    {
        logSemanticErrors("Pointer '" + ptrName + "'is pointing to an undeclared variable '" + varName + "'", line, col);
        hasError = true;
        return;
    }

    // Getting the type being pointed to
    auto ptType = inferNodeDataType(ptrValue);

    // What if the user didnt include the type (We infer for them)
    if (!ptrStmt->type)
        ptrType = ptType;
    else // If the user actually included a type we verify it
    {
        ptrType = inferNodeDataType(ptrStmt);
        // Check if the types are compatible
        // I am comparing with the resolved name since it is sure to either be type_ptr comparing with datatypes purely can allow bugs in the type system
        if (ptrType.resolvedName != ptType.resolvedName)
        {
            logSemanticErrors("Type mismatch pointer '" + ptrName + "' of type '" + ptrType.resolvedName + "' does not match '" + varName + "' of type '" + ptType.resolvedName + "'", line, col);
            hasError = true;
        }
    }

    /*
    Checking for mutability (It should be noted that by default the mutability is... well immutable
    That means if the user didnt add it we are dealing with an immutable pointer
    */

    if (ptrStmt->mutability == Mutability::MUTABLE)
    {
        std::cout << "POINTER IS MUTABLE\n";
        isMutable = true;
    }
    else if (ptrStmt->mutability == Mutability::CONSTANT)
    {
        std::cout << "POINTER IS CONSTANT\n";
        isConstant = true;
    }

    std::cout << "POINTER TYPE: " << ptrType.resolvedName << "\n";

    auto ptrInfo = std::make_shared<SymbolInfo>();
    ptrInfo->isHeap = ptSymbol->isHeap;
    ptrInfo->lastUseNode = ptrStmt;
    ptrInfo->type = ptrType;
    ptrInfo->hasError = hasError;
    ptrInfo->isPointer = true;
    ptrInfo->targetSymbol = ptSymbol;
    ptrInfo->isMutable = isMutable;
    ptrInfo->isConstant = isConstant;
    ptrInfo->isInitialized = true;

    metaData[ptrStmt] = ptrInfo;
    symbolTable.back()[ptrName] = ptrInfo;
}
