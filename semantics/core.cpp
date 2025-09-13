#include "semantics.hpp"
#include "ast.hpp"
#include <algorithm>
#include <unordered_set>

#define CPPREST_FORCE_REBUILD

Semantics::Semantics()
{
    symbolTable.push_back({});
    registerWalkerFunctions();
}

// Main walker function
void Semantics::walker(Node *node)
{
    if (!node)
        return;

    std::cout << "Analyzing AST node: " << node->toString() << "\n";
    std::cout << "Type at runtime: " << typeid(*node).name() << "\n";
    auto walkerIt = walkerFunctionsMap.find(typeid(*node));

    if (walkerIt != walkerFunctionsMap.end())
    {
        (this->*walkerIt->second)(node);
    }
    else
    {
        std::cerr << "[SEMANTIC LOG]: Failed to find analyzer for: " << node->toString() << "\n";
        std::cout << "Actual runtime type: " << typeid(*node).name() << "\n";
    }
}

// HELPER FUNCTIONS
void Semantics::registerWalkerFunctions()
{
    // Walker registration for the native data type literals
    walkerFunctionsMap[typeid(ShortLiteral)] = &Semantics::walkShortLiteral;
    walkerFunctionsMap[typeid(UnsignedShortLiteral)] = &Semantics::walkUnsignedShortLiteral;
    walkerFunctionsMap[typeid(IntegerLiteral)] = &Semantics::walkIntegerLiteral;
    walkerFunctionsMap[typeid(UnsignedIntegerLiteral)] = &Semantics::walkUnsignedIntegerLiteral;
    walkerFunctionsMap[typeid(LongLiteral)] = &Semantics::walkLongLiteral;
    walkerFunctionsMap[typeid(UnsignedLongLiteral)] = &Semantics::walkUnsignedLongLiteral;
    walkerFunctionsMap[typeid(ExtraLiteral)] = &Semantics::walkExtraLiteral;
    walkerFunctionsMap[typeid(UnsignedExtraLiteral)] = &Semantics::walkUnsignedExtraLiteral;
    walkerFunctionsMap[typeid(FloatLiteral)] = &Semantics::walkFloatLiteral;
    walkerFunctionsMap[typeid(DoubleLiteral)] = &Semantics::walkDoubleLiteral;
    walkerFunctionsMap[typeid(StringLiteral)] = &Semantics::walkStringLiteral;

    walkerFunctionsMap[typeid(CharLiteral)] = &Semantics::walkCharLiteral;
    walkerFunctionsMap[typeid(Char16Literal)] = &Semantics::walkChar16Literal;
    walkerFunctionsMap[typeid(Char32Literal)] = &Semantics::walkChar32Literal;

    walkerFunctionsMap[typeid(BooleanLiteral)] = &Semantics::walkBooleanLiteral;
    walkerFunctionsMap[typeid(Identifier)] = &Semantics::walkIdentifierExpression;

    // Walker registration for array walker
    walkerFunctionsMap[typeid(ArrayStatement)] = &Semantics::walkArrayStatement;
    walkerFunctionsMap[typeid(ArrayLiteral)] = &Semantics::walkArrayLiteral;

    // Walker registration for let statement and assignment statements
    walkerFunctionsMap[typeid(LetStatement)] = &Semantics::walkLetStatement;
    walkerFunctionsMap[typeid(AssignmentStatement)] = &Semantics::walkAssignStatement;
    walkerFunctionsMap[typeid(FieldAssignment)] = &Semantics::walkFieldAssignmentStatement;
    walkerFunctionsMap[typeid(EachStatement)] = &Semantics::walkEachStatement;

    // Walker registration for control flow
    walkerFunctionsMap[typeid(ifStatement)] = &Semantics::walkIfStatement;
    walkerFunctionsMap[typeid(SwitchStatement)] = &Semantics::walkSwitchStatement;
    walkerFunctionsMap[typeid(CaseClause)] = &Semantics::walkCaseStatement;

    // Loop disruption statements
    walkerFunctionsMap[typeid(BreakStatement)] = &Semantics::walkBreakStatement;
    walkerFunctionsMap[typeid(ContinueStatement)] = &Semantics::walkContinueStatement;

    // Walker registration for functions
    walkerFunctionsMap[typeid(FunctionStatement)] = &Semantics::walkFunctionStatement;
    walkerFunctionsMap[typeid(FunctionExpression)] = &Semantics::walkFunctionExpression;
    walkerFunctionsMap[typeid(FunctionDeclaration)] = &Semantics::walkFunctionDeclarationStatement;
    walkerFunctionsMap[typeid(FunctionDeclarationExpression)] = &Semantics::walkFunctionDeclarationExpression;
    walkerFunctionsMap[typeid(CallExpression)] = &Semantics::walkFunctionCallExpression;
    walkerFunctionsMap[typeid(ReturnStatement)] = &Semantics::walkReturnStatement;

    // Walker registration for return and error statements
    walkerFunctionsMap[typeid(ErrorStatement)] = &Semantics::walkErrorStatement;
    walkerFunctionsMap[typeid(ErrorExpression)] = &Semantics::walkErrorExpression;

    // Walker registration for loops
    walkerFunctionsMap[typeid(WhileStatement)] = &Semantics::walkWhileStatement;
    walkerFunctionsMap[typeid(ForStatement)] = &Semantics::walkForStatement;

    // Walker registration for blocks
    walkerFunctionsMap[typeid(BlockStatement)] = &Semantics::walkBlockStatement;
    walkerFunctionsMap[typeid(BlockExpression)] = &Semantics::walkBlockExpression;

    // Walker registration for the main expression types
    walkerFunctionsMap[typeid(InfixExpression)] = &Semantics::walkInfixExpression;
    walkerFunctionsMap[typeid(PrefixExpression)] = &Semantics::walkPrefixExpression;
    walkerFunctionsMap[typeid(PostfixExpression)] = &Semantics::walkPostfixExpression;

    walkerFunctionsMap[typeid(ExpressionStatement)] = &Semantics::walkExpressionStatement;

    // Walker registration for the component system
    walkerFunctionsMap[typeid(DataStatement)] = &Semantics::walkDataStatement;
    walkerFunctionsMap[typeid(BehaviorStatement)] = &Semantics::walkBehaviorStatement;
    walkerFunctionsMap[typeid(UseStatement)] = &Semantics::walkUseStatement;
    walkerFunctionsMap[typeid(ComponentStatement)] = &Semantics::walkComponentStatement;
    walkerFunctionsMap[typeid(NewComponentExpression)] = &Semantics::walkNewComponentExpression;
    walkerFunctionsMap[typeid(SelfExpression)] = &Semantics::walkSelfExpression;
    walkerFunctionsMap[typeid(EnumClassStatement)] = &Semantics::walkEnumClassStatement;
}

ResolvedType Semantics::inferNodeDataType(Node *node)
{
    if (!node)
        return ResolvedType{DataType::UNKNOWN, "unknown"};

    if (auto shortLit = dynamic_cast<ShortLiteral *>(node))
        return ResolvedType{DataType::SHORT_INT, "short"};
    if (auto ushortLit = dynamic_cast<UnsignedShortLiteral *>(node))
        return ResolvedType{DataType::USHORT_INT, "ushort"};

    if (auto longLit = dynamic_cast<LongLiteral *>(node))
        return ResolvedType{DataType::LONG_INT, "long"};
    if (auto ulongLit = dynamic_cast<UnsignedLongLiteral *>(node))
        return ResolvedType{DataType::ULONG_INT, "ulong"};

    if (auto extraLit = dynamic_cast<ExtraLiteral *>(node))
        return ResolvedType{DataType::EXTRA_INT, "extra"};
    if (auto uextraLit = dynamic_cast<UnsignedExtraLiteral *>(node))
        return ResolvedType{DataType::UEXTRA_INT, "uextra"};

    if (auto inLit = dynamic_cast<IntegerLiteral *>(node))
        return ResolvedType{DataType::INTEGER, "int"};

    if (auto uintLit = dynamic_cast<UnsignedIntegerLiteral *>(node))
        return ResolvedType{DataType::UINTEGER, "uint"};

    if (auto fltLit = dynamic_cast<FloatLiteral *>(node))
        return ResolvedType{DataType::FLOAT, "float"};

    if (auto dbLit = dynamic_cast<DoubleLiteral *>(node))
        return ResolvedType{DataType::DOUBLE, "double"};

    if (auto strLit = dynamic_cast<StringLiteral *>(node))
        return ResolvedType{DataType::STRING, "string"};

    if (auto chrLit = dynamic_cast<CharLiteral *>(node))
        return ResolvedType{DataType::CHAR, "char"};

    if (auto char16Lit = dynamic_cast<Char16Literal *>(node))
        return ResolvedType{DataType::CHAR16, "char16"};
    if (auto char32Lit = dynamic_cast<Char32Literal *>(node))
        return ResolvedType{DataType::CHAR32, "char32"};

    if (auto boolLit = dynamic_cast<BooleanLiteral *>(node))
        return ResolvedType{DataType::BOOLEAN, "bool"};

    if (auto errExpr = dynamic_cast<ErrorExpression *>(node))
        return ResolvedType{DataType::ERROR, "error"};

    if (auto selfExpr = dynamic_cast<SelfExpression *>(node))
    {
        auto fieldExpr = dynamic_cast<Identifier *>(selfExpr->field.get());
        return inferNodeDataType(fieldExpr);
    }

    // Dealing with the let statement node type
    if (auto letStmt = dynamic_cast<LetStatement *>(node))
    {
        auto letStmtDataToken = letStmt->data_type_token;
        return resolvedDataType(letStmtDataToken, letStmt);
    }

    if (auto assignStmt = dynamic_cast<AssignmentStatement *>(node))
    {
        std::string nameToResolve;

        if (auto selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
        {
            auto fieldIdent = dynamic_cast<Identifier *>(selfExpr->field.get());
            if (!fieldIdent)
                return ResolvedType{DataType::UNKNOWN, "unknown"};

            nameToResolve = fieldIdent->identifier.TokenLiteral; // <-- real field name
        }
        else
        {
            nameToResolve = assignStmt->identifier->expression.TokenLiteral;
        }
        std::cout << "NAME BEING USED TO RESOLVE INSIDE INFERER: " << nameToResolve << "\n";
        auto assignSymbol = resolveSymbolInfo(nameToResolve);
        auto assignStmtVal = assignStmt->value.get();
        ResolvedType assignStmtValType = inferNodeDataType(assignStmtVal);
        if (!isTypeCompatible(assignSymbol->type, assignStmtValType))
        {
            logSemanticErrors("Type mismatch expected '" + assignStmtValType.resolvedName + "' but got '" + assignSymbol->type.resolvedName + "'", assignStmt->identifier->expression.line, assignStmt->identifier->expression.column);
        }
        else
        {
            return assignSymbol->type;
        }
    }

    if (auto newExpr = dynamic_cast<NewComponentExpression *>(node))
    {
        auto componentName = newExpr->component_name.TokenLiteral;
        int line = newExpr->expression.line;
        int column = newExpr->expression.column;
        auto componentIt = customTypesTable.find(componentName);
        if (componentIt == customTypesTable.end())
        {
            logSemanticErrors("Component '" + componentName + "' does not exist", line, column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        return componentIt->second.type;
    }

    if (auto infixExpr = dynamic_cast<InfixExpression *>(node))
    {
        return inferInfixExpressionType(infixExpr);
    }

    if (auto prefixExpr = dynamic_cast<PrefixExpression *>(node))
    {
        return inferPrefixExpressionType(prefixExpr);
    }

    if (auto postfixExpr = dynamic_cast<PostfixExpression *>(node))
    {
        return inferPostfixExpressionType(postfixExpr);
    }

    if (auto ident = dynamic_cast<Identifier *>(node))
    {
        std::string name = ident->identifier.TokenLiteral;
        std::cout << "NAME BEING USED IN IDENTIFIER INFERER: " << name << "\n";

        // Look up the variable in current scope(s)
        auto symbol = resolveSymbolInfo(name);
        if (symbol)
        {
            std::cout << "IDENTIFIER DATA TYPE: " << symbol->type.resolvedName << "\n";
            return symbol->type;
        }
        else
        {
            logSemanticErrors("Undefined variable '" + name + "'", ident->expression.line, ident->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
    }

    if (auto retTypeExpr = dynamic_cast<ReturnTypeExpression *>(node))
    {
        return tokenTypeToResolvedType(retTypeExpr->expression, false);
    }

    if (auto callExpr = dynamic_cast<CallExpression *>(node))
    {
        auto symbol = resolveSymbolInfo(callExpr->function_identifier->expression.TokenLiteral);
        if (symbol)
        {
            return symbol->type;
        }
        else
        {
            logSemanticErrors("Undefined function name '" + callExpr->function_identifier->expression.TokenLiteral + "'", callExpr->function_identifier->expression.line, callExpr->function_identifier->expression.column);
            return {DataType::UNKNOWN, "unknown"};
        }
    }

    return {DataType::UNKNOWN, "unknown"};
}

ResolvedType Semantics::inferInfixExpressionType(Node *node)
{
    auto infixNode = dynamic_cast<InfixExpression *>(node);
    if (!infixNode)
        return ResolvedType{DataType::UNKNOWN, "unknown"};

    std::cout << "[SEMANTIC LOG] INFERING INFIX TYPE\n";
    ResolvedType leftType = inferNodeDataType(infixNode->left_operand.get());
    ResolvedType rightType;
    TokenType operatorType = infixNode->operat.type;
    if (operatorType == TokenType::FULLSTOP || operatorType == TokenType::SCOPE_OPERATOR)
    {
        std::cout << "SCOPE RESOLVING INFIX\n";
        std::string parentName = infixNode->left_operand->expression.TokenLiteral;
        std::string childName = infixNode->right_operand->expression.TokenLiteral;
        return resultOfScopeOrDot(operatorType, parentName, childName, infixNode);
    }
    else
    {
        std::cout << "GETTING RIGHT SIDE TYPE\n";
        rightType = inferNodeDataType(infixNode->right_operand.get());
    }
    return resultOfBinary(operatorType, leftType, rightType);
}

ResolvedType Semantics::inferPrefixExpressionType(Node *node)
{
    auto prefixNode = dynamic_cast<PrefixExpression *>(node);
    if (!prefixNode)
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    std::cout << "[SEMANTIC LOG] Infering prefix type\n";
    auto prefixOperator = prefixNode->operat.type;
    ResolvedType operandType = inferNodeDataType(prefixNode->operand.get());
    return resultOfUnary(prefixOperator, operandType);
}

ResolvedType Semantics::inferPostfixExpressionType(Node *node)
{
    auto postfixNode = dynamic_cast<PostfixExpression *>(node);
    if (!postfixNode)
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    ResolvedType operandType = inferNodeDataType(postfixNode->operand.get());
    auto postfixOperator = postfixNode->operator_token.type;
    return resultOfUnary(postfixOperator, operandType);
}
ResolvedType Semantics::resultOfScopeOrDot(TokenType operatorType, const std::string &parentName, const std::string &childName, InfixExpression *infixExpr)
{
    std::cout << "INSIDE SCOPE RESOLVER FOR INFIX\n";
    /*This function's role is to deal with . or :: operators
    We shall have to check where . and :: are being used . is for behavior and components
    And :: is for data blocks and enum classes*/
    // Okay so let us handle the first case which is .
    if (operatorType == TokenType::FULLSTOP)
    {
        std::cout << "DEALING WITH THE DOT OPERATOR\n";
        // So here we first of all resolve the parent name to see if it exists in the customTypesTable
        auto typeIt = customTypesTable.find(parentName);
        // Checking if the parent name exists
        if (typeIt == customTypesTable.end())
        {
            logSemanticErrors("Parent name '" + parentName + "' does not exist", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        // Adding the check to only use full stop for datablock members only
        if (typeIt->second.type.kind == DataType::DATABLOCK || typeIt->second.type.kind == DataType::ENUM)
        {
            logSemanticErrors("Only use . to access members of behavior blocks and components", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        // If we have the parent name let us look into the child members but we need to ensure the members arent empty
        auto members = typeIt->second.members;
        if (members.empty())
        {
            logSemanticErrors("Type '" + parentName + "' has no members", infixExpr->right_operand->expression.line, infixExpr->right_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        // If it has members let us return the data type of that particular member we encounter
        auto memberIt = members.find(childName);
        // If we dont find the specified member
        if (memberIt == members.end())
        {
            logSemanticErrors("Type '" + parentName + "' does not have member '" + childName + "'", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        // If we have that member we have to return its data Type
        return memberIt->second.type;
    }

    if (operatorType == TokenType::SCOPE_OPERATOR)
    {
        std::cout << "DEALING WITH THE SCOPE OPERATOR\n";
        // So here we first of all resolve the parent name to see if it exists in the customTypesTable
        auto typeIt = customTypesTable.find(parentName);
        // Checking if the parent name exists
        if (typeIt == customTypesTable.end())
        {
            logSemanticErrors("Parent name '" + parentName + "' does not exist", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        // Adding the check to only use full stop for datablock members only
        if (typeIt->second.type.kind == DataType::BEHAVIORBLOCK || typeIt->second.type.kind == DataType::COMPONENT)
        {
            logSemanticErrors("Only use :: to access members of data blocks and enum class members", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        // If we have the parent name let us look into the child members but we need to ensure the members arent empty
        auto members = typeIt->second.members;
        if (members.empty())
        {
            logSemanticErrors("Type '" + parentName + "' has no members", infixExpr->right_operand->expression.line, infixExpr->right_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        // If it has members let us return the data type of that particular member we encounter
        auto memberIt = members.find(childName);
        // If we dont find the specified member
        if (memberIt == members.end())
        {
            logSemanticErrors("Type '" + parentName + "' does not have member '" + childName + "'", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        // If we have that member we have to return its data Type
        return memberIt->second.type;
    }

    return ResolvedType{DataType::UNKNOWN, "unknown"};
}

ResolvedType Semantics::resultOfBinary(TokenType operatorType, ResolvedType leftType, ResolvedType rightType)
{
    // Logical operators: &&, ||
    if (operatorType == TokenType::AND || operatorType == TokenType::OR)
    {
        if (isBoolean(leftType) && isBoolean(rightType))
            return ResolvedType{DataType::BOOLEAN, "boolean"};
        else
            return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    if (operatorType == TokenType::ASSIGN)
    {
        std::cerr << "Cannot use '=' in binary operations; only for assignments\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    // Comparison operators
    bool isComparison = (operatorType == TokenType::GREATER_THAN ||
                         operatorType == TokenType::GT_OR_EQ ||
                         operatorType == TokenType::LESS_THAN ||
                         operatorType == TokenType::LT_OR_EQ ||
                         operatorType == TokenType::EQUALS ||
                         operatorType == TokenType::NOT_EQUALS);

    if (isComparison)
    {
        if (leftType.kind == rightType.kind)
            return ResolvedType{DataType::BOOLEAN, "boolean"};

        std::cerr << "[SEMANTIC ERROR] Cannot compare " << leftType.resolvedName << " and " << rightType.resolvedName << "\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    // String concatenation
    if (operatorType == TokenType::PLUS && isString(leftType) && isString(rightType))
    {
        return ResolvedType{DataType::STRING, "string"};
    }

    // Arithmetic operators: +, -, %, /, *
    bool isArithmetic = (operatorType == TokenType::PLUS ||
                         operatorType == TokenType::MINUS ||
                         operatorType == TokenType::MODULUS ||
                         operatorType == TokenType::DIVIDE ||
                         operatorType == TokenType::ASTERISK);

    if (isArithmetic)
    {
        // Promote mixed int/float combinations
        if ((isInteger(leftType) && isFloat(rightType)) || (isFloat(leftType) && isInteger(rightType)))
        {
            return ResolvedType{DataType::FLOAT, "float"};
        }
        // Promote int/double or float/double to double
        if ((isInteger(leftType) && rightType.kind == DataType::DOUBLE) || (leftType.kind == DataType::DOUBLE && isInteger(rightType)))
        {
            return ResolvedType{DataType::DOUBLE, "double"};
        }
        if ((leftType.kind == DataType::FLOAT && rightType.kind == DataType::DOUBLE) ||
            (leftType.kind == DataType::DOUBLE && rightType.kind == DataType::FLOAT))
        {
            return ResolvedType{DataType::DOUBLE, "double"};
        }

        if (leftType.kind == rightType.kind)
        {
            return leftType;
        }

        std::cerr << "[SEMANTIC ERROR] Type mismatch: " << leftType.resolvedName << " does not match " << rightType.resolvedName << "\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    std::cerr << "[SEMANTIC ERROR] Unknown binary operator: " << TokenTypeToLiteral(operatorType) << " with types "
              << leftType.resolvedName << " and " << rightType.resolvedName << "\n";
    return ResolvedType{DataType::UNKNOWN, "unknown"};
}

ResolvedType Semantics::resultOfUnary(TokenType operatorType, const ResolvedType &operandType)
{
    switch (operatorType)
    {
    case TokenType::BANG:
        if (!isBoolean(operandType))
        {
            std::cerr << "[SEMANTIC ERROR] Cannot apply '!' to type " << operandType.resolvedName << "\n";
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        return ResolvedType{DataType::BOOLEAN, "bool"};

    case TokenType::MINUS:
    case TokenType::PLUS:
    case TokenType::PLUS_PLUS:
    case TokenType::MINUS_MINUS:
        if (isInteger(operandType) || isFloat(operandType))
            return operandType;
        std::cerr << "[SEMANTIC ERROR] Cannot apply " << TokenTypeToLiteral(operatorType) << " to " << operandType.resolvedName << "\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};

    default:
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }
}

std::shared_ptr<SymbolInfo> Semantics::resolveSymbolInfo(const std::string &name)
{
    for (int i = symbolTable.size() - 1; i >= 0; --i)
    {
        auto &scope = symbolTable[i];
        std::cout << "[SEMANTIC LOG] Searching for '" << name << "' in scope level " << i << "\n";
        for (auto &[key, value] : scope)
        {
            std::cout << "    >> Key in scope: '" << key << "'\n";
        }
        if (scope.find(name) != scope.end())
        {
            std::cout << "[SEMANTIC LOG] Found match for '" << name << "'\n";
            return scope[name];
        }
    }
    std::cout << "[SEMANTIC LOG] No match for '" << name << "'\n";
    return nullptr;
}

ResolvedType Semantics::tokenTypeToResolvedType(Token token, bool isNullable)
{
    TokenType type = token.type;
    switch (type)
    {
    case TokenType::SHORT_KEYWORD:
        return {isNullable ? DataType::NULLABLE_SHORT_INT : DataType::SHORT_INT, "short"};
    case TokenType::USHORT_KEYWORD:
        return {isNullable ? DataType::NULLABLE_USHORT_INT : DataType::USHORT_INT, "ushort"};
    case TokenType::INTEGER_KEYWORD:
        return {isNullable ? DataType::NULLABLE_INT : DataType::INTEGER, "int"};
    case TokenType::UINT_KEYWORD:
        return {isNullable ? DataType::NULLABLE_UINT : DataType::UINTEGER, "uint"};
    case TokenType::LONG_KEYWORD:
        return {isNullable ? DataType::NULLABLE_LONG_INT : DataType::LONG_INT, "long"};
    case TokenType::ULONG_KEYWORD:
        return {isNullable ? DataType::NULLABLE_ULONG_INT : DataType::ULONG_INT, "ulong"};
    case TokenType::EXTRA_KEYWORD:
        return {isNullable ? DataType::NULLABLE_EXTRA_INT : DataType::EXTRA_INT, "extra"};
    case TokenType::UEXTRA_KEYWORD:
        return {isNullable ? DataType::NULLABLE_UEXTRA_INT : DataType::UEXTRA_INT, "uextra"};

    case TokenType::FLOAT_KEYWORD:
        return {isNullable ? DataType::NULLABLE_FLT : DataType::FLOAT, "float"};
    case TokenType::DOUBLE_KEYWORD:
        return {isNullable ? DataType::NULLABLE_DOUBLE : DataType::DOUBLE, "double"};
    case TokenType::STRING_KEYWORD:
        return {isNullable ? DataType::NULLABLE_STR : DataType::STRING, "string"};

    case TokenType::CHAR_KEYWORD:
        return {isNullable ? DataType::NULLABLE_CHAR : DataType::CHAR, "char"};
    case TokenType::CHAR16_KEYWORD:
        return {isNullable ? DataType::NULLABLE_CHAR16 : DataType::CHAR16, "char16"};
    case TokenType::CHAR32_KEYWORD:
        return {isNullable ? DataType::NULLABLE_CHAR32 : DataType::CHAR32, "char32"};

    case TokenType::BOOL_KEYWORD:
        return {isNullable ? DataType::NULLABLE_BOOLEAN : DataType::BOOLEAN, "bool"};
    case TokenType::VOID:
        return {DataType::VOID, "void"};
    case TokenType::IDENTIFIER:
    {
        auto [parentName, childName] = splitScopedName(token.TokenLiteral);
        auto parentIt = customTypesTable.find(parentName);
        if (parentIt != customTypesTable.end())
        {
            std::cout << "Found parent name '" << parentName << "'\n";

            // Case: Scoped member type (Parent.Child)
            if (!childName.empty())
            {
                std::cout << "Child name is '" << childName << "'\n";
                auto &members = parentIt->second.members;
                auto memberIt = members.find(childName);
                if (memberIt == members.end())
                {
                    logSemanticErrors("Type '" + childName + "' does not exist in '" + parentName + "'",
                                      token.line, token.column);
                    return ResolvedType{DataType::UNKNOWN, "unknown"};
                }

                auto memberType = memberIt->second.type;
                return ResolvedType{memberType.kind, memberType.resolvedName};
            }

            // Case: Just the parent type
            auto parentType = parentIt->second.type;
            return ResolvedType{parentType.kind, parentType.resolvedName};
        }

        std::cout << "Failed to deal with custom type, resorting to generic.\n";
        return {DataType::GENERIC, "generic"};
    }

    default:
        return {DataType::UNKNOWN, "unknown"};
    }
}

bool Semantics::isTypeCompatible(const ResolvedType &expected, const ResolvedType &actual)
{
    if (actual.kind == DataType::ERROR)
    {
        return true;
    }
    if (expected.kind == actual.kind)
        return true;
    if (expected.kind == DataType::VOID && actual.kind == DataType::UNKNOWN)
    {
        return true;
    }
    if ((expected.kind == DataType::NULLABLE_SHORT_INT && actual.kind == DataType::SHORT_INT) ||
        (expected.kind == DataType::NULLABLE_USHORT_INT && actual.kind == DataType::USHORT_INT) ||
        (expected.kind == DataType::NULLABLE_INT && actual.kind == DataType::INTEGER) ||
        (expected.kind == DataType::NULLABLE_UINT && actual.kind == DataType::UINTEGER) ||
        (expected.kind == DataType::NULLABLE_LONG_INT && actual.kind == DataType::LONG_INT) ||
        (expected.kind == DataType::NULLABLE_ULONG_INT && actual.kind == DataType::ULONG_INT) ||
        (expected.kind == DataType::NULLABLE_EXTRA_INT && actual.kind == DataType::EXTRA_INT) ||
        (expected.kind == DataType::NULLABLE_UEXTRA_INT && actual.kind == DataType::UEXTRA_INT) ||
        (expected.kind == DataType::NULLABLE_FLT && actual.kind == DataType::FLOAT) ||
        (expected.kind == DataType::NULLABLE_DOUBLE && actual.kind == DataType::DOUBLE) ||
        (expected.kind == DataType::NULLABLE_STR && actual.kind == DataType::STRING) ||
        (expected.kind == DataType::NULLABLE_CHAR && actual.kind == DataType::CHAR) ||
        (expected.kind == DataType::NULLABLE_BOOLEAN && actual.kind == DataType::BOOLEAN))
    {
        return true;
    }
    return false;
}

bool Semantics::hasReturnPath(Node *node)
{
    if (currentFunction && currentFunction.value()->returnType.kind == DataType::VOID)
    {
        return true; // Void functions don't need returns
    }

    if (auto blockStmt = dynamic_cast<BlockStatement *>(node))
    {
        for (const auto &stmt : blockStmt->statements)
        {
            if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get()))
            {
                if (retStmt->error_val || retStmt->return_value ||
                    (currentFunction.value()->isNullable && !retStmt->return_value))
                {
                    return true; // Error, value, or null return
                }
            }
            if (auto ifStmt = dynamic_cast<ifStatement *>(stmt.get()))
            {
                auto thenBlock = dynamic_cast<BlockStatement *>(ifStmt->if_result.get());
                bool hasThenReturn = thenBlock && hasReturnPath(thenBlock);
                bool hasElseReturn = ifStmt->else_result.has_value() &&
                                     hasReturnPath(dynamic_cast<BlockStatement *>(
                                         ifStmt->else_result.value().get()));
                if (hasThenReturn && hasElseReturn)
                {
                    return true;
                }
                bool hasElifReturn = true;
                for (const auto &elif : ifStmt->elifClauses)
                {
                    auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
                    if (elifStmt)
                    {
                        auto elifBlock = dynamic_cast<BlockStatement *>(elifStmt->elif_result.get());
                        hasElifReturn &= elifBlock && hasReturnPath(elifBlock);
                    }
                }
                if (hasThenReturn && hasElifReturn && hasElseReturn)
                {
                    return true;
                }
            }
        }
        return false; // BlockStatement has no finalexpr
    }

    if (auto blockExpr = dynamic_cast<BlockExpression *>(node))
    {
        for (const auto &stmt : blockExpr->statements)
        {
            if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get()))
            {
                if (retStmt->error_val || retStmt->return_value ||
                    (currentFunction.value()->isNullable && !retStmt->return_value))
                {
                    return true; // Error, value, or null return
                }
            }
            if (auto ifStmt = dynamic_cast<ifStatement *>(stmt.get()))
            {
                auto thenBlock = dynamic_cast<BlockStatement *>(ifStmt->if_result.get());
                bool hasThenReturn = thenBlock && hasReturnPath(thenBlock);
                bool hasElseReturn = ifStmt->else_result.has_value() &&
                                     hasReturnPath(dynamic_cast<BlockStatement *>(
                                         ifStmt->else_result.value().get()));
                if (hasThenReturn && hasElseReturn)
                {
                    return true;
                }
                bool hasElifReturn = true;
                for (const auto &elif : ifStmt->elifClauses)
                {
                    auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
                    if (elifStmt)
                    {
                        auto elifBlock = dynamic_cast<BlockStatement *>(elifStmt->elif_result.get());
                        hasElifReturn &= elifBlock && hasReturnPath(elifBlock);
                    }
                }
                if (hasThenReturn && hasElifReturn && hasElseReturn)
                {
                    return true;
                }
            }
        }
        if (blockExpr->finalexpr.has_value())
        {
            ResolvedType exprType = inferNodeDataType(blockExpr->finalexpr.value().get());
            return exprType.kind == DataType::ERROR ||
                   isTypeCompatible(currentFunction.value()->returnType, exprType) ||
                   (dynamic_cast<NullLiteral *>(blockExpr->finalexpr.value().get()) &&
                    currentFunction.value()->isNullable);
        }
        return false;
    }

    return false;
}

bool Semantics::areSignaturesCompatible(const SymbolInfo &declInfo, FunctionExpression *funcExpr)
{

    if (declInfo.paramTypes.size() != funcExpr->call.size())
    {
        return false;
    }
    for (size_t i = 0; i < declInfo.paramTypes.size(); ++i)
    {
        auto letStmt = dynamic_cast<LetStatement *>(funcExpr->call[i].get());
        if (!letStmt)
            return false;
        ResolvedType paramType = tokenTypeToResolvedType(letStmt->data_type_token, letStmt->isNullable);
        std::string paramGenericName = letStmt->data_type_token.type == TokenType::IDENTIFIER ? letStmt->data_type_token.TokenLiteral : "";
        // Find declaration's parameter metadata
        bool declParamNullable = false;
        for (const auto &pair : metaData)
        {
            if (auto declLetStmt = dynamic_cast<LetStatement *>(pair.first))
            {
                if (declLetStmt->ident_token.TokenLiteral == letStmt->ident_token.TokenLiteral &&
                    pair.second->type.kind == declInfo.paramTypes[i].first.kind &&
                    pair.second->genericName == declInfo.paramTypes[i].second)
                {
                    declParamNullable = pair.second->isNullable;
                    break;
                }
            }
        }
        if (paramType.kind != declInfo.paramTypes[i].first.kind ||
            paramGenericName != declInfo.paramTypes[i].second ||
            letStmt->isNullable != declParamNullable)
        {
            return false;
        }
    }

    // Check return type
    auto retType = dynamic_cast<ReturnTypeExpression *>(funcExpr->return_type.get());
    if (!retType)
        return false;
    ResolvedType returnType = tokenTypeToResolvedType(retType->expression, funcExpr->isNullable);
    std::string returnGenericName = retType->expression.type == TokenType::IDENTIFIER ? retType->expression.TokenLiteral : "";
    return returnType.kind == declInfo.returnType.kind &&
           funcExpr->isNullable == declInfo.isNullable;
}

bool Semantics::isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr)
{
    // 1. Check parameter count
    if (funcInfo.paramTypes.size() != callExpr->parameters.size())
    {
        logSemanticErrors("Call has " + std::to_string(callExpr->parameters.size()) +
                              " arguments, but function expects " + std::to_string(funcInfo.paramTypes.size()),
                          callExpr->expression.line, callExpr->expression.column);
        return false;
    }

    for (size_t i = 0; i < callExpr->parameters.size(); ++i)
    {
        auto &param = callExpr->parameters[i];
        const auto &expectedType = funcInfo.paramTypes[i]; // pair<ResolvedType, string>
        ResolvedType argType{DataType::UNKNOWN, "unknown"};

        // --- Null literal handling ---
        if (auto nullLit = dynamic_cast<NullLiteral *>(param.get()))
        {
            if (isNullable(expectedType.first))
            {
                argType = expectedType.first; // promote null → nullable type
            }
            else
            {
                logSemanticErrors("Cannot pass null to non-nullable parameter " +
                                      std::to_string(i + 1) + ": expected " + expectedType.first.resolvedName,
                                  param->expression.line, param->expression.column);
                return false;
            }
        }
        else
        {
            argType = inferNodeDataType(param.get());
            if (argType.kind == DataType::UNKNOWN)
            {
                logSemanticErrors("Could not infer type for argument " + std::to_string(i + 1),
                                  param->expression.line, param->expression.column);
                return false;
            }
        }
    }

    return true;
}

Shape Semantics::getArrayShape(Node *node)
{
    auto arrLit = dynamic_cast<ArrayLiteral *>(node);
    // The node we check the shape for must be an arrayLiteral
    if (!arrLit)
    {
        return Shape{0, {}};
    }

    // If the array literal is empty then we have an empty shape
    size_t arrLength = arrLit->array.size();

    if (arrLength == 0)
    {
        return Shape{1, {0}};
    }

    // Getting the first child shape
    Shape firstShape = getArrayShape(arrLit->array[0].get());

    // Getting the child shapes
    for (const auto &item : arrLit->array)
    {
        Shape s = getArrayShape(item.get());
        if (s != firstShape)
        {
            logSemanticErrors("Jagged arrays are not allowed, mismatched shapes", arrLit->expression.line, arrLit->expression.column);
        }
    }

    Shape finalShape;
    finalShape.dimensions = firstShape.dimensions + 1;
    finalShape.lengths = {arrLength};

    finalShape.lengths.insert(finalShape.lengths.end(), firstShape.lengths.begin(), firstShape.lengths.end());
    return finalShape;
}

void Semantics::logSemanticErrors(const std::string &message, int tokenLine, int tokenColumn)
{
    std::cerr << "[SEMANTIC ERROR] " << message << " on line: " << std::to_string(tokenLine) << " and column: " << std::to_string(tokenColumn) << "\n";
}

bool Semantics::isInteger(const ResolvedType &t)
{
    static const std::unordered_set<DataType> intTypes = {
        DataType::SHORT_INT, DataType::USHORT_INT, DataType::INTEGER, DataType::UINTEGER,
        DataType::LONG_INT, DataType::ULONG_INT, DataType::EXTRA_INT, DataType::UEXTRA_INT};
    return intTypes.count(t.kind) > 0;
}

bool Semantics::isNullableInteger(const ResolvedType &t)
{
    static const std::unordered_set<DataType> nullableInts = {
        DataType::NULLABLE_SHORT_INT, DataType::NULLABLE_USHORT_INT,
        DataType::NULLABLE_INT, DataType::NULLABLE_UINT,
        DataType::NULLABLE_LONG_INT, DataType::NULLABLE_ULONG_INT,
        DataType::NULLABLE_EXTRA_INT, DataType::NULLABLE_UEXTRA_INT};
    return nullableInts.count(t.kind) > 0;
}

bool Semantics::isFloat(const ResolvedType &t)
{
    return t.kind == DataType::FLOAT || t.kind == DataType::DOUBLE;
}

bool Semantics::isNullableFloat(const ResolvedType &t)
{
    return t.kind == DataType::NULLABLE_FLT || t.kind == DataType::NULLABLE_DOUBLE;
}

bool Semantics::isBoolean(const ResolvedType &t)
{
    return t.kind == DataType::BOOLEAN ||
           t.kind == DataType::NULLABLE_BOOLEAN;
}

bool Semantics::isString(const ResolvedType &t)
{
    return t.kind == DataType::STRING || t.kind == DataType::NULLABLE_STR;
}

bool Semantics::isChar(const ResolvedType &t)
{
    static const std::unordered_set<DataType> charTypes = {
        DataType::CHAR, DataType::NULLABLE_CHAR,
        DataType::CHAR16, DataType::NULLABLE_CHAR16,
        DataType::CHAR32, DataType::NULLABLE_CHAR32};
    return charTypes.count(t.kind) > 0;
}

bool Semantics::isNullable(const ResolvedType &t)
{
    switch (t.kind)
    {
    case DataType::NULLABLE_SHORT_INT:
    case DataType::NULLABLE_USHORT_INT:
    case DataType::NULLABLE_INT:
    case DataType::NULLABLE_UINT:
    case DataType::NULLABLE_LONG_INT:
    case DataType::NULLABLE_ULONG_INT:
    case DataType::NULLABLE_EXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
    case DataType::NULLABLE_BOOLEAN:
    case DataType::NULLABLE_STR:
    case DataType::NULLABLE_FLT:
    case DataType::NULLABLE_DOUBLE:
    case DataType::NULLABLE_CHAR:
    case DataType::NULLABLE_CHAR16:
    case DataType::NULLABLE_CHAR32:
        return true;
    default:
        return false;
    }
}

ResolvedType Semantics::resolvedDataType(Token token, Node *node)
{
    std::cout << "INSIDE TYPE RESOLVER\n";
    TokenType type = token.type;

    switch (type)
    {
    case TokenType::SHORT_KEYWORD:
        return ResolvedType{DataType::SHORT_INT, "short"};

    case TokenType::USHORT_KEYWORD:
        return ResolvedType{DataType::USHORT_INT, "ushort"};

    case TokenType::INTEGER_KEYWORD:
        return ResolvedType{DataType::INTEGER, "int"};

    case TokenType::UINT_KEYWORD:
        return ResolvedType{DataType::UINTEGER, "uint"};

    case TokenType::LONG_KEYWORD:
        return ResolvedType{DataType::LONG_INT, "long"};

    case TokenType::ULONG_KEYWORD:
        return ResolvedType{DataType::ULONG_INT, "ulong"};

    case TokenType::EXTRA_KEYWORD:
        return ResolvedType{DataType::EXTRA_INT, "extra"};

    case TokenType::UEXTRA_KEYWORD:
        return ResolvedType{DataType::UEXTRA_INT, "uextra"};

    case TokenType::FLOAT_KEYWORD:
        return ResolvedType{DataType::FLOAT, "float"};

    case TokenType::DOUBLE_KEYWORD:
        return ResolvedType{DataType::DOUBLE, "double"};

    case TokenType::STRING_KEYWORD:
        return ResolvedType{DataType::STRING, "string"};

    case TokenType::CHAR_KEYWORD:
        return ResolvedType{DataType::CHAR, "char"};

    case TokenType::CHAR16_KEYWORD:
        return ResolvedType{DataType::CHAR16, "char16"};

    case TokenType::CHAR32_KEYWORD:
        return ResolvedType{DataType::CHAR32, "char32"};

    case TokenType::BOOL_KEYWORD:
        return ResolvedType{DataType::BOOLEAN, "bool"};

    case TokenType::AUTO:
    {
        auto letStmt = dynamic_cast<LetStatement *>(node);
        auto letStmtValue = letStmt->value.get();
        if (!letStmtValue)
        {
            logSemanticErrors("Cannot infer without a value", letStmt->data_type_token.line, letStmt->data_type_token.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        auto inferred = inferNodeDataType(letStmtValue);
        return ResolvedType{inferred.kind, inferred.resolvedName};
    }

    // Dealing with custom types now
    case TokenType::IDENTIFIER:
    {
        std::cout << "INSIDE CUSTOM TYPE RESOLVER\n";
        // Extract the identifier as this how the parser is logging the correct types
        // Case 1 is for let statements
        auto letStmt = dynamic_cast<LetStatement *>(node);
        // Extract the custom data type
        auto letStmtType = letStmt->data_type_token.TokenLiteral;

        // Split the token literal storing the type name
        auto [parentName, childName] = splitScopedName(letStmtType);
        // Let us seacrh for the name of the identifier in the customTypesTable
        // Case a -- A bare bones name
        auto parentIt = customTypesTable.find(parentName);
        if (parentIt == customTypesTable.end())
        {
            logSemanticErrors("Type '" + parentName + "' is unknown", letStmt->data_type_token.line, letStmt->data_type_token.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        // Case b -- having members now this is tricky since the parser stored the whole thing as one name
        if (!childName.empty())
        {
            auto &members = parentIt->second.members;
            auto memberIt = members.find(childName);
            if (memberIt == members.end())
            {
                logSemanticErrors("Type '" + childName + "' does not exist in '" + parentName + "'", letStmt->data_type_token.line, letStmt->data_type_token.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }
            auto memberType = memberIt->second.type;

            return ResolvedType{memberType.kind, memberIt->second.memberName};
        }

        // If we have no members we return the parent
        return ResolvedType{parentIt->second.type.kind, parentIt->second.typeName};
    }

    default:
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }
}

std::pair<std::string, std::string> Semantics::splitScopedName(const std::string &fullName)
{
    std::cout << "Splitting name\n";
    size_t pos = fullName.find("::");
    if (pos == std::string::npos)
    {
        // no scope operator just a plain type
        return {fullName, ""};
    }
    std::string parent = fullName.substr(0, pos);
    std::string child = fullName.substr(pos + 2); // skip ::
    std::cout << "Name has been split into '" + parent + "' and '" + child + "'\n";
    return {parent, child};
}
