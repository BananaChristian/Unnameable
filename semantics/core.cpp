#include "semantics.hpp"
#include "ast.hpp"

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
    walkerFunctionsMap[typeid(IntegerLiteral)] = &Semantics::walkIntegerLiteral;
    walkerFunctionsMap[typeid(FloatLiteral)] = &Semantics::walkFloatLiteral;
    walkerFunctionsMap[typeid(DoubleLiteral)] = &Semantics::walkDoubleLiteral;
    walkerFunctionsMap[typeid(StringLiteral)] = &Semantics::walkStringLiteral;
    walkerFunctionsMap[typeid(CharLiteral)] = &Semantics::walkCharLiteral;
    walkerFunctionsMap[typeid(BooleanLiteral)] = &Semantics::walkBooleanLiteral;
    walkerFunctionsMap[typeid(Identifier)] = &Semantics::walkIdentifierExpression;

    // Walker registration for let statement and assignment statements
    walkerFunctionsMap[typeid(LetStatement)] = &Semantics::walkLetStatement;
    walkerFunctionsMap[typeid(AssignmentStatement)] = &Semantics::walkAssignStatement;
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
}

DataType Semantics::inferNodeDataType(Node *node)
{
    if (!node)
        return DataType::UNKNOWN;
    if (auto inLit = dynamic_cast<IntegerLiteral *>(node))
    {
        return DataType::INTEGER;
    }

    if (auto fltLit = dynamic_cast<FloatLiteral *>(node))
    {
        return DataType::FLOAT;
    }

    if (auto dbLit = dynamic_cast<DoubleLiteral *>(node))
    {
        return DataType::DOUBLE;
    }

    if (auto strLit = dynamic_cast<StringLiteral *>(node))
    {
        return DataType::STRING;
    }

    if (auto chrLit = dynamic_cast<CharLiteral *>(node))
    {
        return DataType::CHAR;
    }

    if (auto boolLit = dynamic_cast<BooleanLiteral *>(node))
    {
        return DataType::BOOLEAN;
    }

    if (auto errExpr = dynamic_cast<ErrorExpression *>(node))
    {
        return DataType::ERROR;
    }

    // Dealing with the let statement node type
    if (auto letStmt = dynamic_cast<LetStatement *>(node))
    {
        auto letStmtDataToken = letStmt->data_type_token;
        if (letStmtDataToken.type == TokenType::INT)
        {
            return DataType::INTEGER;
        }
        if (letStmtDataToken.type == TokenType::FLOAT_KEYWORD)
        {
            return DataType::FLOAT;
        }
        if (letStmtDataToken.type == TokenType::DOUBLE_KEYWORD)
        {
            return DataType::DOUBLE;
        }
        if (letStmtDataToken.type == TokenType::STRING_KEYWORD)
        {
            return DataType::STRING;
        }
        if (letStmtDataToken.type == TokenType::CHAR_KEYWORD)
        {
            return DataType::CHAR;
        }
        if (letStmtDataToken.type == TokenType::BOOL_KEYWORD)
        {
            return DataType::BOOLEAN;
        }
        if (letStmtDataToken.type == TokenType::AUTO)
        {
            auto letStmtValue = letStmt->value.get();
            if (!letStmtValue)
            {
                logSemanticErrors("Cannot infer without a value", letStmt->data_type_token.line, letStmt->data_type_token.column);
                return DataType::UNKNOWN;
            }
            return inferNodeDataType(letStmtValue);
        }
    }

    if (auto assignStmt = dynamic_cast<AssignmentStatement *>(node))
    {
        auto assignStmtIdent = assignStmt->ident_token.TokenLiteral;
        auto assignSymbol = resolveSymbolInfo(assignStmtIdent);
        auto assignStmtVal = assignStmt->value.get();
        DataType assignStmtValType = inferNodeDataType(assignStmtVal);
        if (!isTypeCompatible(assignSymbol->symbolDataType, assignStmtValType))
        {
            logSemanticErrors("Type mismatch expected '" + dataTypetoString(assignStmtValType) + "' but got '" + dataTypetoString(assignSymbol->symbolDataType) + "'", assignStmt->ident_token.line, assignStmt->ident_token.column);
        }
        else
        {
            return assignSymbol->symbolDataType;
        }
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

        // Look up the variable in current scope(s)
        auto symbol = resolveSymbolInfo(name);
        if (symbol)
        {
            std::cout << "IDENTIFIER DATA TYPE: " << dataTypetoString(symbol->symbolDataType) << "\n";
            return symbol->symbolDataType;
        }
        else
        {
            logSemanticErrors("Undefined variable '" + name + "'", ident->expression.line, ident->expression.column);
            return DataType::UNKNOWN;
        }
    }

    if (auto retTypeExpr = dynamic_cast<ReturnTypeExpression *>(node))
    {
        return tokenTypeToDataType(retTypeExpr->expression.type, false);
    }

    if (auto callExpr = dynamic_cast<CallExpression *>(node))
    {
        auto symbol = resolveSymbolInfo(callExpr->function_identifier->expression.TokenLiteral);
        if (symbol)
        {
            return symbol->symbolDataType;
        }
        else
        {
            logSemanticErrors("Undefined function name '" + callExpr->function_identifier->expression.TokenLiteral + "'", callExpr->function_identifier->expression.line, callExpr->function_identifier->expression.column);
            return DataType::UNKNOWN;
        }
    }

    return DataType::UNKNOWN;
}

DataType Semantics::inferInfixExpressionType(Node *node)
{
    auto infixNode = dynamic_cast<InfixExpression *>(node);
    if (!infixNode)
        return DataType::UNKNOWN;
    std::cout << "[SEMANTIC LOG] Infering infix type\n";
    DataType leftType = inferNodeDataType(infixNode->left_operand.get());
    DataType rightType = inferNodeDataType(infixNode->right_operand.get());
    TokenType operatorType = infixNode->operat.type;
    return resultOfBinary(operatorType, leftType, rightType);
}

DataType Semantics::inferPrefixExpressionType(Node *node)
{
    auto prefixNode = dynamic_cast<PrefixExpression *>(node);
    if (!prefixNode)
        return DataType::UNKNOWN;
    std::cout << "[SEMANTIC LOG] Infering prefix type\n";
    auto prefixOperator = prefixNode->operat.type;
    DataType operandType = inferNodeDataType(prefixNode->operand.get());
    return resultOfUnary(prefixOperator, operandType);
}

DataType Semantics::inferPostfixExpressionType(Node *node)
{
    auto postfixNode = dynamic_cast<PostfixExpression *>(node);
    if (!postfixNode)
        return DataType::UNKNOWN;
    DataType operandType = inferNodeDataType(postfixNode->operand.get());
    auto postfixOperator = postfixNode->operator_token.type;
    return resultOfUnary(postfixOperator, operandType);
}

Identifier *lastLeftIdent = nullptr;
Identifier *lastRightIdent = nullptr;

DataType Semantics::resultOfBinary(TokenType operatorType, DataType leftType, DataType rightType)
{
    // Dealing with logical operators
    if (operatorType == TokenType::AND || operatorType == TokenType::OR)
    {
        if (leftType == DataType::BOOLEAN && rightType == DataType::BOOLEAN)
        {
            return DataType::BOOLEAN;
        }
        else
        {
            return DataType::UNKNOWN;
        }
    }

    if (operatorType == TokenType::ASSIGN)
    {
        std::cerr << "Cannot use '=' in binary operations it is only for assignments\n";
        return DataType::UNKNOWN;
    }

    // Dealing with comparison operators
    bool isComparison = (operatorType == TokenType::GREATER_THAN ||
                         operatorType == TokenType::GT_OR_EQ ||
                         operatorType == TokenType::LESS_THAN ||
                         operatorType == TokenType::LT_OR_EQ ||
                         operatorType == TokenType::EQUALS ||
                         operatorType == TokenType::NOT_EQUALS);

    if (isComparison)
    {
        if (leftType == rightType)
        {
            return DataType::BOOLEAN;
        }
        else
        {
            std::cerr << "[SEMANTIC ERROR] Cannot compare " << dataTypetoString(leftType) << " and " << dataTypetoString(rightType) << "\n";
            return DataType::UNKNOWN;
        }
    }

    // Allowing string concatenation
    if (operatorType == TokenType::PLUS && leftType == DataType::STRING && rightType == DataType::STRING)
    {
        return DataType::STRING;
    }

    // Dealing with arithmetic operators
    bool isArithmetic = (operatorType == TokenType::PLUS ||
                         operatorType == TokenType::MINUS ||
                         operatorType == TokenType::MODULUS ||
                         operatorType == TokenType::DIVIDE ||
                         operatorType == TokenType::ASTERISK);

    if (isArithmetic)
    {

        if ((leftType == DataType::INTEGER && rightType == DataType::FLOAT) || (leftType == DataType::FLOAT && rightType == DataType::INTEGER))
        {
            return DataType::FLOAT;
        }
        if ((leftType == DataType::INTEGER && rightType == DataType::DOUBLE) || (leftType == DataType::DOUBLE && rightType == DataType::INTEGER))
        {
            return DataType::DOUBLE;
        }
        if ((leftType == DataType::FLOAT && rightType == DataType::DOUBLE) ||
            (leftType == DataType::DOUBLE && rightType == DataType::FLOAT))
        {
            return DataType::DOUBLE;
        }

        if (leftType == rightType)
        {
            return leftType;
        }
        else
        {
            std::cout << "[SEMANTIC ERROR] Type mismatch " + dataTypetoString(leftType) + " does not match " + dataTypetoString(rightType) + "\n";
            return DataType::UNKNOWN;
        }
    }

    std::cerr << "[SEMANTIC ERROR] Unknown binary operator: " << TokenTypeToLiteral(operatorType) << " with types "
              << dataTypetoString(leftType) << " and " << dataTypetoString(rightType) << "\n";
    return DataType::UNKNOWN;
}

DataType Semantics::resultOfUnary(TokenType operatorType, DataType operandType)
{
    // Handling the bang(!)
    if (operatorType == TokenType::BANG)
    {
        if (operandType != DataType::BOOLEAN)
        {
            std::cerr << "[SEMANTIC ERROR] Cannot apply '!' to type " << dataTypetoString(operandType) << "\n";
            return DataType::UNKNOWN;
        }
        return DataType::BOOLEAN;
    }

    if (operatorType == TokenType::MINUS || operatorType == TokenType::PLUS)
    {
        if (operandType == DataType::INTEGER || operandType == DataType::FLOAT || operandType == DataType::DOUBLE)
            return operandType;

        std::cerr << "[SEMANTIC ERROR] Cannot apply " << TokenTypeToLiteral(operatorType) << " to " << dataTypetoString(operandType) << "\n";
        return DataType::UNKNOWN;
    }

    if (operatorType == TokenType::PLUS_PLUS || operatorType == TokenType::MINUS_MINUS)
    {
        if (operandType == DataType::INTEGER || operandType == DataType::FLOAT || operandType == DataType::DOUBLE)
        {
            return operandType;
        }
        std::cerr << "[SEMANTIC ERROR] Cannot apply " << TokenTypeToLiteral(operatorType) << " to " << dataTypetoString(operandType) << "\n";
        return DataType::UNKNOWN;
    }

    return DataType::UNKNOWN;
}

SymbolInfo *Semantics::resolveSymbolInfo(const std::string &name)
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
            return &scope[name];
        }
    }
    std::cout << "[SEMANTIC LOG] No match for '" << name << "'\n";
    return nullptr;
}

DataType Semantics::tokenTypeToDataType(TokenType type, bool isNullable)
{
    switch (type)
    {
    case TokenType::INTEGER_KEYWORD:
        return isNullable ? DataType::NULLABLE_INT : DataType::INTEGER;
    case TokenType::FLOAT_KEYWORD:
        return isNullable ? DataType::NULLABLE_FLT : DataType::FLOAT;
    case TokenType::DOUBLE_KEYWORD:
        return isNullable ? DataType::NULLABLE_DOUBLE : DataType::DOUBLE;
    case TokenType::STRING_KEYWORD:
        return isNullable ? DataType::NULLABLE_STR : DataType::STRING;
    case TokenType::CHAR_KEYWORD:
        return isNullable ? DataType::NULLABLE_CHAR : DataType::CHAR;
    case TokenType::BOOL_KEYWORD:
        return isNullable ? DataType::NULLABLE_BOOLEAN : DataType::BOOLEAN;
    case TokenType::VOID:
        return DataType::VOID;
    case TokenType::IDENTIFIER:
        return DataType::GENERIC;
    default:
        return DataType::UNKNOWN;
    }
}

std::string Semantics::dataTypetoString(DataType type)
{
    switch (type)
    {
    case DataType::INTEGER:
        return "int";
    case DataType::BOOLEAN:
        return "bool";
    case DataType::STRING:
        return "string";
    case DataType::FLOAT:
        return "float";
    case DataType::DOUBLE:
        return "double";
    case DataType::CHAR:
        return "char";
    case DataType::NULLABLE_STR:
        return "string?";
    case DataType::NULLABLE_INT:
        return "int?";
    case DataType::NULLABLE_FLT:
        return "float?";
    case DataType::NULLABLE_CHAR:
        return "char?";
    case DataType::NULLABLE_DOUBLE:
        return "double?";
    case DataType::NULLABLE_BOOLEAN:
        return "bool?";
    case DataType::VOID:
        return "void";
    case DataType::ERROR:
        return "error";
    case DataType::GENERIC:
        return "generic";
    default:
        return "unknown";
    }
}

bool Semantics::isTypeCompatible(DataType expected, DataType actual)
{
    if (actual == DataType::ERROR)
    {
        return true;
    }
    if (expected == actual)
        return true;
    if (expected == DataType::VOID && actual == DataType::UNKNOWN)
    {
        return true;
    }
    if ((expected == DataType::NULLABLE_INT && actual == DataType::INTEGER) ||
        (expected == DataType::NULLABLE_FLT && actual == DataType::FLOAT) ||
        (expected == DataType::NULLABLE_DOUBLE && actual == DataType::DOUBLE) ||
        (expected == DataType::NULLABLE_STR && actual == DataType::STRING) ||
        (expected == DataType::NULLABLE_CHAR && actual == DataType::CHAR) ||
        (expected == DataType::NULLABLE_BOOLEAN && actual == DataType::BOOLEAN))
    {
        return true;
    }
    return false;
}

bool Semantics::hasReturnPath(Node *node)
{
    if (currentFunction && currentFunction->returnType == DataType::VOID)
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
                    (currentFunction->isNullable && !retStmt->return_value))
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
                    (currentFunction->isNullable && !retStmt->return_value))
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
            DataType exprType = inferNodeDataType(blockExpr->finalexpr.value().get());
            return exprType == DataType::ERROR ||
                   isTypeCompatible(currentFunction->returnType, exprType) ||
                   (dynamic_cast<NullLiteral *>(blockExpr->finalexpr.value().get()) &&
                    currentFunction->isNullable);
        }
        return false;
    }

    return false;
}

bool Semantics::areSignaturesCompatible(const SymbolInfo &declInfo, FunctionExpression *funcExpr)
{
    // Check generics
    if (declInfo.genericParams.size() != funcExpr->generic_parameters.size())
    {
        return false;
    }
    for (size_t i = 0; i < declInfo.genericParams.size(); ++i)
    {
        if (declInfo.genericParams[i] != funcExpr->generic_parameters[i].TokenLiteral)
        {
            return false;
        }
    }

    // Check parameters
    if (declInfo.paramTypes.size() != funcExpr->call.size())
    {
        return false;
    }
    for (size_t i = 0; i < declInfo.paramTypes.size(); ++i)
    {
        auto letStmt = dynamic_cast<LetStatement *>(funcExpr->call[i].get());
        if (!letStmt)
            return false;
        DataType paramType = tokenTypeToDataType(letStmt->data_type_token.type, letStmt->isNullable);
        std::string paramGenericName = letStmt->data_type_token.type == TokenType::IDENTIFIER ? letStmt->data_type_token.TokenLiteral : "";
        // Find declaration's parameter metadata
        bool declParamNullable = false;
        for (const auto &pair : metaData)
        {
            if (auto declLetStmt = dynamic_cast<LetStatement *>(pair.first))
            {
                if (declLetStmt->ident_token.TokenLiteral == letStmt->ident_token.TokenLiteral &&
                    pair.second.symbolDataType == declInfo.paramTypes[i].first &&
                    pair.second.genericName == declInfo.paramTypes[i].second)
                {
                    declParamNullable = pair.second.isNullable;
                    break;
                }
            }
        }
        if (paramType != declInfo.paramTypes[i].first ||
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
    DataType returnType = tokenTypeToDataType(retType->expression.type, funcExpr->isNullable);
    std::string returnGenericName = retType->expression.type == TokenType::IDENTIFIER ? retType->expression.TokenLiteral : "";
    return returnType == declInfo.returnType &&
           returnGenericName == declInfo.returnGenericName &&
           funcExpr->isNullable == declInfo.isNullable;
}

bool Semantics::isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr)
{
    // Check parameter count
    if (funcInfo.paramTypes.size() != callExpr->parameters.size())
    {
        logSemanticErrors("Call has " + std::to_string(callExpr->parameters.size()) +
                              " arguments, but function expects " + std::to_string(funcInfo.paramTypes.size()),
                          callExpr->expression.line, callExpr->expression.column);
        return false;
    }

    // Infer types for arguments and compare with expected types
    std::unordered_map<std::string, DataType> genericBindings; // Track generic type bindings

    for (size_t i = 0; i < callExpr->parameters.size(); ++i)
    {
        auto &param = callExpr->parameters[i];
        const auto &expectedType = funcInfo.paramTypes[i];
        DataType argType = DataType::UNKNOWN;

        // Handle null literal based on expected type
        if (auto nullLit = dynamic_cast<NullLiteral *>(param.get()))
        {
            if (expectedType.first == DataType::NULLABLE_INT ||
                expectedType.first == DataType::NULLABLE_STR ||
                expectedType.first == DataType::NULLABLE_BOOLEAN ||
                expectedType.first == DataType::NULLABLE_FLT ||
                expectedType.first == DataType::NULLABLE_DOUBLE ||
                expectedType.first == DataType::NULLABLE_CHAR)
            {
                argType = expectedType.first; // Assign nullable type (e.g., NULLABLE_INT for int?)
            }
            else
            {
                logSemanticErrors("Cannot pass null to non-nullable parameter at position " + std::to_string(i + 1) +
                                      ": expected " + dataTypetoString(expectedType.first),
                                  param->expression.line, param->expression.column);
                return false;
            }
        }
        else
        {
            argType = inferNodeDataType(param.get());
            if (argType == DataType::UNKNOWN)
            {
                logSemanticErrors("Could not infer type for argument at position " + std::to_string(i + 1),
                                  param->expression.line, param->expression.column);
                return false;
            }
        }

        // Handle generic parameters
        if (expectedType.first == DataType::GENERIC)
        {
            if (genericBindings.find(expectedType.second) == genericBindings.end())
            {
                genericBindings[expectedType.second] = argType;
            }
            else if (!isTypeCompatible(genericBindings[expectedType.second], argType))
            {
                logSemanticErrors("Inconsistent generic type '" + expectedType.second +
                                      "' in function call: expected " + dataTypetoString(genericBindings[expectedType.second]) +
                                      ", got " + dataTypetoString(argType),
                                  param->expression.line, param->expression.column);
                return false;
            }
        }
        else if (!isTypeCompatible(expectedType.first, argType))
        {
            logSemanticErrors("Argument type mismatch at position " + std::to_string(i + 1) +
                                  ": expected " + dataTypetoString(expectedType.first) +
                                  ", got " + dataTypetoString(argType),
                              param->expression.line, param->expression.column);
            return false;
        }
    }

    // Validate generic return type
    if (funcInfo.returnType == DataType::GENERIC)
    {
        auto it = genericBindings.find(funcInfo.returnGenericName);
        if (it == genericBindings.end())
        {
            logSemanticErrors("Could not infer generic return type '" + funcInfo.returnGenericName + "'",
                              callExpr->expression.line, callExpr->expression.column);
            return false;
        }
        // Ensure inferred return type is compatible
        if (!isTypeCompatible(funcInfo.returnType, it->second))
        {
            logSemanticErrors("Inferred generic return type '" + dataTypetoString(it->second) +
                                  "' does not match expected type",
                              callExpr->expression.line, callExpr->expression.column);
            return false;
        }
    }

    return true;
}

void Semantics::logSemanticErrors(const std::string &message, int tokenLine, int tokenColumn)
{
    std::cerr << "[SEMANTIC ERROR] " << message << " on line: " << std::to_string(tokenLine) << " and column: " << std::to_string(tokenColumn) << "\n";
}
