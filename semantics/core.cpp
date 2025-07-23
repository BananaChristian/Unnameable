#include "semantics.hpp"

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
    walkerFunctionsMap[typeid(NullLiteral)] = &Semantics::walkNullLiteral;
    walkerFunctionsMap[typeid(Identifier)] = &Semantics::walkIdentifierExpression;

    // Walker registration for let statement and assignment statements
    walkerFunctionsMap[typeid(LetStatement)] = &Semantics::walkLetStatement;
    walkerFunctionsMap[typeid(AssignmentStatement)] = &Semantics::walkAssignStatement;

    // Walker registration for control flow
    walkerFunctionsMap[typeid(ifStatement)] = &Semantics::walkIfStatement;

    // Walker registration for loops
    walkerFunctionsMap[typeid(WhileStatement)] = &Semantics::walkWhileStatement;

    // Walker registration for blocks
    walkerFunctionsMap[typeid(BlockStatement)] = &Semantics::walkBlockStatement;

    // Walker registration for the main expression types
    walkerFunctionsMap[typeid(InfixExpression)] = &Semantics::walkInfixExpression;
    walkerFunctionsMap[typeid(PrefixExpression)] = &Semantics::walkPrefixExpression;
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

    if (auto nullLit = dynamic_cast<NullLiteral *>(node))
    {
        return DataType::NULLABLE;
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
                std::cerr << "[SEMANTIC ERROR] Cannot infer without a value\n";
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
        if (assignSymbol->symbolDataType != assignStmtValType)
        {
            std::cerr << "[SEMANTIC ERROR] Type mismatch";
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

    if (auto ident = dynamic_cast<Identifier *>(node))
    {
        std::string name = ident->identifier.TokenLiteral;

        // Look up the variable in current scope(s)
        auto symbol = resolveSymbolInfo(name);
        if (symbol)
        {
            return symbol->symbolDataType;
        }
        else
        {
            std::cerr << "[SEMANTIC ERROR] Undefined variable: " << name << "\n";
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
    case DataType::NULLABLE:
        return "null";
    default:
        return "unknown";
    }
}
