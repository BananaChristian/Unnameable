#include "semantics_test.hpp"

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

    // Walker registration for let statement and assignment statements
    walkerFunctionsMap[typeid(LetStatement)] = &Semantics::walkLetStatement;
    walkerFunctionsMap[typeid(AssignmentStatement)] = &Semantics::walkAssignStatement;
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