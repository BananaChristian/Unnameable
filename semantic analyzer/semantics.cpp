#include <iostream>
#include "semantics.hpp"
#include "ast.hpp"

Semantics::Semantics(Node *node)
{
    symbolTable.push_back({});
    registerAnalyzerFunctions();
    analyzer(node);
};

// Main walker function
void Semantics::analyzer(Node *node)
{
    if (!node)
    {
        return;
    }
    auto analyzerIt = analyzerFunctionsMap.find(typeid(*node));
    if (analyzerIt != analyzerFunctionsMap.end())
    {
        (this->*analyzerIt->second)(node);
    }
    else
    {
        std::cout << "Failed to find analyzer for node: " << node->toString() << "\n";
    }
}

// WALKING FUNCTIONS FOR DIFFERENT NODES
void Semantics::analyzeLetStatements(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;
    std::string declaredTypeStr = letStmt->data_type_token.TokenLiteral; // Getting the data type of the variable
    std::string varName = letStmt->ident_token.TokenLiteral;             // Getting the variable name

    TypeSystem varType = mapTypeStringToTypeSystem(declaredTypeStr); // Getting the type of the variable

    // Checking if the user provided a variable after using auto if not we get the error early
    if (!letStmt->value && declaredTypeStr == "auto")
    {
        std::cerr << "[SEMANTIC ERROR] Cannot use 'auto' without initialization in variable '" << varName << "'\n";
    }

    // Analysing the assigned expressions value if it exists
    if (letStmt->value)
    {
        analyzer(letStmt->value.get());
        TypeSystem exprType = inferExpressionType(letStmt->value.get());

        if (varType == TypeSystem::UNKNOWN)
        {
            // Allowing type inference based on the value of the variable if the keyword used is auto
            if (declaredTypeStr == "auto")
            {
                varType = exprType;
                if (varType == TypeSystem::UNKNOWN)
                {
                    std::cerr << "[SEMANTIC ERROR]: Type inference failed, could not infer type for unkown type for variable" << varName << "\n";
                }
            }
            else
            {
                // No 'auto' and no valid type — reject this statement
                std::cerr << "[SEMANTIC ERROR]: variable '" << varName << "' has no valid type and 'auto' keyword was not used.\n";
            }
        }
        else
        {
            if (exprType != TypeSystem::UNKNOWN && exprType != varType)
            {
                std::cerr << "[SEMANTIC ERROR]: Type mismatch: variable '" << varName << "' declared as '" << declaredTypeStr << "' but assigned value of different type\n";
            }
        }
    }

    Symbol sym{
        .nodeName = varName,
        .nodeType = varType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};

    annotations[letStmt] = SemanticInfo{
        .nodeType = varType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};

    symbolTable.back()[varName] = sym;
}

void Semantics::analyzeIntegerLiteral(Node* node){}
void Semantics::analyzeFloatLiteral(Node* node) {}
void Semantics::analyzeStringLiteral(Node* node) {}
void Semantics::analyzeBooleanLiteral(Node* node) {}
void Semantics::analyzeCharLiteral(Node* node) {}

// HELPER FUNCTIONS
// Functions registers analyzer functions for different nodes
void Semantics::registerAnalyzerFunctions()
{
    analyzerFunctionsMap[typeid(LetStatement)] = &Semantics::analyzeLetStatements;
    analyzerFunctionsMap[typeid(IntegerLiteral)] = &Semantics::analyzeIntegerLiteral;
    analyzerFunctionsMap[typeid(FloatLiteral)] = &Semantics::analyzeFloatLiteral;
    analyzerFunctionsMap[typeid(StringLiteral)] = &Semantics::analyzeStringLiteral;
    analyzerFunctionsMap[typeid(CharLiteral)] = &Semantics::analyzeCharLiteral;
    analyzerFunctionsMap[typeid(BooleanLiteral)]=&Semantics::analyzeBooleanLiteral;
}

// Function maps the type string to the respective type system
TypeSystem Semantics::mapTypeStringToTypeSystem(const std::string &typeStr)
{
    if (typeStr == "int")
        return TypeSystem::INTEGER;
    if (typeStr == "float")
        return TypeSystem::FLOAT;
    if (typeStr == "string")
        return TypeSystem::STRING;
    if (typeStr == "char")
        return TypeSystem::CHAR;
    if (typeStr == "bool")
        return TypeSystem::BOOLEAN;
    return TypeSystem::UNKNOWN;
}

// Type inference helper function
TypeSystem Semantics::inferExpressionType(Expression *expr)
{
    if (!expr)
        return TypeSystem::UNKNOWN;
    if (auto inLit = dynamic_cast<IntegerLiteral *>(expr))
    {
        return TypeSystem::INTEGER;
    }

    if (auto fltLit = dynamic_cast<FloatLiteral *>(expr))
    {
        return TypeSystem::FLOAT;
    }

    if (auto strLit = dynamic_cast<StringLiteral *>(expr))
    {
        return TypeSystem::STRING;
    }

    if (auto chrLit = dynamic_cast<CharLiteral *>(expr))
    {
        return TypeSystem::CHAR;
    }

    if (auto boolLit = dynamic_cast<BooleanLiteral *>(expr))
    {
        return TypeSystem::BOOLEAN;
    }

    if (auto ident = dynamic_cast<Identifier *>(expr))
    {
        std::string name = ident->identifier.TokenLiteral;
        for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it)
        {
            auto symIt = it->find(name);
            if (symIt != it->end())
            {
                return symIt->second.nodeType;
            }
        }
        std::cerr << "Semantic error: identifier '" << name << "' not declared\n";
        return TypeSystem::UNKNOWN;
    }

    if (auto infix = dynamic_cast<InfixExpression *>(expr))
    {
        TypeSystem leftType = inferExpressionType(infix->left_operand.get());
        TypeSystem rightType = inferExpressionType(infix->right_operand.get());
        if (leftType == rightType)
        {
            return leftType;
        }

        if ((leftType == TypeSystem::INTEGER && rightType == TypeSystem::FLOAT) || (leftType == TypeSystem::FLOAT && rightType == TypeSystem::INTEGER))
        {
            return TypeSystem::FLOAT;
        }

        std::cerr << "Type mismatch in infix expression: " << TypeSystemString(leftType) << "vs" << TypeSystemString(rightType) << "\n";
        return TypeSystem::UNKNOWN;
    }

    if (auto prefix = dynamic_cast<PrefixExpression *>(expr))
    {
        return inferExpressionType(prefix->operand.get());
    }

    return TypeSystem::UNKNOWN;
}

// Function converts the type system to a respective string
std::string Semantics::TypeSystemString(TypeSystem type)
{
    switch (type)
    {
    case TypeSystem::INTEGER:
        return "Type: INTEGER ";
    case TypeSystem::FLOAT:
        return "Type: FLOAT ";
    case TypeSystem::STRING:
        return "Type: STRING ";
    case TypeSystem::CHAR:
        return "Type: CHAR ";
    case TypeSystem::BOOLEAN:
        return "Type: BOOLEAN ";
    default:
        return "Type: UNKOWN ";
    }
}
