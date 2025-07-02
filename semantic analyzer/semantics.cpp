#include <iostream>
#include "semantics.hpp"
#include "ast.hpp"

Semantics::Semantics()
{
    symbolTable.push_back({});
    registerAnalyzerFunctions();
};

// Main walker function
void Semantics::analyzer(Node *node)
{
    if (!node)
    {
        return;
    }
    std::cout << "Analyzing AST node: "<<node->toString()<<"\n";
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
    std::cout << "[SEMANTIC LOG]: Analyzing let statement: " << dynamic_cast<LetStatement *>(node)->toString() << "\n";
    std::cout << "[DEBUG]: Current symbol table size: " << symbolTable.size() << "\n";
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
        logError("Cannot use 'auto' without initialization in variable ", letStmt);
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
    std::cout << "[DEBUG] Inserted '" << varName << "' into scope 0\n";

}

void Semantics::analyzeLetStatementsNoType(Node *node)
{
    std::cout << "[SEMANTIC LOG] Analyzing Let Statement No type: " << dynamic_cast<LetStatementNoType *>(node)->ident_token.TokenLiteral << "\n";
    auto stmtNode = dynamic_cast<LetStatementNoType *>(node);
    if (!stmtNode)
        return;
    // Getting the datatype of x by walking the scope stack to see if it was stored somewhere
    auto identifierName = stmtNode->ident_token.TokenLiteral;
    auto identSymbol = resolveSymbol(identifierName);
    if (!identSymbol)
    {
        std::cerr << "[SEMANTIC ERROR]: Variable '" << identifierName << "' not declared.\n";
        return;
    }
    auto identType = identSymbol->nodeType;

    auto valueType = inferExpressionType(stmtNode->value.get());
    if (identType != valueType)
    {
        std::cerr << "[SEMANTIC ERROR]: Type mismatch: " << identifierName << " doesnt match " << TypeSystemString(valueType) << "\n";
        return;
    }

    annotations[stmtNode] = SemanticInfo{
        .nodeType = identType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeIntegerLiteral(Node *node)
{
    std::cout << "[SEMANTIC LOG] Analyzing IntegerLiteral: " << dynamic_cast<IntegerLiteral *>(node)->int_token.TokenLiteral << "\n";
    if (!node)
        return;

    IntegerLiteral *intNode = dynamic_cast<IntegerLiteral *>(node);
    if (!intNode)
    {
        std::cerr << "[SEMANTIC ERROR]: Failed to analyze integer node received wrong node" << "\n";
        return;
    }
    std::string intName = intNode->int_token.TokenLiteral;
    TypeSystem intType = TypeSystem::INTEGER;

    annotations[intNode] = SemanticInfo{
        .nodeType = intType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeFloatLiteral(Node *node)
{
    std::cout << "[SEMANTIC LOG] Analyzing FloatLiteral: " << dynamic_cast<FloatLiteral *>(node)->float_token.TokenLiteral << "\n";
    if (!node)
        return;
    FloatLiteral *fltNode = dynamic_cast<FloatLiteral *>(node);
    if (!fltNode)
    {
        std::cerr << "[SEMANTIC ERROR]: Failed to analyze float node recieved";
        return;
    }
    std::string fltName = fltNode->float_token.TokenLiteral;
    TypeSystem fltType = TypeSystem::FLOAT;

    annotations[fltNode] = SemanticInfo{
        .nodeType = fltType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeStringLiteral(Node *node)
{
    if (!node)
        return;
    StringLiteral *strNode = dynamic_cast<StringLiteral *>(node);
    std::cout << "[SEMANTIC LOG]: Analyzing string node: " << strNode->string_token.TokenLiteral << "\n";
    if (!strNode)
    {
        std::cout << "Failed to analyze string node\n";
        return;
    }
    TypeSystem strType = TypeSystem::STRING;
    annotations[strNode] = SemanticInfo{
        .nodeType = strType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}
void Semantics::analyzeBooleanLiteral(Node *node)
{
    if (!node)
        return;
    BooleanLiteral *boolNode = dynamic_cast<BooleanLiteral *>(node);
    std::cout << "[SEMANTIC LOG]: Analyzing boolean node: " << boolNode->boolean_token.TokenLiteral << "\n";
    if (!boolNode)
    {
        std::cout << "Failed to analyze boolean node\n";
        return;
    }
    TypeSystem boolType = TypeSystem::BOOLEAN;
    annotations[boolNode] = SemanticInfo{
        .nodeType = boolType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeCharLiteral(Node *node)
{
    if (!node)
        return;
    CharLiteral *charNode = dynamic_cast<CharLiteral *>(node);
    std::cout << "[SEMANTIC LOG]: Analyzing char node: " << charNode->char_token.TokenLiteral << "\n";
    if (!charNode)
    {
        std::cout << "Failed to analyze char node\n";
        return;
    }
    TypeSystem charType = TypeSystem::CHAR;
    annotations[charNode] = SemanticInfo{
        .nodeType = charType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeInfixExpression(Node *node)
{
    std::cout << "[SEMANTIC LOG]: Analyzing infix node\n";
    auto infixNode = dynamic_cast<InfixExpression *>(node);
    if (!infixNode)
        return;
    TypeSystem leftType = inferExpressionType(infixNode->left_operand.get());
    TypeSystem rightType = inferExpressionType(infixNode->right_operand.get());

    TypeSystem resultType;
    if ((leftType == TypeSystem::INTEGER && rightType == TypeSystem::FLOAT) || (leftType == TypeSystem::FLOAT && rightType == TypeSystem::INTEGER))
    {
        resultType = TypeSystem::FLOAT;
    }
    else if (leftType != rightType)
    {
        std::cerr << "[SEMANTIC ERROR]: Type mismatch (" << TypeSystemString(leftType) << " vs " << TypeSystemString(rightType) << ")\n";
        return;
    }
    else
    {
        resultType = leftType;
    }
    if (!infixNode)
        return;
    annotations[infixNode] = SemanticInfo{
        .nodeType = resultType,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

// HELPER FUNCTIONS
// Functions registers analyzer functions for different nodes
void Semantics::registerAnalyzerFunctions()
{
    analyzerFunctionsMap[typeid(LetStatement)] = &Semantics::analyzeLetStatements;
    analyzerFunctionsMap[typeid(IntegerLiteral)] = &Semantics::analyzeIntegerLiteral;
    analyzerFunctionsMap[typeid(FloatLiteral)] = &Semantics::analyzeFloatLiteral;
    analyzerFunctionsMap[typeid(StringLiteral)] = &Semantics::analyzeStringLiteral;
    analyzerFunctionsMap[typeid(CharLiteral)] = &Semantics::analyzeCharLiteral;
    analyzerFunctionsMap[typeid(BooleanLiteral)] = &Semantics::analyzeBooleanLiteral;
    analyzerFunctionsMap[typeid(InfixExpression)] = &Semantics::analyzeInfixExpression;
    analyzerFunctionsMap[typeid(LetStatementNoType)] = &Semantics::analyzeLetStatementsNoType;
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

std::optional<Symbol> Semantics::resolveSymbol(const std::string &name)
{
    for (int i = symbolTable.size() - 1; i >= 0; --i)
    {
        auto &scope = symbolTable[i];
        std::cout << "[DEBUG] Searching for '" << name << "' in scope level " << i << "\n";
        for (auto& [key, val] : scope) {
            std::cout << "    >> Key in scope: '" << key << "'\n";
        }
        if (scope.find(name) != scope.end())
        {
            std::cout << "[DEBUG] Found match for '" << name << "'\n";
            return scope[name];
        }
    }
    std::cout << "[DEBUG] No match for '" << name << "'\n";
    return std::nullopt;
}

// Error logging function
void Semantics::logError(const std::string &message, Node *node)
{
    std::cerr << "[SEMANTIC ERROR]: " << message << "line: " << node->token.line << " column: " << node->token.column << "\n";
}
