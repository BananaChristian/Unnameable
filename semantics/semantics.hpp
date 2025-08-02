#define CPPREST_FORCE_REBUILD
#ifndef SEMANTICS_HPP
#define SEMANTICS_HPP

#include "ast.hpp"
#include <string>
#include <typeindex>

// Type system tracker
enum class DataType
{
    INTEGER,
    NULLABLE_INT,
    BOOLEAN,
    NULLABLE_BOOLEAN,
    STRING,
    NULLABLE_STR,
    FLOAT,
    NULLABLE_FLT,
    DOUBLE,
    NULLABLE_DOUBLE,
    CHAR,
    NULLABLE_CHAR,
    ERROR,
    UNKNOWN
};

// Information about the symbol(variable or object, whatever)
struct SymbolInfo
{
    DataType symbolDataType;
    bool isNullable = false;
    bool isMutable = false;
    bool isConstant = false;
    bool isInitialized = false;
};

// Information about the current scope
struct Scope
{
    std::unordered_map<std::string, SymbolInfo> symbol;
    bool isGlobal = false;
    bool isComponent = false;
    bool isFunction = false;
    std::shared_ptr<Scope> parentScope;
};

class Semantics
{

public:
    Semantics();
    void walker(Node *node);
    using walkerFunctions = void (Semantics::*)(Node *);
    std::unordered_map<std::type_index, walkerFunctions> walkerFunctionsMap;
    std::vector<std::unordered_map<std::string, SymbolInfo>> symbolTable;
    std::unordered_map<std::string, std::vector<SymbolInfo>> sharedDataBlocks;
    std::unordered_map<Node *, SymbolInfo> metaData;
    std::vector<bool> loopContext;

    SymbolInfo *resolveSymbolInfo(const std::string &name);

    std::string dataTypetoString(DataType type);

private:
    // Walking the data type literals
    void walkIntegerLiteral(Node *node);
    void walkStringLiteral(Node *node);
    void walkBooleanLiteral(Node *node);
    void walkCharLiteral(Node *node);
    void walkDoubleLiteral(Node *node);
    void walkFloatLiteral(Node *node);

    // Walking the component functions declaration
    void walkDataStatement(Node *node);

    // Waling infix, prefix and postfix expressions
    void walkInfixExpression(Node *node);
    void walkPrefixExpression(Node *node);
    void walkPostfixExpression(Node *node);

    // Waling identifier expression
    void walkIdentifierExpression(Node *node);

    // Walking expression statement
    void walkExpressionStatement(Node *node);

    // Walking the let statements and assignment statements
    void walkLetStatement(Node *node);
    void walkAssignStatement(Node *node);

    // Walking the loop disruption statements
    void walkBreakStatement(Node *node);
    void walkContinueStatement(Node *node);

    void walkErrorStatement(Node *node);
    void walkErrorExpression(Node *node);

    // Walking control flow nodes
    void walkWhileStatement(Node *node);
    void walkForStatement(Node *node);
    void walkEachStatement(Node *node);
    void walkIfStatement(Node *node);
    void walkElifStatement(Node *node);
    void walkSwitchStatement(Node *node);
    void walkCaseStatement(Node *node);

    // Walking blocks
    void walkBlockStatement(Node *node);

    // HELPER FUNCTIONS
    void registerWalkerFunctions();
    DataType inferNodeDataType(Node *node);
    DataType inferInfixExpressionType(Node *node);
    DataType inferPrefixExpressionType(Node *node);
    DataType inferPostfixExpressionType(Node *node);
    DataType resultOfBinary(TokenType operatorType, DataType leftType, DataType rightType);
    DataType resultOfUnary(TokenType operatorType, DataType oprendType);
    Token getErrorToken(Node *node);
    DataType tokenTypeToDataType(TokenType type, bool isNullable);
    bool isTypeCompatible(DataType expected, DataType actual);
    void logSemanticErrors(const std::string &message, Node *node);
};

#endif