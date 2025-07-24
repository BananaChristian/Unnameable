#include "ast.hpp"
#include <string>
#include <typeindex>

// Type system tracker
enum class DataType
{
    INTEGER,
    BOOLEAN,
    STRING,
    FLOAT,
    DOUBLE,
    CHAR,
    NULLABLE,
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
    std::unordered_map<Node *, SymbolInfo> metaData;
    std::vector<bool> loopContext;

private:
    // Walking the data type literals
    void walkIntegerLiteral(Node *node);
    void walkStringLiteral(Node *node);
    void walkBooleanLiteral(Node *node);
    void walkCharLiteral(Node *node);
    void walkDoubleLiteral(Node *node);
    void walkFloatLiteral(Node *node);
    void walkNullLiteral(Node *node);

    // Waling infix and prefix expressions
    void walkInfixExpression(Node *node);
    void walkPrefixExpression(Node *node);

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

    // Walking control flow nodes
    void walkWhileStatement(Node *node);
    void walkForStatement(Node *node);
    void walkEachStatement(Node *node);
    void walkIfStatement(Node *node);
    void walkSwitchStatement(Node *node);
    void walkCaseStatement(Node *node);

    // Walking blocks
    void walkBlockStatement(Node *node);

    // HELPER FUNCTIONS
    void registerWalkerFunctions();
    DataType inferNodeDataType(Node *node);
    DataType inferInfixExpressionType(Node *node);
    DataType inferPrefixExpressionType(Node *node);
    DataType resultOfBinary(TokenType operatorType, DataType leftType, DataType rightType);
    DataType resultOfUnary(TokenType operatorType, DataType oprendType);
    SymbolInfo *resolveSymbolInfo(const std::string &name);
    std::string dataTypetoString(DataType type);
    void logSemanticErrors(const std::string &message);
};