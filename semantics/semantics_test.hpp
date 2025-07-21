#include "ast.hpp"
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

private:
    // Walking the data type literals
    void walkIntegerLiteral(Node *node);
    void walkStringLiteral(Node *node);
    void walkBooleanLiteral(Node *node);
    void walkCharLiteral(Node *node);
    void walkDoubleLiteral(Node *node);
    void walkFloatLiteral(Node *node);
    void walkNullLiteral(Node *node);

    // Walking the let statements and assignment statements
    void walkLetStatement(Node *node);
    void walkAssignStatement(Node *node);

    // HELPER FUNCTIONS
    void registerWalkerFunctions();
    DataType inferNodeDataType(Node *node);
    SymbolInfo *resolveSymbolInfo(const std::string &name);
    void logSemanticErrors(const std::string &message);
};