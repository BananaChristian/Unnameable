#define CPPREST_FORCE_REBUILD
#ifndef SEMANTICS_HPP
#define SEMANTICS_HPP

#include "ast.hpp"
#include <string>
#include <typeindex>

// Type system tracker
enum class DataType
{
    SHORT_INT, // 16 BIT signed integer
    NULLABLE_SHORT_INT,

    USHORT_INT, // 16 BUT unsigned integer
    NULLABLE_USHORT_INT,

    INTEGER, // 32 BIT signed integer
    NULLABLE_INT,

    UINTEGER, // 32 BIT unsigned integer
    NULLABLE_UINT,

    LONG_INT, // 64 BIT signed integer
    NULLABLE_LONG_INT,

    ULONG_INT, // 64 BIT unsigned integer
    NULLABLE_ULONG_INT,

    EXTRA_INT, // 128 BIT signed integer
    NULLABLE_EXTRA_INT,

    UEXTRA_INT, // 128 BIT unsigned integer
    NULLABLE_UEXTRA_INT,

    BOOLEAN,
    NULLABLE_BOOLEAN,

    STRING,
    NULLABLE_STR,

    FLOAT,
    NULLABLE_FLT,

    DOUBLE,
    NULLABLE_DOUBLE,

    CHAR, // 8 BIT Char
    NULLABLE_CHAR,

    CHAR16, // 16 BIT Char
    NULLABLE_CHAR16,

    CHAR32, // 32 BIT Char
    NULLABLE_CHAR32,

    ENUM,
    ERROR,
    VOID,
    GENERIC,
    UNKNOWN
};

// Information about the symbol(variable or object, whatever)
struct SymbolInfo
{
    DataType symbolDataType;
    std::string genericName;
    bool isNullable = false;
    bool isMutable = false;
    bool isConstant = false;
    bool isInitialized = false;
    std::vector<std::pair<DataType, std::string>> paramTypes;
    DataType returnType;
    std::string returnGenericName;
    std::vector<std::string> genericParams;
    // Function flags
    bool isDeclaration = false;
    bool isDefined = false;
    // enum info
    std::string enumName;
    std::vector<std::string> enumContent;
    int constantValue = 0; // For enum members to store assigned int value
    std::vector<std::pair<std::string, int>> enumMembers;
    DataType enumIntType = DataType::INTEGER; // Defaults to a 32 bit integer data type
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
    std::optional<SymbolInfo> currentFunction;
    std::vector<bool> loopContext;

    SymbolInfo *resolveSymbolInfo(const std::string &name);

    std::string dataTypetoString(DataType type);

private:
    // Walking the data type literals
    void walkShortLiteral(Node *node);
    void walkUnsignedShortLiteral(Node *node);
    void walkIntegerLiteral(Node *node);
    void walkUnsignedIntegerLiteral(Node *node);
    void walkLongLiteral(Node *node);
    void walkUnsignedLongLiteral(Node *node);
    void walkExtraLiteral(Node *node);
    void walkUnsignedExtraLiteral(Node *node);

    void walkStringLiteral(Node *node);
    void walkBooleanLiteral(Node *node);

    void walkCharLiteral(Node *node);
    void walkChar16Literal(Node *node);
    void walkChar32Literal(Node *node);

    void walkDoubleLiteral(Node *node);
    void walkFloatLiteral(Node *node);

    // Walking the component functions declaration
    void walkDataStatement(Node *node);
    void walkEnumClassStatement(Node *node);

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
    void walkFunctionParameterLetStatement(Node *node);
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

    // walking functional programming nodes
    void walkFunctionStatement(Node *node);
    void walkFunctionExpression(Node *node);
    void walkFunctionDeclarationStatement(Node *node);
    void walkFunctionDeclarationExpression(Node *node);
    void walkFunctionCallExpression(Node *node);
    void walkReturnStatement(Node *node);

    // Walking blocks
    void walkBlockStatement(Node *node);
    void walkBlockExpression(Node *node);

    // HELPER FUNCTIONS
    void registerWalkerFunctions();
    DataType inferNodeDataType(Node *node);
    DataType inferInfixExpressionType(Node *node);
    DataType inferPrefixExpressionType(Node *node);
    DataType inferPostfixExpressionType(Node *node);
    DataType resultOfBinary(TokenType operatorType, DataType leftType, DataType rightType);
    DataType resultOfUnary(TokenType operatorType, DataType oprendType);
    DataType tokenTypeToDataType(TokenType type, bool isNullable);
    bool isTypeCompatible(DataType expected, DataType actual);
    bool areSignaturesCompatible(const SymbolInfo &declInfo, FunctionExpression *funcExpr);
    bool isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr);
    bool hasReturnPath(Node *node);
    bool isInteger(DataType t);
    bool isNullableInteger(DataType t);
    bool isFloat(DataType t);
    bool isNullableFloat(DataType t);
    bool isBoolean(DataType t);
    bool isString(DataType t);
    bool isChar(DataType t);
    bool isNullable(DataType t);
    void logSemanticErrors(const std::string &message, int tokenLine, int tokenColumn);
};

#endif