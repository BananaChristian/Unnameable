#pragma once

#include "ast.hpp"
#include <string>
#include <typeindex>
#include <cstdint>
#include <llvm/IR/Value.h>

#define CPPREST_FORCE_REBUILD

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

    NULLABLE_ARR, // Special marker datatype for array member inference with nulls

    ENUM,
    DATABLOCK,
    BEHAVIORBLOCK,
    COMPONENT,
    ARRAY,

    ERROR,
    VOID,
    GENERIC,
    UNKNOWN
};

struct ResolvedType
{
    DataType kind; // For the custom inbuilt types
    std::string resolvedName;
    bool isPointer=false;
};

struct MemberInfo
{
    std::string memberName;
    ResolvedType type; // Type of the member
    bool isNullable = false;
    bool isMutable = false;
    bool isConstant = false;
    bool isInitialised = false;

    // Info for enum members
    std::int64_t constantValue = 0;
    ResolvedType parentType; // Parent type for enum members

    Node *node = nullptr;
    llvm::Value *llvmValue = nullptr;
    llvm::Type *llvmType = nullptr;
    int memberIndex = -1;
    bool isHeap = false;
    bool isRef=false; //Reference flag
    bool isPointer=false; //Pointer flag
    Node *lastUseNode = nullptr;
};

struct CustomTypeInfo
{
    std::string typeName;
    ResolvedType type;
    // Special for enum class
    DataType underLyingType = DataType::INTEGER; // Defaulting to 32 bit integer
    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
};

struct ScopeInfo
{
    ResolvedType type;
    std::string typeName;
    bool hasInitConstructor = false;
    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
    Node *node = nullptr;
};

struct ArrayMeta
{
    ResolvedType underLyingType;
    int arrLen;
};

// Information about the symbol(variable or object, whatever)
struct SymbolInfo
{
    ResolvedType type;
    std::string genericName;
    bool isNullable = false;
    bool isDefinitelyNull = false;
    bool isMutable = false;
    bool isConstant = false;
    bool isInitialized = false;
    int64_t constIntVal;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
    ResolvedType returnType;
    // Function flags
    bool isDeclaration = false;
    bool isDefined = false;

    // Array flags
    ArrayMeta arrayMeta;

    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
    llvm::Value *llvmValue = nullptr;
    llvm::Type *llvmType = nullptr;
    int memberIndex = -1;

    bool isHeap = false;
    bool isRef=false; //Reference flag
    bool isPointer=false;//Pointer flag

    size_t componentSize;
    int alloc_id = 0; // This is a field for the sentinel layer
    Node *lastUseNode = nullptr;

    // Error flag
    bool hasError = false;
};

class Semantics
{
public:
    Semantics();
    void walker(Node *node);
    using walkerFunctions = void (Semantics::*)(Node *);
    std::unordered_map<std::type_index, walkerFunctions> walkerFunctionsMap;
    std::vector<std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>> symbolTable;
    std::unordered_map<std::string, CustomTypeInfo> customTypesTable;
    std::unordered_map<Node *, std::shared_ptr<SymbolInfo>> metaData;
    std::optional<std::shared_ptr<SymbolInfo>> currentFunction;
    std::vector<bool> loopContext;
    std::vector<ScopeInfo> currentTypeStack;

    std::unordered_map<std::string, std::vector<ResolvedType>> componentInitArgs;

    // Public helpers
    std::shared_ptr<SymbolInfo> resolveSymbolInfo(const std::string &name);
    ResolvedType resolvedDataType(Token token, Node *node);
    bool hasReturnPath(Node *node);
    ResolvedType inferNodeDataType(Node *node);
    std::pair<std::string, std::string> splitScopedName(const std::string &fullName);

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

    void walkNullLiteral(Node *node);

    // Walking the component functions declaration
    void walkDataStatement(Node *node);
    void walkBehaviorStatement(Node *node);
    void walkUseStatement(Node *node);
    void walkComponentStatement(Node *node);
    void walkNewComponentExpression(Node *node);
    void walkInitConstructor(Node *node);
    void walkSelfExpression(Node *node);
    void walkEnumClassStatement(Node *node);

    // Waling infix, prefix and postfix expressions
    void walkInfixExpression(Node *node);
    void walkPrefixExpression(Node *node);
    void walkPostfixExpression(Node *node);

    // Waling identifier expression
    void walkIdentifierExpression(Node *node);
    void walkAddressExpression(Node *node);
    void walkDereferenceExpression(Node *node);

    // Walking expression statement
    void walkExpressionStatement(Node *node);

    // Walking the let statements and assignment statements
    void walkLetStatement(Node *node);
    void walkAssignStatement(Node *node);
    void walkFieldAssignmentStatement(Node *node);

    //Walking reference and pointer statement
    void walkReferenceStatement(Node *node);
    void walkPointerStatement(Node *node);

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

    // Walking type expressions
    void walkBasicType(Node *node);
    void walkArrayType(Node *node);

    // Walking blocks
    void walkBlockStatement(Node *node);
    void walkBlockExpression(Node *node);

    // Walking arrays
    void walkArrayLiteral(Node *node);
    void walkArrayStatement(Node *node);
    void walkArraySubscriptExpression(Node *node);

    // HELPER FUNCTIONS
    void registerWalkerFunctions();
    ResolvedType inferInfixExpressionType(Node *node);
    ResolvedType inferPrefixExpressionType(Node *node);
    ResolvedType inferPostfixExpressionType(Node *node);
    ResolvedType resultOfBinary(TokenType operatorType, ResolvedType leftType, ResolvedType rightType);
    ResolvedType resultOfUnary(TokenType operatorType, const ResolvedType &oprendType);
    ResolvedType tokenTypeToResolvedType(Token token, bool isNullable);
    ResolvedType resultOfScopeOrDot(TokenType operatorType, const std::string &parentName, const std::string &childName, InfixExpression *infix);
    ResolvedType isPointerType(ResolvedType t);
    bool isTypeCompatible(const ResolvedType &expected, const ResolvedType &actual);
    int64_t SubscriptIndexVerifier(Node *indexNode, int64_t arrLen);
    ArrayMeta getArrayMeta(Node *node);
    int64_t getIntExprVal(Node *node);
    bool areSignaturesCompatible(const SymbolInfo &declInfo, FunctionExpression *funcExpr);
    bool isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr);
    bool isInteger(const ResolvedType &t);
    bool isNullableInteger(const ResolvedType &t);
    bool isFloat(const ResolvedType &t);
    bool isNullableFloat(const ResolvedType &t);
    bool isBoolean(const ResolvedType &t);
    bool isString(const ResolvedType &t);
    bool isChar(const ResolvedType &t);
    bool isNullable(const ResolvedType &t);
    void logSemanticErrors(const std::string &message, int tokenLine, int tokenColumn);
};
