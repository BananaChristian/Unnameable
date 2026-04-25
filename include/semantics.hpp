#pragma once
#include <llvm/IR/Value.h>

#include <cstdint>
#include <memory>
#include <string>
#include <typeindex>
#include <unordered_map>
#include <unordered_set>

#include "ast.hpp"
#include "defs.hpp"
#include "deserial.hpp"
#include "errors.hpp"
#include "token.hpp"

class Semantics {
    ErrorHandler &errorHandler;

   public:
    Deserializer &deserializer;
    Semantics(Deserializer &deserializer, ErrorHandler &handler, bool isVerbose);
    void walker(Node *node);
    bool failed();

    using walkerFunctions = void (Semantics::*)(Node *);
    std::unordered_map<std::type_index, walkerFunctions> walkerFunctionsMap;
    std::vector<std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>> symbolTable;
    std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>> customTypesTable;
    std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>> ImportedComponentTable;
    std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>> ImportedRecordTable;
    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> ImportedFunctionsTable;
    std::unordered_map<std::string,std::shared_ptr<SymbolInfo>> ImportedVariablesTable;
    std::unordered_map<Node *, std::shared_ptr<SymbolInfo>> metaData;

    using LifeTimeTable = std::unordered_map<Node *, std::unique_ptr<LifeTime>>;
    LifeTimeTable responsibilityTable;

    std::unordered_map<std::string, std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>>
        sealTable;
    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> importedInits;
    std::optional<std::shared_ptr<SymbolInfo>> currentFunction;
    std::vector<bool> loopContext;
    std::vector<bool> caseContext;
    std::vector<ScopeInfo> currentTypeStack;
    std::unordered_set<std::string> heistIDs;

    std::unordered_map<std::string, GenericBluePrint> genericMap;
    std::unordered_map<std::string, AllocatorHandle> allocatorMap;
    std::unordered_map<std::string, std::vector<ResolvedType>> componentInitArgs;

    std::unordered_map<Node *, std::vector<std::string>> bornInBlock;

    // Public helpers
    std::shared_ptr<SymbolInfo> resolveSymbolInfo(const std::string &name);
    std::shared_ptr<SymbolInfo> lookUpInCurrentScope(const std::string &name);
    ResolvedType inferDeclarationBaseType(VariableDeclaration *declaration);
    bool hasReturnPath(Node *node);
    bool switchReturnsInAllPaths(SwitchStatement *sw);
    bool ifReturnsInAllPaths(ifStatement *ifStmt);
    bool hasReturnPathInBlock(const std::vector<std::unique_ptr<Statement>> &statements);
    bool hasReturnPathList(const std::vector<std::unique_ptr<Statement>> &stmts);
    ResolvedType inferNodeDataType(Node *node);
    std::string extractIdentifierName(Node *node);
    std::string extractDeclarationName(Node *node);
    ResolvedType peelRef(const ResolvedType &t);
    bool isBornInScope(Node *root, const std::string &ID);
    bool isTerminator(Node *node);

    bool isInteger(const ResolvedType &t);
    bool isFloat(const ResolvedType &t);
    bool isBoolean(const ResolvedType &t);
    bool isString(const ResolvedType &t);
    bool isChar(const ResolvedType &t);
    bool isConstLiteral(Node *node);

    LifeTime *getBaton(const std::string &ID);
    std::vector<Identifier *> digIdentifiers(Node *node);
    std::shared_ptr<SymbolInfo> getSymbolFromMeta(Node *node);
    void transferResponsibility(LifeTime *currentBaton, LifeTime *targetBaton,
                                const std::shared_ptr<SymbolInfo> &tagetSym);
    Node *queryForLifeTimeBaton(const std::string &familyID);
    const std::unique_ptr<LifeTime> &readBatonInfo(const std::string &batonID);
    ResolvedType getArrayElementType(const ResolvedType &arrayType);
    void collectDimensions(TypeModifier *modifier, std::vector<uint64_t> &staticDims,
                           std::vector<Node *> &dynamicDims);
    bool isIntegerConstant(Node *node);
    bool isFloatConstant(Node *node);
    std::vector<uint64_t> getSizePerDimesion(Node *node);
    std::string getBaseTypeName(const ResolvedType &type);
    uint64_t getIntegerConstant(Node *node);

   private:
    bool insideFunction = false;
    bool insideComponent = false;
    bool insideRecord = false;
    bool insideSeal = false;
    bool insideAllocator = false;
    bool insideTrace = false;

    bool hasFailed = false;
    bool hasError = false;
    bool verbose = false;
    std::vector<Node *> activeBlocks;

    uint64_t normalDeclCount = 0;
    uint64_t ptrDeclCount = 0;
    uint64_t arrDeclCount = 0;

    // Walking the data type literals
    void walkI8Literal(Node *node);
    void walkU8Literal(Node *node);
    void walkI16Literal(Node *node);
    void walkU16Literal(Node *node);
    void walkI32Literal(Node *node);
    void walkU32Literal(Node *node);
    void walkI64Literal(Node *node);
    void walkU64Literal(Node *node);
    void walkI128Literal(Node *node);
    void walkU128Literal(Node *node);
    void walkISIZELiteral(Node *node);
    void walkUSIZELiteral(Node *node);
    void walkINTLiteral(Node *node);

    void walkStringLiteral(Node *node);
    void walkFStringLiteral(Node *node);
    void walkBooleanLiteral(Node *node);

    void walkChar8Literal(Node *node);
    void walkChar16Literal(Node *node);
    void walkChar32Literal(Node *node);

    void walkF64Literal(Node *node);
    void walkF32Literal(Node *node);
    void walkFloatLiteral(Node *node);

    void walkSizeOfExpression(Node *node);

    void walkNullLiteral(Node *node);

    // Walking the component functions declaration
    void walkRecordStatement(Node *node);
    void walkComponentStatement(Node *node);
    void walkNewComponentExpression(Node *node);
    void walkInitConstructor(Node *node);
    void walkSelfExpression(Node *node);
    void walkEnumStatement(Node *node);
    void walkInstanceExpression(Node *node);
    void walkMethodCallExpression(Node *node);

    // Waling infix, prefix and postfix expressions
    void walkInfixExpression(Node *node);
    void walkPrefixExpression(Node *node);
    void walkPostfixExpression(Node *node);

    void walkIdentifierExpression(Node *node);
    void walkAddressExpression(Node *node);
    void walkDereferenceExpression(Node *node);

    // Walking expression statement
    void walkExpressionStatement(Node *node);

    // Walking the let statements and assignment statements
    void walkVariableDeclaration(Node *node);
    void walkSelfAssignment(AssignmentStatement *assignStmt);
    void walkAssignStatement(Node *node);
    void walkFieldAssignmentStatement(Node *node);

    // Walking the loop disruption statements
    void walkBreakStatement(Node *node);
    void walkContinueStatement(Node *node);

    // Walking control flow nodes
    void walkWhileStatement(Node *node);
    void walkForStatement(Node *node);
    void walkIfStatement(Node *node);
    void walkElifStatement(Node *node);
    void walkSwitchStatement(Node *node);
    void walkCaseStatement(Node *node, const ResolvedType &targetType);

    // walking functional programming nodes
    void walkFunctionStatement(Node *node);
    void walkFunctionExpression(Node *node);
    void walkFunctionDeclarationStatement(Node *node);
    void walkFunctionDeclarationExpression(Node *node);
    void walkFunctionCallExpression(Node *node);
    void walkUnwrapExpression(Node *node);
    void walkReturnStatement(Node *node);

    // Walking type expressions
    void walkBasicType(Node *node);
    // Walking blocks
    void walkBlockStatement(Node *node);
    void walkBlockExpression(Node *node);

    // Walking arrays
    void walkArrayLiteral(Node *node);
    void walkArraySubscriptExpression(Node *node);

    // Walking allocator interface
    void walkAllocatorInterface(Node *node);

    // Walking generics
    void walkGenericStatement(Node *node);
    void walkInstantiateStatement(Node *node);

    // Walking casts
    void walkCastExpression(Node *node);
    void walkBitcastExpression(Node *node);

    void walkSealStatement(Node *node);

    // Walking the shout statement
    void walkTraceStatement(Node *node);

    void walkFunctionParameters(Node *node);

    void walkSealCallExpression(Node *node, const std::string &sealName);

    // HELPER FUNCTIONS
    void registerWalkerFunctions();
    void enforceDeclarationRules(VariableDeclaration *declaration,
                                 const std::shared_ptr<SymbolInfo> &declInfo);
    void handleNullInitializers(VariableDeclaration *declaration,
                                const std::shared_ptr<SymbolInfo> &declInfo,
                                const std::shared_ptr<SymbolInfo> &nullInfo);
    ResolvedType inferInfixExpressionType(Node *node);
    ResolvedType inferPrefixExpressionType(Node *node);
    ResolvedType inferPostfixExpressionType(Node *node);
    ResolvedType resultOfBinary(TokenType operatorType, ResolvedType leftType,
                                ResolvedType rightType, Node *expr);
    ResolvedType resultOfUnary(TokenType operatorType, const ResolvedType &oprendType, Node *node);
    ResolvedType tokenTypeToResolvedType(Token token, bool isNullable);
    std::shared_ptr<SymbolInfo> resultOfScopeOrDot(TokenType operatorType,
                                                   const ResolvedType &parentType,
                                                   const std::string &childName,
                                                   InfixExpression *infix);
    ResolvedType resolveTypeWithModifier(Node *modifier, const ResolvedType &base);
    ResolvedType makePointerType(const ResolvedType &inner, bool isNull);
    ResolvedType makeRefType(const ResolvedType &inner, bool isNull);
    ResolvedType makeArrayType(const ResolvedType &inner, uint64_t size, bool isNull);
    ResolvedType *resolveSelfChain(SelfExpression *selfExpr, const std::string &componentName);
    bool isTypeCompatible(const ResolvedType &expected, const ResolvedType &actual);
    bool isIdentInSelf(SelfExpression *selfExpr,Identifier *target);
    int inferLiteralDimensions(ArrayLiteral *arrLit);
    void inferSizePerDimension(ArrayLiteral *lit, std::vector<int64_t> &sizes);
    void substituteTypes(Node *node, std::unordered_map<std::string, Token> &subMap);
    void mangleGenericName(Node *node, std::string aliasName);
    void checkOperatorStyle(TokenType op, bool isPointer, const std::string &name, Node *site);
    void checkMutability(const SymbolInfo &sym, const std::string &name, Node *site);
    bool checkTypeCompatible(ResolvedType lhs, ResolvedType rhs, bool rhsIsNull,
                             const std::string &name, Node *site);
    bool handleNullRhs(NullLiteral *nullLit, const std::shared_ptr<SymbolInfo> &lhsSym,
                       const std::string &name, AssignmentStatement *assignStmt);
    void giveGenericLiteralContext(Node *literal, const ResolvedType &contextType,
                                   const std::shared_ptr<SymbolInfo> &litSym);

    void importSeals();
    void importComponents();
    void importComponentInits();
    void importRecords();
    void importEnums();
    void importAllocators();
    void importFunctions();
    void importVariables();
    void importGenerics();
    void import();

    void registerInbuiltAllocatorTypes();
    bool isGlobalScope();
    AllocatorRole getFunctionRole(const std::vector<std::unique_ptr<Statement>> &params,
                                  Expression *returnType, const std::string &funcName);
    bool areSignaturesCompatible(const SymbolInfo &declInfo, FunctionExpression *funcExpr);
    bool signaturesMatchBehaviorDeclaration(const std::shared_ptr<MemberInfo> &declMember,
                                            FunctionExpression *funcExpr);
    bool checkParamListCompatibility(
        const std::vector<std::pair<ResolvedType, std::string>> &expectedParams,
        const std::vector<std::unique_ptr<Statement>> &actualParams);

    bool isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr);
    bool isMethodCallCompatible(const MemberInfo &memFuncInfo, CallExpression *callExpr);
    bool isLiteral(Node *node);
    bool isGenericIntOrFloat(Node *node);
    bool rhsIsHeap(Node *node);
    void popScope();
    void registerLiteral(Node *literal, const ResolvedType &type);
    Node *getCurrentBlock();
    std::string getTerminatorString(Node *node);
    std::string generateLifetimeID(const std::shared_ptr<SymbolInfo> &sym);
    std::unique_ptr<LifeTime> createLifeTimeTracker(Node *declarationNode, Node *initializer,
                                                    const std::shared_ptr<SymbolInfo> &declSym);
    void transferBaton(Node *receiver, const std::string &familyID);
    void logSemanticErrors(const std::string &message, Node *contextNode);
    void logSpecialErrors(const std::string &message, int line, int col);
    void reportDevBug(const std::string &message, Node *contextNode);
    void logInternal(const std::string &message);
};
