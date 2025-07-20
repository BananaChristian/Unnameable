#pragma once
#include <memory>
#include <map>
#include <string>
#include <typeindex>
#include "ast.hpp"
#include "token.hpp"

#define CPPREST_FORCE_REBUILD

// Type system
enum class TypeSystem
{
    INTEGER,
    FLOAT,
    BOOLEAN,
    STRING,
    CHAR,
    VOID,
    COMPONENT,
    UNKNOWN,
    INVALID,
};

enum class SymbolKind{
    VARIABLE,
    FUNCTION,
};

// Meta data struct that will be attached to each node after semantic analysis
struct SemanticInfo
{
    TypeSystem nodeType = TypeSystem::UNKNOWN; // Info on the node's type
    std::string customTypeName="";
    bool isMutable = false;                    // Flag on the mutability of the node
    bool isConstant = false;                   // Flag on the node being constant at compile time
    int scopeDepth = -1;                       // Info on scope depth of the node
};

// Symbol that will be created per node and pushed to the symbol table
struct Symbol
{
    std::string nodeName;
    TypeSystem nodeType;
    std::vector<TypeSystem> parameterTypes;
    SymbolKind kind;
    bool isMutable;
    bool isConstant;
    bool initialized=false;
    int scopeDepth;
};

// The semantic analyser class
class Semantics
{
    std::unordered_map<Node *, SemanticInfo> annotations;             // Annotations map this will store the meta data per AST node
    std::vector<std::optional<TypeSystem>> returnTypeStack;
    std::vector<std::unordered_map<std::string, Symbol>> symbolTable; // This the symbol table which is a stack of hashmaps that will store info about the node during analysis
    std::unordered_map<std::string, std::vector<Symbol>> sharedDataBlocks;
    std::unordered_map<std::string,std::vector<Symbol>> sharedBehaviorBlocks;
    std::unordered_map<std::string,ComponentStatement*> components;

    ComponentStatement* currentComponent=nullptr;

public:
    Semantics();     // Semantics class analyzer

    void analyzer(Node *node); // The walker that will traverse the AST
    void preDeclareFunctions(std::vector<std::unique_ptr<Node>> &program);

    using analyzerFuncs = void (Semantics::*)(Node *);
    std::map<std::type_index, analyzerFuncs> analyzerFunctionsMap;

    //----------WALKER FUNCTIONS FOR DIFFERENT NODES---------
    void analyzeNewComponentExpression(Node *node);
    void analyzeComponentStatementBlock(Node *node);
    void analyzeInitConstructorStatement(Node *node);
    void analyzeFieldAccessExpression(Node *node);
    void analyzeUseStatement(Node *node);
    void analyzeDataStatementBlock(Node *node);
    void analyzeBehaviorStatementBlock(Node *node);
    void analyzeReturnStatement(Node *node);
    void analyzeFunctionStatement(Node *node);
    void analyzeFunctionExpression(Node *node);
    void analyzeIdentifierExpression(Node *node);
    void analyzeForStatement(Node *node);
    void analyzeWhileStatement(Node *node);
    void analyzeFunctionCallExpression(Node *node);
    void analyzeIfStatements(Node *node);
    void analyzeBlockStatements(Node *node);
    void analyzeBlockExpression(Node *node);
    void analyzeLetStatements(Node *node);
    void analyzeAssignmentStatement(Node *node);
    void analyzeExpressionStatement(Node *node);
    void analyzeInfixExpression(Node *node);
    void analyzeIntegerLiteral(Node *node);
    void analyzeFloatLiteral(Node *node);
    void analyzeStringLiteral(Node *node);
    void analyzeCharLiteral(Node *node);
    void analyzeErrorStatement(Node *node);
    void analyzeErrorExpression(Node *node);
    void analyzeBooleanLiteral(Node *node);

private:
    //---------HELPER FUNCTIONS----------
    void registerAnalyzerFunctions();
    void logError(const std::string &message, Node *node);
    Token getErrorToken(Node *node);
    TypeSystem resultOf(TokenType operatorType,TypeSystem leftType,TypeSystem rightType);
    TypeSystem resultOfUnary(TokenType operatorType,TypeSystem operandType);
    TypeSystem mapTypeStringToTypeSystem(const std::string &typeStr);
    TypeSystem inferExpressionType(Node *node);
    TypeSystem analyzeReturnTypeExpression(Node *node);
    bool blockAlwaysReturns(Node *block);
    std::string TypeSystemString(TypeSystem type);
    Symbol* resolveSymbol(const std::string& name);
    bool isConstantExpression(Node *node);
};
