#pragma once
#include <set>
#include <typeindex>
#include <unordered_map>
#include <utility>

#include "ast.hpp"
#include "errors.hpp"
#include "semantics.hpp"

struct BlockInfo {
    std::vector<std::string> natives;
    std::vector<std::string> foreigners;
};

struct CycleGroup {
    std::vector<std::string> ids;
    bool hasNative = false;
    bool hasForeigner = false;
};

class Auditor {
    Semantics &semantics;
    ErrorHandler &errorHandler;
    bool hasFailed = false;
    bool verbose = false;

   public:
    Auditor(Semantics &semantics, ErrorHandler &handler, bool verbose);
    void audit(Node *node);
    void runClassifier(Node *node);
    bool failed();
    std::unordered_map<
        Node *, std::vector<std::pair<std::unique_ptr<LifeTime>, std::shared_ptr<SymbolInfo>>>>
        foreignersToFree;
    std::unordered_map<
        Node *, std::vector<std::pair<std::unique_ptr<LifeTime>, std::shared_ptr<SymbolInfo>>>>
        nativesToFree;
    std::unordered_map<Node *, std::unique_ptr<BlockInfo>> deferedFrees;

   private:
    using auditFn = void (Auditor::*)(Node *node);
    std::unordered_map<std::type_index, auditFn> auditFnsMap;
    std::unordered_map<std::string, std::vector<std::string>> candidateRegistry;
    std::unordered_map<std::string, int> usageMap;
    std::set<std::string> bunkeredIDs;
    std::vector<Node *> activeBlocks;
    std::vector<std::string> cycle;
    int currentStmtIdx = 0;
    bool inhibit = false;

    void registerAuditorFunctions();
    // Variable declaration auditors
    void auditVariableDeclaration(Node *node);

    // Function auditors
    void auditFunctionStatement(Node *node);
    void auditFunctionExpression(Node *node);

    // Block auditors
    void auditBlockStatement(Node *node);
    void auditBlockExpression(Node *node);

    // Expression auditors
    void auditInfix(Node *node);
    void auditIdentifier(Node *node);
    void auditFStringLiteral(Node *node);

    // Statement auditors
    void auditAssignmentStatement(Node *node);
    void auditFieldAssignmentStatement(Node *node);
    void auditReturnStatement(Node *node);
    void auditTraceStatement(Node *node);
    void auditIfStatement(Node *node);
    void auditElifStatement(Node *node);
    void auditCaseStatement(Node *node);
    void auditSwitchStatement(Node *node);
    void auditASMStatement(Node *node);

    void auditWhileStatement(Node *node);
    void auditForStatement(Node *node);
    void auditExpressionStatement(Node *node);
    void auditAddressExpression(Node *node);
    void auditDereferenceExpression(Node *node);
    void auditArraySubscriptExpression(Node *node);
    void auditCallExpression(Node *node);

    // Helpers
    void classifyNode(Node *node);
    void classifyBlock(Node *block);
    void classifySwitch(SwitchStatement *sw);
    void classifyClause(SwitchStatement *sw, Node *stmt,
                        const std::vector<std::unique_ptr<Statement>> &clause,
                        const std::unique_ptr<BlockInfo> &blockInfo);
    void classifySymbol(Node *node, Node *block, BlockInfo *info);
    Node *peelNode(Node *node);
    bool hasDisruptors(Node *block);
    bool shouldForeignBunkerBlock(Node *block);
    bool shouldNativeBunkerBlock(Node *block);
    void transferDependent(const std::string &dependentID,
                           const std::shared_ptr<SymbolInfo> &dependentSym, bool dropCount);
    void assignmentRob(Node *contextAssign, const std::shared_ptr<SymbolInfo> &assignSym);
    bool isCycle(LifeTime *baton);
    bool followDependents(LifeTime *current, const std::string &originalID,
                          std::set<std::string> &visited);
    bool diesInBlock(const std::string &ID, Node *block);
    bool doesSymbolDieInSwitch(const std::string &id,
                               const std::vector<std::unique_ptr<Statement>> &clause);
    std::set<std::string> getAllActiveBatonIDs();
    Node *getCurrentBlock();
    std::vector<std::string> getAllActiveBatonIDsInBlock(Node *block);
    void scanForBaton(const std::string &id);
    const CycleGroup findCycleGroup(const std::unique_ptr<BlockInfo> &blockInfo);
    void checkCycleSafety(const std::unique_ptr<BlockInfo> &blockInfo, Node *contextNode);
    std::string getIDsString(const std::vector<std::string> &ids,
                             const std::vector<std::string> &filter);

    void bunkerNatives(Node *block);
    void bunkerForeigners(Node *block);
    void bunkerCycles(Node *block);
    void bunkerPersists(Node *block);
    void bunkerNativeHeists(Node *block);
    void bunkerForeignHeists(Node *block);
    std::vector<std::string> sortHeists(const std::unique_ptr<BlockInfo> &info,bool isForeign);

    bool isBunkered(const std::string &id);
    bool isAlreadyClassified(const std::string &id, BlockInfo *blockInfo);
    bool isBlock(Node *node);
    bool checkBirth(const std::string &id, const std::vector<std::unique_ptr<Statement>> &clause);
    bool isDeclaration(Node *node);

    bool containsNode(Node *root, Node *target);
    void buildUsageMap(BlockExpression *block);
    bool isUsedDownwards(const std::string &ID);
    void rearmBaton(const std::string &ID, Node *robber);
    void simulateDeclFree(VariableDeclaration *declaration, const std::string &contextID);
    void simulateFree(Node *contextNode, const std::string &contextID);
    void freeCore(LifeTime *baton,const std::shared_ptr<SymbolInfo> &contextSym,Node *holderNode);
    void interceptHeist(Node *contextNode);

    // Loggers and reporters
    void logAuditError(const std::string &message, Node *contextNode);
    void reportDevBug(const std::string &message, Node *contextNode);
    void logInternal(const std::string &message);
};
