#pragma once
#include "ast.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include <typeindex>
#include <unordered_map>

struct TemporalInfo {
  bool insideBranch = false;
};

class Auditor {
  Semantics &semantics;
  ErrorHandler &errorHandler;
  bool hasFailed = false;
  bool verbose = false;

public:
  Auditor(Semantics &semantics, ErrorHandler &handler, bool verbose);
  void audit(Node *node);
  bool failed();

  std::unordered_map<Node *, std::vector<std::unique_ptr<LifeTime>>>
      leakedDeputiesBag;

private:
  using auditFn = void (Auditor::*)(Node *node);
  std::unordered_map<std::type_index, auditFn> auditFnsMap;
  std::unordered_map<std::string, std::vector<std::string>> candidateRegistry;
  std::unordered_map<Node *, std::shared_ptr<TemporalInfo>> temporaryData;

  void registerAuditorFunctions();
  bool isInsideBranch = true;

  // Variable declaration auditors
  void auditHeapStatement(Node *node);
  void auditLetStatement(Node *node);
  void auditPointerStatement(Node *node);

  // Function auditors
  void auditFunctionStatement(Node *node);
  void auditFunctionExpression(Node *node);

  // Block auditors
  void auditBlockStatement(Node *node);
  void auditBlockExpression(Node *node);

  // Expression auditors
  void auditIdentifier(Node *node);

  // Statement auditors
  void auditAssignmentStatement(Node *node);
  void auditReturnStatement(Node *node);
  void auditTraceStatement(Node *node);
  void auditIfStatement(Node *node);
  void auditElifStatement(Node *node);

  // Helpers
  void transferDependent(const std::string &dependentID,
                         const std::shared_ptr<SymbolInfo> &dependentSym,
                         bool dropCount);
  bool containsNaturalDeath(Node *branchNode, const std::string &targetID);
  void performSuperpositionSync(ifStatement *ifStmt);
  void reconcileWithSnapshots(ifStatement *ifStmt, const std::string &id);
  void materializeDeputy(const std::string &id, const BatonStateSnapshot &snap,
                         Node *branchRoot);
  std::set<std::string> getAllActiveBatonIDs();
  bool containsNode(Node *root, Node *target);
  void simulateFree(const std::string &contextID);

  // Loggers and reporters
  void logAuditError(const std::string &message, Node *contextNode);
  void reportDevBug(const std::string &message, Node *contextNode);
  void logInternal(const std::string &message);
};
