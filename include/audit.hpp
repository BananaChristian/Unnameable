#pragma once
#include "errors.hpp"
#include "semantics.hpp"
#include <typeindex>

class Auditor {
  Semantics &semantics;
  ErrorHandler &errorHandler;
  bool hasFailed = false;
  bool verbose = false;

public:
  Auditor(Semantics &semantics, ErrorHandler &handler, bool verbose);
  void audit(Node *node);
  bool failed();

private:
  using auditFn = void (Auditor::*)(Node *node);
  std::unordered_map<std::type_index, auditFn> auditFnsMap;
  void registerAuditorFunctions();

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

  // Statement auditors
  void auditReturnStatement(Node *node);

  // Helpers
  void logAuditError(const std::string &message, Node *contextNode);
  void reportDevBug(const std::string &message, Node *contextNode);
  void logInternal(const std::string &message);
};
