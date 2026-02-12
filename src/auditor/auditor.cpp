#include "ast.hpp"
#include "audit.hpp"
#include "errors.hpp"
#include "semantics.hpp"
Auditor::Auditor(Semantics &semantics, ErrorHandler &errorHandler,
                 bool isVerbose)
    : semantics(semantics), errorHandler(errorHandler), verbose(isVerbose) {
  registerAuditorFunctions();
}

// Registration of the mini auditors
void Auditor::registerAuditorFunctions() {
  auditFnsMap[typeid(HeapStatement)] = &Auditor::auditHeapStatement;
  auditFnsMap[typeid(LetStatement)] = &Auditor::auditLetStatement;
  auditFnsMap[typeid(PointerStatement)] = &Auditor::auditPointerStatement;

  auditFnsMap[typeid(FunctionStatement)] = &Auditor::auditFunctionStatement;
  auditFnsMap[typeid(FunctionExpression)] = &Auditor::auditFunctionExpression;
  auditFnsMap[typeid(ReturnStatement)] = &Auditor::auditReturnStatement;

  auditFnsMap[typeid(BlockStatement)] = &Auditor::auditBlockStatement;
  auditFnsMap[typeid(BlockExpression)] = &Auditor::auditBlockExpression;
}

void Auditor::audit(Node *node) {
  auto auditIt = auditFnsMap.find(typeid(*node));
  logInternal("[Dispatch node]:" + node->toString());
  if (auditIt == auditFnsMap.end()) {
    return;
  }
  (this->*auditIt->second)(node);
}
void Auditor::auditHeapStatement(Node *node) {
  auto heapStmt = dynamic_cast<HeapStatement *>(node);
  if (!heapStmt)
    return;

  audit(heapStmt->stmt.get());
}
void Auditor::auditLetStatement(Node *node) {}

void Auditor::auditPointerStatement(Node *node) {
  logInternal("Auditing pointer statement");
  auto ptrStmt = dynamic_cast<PointerStatement *>(node);
  if (!ptrStmt)
    return;

  auto ptrSym = semantics.getSymbolFromMeta(ptrStmt);
  if (!ptrSym)
    return;
  // First check the value's baton
  bool ValueDoesntDieHere = false;
  if (ptrStmt->value) {
    auto valSym = semantics.getSymbolFromMeta(ptrStmt->value.get());
    if (!valSym)
      return;
    Node *valBatonHolder = semantics.queryForLifeTimeBaton(valSym->ID);
    if (valBatonHolder != ptrStmt->value.get()) {
      auto &valBaton = semantics.responsibilityTable[valBatonHolder];
      ValueDoesntDieHere = true;

      // Get the baton for p's family wherever it is rightnow
      Node *ptrBatonHolder = semantics.queryForLifeTimeBaton(ptrSym->ID);
      auto &ptrBaton = semantics.responsibilityTable[ptrBatonHolder];
      // So the last use of the value wasnt here that means semantics cooked us
      // reverse its mess
      if (ValueDoesntDieHere) {
        logInternal("The value doesnt die here so remove the job of freeing "
                    "from this pointer");
        ptrBaton->dependents.erase(valSym->ID);
        // Rearm that baton
        valBaton->isResponsible = true;
      }
    }
  }
}

void Auditor::auditFunctionStatement(Node *node) {
  auto funcStmt = dynamic_cast<FunctionStatement *>(node);
  if (!funcStmt)
    return;

  audit(funcStmt->funcExpr.get());
}

void Auditor::auditFunctionExpression(Node *node) {
  auto funcExpr = dynamic_cast<FunctionExpression *>(node);
  if (!funcExpr)
    return;

  audit(funcExpr->block.get());
}

void Auditor::auditReturnStatement(Node *node) {
  auto retStmt = dynamic_cast<ReturnStatement *>(node);
  if (!retStmt)
    return;

  auto retSym = semantics.getSymbolFromMeta(retStmt);

  logInternal("Return statement ID: " + retSym->ID);

  const auto &valName =
      semantics.extractIdentifierName(retStmt->return_value.get());

  Node *batonHolder = semantics.queryForLifeTimeBaton(retSym->ID);
  if (!batonHolder) {
    logInternal("Failed to get baton holder");
    return;
  }
  // If the return statement isnt the baton holder this is an escape
  if (batonHolder != retStmt) {
    logInternal("The baton escaped");
    auto &baton = semantics.responsibilityTable[batonHolder];
    if (!baton) {
      logInternal("Failed to get baton yet holder exists");
      return;
    }

    logInternal("The baton has '" + std::to_string(baton->dependents.size()) +
                "' dependents");
    if (baton && !baton->dependents.empty()) {
      logInternal("The dependents exist attempting to block this return");
      errorHandler.addHint(
          "The compiler cannot allow the escape of a variable that is "
          "extending the lifetime of other variables as if it frees them and "
          "allows your pointer to escape then you will have a dangling "
          "pointer");
      logAuditError("The return value '" + valName +
                        "' cannot escape with hidden dependecies",
                    retStmt->return_value.get());
    }
  }
}

void Auditor::auditBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt)
    return;

  for (const auto &stmt : blockStmt->statements) {
    audit(stmt.get());
  }
}

void Auditor::auditBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr)
    return;

  if (!blockExpr->statements.empty()) {
    for (const auto &stmt : blockExpr->statements) {
      audit(stmt.get());
    }
  }

  if (blockExpr->finalexpr.has_value()) {
    audit(blockExpr->finalexpr.value().get());
  }
}

// Helpers
void Auditor::logInternal(const std::string &message) {
  if (verbose) {
    std::cout << message << "\n";
  }
}

void Auditor::logAuditError(const std::string &message, Node *contextNode) {
  hasFailed = true;

  auto line = 0;
  auto col = 0;
  if (contextNode) {
    line = contextNode->token.line;
    col = contextNode->token.column;
  }

  CompilerError error;
  error.level = ErrorLevel::AUDITOR;
  error.line = line;
  error.col = col;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);
}

void Auditor::reportDevBug(const std::string &message, Node *contextNode) {
  auto line = 0;
  auto col = 0;
  if (contextNode) {
    line = contextNode->token.line;
    col = contextNode->token.column;
  }

  CompilerError error;
  error.level = ErrorLevel::INTERNAL;
  error.line = line;
  error.col = col;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);

  std::abort();
}

bool Auditor::failed() { return hasFailed; }
