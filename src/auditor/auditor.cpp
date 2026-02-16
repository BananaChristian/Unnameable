#include "ast.hpp"
#include "audit.hpp"
#include "errors.hpp"
#include "semantics.hpp"

#include <memory>
#include <string>
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

  auditFnsMap[typeid(AssignmentStatement)] = &Auditor::auditAssignmentStatement;
  auditFnsMap[typeid(ifStatement)] = &Auditor::auditIfStatement;
}

void Auditor::audit(Node *node) {
  auto auditIt = auditFnsMap.find(typeid(*node));
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
  auto ptrStmt = dynamic_cast<PointerStatement *>(node);
  if (!ptrStmt)
    return;

  auto ptrSym = semantics.getSymbolFromMeta(ptrStmt);
  if (!ptrSym)
    return;

  if (!ptrSym->isHeap) {
    logInternal("Pointer statement is not heap raised not tracking");
    return;
  }

  logInternal("\n[AUDIT] >>> PointerStatement: " + ptrSym->ID);

  if (ptrStmt->value) {
    auto valSym = semantics.getSymbolFromMeta(ptrStmt->value.get());
    if (!valSym)
      return;

    Node *valBatonHolder = semantics.queryForLifeTimeBaton(valSym->ID);
    Node *ptrBatonHolder = semantics.queryForLifeTimeBaton(ptrSym->ID);

    logInternal("  [VALUE] ID: " + valSym->ID +
                " | Holder Node: " + (valBatonHolder ? "VALID" : "NULL"));
    logInternal("  [POINTER] ID: " + ptrSym->ID +
                " | Holder Node: " + (ptrBatonHolder ? "VALID" : "NULL"));

    if (valBatonHolder && valBatonHolder != ptrStmt->value.get()) {
      auto &valBaton = semantics.responsibilityTable[valBatonHolder];
      auto &ptrBaton = semantics.responsibilityTable[ptrBatonHolder];

      logInternal("  [REVERSE-LOGIC] Detected non-death use. Correcting baton "
                  "states...");
      logInternal("    Before: Ptr(" + ptrSym->ID +
                  ") Deps: " + std::to_string(ptrBaton->dependents.size()));

      // The "Undo"
      if (ptrBaton->dependents.count(valSym->ID)) {
        ptrBaton->dependents.erase(valSym->ID);
        logInternal("    Action: Erased " + valSym->ID + " from Ptr " +
                    ptrSym->ID);
      } else {
        logInternal("    Warning: " + valSym->ID +
                    " was NOT in Ptr dependents. Exclusivity might be broken.");
      }

      valBaton->isResponsible = true;
      logInternal("    Action: Rearmed ValBaton " + valBaton->ID +
                  " (isResponsible = TRUE)");
    }
  }

  logInternal("  [FINISHING] Calling simulateFree for: " + ptrSym->ID);
  auto ptrInfo = std::make_shared<TemporalInfo>();
  ptrInfo->insideBranch = isInsideBranch;
  temporaryData[ptrStmt] = ptrInfo;
  simulateFree(ptrSym->ID);
}

void Auditor::auditAssignmentStatement(Node *node) {
  auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
  if (!assignStmt)
    return;

  auto assignSym = semantics.getSymbolFromMeta(assignStmt);
  if (!assignSym->isHeap)
    return;

  auto valSym = semantics.getSymbolFromMeta(assignStmt->value.get());

  logInternal("\n[AUDIT] >>> Assignment: " + assignSym->ID + " = " +
              (valSym ? valSym->ID : "NULL"));

  if (assignSym && valSym) {
    if (assignSym->isPointer) {
      valSym->pointerCount++;
      logInternal("  [COUNT] Incrementing pointerCount for " + valSym->ID +
                  " to " + std::to_string(valSym->pointerCount));
    }

    if (valSym->isHeap) {
      Node *valBatonHolder = semantics.queryForLifeTimeBaton(valSym->ID);
      if (valBatonHolder == assignStmt->value.get()) {
        auto &valBaton = semantics.responsibilityTable[valBatonHolder];

        logInternal(
            "  [HEIST-CHECK] ValBaton " + valBaton->ID +
            " isResponsible: " + (valBaton->isResponsible ? "YES" : "NO"));

        if (valBaton->isResponsible) {
          Node *currentBatonHolder =
              semantics.queryForLifeTimeBaton(assignSym->ID);
          auto &currentBaton =
              semantics.responsibilityTable[currentBatonHolder];

          logInternal("  [ACTION] Attempting Robbery: " + currentBaton->ID +
                      " robbing " + valBaton->ID);
          semantics.transferResponsibility(currentBaton.get(), valBaton.get(),
                                           valSym);
        } else {
          logInternal("  [ACTION] Already robbed. Registering " +
                      assignSym->ID + " as candidate for " + valSym->ID);
          candidateRegistry[valBaton->ID].push_back(assignSym->ID);
        }
      } else {
        logInternal("  [BYPASS] valBatonHolder is not current value node. No "
                    "robbery possible here.");
      }
    }
  }

  auto assignInfo = std::make_shared<TemporalInfo>();
  assignInfo->insideBranch = isInsideBranch;
  simulateFree(assignSym->ID);
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

void Auditor::auditElifStatement(Node *node) {
  auto elifStmt = dynamic_cast<elifStatement *>(node);
  if (!elifStmt)
    return;
}

void Auditor::auditIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  // The superposition pass
  performSuperpositionSync(ifStmt);

  // The normal standard pass
  audit(ifStmt->if_result.get());
  for (const auto &elif : ifStmt->elifClauses) {
    audit(elif.get());
  }
  if (ifStmt->else_result.has_value() && ifStmt->else_result.value()) {
    audit(ifStmt->else_result.value().get());
  }
}

void Auditor::auditReturnStatement(Node *node) {
  auto retStmt = dynamic_cast<ReturnStatement *>(node);
  if (!retStmt)
    return;

  auto retSym = semantics.getSymbolFromMeta(retStmt);
  if (!retSym->isHeap)
    return;

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

  auto retInfo = std::make_shared<TemporalInfo>();
  retInfo->insideBranch = isInsideBranch;
  simulateFree(retSym->ID);
}

void Auditor::auditIdentifier(Node *node) {
  auto ident = dynamic_cast<Identifier *>(node);
  if (!ident)
    return;

  auto identSym = semantics.getSymbolFromMeta(ident);
  simulateFree(identSym->ID);
}

void Auditor::auditTraceStatement(Node *node) {
  auto traceStmt = dynamic_cast<TraceStatement *>(node);
  if (!traceStmt)
    return;

  auto traceSym = semantics.getSymbolFromMeta(traceStmt);

  audit(traceStmt->expr.get());

  simulateFree(traceSym->ID);
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
bool Auditor::containsNaturalDeath(Node *branchRoot,
                                   const std::string &targetID) {
  if (!branchRoot)
    return false;

  logInternal("[SCOUT] Investigating branch: " + branchRoot->toString());

  // Get the Snapshot for THIS specific branch root.
  auto it = semantics.snapshotRegistry.find(branchRoot);
  if (it == semantics.snapshotRegistry.end() || it->second.empty()) {
    logInternal("[SCOUT] No snapshot found for branch. Skipping...");
    return false;
  }

  // We take the last frame of the branch (the exit state)
  const auto &exitSnapshot = it->second.back();

  // Find the baton's status in this specific timeline
  auto snapIt = exitSnapshot.batonStates.find(targetID);
  if (snapIt == exitSnapshot.batonStates.end()) {
    logInternal("[SCOUT] Baton " + targetID + " not found in branch snapshot.");
    return false;
  }

  Node *branchTerminalNode = snapIt->second.terminalNode;

  logInternal("[SCOUT] Snapshot terminal node for " + targetID + " is: " +
              (branchTerminalNode ? branchTerminalNode->toString() : "NULL"));

  if (containsNode(branchRoot, branchTerminalNode)) {
    logInternal("[SCOUT] Found NATURAL DEATH for " + targetID +
                " inside branch at: " + branchTerminalNode->toString());
    return true;
  }

  logInternal("[SCOUT] Baton " + targetID +
              " survives this branch (Terminal node is outside).");
  return false;
}

std::set<std::string> Auditor::getAllActiveBatonIDs() {
  logInternal(
      "[ACTIVE_SCAN] Identifying all live batons in responsibilityTable...");
  std::set<std::string> activeIDs;

  for (const auto &[node, baton] : semantics.responsibilityTable) {
    if (!baton)
      continue;

    // Log every baton we encounter to see what's in the table
    logInternal("[ACTIVE_SCAN] Inspecting Table Entry: Node(" +
                node->toString() + ") -> BatonID(" + baton->ID + ")");

    if (baton->isResponsible) {
      Node *holder = semantics.queryForLifeTimeBaton(baton->ID);
      if (!holder) {
        logInternal("[ACTIVE_SCAN] ! ERROR: No holder found for ID: " +
                    baton->ID);
        continue;
      }

      auto holderSym = semantics.getSymbolFromMeta(holder);
      if (!holderSym) {
        logInternal(
            "[ACTIVE_SCAN] ! ERROR: No symbol metadata for holder of: " +
            baton->ID);
        continue;
      }

      logInternal("[ACTIVE_SCAN] Checking Stats for " + baton->ID +
                  ": isResponsible(" + std::to_string(baton->isResponsible) +
                  ") refCount(" + std::to_string(holderSym->refCount) +
                  ") ptrCount(" + std::to_string(holderSym->pointerCount) +
                  ")");

      logInternal("[ACTIVE_SCAN] Found LIVE baton: " + baton->ID);
      activeIDs.insert(baton->ID);
    }
  }

  if (activeIDs.empty()) {
    logInternal("[ACTIVE_SCAN] FATAL: Finished scan, found 0 active IDs.");
  } else {
    logInternal("[ACTIVE_SCAN] Scan complete. Total Active IDs: " +
                std::to_string(activeIDs.size()));
  }

  return activeIDs;
}

void Auditor::performSuperpositionSync(ifStatement *ifStmt) {
  logInternal("[SYNC] Starting Superposition Check for IfStatement: " +
              ifStmt->toString());
  std::set<std::string> allActiveIDs = getAllActiveBatonIDs();

  if (allActiveIDs.empty())
    logInternal("Active ID list is empty");

  for (const std::string &id : allActiveIDs) {
    logInternal("[SYNC] Investigating Baton: " + id);
    bool diesInThen = containsNaturalDeath(ifStmt->if_result.get(), id);

    bool diesInAnyElif = false;
    for (auto &elifs : ifStmt->elifClauses) {
      auto elifStmt = dynamic_cast<elifStatement *>(elifs.get());
      if (containsNaturalDeath(elifStmt->elif_result.get(), id)) {
        diesInAnyElif = true;
        break;
      }
    }

    // Does it die in the 'Else' block?
    bool diesInElse = false;
    if (ifStmt->else_result.has_value()) {
      diesInElse = containsNaturalDeath(ifStmt->else_result.value().get(), id);
    }

    bool diesSomewhere = diesInThen || diesInAnyElif || diesInElse;
    bool survivesSomewhere =
        !diesInThen || (ifStmt->elifClauses.size() > 0 && !diesInAnyElif) ||
        (ifStmt->else_result.has_value() && !diesInElse);

    if (diesSomewhere && survivesSomewhere) {
      logInternal("[SYNC] DISCREPANCY DETECTED for " + id);
      reconcileWithSnapshots(ifStmt, id);
    } else {
      logInternal("[SYNC] " + id + " state is consistent across all branches.");
    }
  }
}

void Auditor::reconcileWithSnapshots(ifStatement *ifStmt,
                                     const std::string &id) {

  logInternal("[RECONCILE] Investigating timelines for baton: " + id);

  // Find where the baton ended up in the Global Reality
  Node *globalHolder = semantics.queryForLifeTimeBaton(id);
  if (!globalHolder) {
    logInternal("[RECONCILE] Baton " + id +
                " is not in the live table. Likely already dead.");
    return;
  }

  logInternal("[RECONCILE] The baton holder for id '" + id +
              "' is: " + globalHolder->toString());

  // Helper to process a specific branch node
  auto reconcileBranch = [&](Node *branchRoot) {
    if (!branchRoot)
      return;

    auto it = semantics.snapshotRegistry.find(branchRoot);
    if (it == semantics.snapshotRegistry.end()) {
      // If no snapshot exists for an existing branch, it's a gap.
      logInternal("[RECONCILE] No snapshot for branch: " +
                  branchRoot->toString());
      return;
    }

    // We take the last snapshot (the "Exit Frame" from the end of the branch)
    const auto &snapshot = it->second.back();
    auto snapIt = snapshot.batonStates.find(id);

    if (snapIt != snapshot.batonStates.end()) {
      Node *branchTerminal = snapIt->second.terminalNode;
      if (!branchTerminal) {
        logInternal("[RECONCILE] No terminal node found for branch: " +
                    branchRoot->toString());
        return;
      }

      logInternal(
          "[RECONCILE] Terminal node for branch: " + branchRoot->toString() +
          " is: " + branchTerminal->toString());

      if (branchTerminal != globalHolder) {
        logInternal("[RECONCILE] Branch " + branchRoot->toString() +
                    " is passive for " + id);
        materializeDeputy(id, snapIt->second, branchRoot);
      } else {
        logInternal("[RECONCILE] Branch " + branchRoot->toString() +
                    " is the active for " + id);
      }
    }
  };

  // Handling the then block
  reconcileBranch(ifStmt->if_result.get());

  // Handle the elifs
  for (const auto &elif : ifStmt->elifClauses) {
    if (elif) {
      auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
      reconcileBranch(elifStmt->elif_result.get());
    }
  }

  // Handle else
  if (ifStmt->else_result.has_value() && ifStmt->else_result.value()) {
    reconcileBranch(ifStmt->else_result.value().get());
  } else {
    logInternal("[RECONCILE] Handling implicit 'else' leak for " + id);
    // Create a dummy snapshot
    BatonStateSnapshot implicitSnap;
    implicitSnap.id = id;
    implicitSnap.terminalNode = ifStmt;
    implicitSnap.isResponsible = true;

    materializeDeputy(id, implicitSnap, ifStmt);
  }
}

void Auditor::materializeDeputy(const std::string &id,
                                const BatonStateSnapshot &snap,
                                Node *branchRoot) {
  logInternal("[MATERIALIZE] Creating Death Warrant for " + id + " on node " +
              snap.terminalNode->toString());

  bool isLegalTarget = containsNode(branchRoot, snap.terminalNode);

  auto deputy = std::make_unique<LifeTime>();
  deputy->ID = id;
  deputy->isResponsible = true;
  deputy->isAlive = false;
  deputy->dependents = snap.dependents;
  deputy->ownedBy = snap.ownedBy;

  if (isLegalTarget) {
    if (snap.terminalNode) {
      logInternal("[MATERIALIZE] Restoring baton to holder: " +
                  snap.terminalNode->toString() +
                  " inside branch: " + branchRoot->toString());
      semantics.responsibilityTable[snap.terminalNode] = std::move(deputy);
    } else {
      logInternal(
          "[MATERIALIZE] No potential holder found in branch. Using Bag for " +
          branchRoot->toString());
      leakedDeputiesBag[branchRoot].push_back(std::move(deputy));
    }
  } else {
    logInternal("Terminal node: " + snap.terminalNode->toString() +
                " not inside branch: " + branchRoot->toString() +
                " vetoing to deputy leak...");
    leakedDeputiesBag[branchRoot].push_back(std::move(deputy));
  }
}

bool Auditor::containsNode(Node *root, Node *target) {
  if (!root || !target)
    return false;
  if (root == target)
    return true;

  if (auto block = dynamic_cast<BlockStatement *>(root)) {
    for (auto &stmt : block->statements) {
      if (containsNode(stmt.get(), target))
        return true;
    }

  } else if (auto ifStmt = dynamic_cast<ifStatement *>(root)) {
    if (containsNode(ifStmt->condition.get(), target))
      return true;
    if (containsNode(ifStmt->if_result.get(), target))
      return true;
    for (const auto &elif : ifStmt->elifClauses) {
      if (containsNode(elif.get(), target))
        return true;
      continue;
    }
    if (ifStmt->else_stmt.has_value()) {
      if (containsNode(ifStmt->else_result.value().get(), target))
        return true;
    }

  } else if (auto infix = dynamic_cast<InfixExpression *>(root)) {
    if (containsNode(infix->left_operand.get(), target))
      return true;
    if (containsNode(infix->right_operand.get(), target))
      return true;
  }

  return false;
}

void Auditor::simulateFree(const std::string &contextID) {
  logInternal("\n  [SIMULATE-FREE] Target: " + contextID);

  Node *holderNode = semantics.queryForLifeTimeBaton(contextID);
  if (!holderNode) {
    logInternal("    [ERROR] No holder node found for " + contextID);
    return;
  }

  auto &baton = semantics.responsibilityTable[holderNode];
  auto contextSym = semantics.getSymbolFromMeta(holderNode);

  logInternal("    Baton ID: " + baton->ID);
  logInternal("    State: isResponsible=" +
              std::string(baton->isResponsible ? "T" : "F") +
              ", isAlive=" + std::string(baton->isAlive ? "T" : "F") +
              ", ptrCount=" + std::to_string(contextSym->pointerCount));

  if (contextSym->pointerCount == 0 && contextSym->refCount == 0) {
    if (baton->isResponsible && baton->isAlive) {
      logInternal("    [DEATH] Condition met. Killing family: " + contextID);
      baton->isAlive = false;

      bool dropCount = (contextSym->isPointer && contextSym->isHeap);
      logInternal("    Dependents to process: " +
                  std::to_string(baton->dependents.size()));

      for (const auto &[id, depSym] : baton->dependents) {
        logInternal("    [TRANSFER] Dependent " + id +
                    " (dropCount=" + (dropCount ? "T" : "F") + ")");
        transferDependent(id, depSym, dropCount);
      }
    } else {
      logInternal(
          "    [STAY-ALIVE] Family is already dead or not responsible.");
    }
  } else {
    logInternal("    [STAY-ALIVE] References/Pointers still exist (" +
                std::to_string(contextSym->pointerCount) + ")");
  }
}

void Auditor::transferDependent(const std::string &dependentID,
                                const std::shared_ptr<SymbolInfo> &dependentSym,
                                bool dropCount) {
  logInternal("\n    [TRANSFER-LOGIC] Processing Dependent: " + dependentID);

  auto regIt = candidateRegistry.find(dependentID);
  if (regIt == candidateRegistry.end()) {
    logInternal("      [STATUS] Victim " + dependentID +
                " not in Candidate Registry.");
    if (dropCount) {
      dependentSym->pointerCount--;
      logInternal("      [ACTION] dropCount=TRUE. PointerCount for " +
                  dependentID +
                  " is now: " + std::to_string(dependentSym->pointerCount));
    }
    return;
  }

  bool adoptionPassed = false;
  auto &robbers = regIt->second;
  logInternal("      [REGISTRY] Found " + std::to_string(robbers.size()) +
              " potential robbers for this loot.");

  for (const auto &robberID : robbers) {
    logInternal("      [CHECKING ROBBER] ID: " + robberID);

    Node *robberBatonHolder = semantics.queryForLifeTimeBaton(robberID);
    if (!robberBatonHolder) {
      logInternal("        [SKIP] Robber " + robberID +
                  " has no baton holder node.");
      continue;
    }

    auto &robberBaton = semantics.responsibilityTable[robberBatonHolder];
    logInternal("        [STATE] isAlive: " +
                std::string(robberBaton->isAlive ? "YES" : "NO") +
                " | isResponsible: " +
                std::string(robberBaton->isResponsible ? "YES" : "NO"));

    if (robberBaton->isAlive) {
      adoptionPassed = true;

      if (robberBaton->isResponsible) {
        logInternal("        [ADOPTION] Robber " + robberID +
                    " is a MASTER. Taking loot directly.");
        robberBaton->dependents[dependentID] = dependentSym;
      } else {
        const std::string &masterID = robberBaton->ownedBy;
        logInternal("        [RELOCATION] Robber " + robberID +
                    " is a SLAVE. Passing loot to Master: " + masterID);

        Node *masterBatonHolder = semantics.queryForLifeTimeBaton(masterID);
        if (masterBatonHolder) {
          auto &masterBaton = semantics.responsibilityTable[masterBatonHolder];
          masterBaton->dependents[dependentID] = dependentSym;
          logInternal("        [SUCCESS] Loot " + dependentID +
                      " moved to Master " + masterID + " briefcase.");
        } else {
          logInternal("        [CRITICAL] Master " + masterID +
                      " exists in records but has no Baton Holder!");
        }
      }
      // Once one valid robber takes it, the dependent is saved for now.
      logInternal("      [SUCCESS] Adoption complete for " + dependentID);
      break;
    } else {
      logInternal("        [SKIP] Robber " + robberID + " is already dead.");
    }
  }

  if (!adoptionPassed) {
    logInternal("      [FINAL-JUDGMENT] No living heirs found for " +
                dependentID);
    if (dropCount) {
      dependentSym->pointerCount--;
      logInternal("      [ACTION] dropCount=TRUE. Final PointerCount for " +
                  dependentID + ": " +
                  std::to_string(dependentSym->pointerCount));
      if (dependentSym->pointerCount == 0) {
        logInternal("      [TERMINAL] " + dependentID +
                    " is now eligible for physical deallocation.");
      }
    }
  }
}

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
