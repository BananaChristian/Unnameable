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
  auditFnsMap[typeid(ExpressionStatement)] = &Auditor::auditExpressionStatement;
  auditFnsMap[typeid(InfixExpression)] = &Auditor::auditInfix;
  auditFnsMap[typeid(Identifier)] = &Auditor::auditIdentifier;

  auditFnsMap[typeid(AssignmentStatement)] = &Auditor::auditAssignmentStatement;
  auditFnsMap[typeid(ifStatement)] = &Auditor::auditIfStatement;

  auditFnsMap[typeid(WhileStatement)] = &Auditor::auditWhileStatement;
}

void Auditor::NativeAndForeignerClassifierPass(Node *node) {
  if (!node)
    return;

  logInternal("[CLASSIFIER-RECURSION] Visiting node type: " + node->toString());

  if (auto blockStmt = dynamic_cast<BlockStatement *>(node)) {
    logInternal("[CLASSIFIER-RECURSION] Hitting BlockStatement, entering "
                "classifyBlock");
    classifyBlock(blockStmt);
  } else if (auto blockExpr = dynamic_cast<BlockExpression *>(node)) {
    logInternal("[CLASSIFIER-RECURSION] Hitting BlockExpression, entering "
                "classifyBlock");
    classifyBlock(blockExpr);
  }

  if (auto func = dynamic_cast<FunctionStatement *>(node)) {
    logInternal("[CLASSIFIER-RECURSION] Drilling into FunctionStatement -> "
                "FunctionExpression");
    NativeAndForeignerClassifierPass(func->funcExpr.get());
  } else if (auto funcExpr = dynamic_cast<FunctionExpression *>(node)) {
    logInternal("[CLASSIFIER-RECURSION] Drilling into FunctionExpression -> "
                "Body Block");
    NativeAndForeignerClassifierPass(funcExpr->block.get());
  } else if (auto whileStmt = dynamic_cast<WhileStatement *>(node)) {
    logInternal(
        "[CLASSIFIER-RECURSION] Drilling into WhileStatement -> Loop Body");
    NativeAndForeignerClassifierPass(whileStmt->loop.get());
  }
}

void Auditor::classifyBlock(Node *block) {
  logInternal("[CLASSIFY BLOCK] Starting classification for block: " +
              block->toString());

  if (auto blockStmt = dynamic_cast<BlockStatement *>(block)) {
    if (deferedFrees.find(blockStmt) == deferedFrees.end()) {
      deferedFrees[blockStmt] = std::make_unique<BlockInfo>();
    }
    for (const auto &stmt : blockStmt->statements) {
      Node *actualNode = stmt.get();
      if (auto exprStmt = dynamic_cast<ExpressionStatement *>(actualNode)) {
        actualNode = exprStmt->expression.get();
      }
      if (auto whileStmt = dynamic_cast<WhileStatement *>(actualNode)) {
        logInternal("[CLASSIFY BLOCK] Detected nested while statement");
        classifyBlock(whileStmt->loop.get());
      }
      auto sym = semantics.getSymbolFromMeta(actualNode);
      if (!sym) {
        logInternal("[CLASSIFY BLOCK] Skipping non-symbol statement: " +
                    stmt->toString());
        continue;
      }

      if (!sym->isHeap) {
        logInternal("[CLASSIFY BLOCK] Skipping non-heap symbol: " + sym->ID);
        continue;
      }

      bool bornHere = semantics.isBornInScope(blockStmt, sym->ID);
      if (bornHere) {
        logInternal("[CLASSIFY BLOCK] NATIVE FOUND: " + sym->ID +
                    " born in this BlockStatement");
        deferedFrees[blockStmt]->natives.push_back(sym->ID);
      } else {
        logInternal("[CLASSIFY BLOCK] POTENTIAL FOREIGNER: " + sym->ID +
                    " - Calling identifyForeigners");
        identifyForeigners(blockStmt);
      }
    }
  } else if (auto blockExpr = dynamic_cast<BlockExpression *>(block)) {
    if (deferedFrees.find(blockExpr) == deferedFrees.end()) {
      deferedFrees[blockExpr] = std::make_unique<BlockInfo>();
    }
    for (const auto &stmt : blockExpr->statements) {
      Node *actualNode = stmt.get();
      if (auto exprStmt = dynamic_cast<ExpressionStatement *>(actualNode)) {
        actualNode = exprStmt->expression.get();
      }
      if (auto whileStmt = dynamic_cast<WhileStatement *>(actualNode)) {
        logInternal("[CLASSIFY BLOCK] Detected nested while statement");
        classifyBlock(whileStmt->loop.get());
      }
      auto sym = semantics.getSymbolFromMeta(actualNode);
      if (!sym) {
        logInternal("[CLASSIFY BLOCK] Skipping non-symbol statement: " +
                    stmt->toString());
        continue;
      }

      if (!sym->isHeap) {
        logInternal("[CLASSIFY BLOCK] Skipping non-heap symbol: " + sym->ID);
        continue;
      }

      bool bornHere = semantics.isBornInScope(blockExpr, sym->ID);
      if (bornHere) {
        logInternal("[CLASSIFY BLOCK] NATIVE FOUND: " + sym->ID +
                    " born in this BlockExpression");
        deferedFrees[blockExpr]->natives.push_back(sym->ID);
      } else {
        logInternal("[CLASSIFY BLOCK] POTENTIAL FOREIGNER: " + sym->ID +
                    " - Calling identifyForeigners");
        identifyForeigners(blockExpr);
      }
    }
  }
}

void Auditor::identifyForeigners(Node *block) {
  logInternal("[FOREIGN CLASSIFIER] Investigating block for foreign IDs...");

  if (auto blockStmt = dynamic_cast<BlockStatement *>(block)) {
    if (deferedFrees.find(blockStmt) == deferedFrees.end()) {
      deferedFrees[blockStmt] = std::make_unique<BlockInfo>();
    }
    for (const auto &stmt : blockStmt->statements) {
      Node *target = stmt.get();

      if (auto exprStmt = dynamic_cast<ExpressionStatement *>(target)) {
        target = exprStmt->expression.get();
      }

      if (auto whileStmt = dynamic_cast<WhileStatement *>(target)) {
        logInternal("[FOREIGN CLASSIFIER] Encountered while statement");
        identifyForeigners(whileStmt->loop.get());
      }
      auto stmtSym = semantics.getSymbolFromMeta(target);
      if (!stmtSym)
        continue;

      logInternal("[FOREIGN CLASSIFIER] Checking symbol: " + stmtSym->ID);

      Node *holder = semantics.queryForLifeTimeBaton(stmtSym->ID);
      if (!holder) {
        logInternal("[FOREIGN CLASSIFIER] FAILED: No baton holder for " +
                    stmtSym->ID);
        continue;
      }

      auto &baton = semantics.responsibilityTable[holder];
      if (!baton) {
        logInternal("[FOREIGN CLASSIFIER] FAILED: No baton in "
                    "responsibilityTable for ID: " +
                    stmtSym->ID);
        continue;
      }

      bool bornInThisScope = semantics.isBornInScope(blockStmt, stmtSym->ID);
      bool diesInThisBlock = diesInBlock(stmtSym->ID, blockStmt);

      logInternal("[FOREIGN CLASSIFIER] Evaluation for " + stmtSym->ID +
                  " | BornHere: " + (bornInThisScope ? "YES" : "NO") +
                  " | DiesInBlock: " + (diesInThisBlock ? "YES" : "NO"));

      if (!bornInThisScope && diesInThisBlock) {
        logInternal("[FOREIGN CLASSIFIER] SUCCESS: Adding " + stmtSym->ID +
                    " to foreigners list");
        deferedFrees[blockStmt]->foreigners.push_back(stmtSym->ID);
      }
    }
  }
}

// This is a power function I will only call it in special cases where the baton
// system cannot work or is compromised
void Auditor::bunkerNatives(Node *block) {
  auto &blockInfo = deferedFrees[block];
  for (const auto &nativeID : blockInfo->natives) {
    Node *holder = semantics.queryForLifeTimeBaton(nativeID);
    if (!holder) {
      logInternal("[NATIVE BUNKERING] Could not find the baton holder for "
                  "lifetime ID:" +
                  nativeID + " skipping...");
      continue;
    }

    auto holderSym = semantics.getSymbolFromMeta(holder);
    if (!holderSym) {
      logInternal(
          "[NATIVE BUNKERING] Failed to get the symbol info for holder " +
          holder->toString() + " for lifetime ID: " + nativeID +
          " skipping...");
      continue;
    }

    auto &baton = semantics.responsibilityTable[holder];
    if (!baton) {
      logInternal("[NATIVE BUNKERING] Could not find the baton for "
                  "lifetime ID:" +
                  nativeID + " skipping...");
      continue;
    }

    nativesToFree[block].push_back({std::move(baton), holderSym});
    bunkeredIDs.insert(nativeID);
  }
}

void Auditor::bunkerForeigners(Node *block) {
  logInternal("[FOREIGNER BUNKERING] Starting action for block: " +
              block->toString());

  auto &blockInfo = deferedFrees[block];
  if (!blockInfo) {
    logInternal("[FOREIGNER BUNKERING] No BlockInfo (To-Do list) found for "
                "this block address. Bailing.");
    return;
  }

  logInternal("[FOREIGNER BUNKERING] Found " +
              std::to_string(blockInfo->foreigners.size()) +
              " foreigners to process.");

  for (const auto &foreignerID : blockInfo->foreigners) {
    logInternal("[FOREIGNER BUNKERING] Attempting to bunker: " + foreignerID);

    Node *holder = semantics.queryForLifeTimeBaton(foreignerID);
    if (!holder) {
      logInternal("[FOREIGNER BUNKERING] FAILED: Could not find the baton "
                  "holder for ID: " +
                  foreignerID);
      continue;
    }

    auto holderSym = semantics.getSymbolFromMeta(holder);
    if (!holderSym) {
      logInternal("[FOREIGNER BUNKERING] FAILED: No symbol info for holder " +
                  holder->toString());
      continue;
    }

    auto &baton = semantics.responsibilityTable[holder];
    if (!baton) {
      logInternal(
          "[FOREIGNER BUNKERING] FAILED: No baton in responsibilityTable for " +
          foreignerID);
      continue;
    }

    // SUCCESS LOGS
    logInternal("[FOREIGNER BUNKERING] SUCCESS: Moving baton for " +
                foreignerID + " into bunker.");

    foreignersToFree[block].push_back({std::move(baton), holderSym});
    bunkeredIDs.insert(foreignerID);

    logInternal("[FOREIGNER BUNKERING] ID " + foreignerID +
                " is now officially bunkered.");
  }
}

bool Auditor::isBlock(Node *node) {
  if (dynamic_cast<WhileStatement *>(node))
    return true;
  if (dynamic_cast<ForStatement *>(node))
    return true;

  return false;
}

void Auditor::audit(Node *node) {
  auto auditIt = auditFnsMap.find(typeid(*node));
  if (auditIt == auditFnsMap.end()) {
    return;
  }
  logInternal("[AUDITING] Node: " + node->toString());
  (this->*auditIt->second)(node);
}

void Auditor::auditExpressionStatement(Node *node) {
  auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
  audit(exprStmt->expression.get());
}

void Auditor::auditHeapStatement(Node *node) {
  auto heapStmt = dynamic_cast<HeapStatement *>(node);
  if (!heapStmt)
    return;

  audit(heapStmt->stmt.get());
}

void Auditor::auditLetStatement(Node *node) {
  auto letStmt = dynamic_cast<LetStatement *>(node);
  if (!letStmt)
    return;

  auto letSym = semantics.getSymbolFromMeta(letStmt);
  if (!letSym)
    return;

  if (!letSym->isHeap) {
    logInternal("Let statement is not heap raised not tracking");
    return;
  }

  logInternal("\n[AUDIT] >>> LetStatement: " + letSym->ID);
  simulateFree(letStmt, letSym->ID);
}

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
  simulateFree(ptrStmt, ptrSym->ID);
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
  simulateFree(assignStmt, assignSym->ID);
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

void Auditor::auditWhileStatement(Node *node) {
  auto whileStmt = dynamic_cast<WhileStatement *>(node);
  if (!whileStmt)
    return;

  // Force a bunker on any foreigner in the while loop
  logInternal("[DEBUG] Attempting to bunker block at address: " +
              std::to_string((uintptr_t)whileStmt->loop.get()));

  auto it = deferedFrees.find(whileStmt->loop.get());
  if (it != deferedFrees.end() && it->second) {
    if (!it->second->foreigners.empty()) {
      logInternal("Triggered actual foreigner bunkering");
      bunkerForeigners(whileStmt->loop.get());
    }
  }

  audit(whileStmt->loop.get());
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
  simulateFree(retStmt, retSym->ID);
}

void Auditor::auditInfix(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  if (!infix)
    return;

  audit(infix->left_operand.get());
  audit(infix->right_operand.get());
}

void Auditor::auditIdentifier(Node *node) {
  auto ident = dynamic_cast<Identifier *>(node);
  if (!ident)
    return;

  auto identSym = semantics.getSymbolFromMeta(ident);
  simulateFree(ident, identSym->ID);
}

void Auditor::auditTraceStatement(Node *node) {
  auto traceStmt = dynamic_cast<TraceStatement *>(node);
  if (!traceStmt)
    return;

  auto traceSym = semantics.getSymbolFromMeta(traceStmt);

  audit(traceStmt->expr.get());

  simulateFree(traceStmt, traceSym->ID);
}

void Auditor::auditBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt)
    return;

  for (const auto &stmt : blockStmt->statements) {
    auto breakStmt = dynamic_cast<BreakStatement *>(stmt.get());
    auto continueStmt = dynamic_cast<ContinueStatement *>(stmt.get());
    if (containsNode(blockStmt, breakStmt) ||
        containsNode(blockStmt, continueStmt)) {
      logInternal(
          "Block contains break or continue must execute natives population");
      bunkerNatives(blockStmt);
    }

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

    if (diesSomewhere) {
      if (survivesSomewhere) {
        logInternal("[SYNC] DISCREPANCY DETECTED for " + id);
      } else {
        logInternal(
            "[SYNC] " + id +
            " dies in all paths. Materializing warrants for all branches.");
      }
      reconcileWithSnapshots(ifStmt, id);
    } else {
      logInternal("[SYNC] " + id +
                  " survives all branches. No local deallocs needed.");
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
    implicitSnap.terminalNode = nullptr;
    implicitSnap.isResponsible = true;

    materializeDeputy(id, implicitSnap, ifStmt);
  }
}

void Auditor::materializeDeputy(const std::string &id,
                                const BatonStateSnapshot &snap,
                                Node *branchRoot) {
  logInternal("[MATERIALIZE] Creating Death Warrant for " + id + " on node " +
              (snap.terminalNode ? snap.terminalNode->toString() : "NULL"));

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
      logInternal("[MATERIALIZE] No potential holder found in branch. Using "
                  "Bag for " +
                  branchRoot->toString());
      leakedDeputiesBag[branchRoot].push_back(std::move(deputy));
    }
  } else {
    logInternal("Terminal node: " +
                (snap.terminalNode ? snap.terminalNode->toString() : "NULL") +
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
        return true; // Dig into the elif wrapper
    }
    if (ifStmt->else_result.has_value()) {
      if (containsNode(ifStmt->else_result.value().get(), target))
        return true;
    }
  } else if (auto elif = dynamic_cast<elifStatement *>(root)) {
    if (containsNode(elif->elif_condition.get(), target))
      return true;
    if (containsNode(elif->elif_result.get(), target))
      return true;
  }

  else if (auto whileStmt = dynamic_cast<WhileStatement *>(root)) {
    if (containsNode(whileStmt->loop.get(), target))
      return true;
  }

  else if (auto exprStmt = dynamic_cast<ExpressionStatement *>(root)) {
    if (containsNode(exprStmt->expression.get(), target))
      return true;
  }

  return false;
}

bool Auditor::isBunkered(const std::string &id) {
  return bunkeredIDs.find(id) != bunkeredIDs.end();
}

void Auditor::simulateFree(Node *contextNode, const std::string &contextID) {
  logInternal("\n  [SIMULATE-FREE] Target: " + contextID);
  if (isBunkered(contextID)) {
    logInternal("    [SIMULATE FREE] " + contextID +
                " is bunkered. Skipping freeing simulation.");
    return;
  }

  Node *holderNode = semantics.queryForLifeTimeBaton(contextID);
  if (!holderNode) {
    logInternal("    [ERROR] No holder node found for " + contextID);
    return;
  }

  // Check if the holder node is the actual contextNode
  if (holderNode != contextNode) {
    logInternal("The baton was moved node: " + contextNode->toString() +
                "' is not the executor");
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

bool Auditor::diesInBlock(const std::string &ID, Node *block) {
  logInternal("[DIES-IN-BLOCK] Checking if ID: " + ID +
              " dies in block: " + block->toString());

  Node *holder = semantics.queryForLifeTimeBaton(ID);
  if (!holder) {
    logInternal("[DIES-IN-BLOCK] FAILED: No holder found for " + ID);
    return false;
  }

  bool contains = containsNode(block, holder);
  logInternal("[DIES-IN-BLOCK] Holder for " + ID + " is " + holder->toString());
  logInternal("[DIES-IN-BLOCK] Does block contain holder? " +
              std::string(contains ? "YES" : "NO"));

  if (contains) {
    auto &baton = semantics.responsibilityTable[holder];
    if (!baton) {
      logInternal("[DIES-IN-BLOCK] FAILED: No baton for holder");
      return false;
    }

    auto holderSym = semantics.getSymbolFromMeta(holder);
    if (!holderSym) {
      logInternal("[DIES-IN-BLOCK] FAILED: No symbol info for holder");
      return false;
    }

    logInternal(
        "[DIES-IN-BLOCK] PtrCount: " + std::to_string(holderSym->pointerCount) +
        " | RefCount: " + std::to_string(holderSym->refCount) +
        " | Responsible: " + (baton->isResponsible ? "T" : "F"));

    if (holderSym->pointerCount == 0 && holderSym->refCount == 0) {
      if (baton->isResponsible) {
        logInternal("[DIES-IN-BLOCK] SUCCESS: ID " + ID + " dies here.");
        return true;
      }
    }
  } else {
    logInternal("[DIES-IN-BLOCK] Bailing because holder is outside this block. "
                "(This is why foreigners fail!)");
  }

  return false;
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
