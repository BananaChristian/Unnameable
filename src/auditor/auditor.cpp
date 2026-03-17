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
  auditFnsMap[typeid(CaseClause)] = &Auditor::auditCaseStatement;
  auditFnsMap[typeid(SwitchStatement)] = &Auditor::auditSwitchStatement;

  auditFnsMap[typeid(WhileStatement)] = &Auditor::auditWhileStatement;
  auditFnsMap[typeid(ForStatement)] = &Auditor::auditForStatement;
}

void Auditor::runClassifier(Node *node) {
  logInternal("[CLASSIFIER] Starting classification pass");
  classifyNode(node);
}

void Auditor::classifyNode(Node *node) {
  if (!node)
    return;

  // Classify this node if it's a block
  if (auto blockStmt = dynamic_cast<BlockStatement *>(node)) {
    classifyBlock(blockStmt);
  } else if (auto blockExpr = dynamic_cast<BlockExpression *>(node)) {
    classifyBlock(blockExpr);
  }

  // Recurse into children
  if (auto func = dynamic_cast<FunctionStatement *>(node)) {
    classifyNode(func->funcExpr.get());
  } else if (auto funcExpr = dynamic_cast<FunctionExpression *>(node)) {
    classifyNode(funcExpr->block.get());
  } else if (auto whileStmt = dynamic_cast<WhileStatement *>(node)) {
    classifyNode(whileStmt->loop.get());
  } else if (auto forStmt = dynamic_cast<ForStatement *>(node)) {
    classifyNode(forStmt->body.get());
  } else if (auto ifStmt = dynamic_cast<ifStatement *>(node)) {
    classifyNode(ifStmt->if_result.get());
    for (const auto &elif : ifStmt->elifClauses) {
      auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
      classifyNode(elifStmt->elif_result.get());
    }
    if (ifStmt->else_result.has_value()) {
      classifyNode(ifStmt->else_result.value().get());
    }
  } else if (auto switchStmt = dynamic_cast<SwitchStatement *>(node)) {
    classifySwitch(switchStmt);
  }
}

void Auditor::classifyBlock(Node *block) {
  logInternal("[CLASSIFIER] Classifying block: " + block->toString());

  // Ensure BlockInfo exists
  if (deferedFrees.find(block) == deferedFrees.end()) {
    deferedFrees[block] = std::make_unique<BlockInfo>();
  }
  auto &info = deferedFrees[block];

  info->natives.clear();
  info->foreigners.clear();

  // Get all statements in this block
  std::vector<Node *> statements;
  if (auto blockStmt = dynamic_cast<BlockStatement *>(block)) {
    for (auto &stmt : blockStmt->statements) {
      statements.push_back(stmt.get());
    }
  } else if (auto blockExpr = dynamic_cast<BlockExpression *>(block)) {
    for (auto &stmt : blockExpr->statements) {
      statements.push_back(stmt.get());
    }
  }

  // Classify each statement
  for (Node *stmt : statements) {
    Node *actualNode = peelNode(stmt);

    // Recurse into nested blocks
    if (auto whileStmt = dynamic_cast<WhileStatement *>(actualNode)) {
      classifyNode(whileStmt->loop.get());
    } else if (auto forStmt = dynamic_cast<ForStatement *>(actualNode)) {
      classifyNode(forStmt->body.get());
    } else if (auto ifStmt = dynamic_cast<ifStatement *>(actualNode)) {
      classifyNode(ifStmt->if_result.get());
      for (const auto &elif : ifStmt->elifClauses) {
        auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
        classifyNode(elifStmt->elif_result.get());
      }
      if (ifStmt->else_result.has_value()) {
        classifyNode(ifStmt->else_result.value().get());
      }
    } else if (auto switchStmt = dynamic_cast<SwitchStatement *>(actualNode)) {
      classifySwitch(switchStmt);
    }

    // Get symbol
    auto sym = semantics.getSymbolFromMeta(actualNode);
    if (!sym || !sym->isHeap)
      continue;

    // Determine if native (born here) or foreigner
    bool bornHere = semantics.isBornInScope(block, sym->ID);

    if (bornHere) {
      if (!isAlreadyClassified(sym->ID, info)) {
        logInternal("[CLASSIFIER] NATIVE: " + sym->ID + " in " +
                    block->toString());
        info->natives.push_back(sym->ID);
      } else {
        logInternal("[CLASSIFIER] NATIVE (DUPLICATE SKIPPED): " + sym->ID +
                    " in " + block->toString());
      }
    } else {
      // Check if it's a true foreigner (dies in this block)
      if (diesInBlock(sym->ID, block)) {
        if (!isAlreadyClassified(sym->ID, info)) {
          logInternal("[CLASSIFIER] FOREIGNER: " + sym->ID + " in " +
                      block->toString());
          info->foreigners.push_back(sym->ID);
        } else {
          logInternal("[CLASSIFIER] FOREIGNER (DUPLICATE SKIPPED): " + sym->ID +
                      " in " + block->toString());
        }
      }
    }
  }
}

void Auditor::classifyClause(
    SwitchStatement *sw, Node *stmt,
    const std::vector<std::unique_ptr<Statement>> &clause,
    const std::unique_ptr<BlockInfo> &blockInfo) {

  logInternal("\n========== CLAUSE CLASSIFIER START ==========");
  logInternal("[CLAUSE-CLASSIFIER] Processing statement in switch: " +
              (stmt ? stmt->toString() : "NULL"));
  logInternal("[CLAUSE-CLASSIFIER] Switch being classified: " +
              (sw ? sw->toString() : "NULL"));
  logInternal("[CLAUSE-CLASSIFIER] Clause vector size: " +
              std::to_string(clause.size()));

  Node *actual = peelNode(stmt);
  logInternal("[CLAUSE-CLASSIFIER] After peelNode:");
  logInternal("  - Original node: " + (stmt ? stmt->toString() : "NULL"));
  logInternal("  - Peeled node: " + (actual ? actual->toString() : "NULL"));
  logInternal("  - Original node type: " +
              (stmt ? std::string(typeid(*stmt).name()) : "NULL"));
  logInternal("  - Peeled node type: " +
              (actual ? std::string(typeid(*actual).name()) : "NULL"));
  logInternal("  - Are they the same? " +
              std::string((stmt == actual) ? "YES" : "NO"));

  // If the statement is a block call the normal block classifier
  if (isBlock(actual)) {
    logInternal(
        "[CLAUSE-CLASSIFIER] Node is a block - delegating to classifyNode");
    logInternal("  - Block type detected, recursing...");
    classifyNode(actual);
    logInternal("[CLAUSE-CLASSIFIER] Block classification complete");
  } else {
    logInternal(
        "[CLAUSE-CLASSIFIER] Node is not a block - treating as floater");

    // If it is among the floaters
    logInternal("[CLAUSE-CLASSIFIER] Attempting to get symbol from node");
    auto sym = semantics.getSymbolFromMeta(actual);

    if (!sym) {
      logInternal("[CLAUSE-CLASSIFIER] No symbol found for node");
      logInternal("  - Possible reasons:");
      logInternal("    * Node has no metadata attached");
      logInternal("    * Node type not tracked by semantics");
      logInternal("    * Node is not a heap variable usage");
      logInternal("[CLAUSE-CLASSIFIER] Returning early");
      logInternal("========== CLAUSE CLASSIFIER END (NO SYMBOL) ==========\n");
      return;
    }

    logInternal("[CLAUSE-CLASSIFIER] Symbol found:");
    logInternal("  - ID: " + sym->ID);
    logInternal("  - Is heap: " + std::string(sym->isHeap ? "YES" : "NO"));
    logInternal("  - Is pointer: " +
                std::string(sym->isPointer ? "YES" : "NO"));
    logInternal("  - Pointer count: " + std::to_string(sym->pointerCount));
    logInternal("  - Ref count: " + std::to_string(sym->refCount));

    if (!sym->isHeap) {
      logInternal("[CLAUSE-CLASSIFIER] Symbol is not heap - skipping");
      logInternal("========== CLAUSE CLASSIFIER END (NOT HEAP) ==========\n");
      return;
    }

    logInternal(
        "[CLAUSE-CLASSIFIER] Checking if symbol was born in this clause");
    bool bornHere = checkBirth(sym->ID, clause);
    logInternal("  - Born here? " + std::string(bornHere ? "YES" : "NO"));

    if (bornHere) {
      logInternal("[CLAUSE-CLASSIFIER] NATIVE DETECTED: " + sym->ID);
      logInternal("  - Adding to natives list for switch: " + sw->toString());
      blockInfo->natives.push_back(sym->ID);
      logInternal("  - Natives count now: " +
                  std::to_string(blockInfo->natives.size()));
    } else {
      logInternal(
          "[CLAUSE-CLASSIFIER] Not born here - checking if it dies in switch");
      logInternal("  - Calling doesSymbolDieInSwitch for ID: " + sym->ID);

      bool diesHere = doesSymbolDieInSwitch(sym->ID, clause);
      logInternal("  - Dies in switch? " +
                  std::string(diesHere ? "YES" : "NO"));

      if (diesHere) {
        logInternal("[CLAUSE-CLASSIFIER] FOREIGNER DETECTED: " + sym->ID);
        logInternal("  - Adding to foreigners list for switch: " +
                    sw->toString());
        blockInfo->foreigners.push_back(sym->ID);
        logInternal("  - Foreigners count now: " +
                    std::to_string(blockInfo->foreigners.size()));
      } else {
        logInternal("[CLAUSE-CLASSIFIER] Symbol neither native nor foreigner "
                    "in this switch");
        logInternal("  - Not born here, doesn't die here - ignoring");
      }
    }
  }

  logInternal("[CLAUSE-CLASSIFIER] Clause classification complete");
  logInternal("  - Current natives in switch: " +
              std::to_string(blockInfo->natives.size()));
  logInternal("  - Current foreigners in switch: " +
              std::to_string(blockInfo->foreigners.size()));
  logInternal("========== CLAUSE CLASSIFIER END ==========\n");
}

void Auditor::classifySwitch(SwitchStatement *sw) {
  if (deferedFrees.find(sw) == deferedFrees.end()) {
    deferedFrees[sw] = std::make_unique<BlockInfo>();
  }
  auto &info = deferedFrees[sw];
  logInternal("[SWICTH CLASSIFIER] Classifying switch");

  for (const auto &caseClause : sw->case_clauses) {
    auto caseNode = dynamic_cast<CaseClause *>(caseClause.get());
    if (!caseNode)
      continue;

    for (auto &stmt : caseNode->body) {
      classifyClause(sw, stmt.get(), caseNode->body, info);
    }
  }

  // Process default with its OWN body
  for (auto &stmt : sw->default_statements) {
    classifyClause(sw, stmt.get(), sw->default_statements, info);
  }
}

Node *Auditor::peelNode(Node *node) {
  if (auto exprStmt = dynamic_cast<ExpressionStatement *>(node)) {
    return exprStmt->expression.get();
  }

  if (auto heapStmt = dynamic_cast<HeapStatement *>(node))
    return heapStmt->stmt.get();

  return node;
}

bool Auditor::hasDisruptors(Node *block) {
  if (auto blockStmt = dynamic_cast<BlockStatement *>(block)) {
    for (auto &stmt : blockStmt->statements) {
      if (dynamic_cast<BreakStatement *>(stmt.get()) ||
          dynamic_cast<ContinueStatement *>(stmt.get())) {
        return true;
      }
    }
  }
  return false;
}

bool Auditor::shouldForeignBunkerBlock(Node *block) {
  auto it = deferedFrees.find(block);
  if (it == deferedFrees.end() || !it->second)
    return false;

  // A block needs bunkering if it has any foreigners
  return !it->second->foreigners.empty();
}

bool Auditor::shouldNativeBunkerBlock(Node *block) {
  auto it = deferedFrees.find(block);
  if (it == deferedFrees.end() || !it->second)
    return false;

  // A block needs bunkering if it has any foreigners
  return !it->second->natives.empty();
}

void Auditor::bunkerNatives(Node *block) {
  auto &blockInfo = deferedFrees[block];
  for (const auto &nativeID : blockInfo->natives) {
    if (isBunkered(nativeID)) {
      logInternal("[NATIVE BUNKERING] Baton " + nativeID +
                  " already bunkered, skipping");
      continue;
    }

    Node *holder = semantics.queryForLifeTimeBaton(nativeID);
    if (!holder) {
      scanForBaton(nativeID);
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
      scanForBaton(nativeID);
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
    if (isBunkered(foreignerID)) {
      logInternal("[FOREIGNER BUNKERING] Baton " + foreignerID +
                  " already bunkered, skipping");
      continue;
    }

    Node *holder = semantics.queryForLifeTimeBaton(foreignerID);
    if (!holder) {
      scanForBaton(foreignerID);
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
      scanForBaton(foreignerID);
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
  if (dynamic_cast<ifStatement *>(node))
    return true;
  if (dynamic_cast<BlockStatement *>(node))
    return true;

  return false;
}

bool Auditor::isDeclaration(Node *node) {
  if (dynamic_cast<LetStatement *>(node))
    return true;
  if (dynamic_cast<PointerStatement *>(node))
    return true;
  if (dynamic_cast<ArrayStatement *>(node))
    return true;

  return false;
}

bool Auditor::checkBirth(
    const std::string &id,
    const std::vector<std::unique_ptr<Statement>> &clause) {
  logInternal("[CHECKING BIRTH] Checking heap statement birth in clause");
  for (const auto &stmt : clause) {
    Node *actual = peelNode(stmt.get());
    if (isDeclaration(actual)) {
      auto declSym = semantics.getSymbolFromMeta(actual);
      if (!declSym) {
        logInternal("[CHEKCING BIRTH] Failed to find declaration symbol info "
                    "skipping...");
        continue;
      }

      if (id == declSym->ID) {
        logInternal("[CHECKING BIRTH] Found heap birth for id: " + id);
        return true;
      }
    }
  }

  return false;
}

void Auditor::audit(Node *node) {
  static bool classifierRun = false;
  if (!classifierRun) {
    runClassifier(node);
    classifierRun = true;
  }

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

void Auditor::auditForStatement(Node *node) {
  auto forStmt = dynamic_cast<ForStatement *>(node);
  if (!forStmt)
    return;

  if (shouldForeignBunkerBlock(forStmt->body.get())) {
    logInternal("[TRIGGER] For Loop with foreigners - bunkering");
    bunkerForeigners(forStmt->body.get());
  }

  audit(forStmt->body.get());
}

void Auditor::auditWhileStatement(Node *node) {
  auto whileStmt = dynamic_cast<WhileStatement *>(node);
  if (!whileStmt)
    return;

  if (shouldForeignBunkerBlock(whileStmt->loop.get())) {
    logInternal("[TRIGGER] Loop with foreigners - bunkering");
    bunkerForeigners(whileStmt->loop.get());
  }

  audit(whileStmt->loop.get());
}

void Auditor::auditCaseStatement(Node *node) {
  auto caseClause = dynamic_cast<CaseClause *>(node);
  if (!caseClause)
    return;

  for (auto &stmt : caseClause->body) {
    audit(stmt.get());
  }
}

void Auditor::auditSwitchStatement(Node *node) {
  auto switchStmt = dynamic_cast<SwitchStatement *>(node);
  if (!switchStmt)
    return;

  logInternal("[AUDIT] SwitchStatement");

  if (deferedFrees.find(switchStmt) == deferedFrees.end()) {
    deferedFrees[switchStmt] = std::make_unique<BlockInfo>();
  }

  if (shouldForeignBunkerBlock(switchStmt)) {
    logInternal("[TRIGGER] Switch with foreigners - bunkering");
    bunkerForeigners(switchStmt);
  }

  // Now audit everything normally
  for (auto &caseClause : switchStmt->case_clauses) {
    audit(caseClause.get());
  }
  for (auto &stmt : switchStmt->default_statements) {
    audit(stmt.get());
  }
}

void Auditor::auditElifStatement(Node *node) {
  auto elifStmt = dynamic_cast<elifStatement *>(node);
  if (!elifStmt)
    return;

  audit(elifStmt->elif_result.get());
}

void Auditor::auditIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  // Check then block
  if (shouldForeignBunkerBlock(ifStmt->if_result.get())) {
    logInternal("[TRIGGER] Then block has foreigners - bunkering");
    bunkerForeigners(ifStmt->if_result.get());
  }

  // Check elif blocks
  for (auto &elif : ifStmt->elifClauses) {
    auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
    if (shouldForeignBunkerBlock(elifStmt->elif_result.get())) {
      logInternal("[TRIGGER] Elif block has foreigners - bunkering");
      bunkerForeigners(elifStmt->elif_result.get());
    }
  }

  // Check else block (if it exists)
  if (ifStmt->else_result.has_value()) {
    if (shouldForeignBunkerBlock(ifStmt->else_result.value().get())) {
      logInternal("[TRIGGER] Else block has foreigners - bunkering");
      bunkerForeigners(ifStmt->else_result.value().get());
    }
  }

  // Audit all blocks normally
  audit(ifStmt->if_result.get());
  for (auto &elif : ifStmt->elifClauses) {
    audit(elif.get());
  }
  if (ifStmt->else_result.has_value()) {
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

  if (hasDisruptors(blockStmt) && shouldNativeBunkerBlock(blockStmt)) {
    logInternal(
        "[TRIGGER] Block disruptor detected bunkering natives for block: " +
        blockStmt->toString());
    bunkerNatives(blockStmt);
  }

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
std::vector<std::string> Auditor::getAllActiveBatonIDsInBlock(Node *block) {
  logInternal("[SELECTED_SCAN] Identifying all live batons in "
              "responsibilityTable for this block " +
              block->toString());

  std::vector<std::string> activeIDs;

  for (const auto &[node, baton] : semantics.responsibilityTable) {
    if (!baton) {
      logInternal("[SELECTED_SCAN] Skipping since there is no baton...");
      continue;
    }

    logInternal("[SELECTED_SCAN] Inspecting Table Entry: Node(" +
                node->toString() + ") -> BatonID(" + baton->ID + ")");

    if (baton->isResponsible) {
      Node *holder = semantics.queryForLifeTimeBaton(baton->ID);
      if (!holder) {
        logInternal("[SELECTED_SCAN] ! ERROR: No holder found for ID: " +
                    baton->ID);
        continue;
      }

      if (!containsNode(block, holder)) {
        logInternal("[SELECTED_SCAN] Lieftime ID: " + baton->ID +
                    " dies outside this statement skiping...'");
        continue;
      }

      auto holderSym = semantics.getSymbolFromMeta(holder);
      if (!holderSym) {
        logInternal(
            "[SELECTED_SCAN] ! ERROR: No symbol metadata for holder of: " +
            baton->ID);
        continue;
      }

      logInternal("[SELECTED_SCAN] Checking Stats for " + baton->ID +
                  ": isResponsible(" + std::to_string(baton->isResponsible) +
                  ") refCount(" + std::to_string(holderSym->refCount) +
                  ") ptrCount(" + std::to_string(holderSym->pointerCount) +
                  ")");

      logInternal("[SELECTED_SCAN] Found LIVE baton: " + baton->ID);
      activeIDs.push_back(baton->ID);
    }
  }
  return activeIDs;
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

bool Auditor::containsNode(Node *root, Node *target) {
  if (!root || !target)
    return false;
  if (root == target)
    return true;

  if (auto block = dynamic_cast<BlockStatement *>(root)) {
    for (auto &stmt : block->statements) {
      Node *peeled = peelNode(stmt.get());
      if (containsNode(peeled, target))
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
    if (containsNode(whileStmt->condition.get(), target))
      return true;
    if (containsNode(whileStmt->loop.get(), target))
      return true;
  } else if (auto forStmt = dynamic_cast<ForStatement *>(root)) {
    if (containsNode(forStmt->initializer.get(), target))
      return true;
    if (containsNode(forStmt->condition.get(), target))
      return true;
    if (containsNode(forStmt->step.get(), target))
      return true;
    if (containsNode(forStmt->body.get(), target))
      return true;
  } else if (auto infix = dynamic_cast<InfixExpression *>(root)) {
    if (containsNode(infix->left_operand.get(), target))
      return true;
    if (containsNode(infix->right_operand.get(), target))
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
    logInternal(
        "[DIES-IN-BLOCK] Bailing because holder is outside this block. ");
  }

  return false;
}

bool Auditor::doesSymbolDieInSwitch(
    const std::string &id,
    const std::vector<std::unique_ptr<Statement>> &clause) {
  logInternal("[DIES IN CLAUSE] Checking if ID: " + id + " dies in clause");

  for (const auto &stmt : clause) {
    Node *stmtPtr = stmt.get();

    // If the statement we hit is some sort of block call the block detector
    if (isBlock(stmtPtr)) {
      if (diesInBlock(id, stmtPtr)) {
        logInternal("[DIES-IN-CLAUSE] SUCCESS: ID " + id + " dies in block");
        return true;
      }
      continue;
    }

    // For non-block statements, check if the holder is directly in this clause
    Node *holder = semantics.queryForLifeTimeBaton(id);
    if (!holder) {
      logInternal("[DIES-IN-CLAUSE] FAILED: No holder found for " + id);
      continue;
    }

    // Check if the holder is among the statements in this clause
    bool contains =
        std::find_if(clause.begin(), clause.end(), [holder](const auto &ptr) {
          return ptr.get() == holder;
        }) != clause.end();

    logInternal("[DIES-IN-CLAUSE] Holder for " + id + " is " +
                holder->toString());
    logInternal("[DIES-IN-CLAUSE] Does clause contain holder? " +
                std::string(contains ? "YES" : "NO"));

    if (!contains) {
      logInternal(
          "[DIES-IN-CLAUSE] Bailing because holder is outside this clause");
      continue;
    }

    auto &baton = semantics.responsibilityTable[holder];
    if (!baton) {
      logInternal("[DIES-IN-CLAUSE] FAILED: No baton for holder");
      continue;
    }

    auto holderSym = semantics.getSymbolFromMeta(holder);
    if (!holderSym) {
      logInternal("[DIES-IN-CLAUSE] FAILED: No symbol info for holder");
      continue;
    }

    logInternal("[DIES-IN-CLAUSE] PtrCount: " +
                std::to_string(holderSym->pointerCount) +
                " | RefCount: " + std::to_string(holderSym->refCount) +
                " | Responsible: " + (baton->isResponsible ? "T" : "F"));

    if (holderSym->pointerCount == 0 && holderSym->refCount == 0 &&
        baton->isResponsible) {
      logInternal("[DIES-IN-CLAUSE] SUCCESS: ID " + id + " dies here.");
      return true;
    }
  }

  logInternal("[DIES-IN-CLAUSE] ID " + id + " does not die in this clause");
  return false;
}

void Auditor::scanForBaton(const std::string &id) {
  logInternal("\n========== GLOBAL BATON SCANNER ==========");
  logInternal("[SCANNER] Performing global baton scan for lifetime ID: " + id);

  //===========================================================================
  // LAYER 1: Responsibility Table (Active Batons)
  //===========================================================================
  logInternal("\n--- LAYER 1: Responsibility Table (Active Batons) ---");
  Node *holder = semantics.queryForLifeTimeBaton(id);
  if (!holder) {
    logInternal("[SCANNER] LAYER 1: ❌ Baton " + id +
                " NOT FOUND in responsibility table");
  } else {
    logInternal("[SCANNER] LAYER 1: Found baton " + id +
                " in responsibility table");
    logInternal("  Holder node: " + holder->toString());

    auto &baton = semantics.responsibilityTable[holder];
    if (baton) {
      logInternal("  Baton state:");
      logInternal("    - isResponsible: " +
                  std::string(baton->isResponsible ? "YES" : "NO"));
      logInternal("    - isAlive: " +
                  std::string(baton->isAlive ? "YES" : "NO"));
      logInternal("    - ownedBy: " + baton->ownedBy);
      logInternal("    - Dependents count: " +
                  std::to_string(baton->dependents.size()));
    } else {
      logInternal("  [ERROR] Baton object is null despite having holder!");
    }
  }

  //===========================================================================
  // LAYER 2: Natives To Free (Bunkered Natives)
  //===========================================================================
  logInternal("\n--- LAYER 2: Natives To Free (Bunkered Natives) ---");
  bool foundInNatives = false;
  for (const auto &[block, batonList] : nativesToFree) {
    for (size_t i = 0; i < batonList.size(); ++i) {
      const auto &[baton, sym] = batonList[i];
      if (baton && baton->ID == id) {
        foundInNatives = true;
        logInternal("[SCANNER] LAYER 2: Found baton " + id +
                    " in nativesToFree");
        logInternal("  Block: " + block->toString());
        logInternal("  Position in list: " + std::to_string(i));
        logInternal("  Symbol info: " + (sym ? sym->ID : "null"));
        logInternal("  Baton state:");
        logInternal("    - isResponsible: " +
                    std::string(baton->isResponsible ? "YES" : "NO"));
        logInternal("    - isAlive: " +
                    std::string(baton->isAlive ? "YES" : "NO"));
        logInternal("    - ownedBy: " + baton->ownedBy);
      }
    }
  }
  if (!foundInNatives) {
    logInternal("[SCANNER] LAYER 2: ❌ Baton " + id +
                " NOT FOUND in nativesToFree");
  }

  //===========================================================================
  // LAYER 3: Foreigners To Free (Bunkered Foreigners)
  //===========================================================================
  logInternal("\n--- LAYER 3: Foreigners To Free (Bunkered Foreigners) ---");
  bool foundInForeigners = false;
  for (const auto &[block, batonList] : foreignersToFree) {
    for (size_t i = 0; i < batonList.size(); ++i) {
      const auto &[baton, sym] = batonList[i];
      if (baton && baton->ID == id) {
        foundInForeigners = true;
        logInternal("[SCANNER] LAYER 3: Found baton " + id +
                    " in foreignersToFree");
        logInternal("  Block: " + block->toString());
        logInternal("  Position in list: " + std::to_string(i));
        logInternal("  Symbol info: " + (sym ? sym->ID : "null"));
        logInternal("  Baton state:");
        logInternal("    - isResponsible: " +
                    std::string(baton->isResponsible ? "YES" : "NO"));
        logInternal("    - isAlive: " +
                    std::string(baton->isAlive ? "YES" : "NO"));
        logInternal("    - ownedBy: " + baton->ownedBy);
      }
    }
  }
  if (!foundInForeigners) {
    logInternal("[SCANNER] LAYER 3: ❌ Baton " + id +
                " NOT FOUND in foreignersToFree");
  }

  //===========================================================================
  // SUMMARY
  //===========================================================================
  logInternal("\n--- SCAN SUMMARY ---");
  logInternal("Baton ID: " + id);
  logInternal("Layer 1 (Responsibility Table): " +
              std::string(holder ? "FOUND" : "NOT FOUND"));
  logInternal("Layer 2 (Natives To Free): " +
              std::string(foundInNatives ? "FOUND" : "NOT FOUND"));
  logInternal("Layer 3 (Foreigners To Free): " +
              std::string(foundInForeigners ? "FOUND" : "NOT FOUND"));

  if (!holder && !foundInNatives && !foundInForeigners) {
    logInternal("[SCANNER] WARNING: Baton " + id +
                " not found in ANY layer! It may have been destroyed.");
  } else if (holder && (foundInNatives || foundInForeigners)) {
    logInternal("[SCANNER]  WARNING: Baton " + id +
                " appears in MULTIPLE layers!");
  }

  logInternal("========== GLOBAL BATON SCANNER END ==========\n");
}

bool Auditor::isAlreadyClassified(const std::string &id,
                                  const std::unique_ptr<BlockInfo> &blockInfo) {
  // Check natives list
  if (std::find(blockInfo->natives.begin(), blockInfo->natives.end(), id) !=
      blockInfo->natives.end()) {
    logInternal("[CLASSIFIER CHECK] ID: " + id +
                " is already native classified");
    return true;
  }

  // Check foreigners list
  if (std::find(blockInfo->foreigners.begin(), blockInfo->foreigners.end(),
                id) != blockInfo->foreigners.end()) {
    logInternal("[CLASSIFIER CHECK] ID: " + id +
                " is already foreign classified");
    return true;
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
