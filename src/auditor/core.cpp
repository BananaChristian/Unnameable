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
  auditFnsMap[typeid(VariableDeclaration)] = &Auditor::auditVariableDeclaration;

  auditFnsMap[typeid(FunctionStatement)] = &Auditor::auditFunctionStatement;
  auditFnsMap[typeid(FunctionExpression)] = &Auditor::auditFunctionExpression;
  auditFnsMap[typeid(ReturnStatement)] = &Auditor::auditReturnStatement;

  auditFnsMap[typeid(BlockStatement)] = &Auditor::auditBlockStatement;
  auditFnsMap[typeid(BlockExpression)] = &Auditor::auditBlockExpression;
  auditFnsMap[typeid(ExpressionStatement)] = &Auditor::auditExpressionStatement;
  auditFnsMap[typeid(InfixExpression)] = &Auditor::auditInfix;
  auditFnsMap[typeid(Identifier)] = &Auditor::auditIdentifier;
  auditFnsMap[typeid(AddressExpression)] = &Auditor::auditAddressExpression;
  auditFnsMap[typeid(DereferenceExpression)] =
      &Auditor::auditDereferenceExpression;

  auditFnsMap[typeid(AssignmentStatement)] = &Auditor::auditAssignmentStatement;
  auditFnsMap[typeid(FieldAssignment)] =
      &Auditor::auditFieldAssignmentStatement;
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
  if (!node) {
    logInternal("[CLASSIFY-NODE] Node is NULL, returning");
    return;
  }

  logInternal("[CLASSIFY-NODE] Entering with node type: " +
              std::string(typeid(*node).name()) +
              " | toString: " + node->toString());

  // Classify this node if it's a block
  if (auto blockStmt = dynamic_cast<BlockStatement *>(node)) {
    logInternal("[CLASSIFY-NODE] Detected BlockStatement");
    classifyBlock(blockStmt);
  } else if (auto blockExpr = dynamic_cast<BlockExpression *>(node)) {
    logInternal("[CLASSIFY-NODE] Detected BlockExpression");
    classifyBlock(blockExpr);
  } else {
    logInternal("[CLASSIFY-NODE] Not a block node");
  }

  // Recurse into children
  if (auto func = dynamic_cast<FunctionStatement *>(node)) {
    logInternal("[CLASSIFY-NODE] Recurse into FunctionStatement");
    classifyNode(func->funcExpr.get());
  } else if (auto funcExpr = dynamic_cast<FunctionExpression *>(node)) {
    logInternal("[CLASSIFY-NODE] Recurse into FunctionExpression block");
    classifyNode(funcExpr->block.get());
  } else if (auto whileStmt = dynamic_cast<WhileStatement *>(node)) {
    logInternal("[CLASSIFY-NODE] Recurse into WhileStatement loop");
    classifyNode(whileStmt->loop.get());
  } else if (auto forStmt = dynamic_cast<ForStatement *>(node)) {
    logInternal("[CLASSIFY-NODE] Recurse into ForStatement body");
    classifyNode(forStmt->body.get());
  } else if (auto ifStmt = dynamic_cast<ifStatement *>(node)) {
    logInternal("[CLASSIFY-NODE] Recurse into ifStatement");
    classifyNode(ifStmt->if_result.get());
    for (const auto &elif : ifStmt->elifClauses) {
      auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
      classifyNode(elifStmt->elif_result.get());
    }
    if (ifStmt->else_result.has_value()) {
      classifyNode(ifStmt->else_result.value().get());
    }
  } else if (auto switchStmt = dynamic_cast<SwitchStatement *>(node)) {
    logInternal("[CLASSIFY-NODE] → Recurse into SwitchStatement");
    classifySwitch(switchStmt);
  } else {
    logInternal("[CLASSIFY-NODE] No classification pass fort this node type");
  }
}

void Auditor::classifyBlock(Node *block) {
  logInternal("[CLASSIFY-BLOCK] Block pointer: " +
              std::to_string(reinterpret_cast<uintptr_t>(block)));
  logInternal("[CLASSIFY-BLOCK] Block toString: " + block->toString());

  // Ensure BlockInfo exists
  if (deferedFrees.find(block) == deferedFrees.end()) {
    deferedFrees[block] = std::make_unique<BlockInfo>();
    logInternal("[CLASSIFY-BLOCK] Created new BlockInfo for this block");
  } else {
    logInternal("[CLASSIFY-BLOCK] BlockInfo already exists for this block");
  }
  auto &info = deferedFrees[block];

  info->natives.clear();
  info->foreigners.clear();

  // Get all statements in this block
  std::vector<Node *> statements;
  if (auto blockStmt = dynamic_cast<BlockStatement *>(block)) {
    logInternal("[CLASSIFY-BLOCK] Block is BlockStatement, has " +
                std::to_string(blockStmt->statements.size()) + " statements");
    for (auto &stmt : blockStmt->statements) {
      statements.push_back(stmt.get());
    }
  } else if (auto blockExpr = dynamic_cast<BlockExpression *>(block)) {
    logInternal("[CLASSIFY-BLOCK] Block is BlockExpression, has " +
                std::to_string(blockExpr->statements.size()) + " statements");
    for (auto &stmt : blockExpr->statements) {
      statements.push_back(stmt.get());
    }
  } else {
    logInternal("[CLASSIFY-BLOCK] ERROR: Block is neither BlockStatement nor "
                "BlockExpression!");
    return;
  }

  logInternal("[CLASSIFY-BLOCK] Total statements to classify: " +
              std::to_string(statements.size()));

  // Classify each statement
  int stmtIndex = 0;
  for (Node *stmt : statements) {
    logInternal("[CLASSIFY-BLOCK] Processing statement " +
                std::to_string(stmtIndex++));
    Node *actualNode = peelNode(stmt);
    logInternal("[CLASSIFY-BLOCK] After peelNode: " + actualNode->toString());

    // Recurse into nested blocks
    if (auto whileStmt = dynamic_cast<WhileStatement *>(actualNode)) {
      logInternal("[CLASSIFY-BLOCK] Found WhileStatement, recursing");
      classifyNode(whileStmt->loop.get());
    } else if (auto forStmt = dynamic_cast<ForStatement *>(actualNode)) {
      logInternal("[CLASSIFY-BLOCK] Found ForStatement, recursing");
      classifyNode(forStmt->body.get());
    } else if (auto ifStmt = dynamic_cast<ifStatement *>(actualNode)) {
      logInternal("[CLASSIFY-BLOCK] Found ifStatement, recursing");
      classifyNode(ifStmt->if_result.get());
      for (const auto &elif : ifStmt->elifClauses) {
        auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
        classifyNode(elifStmt->elif_result.get());
      }
      if (ifStmt->else_result.has_value()) {
        classifyNode(ifStmt->else_result.value().get());
      }
    } else if (auto switchStmt = dynamic_cast<SwitchStatement *>(actualNode)) {
      logInternal("[CLASSIFY-BLOCK] Found SwitchStatement, recursing");
      classifySwitch(switchStmt);
    }

    // Get symbol
    auto sym = semantics.getSymbolFromMeta(actualNode);
    if (!sym || !sym->isHeap) {
      logInternal("[CLASSIFY-BLOCK] No heap symbol for node, skipping");
      continue;
    }

    logInternal("[CLASSIFY-BLOCK] Found heap symbol: " + sym->ID);

    // Determine if native (born here) or foreigner
    bool bornHere = semantics.isBornInScope(block, sym->ID);
    logInternal("[CLASSIFY-BLOCK] bornHere = " +
                std::string(bornHere ? "true" : "false"));

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
      } else {
        logInternal("[CLASSIFIER] " + sym->ID +
                    " does not die in this block, skipping");
      }
    }
  }

  logInternal("[CLASSIFY-BLOCK] Finished. Natives: " +
              std::to_string(info->natives.size()) +
              ", Foreigners: " + std::to_string(info->foreigners.size()));
}

void Auditor::classifyClause(
    SwitchStatement *sw, Node *stmt,
    const std::vector<std::unique_ptr<Statement>> &clause,
    const std::unique_ptr<BlockInfo> &blockInfo) {

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
  logInternal("[SHOULD-BUNKER] Checking block at: " +
              std::to_string((uintptr_t)block));

  auto it = deferedFrees.find(block);
  if (it == deferedFrees.end()) {
    logInternal("[SHOULD-BUNKER] No BlockInfo found for this block");
    return false;
  }

  if (!it->second) {
    logInternal("[SHOULD-BUNKER] BlockInfo is null");
    return false;
  }

  logInternal("[SHOULD-BUNKER] BlockInfo has " +
              std::to_string(it->second->natives.size()) + " natives");

  // A block needs bunkering if it has any natives
  bool result = !it->second->natives.empty();
  logInternal("[SHOULD-BUNKER] Returning: " +
              std::string(result ? "true" : "false"));
  return result;
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

void Auditor::bunkerCycles(Node *block) {
  logInternal("[BUNKER-CYCLES] Block pointer: " +
              std::to_string(reinterpret_cast<uintptr_t>(block)));
  logInternal("[BUNKER-CYCLES] Block toString: " + block->toString());

  // Check if blockInfo exists
  auto blockIt = deferedFrees.find(block);
  if (blockIt == deferedFrees.end()) {
    logInternal("[BUNKER-CYCLES] ERROR: No BlockInfo found for this block!");
    return;
  }

  auto &blockInfo = blockIt->second;
  if (!blockInfo) {
    logInternal("[BUNKER-CYCLES] ERROR: BlockInfo is null!");
    return;
  }

  logInternal("[BUNKER-CYCLES] BlockInfo has " +
              std::to_string(blockInfo->natives.size()) + " natives");
  logInternal("[BUNKER-CYCLES] BlockInfo has " +
              std::to_string(blockInfo->foreigners.size()) + " foreigners");

  // Log all natives
  for (const auto &id : blockInfo->natives) {
    logInternal("[BUNKER-CYCLES] Native ID: " + id);
  }
  for (const auto &id : blockInfo->foreigners) {
    logInternal("[BUNKER-CYCLES] Foreigner ID: " + id);
  }

  // Find cycle group
  logInternal("[BUNKER-CYCLES] Calling findCycleGroup...");
  auto cycleGroup = findCycleGroup(blockInfo);

  logInternal("[BUNKER-CYCLES] cycleGroup.ids.size() = " +
              std::to_string(cycleGroup.ids.size()));
  logInternal("[BUNKER-CYCLES] cycleGroup.hasNative = " +
              std::string(cycleGroup.hasNative ? "true" : "false"));
  logInternal("[BUNKER-CYCLES] cycleGroup.hasForeigner = " +
              std::string(cycleGroup.hasForeigner ? "true" : "false"));

  for (const auto &id : cycleGroup.ids) {
    logInternal("[BUNKER-CYCLES] cycleGroup ID: " + id);
  }

  if (cycleGroup.ids.empty()) {
    logInternal("[BUNKER-CYCLES] No cycle detected, nothing to bunker");
    return;
  }

  // Process each cycle ID
  int bunkeredCount = 0;
  for (const auto &validID : cycleGroup.ids) {
    logInternal("\n[BUNKER-CYCLES] Processing ID: " + validID);

    if (isBunkered(validID)) {
      logInternal("[CYCLE BUNKERING] Baton " + validID +
                  " already bunkered, skipping");
      continue;
    }
    logInternal("[BUNKER-CYCLES] ID " + validID + " not bunkered yet");

    Node *holder = semantics.queryForLifeTimeBaton(validID);
    if (!holder) {
      logInternal("[BUNKER-CYCLES] holder is NULL for " + validID);
      scanForBaton(validID);
      logInternal("[CYCLE BUNKERING] Could not find the baton holder for "
                  "lifetime ID:" +
                  validID + " skipping...");
      continue;
    }
    logInternal("[BUNKER-CYCLES] holder found: " + holder->toString());

    auto holderSym = semantics.getSymbolFromMeta(holder);
    if (!holderSym) {
      logInternal("[BUNKER-CYCLES] holderSym is NULL for holder: " +
                  holder->toString());
      logInternal(
          "[CYCLE BUNKERING] Failed to get the symbol info for holder " +
          holder->toString() + " for lifetime ID: " + validID + " skipping...");
      continue;
    }
    logInternal("[BUNKER-CYCLES] holderSym ID: " + holderSym->ID);

    auto &baton = semantics.responsibilityTable[holder];
    if (!baton) {
      logInternal("[BUNKER-CYCLES] baton is NULL for holder");
      scanForBaton(validID);
      logInternal("[CYCLE BUNKERING] Could not find the baton for "
                  "lifetime ID:" +
                  validID + " skipping...");
      continue;
    }
    logInternal("[BUNKER-CYCLES] baton ID: " + baton->ID);
    logInternal("[BUNKER-CYCLES] baton.isResponsible: " +
                std::string(baton->isResponsible ? "true" : "false"));

    // SUCCESS LOGS
    logInternal("[CYCLE BUNKERING] SUCCESS: Moving baton for " + validID +
                " into bunker.");

    nativesToFree[block].push_back({std::move(baton), holderSym});
    bunkeredIDs.insert(validID);
    bunkeredCount++;

    logInternal("[CYCLE BUNKERING] ID " + validID +
                " is now officially bunkered.");
  }

  logInternal("[BUNKER-CYCLES] Total bunkered: " +
              std::to_string(bunkeredCount));
  logInternal("[BUNKER-CYCLES] nativesToFree size for this block: " +
              std::to_string(nativesToFree[block].size()));
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
  if (dynamic_cast<VariableDeclaration *>(node))
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
  auto auditIt = auditFnsMap.find(typeid(*node));
  if (auditIt == auditFnsMap.end()) {
    return;
  }
  logInternal("[AUDITING] Node: " + node->toString());
  (this->*auditIt->second)(node);
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

  // EXPRESSIONS
  if (auto *addr = dynamic_cast<AddressExpression *>(root)) {
    if (containsNode(addr->identifier.get(), target))
      return true;
  }

  if (auto *deref = dynamic_cast<DereferenceExpression *>(root)) {
    if (containsNode(deref->identifier.get(), target))
      return true;
  }

  if (auto *move = dynamic_cast<MoveExpression *>(root)) {
    if (containsNode(move->expr.get(), target))
      return true;
  }

  if (auto *self = dynamic_cast<SelfExpression *>(root)) {
    for (auto &field : self->fields) {
      if (containsNode(field.get(), target))
        return true;
    }
  }

  if (auto *newComp = dynamic_cast<NewComponentExpression *>(root)) {
    for (auto &arg : newComp->arguments) {
      if (containsNode(arg.get(), target))
        return true;
    }
  }

  if (auto *call = dynamic_cast<CallExpression *>(root)) {
    if (containsNode(call->function_identifier.get(), target))
      return true;
    for (auto &param : call->parameters) {
      if (containsNode(param.get(), target))
        return true;
    }
  }

  if (auto *unwrap = dynamic_cast<UnwrapExpression *>(root)) {
    if (containsNode(unwrap->expr.get(), target))
      return true;
  }

  if (auto *method = dynamic_cast<MethodCallExpression *>(root)) {
    if (containsNode(method->instance.get(), target))
      return true;
    if (containsNode(method->call.get(), target))
      return true;
  }

  if (auto *instance = dynamic_cast<InstanceExpression *>(root)) {
    if (instance->blockIdent) {
      if (containsNode(instance->blockIdent.get(), target))
        return true;
    }
    for (auto &field : instance->fields) {
      if (containsNode(field.get(), target))
        return true;
    }
  }

  if (auto *funcExpr = dynamic_cast<FunctionExpression *>(root)) {
    for (auto &param : funcExpr->call) {
      if (containsNode(param.get(), target))
        return true;
    }
    if (funcExpr->return_type) {
      if (containsNode(funcExpr->return_type.get(), target))
        return true;
    }
    if (funcExpr->block) {
      if (containsNode(funcExpr->block.get(), target))
        return true;
    }
  }

  if (auto *cast = dynamic_cast<CastExpression *>(root)) {
    if (cast->expr && containsNode(cast->expr.get(), target))
      return true;
  }

  if (auto *bitcast = dynamic_cast<BitcastExpression *>(root)) {
    if (bitcast->expr && containsNode(bitcast->expr.get(), target))
      return true;
  }

  if (auto *prefix = dynamic_cast<PrefixExpression *>(root)) {
    if (containsNode(prefix->operand.get(), target))
      return true;
  }

  if (auto *postfix = dynamic_cast<PostfixExpression *>(root)) {
    if (containsNode(postfix->operand.get(), target))
      return true;
  }

  if (auto *infix = dynamic_cast<InfixExpression *>(root)) {
    if (containsNode(infix->left_operand.get(), target))
      return true;
    if (containsNode(infix->right_operand.get(), target))
      return true;
  }

  if (auto *arrayLit = dynamic_cast<ArrayLiteral *>(root)) {
    for (auto &item : arrayLit->array) {
      if (containsNode(item.get(), target))
        return true;
    }
  }

  if (auto *subscript = dynamic_cast<ArraySubscript *>(root)) {
    if (subscript->identifier &&
        containsNode(subscript->identifier.get(), target))
      return true;
    for (auto &index : subscript->index_exprs) {
      if (containsNode(index.get(), target))
        return true;
    }
  }

  // STATEMENTS
  if (auto *exprStmt = dynamic_cast<ExpressionStatement *>(root)) {
    if (containsNode(exprStmt->expression.get(), target))
      return true;
  }

  if (auto *decl = dynamic_cast<VariableDeclaration *>(root)) {
    if (decl->initializer && containsNode(decl->initializer.get(), target))
      return true;
  }

  if (auto *assign = dynamic_cast<AssignmentStatement *>(root)) {
    if (containsNode(assign->identifier.get(), target))
      return true;
    if (containsNode(assign->value.get(), target))
      return true;
  }

  if (auto *fieldAssign = dynamic_cast<FieldAssignment *>(root)) {
    if (containsNode(fieldAssign->lhs_chain.get(), target))
      return true;
    if (containsNode(fieldAssign->value.get(), target))
      return true;
  }

  if (auto *ret = dynamic_cast<ReturnStatement *>(root)) {
    if (ret->return_value && containsNode(ret->return_value.get(), target))
      return true;
  }

  if (auto *trace = dynamic_cast<TraceStatement *>(root)) {
    if (containsNode(trace->expr.get(), target))
      return true;
  }

  if (auto *ifStmt = dynamic_cast<ifStatement *>(root)) {
    if (ifStmt->condition && containsNode(ifStmt->condition.get(), target))
      return true;
    if (ifStmt->if_result && containsNode(ifStmt->if_result.get(), target))
      return true;
    for (const auto &elif : ifStmt->elifClauses) {
      if (containsNode(elif.get(), target))
        return true;
    }
    if (ifStmt->else_result.has_value() && ifStmt->else_result.value()) {
      if (containsNode(ifStmt->else_result.value().get(), target))
        return true;
    }
  }

  if (auto *elif = dynamic_cast<elifStatement *>(root)) {
    if (elif->elif_condition &&
        containsNode(elif->elif_condition.get(), target))
      return true;
    if (elif->elif_result && containsNode(elif->elif_result.get(), target))
      return true;
  }

  if (auto *switchStmt = dynamic_cast<SwitchStatement *>(root)) {
    if (switchStmt->switch_init &&
        containsNode(switchStmt->switch_init.get(), target))
      return true;
    for (auto &caseClause : switchStmt->case_clauses) {
      if (containsNode(caseClause.get(), target))
        return true;
    }
    for (auto &stmt : switchStmt->default_statements) {
      if (containsNode(stmt.get(), target))
        return true;
    }
  }

  if (auto *caseClause = dynamic_cast<CaseClause *>(root)) {
    if (caseClause->condition &&
        containsNode(caseClause->condition.get(), target))
      return true;
    for (auto &stmt : caseClause->body) {
      if (containsNode(stmt.get(), target))
        return true;
    }
  }

  if (auto *whileStmt = dynamic_cast<WhileStatement *>(root)) {
    if (whileStmt->condition &&
        containsNode(whileStmt->condition.get(), target))
      return true;
    if (whileStmt->loop && containsNode(whileStmt->loop.get(), target))
      return true;
  }

  if (auto *forStmt = dynamic_cast<ForStatement *>(root)) {
    if (forStmt->initializer &&
        containsNode(forStmt->initializer.get(), target))
      return true;
    if (forStmt->condition && containsNode(forStmt->condition.get(), target))
      return true;
    if (forStmt->step && containsNode(forStmt->step.get(), target))
      return true;
    if (forStmt->body && containsNode(forStmt->body.get(), target))
      return true;
  }

  if (auto *blockStmt = dynamic_cast<BlockStatement *>(root)) {
    for (auto &stmt : blockStmt->statements) {
      Node *peeled = peelNode(stmt.get());
      if (containsNode(peeled, target))
        return true;
    }
  }

  if (auto *blockExpr = dynamic_cast<BlockExpression *>(root)) {
    for (auto &stmt : blockExpr->statements) {
      if (containsNode(stmt.get(), target))
        return true;
    }
    if (blockExpr->finalexpr.has_value() && blockExpr->finalexpr.value()) {
      if (containsNode(blockExpr->finalexpr.value().get(), target))
        return true;
    }
  }

  if (auto *funcStmt = dynamic_cast<FunctionStatement *>(root)) {
    if (funcStmt->funcExpr && containsNode(funcStmt->funcExpr.get(), target))
      return true;
  }

  if (auto *funcDecl = dynamic_cast<FunctionDeclaration *>(root)) {
    if (funcDecl->function_name &&
        containsNode(funcDecl->function_name.get(), target))
      return true;
    for (auto &param : funcDecl->parameters) {
      if (containsNode(param.get(), target))
        return true;
    }
    if (funcDecl->return_type &&
        containsNode(funcDecl->return_type.get(), target))
      return true;
  }

  if (auto *funcDeclExpr =
          dynamic_cast<FunctionDeclarationExpression *>(root)) {
    if (funcDeclExpr->funcDeclrStmt &&
        containsNode(funcDeclExpr->funcDeclrStmt.get(), target))
      return true;
  }

  if (auto *comp = dynamic_cast<ComponentStatement *>(root)) {
    for (auto &method : comp->methods) {
      if (containsNode(method.get(), target))
        return true;
    }

    if (comp->initConstructor.has_value() && comp->initConstructor.value()) {
      if (containsNode(comp->initConstructor.value().get(), target))
        return true;
    }
  }

  if (auto *init = dynamic_cast<InitStatement *>(root)) {
    for (auto &arg : init->constructor_args) {
      if (containsNode(arg.get(), target))
        return true;
    }
    if (init->block && containsNode(init->block.get(), target))
      return true;
  }

  if (auto *seal = dynamic_cast<SealStatement *>(root)) {
    if (seal->block && containsNode(seal->block.get(), target))
      return true;
  }

  if (auto *generic = dynamic_cast<GenericStatement *>(root)) {
    if (generic->block && containsNode(generic->block.get(), target))
      return true;
  }

  if (auto *alloc = dynamic_cast<AllocatorStatement *>(root)) {
    if (alloc->block && containsNode(alloc->block.get(), target))
      return true;
  }

  return false;
}

bool Auditor::isBunkered(const std::string &id) {
  return bunkeredIDs.find(id) != bunkeredIDs.end();
}

void Auditor::simulateDeclFree(Node *contextNode,
                               const std::string &contextID) {
  logInternal("\n  [SIMULATE-FREE] Target: " + contextID);
  if (inhibit) {
    logInternal("Inhibiting freeing simulation");
    return;
  }
  if (isBunkered(contextID)) {
    logInternal("    [SIMULATE FREE] " + contextID +
                " is bunkered. Skipping freeing simulation.");
    return;
  }

  Node *holderNode = semantics.queryForLifeTimeBaton(contextID);
  if (!holderNode) {
    logInternal("    [DECL-SIMULATE FREE] No holder node found for " +
                contextID);
    return;
  }

  // Check if the holder node is the actual contextNode
  if (holderNode != contextNode) {
    logInternal("[DECL-SIMULATE FREE] The baton was moved node: " +
                contextNode->toString() + "' is not the executor");
    return;
  }

  auto &baton = semantics.responsibilityTable[holderNode];
  auto contextSym = semantics.getSymbolFromMeta(holderNode);

  logInternal("   [DECL-SIMULATE FREE] Baton ID: " + baton->ID);
  logInternal("   [DECL-SIMULATE FREE] State: isResponsible=" +
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

void Auditor::simulateFree(Node *contextNode, const std::string &contextID) {
  logInternal("\n  [SIMULATE-FREE] Target: " + contextID);
  if (inhibit) {
    logInternal("Inhibiting freeing simulation");
    return;
  }
  if (isBunkered(contextID)) {
    logInternal("    [SIMULATE FREE] " + contextID +
                " is bunkered. Skipping freeing simulation.");
    return;
  }

  Node *holderNode = semantics.queryForLifeTimeBaton(contextID);
  if (!holderNode) {
    logInternal("    [SIMULATE FREE] No holder node found for " + contextID);
    return;
  }

  auto identifiers = semantics.digIdentifiers(contextNode);
  for (const auto &identifier : identifiers) {
    // Check if the holder node is the actual contextNode
    if (holderNode != identifier) {
      logInternal("[SIMULATE FREE] The baton was moved node: " +
                  identifier->toString() + "' is not the executor");
      return;
    }

    auto &baton = semantics.responsibilityTable[holderNode];
    auto contextSym = semantics.getSymbolFromMeta(holderNode);

    logInternal("   [SIMULATE FREE] Baton ID: " + baton->ID);
    logInternal("   [SIMULATE FREE] State: isResponsible=" +
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
}

void Auditor::assignmentRob(Node *contextAssign,
                            const std::shared_ptr<SymbolInfo> &assignSym) {
  logInternal("[ASSIGNMENT HEIST] Carrying out an assignment heist");

  // Get the RHS value node
  Node *valNode = nullptr;
  Node *lhsNode = nullptr;

  if (auto assignStmt = dynamic_cast<AssignmentStatement *>(contextAssign)) {
    valNode = assignStmt->value.get();
    lhsNode = assignStmt->identifier.get();
  } else if (auto fieldAssign =
                 dynamic_cast<FieldAssignment *>(contextAssign)) {
    valNode = fieldAssign->value.get();
    lhsNode = fieldAssign->lhs_chain.get();
  }

  if (!valNode || !lhsNode)
    reportDevBug("Failed to get nodes for assignment", contextAssign);

  // DIG THE LHS to get the struct identifier (the robber)
  auto lhsIdentifiers = semantics.digIdentifiers(lhsNode);
  if (lhsIdentifiers.empty()) {
    logInternal("[ASSIGNMENT HEIST] No LHS identifier found");
    return;
  }

  auto lhsIdent = lhsIdentifiers[0]; // The struct (a or b)
  auto lhsSym = semantics.getSymbolFromMeta(lhsIdent);
  if (!lhsSym) {
    logInternal("[ASSIGNMENT HEIST] No symbol for LHS identifier");
    return;
  }

  // Get the robber baton (L0 or L1)
  Node *robberHolder = semantics.queryForLifeTimeBaton(lhsSym->ID);
  if (!robberHolder) {
    logInternal("[ASSIGNMENT HEIST] No robber baton found for " + lhsSym->ID);
    return;
  }

  auto &robberBaton = semantics.responsibilityTable[robberHolder];
  if (!robberBaton) {
    logInternal("[ASSIGNMENT HEIST] Robber baton is null");
    return;
  }

  logInternal("[ASSIGNMENT HEIST] Robber = " + robberBaton->ID +
              " (from LHS: " + lhsIdent->toString() + ")");

  // Now process RHS identifiers (the victim)
  auto identifiers = semantics.digIdentifiers(valNode);
  for (const auto &identifier : identifiers) {
    auto valSym = semantics.getSymbolFromMeta(identifier);
    if (!valSym) {
      reportDevBug("Failed to get value assignment node", identifier);
    }

    if (assignSym->isPointer) {
      valSym->pointerCount++;
      logInternal("  [ASSIGNMENT HEIST] Incrementing pointerCount for " +
                  valSym->ID + " to " + std::to_string(valSym->pointerCount));
    }

    if (valSym->isHeap) {
      Node *valBatonHolder = semantics.queryForLifeTimeBaton(valSym->ID);
      auto &valBaton = semantics.responsibilityTable[valBatonHolder];
      if (valBatonHolder == identifier) {
        logInternal(
            "  [ASSIGNMENT HEIST] ValBaton " + valBaton->ID +
            " isResponsible: " + (valBaton->isResponsible ? "YES" : "NO"));

        if (valBaton->isResponsible) {
          logInternal("  [ASSIGNMENT HEIST] Attempting Robbery: " +
                      robberBaton->ID + " robbing " + valBaton->ID);
          semantics.transferResponsibility(robberBaton.get(), valBaton.get(),
                                           valSym);
        } else {
          logInternal("  [ASSIGNMENT HEIST] Already robbed. Registering " +
                      assignSym->ID + " as candidate for " + valSym->ID);
          candidateRegistry[valBaton->ID].push_back(assignSym->ID);
        }
      } else {
        // Force a robbery
        logInternal("  [ASSIGNMENT HEIST] valBatonHolder is not current value "
                    "node. attempting a spirit robbery");
        semantics.transferResponsibility(robberBaton.get(), valBaton.get(),
                                         valSym);
      }
    }
  }

  auto currentBlock = getCurrentBlock();
  if (!currentBlock) {
    logInternal("[ASSIGNMENT HEIST] No current block, skipping cycle check");
    return;
  }
  logInternal("[ASSIGNMENT HEIST] currentBlock = " +
              (currentBlock ? currentBlock->toString() : "NULL"));

  auto &info = deferedFrees[currentBlock];
  if (!info) {
    logInternal("[ASSIGNMENT HEIST] No BlockInfo for current block, skipping "
                "cycle check");
    return;
  }

  checkCycleSafety(info, contextAssign);
  if (shouldNativeBunkerBlock(currentBlock)) {
    logInternal("[ASSIGNMENT HEIST] ABOUT TO CALL bunkerCycles");
    bunkerCycles(currentBlock);
    logInternal("[ASSIGNMENT HEIST] RETURNED FROM bunkerCycles");
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
  logInternal("[SCANNER] Performing global baton scan for lifetime ID: " + id);

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
    logInternal("[SCANNER] LAYER 2: Baton " + id +
                " NOT FOUND in nativesToFree");
  }

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

bool Auditor::isCycle(LifeTime *baton) {
  logInternal("\n[IS-CYCLE] Starting cycle detection for baton: " + baton->ID);
  logInternal("[IS-CYCLE] baton.isResponsible = " +
              std::string(baton->isResponsible ? "true" : "false"));
  logInternal("[IS-CYCLE] baton.dependents.size() = " +
              std::to_string(baton->dependents.size()));

  // Log dependents
  for (const auto &[depID, depSym] : baton->dependents) {
    logInternal("[IS-CYCLE]   Dependent: " + depID);
  }

  // Clear the cycle data before we detect a new one
  cycle.clear();
  std::set<std::string> visited;
  bool hasCycle = followDependents(baton, baton->ID, visited);

  if (hasCycle) {
    logInternal("[IS-CYCLE] CYCLE FOUND for " + baton->ID);
    logInternal("[CYCLE-DETECTED] Cycle group:");
    for (const auto &id : cycle) {
      logInternal("  → " + id);
    }
  } else {
    logInternal("[IS-CYCLE] No cycle for " + baton->ID);
  }

  return hasCycle;
}

bool Auditor::followDependents(LifeTime *current, const std::string &originalID,
                               std::set<std::string> &visited) {
  if (!current) {
    logInternal("[FOLLOW] current is NULL, returning false");
    return false;
  }

  logInternal("[FOLLOW] Current: " + current->ID + " (dependents: " +
              std::to_string(current->dependents.size()) + ")");

  // If we've seen this baton before
  if (visited.count(current->ID)) {
    logInternal("[FOLLOW] Already visited: " + current->ID);
    // If it's the original, we found the cycle!
    if (current->ID == originalID) {
      logInternal("[FOLLOW] Found cycle back to original: " + originalID);
      return true;
    }
    logInternal("[FOLLOW] Not original, no cycle");
    return false;
  }

  visited.insert(current->ID);
  cycle.push_back(current->ID);
  logInternal("[FOLLOW] Added to visited and cycle. visited size: " +
              std::to_string(visited.size()) +
              ", cycle size: " + std::to_string(cycle.size()));

  // Follow dependents (what this baton points to)
  int depIndex = 0;
  for (const auto &[depID, depSym] : current->dependents) {
    logInternal("[FOLLOW] Checking dependent " + std::to_string(depIndex++) +
                ": " + depID);
    auto *depBaton = semantics.getBaton(depID);
    if (!depBaton) {
      logInternal("[FOLLOW]   depBaton is NULL, skipping");
      continue;
    }
    logInternal("[FOLLOW]   Calling followDependents on " + depID);
    if (followDependents(depBaton, originalID, visited)) {
      logInternal("[FOLLOW]   Cycle found through " + depID);
      return true;
    }
    logInternal("[FOLLOW]   No cycle through " + depID);
  }

  // No cycle found on this path, backtrack
  logInternal("[FOLLOW] Backtracking from " + current->ID);
  cycle.pop_back();
  visited.erase(current->ID);
  logInternal(
      "[FOLLOW] After backtrack - cycle size: " + std::to_string(cycle.size()) +
      ", visited size: " + std::to_string(visited.size()));
  return false;
}

const CycleGroup
Auditor::findCycleGroup(const std::unique_ptr<BlockInfo> &blockInfo) {
  CycleGroup group;

  if (!blockInfo) {
    logInternal("[FIND-CYCLE] BlockInfo is null, returning empty group");
    return group;
  }

  logInternal("[FIND-CYCLE] BlockInfo has " +
              std::to_string(blockInfo->natives.size()) + " natives");
  logInternal("[FIND-CYCLE] BlockInfo has " +
              std::to_string(blockInfo->foreigners.size()) + " foreigners");

  std::set<std::string> processed;

  std::vector<std::string> allBatons;
  allBatons.insert(allBatons.end(), blockInfo->natives.begin(),
                   blockInfo->natives.end());
  allBatons.insert(allBatons.end(), blockInfo->foreigners.begin(),
                   blockInfo->foreigners.end());

  logInternal("[FIND-CYCLE] Total batons to check: " +
              std::to_string(allBatons.size()));

  int batonIndex = 0;
  for (const auto &id : allBatons) {
    logInternal("\n[FIND-CYCLE] Checking baton " +
                std::to_string(batonIndex++) + ": ID = " + id);

    if (processed.count(id)) {
      logInternal("[FIND-CYCLE] Already processed, skipping");
      continue;
    }
    logInternal("[FIND-CYCLE] Not processed yet, checking for cycle");

    auto *baton = semantics.getBaton(id);
    if (!baton) {
      logInternal("[FIND-CYCLE] getBaton returned NULL for " + id);
      continue;
    }
    logInternal("[FIND-CYCLE] Got baton, calling isCycle...");

    bool hasCycle = isCycle(baton);
    logInternal("[FIND-CYCLE] isCycle returned: " +
                std::string(hasCycle ? "true" : "false"));

    if (hasCycle) {
      logInternal("[FIND-CYCLE] CYCLE FOUND for " + id);
      logInternal("[FIND-CYCLE] Cycle size: " + std::to_string(cycle.size()));

      for (const auto &cycleID : cycle) {
        logInternal("[FIND-CYCLE] Processing cycle member: " + cycleID);
        group.ids.push_back(cycleID);
        logInternal("[FIND-CYCLE]   Added to group.ids");

        // Check if native
        if (std::find(blockInfo->natives.begin(), blockInfo->natives.end(),
                      cycleID) != blockInfo->natives.end()) {
          group.hasNative = true;
          logInternal("[FIND-CYCLE]   This is a NATIVE");
        }
        // Check if foreigner
        if (std::find(blockInfo->foreigners.begin(),
                      blockInfo->foreigners.end(),
                      cycleID) != blockInfo->foreigners.end()) {
          group.hasForeigner = true;
          logInternal("[FIND-CYCLE]   This is a FOREIGNER");
        }

        processed.insert(cycleID);
        logInternal("[FIND-CYCLE]   Added to processed set");
      }
      cycle.clear();
      logInternal(
          "[FIND-CYCLE] Cycle processed, cleared temporary cycle vector");
    } else {
      logInternal("[FIND-CYCLE] No cycle for " + id);
      processed.insert(id);
      logInternal("[FIND-CYCLE] Added " + id + " to processed (non-cycle)");
    }
  }

  logInternal("\n[FIND-CYCLE] FINAL RESULT:");
  logInternal("[FIND-CYCLE] group.ids.size() = " +
              std::to_string(group.ids.size()));
  logInternal("[FIND-CYCLE] group.hasNative = " +
              std::string(group.hasNative ? "true" : "false"));
  logInternal("[FIND-CYCLE] group.hasForeigner = " +
              std::string(group.hasForeigner ? "true" : "false"));

  for (const auto &id : group.ids) {
    logInternal("[FIND-CYCLE] Final group ID: " + id);
  }

  return group;
}

void Auditor::checkCycleSafety(const std::unique_ptr<BlockInfo> &blockInfo,
                               Node *contextNode) {
  auto cycleGroup = findCycleGroup(blockInfo);
  logInternal("[CYCLE CHECK] Checking cycle safety");

  std::string nativeList = getIDsString(cycleGroup.ids, blockInfo->natives);
  std::string foreignerList =
      getIDsString(cycleGroup.ids, blockInfo->foreigners);

  if (cycleGroup.hasNative && cycleGroup.hasForeigner) {
    errorHandler.addHint(
        "Cannot create cycle between native and foreigner lifetimes\n"
        "\n"
        "  Native batons in cycle:    " +
        nativeList +
        "\n"
        "  Foreigner batons in cycle: " +
        foreignerList +
        "\n"
        "\n"
        "  Why: Native batons die with their block, foreigner batons live "
        "across blocks.\n"
        "       A cycle between them cannot be freed safely.\n"
        "\n"
        "  Fix:\n"
        "    • Ensure all batons in the cycle are from the same block\n"
        "    • Use weak references to break the cycle\n"
        "    • Restructure your code to avoid the cycle");
    logAuditError(
        "Illegal cycle detected between native and foreigner lifetimes",
        contextNode);
  }
}

std::string Auditor::getIDsString(const std::vector<std::string> &ids,
                                  const std::vector<std::string> &filter) {
  std::string result;
  for (const auto &id : ids) {
    if (std::find(filter.begin(), filter.end(), id) != filter.end()) {
      if (!result.empty())
        result += ", ";
      result += id;
    }
  }
  return result.empty() ? "none" : result;
}

Node *Auditor::getCurrentBlock() {
  if (activeBlocks.empty())
    return nullptr;

  return activeBlocks.back();
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
  error.tokenLength = errorHandler.getTokenLength(contextNode);
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
  error.tokenLength = errorHandler.getTokenLength(contextNode);
  error.hints = {};

  errorHandler.report(error);

  std::abort();
}

bool Auditor::failed() { return hasFailed; }
