#include "defs.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include <memory>
std::string
Semantics::generateLifetimeID(const std::shared_ptr<SymbolInfo> &sym) {
  if (sym->type().isPointer)
    return "P" + std::to_string(ptrDeclCount++);
  else if (sym->type().isArray)
    return "A" + std::to_string(arrDeclCount++);
  else
    return "N" + std::to_string(normalDeclCount++);

  return "NO ID";
}

std::unique_ptr<LifeTime>
Semantics::createLifeTimeTracker(Node *declarationNode, Node *initializer,
                                 const std::shared_ptr<SymbolInfo> &declSym) {
  logInternal("Creating lifetime baton...");
  auto lifetime = std::make_unique<LifeTime>();
  lifetime->ID = generateLifetimeID(declSym);
  lifetime->isResponsible = true;
  lifetime->isAlive = true;
  lifetime->persist = declSym->storage().isPersist;

  auto idents = digIdentifiers(initializer);
  for (const auto &identifier : idents) {
    auto identSym = getSymbolFromMeta(identifier);
    if (!identSym) {
      logInternal("Failed to get identifier symbol info during lifetime "
                  "creation skipping....");
      continue;
    }

    if (!identSym->storage().isHeap) {
      logInternal("Encountered non heap ident skipping...");
      continue;
    }

    auto targetBaton = getBaton(identSym->codegen().ID);
    if (!targetBaton)
      reportDevBug("Failed to get lifetime baton for ident of lifetime ID: " +
                       identSym->codegen().ID,
                   identifier);

    transferResponsibility(lifetime.get(), targetBaton, identSym);
  }

  Node *currentBlock = getCurrentBlock();
  if (currentBlock) {
    bornInBlock[currentBlock].push_back(lifetime->ID);
    logInternal("[BORN] Baton " + lifetime->ID +
                " born in block: " + currentBlock->toString());
  } else {
    if (isGlobalScope()) {
      logSemanticErrors(ErrorCode::GlobalHeapVar, declarationNode,
                        {extractDeclarationName(declarationNode)});
    }
  }

  logInternal("Created baton: " + lifetime->ID +
              " for Node: " + declarationNode->toString());
  return lifetime;
}

LifeTime *Semantics::getBaton(const std::string &ID) {
  // Primary Attempt: Use the ID to find the ORIGINAL holder node
  Node *holder = queryForLifeTimeBaton(ID);

  if (holder) {
    auto it = responsibilityTable.find(holder);
    if (it != responsibilityTable.end()) {
      return it->second.get();
    }
  } else {
    logInternal("[BATON GETTER] Cannot find baton holder node for ID: " + ID);
  }

  // Fallback, Deep Search by ID
  for (auto const &[node, baton] : responsibilityTable) {
    if (baton && baton->ID == ID) {
      logInternal("[BATON GETTER] Found via Deep Search for ID: " + ID);
      return baton.get();
    }
  }

  logInternal("[BATON GETTER] Total Failure: ID " + ID + " exists nowhere.");
  return nullptr;
}

void Semantics::transferResponsibility(
    LifeTime *currentBaton, LifeTime *targetBaton,
    const std::shared_ptr<SymbolInfo> &targetSym) {
  if (!targetBaton) {
    logInternal(
        "[HEIST] Failed to carry out heist due to missing target baton");
    return;
  }

  if (!currentBaton) {
    logInternal("[HEIST] Failed to carry out normal heist a non heap variable "
                "might have been "
                "encountered");
    if (targetSym->storage().isHeap) {
      logInternal("[HEIST] Carrying out heist registration");
      heistIDs.insert(targetSym->codegen().ID);
    }
    return;
  }

  logInternal("[HEIST] Robber (Current): " + currentBaton->ID);
  logInternal("[HEIST] Victim (Target): " + targetBaton->ID);
  logInternal("[HEIST] Target Pointer Count: " +
              std::to_string(targetSym->storage().pointerCount));

  if (currentBaton->ID == targetBaton->ID) {
    logInternal("[HEIST] SKIPPED: Cannot create self-dependency");
    return;
  }

  if (!targetSym->storage().isHeap) {
    logInternal("[HEIST] The victim is not heap raised dont bother");
    return;
  }

  // Assign the baton pointer count
  targetBaton->ptrCount = targetSym->storage().pointerCount;

  auto targetPointerCount = targetBaton->ptrCount;

  // The moment of truth: Can we rob it?
  if (targetPointerCount <= 2) {
    logInternal("[HEIST] SUCCESS: Robbery approved. Disarming " +
                targetBaton->ID);

    targetBaton->isResponsible = false;      // The Victim is now impotent
    targetBaton->ownedBy = currentBaton->ID; // The Master-Slave link

    // The Robber takes the specific target
    currentBaton->dependents[targetBaton->ID] = targetSym;

    // THE LOOT: Robber takes all the victim's existing dependents too
    for (auto const &[id, sym] : targetBaton->dependents) {
      logInternal("[HEIST] COLLECTING LOOT: Robber " + currentBaton->ID +
                  " now owns " + id);
      currentBaton->dependents[id] = sym;
    }
  } else {
    logInternal("[HEIST] FAILURE: Robbery vetoed. Pointer count (" +
                std::to_string(targetPointerCount) + ") is too high.");
  }

  logInternal("[HEIST] Victim " + targetBaton->ID + " isResponsible = " +
              (targetBaton->isResponsible ? "TRUE" : "FALSE") + " ---");
}

const std::unique_ptr<LifeTime> &
Semantics::readBatonInfo(const std::string &batonID) {
  for (const auto &[node, baton] : responsibilityTable) {
    if (baton && baton->ID == batonID) {
      return baton;
    }
  }

  static const std::unique_ptr<LifeTime> nullBaton = nullptr;
  return nullBaton;
}

Node *Semantics::queryForLifeTimeBaton(const std::string &familyID) {
  for (const auto &[node, baton] : responsibilityTable) {
    // If the node has the briefcase
    if (baton) {
      // Check if we are in the same family
      if (familyID == baton->ID) {
        // If we are please tell me the node holding it
        return node;
      }
      continue;
    }
    continue;
  }
  return nullptr;
}

void Semantics::transferBaton(Node *receiver, const std::string &familyID) {
  Node *holder = queryForLifeTimeBaton(familyID);
  if (!holder) {
    reportDevBug("Could not find the baton holder for lifetime ID: " + familyID,
                 receiver);
  }
  if (!dynamic_cast<Identifier *>(receiver)) {
    logInternal("[BATON TRANSFER] Transfering baton of ID: " + familyID +
                " from node: " + holder->toString() +
                " to composite node: " + receiver->toString());
  }
  std::string identName = "<no name>";
  auto identifiers = digIdentifiers(receiver);
  for (const auto &identifier : identifiers) {
    identName = extractIdentifierName(identifier);
    auto identSym = getSymbolFromMeta(identifier);
    if (!identSym) {
      reportDevBug("Failed to get symbol info for identifer node during baton "
                   "transfer '" +
                       identName + "'",
                   identifier);
    }

    if (!identSym->storage().isHeap) {
      logInternal("[BATON TRANSFER] The identifier '" + identName +
                  "' is not heap allocated skipping..");
    }
    // Ensure we give the baton to the correct identifier
    if (familyID == identSym->codegen().ID) {
      responsibilityTable[identifier] = std::move(responsibilityTable[holder]);
    }
  }
}

bool Semantics::isBornInScope(Node *root, const std::string &ID) {
  auto it = bornInBlock.find(root);
  if (it == bornInBlock.end()) {
    return false;
  }

  for (const std::string &id : it->second) {
    if (id == ID) {
      return true;
    }
  }
  return false;
}

std::vector<Identifier *> Semantics::digIdentifiers(Node *node) {
  std::vector<Identifier *> results;

  if (!node)
    return results;

  // Base case: node is already an identifier
  if (auto *ident = dynamic_cast<Identifier *>(node)) {
    results.push_back(ident);
    return results;
  }

  // AddressExpression (addr b)
  if (auto *addr = dynamic_cast<AddressExpression *>(node)) {
    return digIdentifiers(addr->identifier.get());
  }

  // DereferenceExpression (deref p)
  if (auto *deref = dynamic_cast<DereferenceExpression *>(node)) {
    return digIdentifiers(deref->identifier.get());
  }

  // SelfExpression (self.field)
  if (auto *self = dynamic_cast<SelfExpression *>(node)) {
    for (auto &field : self->fields) {
      auto fieldIds = digIdentifiers(field.get());
      results.insert(results.end(), fieldIds.begin(), fieldIds.end());
    }
    return results;
  }

  // Subscript expression
  if (auto *arrSub = dynamic_cast<ArraySubscript *>(node)) {
    if (auto *ident = dynamic_cast<Identifier *>(arrSub->identifier.get()))
      results.push_back(ident);

    for (auto &lens : arrSub->index_exprs) {
      auto lenIds = digIdentifiers(lens.get());
      results.insert(results.end(), lenIds.begin(), lenIds.end());
    }
    return results;
  }

  // NewComponentExpression (new Player(args))
  if (auto *newComp = dynamic_cast<NewComponentExpression *>(node)) {
    for (auto &arg : newComp->arguments) {
      auto argIds = digIdentifiers(arg.get());
      results.insert(results.end(), argIds.begin(), argIds.end());
    }
    return results;
  }

  // CallExpression (func(x, y))
  if (auto *call = dynamic_cast<CallExpression *>(node)) {
    if (auto callIdent =
            dynamic_cast<Identifier *>(call->function_identifier.get()))
      results.push_back(callIdent);

    for (auto &arg : call->parameters) {
      auto argIds = digIdentifiers(arg.get());
      results.insert(results.end(), argIds.begin(), argIds.end());
    }
    return results;
  }

  // UnwrapExpression (unwrap x)
  if (auto *unwrap = dynamic_cast<UnwrapExpression *>(node)) {
    return digIdentifiers(unwrap->expr.get());
  }

  // InstanceExpression (Type { fields })
  if (auto *instance = dynamic_cast<InstanceExpression *>(node)) {
    if (instance->blockIdent) {
      auto blockIds = digIdentifiers(instance->blockIdent.get());
      results.insert(results.end(), blockIds.begin(), blockIds.end());
    }
    for (auto &field : instance->fields) {
      auto fieldIds = digIdentifiers(field.get());
      results.insert(results.end(), fieldIds.begin(), fieldIds.end());
    }
    return results;
  }

  // CastExpression (cast<i32>(x))
  if (auto *cast = dynamic_cast<CastExpression *>(node)) {
    if (cast->expr) {
      auto exprIds = digIdentifiers(cast->expr.get());
      results.insert(results.end(), exprIds.begin(), exprIds.end());
    }
    return results;
  }

  // BitcastExpression (bitcast<ptr>(x))
  if (auto *bitcast = dynamic_cast<BitcastExpression *>(node)) {
    if (bitcast->expr) {
      auto exprIds = digIdentifiers(bitcast->expr.get());
      results.insert(results.end(), exprIds.begin(), exprIds.end());
    }
    return results;
  }

  // PrefixExpression (!x, -y)
  if (auto *prefix = dynamic_cast<PrefixExpression *>(node)) {
    return digIdentifiers(prefix->operand.get());
  }

  // PostfixExpression (i++, j--)
  if (auto *postfix = dynamic_cast<PostfixExpression *>(node)) {
    return digIdentifiers(postfix->operand.get());
  }

  // FString literal
  if (auto *fStr = dynamic_cast<FStringLiteral *>(node)) {
    for (const auto &seg : fStr->segments) {
      for (const auto &value : seg.values) {
        auto ids = digIdentifiers(value.get());
        results.insert(results.end(), ids.begin(), ids.end());
      }
    }
    return results;
  }

  // InfixExpression (x + y, a.next, b.prev)
  if (auto *infix = dynamic_cast<InfixExpression *>(node)) {
    auto leftIds = digIdentifiers(infix->left_operand.get());
    auto rightIds = digIdentifiers(infix->right_operand.get());
    results.insert(results.end(), leftIds.begin(), leftIds.end());
    results.insert(results.end(), rightIds.begin(), rightIds.end());
    return results;
  }

  // ArrayLiteral ([1, 2, 3])
  if (auto *arrayLit = dynamic_cast<ArrayLiteral *>(node)) {
    for (auto &item : arrayLit->array) {
      auto itemIds = digIdentifiers(item.get());
      results.insert(results.end(), itemIds.begin(), itemIds.end());
    }
    return results;
  }

  // ArraySubscript (arr[5])
  if (auto *subscript = dynamic_cast<ArraySubscript *>(node)) {
    if (subscript->identifier) {
      auto identIds = digIdentifiers(subscript->identifier.get());
      results.insert(results.end(), identIds.begin(), identIds.end());
    }
    for (auto &index : subscript->index_exprs) {
      auto indexIds = digIdentifiers(index.get());
      results.insert(results.end(), indexIds.begin(), indexIds.end());
    }
    return results;
  }

  // STATEMENTS
  // ExpressionStatement
  if (auto *exprStmt = dynamic_cast<ExpressionStatement *>(node)) {
    return digIdentifiers(exprStmt->expression.get());
  }

  // AssignmentStatement (x = y)
  if (auto *assign = dynamic_cast<AssignmentStatement *>(node)) {
    auto identIds = digIdentifiers(assign->identifier.get());
    auto valueIds = digIdentifiers(assign->value.get());
    results.insert(results.end(), identIds.begin(), identIds.end());
    results.insert(results.end(), valueIds.begin(), valueIds.end());
    return results;
  }

  // FieldAssignment (a.next = addr b)
  if (auto *fieldAssign = dynamic_cast<FieldAssignment *>(node)) {
    auto lhsIds = digIdentifiers(fieldAssign->lhs_chain.get());
    auto valueIds = digIdentifiers(fieldAssign->value.get());
    results.insert(results.end(), lhsIds.begin(), lhsIds.end());
    results.insert(results.end(), valueIds.begin(), valueIds.end());
    return results;
  }

  // ReturnStatement
  if (auto *ret = dynamic_cast<ReturnStatement *>(node)) {
    if (ret->return_value) {
      return digIdentifiers(ret->return_value.get());
    }
    return results;
  }

  // TraceStatement
  if (auto *trace = dynamic_cast<TraceStatement *>(node)) {
    for (const auto &expr : trace->arguments) {
      auto idents = digIdentifiers(expr.get());
      results.insert(results.end(), idents.begin(), idents.end());
    }
    return results;
  }
  return results;
}

void Semantics::executeFieldsCapture(
    const std::string &type_name, const std::unique_ptr<LifeTime> &type_baton) {
  if (!type_baton) {
    logInternal("[FIELD CAPTURE] Cannot carryout capture as there is no "
                "liftetime to perform it");
    return;
  }

  auto it = payload.customTypesTable.find(type_name);
  if (it == payload.customTypesTable.end()) {
    logSemanticErrors(ErrorCode::UndefinedVariable, nullptr);
    return;
  }

  const std::shared_ptr<CustomTypeInfo> typeInfo = it->second;

  for (const auto &captureID : typeInfo->captureCandidates) {
    Node *capture_holder = queryForLifeTimeBaton(captureID);
    if (!capture_holder)
      reportDevBug("Could not find baton holder for family id: " + captureID,
                   nullptr);

    auto it = responsibilityTable.find(capture_holder);
    if (it == responsibilityTable.end())
      reportDevBug("Cannot find the baton for family id: " + captureID,
                   capture_holder);

    CapturedField field;
    field.node = capture_holder;
    field.capturedID = captureID;
    field.symbol = getSymbolFromMeta(capture_holder);

    type_baton->captured_fields.push_back(std::move(field));
    logInternal("[FIELD CAPTURE]: Baton : " + type_baton->ID +
                " has capture ID: " + captureID);
  }
}
