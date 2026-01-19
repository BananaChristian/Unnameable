#include "sentinel.hpp"
#include "ast.hpp"
#include <iomanip>

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_GREEN "\033[32m"

Sentinel::Sentinel(Semantics &semantics) : semantics(semantics) {
  registerSentinelFns();
}

void Sentinel::sentinelDriver(Node *node) {
  if (!node) {
    return;
  }

  auto sentinelIt = sentinelFnsMap.find(typeid(*node));
  if (sentinelIt == sentinelFnsMap.end()) {
    std::cout << "Sentinel skipping node: " << node->toString() << "\n";
    return;
  }

  (this->*sentinelIt->second)(node);
}

void Sentinel::registerSentinelFns() {
  sentinelFnsMap[typeid(LetStatement)] = &Sentinel::checkLetStatement;
  sentinelFnsMap[typeid(PointerStatement)] = &Sentinel::checkPointerStatement;
  sentinelFnsMap[typeid(AssignmentStatement)] =
      &Sentinel::checkAssignmentStatement;
  sentinelFnsMap[typeid(Identifier)] = &Sentinel::checkIdentifier;
  sentinelFnsMap[typeid(DereferenceExpression)] =
      &Sentinel::checkDereferenceExpression;
  sentinelFnsMap[typeid(BlockStatement)] = &Sentinel::checkBlockStatement;
  sentinelFnsMap[typeid(ifStatement)] = &Sentinel::checkIfStatement;
  sentinelFnsMap[typeid(elifStatement)] = &Sentinel::checkElifStatement;
  sentinelFnsMap[typeid(WhileStatement)] = &Sentinel::checkWhileStatement;
  sentinelFnsMap[typeid(ForStatement)] = &Sentinel::checkForStatement;
  sentinelFnsMap[typeid(FunctionStatement)] = &Sentinel::checkFunctionStatement;
  sentinelFnsMap[typeid(FunctionExpression)] =
      &Sentinel::checkFunctionExpression;
  sentinelFnsMap[typeid(BlockExpression)] = &Sentinel::checkBlockExpression;
  sentinelFnsMap[typeid(CallExpression)] = &Sentinel::checkCallExpression;
  sentinelFnsMap[typeid(ExpressionStatement)] =
      &Sentinel::checkExpressionStatement;
  sentinelFnsMap[typeid(InfixExpression)] = &Sentinel::checkInfixExpression;
  sentinelFnsMap[typeid(PrefixExpression)] = &Sentinel::checkPrefixExpression;
  sentinelFnsMap[typeid(PostfixExpression)] = &Sentinel::checkPostfixExpression;
  sentinelFnsMap[typeid(CastExpression)] = &Sentinel::checkCastExpression;
  sentinelFnsMap[typeid(BitcastExpression)] = &Sentinel::checkBitcastExpression;
  sentinelFnsMap[typeid(RecordStatement)] = &Sentinel::checkRecordStatement;
  sentinelFnsMap[typeid(FieldAssignment)] = &Sentinel::checkFieldAssignment;
  sentinelFnsMap[typeid(ComponentStatement)] =
      &Sentinel::checkComponentStatement;
  sentinelFnsMap[typeid(InstantiateStatement)] =
      &Sentinel::checkInstantiateStatement;
  sentinelFnsMap[typeid(SealStatement)] = &Sentinel::checkSealStatement;
  sentinelFnsMap[typeid(AllocatorStatement)] =
      &Sentinel::checkAllocatorInterface;
}

void Sentinel::checkExpressionStatement(Node *node) {
  auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
  if (!exprStmt)
    return;

  sentinelDriver(exprStmt->expression.get());
}

void Sentinel::checkInfixExpression(Node *node) {
  auto infixExpr = dynamic_cast<InfixExpression *>(node);
  if (!infixExpr)
    return;

  auto left = infixExpr->left_operand.get();
  auto right = infixExpr->right_operand.get();

  sentinelDriver(left);
  sentinelDriver(right);
}

void Sentinel::checkPrefixExpression(Node *node) {
  auto prefixExpr = dynamic_cast<PrefixExpression *>(node);
  if (!prefixExpr)
    return;

  sentinelDriver(prefixExpr->operand.get());
}

void Sentinel::checkPostfixExpression(Node *node) {
  auto postfixExpr = dynamic_cast<PostfixExpression *>(node);
  if (!postfixExpr)
    return;

  sentinelDriver(postfixExpr->operand.get());
}

void Sentinel::checkIdentifier(Node *node) {
  auto ident = dynamic_cast<Identifier *>(node);
  if (!ident)
    return;

  auto line = ident->expression.line;
  auto col = ident->identifier.column;
  auto name = ident->identifier.TokenLiteral;

  auto metaIt = semantics.metaData.find(ident);
  if (metaIt == semantics.metaData.end()) {
    logError("Could not find identifier '" + name + "' metaData", line, col);
    return;
  }

  auto identSym = metaIt->second;
  if (!identSym) {
    logError("Unidentified identitfier '" + name + "' ", line, col);
    return;
  }

  if (!identSym->isHeap)
    return;

  identSym->genericName = name;

  if ((identSym->lastUseNode == ident) && (identSym->refCount == 0)) {
    if (sentinelStack.back()->alloc_id != identSym->alloc_id) {
      logError("Non LIFO free detected, Tried to free '" + name +
                   "' which is not on top of the SAGE stack",
               line, col);
      printStackSnapshot();
      identSym->hasError = true;
      return;
    }
    sentinelStack.pop_back();
  }
}

void Sentinel::checkCastExpression(Node *node) {
  auto castExpr = dynamic_cast<CastExpression *>(node);
  if (!castExpr)
    return;

  auto src = castExpr->expr.get();
  if (!src)
    return;
  
  sentinelDriver(src);
}

void Sentinel::checkBitcastExpression(Node *node) {
  auto bitcastExpr = dynamic_cast<BitcastExpression *>(node);
  if (!bitcastExpr)
    return;

  auto src = bitcastExpr->expr.get();
  if (!src)
    return;
  sentinelDriver(src);
}

void Sentinel::checkDereferenceExpression(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
  if (!derefExpr)
    return;

  auto line = derefExpr->deref_token.line;
  auto col = derefExpr->deref_token.column;

  auto name = semantics.extractIdentifierName(derefExpr->identifier.get());

  auto metaIt = semantics.metaData.find(derefExpr);
  if (metaIt == semantics.metaData.end()) {
    logError("Could not find dereference '" + name + "' metaData", line, col);
    return;
  }

  auto derefSym = metaIt->second;
  if (!derefSym) {
    logError("Unidentified dereference '" + name + "' ", line, col);
    return;
  }

  if (!derefSym->isHeap)
    return;

  derefSym->genericName = name;

  bool found = false;
  for (auto const &liveSym : sentinelStack) {
    if (liveSym->alloc_id == derefSym->alloc_id) {
      found = true;
      break;
    }
  }

  if (!found) {
    logError("Use-after-free: Pointer '" + name + "' is already dead.", line,
             col);
    printStackSnapshot();
    return;
  }

  if (derefSym->lastUseNode == derefExpr) {
    // If 'p' dies here, we can pop it.
    if (sentinelStack.back()->alloc_id == derefSym->alloc_id) {
      sentinelStack.pop_back();
    } else {
      logError("Non LIFO free detected, Tried to free '" + name +
                   "' which is not on top of the SAGE stack",
               line, col);
      printStackSnapshot();
    }
  }
}

void Sentinel::checkLetStatement(Node *node) {
  auto letStmt = dynamic_cast<LetStatement *>(node);
  if (!letStmt)
    return;

  auto line = letStmt->ident_token.line;
  auto col = letStmt->ident_token.column;
  const std::string &name = letStmt->ident_token.TokenLiteral;

  // Getting the let statement symbol
  auto metaIt = semantics.metaData.find(letStmt);
  if (metaIt == semantics.metaData.end()) {
    logError("Could not find let statement metaData", line, col);
    return;
  }
  auto letSym = metaIt->second;
  if (!letSym) {
    logError("Invalid let statement '" + name + "'", line, col);
    return;
  }
  // Getting if the let statement is not heap raised and stopping there
  if (!letSym->isHeap)
    return;

  letSym->genericName = name;

  // Assigning the alloc_id
  letSym->alloc_id = nextAllocId++;

  // Pushing the symbol onto the sentinel stack
  sentinelStack.push_back(letSym);

  if (letSym->lastUseNode == letStmt) {
    if (sentinelStack.back()->alloc_id != letSym->alloc_id) {
      logError("Non LIFO free detected, Tried to free '" + name +
                   "' which is not on top of the SAGE stack",
               line, col);
      printStackSnapshot();
      letSym->hasError = true;
      return;
    }
    sentinelStack.pop_back();
  }
}

void Sentinel::checkPointerStatement(Node *node) {
  auto ptrStmt = dynamic_cast<PointerStatement *>(node);

  if (!ptrStmt)
    return;

  auto line = ptrStmt->name->expression.line;
  auto col = ptrStmt->name->expression.column;
  const std::string &name = ptrStmt->name->expression.TokenLiteral;

  auto metaIt = semantics.metaData.find(ptrStmt);
  if (metaIt == semantics.metaData.end()) {
    logError("Could not find pointer statement metaData", line, col);
    return;
  }

  auto ptrSym = metaIt->second;
  if (!ptrSym) {
    logError("Invalid pointer statement '" + name + "'", line, col);
    return;
  }

  if (!ptrSym->isHeap)
    return;

  ptrSym->genericName = name;

  // Check the target
  auto addrExpr = dynamic_cast<AddressExpression *>(ptrStmt->value.get());
  if (addrExpr) {
    std::string targetName = addrExpr->identifier->expression.TokenLiteral;
    auto targetSym = ptrSym->targetSymbol;

    if (targetSym && targetSym->isHeap) {
      if (targetSym->lastUseNode == addrExpr) {
        if (sentinelStack.back()->alloc_id != targetSym->alloc_id) {
          logError("Non LIFO free detected, Tried to free '" + targetName +
                       "' which is not on top of the SAGE stack",
                   line, col);
          printStackSnapshot();
          ptrSym->hasError = true;
        } else {
          sentinelStack.pop_back();
        }
      }
    }
  }

  ptrSym->alloc_id = nextAllocId++;
  sentinelStack.push_back(ptrSym);

  // Check the pointer itself
  if (ptrSym->lastUseNode == ptrStmt) {
    if (sentinelStack.back()->alloc_id != ptrSym->alloc_id) {
      logError("Non LIFO free detected, Tried to free '" + name +
                   "' which is not on top of the SAGE stack",
               line, col);
      ptrSym->hasError = true;
      return;
    }
    sentinelStack.pop_back();
  }
}

void Sentinel::checkAssignmentStatement(Node *node) {
  auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
  if (!assignStmt)
    return;

  auto ident = assignStmt->identifier.get();
  auto value = assignStmt->value.get();

  sentinelDriver(ident);
  sentinelDriver(value);
}

void Sentinel::checkBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt)
    return;

  for (const auto &stmt : blockStmt->statements) {
    sentinelDriver(stmt.get());
  }
}

void Sentinel::checkIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  sentinelDriver(ifStmt->if_result.get());

  if (!ifStmt->elifClauses.empty()) {
    for (const auto &elif : ifStmt->elifClauses) {
      sentinelDriver(elif.get());
    }
  }

  if (ifStmt->else_result.has_value()) {
    sentinelDriver(ifStmt->else_result.value().get());
  }
}

void Sentinel::checkElifStatement(Node *node) {
  auto elifStmt = dynamic_cast<elifStatement *>(node);
  if (!elifStmt)
    return;

  sentinelDriver(elifStmt->elif_result.get());
}

void Sentinel::checkWhileStatement(Node *node) {
  auto whileStmt = dynamic_cast<WhileStatement *>(node);
  if (!whileStmt)
    return;

  sentinelDriver(whileStmt->loop.get());
}

void Sentinel::checkForStatement(Node *node) {
  auto forStmt = dynamic_cast<ForStatement *>(node);
  if (!forStmt)
    return;

  sentinelDriver(forStmt->body.get());
}

void Sentinel::checkFunctionStatement(Node *node) {
  auto funcStmt = dynamic_cast<FunctionStatement *>(node);
  if (!funcStmt)
    return;

  sentinelDriver(funcStmt->funcExpr.get());
}

void Sentinel::checkFunctionExpression(Node *node) {
  auto funcExpr = dynamic_cast<FunctionExpression *>(node);
  if (!funcExpr)
    return;

  sentinelDriver(funcExpr->block.get());
}

void Sentinel::checkBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr)
    return;

  if (!blockExpr->statements.empty()) {
    for (const auto &stmt : blockExpr->statements) {
      sentinelDriver(stmt.get());
    }
  }

  if (blockExpr->finalexpr.has_value()) {
    sentinelDriver(blockExpr->finalexpr.value().get());
  }
}

void Sentinel::checkCallExpression(Node *node) {
  auto callExpr = dynamic_cast<CallExpression *>(node);
  if (!callExpr)
    return;

  for (const auto &arg : callExpr->parameters) {
    sentinelDriver(arg.get());
  }
}

void Sentinel::checkRecordStatement(Node *node) {
  auto dataStmt = dynamic_cast<RecordStatement *>(node);
  if (!dataStmt)
    return;

  for (const auto &stmt : dataStmt->fields) {
    sentinelDriver(stmt.get());
  }
}

void Sentinel::checkFieldAssignment(Node *node) {
  auto fieldStmt = dynamic_cast<FieldAssignment *>(node);
  if (!fieldStmt)
    return;

  int line = fieldStmt->statement.line;
  int column = fieldStmt->statement.column;

  // Dealing with field assignment name
  auto [parentName, childName] =
      semantics.splitScopedName(fieldStmt->assignment_token.TokenLiteral);

  // Get the symbol
  auto fieldIt = semantics.metaData.find(fieldStmt);
  if (fieldIt == semantics.metaData.end()) {
    logError("Field assignment metaData not found for '" + parentName + "'",
             line, column);
    return;
  }

  auto fieldSym = fieldIt->second;
  if (!fieldSym) {
    logError("Unidentified variable '" + parentName + "'", line, column);
    return;
  }

  // Get the base symbol
  auto baseSym = fieldSym->baseSymbol;
  if (!baseSym) {

    logError("Unidentified variable '" + parentName + "'", line, column);
    return;
  }

  // Get the parent type from the baseSymbol
  auto parentTypeName = baseSym->type.resolvedName;
  std::string lookUpName=parentTypeName;
  if(baseSym->isPointer){
      lookUpName = semantics.stripPtrSuffix(parentTypeName);
  }else if(baseSym->isRef){
      lookUpName=semantics.stripRefSuffix(parentTypeName);
  }
  
  std::cout << "PARENT TYPE NAME: " << lookUpName << "\n";

  // Check inside the parent
  auto parentIt = semantics.customTypesTable.find(lookUpName);
  if (parentIt == semantics.customTypesTable.end()) {
    logError("Unknown type '" + lookUpName + "'", line, column);
    return;
  }

  auto members = parentIt->second->members;
  auto childIt = members.find(childName);
  if (childIt == members.end()) {
    logError("'" + childName + "' does not exist under type '" +
                 parentTypeName + "'",
             line, column);
    return;
  }

  sentinelDriver(childIt->second->node);
  sentinelDriver(fieldStmt->value.get());
}

void Sentinel::checkComponentStatement(Node *node) {
  auto compStmt = dynamic_cast<ComponentStatement *>(node);
  if (!compStmt)
    return;

  std::cout << "RUNNING Sentinel Analysis on component\n";

  auto compName = compStmt->component_name->expression.TokenLiteral;
  auto line = compStmt->component_name->expression.line;
  auto col = compStmt->component_name->expression.column;

  // Extract the members
  auto compMeta = semantics.metaData.find(compStmt);
  if (compMeta == semantics.metaData.end()) {
    logError("Could not find '" + compName + "' metaData", line, col);
    return;
  }

  auto compSym = compMeta->second;
  if (!compSym) {
    logError("Unidentified variable '" + compName + "' ", line, col);
    return;
  }

  // Get the imported data and tell sentinel to analyze it
  for (const auto &[key, value] : compSym->members) {
    // Extract the member's node from the memberInfo and call sentinel driver
    sentinelDriver(value->node);
  }

  // Call the sentinel driver on the private members(Data)
  for (const auto &data : compStmt->privateData) {
    sentinelDriver(data.get());
  }

  // Call the sentinel driver on the private members(methods)
  for (const auto &method : compStmt->privateMethods) {
    sentinelDriver(method.get());
  }
}

void Sentinel::checkInstantiateStatement(Node *node) {
  auto instStmt = dynamic_cast<InstantiateStatement *>(node);
  if (!instStmt)
    return;

  auto line = instStmt->instantiate_token.line;
  auto col = instStmt->instantiate_token.column;

  auto it = semantics.metaData.find(instStmt);
  if (it == semantics.metaData.end()) {
    logError("Failed to find metaData for instantiation statement", line, col);
    return;
  }

  auto sym = it->second;
  const auto &instTable = sym->instTable;

  if (instTable.has_value()) {
    sentinelDriver(instTable->instantiatedAST.get());
  }
}

void Sentinel::checkSealStatement(Node *node) {
  auto sealStmt = dynamic_cast<SealStatement *>(node);
  if (!sealStmt)
    return;

  sentinelDriver(sealStmt->block.get());
}

void Sentinel::checkAllocatorInterface(Node *node) {
  auto allocStmt = dynamic_cast<AllocatorStatement *>(node);
  if (!allocStmt)
    return;

  sentinelDriver(allocStmt->block.get());
}

void Sentinel::logError(const std::string &message, int line, int col) {
  std::cerr << COLOR_RED << "[SENTINEL ERROR] " << COLOR_RESET << message
            << " on line: " << line << ", column: " << col << "\n";
}

void Sentinel::printStackSnapshot() {
  std::cerr << "\n"
            << COLOR_YELLOW
            << "--- SAGE STACK SNAPSHOT (Current Allocation State) ---"
            << COLOR_RESET << "\n";
  std::cerr << std::left << std::setw(10) << "ID" << std::setw(15) << "Variable"
            << std::setw(12) << "Size (B)" << std::setw(12) << "Align"
            << "Status" << "\n";
  std::cerr << std::string(60, '-') << "\n";

  for (size_t i = 0; i < sentinelStack.size(); ++i) {
    auto sym = sentinelStack[i];
    bool isTop = (i == sentinelStack.size() - 1);
    std::string status =
        isTop ? (std::string(COLOR_GREEN) + "[TOP]" + COLOR_RESET) : "Blocked";

    std::cerr << std::left << std::setw(10) << sym->alloc_id << std::setw(15)
              << sym->genericName << std::setw(12) << sym->componentSize
              << std::setw(12)
              << (sym->alignment.value()
                      ? std::to_string(sym->alignment.value())
                      : "N/A")
              << status << "\n";
  }
  std::cerr << std::string(60, '-') << "\n\n";
}
