#include "semantics.hpp"

void Semantics::walkAllocatorInterface(Node *node) {
  auto allocStmt = dynamic_cast<AllocatorStatement *>(node);
  if (!allocStmt)
    return;

  const std::string allocName =
      allocStmt->allocator_name->expression.TokenLiteral;
  const int nameLine = allocStmt->allocator_name->expression.line;
  const int nameCol = allocStmt->allocator_name->expression.column;

  bool hasError = false;

  // allocator name must be unique
  auto existing = resolveSymbolInfo(allocName);
  if (existing) {
    logSemanticErrors("Allocator name '" + allocName + "' is already defined",
                      nameLine, nameCol);
    return;
  }

  // allocator must have a block
  auto block = dynamic_cast<BlockStatement *>(allocStmt->block.get());
  if (!block) {
    logSemanticErrors("Allocator interface '" + allocName +
                          "' must have a block body",
                      nameLine, nameCol);
    return;
  }

  bool isExportable = allocStmt->isExportable;

  // allocator scope
  symbolTable.push_back({});
  insideAllocator = true;

  // allocator contract: exactly two functions
  if (block->statements.size() != 2) {
    logSemanticErrors(
        "Allocator interface '" + allocName +
            "' must define exactly two functions (allocate and free)",
        block->statement.line, block->statement.column);
    hasError = true;
  }

  int allocateCount = 0;
  int freeCount = 0;

  AllocatorHandle handle;

  // validate each statement
  for (const auto &stmt : block->statements) {
    auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
    if (!fnStmt) {
      logSemanticErrors(
          "Only functions are allowed inside an allocator interface",
          stmt->statement.line, stmt->statement.column);
      continue;
    }

    auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
    if (!fnExpr) {
      // declarations are allowed syntactically but meaningless here
      logSemanticErrors("Allocator functions must have a body",
                        stmt->statement.line, stmt->statement.column);
      continue;
    }

    std::string &funcName = fnExpr->func_key.TokenLiteral;

    auto funcLine = fnExpr->func_key.line;
    auto funCol = fnExpr->func_key.column;

    if (isExportable) {
      fnExpr->isExportable = true;
    }

    if (fnExpr->call.size() != 1) {
      logSemanticErrors("Allocator function '" + funcName +
                            "' must take exactly one parameter",
                        funcLine, funCol);
      return;
    }

    AllocatorRole role =
        getFunctionRole(fnExpr->call, fnExpr->return_type.get(), funcName);
    // walk as a normal function
    walkFunctionExpression(fnStmt->funcExpr.get());

    auto funcSym = lookUpInCurrentScope(funcName);
    if (!funcSym) {
      logSemanticErrors("Allocator function '" + funcName +
                            "' does not exist in allocator '" + allocName + "'",
                        funcLine, funCol);
      continue;
    }

    if (role == AllocatorRole::ALLOCATE) {
      allocateCount++;
      handle.allocateName = funcName;
      handle.allocatorSymbol = funcSym;
    }

    else if (role == AllocatorRole::FREE) {
      freeCount++;
      handle.freeName = funcName;
      handle.freeSymbol = funcSym;
    } else {
      logSemanticErrors("Function '" + fnExpr->func_key.TokenLiteral +
                            "' does not satisfy allocator contract",
                        fnExpr->func_key.line, fnExpr->func_key.column);
      hasError = true;
    }
  }

  // final contract validation
  if (allocateCount != 1 || freeCount != 1) {
    logSemanticErrors("Allocator interface '" + allocName +
                          "' must define exactly one allocation function and "
                          "one free function",
                      nameLine, nameCol);
    hasError = true;
  }

  auto allocSym = std::make_shared<SymbolInfo>();
  allocSym->isExportable = isExportable;
  allocSym->hasError = hasError;

  metaData[allocStmt] = allocSym;
  symbolTable[0][allocName] = allocSym;
  allocatorMap[allocName] = handle;

  insideAllocator = false;
  popScope();
}

AllocatorRole Semantics::getFunctionRole(
    const std::vector<std::unique_ptr<Statement>> &params,
    Expression *returnType, const std::string &funcName) {
  const int retLine = returnType->expression.line;
  const int retCol = returnType->expression.column;

  auto &paramStmt = params[0];
  const int line = paramStmt->statement.line;
  const int col = paramStmt->statement.column;

  ResolvedType paramType = inferNodeDataType(paramStmt.get());
  ResolvedType retType = inferNodeDataType(returnType);

  // ALLOCATE function
  if (retType.isPointer) {
    if (paramType.kind != DataType::USIZE || paramType.isPointer) {
      logSemanticErrors("Allocation function '" + funcName +
                            "' must take 'usize' as its size parameter",
                        line, col);
      return AllocatorRole::NONE;
    }

    if (!(retType.kind == DataType::USIZE || retType.kind == DataType::U8)) {
      logSemanticErrors("Allocation function '" + funcName +
                            "' must return 'ptr usize' or 'ptr u8'",
                        retLine, retCol);
      return AllocatorRole::NONE;
    }

    if (retType.isArray || retType.isRef) {
      logSemanticErrors("Allocation function '" + funcName +
                            "' cannot return arrays or references",
                        retLine, retCol);
      return AllocatorRole::NONE;
    }

    return AllocatorRole::ALLOCATE;
  }

  // FREE function
  if (retType.kind == DataType::VOID) {
    if (!paramType.isPointer || !(paramType.kind == DataType::USIZE ||
                                  paramType.kind == DataType::U8)) {
      logSemanticErrors("Free function '" + funcName +
                            "' must take 'ptr usize' or 'ptr u8'",
                        line, col);
      return AllocatorRole::NONE;
    }

    return AllocatorRole::FREE;
  }

  // invalid allocator signature
  logSemanticErrors(
      "Allocator function '" + funcName +
          "' must either return a pointer (allocate) or void (free)",
      retLine, retCol);

  return AllocatorRole::NONE;
}

void Semantics::walkHeapStatement(Node *node) {
  auto heapStmt = dynamic_cast<HeapStatement *>(node);
  if (!heapStmt)
    return;

  bool hasError = false;

  std::string allocType;
  // Check if it has the allocator type
  if (heapStmt->allocType) {
    auto allocIdent = dynamic_cast<Identifier *>(heapStmt->allocType.get());
    const std::string &allocName = allocIdent->identifier.TokenLiteral;
    // Check if the allocator exists in the allocatorMap
    auto allocIt = allocatorMap.find(allocName);
    if (allocIt == allocatorMap.end()) {
      logSemanticErrors("Unknown allocator type '" + allocName + "'",
                        allocIdent->identifier.line,
                        allocIdent->identifier.column);
      return;
    }

    allocType = allocIt->first;
  } else {
    allocType = "GPA"; // The default GPA type
  }

  // If it has the stmt
  auto stmt = heapStmt->stmt.get();
  walker(stmt);

  // Get the symbol info of the stmt using metaData search
  auto it = metaData.find(heapStmt->stmt.get());
  if (it == metaData.end()) {
    logSemanticErrors("Could not find statement metaData for declaration in "
                      "the dheap statement ",
                      stmt->statement.line, stmt->statement.column);
    return;
  }

  auto stmtSym = it->second;
  // Toggle the dheap flag, and other flags
  stmtSym->isHeap = true;
  stmtSym->lastUseNode = stmt;
  stmtSym->allocType = allocType;

  // If the walked stmt has an error then so does the overall dheap statement
  hasError = stmtSym->hasError;

  auto heapSym = std::make_shared<SymbolInfo>();
  heapSym->hasError = hasError;

  metaData[heapStmt] =
      heapSym; // Only store in the metaData table since the initial stmt walk
               // already registered in the semantic symbol table
}

void Semantics::registerInbuiltAllocatorTypes() {
  // alloc for the GPA
  AllocatorHandle stdHandle;
  stdHandle.allocateName = "alloc";
  stdHandle.freeName = "free";

  // Create handle symbol for alloc
  std::vector<std::pair<ResolvedType, std::string>> allocParams;
  allocParams.emplace_back(ResolvedType{DataType::USIZE, "usize", false},
                           "size");
  auto allocSym = std::make_shared<SymbolInfo>();
  allocSym->isFunction = true;
  allocSym->isDeclaration = true;
  allocSym->returnType = ResolvedType{DataType::USIZE, "usize_ptr",
                                      true}; // The ptr usize return type
  allocSym->paramTypes = allocParams;
  allocSym->isDefined = false;

  stdHandle.allocatorSymbol = allocSym;

  // Create handle symbol for free
  std::vector<std::pair<ResolvedType, std::string>> freeParams;
  freeParams.emplace_back(ResolvedType{DataType::USIZE, "usize_ptr", true},
                          "p");
  auto freeSym = std::make_shared<SymbolInfo>();
  freeSym->isFunction = true;
  freeSym->isDeclaration = true;
  freeSym->returnType =
      ResolvedType{DataType::VOID, "void"}; // The void return type
  freeSym->paramTypes = freeParams;
  freeSym->isDefined = false;

  stdHandle.freeSymbol = freeSym;

  allocatorMap["GPA"] = stdHandle; // Register malloc allocator type
}
