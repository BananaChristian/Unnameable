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
    logSemanticErrors("Allocator interface '" + allocName +
                          "' must define exactly two functions (allocation "
                          "function and freeing function)",
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

    std::string funcName = "Empty";

    int funcLine;
    int funcCol;

    AllocatorRole role = AllocatorRole::NONE;

    if (auto fnExpr =
            dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get())) {
      funcName = fnExpr->func_key.TokenLiteral;
      funcLine = fnExpr->func_key.line;
      funcCol = fnExpr->func_key.column;

      if (isExportable) {
        fnExpr->isExportable = true;
      }

      if (fnExpr->call.size() != 1) {
        logSemanticErrors("Allocator function '" + funcName +
                              "' must take exactly one parameter",
                          funcLine, funcCol);
        return;
      }

      role = getFunctionRole(fnExpr->call, fnExpr->return_type.get(), funcName);

    } else if (auto fnExpr = dynamic_cast<FunctionDeclarationExpression *>(
                   fnStmt->funcExpr.get())) {
      auto fnDecl =
          dynamic_cast<FunctionDeclaration *>(fnExpr->funcDeclrStmt.get());
      funcName = fnDecl->function_name->expression.TokenLiteral;
      funcLine = fnDecl->function_name->expression.line;
      funcCol = fnDecl->function_name->expression.column;

      if (isExportable) {
        fnDecl->isExportable = true;
      }

      if (fnDecl->parameters.size() != 1) {
        logSemanticErrors("Allocator function declaration'" + funcName +
                              "' must take exactly one parameter",
                          funcLine, funcCol);
        return;
      }

      role = getFunctionRole(fnDecl->parameters, fnDecl->return_type.get(),
                             funcName);
    }

    // walk whatever is there
    walkFunctionStatement(fnStmt);

    auto funcSym = lookUpInCurrentScope(funcName);
    if (!funcSym) {
      logSemanticErrors("Allocator function '" + funcName +
                            "' does not exist in allocator '" + allocName + "'",
                        funcLine, funcCol);
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
      logSemanticErrors("Function '" + funcName +
                            "' does not satisfy allocator contract",
                        funcLine, funcCol);
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
      logSemanticErrors(
          "Allocation function '" + funcName +
              "' must take a size parameter of type 'usize' but got '" +
              paramType.resolvedName + "'",
          line, col);
      return AllocatorRole::NONE;
    }

    if (!(retType.kind == DataType::OPAQUE)) {
      logSemanticErrors("Allocation function '" + funcName +
                            "' must return 'opaque_ptr' but got '" +
                            retType.resolvedName + "'",
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
    if (!paramType.isPointer || !(paramType.kind == DataType::OPAQUE)) {
      logSemanticErrors(
          "Free function '" + funcName +
              "' must take a parameter of 'ptr opaque' but got '" +
              paramType.resolvedName + "'",
          line, col);
      return AllocatorRole::NONE;
    }

    return AllocatorRole::FREE;
  }

  // invalid allocator signature
  logSemanticErrors("Allocator function '" + funcName +
                        "' must either return an opaque pointer (for "
                        "allocation) or void (for freeing)",
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
    reportDevBug("Could not find statement metaData for declaration in "
                 "the heap statement ");
    return;
  }

  auto stmtSym = it->second;
  // Toggle the heap flag, and other flags
  stmtSym->isHeap = true;
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
  allocSym->returnType = ResolvedType{DataType::OPAQUE, "opaque_ptr",
                                      true}; // The ptr opaque return type
  allocSym->paramTypes = allocParams;
  allocSym->isDefined = false;

  stdHandle.allocatorSymbol = allocSym;

  // Create handle symbol for free
  std::vector<std::pair<ResolvedType, std::string>> freeParams;
  freeParams.emplace_back(ResolvedType{DataType::OPAQUE, "opaque_ptr", true},
                          "p");
  auto freeSym = std::make_shared<SymbolInfo>();
  freeSym->isFunction = true;
  freeSym->isDeclaration = true;
  freeSym->returnType =
      ResolvedType{DataType::VOID, "void"}; // The void return type
  freeSym->paramTypes = freeParams;
  freeSym->isDefined = false;

  stdHandle.freeSymbol = freeSym;

  allocatorMap["GPA"] = stdHandle; // Register GPA allocator type
}
