#include "semantics.hpp"

void Semantics::walkAllocatorInterface(Node *node) {
  auto allocStmt = dynamic_cast<AllocatorStatement *>(node);
  if (!allocStmt)
    return;

  const std::string allocName =
      allocStmt->allocator_name->expression.TokenLiteral;

  bool hasError = false;

  // allocator name must be unique
  auto existing = resolveSymbolInfo(allocName);
  if (existing) {
    logSemanticErrors("Allocator name '" + allocName + "' is already defined",
                      allocStmt->allocator_name.get());
    return;
  }

  // allocator must have a block
  auto block = dynamic_cast<BlockStatement *>(allocStmt->block.get());
  if (!block) {
    logSemanticErrors("Allocator interface '" + allocName +
                          "' must have a block body",
                      allocStmt->allocator_name.get());
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
                      block);
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
          stmt.get());
      continue;
    }

    std::string funcName = "Empty";

    AllocatorRole role = AllocatorRole::NONE;

    if (auto fnExpr =
            dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get())) {
      funcName = fnExpr->func_key.TokenLiteral;

      if (isExportable) {
        fnExpr->isExportable = true;
      }

      if (fnExpr->call.size() != 1) {
        logSemanticErrors("Allocator function '" + funcName +
                              "' must take exactly one parameter",
                          fnExpr);
        return;
      }

      role = getFunctionRole(fnExpr->call, fnExpr->return_type.get(), funcName);

    } else if (auto fnExpr = dynamic_cast<FunctionDeclarationExpression *>(
                   fnStmt->funcExpr.get())) {
      auto fnDecl =
          dynamic_cast<FunctionDeclaration *>(fnExpr->funcDeclrStmt.get());
      funcName = fnDecl->function_name->expression.TokenLiteral;

      if (isExportable) {
        fnDecl->isExportable = true;
      }

      if (fnDecl->parameters.size() != 1) {
        logSemanticErrors("Allocator function declaration'" + funcName +
                              "' must take exactly one parameter",
                          fnDecl->function_name.get());
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
                        fnStmt);
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
                        fnStmt);
      hasError = true;
    }
  }

  // final contract validation
  if (allocateCount != 1 || freeCount != 1) {
    logSemanticErrors("Allocator interface '" + allocName +
                          "' must define exactly one allocation function and "
                          "one free function",
                      nullptr);
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

  auto &paramStmt = params[0];

  ResolvedType paramType = inferNodeDataType(paramStmt.get());
  ResolvedType retType = inferNodeDataType(returnType);

  // ALLOCATE function
  if (retType.isPointer()) {
    if (paramType.kind != DataType::USIZE || paramType.isPointer()) {
      logSemanticErrors(
          "Allocation function '" + funcName +
              "' must take a size parameter of type 'usize' but got '" +
              paramType.resolvedName + "'",
          paramStmt.get());
      return AllocatorRole::NONE;
    }

    if (!(retType.kind == DataType::OPAQUE)) {
      logSemanticErrors("Allocation function '" + funcName +
                            "' must return 'opaque_ptr' but got '" +
                            retType.resolvedName + "'",
                        returnType);
      return AllocatorRole::NONE;
    }

    if (retType.isArray() || retType.isRef()) {
      logSemanticErrors("Allocation function '" + funcName +
                            "' cannot return arrays or references",
                        returnType);
      return AllocatorRole::NONE;
    }

    return AllocatorRole::ALLOCATE;
  }

  // FREE function
  if (retType.kind == DataType::VOID) {
    if (!paramType.isPointer() || !(paramType.kind == DataType::OPAQUE)) {
      logSemanticErrors(
          "Free function '" + funcName +
              "' must take a parameter of 'ptr opaque' but got '" +
              paramType.resolvedName + "'",
          returnType);
      return AllocatorRole::NONE;
    }

    return AllocatorRole::FREE;
  }

  // invalid allocator signature
  logSemanticErrors("Allocator function '" + funcName +
                        "' must either return an opaque pointer (for "
                        "allocation) or void (for freeing)",
                    returnType);

  return AllocatorRole::NONE;
}

void Semantics::registerInbuiltAllocatorTypes() {
  AllocatorHandle stdHandle;
  stdHandle.allocateName = "unn_alloc";
  stdHandle.freeName = "unn_dealloc";

  // Base types for reuse
  auto opaqueBase = ResolvedType::makeBase(DataType::OPAQUE, "opaque");
  auto usizeBase = ResolvedType::makeBase(DataType::USIZE, "usize");
  auto voidBase = ResolvedType::makeBase(DataType::VOID, "void");

  // alloc: (usize) -> ptr<opaque>
  auto allocSym = std::make_shared<SymbolInfo>();
  allocSym->isFunction = true;
  allocSym->func().isDeclaration = true;
  allocSym->func().isDefined = false;
  allocSym->func().returnType =
      makePointerType(opaqueBase, false); // ptr<opaque>
  allocSym->func().paramTypes.emplace_back(usizeBase, "size");
  stdHandle.allocatorSymbol = allocSym;

  // free: (ptr<opaque>) -> void
  auto freeSym = std::make_shared<SymbolInfo>();
  freeSym->isFunction = true;
  freeSym->func().isDeclaration = true;
  freeSym->func().isDefined = false;
  freeSym->func().returnType = voidBase;
  freeSym->func().paramTypes.emplace_back(makePointerType(opaqueBase, false),
                                          "p");
  stdHandle.freeSymbol = freeSym;

  allocatorMap["GPA"] = stdHandle;
}
