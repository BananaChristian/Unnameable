#include "semantics.hpp"
#include <string>

void Semantics::walkAllocatorInterface(Node *node) {
  auto allocStmt = dynamic_cast<AllocatorStatement *>(node);
  if (!allocStmt)
    return;

  const std::string allocName =
      allocStmt->allocator_name->expression.TokenLiteral;

  // allocator name must be unique
  auto existing = resolveSymbolInfo(allocName);
  if (existing) {
    logSemanticErrors(ErrorCode::UndefinedVariable,
                      allocStmt->allocator_name.get(), {allocName});
    return;
  }

  // allocator must have a block
  auto block = dynamic_cast<BlockStatement *>(allocStmt->block.get());
  if (!block) {
    logSemanticErrors(ErrorCode::MissingOrInvalidBody,
                      allocStmt->allocator_name.get());
    return;
  }

  bool isExportable = allocStmt->isExportable;

  // allocator scope
  symbolTable.push_back({});
  insideAllocator = true;

  // allocator contract: exactly two functions
  if (block->statements.size() != 2) {
    logSemanticErrors(ErrorCode::InvalidFuncsInAllocator, block);
  }

  int allocateCount = 0;
  int freeCount = 0;

  AllocatorHandle handle;

  // validate each statement
  for (const auto &stmt : block->statements) {
    auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
    if (!fnStmt) {
      logSemanticErrors(ErrorCode::IllegalStmtInAllocator, stmt.get());
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
        logSemanticErrors(
            ErrorCode::ArgumentSizeMismatch, fnExpr,
            {funcName, std::to_string(1), std::to_string(fnExpr->call.size())});
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
        logSemanticErrors(ErrorCode::ArgumentSizeMismatch, fnExpr,
                          {funcName, std::to_string(1),
                           std::to_string(fnDecl->parameters.size())});
        return;
      }

      role = getFunctionRole(fnDecl->parameters, fnDecl->return_type.get(),
                             funcName);
    }

    // walk whatever is there
    walkFunctionStatement(fnStmt);

    auto funcSym = lookUpInCurrentScope(funcName);
    if (!funcSym) {
      logSemanticErrors(ErrorCode::UndefinedVariable, fnStmt, {allocName});
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
      logSemanticErrors(ErrorCode::InvalidAllocatorContract, fnStmt,
                        {funcName});
    }
  }

  // final contract validation
  if (allocateCount != 1 || freeCount != 1) {
    logSemanticErrors(ErrorCode::InvalidCountInAllocator, nullptr);
  }

  auto allocSym = std::make_shared<SymbolInfo>();
  allocSym->isExportable = isExportable;
  allocSym->hasError = hasError;

  insertMetaData(allocStmt, allocSym);
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
      logSemanticErrors(ErrorCode::InvalidAllocationFuncParam, paramStmt.get(),
                        {funcName, paramType.resolvedName});
      return AllocatorRole::NONE;
    }

    logInternal("Return type :" + retType.resolvedName);
    if (!(retType.kind == DataType::OPAQUE)) {
      logSemanticErrors(ErrorCode::TypeMismatch, returnType,
                        {"ptr <opaque>", retType.resolvedName});
      return AllocatorRole::NONE;
    }

    if (retType.isArray() || retType.isRef()) {
      logSemanticErrors(ErrorCode::InvalidReturnType, returnType);
      return AllocatorRole::NONE;
    }

    return AllocatorRole::ALLOCATE;
  }

  // FREE function
  if (retType.kind == DataType::VOID) {
    if (!paramType.isPointer() || !(paramType.kind == DataType::OPAQUE)) {
      logSemanticErrors(ErrorCode::InvalidDeallocatorParam, returnType,
                        {funcName, paramType.resolvedName});
      return AllocatorRole::NONE;
    }

    return AllocatorRole::FREE;
  }

  // invalid allocator signature
  logSemanticErrors(ErrorCode::InvalidAllocationReturn, returnType, {funcName});

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

void Semantics::walkGlobalAllocator(Node *node) {
  auto globalAlloc = dynamic_cast<GlobalAllocatorStatement *>(node);
  if (!globalAlloc)
    return;

  auto allocator_name =
      extractIdentifierName(globalAlloc->allocator_name.get());

  // Check if the allocator exists in the allocator map
  auto it = allocatorMap.find(allocator_name);
  if (it == allocatorMap.end()) {
    logSemanticErrors(ErrorCode::UnknownAllocator,
                      globalAlloc->allocator_name.get(),{allocator_name});
    return;
  }

  if (globalAllocatorSet) {
    logSemanticErrors(ErrorCode::AlreadySetGlobalAllocator,
                      globalAlloc->allocator_name.get());
    return;
  }

  if (!isGlobalScope()) {
    logSemanticErrors(ErrorCode::globalAllocatorMustBeGlobal,
                      globalAlloc->allocator_name.get());
    return;
  }

  globalAllocatorSet = true;
  globalAllocatorName =
      allocator_name; // Override GPA with whatever the user has provided
}
