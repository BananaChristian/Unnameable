#include "ast.hpp"
#include "semantics.hpp"
#include <memory>
#include <string>

static std::shared_ptr<SymbolInfo>
makeFuncSymbol(const std::string &name, bool isExportable, bool isNaked,
               bool isInterrupt, bool hasError) {
  auto sym = std::make_shared<SymbolInfo>();
  sym->isFunction = true;
  sym->isExportable = isExportable;
  sym->hasError = hasError;
  sym->func().funcName = name;
  sym->func().isDeclaration = false;
  sym->func().isDefined = false;
  sym->func().returnType = ResolvedType::unknown();
  sym->func().isInterrupt = isInterrupt;
  sym->func().isNaked = isNaked;
  return sym;
}

void Semantics::walkBlockExpression(Node *node) {
  auto *blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr)
    return;

  activeBlocks.push_back(blockExpr);

  for (const auto &stmt : blockExpr->statements)
    walker(stmt.get());

  if (blockExpr->finalexpr.has_value()) {
    auto *finalNode = blockExpr->finalexpr.value().get();
    walker(finalNode);

    if (currentFunction) {
      auto &fn = *currentFunction.value();
      if (fn.func().returnType.kind == DataType::VOID) {
        logSemanticErrors(ErrorCode::InvalidFinalExpression, finalNode);
      } else {
        ResolvedType exprType = inferNodeDataType(finalNode);
        bool nullMismatch =
            dynamic_cast<NullLiteral *>(finalNode) && !fn.type().isNullable;
        if (exprType.kind != DataType::ERROR &&
            !isTypeCompatible(fn.func().returnType, exprType) &&
            !nullMismatch) {
          logSemanticErrors(
              ErrorCode::TypeMismatch, finalNode,
              {exprType.resolvedName, fn.func().returnType.resolvedName});
        }
      }
    }
  }

  activeBlocks.pop_back();
}

void Semantics::walkReturnStatement(Node *node) {
  auto *retStmt = dynamic_cast<ReturnStatement *>(node);
  if (!retStmt) {
    reportDevBug("Invalid return statement node", node);
  }

  if (!currentFunction) {
    logSemanticErrors(ErrorCode::FloatingReturns, retStmt);
  }

  const ResolvedType &expectedReturn =
      currentFunction.value()->func().returnType;

  if (currentFunction.value()->func().isInterrupt) {
    if (expectedReturn.base().kind != DataType::VOID) {
      logSemanticErrors(ErrorCode::InterruptsMustBeVoid, retStmt);
      return;
    }
  }

  if (!retStmt->return_value) {
    if (expectedReturn.base().kind != DataType::VOID) {
      logSemanticErrors(ErrorCode::NonVoidReturn, retStmt,
                        {currentFunction.value()->func().funcName});
    }

    auto voidSym = std::make_shared<SymbolInfo>();
    voidSym->type().type = expectedReturn;
    voidSym->hasError = hasError;
    insertMetaData(retStmt, voidSym);
    return;
  }

  walker(retStmt->return_value.get());

  auto valSym = getSymbolFromMeta(retStmt->return_value.get());
  if (!valSym) {
    reportDevBug("Could not resolve return value symbol", retStmt);
  }

  giveGenericLiteralContext(retStmt->return_value.get(),
                            currentFunction.value()->type().type, valSym);

  ResolvedType valueType = valSym->type().type;
  if (valueType.base().kind == DataType::UNKNOWN)
    valueType = inferNodeDataType(retStmt->return_value.get());

  logInternal("Return value type: " + valueType.resolvedName);

  if (dynamic_cast<NullLiteral *>(retStmt->return_value.get())) {
    if (!expectedReturn.isNull) {
      logSemanticErrors(ErrorCode::InvalidNullReturn, retStmt);
    } else {
      valSym->type().type = expectedReturn;
    }
  } else if (!isTypeCompatible(expectedReturn, valueType)) {
    logSemanticErrors(ErrorCode::TypeMismatch, retStmt,
                      {expectedReturn.resolvedName, valueType.resolvedName});
  }

  if (valueType.isRef()) {
    if (valSym && !valSym->isParam) {
      if (!valSym->relations().refereeSymbol) {
        logSemanticErrors(ErrorCode::DanglingReferenceReturn, retStmt,
                          {extractIdentifierName(retStmt->return_value.get())});
        return;
      }
    }
  }

  // Transfer ownership baton to the caller.
  currentFunction->get()->codegen().ID = valSym->codegen().ID;
  // This doesnt mean the function is heap raised  it means the return value is
  // heap raised
  currentFunction->get()->storage().isHeap = valSym->storage().isHeap;
  currentFunction->get()->storage().allocType = valSym->storage().allocType;

  auto retSym = std::make_shared<SymbolInfo>();
  retSym->type().type = expectedReturn;
  retSym->hasError = hasError;
  retSym->codegen().ID = valSym->codegen().ID;

  if (valSym->storage().isHeap) {
    retSym->storage().allocType = valSym->storage().allocType;
    transferBaton(retStmt, retSym->codegen().ID);
  }
  insertMetaData(retStmt, retSym);
}

void Semantics::walkFunctionParameters(Node *node) {
  auto *param = dynamic_cast<VariableDeclaration *>(node);
  if (!param)
    reportDevBug("Function parameter is not a VariableDeclaration", node);

  const std::string &paramName = param->var_name->expression.TokenLiteral;

  if (lookUpInCurrentScope(paramName)) {
    logSemanticErrors(ErrorCode::DuplicateName, param, {paramName});
    return;
  }

  if (param->initializer) {
    logSemanticErrors(ErrorCode::NoParamDefaultVal, param);
    return;
  }

  auto *baseType = dynamic_cast<BasicType *>(param->base_type.get());
  if (baseType && baseType->data_token.type == TokenType::AUTO) {
    logSemanticErrors(ErrorCode::InvalidAutoUse, param);
    return;
  }

  ResolvedType resolvedType = inferNodeDataType(param);
  if (currentFunction.value()->func().isInterrupt &&
      isCustomTypeByValue(resolvedType)) {
    logSemanticErrors(ErrorCode::CannotPassCustomByVal, param,
                      {resolvedType.resolvedName});
    return;
  }

  if (param->isHeap) {
    logSemanticErrors(ErrorCode::InvalidHeapParam, param, {paramName});
    return;
  }

  if (resolvedType.isRef() && resolvedType.isNull) {
    logSemanticErrors(ErrorCode::InvalidNullReferenceParam, param, {paramName});
    return;
  }

  if (param->isPersist) {
    logSemanticErrors(ErrorCode::InvalidPersistParam, param, {paramName});
    return;
  }

  auto info = std::make_shared<SymbolInfo>();
  info->type().type = resolvedType;
  info->type().isFnPtr = resolvedType.kind == DataType::FUNC_PTR;
  info->storage().isMutable = (param->mutability == Mutability::MUTABLE);
  info->storage().isConstant =
      false; // parameters are never compile-time constants
  info->storage().isInitialized =
      true; // parameters are always initialised on entry
  info->storage().isHeap = false;
  info->type().isDefinitelyNull = false;
  info->isParam = true;
  info->hasError = hasError;

  insertMetaData(param, info);
  symbolTable.back()[paramName] = info;

  logInternal("Parameter '" + paramName +
              "' resolved type: " + resolvedType.resolvedName);
}

void Semantics::walkFunctionStatement(Node *node) {
  auto *funcStmt = dynamic_cast<FunctionStatement *>(node);
  if (!funcStmt)
    return;
  walker(funcStmt->funcExpr.get());
}

void Semantics::walkFunctionExpression(Node *node) {
  auto *funcExpr = dynamic_cast<FunctionExpression *>(node);
  if (!funcExpr)
    reportDevBug("Invalid function expression", node);

  const std::string funcName = funcExpr->func_key.TokenLiteral;
  const bool isExportable = funcExpr->isExportable;
  const bool isInterrupt = funcExpr->isInterrupt;
  const bool isNaked = funcExpr->isNaked;

  if (insideFunction) {
    logSemanticErrors(ErrorCode::IllegalFunctionDefinition, funcExpr);
    return;
  }

  insideFunction = true;

  auto existing = lookUpInCurrentScope(funcName);
  if (existing) {
    if (existing->isFunction) {
      if (existing->func().isDefined) {
        logSemanticErrors(ErrorCode::AlreadyDefinedFunc, funcExpr, {funcName});
        insideFunction = false;
        return;
      } else if (existing->func().isDeclaration) {
        if (!areSignaturesCompatible(*existing, funcExpr)) {
          logSemanticErrors(ErrorCode::DefnDeclMismatch, funcExpr, {funcName});
        }
      }
    } else {
      logSemanticErrors(ErrorCode::DuplicateName, funcExpr, {funcName});
    }
  }

  auto funcInfo =
      makeFuncSymbol(funcName, isExportable, isNaked, isInterrupt, hasError);
  funcInfo->func().isDeclaration = true;

  bool insideMemberContext = insideComponent || insideSeal || insideAllocator;
  if (!insideMemberContext)
    symbolTable[0][funcName] = funcInfo;
  else
    symbolTable.back()[funcName] = funcInfo;

  currentFunction = funcInfo;
  logInternal("Set current function to '" + funcName + "'");
  symbolTable.push_back({});

  std::vector<std::pair<ResolvedType, std::string>> paramTypes;

  for (const auto &param : funcExpr->call) {
    if (!param)
      continue;

    auto *varDecl = dynamic_cast<VariableDeclaration *>(param.get());
    if (!varDecl) {
      logSemanticErrors(ErrorCode::InvalidParam, param.get());
      continue;
    }

    const std::string &paramName = varDecl->var_name->expression.TokenLiteral;
    walkFunctionParameters(param.get());

    auto paramInfo = getSymbolFromMeta(param.get());
    if (!paramInfo) {
      reportDevBug("Parameter '" + paramName + "' was not analysed",
                   param.get());
      continue;
    }

    // Exportable function using a non-exportable custom type
    const std::string baseTypeName = getBaseTypeName(paramInfo->type().type);
    auto typeIt = customTypesTable.find(baseTypeName);
    if (typeIt != customTypesTable.end() && isExportable &&
        !typeIt->second->isExportable) {
      logSemanticErrors(ErrorCode::MatchExportsToTypes, param.get(),
                        {funcName, baseTypeName});
    }

    symbolTable.back()[paramName] = paramInfo;
    paramTypes.emplace_back(paramInfo->type().type, paramName);
    logInternal("Parameter '" + paramName +
                "' type: " + paramInfo->type().type.resolvedName);
  }

  if (!funcExpr->return_type) {
    logSemanticErrors(ErrorCode::MissingOrInvalidReturnType, funcExpr);
    insideFunction = false;
    return;
  }

  auto *retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
  if (!retType) {
    reportDevBug("Invalid return type node for '" + funcName + "'",
                 funcExpr->return_type.get());
    insideFunction = false;
    return;
  }

  ResolvedType returnType = inferNodeDataType(retType);
  if (returnType.base().kind == DataType::UNKNOWN) {
    logSemanticErrors(ErrorCode::MissingOrInvalidReturnType, retType);
  }

  // Exportable function using a non-exportable custom return type
  if (!retType->isVoid) {
    const std::string retBaseName = getBaseTypeName(returnType);
    auto retTypeIt = customTypesTable.find(retBaseName);
    if (retTypeIt != customTypesTable.end() && isExportable &&
        !retTypeIt->second->isExportable) {
      logSemanticErrors(ErrorCode::MatchExportsToTypes, retType,
                        {funcName, retBaseName});
    }
  }

  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;
  funcInfo->type().type = returnType;
  funcInfo->type().type = returnType;
  funcInfo->type().isPointer = returnType.isPointer();
  funcInfo->type().isArray = returnType.isArray();
  funcInfo->type().isRef = returnType.isRef();
  funcInfo->func().isNaked = isNaked;
  funcInfo->func().isInterrupt = isInterrupt;
  funcInfo->func().returnType = returnType;
  funcInfo->func().paramTypes = paramTypes;
  funcInfo->func().isDefined = true;

  if (!insideMemberContext)
    symbolTable[0][funcName] = funcInfo;
  else
    symbolTable.back()[funcName] = funcInfo;

  insertMetaData(funcExpr, funcInfo);
  currentFunction = funcInfo;
  logInternal("Finalised '" + funcName +
              "' return type: " + returnType.resolvedName);

  if (!funcExpr->block) {
    logSemanticErrors(ErrorCode::MissingOrInvalidBody, funcExpr);
    insideFunction = false;
    return;
  }

  auto *block = dynamic_cast<BlockExpression *>(funcExpr->block.get());
  if (!block) {
    reportDevBug("Invalid function body for '" + funcName + "'",
                 funcExpr->block.get());
    insideFunction = false;
    return;
  }

  logInternal("Walking body of '" + funcName + "'");
  activeBlocks.push_back(block);

  for (const auto &stmt : block->statements) {
    std::optional<std::shared_ptr<SymbolInfo>> savedFunc = currentFunction;
    walker(stmt.get());
    currentFunction = savedFunc;
  }

  if (returnType.base().kind != DataType::VOID && !hasReturnPath(block)) {
    logSemanticErrors(ErrorCode::NonVoidNoReturn, funcExpr, {funcName});
  }

  popScope();
  activeBlocks.pop_back();
  funcInfo->hasError = hasError;
  insideFunction = false;
  currentFunction = std::nullopt;
  logInternal("Finished analysing '" + funcName + "'");
}

void Semantics::walkFunctionDeclarationExpression(Node *node) {
  auto *funcDeclExpr = dynamic_cast<FunctionDeclarationExpression *>(node);
  if (!funcDeclExpr)
    return;
  walkFunctionDeclarationStatement(funcDeclExpr->funcDeclrStmt.get());
}

void Semantics::walkFunctionDeclarationStatement(Node *node) {
  auto *funcDeclrStmt = dynamic_cast<FunctionDeclaration *>(node);
  if (!funcDeclrStmt)
    reportDevBug("Invalid function declaration statement", node);

  const std::string funcName =
      funcDeclrStmt->function_name->expression.TokenLiteral;
  const bool isExportable = funcDeclrStmt->isExportable;
  const bool isNaked = funcDeclrStmt->isNaked;
  const bool isInterrupt = funcDeclrStmt->isInterrupt;

  if (insideFunction) {
    logSemanticErrors(ErrorCode::IllegalFunctionDeclaration, funcDeclrStmt);
    return;
  }

  auto existing = resolveSymbolInfo(funcName);
  if (existing) {
    if (existing->func().isDeclaration) {
      logSemanticErrors(ErrorCode::AlreadyDeclaredFunc, funcDeclrStmt,
                        {funcName});
    } else if (existing->func().isDefined) {
      logSemanticErrors(ErrorCode::AlreadyDefinedFunc, funcDeclrStmt,
                        {funcName});
    } else {
      logSemanticErrors(ErrorCode::DuplicateName, funcDeclrStmt, {funcName});
    }
    return;
  }

  auto funcInfo =
      makeFuncSymbol(funcName, isExportable, isNaked, isInterrupt, hasError);
  funcInfo->func().isDeclaration = true;

  currentFunction = funcInfo;
  symbolTable.push_back({});

  std::vector<std::pair<ResolvedType, std::string>> paramTypes;

  for (const auto &param : funcDeclrStmt->parameters) {
    if (!param)
      continue;

    auto *varDecl = dynamic_cast<VariableDeclaration *>(param.get());
    if (!varDecl) {
      logSemanticErrors(ErrorCode::InvalidParam, param.get());
      continue;
    }

    const std::string &paramName = varDecl->var_name->expression.TokenLiteral;
    walkFunctionParameters(param.get());

    auto paramIt = metaData.find(param.get());
    if (paramIt == metaData.end()) {
      reportDevBug("Parameter '" + paramName + "' could not be analysed.",
                   param.get());
    }

    auto &paramInfo = paramIt->second;
    const std::string baseTypeName = getBaseTypeName(paramInfo->type().type);
    auto typeIt = customTypesTable.find(baseTypeName);
    if (typeIt != customTypesTable.end() && isExportable &&
        !typeIt->second->isExportable) {
      logSemanticErrors(ErrorCode::MatchExportsToTypes, param.get(),
                        {funcName, baseTypeName});
    }

    // genericName is stored on TypeInfo in the new layout
    paramTypes.emplace_back(paramInfo->type().type,
                            paramInfo->generic().genericName);
  }

  auto *retType = dynamic_cast<ReturnType *>(funcDeclrStmt->return_type.get());
  if (!retType) {
    logSemanticErrors(ErrorCode::MissingOrInvalidReturnType, funcDeclrStmt);
    symbolTable.pop_back();
    currentFunction = std::nullopt;
    return;
  }

  ResolvedType returnType = inferNodeDataType(retType);
  if (returnType.base().kind == DataType::UNKNOWN) {
    logSemanticErrors(MissingOrInvalidReturnType, retType);
  }

  if (!retType->isVoid) {
    const std::string retBaseName = getBaseTypeName(returnType);
    auto retTypeIt = customTypesTable.find(retBaseName);
    if (retTypeIt != customTypesTable.end() && isExportable &&
        !retTypeIt->second->isExportable) {
      logSemanticErrors(ErrorCode::MatchExportsToTypes, retType,
                        {funcName, retBaseName});
    }
  }

  funcInfo->type().type = returnType;
  funcInfo->type().isPointer = returnType.isPointer();
  funcInfo->type().isArray = returnType.isArray();
  funcInfo->type().isRef = returnType.isRef();
  funcInfo->func().returnType = returnType;
  funcInfo->func().paramTypes = paramTypes;
  funcInfo->func().isInterrupt = isInterrupt;
  funcInfo->func().isNaked = isNaked;
  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;

  symbolTable.pop_back();
  currentFunction = std::nullopt;

  symbolTable.back()[funcName] = funcInfo;
  metaData[funcDeclrStmt] = funcInfo;

  logInternal("Stored declaration '" + funcName +
              "' return type: " + returnType.resolvedName);
}

void Semantics::analyzeFnPtrCall(
    CallExpression *callNode, std::vector<ResolvedType> &argTypes,
    std::vector<Node *> &toTransfer,
    const std::shared_ptr<SymbolInfo> &contextSym) {
  auto callName = extractIdentifierName(callNode);
  auto fnPtrType = contextSym->type().type;

  // In a dot context skip the first parameter as it is the implicit self
  logInternal("lhsNode is: " + (lhsNode ? lhsNode->toString() : "null"));

  size_t param_offset = (lhsNode != nullptr) ? 1 : 0;

  size_t expectedCount = (param_offset <= fnPtrType.fnParamTypes.size())
                             ? fnPtrType.fnParamTypes.size() - param_offset
                             : 0;

  if (argTypes.size() != expectedCount) {
    logSemanticErrors(ErrorCode::ArgumentSizeMismatch, callNode,
                      {callName, std::to_string(expectedCount),
                       std::to_string(argTypes.size())});
    return;
  }

  for (size_t i = 0; i < argTypes.size(); ++i) {
    auto expectedType = fnPtrType.fnParamTypes[i + param_offset];
    if (!isTypeCompatible(expectedType, argTypes[i])) {
      logSemanticErrors(ErrorCode::ArgumentTypeMismatch, callNode,
                        {std::to_string(i + 1), expectedType.resolvedName,
                         argTypes[i].resolvedName});
    }
  }

  auto callSym = std::make_shared<SymbolInfo>();
  callSym->hasError = hasError;
  callSym->type().type = *fnPtrType.fnReturnType;
  callSym->isFunction = true;
  callSym->type().isFnPtr = true;

  for (const auto &arg : toTransfer) {
    auto argSym = getSymbolFromMeta(arg);
    if (!argSym)
      reportDevBug("Failed to get argument symbol info", arg);

    if (argSym->storage().isHeap)
      transferBaton(arg, argSym->codegen().ID);
  }
  insertMetaData(callNode->function_identifier.get(), contextSym);
  insertMetaData(callNode, callSym);
}

void Semantics::walkFunctionCallExpression(Node *node) {
  auto *funcCall = dynamic_cast<CallExpression *>(node);
  if (!funcCall)
    reportDevBug("Invalid function call expression", node);

  const std::string callName = extractIdentifierName(funcCall);

  auto callSymbolInfo = resolveSymbolInfo(callName);

  if (!callSymbolInfo) {
    // If it fails maybe the guy is in a seal
    callSymbolInfo = getSealedFunctionSym(callName, lhsNode);
    // If it fails again check component instances
    if (!callSymbolInfo) {
      callSymbolInfo = getMemberSym(callName, lhsNode);
      // Now if it fails here there is no hope
      if (!callSymbolInfo) {
        logSemanticErrors(ErrorCode::UndefinedVariable, funcCall, {callName});
        return;
      }
    }
  }

  bool isFnPtr = callSymbolInfo->type().isFnPtr;

  if (!callSymbolInfo->isFunction && !isFnPtr) {
    logSemanticErrors(ErrorCode::NotaFuncOrFnPtr, funcCall, {callName});
    return;
  }

  if (!callSymbolInfo->func().isDeclaration && !isFnPtr)
    logSemanticErrors(ErrorCode::NotDefinedOrDeclared, funcCall, {callName});

  if (callSymbolInfo->func().isInterrupt)
    logSemanticErrors(ErrorCode::CannotCallInterrupts, funcCall, {callName});

  std::vector<Node *> toTransfer;
  std::vector<ResolvedType> argTypes;
  for (size_t i = 0; i < funcCall->parameters.size(); ++i) {
    const auto &arg = funcCall->parameters[i];
    walker(arg.get());
    auto argSym = getSymbolFromMeta(arg.get());
    if (!argSym)
      reportDevBug("Failed to get argument symbol info", arg.get());
    if (!isFnPtr) {
      giveGenericLiteralContext(
          arg.get(), callSymbolInfo->func().paramTypes[i].first, argSym);
      if (auto *nullLit = dynamic_cast<NullLiteral *>(arg.get())) {
        if (i < callSymbolInfo->func().paramTypes.size()) {
          auto expected = callSymbolInfo->func().paramTypes[i].first;
          if (expected.isNull) {
            metaData[nullLit]->type().type = expected;
          } else {
            logSemanticErrors(ErrorCode::NullPassFailure, arg.get(),
                              {std::to_string(i + 1), expected.resolvedName});
          }
        }
      }
    } else {
      giveGenericLiteralContext(
          arg.get(), callSymbolInfo->type().type.fnParamTypes[i], argSym);
      if (auto *nullLit = dynamic_cast<NullLiteral *>(arg.get())) {
        if (i < callSymbolInfo->type().type.fnParamTypes.size()) {
          auto expected = callSymbolInfo->type().type.fnParamTypes[i];
          if (expected.isNull) {
            metaData[nullLit]->type().type = expected;
          } else {
            logSemanticErrors(ErrorCode::NullPassFailure, arg.get(),
                              {std::to_string(i + 1), expected.resolvedName});
          }
        }
      }
    }

    argTypes.push_back(argSym->type().type);
    toTransfer.push_back(arg.get());
  }

  // This takes an entirely different branch
  if (isFnPtr) {
    analyzeFnPtrCall(funcCall, argTypes, toTransfer, callSymbolInfo);
    return;
  }

  if (!isCallCompatible(*callSymbolInfo, funcCall)) {
    hasError = true;
    return;
  }

  auto callSym = std::make_shared<SymbolInfo>();
  callSym->hasError = hasError;
  callSym->type().type = callSymbolInfo->func().returnType;
  callSym->type().isNullable = callSymbolInfo->type().isNullable;
  callSym->type().isPointer = callSymbolInfo->type().isPointer;
  callSym->type().isRef = callSymbolInfo->type().isRef;
  callSym->type().isArray = callSymbolInfo->type().isArray;
  callSym->codegen().ID = callSymbolInfo->codegen().ID;
  // This means the return value was heap raised
  callSym->storage().isHeap = callSymbolInfo->storage().isHeap;
  callSym->storage().allocType = callSymbolInfo->storage().allocType;

  /* I am doing this because there are two issues I am combatting I dont want
   the baton to escape its scope if the return type is not a pointer at the same
   time I dont wanna block heap variables that are being passed into the call*/
  if (callSym->type().isPointer) {
    if (callSym->storage().isHeap) {
      insertMetaData(funcCall->function_identifier.get(), callSym);
      transferBaton(funcCall, callSym->codegen().ID);
    }
  } else {
    for (const auto &arg : toTransfer) {
      auto argSym = getSymbolFromMeta(arg);
      if (!argSym)
        reportDevBug("Failed to get argument symbol info", arg);

      logInternal("IS THE ARG HEAP RAISED: " +
                  std::to_string(argSym->storage().isHeap));
      if (argSym->storage().isHeap)
        transferBaton(arg, argSym->codegen().ID);
    }
  }

  insertMetaData(funcCall, callSym);
}

void Semantics::walkTraceStatement(Node *node) {
  auto *traceStmt = dynamic_cast<TraceStatement *>(node);
  if (!traceStmt)
    return;

  if (freeStanding) {
    logSemanticErrors(ErrorCode::IllegalUseInFreeStanding, traceStmt,
                      {"trace"});
    return;
  }

  if (isGlobalScope()) {
    logSemanticErrors(ErrorCode::FloatingTrace, traceStmt);
    return;
  }

  insideTrace = true;

  for (const auto &expr : traceStmt->arguments) {
    walker(expr.get());
    auto argInfo = getSymbolFromMeta(expr.get());
    if (!argInfo)
      reportDevBug("Failed to get argument info", expr.get());

    if (argInfo->storage().isHeap)
      transferBaton(expr.get(), argInfo->codegen().ID);
  }

  insideTrace = false;
}

void Semantics::walkSealStatement(Node *node) {
  auto *sealStmt = dynamic_cast<SealStatement *>(node);
  if (!sealStmt)
    return;

  const std::string sealName = sealStmt->sealName->expression.TokenLiteral;
  const bool isExportable = sealStmt->isExportable;

  if (resolveSymbolInfo(sealName)) {
    logSemanticErrors(ErrorCode::DuplicateName, sealStmt->sealName.get(),
                      {sealName});
  }

  std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> sealMap;

  symbolTable.push_back({});
  insideSeal = true;

  auto *blockStmt = dynamic_cast<BlockStatement *>(sealStmt->block.get());
  if (!blockStmt) {
    logSemanticErrors(ErrorCode::MissingOrInvalidBody,
                      sealStmt->sealName.get());
    return;
  }

  for (const auto &stmt : blockStmt->statements) {
    auto *funcStmt = dynamic_cast<FunctionStatement *>(stmt.get());
    if (!funcStmt) {
      logSemanticErrors(ErrorCode::IllegalStmtInSeal, stmt.get());
      return;
    }

    walker(funcStmt);

    std::string name;
    if (auto *fnExpr =
            dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get())) {
      name = fnExpr->func_key.TokenLiteral;
      logInternal("Sealing function '" + name + "'");

      // Mangle the name so IRGen can distinguish seal members.
      const std::string mangled = sealName + "_" + name;
      fnExpr->func_key.TokenLiteral = mangled;
      fnExpr->token.TokenLiteral = mangled;
      fnExpr->expression.TokenLiteral = mangled;
      logInternal("Mangled to '" + mangled + "'");

      if (isExportable)
        fnExpr->isExportable = true;
    } else if (dynamic_cast<FunctionDeclarationExpression *>(
                   funcStmt->funcExpr.get())) {
      logSemanticErrors(ErrorCode::IllegalFunctionDeclaration,
                        funcStmt->funcExpr.get());
      return;
    }

    auto funcSym = resolveSymbolInfo(name);
    if (!funcSym)
      reportDevBug("Internal error: could not find '" + name +
                       "' after walking it.",
                   funcStmt);

    if (isExportable)
      funcSym->isExportable = true;

    funcSym->isFunction = true;
    sealMap[name] = funcSym;
  }

  auto sealSym = std::make_shared<SymbolInfo>();
  sealSym->isExportable = isExportable;
  sealSym->hasError = hasError;

  insertMetaData(sealStmt, sealSym);
  symbolTable[0][sealName] = sealSym;
  sealTable[sealName] = sealMap;

  insideSeal = false;
  popScope();
}
