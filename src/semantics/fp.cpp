#include "ast.hpp"
#include "semantics.hpp"
#include <memory>

static std::shared_ptr<SymbolInfo>
makeFuncSymbol(const std::string &name, bool isExportable, bool hasError) {
  auto sym = std::make_shared<SymbolInfo>();
  sym->isFunction = true;
  sym->isExportable = isExportable;
  sym->hasError = hasError;
  sym->func().funcName = name;
  sym->func().isDeclaration = false;
  sym->func().isDefined = false;
  sym->func().returnType = ResolvedType::unknown();
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
        logSemanticErrors("A void function cannot have a final expression. "
                          "Remove the expression or change the return type.",
                          finalNode);
      } else {
        ResolvedType exprType = inferNodeDataType(finalNode);
        bool nullMismatch =
            dynamic_cast<NullLiteral *>(finalNode) && !fn.type().isNullable;
        if (exprType.kind != DataType::ERROR &&
            !isTypeCompatible(fn.func().returnType, exprType) &&
            !nullMismatch) {
          logSemanticErrors("Final expression has type '" +
                                exprType.resolvedName +
                                "' but the function returns '" +
                                fn.func().returnType.resolvedName + "'.",
                            finalNode);
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
    errorHandler.addHint("Return statements must live inside a function body.")
        .addHint("Move this return statement into a function.");
    logSemanticErrors("Return statement found outside of a function body",
                      retStmt);
  }

  const ResolvedType &expectedReturn =
      currentFunction.value()->func().returnType;

  if (!retStmt->return_value) {
    if (expectedReturn.base().kind != DataType::VOID) {
      errorHandler
          .addHint("This function must return a value of type '" +
                   expectedReturn.resolvedName + "'.")
          .addHint("Add a return value, or change the return type to 'void'.");
      logSemanticErrors("Non-void function '" +
                            currentFunction.value()->func().funcName +
                            "' has a return statement with no value.",
                        retStmt);
    }

    auto voidSym = std::make_shared<SymbolInfo>();
    voidSym->type().type = expectedReturn;
    voidSym->hasError = hasError;
    metaData[retStmt] = voidSym;
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
      errorHandler
          .addHint("The return type '" + expectedReturn.resolvedName +
                   "' is not nullable.")
          .addHint("Add '?' to the return type to allow null (e.g. 'i32?').")
          .addHint("Example: func foo: i32? { return null }");
      logSemanticErrors(
          "Cannot return 'null' from a function whose return type '" +
              expectedReturn.resolvedName + "' is not nullable.",
          retStmt);
    } else {
      valSym->type().type = expectedReturn;
    }
  } else if (!isTypeCompatible(expectedReturn, valueType)) {
    errorHandler.addHint("Expected: " + expectedReturn.resolvedName)
        .addHint("Got:      " + valueType.resolvedName)
        .addHint("Check the function signature or the return expression.");
    logSemanticErrors("Return type mismatch in function '" +
                          currentFunction.value()->func().funcName + "'.",
                      retStmt);
  }

  if (valueType.isRef()) {
    if (valSym && !valSym->isParam) {
      if (!valSym->relations().refereeSymbol) {
        errorHandler
            .addHint("Returning a reference to a local variable is unsafe "
                     "the memory is freed when the function returns.")
            .addHint("Return the value directly, or heap-allocate it first.");
        logSemanticErrors(
            "Cannot return a dangling reference to local variable '" +
                extractIdentifierName(retStmt->return_value.get()) + "'.",
            retStmt);
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
  metaData[retStmt] = retSym;
}

void Semantics::walkFunctionParameters(Node *node) {
  auto *param = dynamic_cast<VariableDeclaration *>(node);
  if (!param) {
    reportDevBug("Function parameter is not a VariableDeclaration", node);
  }

  logInternal("Analyzing function parameter...");
  const std::string &paramName = param->var_name->expression.TokenLiteral;

  if (lookUpInCurrentScope(paramName)) {
    errorHandler.addHint("Each parameter must have a unique name.")
        .addHint("Rename one of the '" + paramName + "' parameters.");
    logSemanticErrors("Duplicate parameter name '" + paramName + "'.", param);
    return;
  }

  auto *baseType = dynamic_cast<BasicType *>(param->base_type.get());
  if (baseType && baseType->data_token.type == TokenType::AUTO) {
    errorHandler.addHint("Parameter types must be written out explicitly.")
        .addHint(
            "Replace 'auto' with a concrete type such as 'i32' or 'ptr i32'.")
        .addHint("Example: func foo(i32 x, ptr i32 p): i32 { ... }");
    logSemanticErrors("Parameter '" + paramName +
                          "' cannot use 'auto' "
                          "the type must be explicit.",
                      param);
    return;
  }

  ResolvedType base = inferDeclarationBaseType(param);
  ResolvedType resolvedType =
      resolveTypeWithModifier(param->modified_type.get(), base);

  if (param->isHeap) {
    errorHandler
        .addHint("Parameters are managed by the caller, heap allocation "
                 "inside a parameter declaration is not allowed.")
        .addHint("Remove 'heap' from the parameter.")
        .addHint(
            "If you need heap data, accept a pointer instead: 'ptr i32 p'.");
    logSemanticErrors("Parameter '" + paramName + "' cannot be heap-allocated.",
                      param);
    return;
  }

  if (resolvedType.isRef() && resolvedType.isNull) {
    errorHandler
        .addHint("References always point to valid memory, so nullable "
                 "references are contradictory.")
        .addHint("Remove '?' from the reference type.")
        .addHint(
            "If null is a valid state, use a nullable pointer: 'ptr i32?'.");
    logSemanticErrors(
        "Reference parameter '" + paramName + "' cannot be nullable.", param);
    return;
  }

  if (param->isPersist) {
    errorHandler
        .addHint(
            "'persist' only applies to heap variables inside function bodies.")
        .addHint("Remove 'persist' from the parameter declaration.");
    logSemanticErrors(
        "Parameter '" + paramName + "' cannot be marked 'persist'.", param);
    return;
  }

  auto info = std::make_shared<SymbolInfo>();
  info->type().type = resolvedType;
  info->storage().isMutable = (param->mutability == Mutability::MUTABLE);
  info->storage().isConstant =
      false; // parameters are never compile-time constants
  info->storage().isInitialized =
      true; // parameters are always initialised on entry
  info->storage().isHeap = false;
  info->type().isDefinitelyNull = false;
  info->isParam = true;
  info->hasError = hasError;

  metaData[param] = info;
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

  if (insideFunction) {
    errorHandler.addHint("Move the inner function to the top level.")
        .addHint("Unnameable does not support nested function definitions.");
    logSemanticErrors("Function '" + funcName +
                          "' cannot be defined inside another function.",
                      funcExpr);
    return;
  }

  insideFunction = true;

  auto existing = lookUpInCurrentScope(funcName);
  if (existing) {
    if (existing->isFunction) {
      if (existing->func().isDefined) {
        errorHandler
            .addHint("Function names must be unique within their scope.")
            .addHint("Rename one of the '" + funcName + "' functions.");
        logSemanticErrors("Function '" + funcName + "' is already defined.",
                          funcExpr);
        insideFunction = false;
        return;
      } else if (existing->func().isDeclaration) {
        if (!areSignaturesCompatible(*existing, funcExpr)) {
          errorHandler
              .addHint("The definition signature must match the earlier "
                       "declaration.")
              .addHint("Check parameter types and the return type.");
          logSemanticErrors("Definition of '" + funcName +
                                "' does not match its prior declaration.",
                            funcExpr);
        }
      }
    } else {
      errorHandler
          .addHint("'" + funcName + "' is already used as a variable name.")
          .addHint("Choose a different name for this function.");
      logSemanticErrors("Name '" + funcName + "' is already in use.", funcExpr);
    }
  }

  auto funcInfo = makeFuncSymbol(funcName, isExportable, hasError);
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
      errorHandler
          .addHint("Parameters must follow the unified declaration syntax.")
          .addHint("Example: ptr i32 x, mut i32 y, ref i32 z");
      logSemanticErrors("Invalid parameter in function '" + funcName + "'.",
                        param.get());
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
      errorHandler
          .addHint("Mark '" + baseTypeName +
                   "' as exportable, or use a different type.")
          .addHint("Exportable functions may only use exportable types.");
      logSemanticErrors("Exportable function '" + funcName +
                            "' uses non-exportable type '" + baseTypeName +
                            "' for parameter '" + paramName + "'.",
                        param.get());
    }

    symbolTable.back()[paramName] = paramInfo;
    paramTypes.emplace_back(paramInfo->type().type, paramName);
    logInternal("Parameter '" + paramName +
                "' type: " + paramInfo->type().type.resolvedName);
  }

  if (!funcExpr->return_type) {
    errorHandler.addHint("All functions must declare a return type.")
        .addHint("Use 'void' if the function returns nothing.")
        .addHint("Example: func foo: void { ... }");
    logSemanticErrors("Function '" + funcName + "' is missing a return type.",
                      funcExpr);
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
    errorHandler
        .addHint("Valid return types: i32, ptr i32, arr[N] i32, void, etc.")
        .addHint("For custom types, make sure they are declared before this "
                 "function.");
    logSemanticErrors("Function '" + funcName + "' has an invalid return type.",
                      retType);
  }

  // Exportable function using a non-exportable custom return type
  if (!retType->isVoid) {
    const std::string retBaseName = getBaseTypeName(returnType);
    auto retTypeIt = customTypesTable.find(retBaseName);
    if (retTypeIt != customTypesTable.end() && isExportable &&
        !retTypeIt->second->isExportable) {
      errorHandler
          .addHint("Mark '" + retBaseName +
                   "' as exportable, or change the return type.")
          .addHint("Exportable functions may only use exportable types.");
      logSemanticErrors("Exportable function '" + funcName +
                            "' has non-exportable return type '" + retBaseName +
                            "'.",
                        retType);
    }
  }

  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;
  funcInfo->type().type = returnType;
  funcInfo->type().type = returnType;
  funcInfo->type().isPointer = returnType.isPointer();
  funcInfo->type().isArray = returnType.isArray();
  funcInfo->type().isRef = returnType.isRef();
  funcInfo->func().returnType = returnType;
  funcInfo->func().paramTypes = paramTypes;
  funcInfo->func().isDefined = true;

  if (!insideMemberContext)
    symbolTable[0][funcName] = funcInfo;
  else
    symbolTable.back()[funcName] = funcInfo;

  metaData[funcExpr] = funcInfo;
  currentFunction = funcInfo;
  logInternal("Finalised '" + funcName +
              "' return type: " + returnType.resolvedName);

  if (!funcExpr->block) {
    errorHandler.addHint("Add a body enclosed in '{}'.")
        .addHint("Example: func foo: void { ... }");
    logSemanticErrors("Function '" + funcName + "' is missing a body.",
                      funcExpr);
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
    errorHandler.addHint("Add a return statement at the end of the function.")
        .addHint("Every code path must return a value of type '" +
                 returnType.resolvedName + "'.");
    logSemanticErrors(
        "Non-void function '" + funcName + "' has no return value.", funcExpr);
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

  if (insideFunction) {
    errorHandler.addHint("Move the declaration to the top-level scope.")
        .addHint("Function declarations cannot be nested.");
    logSemanticErrors("Function declaration '" + funcName +
                          "' cannot appear inside another function.",
                      funcDeclrStmt);
    return;
  }

  auto existing = resolveSymbolInfo(funcName);
  if (existing) {
    if (existing->func().isDeclaration) {
      errorHandler
          .addHint("A declaration for '" + funcName +
                   "' already exists in this scope.")
          .addHint("Remove the duplicate declaration.");
      logSemanticErrors("'" + funcName + "' has already been declared.",
                        funcDeclrStmt);
    } else if (existing->func().isDefined) {
      errorHandler.addHint("'" + funcName + "' already has a full definition.")
          .addHint("The definition is sufficient — remove this declaration.");
      logSemanticErrors("'" + funcName + "' has already been defined.",
                        funcDeclrStmt);
    } else {
      errorHandler.addHint("Choose a different name for this function.");
      logSemanticErrors("Name '" + funcName + "' is already in use.",
                        funcDeclrStmt);
    }
    return;
  }

  auto funcInfo = makeFuncSymbol(funcName, isExportable, hasError);
  funcInfo->func().isDeclaration = true;

  currentFunction = funcInfo;
  symbolTable.push_back({});

  std::vector<std::pair<ResolvedType, std::string>> paramTypes;

  for (const auto &param : funcDeclrStmt->parameters) {
    if (!param)
      continue;

    auto *varDecl = dynamic_cast<VariableDeclaration *>(param.get());
    if (!varDecl) {
      errorHandler
          .addHint("Parameters must use the unified declaration syntax.")
          .addHint("Example: ptr i32 x, mut i32 y, ref i32 z");
      logSemanticErrors("Invalid parameter in declaration '" + funcName + "'.",
                        param.get());
      continue;
    }

    const std::string &paramName = varDecl->var_name->expression.TokenLiteral;
    walkFunctionParameters(param.get());

    auto paramIt = metaData.find(param.get());
    if (paramIt == metaData.end()) {
      logSemanticErrors("Parameter '" + paramName + "' could not be analysed.",
                        param.get());
      continue;
    }

    auto &paramInfo = paramIt->second;
    const std::string baseTypeName = getBaseTypeName(paramInfo->type().type);
    auto typeIt = customTypesTable.find(baseTypeName);
    if (typeIt != customTypesTable.end() && isExportable &&
        !typeIt->second->isExportable) {
      errorHandler
          .addHint("Mark '" + baseTypeName +
                   "' as exportable, or use a different type.")
          .addHint("Exportable functions may only use exportable types.");
      logSemanticErrors("Exportable declaration '" + funcName +
                            "' uses non-exportable type '" + baseTypeName +
                            "' for parameter '" + paramName + "'.",
                        param.get());
    }

    // genericName is stored on TypeInfo in the new layout
    paramTypes.emplace_back(paramInfo->type().type,
                            paramInfo->generic().genericName);
  }

  auto *retType = dynamic_cast<ReturnType *>(funcDeclrStmt->return_type.get());
  if (!retType) {
    errorHandler.addHint("All function declarations must have a return type.")
        .addHint("Use 'void' if the function returns nothing.");
    logSemanticErrors("Declaration '" + funcName +
                          "' is missing a return type.",
                      funcDeclrStmt);
    symbolTable.pop_back();
    currentFunction = std::nullopt;
    return;
  }

  ResolvedType returnType = inferNodeDataType(retType);
  if (returnType.base().kind == DataType::UNKNOWN) {
    errorHandler
        .addHint("Valid return types: i32, ptr i32, arr[N] i32, void, etc.")
        .addHint("For custom types, make sure they are declared before this "
                 "function.");
    logSemanticErrors(
        "Declaration '" + funcName + "' has an invalid return type.", retType);
  }

  if (!retType->isVoid) {
    const std::string retBaseName = getBaseTypeName(returnType);
    auto retTypeIt = customTypesTable.find(retBaseName);
    if (retTypeIt != customTypesTable.end() && isExportable &&
        !retTypeIt->second->isExportable) {
      errorHandler
          .addHint("Mark '" + retBaseName +
                   "' as exportable, or change the return type.")
          .addHint("Exportable functions may only use exportable types.");
      logSemanticErrors("Exportable declaration '" + funcName +
                            "' has non-exportable return type '" + retBaseName +
                            "'.",
                        retType);
    }
  }

  funcInfo->type().type = returnType;
  funcInfo->type().isPointer = returnType.isPointer();
  funcInfo->type().isArray = returnType.isArray();
  funcInfo->type().isRef = returnType.isRef();
  funcInfo->func().returnType = returnType;
  funcInfo->func().paramTypes = paramTypes;
  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;

  symbolTable.pop_back();
  currentFunction = std::nullopt;

  symbolTable.back()[funcName] = funcInfo;
  metaData[funcDeclrStmt] = funcInfo;

  logInternal("Stored declaration '" + funcName +
              "' return type: " + returnType.resolvedName);
}

void Semantics::walkFunctionCallExpression(Node *node) {
  auto *funcCall = dynamic_cast<CallExpression *>(node);
  if (!funcCall)
    reportDevBug("Invalid function call expression", node);

  const std::string callName =
      funcCall->function_identifier->expression.TokenLiteral;

  auto callSymbolInfo = resolveSymbolInfo(callName);
  if (!callSymbolInfo) {
    logSemanticErrors(
        "'" + callName +
            "' has not been declared or defined. "
            "Make sure it is spelled correctly and visible in this scope.",
        funcCall);
    return;
  }

  if (!callSymbolInfo->isFunction) {
    logSemanticErrors("'" + callName +
                          "' is a variable, not a function. "
                          "Did you mean to call a different name?",
                      funcCall);
    return;
  }

  if (!callSymbolInfo->func().isDeclaration) {
    logSemanticErrors("'" + callName +
                          "' has not been defined anywhere. "
                          "Add a definition or a forward declaration.",
                      funcCall);
  }

  std::vector<Node *> toTransfer;
  for (size_t i = 0; i < funcCall->parameters.size(); ++i) {
    const auto &arg = funcCall->parameters[i];
    walker(arg.get());
    auto argSym = getSymbolFromMeta(arg.get());
    if (!argSym)
      reportDevBug("Failed to get argument symbol info", arg.get());
    giveGenericLiteralContext(
        arg.get(), callSymbolInfo->func().paramTypes[i].first, argSym);

    if (auto *nullLit = dynamic_cast<NullLiteral *>(arg.get())) {
      if (i < callSymbolInfo->func().paramTypes.size()) {
        auto expected = callSymbolInfo->func().paramTypes[i].first;
        if (expected.isNull) {
          metaData[nullLit]->type().type = expected;
        } else {
          logSemanticErrors("Cannot pass 'null' to parameter " +
                                std::to_string(i + 1) + " of '" + callName +
                                "' — it is not nullable.",
                            arg.get());
        }
      }
    }
    toTransfer.push_back(arg.get());
  }

  if (!isCallCompatible(*callSymbolInfo, funcCall)) {
    logSemanticErrors("Incompatible call", funcCall);
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
  logInternal("IS THE CALL HEAP RAISED: " +
              std::to_string(callSym->storage().isHeap));

  logInternal("IS THE RETURN VALUE OF THIS CALL A POINTER: " +
              std::to_string(callSym->type().isPointer));
  if (callSym->type().isPointer) {
    if (callSym->storage().isHeap) {
      metaData[funcCall->function_identifier.get()] = callSym;
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

  metaData[funcCall] = callSym;
}

void Semantics::walkTraceStatement(Node *node) {
  auto *traceStmt = dynamic_cast<TraceStatement *>(node);
  if (!traceStmt)
    return;

  if (isGlobalScope()) {
    logSemanticErrors("Trace statements are not allowed in global scope. "
                      "Move this inside a function.",
                      traceStmt);
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

void Semantics::walkSealCallExpression(Node *node,
                                       const std::string &sealName) {
  auto *funcCall = dynamic_cast<CallExpression *>(node);
  if (!funcCall)
    reportDevBug("Invalid function call expression", node);

  const std::string callName =
      funcCall->function_identifier->expression.TokenLiteral;

  auto sealIt = sealTable.find(sealName);
  if (sealIt == sealTable.end()) {
    logSemanticErrors(
        "Seal '" + sealName +
            "' does not exist. "
            "Check the seal name or make sure it has been declared.",
        funcCall);
    return;
  }

  auto &sealFnMap = sealIt->second;
  auto sealFnIt = sealFnMap.find(callName);
  if (sealFnIt == sealFnMap.end()) {
    logSemanticErrors("Seal '" + sealName + "' has no function named '" +
                          callName +
                          "'. "
                          "Check the seal definition for a typo.",
                      funcCall);
    return;
  }

  auto callSymbolInfo = sealFnIt->second;
  if (!callSymbolInfo) {
    logSemanticErrors("'" + callName + "' in seal '" + sealName +
                          "' has not been declared or defined.",
                      funcCall);
    return;
  }

  if (!callSymbolInfo->isFunction) {
    logSemanticErrors(
        "'" + callName + "' in seal '" + sealName +
            "' is not a function. Did you mean to call a different name?",
        funcCall);
    return;
  }

  if (!callSymbolInfo->func().isDeclaration) {
    logSemanticErrors("'" + callName + "' in seal '" + sealName +
                          "' has no definition. Add a function body.",
                      funcCall);
  }

    std::vector<Node *> toTransfer;
  for (size_t i = 0; i < funcCall->parameters.size(); ++i) {
    const auto &arg = funcCall->parameters[i];
    walker(arg.get());
    auto argSym = getSymbolFromMeta(arg.get());
    if (!argSym)
      reportDevBug("Failed to get argument symbol info", arg.get());
    giveGenericLiteralContext(
        arg.get(), callSymbolInfo->func().paramTypes[i].first, argSym);

    if (auto *nullLit = dynamic_cast<NullLiteral *>(arg.get())) {
      if (i < callSymbolInfo->func().paramTypes.size()) {
        auto expected = callSymbolInfo->func().paramTypes[i].first;
        if (expected.isNull) {
          metaData[nullLit]->type().type = expected;
        } else {
          logSemanticErrors("Cannot pass 'null' to parameter " +
                                std::to_string(i + 1) + " of '" + callName +
                                "' in seal '" + sealName +
                                "' it is not nullable.",
                            arg.get());
        }
      }
    }
    toTransfer.push_back(arg.get());
  }

  if (!isCallCompatible(*callSymbolInfo, funcCall)) {
    logSemanticErrors("Incompatible call", funcCall);
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

  if(callSym->type().isPointer){
      if(callSym->storage().isHeap){
          metaData[funcCall->function_identifier.get()]=callSym;
          transferBaton(funcCall,callSym->codegen().ID);
      }
  }else{
      for(const auto &arg:toTransfer){
          auto argSym=getSymbolFromMeta(arg);
          if(!argSym)
              reportDevBug("Failed to get argumant symbol info",arg);
          if(argSym->storage().isHeap)
                transferBaton(arg,argSym->codegen().ID);
      }
  }

  metaData[funcCall] = callSym;
  logInternal("Stored metaData for seal call to '" + callName +
              "' return type: " + callSymbolInfo->type().type.resolvedName);
}

void Semantics::walkSealStatement(Node *node) {
  auto *sealStmt = dynamic_cast<SealStatement *>(node);
  if (!sealStmt)
    return;

  const std::string sealName = sealStmt->sealName->expression.TokenLiteral;
  const bool isExportable = sealStmt->isExportable;

  if (resolveSymbolInfo(sealName)) {
    logSemanticErrors("'" + sealName +
                          "' is already defined in this scope. "
                          "Choose a different name for the seal.",
                      sealStmt->sealName.get());
  }

  std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> sealMap;

  symbolTable.push_back({});
  insideSeal = true;

  auto *blockStmt = dynamic_cast<BlockStatement *>(sealStmt->block.get());
  if (!blockStmt) {
    logSemanticErrors("Seal '" + sealName +
                          "' has an invalid or missing body. "
                          "Add a block containing function definitions.",
                      sealStmt->sealName.get());
    return;
  }

  for (const auto &stmt : blockStmt->statements) {
    auto *funcStmt = dynamic_cast<FunctionStatement *>(stmt.get());
    if (!funcStmt) {
      logSemanticErrors("Only function definitions are allowed inside a seal. "
                        "Remove the non-function statement.",
                        stmt.get());
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
      logSemanticErrors("Function declarations cannot appear inside a seal. "
                        "Provide a full function definition instead.",
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

  metaData[sealStmt] = sealSym;
  symbolTable[0][sealName] = sealSym;
  sealTable[sealName] = sealMap;

  insideSeal = false;
  popScope();
}
