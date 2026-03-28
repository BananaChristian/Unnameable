#include "ast.hpp"
#include "semantics.hpp"
#include <memory>

void Semantics::walkBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr)
    return;
  // Analysing the statements inside the block expression
  activeBlocks.push_back(blockExpr);
  auto &stmts = blockExpr->statements;
  for (const auto &stmt : stmts) {
    walker(stmt.get());
  }

  // Checking for the final expression if any
  if (blockExpr->finalexpr.has_value()) {
    walker(blockExpr->finalexpr.value().get());
    if (currentFunction) {
      if (currentFunction.value()->returnType.kind == DataType::VOID) {
        logSemanticErrors("Void function cannot have a final expression",
                          blockExpr->finalexpr.value().get());
      } else {
        ResolvedType exprType =
            inferNodeDataType(blockExpr->finalexpr.value().get());
        if (exprType.kind != DataType::ERROR &&
            !isTypeCompatible(currentFunction.value()->returnType, exprType) &&
            !(dynamic_cast<NullLiteral *>(blockExpr->finalexpr.value().get()) &&
              currentFunction.value()->isNullable)) {
          logSemanticErrors(
              "Final expression type " + exprType.resolvedName +
                  " does not match function return type " +
                  currentFunction.value()->returnType.resolvedName,
              blockExpr->finalexpr.value().get());
        }
      }
    }
  }
  activeBlocks.pop_back();
}

void Semantics::walkReturnStatement(Node *node) {
  auto retStmt = dynamic_cast<ReturnStatement *>(node);
  if (!retStmt) {
    reportDevBug("Invalid return statement node", node);
    return;
  }

  bool hasError = false;

  // Must be inside a function
  if (!currentFunction) {
    errorHandler.addHint("Return statements must be inside a function body")
        .addHint("Move this return statement inside a function");
    logSemanticErrors("Return statement outside of function", retStmt);
    hasError = true;
  }

  const ResolvedType &expectedReturn = currentFunction.value()->returnType;

  if (!retStmt->return_value) {
    if (expectedReturn.base().kind != DataType::VOID) {
      errorHandler
          .addHint("Function must return a value of type '" +
                   expectedReturn.resolvedName + "'")
          .addHint("Add a return value or change the return type to void");
      logSemanticErrors("Non-void function '" +
                            currentFunction.value()->funcname +
                            "' missing return value",
                        retStmt);
      hasError = true;
    }

    auto voidInfo = std::make_shared<SymbolInfo>();
    voidInfo->type = expectedReturn;
    voidInfo->hasError = hasError;
    metaData[retStmt] = voidInfo;
    return;
  }

  walker(retStmt->return_value.get());

  ResolvedType valueType = ResolvedType::unknown();

  auto valSym = getSymbolFromMeta(retStmt->return_value.get());
  if (!valSym) {
    reportDevBug("Could not find return value symbol info", retStmt);
    return;
  }
  
  giveGenericIntegerContext(retStmt->return_value.get(),
                            currentFunction.value(), valSym);
  
  valueType=valSym->type;

  // Fallback to inference
  if (valueType.base().kind == DataType::UNKNOWN)
    valueType = inferNodeDataType(retStmt->return_value.get());

  logInternal("Return value type: " + valueType.resolvedName);

  if (dynamic_cast<NullLiteral *>(retStmt->return_value.get())) {
    if (!expectedReturn.isNull) {
      errorHandler.addHint("The function return type is not nullable")
          .addHint("Add '?' to the return type to allow null returns")
          .addHint("Example: func foo: i32? { return null }");
      logSemanticErrors("Cannot return 'null' for non-nullable return type '" +
                            expectedReturn.resolvedName + "'",
                        retStmt);
      hasError = true;
    } else {
      valSym->type = expectedReturn;
    }
  } else if (!isTypeCompatible(expectedReturn, valueType)) {
    errorHandler.addHint("Expected return type: " + expectedReturn.resolvedName)
        .addHint("Got return type: " + valueType.resolvedName)
        .addHint("Check the function signature or the return expression");
    logSemanticErrors("Return type mismatch in function '" +
                          currentFunction.value()->funcname + "'",
                      retStmt);
  }

  if (valueType.isRef()) {
    if (valSym && !valSym->isParam) {
      if (!valSym->refereeSymbol) {
        errorHandler
            .addHint("Returning a reference to a local variable is unsafe")
            .addHint("The memory will be invalid after the function returns")
            .addHint("Return the value directly or allocate on heap instead");
        logSemanticErrors(
            "Cannot return dangling reference '" +
                extractIdentifierName(retStmt->return_value.get()) + "'",
            retStmt);
        return;
      }
    }
  }

  // Transfer baton to function for retrieval
  currentFunction->get()->ID = valSym->ID;

  auto info = std::make_shared<SymbolInfo>();
  info->type = expectedReturn;
  info->hasError = hasError;
  info->ID = valSym->ID;

  Node *holder = queryForLifeTimeBaton(info->ID);
  responsibilityTable[retStmt] = std::move(responsibilityTable[holder]);
  metaData[retStmt] = info;
}

void Semantics::walkFunctionParameters(Node *node) {
  auto param = dynamic_cast<VariableDeclaration *>(node);
  if (!param) {
    reportDevBug("Function parameter is not a VariableDeclaration", node);
    return;
  }

  logInternal("Analyzing function parameter...");

  const std::string &paramName = param->var_name->expression.TokenLiteral;

  // No duplicate parameter names
  if (lookUpInCurrentScope(paramName)) {
    errorHandler.addHint("Each parameter must have a unique name")
        .addHint("Rename one of the '" + paramName + "' parameters");
    logSemanticErrors("Duplicate parameter name '" + paramName + "'", param);
    return;
  }

  // Parameters cannot use auto — type must be explicit
  auto baseType = dynamic_cast<BasicType *>(param->base_type.get());
  if (baseType && baseType->data_token.type == TokenType::AUTO) {
    errorHandler.addHint("Parameter types must be explicitly declared")
        .addHint("Replace 'auto' with a concrete type like 'i32' or 'ptr i32'")
        .addHint("Example: func foo(i32 x, ptr i32 p):i32 { ... }");
    logSemanticErrors(
        "Parameter '" + paramName + "' cannot use inferred (auto) type", param);
    return;
  }

  // Resolve full type through unified system
  ResolvedType base = inferDeclarationBaseType(param);
  ResolvedType resolvedType =
      resolveTypeWithModifier(param->modified_type.get(), base);

  // Parameters cannot be heap allocated
  if (param->isHeap) {
    errorHandler
        .addHint(
            "Parameters are caller-managed, heap allocation is invalid here")
        .addHint("Remove 'heap' from the parameter declaration")
        .addHint("If you need heap data, pass a pointer instead: ptr i32 p");
    logSemanticErrors("Parameter '" + paramName + "' cannot be heap allocated",
                      param);
    return;
  }

  // References cannot be nullable
  if (resolvedType.isRef() && resolvedType.isNull) {
    errorHandler
        .addHint("References always point to valid memory — nullable is "
                 "contradictory")
        .addHint("Remove '?' from the reference type")
        .addHint(
            "If nullable is needed, use a nullable pointer instead: ptr i32?");
    logSemanticErrors(
        "Reference parameter '" + paramName + "' cannot be nullable", param);
    return;
  }

  // persist makes no sense on a parameter
  if (param->isPersist) {
    errorHandler
        .addHint("'persist' only applies to heap variables in function bodies")
        .addHint("Remove 'persist' from the parameter declaration");
    logSemanticErrors(
        "Parameter '" + paramName + "' cannot be marked 'persist'", param);
    return;
  }

  bool isMutable = (param->mutability == Mutability::MUTABLE);

  auto info = std::make_shared<SymbolInfo>();
  info->type = resolvedType;
  info->isMutable = isMutable;
  info->isConstant = false;   // parameters are never compile time constants
  info->isInitialized = true; // parameters are always initialized as inputs
  info->isHeap = false;       // parameters never start on heap
  info->isParam = true;
  info->isDefinitelyNull = false;
  info->hasError = false;

  metaData[param] = info;
  symbolTable.back()[paramName] = info;

  logInternal("Parameter '" + paramName +
              "' type: " + resolvedType.resolvedName);
}

void Semantics::walkFunctionStatement(Node *node) {
  auto funcStmt = dynamic_cast<FunctionStatement *>(node);
  if (!funcStmt)
    return;
  // Unwrapping whatever is stored in the function statement and walking it
  walker(funcStmt->funcExpr.get());
}

void Semantics::walkFunctionExpression(Node *node) {
  auto funcExpr = dynamic_cast<FunctionExpression *>(node);
  if (!funcExpr) {
    reportDevBug("Invalid function expression", node);
    return;
  }

  std::string funcName = funcExpr->func_key.TokenLiteral;
  bool isExportable = funcExpr->isExportable;

  // No nested functions
  if (insideFunction) {
    errorHandler.addHint("Move the inner function to the top level")
        .addHint("Unnameable does not support nested function definitions");
    logSemanticErrors("Nested function definitions are prohibited", funcExpr);
    return;
  }

  insideFunction = true;

  auto symbol = lookUpInCurrentScope(funcName);
  if (symbol) {
    if (symbol->isFunction) {
      if (symbol->isDefined) {
        errorHandler.addHint("Function names must be unique within their scope")
            .addHint("Rename one of the '" + funcName + "' functions");
        logSemanticErrors("Function '" + funcName + "' is already defined",
                          funcExpr);
        insideFunction = false;
        return;
      } else if (symbol->isDeclaration) {
        if (!areSignaturesCompatible(*symbol, funcExpr)) {
          errorHandler
              .addHint("The definition must match the earlier declaration")
              .addHint("Check parameter types and return type");
          logSemanticErrors("Function definition for '" + funcName +
                                "' does not match prior declaration",
                            funcExpr);
        }
      }
    } else {
      errorHandler
          .addHint("'" + funcName + "' is already used as a variable name")
          .addHint("Choose a different name for the function");
      logSemanticErrors("Name '" + funcName + "' already in use", funcExpr);
    }
  }

  auto funcInfo = std::make_shared<SymbolInfo>();
  funcInfo->hasError = hasError;
  funcInfo->isDeclaration = true;
  funcInfo->isDefined = false;
  funcInfo->isExportable = isExportable;
  funcInfo->isFunction = true;
  funcInfo->returnType = ResolvedType::unknown();
  funcInfo->funcname = funcName;

  bool insideMemberContext = insideComponent || insideSeal || insideAllocator;
  if (!insideMemberContext) {
    logInternal("Inserting Function '" + funcName + "' into Global Scope");
    symbolTable[0][funcName] = funcInfo;
  } else {
    logInternal("Inserting Function '" + funcName + "' into Local Scope");
    symbolTable.back()[funcName] = funcInfo;
  }

  currentFunction = funcInfo;
  logInternal("Set current function for '" + funcName + "'");

  symbolTable.push_back({});

  std::vector<std::pair<ResolvedType, std::string>> paramTypes;
  for (const auto &param : funcExpr->call) {
    if (!param)
      continue;

    auto varDecl = dynamic_cast<VariableDeclaration *>(param.get());
    if (!varDecl) {
      errorHandler
          .addHint("Parameters must follow the unified declaration syntax")
          .addHint("Example: ptr i32 x, mut i32 y, ref i32 z");
      logSemanticErrors("Invalid parameter in function '" + funcName + "'",
                        param.get());
      continue;
    }

    const std::string &paramName = varDecl->var_name->expression.TokenLiteral;

    walkFunctionParameters(param.get());

    auto paramInfo = getSymbolFromMeta(param.get());
    if (!paramInfo) {
      reportDevBug("Parameter '" + paramName + "' not analyzed", param.get());
    }

    // Exportable function using non-exportable custom type
    const std::string baseTypeName = getBaseTypeName(paramInfo->type);
    auto typeIt = customTypesTable.find(baseTypeName);
    if (typeIt != customTypesTable.end()) {
      if (isExportable && !typeIt->second->isExportable) {
        errorHandler
            .addHint("Mark the type '" + baseTypeName +
                     "' as exportable or remove it from the signature")
            .addHint("Exportable functions must only use exportable types");
        logSemanticErrors("Exportable function '" + funcName +
                              "' uses non-exportable type '" + baseTypeName +
                              "' for parameter '" + paramName + "'",
                          param.get());
        hasError = true;
      }
    }

    symbolTable.back()[paramName] = paramInfo;
    paramTypes.emplace_back(paramInfo->type, paramName);
    logInternal("Parameter '" + paramName +
                "' type: " + paramInfo->type.resolvedName);
  }

  if (!funcExpr->return_type) {
    errorHandler.addHint("All functions must declare a return type")
        .addHint("Use 'void' if the function returns nothing")
        .addHint("Example: func foo: void { ... }");
    logSemanticErrors("Function '" + funcName + "' missing return type",
                      funcExpr);
    insideFunction = false;
    return;
  }

  auto retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
  if (!retType) {
    reportDevBug("Invalid return type node for '" + funcName + "'",
                 funcExpr->return_type.get());
    insideFunction = false;
    return;
  }

  ResolvedType returnType = inferNodeDataType(retType);

  if (returnType.base().kind == DataType::UNKNOWN) {
    errorHandler
        .addHint("Use a valid return type: i32, ptr i32, arr[N] i32, void etc")
        .addHint("For custom types make sure they are declared before use");
    logSemanticErrors("Invalid return type for function '" + funcName + "'",
                      retType);
  }

  // Exportable function using non-exportable custom return type
  if (!retType->isVoid) {
    std::string retBaseName = getBaseTypeName(returnType);
    auto retTypeIt = customTypesTable.find(retBaseName);
    if (retTypeIt != customTypesTable.end()) {
      if (isExportable && !retTypeIt->second->isExportable) {
        errorHandler
            .addHint("Mark the return type '" + retBaseName +
                     "' as exportable or change the return type")
            .addHint("Exportable functions must only use exportable types");
        logSemanticErrors("Exportable function '" + funcName +
                              "' uses non-exportable return type '" +
                              retBaseName + "'",
                          retType);
      }
    }
  }

  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;
  funcInfo->type = returnType;
  funcInfo->returnType = returnType;
  funcInfo->paramTypes = paramTypes;
  funcInfo->isDefined = true;

  if (!insideMemberContext) {
    logInternal("Fully inserting '" + funcName + "' into Global Scope");
    symbolTable[0][funcName] = funcInfo;
  } else {
    logInternal("Fully inserting '" + funcName + "' into Local Scope");
    symbolTable.back()[funcName] = funcInfo;
  }

  metaData[funcExpr] = funcInfo;
  currentFunction = funcInfo;
  logInternal("Updated current function '" + funcName +
              "' return type: " + returnType.resolvedName);

  if (!funcExpr->block) {
    errorHandler.addHint("Add a function body enclosed in '{}'")
        .addHint("Example: func foo: void { ... }");
    logSemanticErrors("Function '" + funcName + "' missing body", funcExpr);
    insideFunction = false;
    return;
  }

  auto block = dynamic_cast<BlockExpression *>(funcExpr->block.get());
  if (!block) {
    reportDevBug("Invalid function body for '" + funcName + "'",
                 funcExpr->block.get());
    insideFunction = false;
    return;
  }

  logInternal("Processing block for function '" + funcName + "'");
  activeBlocks.push_back(block);

  for (const auto &stmt : block->statements) {
    std::optional<std::shared_ptr<SymbolInfo>> tempFunction = currentFunction;
    walker(stmt.get());
    currentFunction = tempFunction;
  }

  // Non-void functions must have a return path
  if (returnType.base().kind != DataType::VOID && !hasReturnPath(block)) {
    errorHandler.addHint("Add a return statement at the end of the function")
        .addHint("Every code path must return a value of type '" +
                 returnType.resolvedName + "'");
    logSemanticErrors(
        "Non-void function '" + funcName + "' missing return value", funcExpr);
    hasError = true;
  }

  popScope();
  activeBlocks.pop_back();
  funcInfo->hasError = hasError;
  hasError = true;
  insideFunction = false;
  currentFunction = std::nullopt;
  logInternal("Analysis complete for function '" + funcName + "'");
}

void Semantics::walkFunctionDeclarationExpression(Node *node) {
  auto funcDeclExpr = dynamic_cast<FunctionDeclarationExpression *>(node);
  if (!funcDeclExpr)
    return;
  // Since this is a wrapper for function declaration statement I am gonna call
  // the walker directly
  walkFunctionDeclarationStatement(funcDeclExpr->funcDeclrStmt.get());
}

void Semantics::walkFunctionDeclarationStatement(Node *node) {
  auto funcDeclrStmt = dynamic_cast<FunctionDeclaration *>(node);
  if (!funcDeclrStmt) {
    reportDevBug("Invalid function declaration statement", node);
    return;
  }

  bool hasError = false;
  std::string funcName = funcDeclrStmt->function_name->expression.TokenLiteral;
  bool isExportable = funcDeclrStmt->isExportable;

  // No nested declarations
  if (insideFunction) {
    errorHandler.addHint("Move the declaration to the top level scope")
        .addHint("Function declarations cannot be nested");
    logSemanticErrors("Nested function declarations are prohibited",
                      funcDeclrStmt);
    return;
  }

  // Duplicate check
  auto symbol = resolveSymbolInfo(funcName);
  if (symbol) {
    if (symbol->isDeclaration) {
      errorHandler
          .addHint("A declaration for '" + funcName +
                   "' already exists in this scope")
          .addHint("Remove the duplicate declaration");
      logSemanticErrors("Function '" + funcName + "' has already been declared",
                        funcDeclrStmt);
    } else if (symbol->isDefined) {
      errorHandler
          .addHint("Function '" + funcName + "' already has a definition")
          .addHint("Remove the declaration, the definition is sufficient");
      logSemanticErrors("Function '" + funcName + "' has already been defined",
                        funcDeclrStmt);
    } else {
      errorHandler.addHint("Choose a different name for this function");
      logSemanticErrors("Name '" + funcName + "' already in use",
                        funcDeclrStmt);
    }
    return;
  }

  // Build initial funcInfo
  auto funcInfo = std::make_shared<SymbolInfo>();
  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;
  funcInfo->isDeclaration = true;
  funcInfo->isFunction = true;
  funcInfo->isDefined = false;
  funcInfo->returnType = ResolvedType::unknown();

  currentFunction = funcInfo;
  symbolTable.push_back({});

  std::vector<std::pair<ResolvedType, std::string>> paramTypes;
  for (const auto &param : funcDeclrStmt->parameters) {
    if (!param)
      continue;

    auto varDecl = dynamic_cast<VariableDeclaration *>(param.get());
    if (!varDecl) {
      errorHandler.addHint("Parameters must use the unified declaration syntax")
          .addHint("Example: ptr i32 x, mut i32 y, ref i32 z");
      logSemanticErrors("Invalid parameter in declaration '" + funcName + "'",
                        param.get());
      hasError = true;
      continue;
    }

    const std::string &paramName = varDecl->var_name->expression.TokenLiteral;

    walkFunctionParameters(param.get());

    auto paramInfo = metaData.find(param.get());
    if (paramInfo == metaData.end()) {
      logSemanticErrors("Parameter '" + paramName + "' not analyzed",
                        param.get());
      hasError = true;
      continue;
    }

    // Exportable check
    std::string baseTypeName = getBaseTypeName(paramInfo->second->type);
    auto typeIt = customTypesTable.find(baseTypeName);
    if (typeIt != customTypesTable.end()) {
      if (isExportable && !typeIt->second->isExportable) {
        errorHandler
            .addHint("Mark '" + baseTypeName +
                     "' as exportable or use a different type")
            .addHint("Exportable functions must only use exportable types");
        logSemanticErrors("Exportable declaration '" + funcName +
                              "' uses non-exportable type '" + baseTypeName +
                              "' for parameter '" + paramName + "'",
                          param.get());
        hasError = true;
      }
    }

    paramTypes.emplace_back(paramInfo->second->type,
                            paramInfo->second->genericName);
  }

  auto retType = dynamic_cast<ReturnType *>(funcDeclrStmt->return_type.get());
  if (!retType) {
    errorHandler.addHint("All function declarations must have a return type")
        .addHint("Use 'void' if the function returns nothing");
    logSemanticErrors("Missing return type for declaration '" + funcName + "'",
                      funcDeclrStmt);
    symbolTable.pop_back();
    currentFunction = std::nullopt;
    return;
  }

  ResolvedType returnType = inferNodeDataType(retType);

  if (returnType.base().kind == DataType::UNKNOWN) {
    errorHandler
        .addHint("Use a valid return type: i32, ptr i32, arr[N] i32, void etc")
        .addHint("For custom types make sure they are declared before use");
    logSemanticErrors("Invalid return type for declaration '" + funcName + "'",
                      retType);
    hasError = true;
  }

  // Exportable return type check
  if (!retType->isVoid) {
    std::string retBaseName = getBaseTypeName(returnType);
    auto retTypeIt = customTypesTable.find(retBaseName);
    if (retTypeIt != customTypesTable.end()) {
      if (isExportable && !retTypeIt->second->isExportable) {
        errorHandler
            .addHint("Mark '" + retBaseName +
                     "' as exportable or change the return type")
            .addHint("Exportable functions must only use exportable types");
        logSemanticErrors("Exportable declaration '" + funcName +
                              "' uses non-exportable return type '" +
                              retBaseName + "'",
                          retType);
      }
    }
  }

  funcInfo->type = returnType;
  funcInfo->returnType = returnType;
  funcInfo->paramTypes = paramTypes;
  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;

  symbolTable.pop_back();
  currentFunction = std::nullopt;

  symbolTable.back()[funcName] = funcInfo;
  metaData[funcDeclrStmt] = funcInfo;
  hasError = false;

  logInternal("Stored declaration '" + funcName +
              "' return type: " + returnType.resolvedName);
}

void Semantics::walkFunctionCallExpression(Node *node) {
  auto funcCall = dynamic_cast<CallExpression *>(node);
  bool hasError = false;
  if (!funcCall) {
    reportDevBug("Invalid function call expression", node);
    return;
  }

  std::string callName = funcCall->function_identifier->expression.TokenLiteral;

  auto callSymbolInfo = resolveSymbolInfo(callName);

  // Check if function exists
  if (!callSymbolInfo) {
    logSemanticErrors("Function '" + callName +
                          "' has not been defined or declared anywhere ",
                      funcCall);
    hasError = true;
    return;
  }

  // Checking if the symbol retrieved is actually a function
  if (!callSymbolInfo->isFunction) {
    logSemanticErrors(
        "Error: Identifier '" + callName +
            "' shadows a function and refers to a variable, not a function.",
        funcCall);
    // halt processing, as the arguments/definition checks would be meaningless.
    return;
  }

  // Checking if the function is declaration
  if (!callSymbolInfo->isDeclaration) {
    logSemanticErrors("Function '" + callName + "' was not defined anywhere",
                      funcCall);
    hasError = true;
  }

  // Calling the walker on the arguments
  for (size_t i = 0; i < funcCall->parameters.size(); ++i) {
    const auto &arg = funcCall->parameters[i];

    walker(arg.get());
    // If the user passes a null literal we must give it context
    if (auto nullLit = dynamic_cast<NullLiteral *>(arg.get())) {
      if (i < callSymbolInfo->paramTypes.size()) {
        auto expected = callSymbolInfo->paramTypes[i].first;
        if (expected.isNull) {
          metaData[nullLit]->type = expected;
        } else {
          logSemanticErrors("Cannot pass null to non-nullable parameter",
                            arg.get());
          hasError = true;
          continue;
        }
      }
    }
  }

  // Check if call signature matches
  if (!isCallCompatible(*callSymbolInfo, funcCall)) {
    hasError = true;
  }

  // Store metaData for the call
  auto callSymbol = std::make_shared<SymbolInfo>();
  callSymbol->hasError = hasError;
  callSymbol->type = callSymbolInfo->returnType;
  callSymbol->returnType = callSymbolInfo->returnType;
  callSymbol->isNullable = callSymbolInfo->isNullable;
  callSymbol->ID = callSymbolInfo->ID;
  Node *holder = queryForLifeTimeBaton(callSymbol->ID);
  responsibilityTable[funcCall] = std::move(responsibilityTable[holder]);
  metaData[funcCall] = callSymbol;
}

void Semantics::walkTraceStatement(Node *node) {
  auto traceStmt = dynamic_cast<TraceStatement *>(node);

  if (!traceStmt)
    return;

  if (isGlobalScope()) {
    logSemanticErrors("Cannot use trace statement in global scope", traceStmt);
    return;
  }

  // Just call the walker on whatever is there
  auto innerExpr = traceStmt->expr.get();
  if (!innerExpr)
    return;

  walker(innerExpr);
  auto exprInfo = metaData[innerExpr];
  if (!exprInfo)
    return;

  if (exprInfo->isHeap) {
    transferBaton(traceStmt, exprInfo->ID);
  }

  metaData[traceStmt] = exprInfo;
}

void Semantics::walkSealCallExpression(Node *node,
                                       const std::string &sealName) {
  auto funcCall = dynamic_cast<CallExpression *>(node);
  bool hasError = false;
  if (!funcCall) {
    reportDevBug("Invalid function call expression", node);
    return;
  }

  std::string callName = funcCall->function_identifier->expression.TokenLiteral;

  auto sealIt = sealTable.find(sealName);
  if (sealIt == sealTable.end()) {
    logSemanticErrors("Seal '" + sealName + "' does not exist ", funcCall);
    return;
  }

  auto sealFnMap = sealIt->second;
  auto sealFnIt = sealFnMap.find(callName);
  if (sealFnIt == sealFnMap.end()) {
    logSemanticErrors("Function '" + callName + "' does not exist in seal '" +
                          sealName + "'",
                      funcCall);
    return;
  }

  auto callSymbolInfo = sealFnIt->second;

  // Check if function exists
  if (!callSymbolInfo) {
    logSemanticErrors(
        "Function '" + callName +
            "' has not been defined or declared anywhere in seal '" + sealName +
            "'",
        funcCall);
    hasError = true;
    return;
  }

  // Checking if the symbol retrieved is actually a function
  if (!callSymbolInfo->isFunction) {
    logSemanticErrors(
        "Error: Identifier '" + callName +
            "' shadows a function and refers to a variable, not a function.",
        funcCall);
    // halt processing, as the arguments/definition checks would be meaningless.
    return;
  }

  // Checking if the function is declared
  if (!callSymbolInfo->isDeclaration) {
    logSemanticErrors("Function '" + callName + "' was not defined anywhere",
                      funcCall);
    hasError = true;
  }

  // Calling the walker on the arguments
  for (size_t i = 0; i < funcCall->parameters.size(); ++i) {
    const auto &arg = funcCall->parameters[i];
    walker(arg.get());

    if (auto nullLit = dynamic_cast<NullLiteral *>(arg.get())) {
      if (i < callSymbolInfo->paramTypes.size()) {
        auto expected = callSymbolInfo->paramTypes[i].first;
        if (expected.isNull) {
          metaData[nullLit]->type = expected;
        } else {
          logSemanticErrors("Cannot pass null to non-nullable parameter",
                            arg.get());
          hasError = true;
          continue;
        }
      }
    }
  }

  // Check if call signature matches
  if (!isCallCompatible(*callSymbolInfo, funcCall)) {
    hasError = true;
  }

  // Store metaData for the call
  auto callSymbol = std::make_shared<SymbolInfo>();
  callSymbol->hasError = hasError;
  callSymbol->type = callSymbolInfo->returnType;
  callSymbol->returnType = callSymbolInfo->returnType;
  callSymbol->isNullable = callSymbolInfo->isNullable;

  metaData[funcCall] = callSymbol;
  std::cout << "[SEMANTIC LOG] Stored metaData for seal call to '" << callName
            << "' with return type: " << callSymbolInfo->type.resolvedName
            << "\n";
}

void Semantics::walkSealStatement(Node *node) {
  auto sealStmt = dynamic_cast<SealStatement *>(node);
  if (!sealStmt)
    return;

  bool hasError = false;

  // Extract the name
  auto sealName = sealStmt->sealName->expression.TokenLiteral;

  // Check if the name is already existant
  auto existingSym = resolveSymbolInfo(sealName);
  if (existingSym) {
    logSemanticErrors("'" + sealName + "' already exists",
                      sealStmt->sealName.get());
    hasError = true;
  }

  // Check the export flag
  bool isExportable = sealStmt->isExportable;

  std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> sealMap;

  symbolTable.push_back({});
  insideSeal = true;
  // Only authorise function statements inside the guard block
  auto blockStmt = dynamic_cast<BlockStatement *>(sealStmt->block.get());
  if (!blockStmt) {
    logSemanticErrors("Seal '" + sealName + "' has an invalid or missing body",
                      sealStmt->sealName.get());
    return;
  }

  for (const auto &stmt : blockStmt->statements) {
    auto funcStmt = dynamic_cast<FunctionStatement *>(stmt.get());
    if (!funcStmt) {
      logSemanticErrors("Only function statements can be sealed ", stmt.get());
      hasError = true;
      return;
    }

    walker(funcStmt);

    std::string name;
    if (auto fnExpr =
            dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get())) {
      name = fnExpr->func_key.TokenLiteral; // The original name
      logInternal("Original function name '" + name + "'");
      fnExpr->func_key.TokenLiteral =
          sealName + "_" + name; // Mangled name for IRGen
      fnExpr->token.TokenLiteral = sealName + "_" + name;
      fnExpr->expression.TokenLiteral = sealName + "_" + name;

      logInternal("Mangled function name '" + fnExpr->func_key.TokenLiteral +
                  "'");

      if (isExportable)
        fnExpr->isExportable = isExportable;
    } else if (auto fnDecl = dynamic_cast<FunctionDeclarationExpression *>(
                   funcStmt->funcExpr.get())) {
      logSemanticErrors("Function declarations cannot be sealed", fnDecl);
      hasError = true;
      return;
    }

    auto funcSym = resolveSymbolInfo(name);
    if (!funcSym) {
      logSemanticErrors("Failed to find function '" + name + "'", funcStmt);
      return;
    }
    if (isExportable) {
      funcSym->isExportable =
          isExportable; // Set the export symbol flag for the function to true
                        // if the seal is exportable
    }

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
