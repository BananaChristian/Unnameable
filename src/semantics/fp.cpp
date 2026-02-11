#include "semantics.hpp"
#include <algorithm>
#include <memory>

void Semantics::walkBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr)
    return;
  // Analysing the statements inside the block expression
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
}

void Semantics::walkReturnStatement(Node *node) {
  auto retStmt = dynamic_cast<ReturnStatement *>(node);
  bool hasError = false;
  if (!retStmt) {
    reportDevBug("Invalid return statement node", node);
    return;
  }
  // Block if the return statement isnt inside a function
  if (!currentFunction) {
    logSemanticErrors("Invalid return statement as not in the current function",
                      retStmt);
    hasError = true;
  }

  // If we dont have a return value then check if the current function is void
  if (!retStmt->return_value) {
    if (currentFunction.value()->returnType.kind != DataType::VOID) {
      logSemanticErrors("Function of return type '" +
                            currentFunction.value()->returnType.resolvedName +
                            "' must return a value of type '" +
                            currentFunction.value()->returnType.resolvedName +
                            "'",
                        retStmt);
      hasError = true;
    }
    auto voidInfo = std::make_shared<SymbolInfo>();
    voidInfo->type = currentFunction.value()->returnType;
    voidInfo->hasError = hasError;

    metaData[retStmt] = voidInfo;
    return;
  }
  walker(retStmt->return_value.get());

  ResolvedType valueType = ResolvedType{DataType::UNKNOWN, "unknown"};
  bool isValueNullable = false;
  if (auto ident = dynamic_cast<Identifier *>(retStmt->return_value.get())) {
    auto paramInfo =
        std::find_if(metaData.begin(), metaData.end(), [&](const auto &pair) {
          if (auto letStmt = dynamic_cast<LetStatement *>(pair.first)) {
            return letStmt->ident_token.TokenLiteral ==
                   ident->identifier.TokenLiteral;
          }
          return false;
        });
    if (paramInfo != metaData.end()) {
      valueType = paramInfo->second->type;
      isValueNullable = paramInfo->second->isNullable;
      logInternal("Found parameter '" + ident->identifier.TokenLiteral +
                  "' with type :" + valueType.resolvedName);
    } else {
      auto symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
      if (symbol) {
        valueType = symbol->type;
        isValueNullable = symbol->isNullable;
        logInternal("Found symbol '" + ident->identifier.TokenLiteral +
                    "' with type: " + valueType.resolvedName);
      } else {
        logInternal("No symbol found for '" + ident->identifier.TokenLiteral +
                    "'");
      }
    }
  }

  if (valueType.kind == DataType::UNKNOWN) {
    valueType = inferNodeDataType(retStmt->return_value.get());
    logInternal("Infered type for return value '" + valueType.resolvedName +
                "'");
  }

  if (auto nullLit = dynamic_cast<NullLiteral *>(retStmt->return_value.get())) {
    if (!currentFunction.value()->isNullable) {
      logSemanticErrors("Cannot return 'null' for non-nullable type '" +
                            currentFunction.value()->returnType.resolvedName +
                            "'",
                        node);
      hasError = true;
    } else {
      // If the return value is a null literal give it context
      metaData[nullLit]->type = currentFunction.value()->returnType;
    }
  } else if (!isTypeCompatible(currentFunction.value()->returnType,
                               valueType)) {
    logSemanticErrors("Return value type '" + valueType.resolvedName +
                          "' does not match function return type of  '" +
                          currentFunction.value()->returnType.resolvedName +
                          "'",
                      retStmt);
    hasError = true;
  }

  auto valSym = metaData[retStmt->return_value.get()];
  if (!valSym) {
    reportDevBug("Could not find return value metadata", retStmt);
    return;
  }
  auto valName = extractIdentifierName(retStmt->return_value.get());

  // Safety guard to prevent unsafe returns for pointers
  if (valueType.isPointer) {
    // Identify what the pointer
    // If it isnt a parameter
    if (!valSym->isParam) {
      // Get the target symbol
      auto targetSym = valSym->targetSymbol;
      if (!targetSym && valueType.kind != DataType::OPAQUE) {
        logSemanticErrors("Target of pointer '" + valName + "' does not exist",
                          retStmt);
        return;
      }

      if (targetSym) {
        errorHandler.addHint(
            "The pointer '" + valName + "' of tier '" +
            storageStr(valSym->storage) + "' points to memory of type '" +
            storageStr(targetSym->storage) +
            "' that will be freed and cause a dangling pointer");
        if (!validateLatticeMove(valSym->storage, targetSym->storage)) {
          logSemanticErrors("Illegal pointer return ", retStmt);
        }
      }
    }
  }

  // Safety guard against dangling references
  if (valueType.isRef) {
    if (!valSym->isParam) {
      // Get the storage type for the target
      auto targetSym = valSym->refereeSymbol;
      if (!targetSym) {
        logSemanticErrors(
            "Target of reference '" + valName + "' does not exist", retStmt);
        return;
      }

      if (!validateLatticeMove(valSym->storage, targetSym->storage)) {
        errorHandler.addHint(
            "The reference '" + valName + "' of tier '" +
            storageStr(valSym->storage) + "' points to memory of type '" +
            storageStr(targetSym->storage) +
            "' that will be freed and cause a dangling reference");
        logSemanticErrors("Illegal reference return ", retStmt);
      }
    }
  }

  // Also give the current function the ID of whatever we are returning this is
  // to allow the baton retriever to find this return using the familyID
  currentFunction->get()->ID = valSym->ID;
  currentFunction->get()->storage = valSym->storage;
  logInternal("Return value storage policy is " + storageStr(valSym->storage));
  logInternal("Current function storage policy is " +
              storageStr(currentFunction->get()->storage));

  auto info = std::make_shared<SymbolInfo>();
  info->type = currentFunction.value()->returnType;
  info->hasError = hasError;
  info->isNullable = isValueNullable;
  info->ID = valSym->ID;

  Node *holder = queryForLifeTimeBaton(info->ID);
  responsibilityTable[retStmt] = std::move(responsibilityTable[holder]);
  metaData[retStmt] = info;
}

void Semantics::walkFunctionParameters(Node *node) {
  if (auto param = dynamic_cast<LetStatement *>(node)) {
    logInternal("Analyzing variable declaration parameter");
    auto paramTypeNode = dynamic_cast<BasicType *>(param->type.get());
    // Extract parameter name
    auto paramName = param->ident_token.TokenLiteral;

    // Ensure parameter name not already declared in this parameter scope
    auto existingLocal = lookUpInCurrentScope(paramName);
    if (existingLocal) {
      logSemanticErrors("Duplicate parameter name '" + paramName + "'", param);
      return;
    }

    // Resolve declared type (must NOT be auto)
    if (paramTypeNode->data_token.type == TokenType::AUTO) {
      logSemanticErrors("Function parameter '" + paramName +
                            "' cannot use inferred (auto) type",
                        paramTypeNode);
      return;
    }

    ResolvedType resolvedType = inferNodeDataType(paramTypeNode);

    bool isNullable = resolvedType.isNull;
    bool isMutable = (param->mutability == Mutability::MUTABLE);

    // Create symbol entry
    auto info = std::make_shared<SymbolInfo>();
    info->type = resolvedType;
    info->isNullable = isNullable;
    info->isMutable = isMutable;
    info->isConstant = false;   // Parameters are never compile-time constants
    info->isInitialized = true; // Parameters are always "initialized" as inputs
    info->isDefinitelyNull = false;
    info->isHeap = false; // Parameters never start on heap
    info->isParam = true;
    info->storage = StorageType::STACK;
    info->hasError = false;

    // Store metadata + register symbol
    metaData[param] = info;
    symbolTable.back()[paramName] = info;

    logInternal("Param Data Type: " + resolvedType.resolvedName);
  } else if (auto param = dynamic_cast<PointerStatement *>(node)) {
    logInternal("Registering pointer parameter");

    auto ptrName = param->name->expression.TokenLiteral;

    // Check for duplicate parameter name
    if (lookUpInCurrentScope(ptrName)) {
      logSemanticErrors("Duplicate parameter name '" + ptrName + "'",
                        param->name.get());
      return;
    }

    // Parameters must not infer type; they require explicit type
    if (!param->type) {
      logSemanticErrors("Pointer parameter '" + ptrName +
                            "' must explicitly declare its type",
                        param->name.get());
      return;
    }

    // Resolve declared pointer type directly
    ResolvedType ptrType = inferNodeDataType(param);
    if (!ptrType.isPointer) {
      logSemanticErrors("Parameter '" + ptrName +
                            "' is declared as a pointer but lacks pointer type",
                        param->name.get());
      return;
    }

    bool isMutable = (param->mutability == Mutability::MUTABLE);

    auto ptrInfo = std::make_shared<SymbolInfo>();
    ptrInfo->type = ptrType;
    ptrInfo->isPointer = true;
    ptrInfo->isMutable = isMutable;
    ptrInfo->isConstant = false;
    ptrInfo->isHeap = false;
    ptrInfo->isInitialized = true; // parameters start initialized
    ptrInfo->isNullable = ptrType.isNull;
    ptrInfo->isDefinitelyNull = false;
    ptrInfo->isParam = true;
    ptrInfo->storage = StorageType::STACK;
    ptrInfo->hasError = false;

    metaData[param] = ptrInfo;
    symbolTable.back()[ptrName] = ptrInfo;

    logInternal("Pointer Param Type :" + ptrType.resolvedName);
  } else if (auto param = dynamic_cast<ReferenceStatement *>(node)) {
    logInternal("Analyzing reference parameter ...");

    auto refName = param->name->expression.TokenLiteral;

    // Check for duplicate parameter name
    if (lookUpInCurrentScope(refName)) {
      logSemanticErrors("Duplicate parameter name '" + refName + "'", param);
      return;
    }

    // Parameters MUST declare type (no inference from referenced expression)
    if (!param->type) {
      logSemanticErrors("Reference parameter '" + refName +
                            "' must explicitly declare its type",
                        param);
      return;
    }

    // Resolve the declared type
    ResolvedType refType = inferNodeDataType(param);

    // Reference parameters are never nullable by design:
    if (refType.isNull) {
      logSemanticErrors(
          "Reference parameter '" + refName + "' cannot be nullable", param);
      return;
    }

    bool isMutable = (param->mutability == Mutability::MUTABLE);

    auto refInfo = std::make_shared<SymbolInfo>();
    refInfo->type = refType;
    refInfo->isRef = true;
    refInfo->isMutable = isMutable;
    refInfo->isConstant = false;
    refInfo->isInitialized = true; // parameters are always initialized
    refInfo->isNullable = false;   // references are never nullable
    refInfo->isDefinitelyNull = false;
    refInfo->isParam = true;
    refInfo->isHeap = false;
    refInfo->storage = StorageType::STACK;
    refInfo->hasError = false;

    metaData[param] = refInfo;
    symbolTable.back()[refName] = refInfo;

    logInternal("Reference param Type: " + refType.resolvedName);
  }
}

void Semantics::walkFunctionStatement(Node *node) {
  auto funcStmt = dynamic_cast<FunctionStatement *>(node);
  if (!funcStmt)
    return;
  // Unwrapping whatever is stored in the function statement and walking it
  walker(funcStmt->funcExpr.get());
  auto it = metaData.find(funcStmt->funcExpr.get());
}

void Semantics::walkFunctionExpression(Node *node) {
  auto funcExpr = dynamic_cast<FunctionExpression *>(node);
  // Semantic error flag
  bool hasError = false;

  if (!funcExpr) {
    reportDevBug("Invalid function expression", node);
    return;
  }

  std::string funcName = funcExpr->func_key.TokenLiteral;

  bool isExportable = funcExpr->isExportable;

  if (insideFunction) {
    logSemanticErrors("Nested function definitions are prohibited", funcExpr);
    return;
  }

  insideFunction = true;

  // Checking for duplicates in the same scope
  auto symbol = lookUpInCurrentScope(funcName);

  // Only accessing symbol if it exists
  if (symbol) {
    if (symbol->isFunction) {
      if (symbol->isDefined) {
        logSemanticErrors("Function name '" + funcName + "' already in use",
                          funcExpr);
        insideFunction = false;
        hasError = true;
        return;
      }

      else if (symbol->isDeclaration) {
        if (!areSignaturesCompatible(*symbol, funcExpr)) {
          logSemanticErrors(
              "Function definition for '" + funcName +
                  "' does not match prior declaration in global scope",
              funcExpr);
          hasError = true;
        }
      }
    } else {
      logSemanticErrors("Function name '" + funcName + "' already in use",
                        funcExpr);
      hasError = true;
    }
  }

  // Creating the initial funcInfo with minimal info for recursion
  auto funcInfo = std::make_shared<SymbolInfo>();
  funcInfo->hasError = hasError;
  funcInfo->isDeclaration = true; // Mark as declared early for recursion
  funcInfo->isDefined = false;
  funcInfo->isExportable = isExportable;
  funcInfo->isFunction = true; // It is a function after all
  funcInfo->returnType = ResolvedType{
      DataType::UNKNOWN, "unknown"}; // Initially unknown return type

  // Inserting function symbol into current scope early for recursion
  bool insideMemberContext = insideComponent || insideSeal || insideAllocator;
  if (!insideMemberContext) {
    logInternal("Inserting Function '" + funcName + "' into Global Scope");
    symbolTable[0][funcName] = funcInfo;
  } else // If we are in a behavior, component or whatever insert in that scope
  {
    logInternal("Inserting Function '" + funcName + "' into Local Scope");
    symbolTable.back()[funcName] = funcInfo;
  }

  currentFunction = funcInfo;
  logInternal("Set current function for '" + funcName +
              "' with returnt type '" + funcInfo->returnType.resolvedName +
              "'");

  // Pushing new scope for function parameters and body
  symbolTable.push_back({});

  // Walking parameters and storing their info
  std::vector<std::pair<ResolvedType, std::string>> paramTypes;
  for (const auto &param : funcExpr->call) {
    if (!param)
      continue;

    auto letStmt = dynamic_cast<LetStatement *>(param.get());
    auto ptrStmt = dynamic_cast<PointerStatement *>(param.get());
    auto refStmt = dynamic_cast<ReferenceStatement *>(param.get());
    std::string paramName = "blank";
    if (!letStmt && !ptrStmt && !refStmt) {
      logSemanticErrors("Invalid statement in '" + funcName + "' parameters",
                        param.get());
      hasError = true;
      continue;
    }

    if (letStmt) {
      paramName = letStmt->ident_token.TokenLiteral;
    } else if (ptrStmt) {
      paramName = ptrStmt->name->expression.TokenLiteral;
    } else if (refStmt) {
      paramName = refStmt->name->expression.TokenLiteral;
    }

    walkFunctionParameters(param.get());
    auto paramInfo = metaData.find(param.get());
    if (paramInfo == metaData.end()) {
      logSemanticErrors("Parameter '" + paramName + "' not analyzed",
                        param.get());
      hasError = true;
      continue;
    }

    // Extract the paramInfo type
    auto paramTypeName = paramInfo->second->type.resolvedName;
    // Check the custom types table
    auto typeIt = customTypesTable.find(paramTypeName);
    if (typeIt != customTypesTable.end()) {
      if (isExportable) {
        if (!typeIt->second->isExportable) {
          logSemanticErrors("Exportable function '" + funcName +
                                "' is using a non exportable type '" +
                                paramTypeName + "' for its parameter '" +
                                paramName + "'",
                            param.get());
          hasError = true;
        }
      }
    }

    symbolTable.back()[paramName] = paramInfo->second;
    paramTypes.emplace_back(paramInfo->second->type, paramName);

    logInternal("Parameter '" + paramName + "' stored with type '" +
                paramInfo->second->type.resolvedName + "'");
  }

  // Processing return type expression
  if (!funcExpr->return_type) {
    logSemanticErrors("Function '" + funcName + "' is missing a return type",
                      funcExpr);
    return; // This is a fatal error so block further analysis to prevent
            // segfaults
  }

  auto retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
  if (!retType) {
    logSemanticErrors("Unexpected function return type",
                      funcExpr->return_type.get());
    hasError = true;
  }

  if (!retType->returnExpr) {
    logSemanticErrors("Return type expression missing for function '" +
                          funcName + "'",
                      retType);
    return;
  }

  // Getting the nullability from the return type
  bool isNullable = false;
  auto basicRet = dynamic_cast<BasicType *>(retType->returnExpr.get());
  auto arrayRet = dynamic_cast<ArrayType *>(retType->returnExpr.get());
  auto ptrRet = dynamic_cast<PointerType *>(retType->returnExpr.get());
  auto refRet = dynamic_cast<RefType *>(retType->returnExpr.get());
  if (basicRet) {
    isNullable = basicRet->isNullable;
  } else if (arrayRet) {
    isNullable = arrayRet->isNullable;
  } else if (ptrRet) {
    auto basicRetPtr = dynamic_cast<BasicType *>(ptrRet->underlyingType.get());
    auto arrayRetPtr = dynamic_cast<ArrayType *>(ptrRet->underlyingType.get());
    if (basicRetPtr) {
      isNullable = basicRetPtr->isNullable;
    } else if (arrayRetPtr) {
      isNullable = arrayRetPtr->isNullable;
    }
  } else if (refRet) {
    auto basicRetPtr = dynamic_cast<BasicType *>(refRet->underLyingType.get());
    auto arrayRetPtr = dynamic_cast<ArrayType *>(refRet->underLyingType.get());
    if (basicRetPtr) {
      isNullable = basicRetPtr->isNullable;
    } else if (arrayRetPtr) {
      isNullable = arrayRetPtr->isNullable;
    }
  }
  ResolvedType returnType = inferNodeDataType(funcExpr->return_type.get());
  std::string customTypeName = retType->returnExpr->expression.TokenLiteral;
  if (retType->returnExpr->expression.type == TokenType::IDENTIFIER) {

    auto it = customTypesTable.find(customTypeName);
    if (it == customTypesTable.end()) {
      logSemanticErrors("Type '" + customTypeName + "' does not exist",
                        retType);

      return;
    }

    if (isExportable) {
      if (!it->second->isExportable) {
        logSemanticErrors("Exportable function '" + funcName +
                              "' is using non exportable type '" +
                              customTypeName + "'",
                          retType);
        hasError = true;
      }
    }
    returnType = it->second->type;
  } else if (returnType.kind == DataType::UNKNOWN) {
    logSemanticErrors("Invalid return type '" +
                          retType->expression.TokenLiteral + "'",
                      retType);
    hasError = true;
  }

  // Updating funcInfo with full signature info
  funcInfo->hasError = hasError;
  funcInfo->isNullable = isNullable;
  funcInfo->isExportable = isExportable;
  funcInfo->type = returnType;
  funcInfo->returnType = returnType;
  funcInfo->paramTypes = paramTypes;

  // Updating the symbol table with final function info
  funcInfo->isDefined = true;
  if (!insideComponent && !insideSeal &&
      !insideAllocator) // If we arent inside a behavior,component or seal we
                        // place in the global scope
  {
    logInternal("Fully inserting the function '" + funcName +
                "' into Global Scope with Updated info");
    symbolTable[0][funcName] = funcInfo;
  } else // If we are in a behavior or component insert in that scope
  {
    logInternal("Fully inserting the function '" + funcName +
                "' into Local Scope with Updated info");
    symbolTable.back()[funcName] = funcInfo;
  }

  metaData[funcExpr] = funcInfo;

  currentFunction = funcInfo; // updating currentFunction with final info
  logInternal("Updated current function for '" + funcName +
              "' with return type '" + funcInfo->returnType.resolvedName + "'");

  // Process the function body block
  if (!funcExpr->block) {
    logSemanticErrors("Function body missing for '" + funcName + "'", funcExpr);
    return;
  }
  auto block = dynamic_cast<BlockExpression *>(funcExpr->block.get());
  if (!block) {
    reportDevBug("Invalid function body for '" + funcName + "'",
                 funcExpr->block.get());
    hasError = true;
  }
  logInternal("Processing block for function '" + funcName + "'");

  for (const auto &stmt : block->statements) {
    std::optional<std::shared_ptr<SymbolInfo>> tempFunction =
        currentFunction; // save before statement
    walker(stmt.get());
    currentFunction = tempFunction; // restore after statement
  }

  // Check if non-void functions have return paths
  if (returnType.kind != DataType::VOID && !hasReturnPath(block)) {
    logSemanticErrors("Non-void function '" + funcName +
                          "' must have a return value of type '" +
                          returnType.resolvedName + "'",
                      funcExpr);
    hasError = true;
  }

  // Pop function scope
  popScope();

  // Updating incase some error fall through
  funcInfo->hasError = hasError;

  // If there was an outer function context, restore it
  insideFunction = false;
  currentFunction = std::nullopt;
  logInternal("Analysis for function '" + funcName + "'");
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
  bool hasError = false;
  if (!funcDeclrStmt) {
    reportDevBug("Invalid function declaration statement", node);
    return;
  }

  // Getting the function name
  std::string funcName = funcDeclrStmt->function_name->expression.TokenLiteral;

  bool isExportable = funcDeclrStmt->isExportable;

  if (insideFunction) {
    logSemanticErrors("Nested function declarations are prohibited",
                      funcDeclrStmt);
    return;
  }

  // Checking if the declaration already exists
  auto symbol = resolveSymbolInfo(funcName);
  if (symbol) {
    logSemanticErrors("Already used the name '" + funcName + "'",
                      funcDeclrStmt);
    if (symbol->isDeclaration) {
      logSemanticErrors("Function '" + funcName + " has already been declared",
                        funcDeclrStmt);
      hasError = true;
    }

    if (symbol->isDefined) {
      logSemanticErrors("Function '" + funcName + "' has already been defined",
                        funcDeclrStmt);
      hasError = true;
    }
    return;
  }

  // Constructing the function signature
  auto funcInfo = std::make_shared<SymbolInfo>();
  funcInfo->isNullable = funcDeclrStmt->isNullable;
  funcInfo->hasError = hasError;
  funcInfo->isExportable = isExportable;
  funcInfo->isDeclaration = true;
  funcInfo->isFunction = true;
  funcInfo->isDefined = false;
  std::vector<std::pair<ResolvedType, std::string>> paramTypes;

  currentFunction = funcInfo;

  symbolTable.push_back({});

  // Dealing with parameters
  for (const auto &param : funcDeclrStmt->parameters) {
    if (!param)
      continue;

    auto letStmt = dynamic_cast<LetStatement *>(param.get());
    auto ptrStmt = dynamic_cast<PointerStatement *>(param.get());
    auto refStmt = dynamic_cast<ReferenceStatement *>(param.get());
    std::string paramName = "blank";
    if (!letStmt && !ptrStmt && !refStmt) {
      logSemanticErrors("Invalid statement in '" + funcName + "' parameters",
                        param.get());
      hasError = true;
      continue;
    }

    if (letStmt) {
      paramName = letStmt->ident_token.TokenLiteral;
    } else if (ptrStmt) {
      paramName = ptrStmt->name->expression.TokenLiteral;
    } else if (refStmt) {
      paramName = refStmt->name->expression.TokenLiteral;
    }

    walkFunctionParameters(param.get());
    auto paramInfo = metaData.find(param.get());
    if (paramInfo == metaData.end()) {
      logSemanticErrors("Parameter '" + paramName + "' not analyzed",
                        param.get());
      hasError = true;
      continue;
    }

    // Extract the paramInfo type
    auto paramTypeName = paramInfo->second->type.resolvedName;
    // Check the custom types table
    auto typeIt = customTypesTable.find(paramTypeName);
    if (typeIt != customTypesTable.end()) {
      if (isExportable) {
        if (!typeIt->second->isExportable) {
          logSemanticErrors("Exportable function declaration'" + funcName +
                                "' is using a non exportable type '" +
                                paramTypeName + "' for its parameter '" +
                                paramName + "'",
                            param.get());
          hasError = true;
        }
      }
    }

    paramTypes.emplace_back(paramInfo->second->type,
                            paramInfo->second->genericName);
  }

  // Processing the return type
  auto retType = dynamic_cast<ReturnType *>(funcDeclrStmt->return_type.get());
  if (!retType) {
    logSemanticErrors("Unexpected function return type",
                      funcDeclrStmt->return_type.get());
    hasError = true;
  }

  ResolvedType returnType = inferNodeDataType(retType->returnExpr.get());
  // Getting nullabitlity from the retType
  bool isNullable = false;
  if (auto basicType = dynamic_cast<BasicType *>(retType->returnExpr.get())) {
    isNullable = basicType->isNullable;
  } else if (auto arrType =
                 dynamic_cast<ArrayType *>(retType->returnExpr.get())) {
    isNullable = arrType->isNullable;
  } else if (auto ptrRet =
                 dynamic_cast<PointerType *>(retType->returnExpr.get())) {
    auto basicRetPtr = dynamic_cast<BasicType *>(ptrRet->underlyingType.get());
    auto arrayRetPtr = dynamic_cast<ArrayType *>(ptrRet->underlyingType.get());
    if (basicRetPtr) {
      isNullable = basicRetPtr->isNullable;
    } else if (arrayRetPtr) {
      isNullable = arrayRetPtr->isNullable;
    }
  }
  std::string customTypeName = retType->expression.TokenLiteral;

  if (retType->expression.type == TokenType::IDENTIFIER) {
    auto it = customTypesTable.find(customTypeName);
    if (it == customTypesTable.end()) {
      logSemanticErrors("Type '" + customTypeName + "' does not exist",
                        retType);
      hasError = true;
    }
    returnType = it->second->type;
  } else if (returnType.kind == DataType::UNKNOWN) {
    logSemanticErrors("Invalid return type '" +
                          retType->expression.TokenLiteral + "'",
                      retType);
    hasError = true;
  }

  funcInfo->isNullable = isNullable;
  funcInfo->type = returnType;
  funcInfo->hasError = hasError;
  funcInfo->returnType = returnType;
  funcInfo->isExportable = isExportable;
  funcInfo->paramTypes = paramTypes;

  currentFunction = funcInfo;
  symbolTable.pop_back();

  // Store in current scope
  symbolTable.back()[funcName] = funcInfo;
  metaData[funcDeclrStmt] = funcInfo;

  logInternal("Stored function declaration for '" + funcName +
              "' with return type '" + funcInfo->returnType.resolvedName + "");

  currentFunction = std::nullopt;
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

  logInternal("The call '" + callName + "' storage policy is " +
              storageStr(callSymbolInfo->storage));

  // Store metaData for the call
  auto callSymbol = std::make_shared<SymbolInfo>();
  callSymbol->hasError = hasError;
  callSymbol->type = callSymbolInfo->returnType;
  callSymbol->returnType = callSymbolInfo->returnType;
  callSymbol->isNullable = callSymbolInfo->isNullable;
  callSymbol->storage = callSymbolInfo->storage;
  callSymbol->ID = callSymbolInfo->ID;
  Node *holder = queryForLifeTimeBaton(callSymbol->ID);
  responsibilityTable[funcCall] = std::move(responsibilityTable[holder]);
  metaData[funcCall] = callSymbol;
}

void Semantics::walkTraceStatement(Node *node) {
  auto traceStmt = dynamic_cast<TraceStatement *>(node);

  if (!traceStmt)
    return;

  // Just call the walker on whatever is there
  auto shoutExpr = traceStmt->expr.get();
  if (!shoutExpr)
    return;

  walker(shoutExpr);
  auto exprInfo = metaData[shoutExpr];
  if (!exprInfo)
    return;

  Node *holder = queryForLifeTimeBaton(exprInfo->ID);
  responsibilityTable[traceStmt] = std::move(responsibilityTable[holder]);
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
