#include "semantics.hpp"
#include <algorithm>

void Semantics::walkBlockExpression(Node *node)
{
    auto blockExpr = dynamic_cast<BlockExpression *>(node);
    if (!blockExpr)
        return;
    // Analysing the statements inside the block expression
    auto &stmts = blockExpr->statements;
    for (const auto &stmt : stmts)
    {
        walker(stmt.get());
    }

    // Checking for the final expression if any
    if (blockExpr->finalexpr.has_value())
    {
        walker(blockExpr->finalexpr.value().get());
        if (currentFunction)
        {
            if (currentFunction.value()->returnType.kind == DataType::VOID)
            {
                logSemanticErrors("Void function cannot have a final expression",
                                  blockExpr->finalexpr.value().get()->expression.line, blockExpr->finalexpr.value().get()->expression.column);
            }
            else
            {
                ResolvedType exprType = inferNodeDataType(blockExpr->finalexpr.value().get());
                if (exprType.kind != DataType::ERROR &&
                    !isTypeCompatible(currentFunction.value()->returnType, exprType) &&
                    !(dynamic_cast<NullLiteral *>(blockExpr->finalexpr.value().get()) &&
                      currentFunction.value()->isNullable))
                {
                    logSemanticErrors("Final expression type " + exprType.resolvedName +
                                          " does not match function return type " +
                                          currentFunction.value()->returnType.resolvedName,
                                      blockExpr->finalexpr.value().get()->expression.line, blockExpr->finalexpr.value().get()->expression.column);
                }
            }
        }
    }
}

void Semantics::walkReturnStatement(Node *node)
{
    auto retStmt = dynamic_cast<ReturnStatement *>(node);
    bool hasError = false;
    if (!retStmt)
    {
        logSemanticErrors("Invalid return statement node", retStmt->return_stmt.line, retStmt->return_stmt.column);
        return;
    }
    if (!currentFunction)
    {
        logSemanticErrors("Invalid return statement as not in the current function", retStmt->return_stmt.line, retStmt->return_stmt.column);
        hasError = true;
    }
    std::cout << "[SEMANTIC LOG] Analyzing return statement\n";

    if (!retStmt->return_value && !retStmt->error_val)
    {
        if (currentFunction.value()->returnType.kind != DataType::VOID)
        {
            logSemanticErrors("Non-void function requires a return statement", retStmt->return_stmt.line, retStmt->return_stmt.column);
            hasError = true;
        }
        return;
    }

    if (retStmt->error_val)
    {
        std::cout << "[SEMANTIC LOG] Return with error statement\n";
        auto errorStmt = dynamic_cast<ErrorStatement *>(retStmt->error_val.get());
        if (!errorStmt)
        {
            logSemanticErrors("Invalid error value in return", retStmt->error_val.get()->statement.line, retStmt->error_val.get()->statement.column);
            hasError = true;
        }
        walker(retStmt->error_val.get());
    }

    if (!retStmt->return_value)
    {
        if (currentFunction.value()->returnType.kind != DataType::VOID)
        {
            logSemanticErrors("Non-void function requires a return value", retStmt->return_stmt.line, retStmt->return_stmt.column);
            hasError = true;
        }
        return;
    }

    if (retStmt->return_value)
    {
        walker(retStmt->return_value.get());
    }

    ResolvedType valueType = ResolvedType{DataType::UNKNOWN, "unknown"};
    std::string genericName;
    bool isValueNullable = false;
    if (auto ident = dynamic_cast<Identifier *>(retStmt->return_value.get()))
    {
        auto paramInfo = std::find_if(metaData.begin(), metaData.end(),
                                      [&](const auto &pair)
                                      {
                                          if (auto letStmt = dynamic_cast<LetStatement *>(pair.first))
                                          {
                                              return letStmt->ident_token.TokenLiteral == ident->identifier.TokenLiteral;
                                          }
                                          return false;
                                      });
        if (paramInfo != metaData.end())
        {
            valueType = paramInfo->second->type;
            genericName = paramInfo->second->genericName;
            isValueNullable = paramInfo->second->isNullable;
            std::cout << "[SEMANTIC LOG] Found parameter '" << ident->identifier.TokenLiteral
                      << "' with type: " << valueType.resolvedName << "\n";
        }
        else
        {
            auto symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
            if (symbol)
            {
                valueType = symbol->type;
                genericName = symbol->genericName;
                isValueNullable = symbol->isNullable;
                std::cout << "[SEMANTIC LOG] Found symbol '" << ident->identifier.TokenLiteral
                          << "' with type: " << valueType.resolvedName << "\n";
            }
            else
            {
                std::cout << "[SEMANTIC LOG] No symbol found for '" << ident->identifier.TokenLiteral << "'\n";
            }
        }
    }

    if (valueType.kind == DataType::UNKNOWN)
    {
        valueType = inferNodeDataType(retStmt->return_value.get());
        std::cout << "[SEMANTIC LOG] Inferred type for return value: " << valueType.resolvedName << "\n";
    }

    if (auto nullLit = dynamic_cast<NullLiteral *>(retStmt->return_value.get()))
    {
        if (!currentFunction.value()->isNullable)
        {
            logSemanticErrors("Cannot return 'null' for non-nullable type '" +
                                  currentFunction.value()->returnType.resolvedName + "'",
                              node->token.line, node->token.column);
            hasError = true;
        }
    }
    else if (!isTypeCompatible(currentFunction.value()->returnType, valueType))
    {
        logSemanticErrors("Return value type '" + valueType.resolvedName +
                              "' does not match '" + currentFunction.value()->returnType.resolvedName + "'",
                          node->token.line, node->token.column);
        hasError = true;
    }

    // Safety guard to prevent unsafe returns
    if (valueType.isPointer)
    {
        // Identify what the pointer actually points to
        if (auto ident = dynamic_cast<Identifier *>(retStmt->return_value.get()))
        {
            auto &name = ident->identifier.TokenLiteral;
            auto sym = resolveSymbolInfo(name);
            if (sym)
            {
                // Get the storage type for the target
                auto targetSym = sym->targetSymbol;
                if (!targetSym)
                {
                    logSemanticErrors(
                        "Target of pointer '" + name + "' does not exist",
                        retStmt->return_stmt.line,
                        retStmt->return_stmt.column);
                }
                // If the pointee is STACK → ILLEGAL RETURN
                if (targetSym->storage == StorageType::STACK)
                {
                    logSemanticErrors(
                        "Illegal return of pointer '" + name + "' (stack memory will be destroyed).",
                        retStmt->return_stmt.line,
                        retStmt->return_stmt.column);

                    hasError = true;
                }
            }
        }

        // If &local_val literal case: e.g., return &x;
        if (auto addr = dynamic_cast<AddressExpression *>(retStmt->return_value.get()))
        {
            if (auto targetIdent = dynamic_cast<Identifier *>(addr->identifier.get()))
            {
                auto &varName = targetIdent->identifier.TokenLiteral;
                auto sym = resolveSymbolInfo(varName);
                if (sym && sym->storage == StorageType::STACK)
                {
                    logSemanticErrors(
                        "Illegal return of address of local variable '" + varName + "'",
                        retStmt->return_stmt.line,
                        retStmt->return_stmt.column);

                    hasError = true;
                }
            }
        }
    }

    // Checking if the return type is nullable if it is not we block return of an error
    if (currentFunction.value()->returnType.isNull)
    {
        if (!retStmt->error_val)
        {
            logSemanticErrors("Must return an error fallback for nullable functions", retStmt->return_stmt.line, retStmt->return_stmt.column);
            hasError = true;
        }
        else
        {
            // Check if the error expression type matches the function return type(concrete)
            auto errIt = metaData.find(retStmt->error_val.get());
            if (errIt == metaData.end())
            {
                std::cout << "Failed to find error statement metaData\n";
                return;
            }
            // Get the error type
            auto errSym = errIt->second;
            if (!errSym)
            {
                std::cout << "Could not find error symbolInfo\n";
                return;
            }

            auto errType = errSym->type;
            // Check if the error type matched the concrete return type
            if (errType.kind != currentFunction.value()->returnType.kind)
            {
                logSemanticErrors("Type mismatch error of type '" + errType.resolvedName + "' does not match function return type of '" + currentFunction.value()->returnType.resolvedName + "'", retStmt->return_stmt.line, retStmt->return_stmt.column);
                hasError = true;
            }
        }
    }
    else if (!currentFunction.value()->returnType.isNull)
    {
        if (retStmt->error_val)
        {
            logSemanticErrors("Function expects a concrete return, do not provide a fallback", retStmt->return_stmt.line, retStmt->return_stmt.column);
            hasError = true;
        }
    }
    auto info = std::make_shared<SymbolInfo>();
    info->type = valueType;
    info->hasError = hasError;
    info->genericName = genericName;
    info->isNullable = isValueNullable;

    metaData[retStmt] = info;
}

void Semantics::walkFunctionStatement(Node *node)
{
    auto funcStmt = dynamic_cast<FunctionStatement *>(node);
    if (!funcStmt)
        return;
    // Unwrapping whatever is stored in the function statement and walking it
    walker(funcStmt->funcExpr.get());
    auto it = metaData.find(funcStmt->funcExpr.get());
}

void Semantics::walkFunctionExpression(Node *node)
{
    auto funcExpr = dynamic_cast<FunctionExpression *>(node);
    // Semantic error flag
    bool hasError = false;
    if (!funcExpr)
    {
        logSemanticErrors("Invalid function expression", node->token.line, node->token.column);
        return;
    }

    std::string funcName = funcExpr->func_key.TokenLiteral;

    if (insideFunction)
    {
        logSemanticErrors("Nested function definitions are prohibited",
                          funcExpr->expression.line, funcExpr->expression.column);
        return;
    }

    insideFunction = true;

    // Checking for duplicates in the same scope
    auto symbol = lookUpInCurrentScope(funcName);

    // Only accessing symbol if it exists
    if (symbol)
    {
        if (symbol->isDefined)
        {
            logSemanticErrors("Reused name '" + funcName + "'", funcExpr->func_key.line, funcExpr->func_key.column);
            insideFunction = false;
            hasError = true;
            return;
        }

        else if (symbol->isDeclaration)
        {
            if (!areSignaturesCompatible(*symbol, funcExpr))
            {
                logSemanticErrors("Function definition for '" + funcName + "' does not match prior declaration in global scope",
                                  funcExpr->func_key.line, funcExpr->func_key.column);
                hasError = true;
            }
        }
    }

    // Creating the initial funcInfo with minimal info for recursion
    auto funcInfo = std::make_shared<SymbolInfo>();
    funcInfo->hasError = hasError;
    funcInfo->isDeclaration = true; // Mark as declared early for recursion
    funcInfo->isDefined = false;
    funcInfo->isFunction = true;                                       // It is a function after all
    funcInfo->returnType = ResolvedType{DataType::UNKNOWN, "unknown"}; // Initially unknown return type

    // Inserting function symbol into current scope early for recursion
    if (!insideBehavior && !insideComponent) // If we arent inside a behavior or component we place in the global scope
    {
        std::cout << "INSERTING FUNCTION INTO GLOBAL SCOPE\n";
        symbolTable[0][funcName] = funcInfo;
    }
    else // If we are in a behavior or component insert in that scope
    {
        std::cout << "INSERTING FUNCTION INTO LOCAL SCOPE\n";
        symbolTable.back()[funcName] = funcInfo;
    }

    currentFunction = funcInfo;
    std::cout << "[SEMANTIC LOG] Set currentFunction for '" << funcName << "' with return type: "
              << funcInfo->returnType.resolvedName << "\n";

    // Pushing new scope for function parameters and body
    symbolTable.push_back({});

    // Walking parameters and storing their info
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
    for (const auto &param : funcExpr->call)
    {
        auto letStmt = dynamic_cast<LetStatement *>(param.get());
        if (!letStmt)
        {
            logSemanticErrors("Invalid parameter: expected let statement", param.get()->statement.line, param.get()->statement.column);
            hasError = true;
        }
        walkLetStatement(param.get());
        auto paramInfo = metaData.find(param.get());
        if (paramInfo == metaData.end())
        {
            logSemanticErrors("Parameter '" + letStmt->ident_token.TokenLiteral + "' not analyzed", param.get()->statement.line, param.get()->statement.column);
            hasError = true;
        }
        symbolTable.back()[letStmt->ident_token.TokenLiteral] = paramInfo->second;
        paramTypes.emplace_back(paramInfo->second->type, paramInfo->second->genericName);
        std::cout << "[SEMANTIC LOG] Parameter '" << letStmt->ident_token.TokenLiteral
                  << "' stored with type: " << paramInfo->second->type.resolvedName << "\n";
    }

    // Processing return type expression
    if (!funcExpr->return_type)
    {
        logSemanticErrors("Function '" + funcName + "' is missing a return type", funcExpr->func_key.line, funcExpr->func_key.column);
        return; // This is a fatal error so block further analysis to prevent segfaults
    }

    auto retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
    if (!retType)
    {
        logSemanticErrors("Unexpected function return type", funcExpr->return_type.get()->expression.line, funcExpr->return_type.get()->expression.column);
        hasError = true;
    }

    // Getting the nullability from the return type
    bool isNullable = false;
    auto basicRet = dynamic_cast<BasicType *>(retType->returnExpr.get());
    auto arrayRet = dynamic_cast<ArrayType *>(retType->returnExpr.get());
    auto ptrRet = dynamic_cast<PointerType *>(retType->returnExpr.get());
    if (basicRet)
    {
        isNullable = basicRet->isNullable;
    }
    else if (arrayRet)
    {
        isNullable = arrayRet->isNullable;
    }
    else if (ptrRet)
    {
        auto basicRetPtr = dynamic_cast<BasicType *>(ptrRet->underlyingType.get());
        auto arrayRetPtr = dynamic_cast<ArrayType *>(ptrRet->underlyingType.get());
        if (basicRetPtr)
        {
            isNullable = basicRetPtr->isNullable;
        }
        else if (arrayRetPtr)
        {
            isNullable = arrayRetPtr->isNullable;
        }
    }
    ResolvedType returnType = inferNodeDataType(funcExpr->return_type.get());
    std::string customTypeName = retType->returnExpr->expression.TokenLiteral;
    if (retType->expression.type == TokenType::IDENTIFIER)
    {

        auto it = customTypesTable.find(customTypeName);
        if (it == customTypesTable.end())
        {
            logSemanticErrors("Type '" + customTypeName + "' does not exist",
                              retType->expression.line, retType->expression.column);

            return;
        }
        returnType = it->second->type;
    }
    else if (returnType.kind == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid return type '" + retType->expression.TokenLiteral + "'",
                          retType->expression.line, retType->expression.column);
        hasError = true;
    }

    // Updating funcInfo with full signature info
    funcInfo->hasError = hasError;
    funcInfo->isNullable = isNullable;
    funcInfo->type = returnType;
    funcInfo->returnType = returnType;
    funcInfo->paramTypes = paramTypes;

    // Updating the symbol table with final function info
    funcInfo->isDefined = true;

    std::cout << "Regsitering in parent scope\n";
    if (!insideBehavior && !insideComponent) // If we arent inside a behavior or component we place in the global scope
    {
        std::cout << "2ND INSERTING FUNCTION INTO GLOBAL SCOPE WITH UPDATED INFO\n";
        symbolTable[0][funcName] = funcInfo;
    }
    else // If we are in a behavior or component insert in that scope
    {
        std::cout << "2ND INSERTING FUNCTION INTO LOCAL SCOPE WITH UPDATED INFO\n";
        symbolTable.back()[funcName] = funcInfo;
    }

    metaData[funcExpr] = funcInfo;

    currentFunction = funcInfo; // updating currentFunction with final info
    std::cout << "[SEMANTIC LOG] Updated currentFunction for '" << funcName << "' with return type: "
              << funcInfo->returnType.resolvedName << "\n";

    // Process the function body block
    auto block = dynamic_cast<BlockExpression *>(funcExpr->block.get());
    if (!block)
    {
        logSemanticErrors("Invalid function body", funcExpr->block.get()->expression.line, funcExpr->block->expression.column);
        hasError = true;
    }
    std::cout << "[SEMANTIC LOG] Processing function block for '" << funcName << "'\n";

    for (const auto &stmt : block->statements)
    {
        std::optional<std::shared_ptr<SymbolInfo>> tempFunction = currentFunction; // save before statement
        walker(stmt.get());
        currentFunction = tempFunction; // restore after statement
    }

    // Check if non-void functions have return paths
    if (returnType.kind != DataType::VOID && !hasReturnPath(block))
    {
        logSemanticErrors("Non-void function '" + funcName + "' must have a return value or error", funcExpr->expression.line, funcExpr->expression.column);
        hasError = true;
    }

    // Pop function scope
    popScope();

    // Updating incase some error fall through
    funcInfo->hasError = hasError;

    // If there was an outer function context, restore it
    insideFunction = false;
    currentFunction = std::nullopt;
    std::cout << "[SEMANTIC LOG] Finished analyzing function '" << funcName << "'\n";
}

void Semantics::walkFunctionDeclarationExpression(Node *node)
{
    auto funcDeclExpr = dynamic_cast<FunctionDeclarationExpression *>(node);
    if (!funcDeclExpr)
        return;
    std::cout << "[SEMANTIC LOG] Analysing function declaration  expression\n";
    // Since this is a wrapper for function declaration statement I am gonna call the walker directly
    walkFunctionDeclarationStatement(funcDeclExpr->funcDeclrStmt.get());
}

void Semantics::walkFunctionDeclarationStatement(Node *node)
{
    auto funcDeclrStmt = dynamic_cast<FunctionDeclaration *>(node);
    bool hasError = false;
    if (!funcDeclrStmt)
    {
        logSemanticErrors("Invalid function declaration statement", node->token.line, node->token.column);
        return;
    }
    std::cout << "[SEMANTIC LOG] Analyzing function declaration statement\n";

    // Getting the function name
    std::string funcName = funcDeclrStmt->function_name->expression.TokenLiteral;

    if (insideFunction)
    {
        logSemanticErrors("Nested function declarations are prohibited", funcDeclrStmt->function_name->expression.line, funcDeclrStmt->function_name->expression.column);
        return;
    }

    // Checking if the declaration already exists
    auto symbol = resolveSymbolInfo(funcName);
    if (symbol)
    {
        logSemanticErrors("Already used the name '" + funcName + "'", funcDeclrStmt->statement.line, funcDeclrStmt->statement.column);
        if (symbol->isDeclaration)
        {
            logSemanticErrors("Function '" + funcName + " has already been declared", funcDeclrStmt->statement.line, funcDeclrStmt->statement.column);
            hasError = true;
        }

        if (symbol->isDefined)
        {
            logSemanticErrors("Function '" + funcName + "' has already been defined", funcDeclrStmt->statement.line, funcDeclrStmt->statement.column);
            hasError = true;
        }
        return;
    }

    // Constructing the function signature
    auto funcInfo = std::make_shared<SymbolInfo>();
    funcInfo->isNullable = funcDeclrStmt->isNullable;
    funcInfo->hasError = hasError;
    funcInfo->isDeclaration = true;
    funcInfo->isDefined = false;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;

    currentFunction = funcInfo;

    // Dealing with parameters
    for (const auto &param : funcDeclrStmt->parameters)
    {
        walkLetStatement(param.get());
        auto paramInfo = metaData.find(param.get());
        if (paramInfo == metaData.end())
        {
            logSemanticErrors("Parameter '" + dynamic_cast<LetStatement *>(param.get())->ident_token.TokenLiteral + "' not analyzed",
                              param.get()->statement.line, param.get()->statement.column);
            hasError = true;
        }
        paramTypes.emplace_back(paramInfo->second->type, paramInfo->second->genericName);
    }

    // Processing the return type
    auto retType = dynamic_cast<ReturnType *>(funcDeclrStmt->return_type.get());
    if (!retType)
    {
        logSemanticErrors("Unexpected function return type", funcDeclrStmt->return_type.get()->expression.line, funcDeclrStmt->return_type.get()->expression.column);
        hasError = true;
    }

    ResolvedType returnType = inferNodeDataType(retType->returnExpr.get());
    // Getting nullabitlity from the retType
    bool isNullable = false;
    if (auto basicType = dynamic_cast<BasicType *>(retType->returnExpr.get()))
    {
        isNullable = basicType->isNullable;
    }
    else if (auto arrType = dynamic_cast<ArrayType *>(retType->returnExpr.get()))
    {
        isNullable = arrType->isNullable;
    }
    else if (auto ptrRet = dynamic_cast<PointerType *>(retType->returnExpr.get()))
    {
        auto basicRetPtr = dynamic_cast<BasicType *>(ptrRet->underlyingType.get());
        auto arrayRetPtr = dynamic_cast<ArrayType *>(ptrRet->underlyingType.get());
        if (basicRetPtr)
        {
            isNullable = basicRetPtr->isNullable;
        }
        else if (arrayRetPtr)
        {
            isNullable = arrayRetPtr->isNullable;
        }
    }
    std::string customTypeName = retType->expression.TokenLiteral;

    if (retType->expression.type == TokenType::IDENTIFIER)
    {
        auto it = customTypesTable.find(customTypeName);
        if (it == customTypesTable.end())
        {
            logSemanticErrors("Type '" + customTypeName + "' does not exist",
                              retType->expression.line, retType->expression.column);
            hasError = true;
        }
        returnType = it->second->type;
    }
    else if (returnType.kind == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid return type '" + retType->expression.TokenLiteral + "'",
                          retType->expression.line, retType->expression.column);
        hasError = true;
    }

    funcInfo->isNullable = isNullable;
    funcInfo->type = returnType;
    funcInfo->hasError = hasError;
    funcInfo->returnType = returnType;
    funcInfo->paramTypes = paramTypes;

    currentFunction = funcInfo;

    // Store in current scope
    symbolTable.back()[funcName] = funcInfo;
    metaData[funcDeclrStmt] = funcInfo;

    std::cout << "[SEMANTIC LOG] Stored function declaration for '" << funcName << "' with return type: "
              << funcInfo->returnType.resolvedName << "\n";

    currentFunction = std::nullopt;
}

void Semantics::walkFunctionCallExpression(Node *node)
{
    auto funcCall = dynamic_cast<CallExpression *>(node);
    bool hasError = false;
    if (!funcCall)
    {
        logSemanticErrors("Invalid function call expression", node->token.line, node->token.column);
        return;
    }

    std::cout << "[SEMANTIC LOG] Analysing function call\n";

    std::string callName = funcCall->function_identifier->expression.TokenLiteral;
    auto callSymbolInfo = resolveSymbolInfo(callName);

    // Check if function exists
    if (!callSymbolInfo)
    {
        logSemanticErrors("Function '" + callName + "' has not been defined or declared anywhere ",
                          funcCall->expression.line, funcCall->expression.column);
        hasError = true;
    }

    // Checking if the symbol retrieved is actually a function
    if (!callSymbolInfo->isFunction)
    {
        logSemanticErrors(
            "Error: Identifier '" + callName + "' shadows a function and refers to a variable, not a function.",
            funcCall->expression.line,
            funcCall->expression.column);
        // halt processing, as the arguments/definition checks would be meaningless.
        return;
    }

    // Checking if the function is defined
    if (!callSymbolInfo->isDefined)
    {
        logSemanticErrors("Function '" + callName + "' was not defined anywhere",
                          funcCall->expression.line, funcCall->expression.column);
        hasError = true;
    }

    // Check if call signature matches
    if (!isCallCompatible(*callSymbolInfo, funcCall))
    {
        hasError = true;
    }

    // Calling the walker on the arguments
    for (const auto &arg : funcCall->parameters)
    {
        walker(arg.get());
    }

    // Store metaData for the call
    auto callSymbol = std::make_shared<SymbolInfo>();
    callSymbol->hasError = hasError;
    callSymbol->type = callSymbolInfo->returnType;
    callSymbol->isNullable = callSymbolInfo->isNullable;

    metaData[funcCall] = callSymbol;
    std::cout << "[SEMANTIC LOG] Stored metaData for call to '" << callName
              << "' with return type: " << callSymbolInfo->type.resolvedName << "\n";
}

void Semantics::walkQualifyStatement(Node *node)
{
    auto qualifyStmt = dynamic_cast<QualifyStatement *>(node);
    if (!qualifyStmt)
        return;

    auto qualifyName = qualifyStmt->expr->expression.TokenLiteral;
    auto line = qualifyStmt->expr->expression.line;
    auto col = qualifyStmt->expr->expression.column;

    // Checking if the name main has been used anywhere
    // Looking through the symbolTable
    auto mainSym = resolveSymbolInfo(qualifyName);
    if (mainSym)
    {
        logSemanticErrors("Already used '" + qualifyName + "'", line, col);
        return;
    }
}

void Semantics::walkShoutStatement(Node *node)
{
    auto shoutStmt = dynamic_cast<ShoutStatement *>(node);

    if (!shoutStmt)
        return;

    // Just call the walker on whatever is there
    walker(shoutStmt->expr.get());
}