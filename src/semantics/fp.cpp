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
                              "' does not match function return type of  '" + currentFunction.value()->returnType.resolvedName + "'",
                          retStmt->return_value->expression.line, retStmt->return_value->expression.column);
        hasError = true;
    }

    // Safety guard to prevent unsafe returns for pointers
    if (valueType.isPointer)
    {
        // Identify what the pointer actually points to
        if (auto ident = dynamic_cast<Identifier *>(retStmt->return_value.get()))
        {
            auto &name = ident->identifier.TokenLiteral;
            auto sym = resolveSymbolInfo(name);
            if (sym)
            {
                if (!sym->isParam)
                {
                    // Get the storage type for the target
                    auto targetSym = sym->targetSymbol;
                    if (!targetSym)
                    {
                        logSemanticErrors(
                            "Target of pointer '" + name + "' does not exist",
                            retStmt->return_stmt.line,
                            retStmt->return_stmt.column);
                        return;
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

    if (valueType.isPointer)
    {
        // Identify what the reference actually references
        if (auto ident = dynamic_cast<Identifier *>(retStmt->return_value.get()))
        {
            auto &name = ident->identifier.TokenLiteral;
            auto sym = resolveSymbolInfo(name);
            if (sym)
            {
                if (!sym->isParam)
                {
                    // Get the storage type for the target
                    auto targetSym = sym->refereeSymbol;
                    if (!targetSym)
                    {
                        logSemanticErrors(
                            "Target of reference '" + name + "' does not exist",
                            retStmt->return_stmt.line,
                            retStmt->return_stmt.column);
                        return;
                    }
                    // If the pointee is STACK → ILLEGAL RETURN
                    if (targetSym->storage == StorageType::STACK)
                    {
                        logSemanticErrors(
                            "Illegal return of reference'" + name + "' (stack memory will be destroyed).",
                            retStmt->return_stmt.line,
                            retStmt->return_stmt.column);

                        hasError = true;
                    }
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

void Semantics::walkFunctionParameters(Node *node)
{
    if (auto param = dynamic_cast<LetStatement *>(node))
    {
        std::cout << "[SEMANTIC LOG]: Registering function parameter\n";
        auto paramType = dynamic_cast<BasicType *>(param->type.get());
        // Extract parameter name
        auto paramName = param->ident_token.TokenLiteral;

        // Ensure parameter name not already declared in this parameter scope
        auto existingLocal = lookUpInCurrentScope(paramName);
        if (existingLocal)
        {
            logSemanticErrors("Duplicate parameter name '" + paramName + "'",
                              param->ident_token.line, param->ident_token.column);
            return;
        }

        // Resolve declared type (must NOT be auto)
        if (paramType->data_token.type == TokenType::AUTO)
        {
            logSemanticErrors("Function parameter '" + paramName + "' cannot use inferred (auto) type",
                              paramType->data_token.line, paramType->data_token.column);
            return;
        }

        bool isNullable = paramType->isNullable;
        bool isMutable = (param->mutability == Mutability::MUTABLE);

        ResolvedType resolvedType = inferNodeDataType(paramType);

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

        std::cout << "PARAM DATA TYPE: " << resolvedType.resolvedName << "\n";
    }
    else if (auto param = dynamic_cast<PointerStatement *>(node))
    {
        std::cout << "[SEMANTIC LOG]: Registering pointer parameter\n";

        auto ptrName = param->name->expression.TokenLiteral;
        auto line = param->name->expression.line;
        auto col = param->name->expression.column;

        // Check for duplicate parameter name
        if (lookUpInCurrentScope(ptrName))
        {
            logSemanticErrors("Duplicate parameter name '" + ptrName + "'", line, col);
            return;
        }

        // Parameters must not infer type; they require explicit type
        if (!param->type)
        {
            logSemanticErrors("Pointer parameter '" + ptrName + "' must explicitly declare its type",
                              line, col);
            return;
        }

        // Resolve declared pointer type directly
        ResolvedType ptrType = inferNodeDataType(param);
        if (!ptrType.isPointer)
        {
            logSemanticErrors("Parameter '" + ptrName + "' is declared as a pointer but lacks pointer type",
                              line, col);
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

        std::cout << "POINTER PARAM TYPE: " << ptrType.resolvedName << "\n";
    }
    else if (auto param = dynamic_cast<ReferenceStatement *>(node))
    {
        std::cout << "[SEMANTIC LOG]: Registering reference parameter\n";

        auto refName = param->referer->expression.TokenLiteral;
        auto line = param->statement.line;
        auto col = param->statement.column;

        // Check for duplicate parameter name
        if (lookUpInCurrentScope(refName))
        {
            logSemanticErrors("Duplicate parameter name '" + refName + "'", line, col);
            return;
        }

        // Parameters MUST declare type (no inference from referenced expression)
        if (!param->type)
        {
            logSemanticErrors("Reference parameter '" + refName + "' must explicitly declare its type",
                              line, col);
            return;
        }

        // Resolve the declared type
        ResolvedType refType = inferNodeDataType(param);

        // Reference parameters are never nullable by design:
        if (refType.isNull)
        {
            logSemanticErrors("Reference parameter '" + refName + "' cannot be nullable", line, col);
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

        std::cout << "REFERENCE PARAM TYPE: " << refType.resolvedName << "\n";
    }
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

    bool isExportable = funcExpr->isExportable;

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
    funcInfo->isExportable = isExportable;
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
        if (!param)
            continue;

        auto letStmt = dynamic_cast<LetStatement *>(param.get());
        auto ptrStmt = dynamic_cast<PointerStatement *>(param.get());
        auto refStmt = dynamic_cast<ReferenceStatement *>(param.get());
        std::string paramName = "blank";
        if (!letStmt && !ptrStmt && !refStmt)
        {
            logSemanticErrors("Invalid statement in '" + funcName + "' parameters", param.get()->statement.line, param.get()->statement.column);
            hasError = true;
            continue;
        }

        if (letStmt)
        {
            paramName = letStmt->ident_token.TokenLiteral;
        }
        else if (ptrStmt)
        {
            paramName = ptrStmt->name->expression.TokenLiteral;
        }
        else if (refStmt)
        {
            paramName = refStmt->referer->expression.TokenLiteral;
        }

        walkFunctionParameters(param.get());
        auto paramInfo = metaData.find(param.get());
        if (paramInfo == metaData.end())
        {
            logSemanticErrors("Parameter '" + paramName + "' not analyzed", param.get()->statement.line, param.get()->statement.column);
            hasError = true;
            continue;
        }

        // Extract the paramInfo type
        auto paramTypeName = paramInfo->second->type.resolvedName;
        // Check the custom types table
        auto typeIt = customTypesTable.find(paramTypeName);
        if (typeIt != customTypesTable.end())
        {
            if (isExportable)
            {
                if (!typeIt->second->isExportable)
                {
                    logSemanticErrors("Exportable function '" + funcName + "' is using a non exportable type '" + paramTypeName + "' for its parameter '" + paramName + "'", param.get()->statement.line, param.get()->statement.column);
                    hasError = true;
                }
            }
        }

        symbolTable.back()[paramName] = paramInfo->second;
        paramTypes.emplace_back(paramInfo->second->type, paramName);

        std::cout << "[SEMANTIC LOG] Parameter '" << paramName
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

    if (!retType->returnExpr)
    {
        logSemanticErrors("Return type expression missing for function '" + funcName + "'", retType->expression.line, retType->expression.column);
        return;
    }

    // Getting the nullability from the return type
    bool isNullable = false;
    auto basicRet = dynamic_cast<BasicType *>(retType->returnExpr.get());
    auto arrayRet = dynamic_cast<ArrayType *>(retType->returnExpr.get());
    auto ptrRet = dynamic_cast<PointerType *>(retType->returnExpr.get());
    auto refRet = dynamic_cast<RefType *>(retType->returnExpr.get());
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
    else if (refRet)
    {
        auto basicRetPtr = dynamic_cast<BasicType *>(refRet->underLyingType.get());
        auto arrayRetPtr = dynamic_cast<ArrayType *>(refRet->underLyingType.get());
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
    if (retType->returnExpr->expression.type == TokenType::IDENTIFIER)
    {

        auto it = customTypesTable.find(customTypeName);
        if (it == customTypesTable.end())
        {
            logSemanticErrors("Type '" + customTypeName + "' does not exist",
                              retType->returnExpr->expression.line, retType->returnExpr->expression.column);

            return;
        }

        if (isExportable)
        {
            if (!it->second->isExportable)
            {
                logSemanticErrors("Exportable function '" + funcName + "' is using non exportable type '" + customTypeName + "'", retType->returnExpr->expression.line, retType->returnExpr->expression.column);
                hasError = true;
            }
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
    funcInfo->isExportable = isExportable;
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
    if (!funcExpr->block)
    {
        logSemanticErrors("Function body missing for '" + funcName + "'", funcExpr->expression.line, funcExpr->expression.column);
        return;
    }
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

    bool isExportable = funcDeclrStmt->isExportable;

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
    funcInfo->isExportable = isExportable;
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
    funcInfo->isExportable = isExportable;
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
        return;
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
    callSymbol->returnType = callSymbolInfo->returnType;
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

void Semantics::walkGuardStatement(Node *node)
{
    auto guardStmt = dynamic_cast<GuardStatement *>(node);
    if (!guardStmt)
        return;

    std::cout << "Analysing guard statement\n";

    bool hasError = false;

    // Extract the name
    auto guardName = guardStmt->guardName->expression.TokenLiteral;
    auto nameLine = guardStmt->guardName->expression.line;
    auto nameCol = guardStmt->guardName->expression.column;

    // Check if the name is already existant
    auto existingSym = resolveSymbolInfo(guardName);
    if (existingSym)
    {
        logSemanticErrors("'" + guardName + "' already exists", nameLine, nameCol);
        hasError = true;
    }

    // Check the export flag
    bool isExportable = guardStmt->isExportable;

    auto funcGuard = std::make_shared<GuardInfo>();
    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> guardMap;

    symbolTable.push_back({});
    // Only authorise function statements inside the guard block
    auto blockStmt = dynamic_cast<BlockStatement *>(guardStmt->block.get());
    for (const auto &stmt : blockStmt->statements)
    {
        auto funcStmt = dynamic_cast<FunctionStatement *>(stmt.get());
        if (!funcStmt)
        {
            logSemanticErrors("Only function statements or declarations are allowed in guard blocks", stmt->statement.line, stmt->statement.column);
            hasError = true;
            return;
        }

        walker(funcStmt);

        std::string name;
        if (auto fnExpr = dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get()))
        {
            name = fnExpr->func_key.TokenLiteral; // The original name
            std::cout << "Original func name: " << name << "\n";
            fnExpr->func_key.TokenLiteral = guardName + "_" + name; // Mangled name for IRGen
            std::cout << "Mangled func name: " << fnExpr->func_key.TokenLiteral << "\n";
            fnExpr->isExportable = isExportable;
        }
        else if (auto fnDecl = dynamic_cast<FunctionDeclarationExpression *>(funcStmt->funcExpr.get()))
        {
            if (auto fnDeclStmt = dynamic_cast<FunctionDeclaration *>(fnDecl->funcDeclrStmt.get()))
            {
                name = fnDeclStmt->function_name->expression.TokenLiteral; // The original name
                std::cout << "Original func name: " << name << "\n";
                fnDeclStmt->function_name->expression.TokenLiteral = guardName + "_" + name;
                std::cout << "Mangled func name: " << fnDeclStmt->function_name->expression.TokenLiteral << "\n";
                fnDeclStmt->isExportable = isExportable;
            }
        }

        funcGuard->funcName = name; // Store the original name for semantics

        auto funcSym = resolveSymbolInfo(name);
        if (!funcSym)
        {
            logSemanticErrors("Failed to find function '" + name + "'", funcStmt->statement.line, funcStmt->statement.column);
            return;
        }
        funcSym->isExportable = isExportable; // Set the export symbol flag for the function to true
        funcGuard->funcSym = funcSym;
        guardMap[name] = funcSym;
    }

    auto guardSym = std::make_shared<SymbolInfo>();
    guardSym->isExportable = isExportable;
    guardSym->hasError = hasError;

    metaData[guardStmt] = guardSym;
    symbolTable[0][guardName] = guardSym;
    guardTable[guardName] = guardMap;
    popScope();
}