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
            if (currentFunction->returnType == DataType::VOID)
            {
                logSemanticErrors("Void function cannot have a final expression",
                                  blockExpr->finalexpr.value().get()->expression.line, blockExpr->finalexpr.value().get()->expression.column);
            }
            else
            {
                DataType exprType = inferNodeDataType(blockExpr->finalexpr.value().get());
                if (exprType != DataType::ERROR &&
                    !isTypeCompatible(currentFunction->returnType, exprType) &&
                    !(dynamic_cast<NullLiteral *>(blockExpr->finalexpr.value().get()) &&
                      currentFunction->isNullable))
                {
                    logSemanticErrors("Final expression type " + dataTypetoString(exprType) +
                                          " does not match function return type " +
                                          dataTypetoString(currentFunction->returnType),
                                      blockExpr->finalexpr.value().get()->expression.line, blockExpr->finalexpr.value().get()->expression.column);
                }
            }
        }
    }
}

void Semantics::walkReturnStatement(Node *node)
{
    auto retStmt = dynamic_cast<ReturnStatement *>(node);
    if (!retStmt)
    {
        logSemanticErrors("Invalid return statement node", retStmt->return_stmt.line, retStmt->return_stmt.column);
        return;
    }
    if (!currentFunction)
    {
        logSemanticErrors("Invalid return statement as not in the current function", retStmt->return_stmt.line, retStmt->return_stmt.column);
        return;
    }
    std::cout << "[SEMANTIC LOG] Analyzing return statement\n";

    if (!retStmt->return_value && !retStmt->error_val)
    {
        if (currentFunction->returnType != DataType::VOID)
        {
            logSemanticErrors("Non-void function requires a return statement", retStmt->return_stmt.line, retStmt->return_stmt.column);
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
        }
        walker(retStmt->error_val.get());
        return;
    }

    if (!retStmt->return_value)
    {
        if (currentFunction->returnType != DataType::VOID)
        {
            logSemanticErrors("Non-void function requires a return value", retStmt->return_stmt.line, retStmt->return_stmt.column);
        }
        return;
    }

    DataType valueType = DataType::UNKNOWN;
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
            valueType = paramInfo->second.symbolDataType;
            genericName = paramInfo->second.genericName;
            isValueNullable = paramInfo->second.isNullable;
            std::cout << "[SEMANTIC LOG] Found parameter '" << ident->identifier.TokenLiteral
                      << "' with type: " << dataTypetoString(valueType) << "\n";
        }
        else
        {
            auto symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
            if (symbol)
            {
                valueType = symbol->symbolDataType;
                genericName = symbol->genericName;
                isValueNullable = symbol->isNullable;
                std::cout << "[SEMANTIC LOG] Found symbol '" << ident->identifier.TokenLiteral
                          << "' with type: " << dataTypetoString(valueType) << "\n";
            }
            else
            {
                std::cout << "[SEMANTIC LOG] No symbol found for '" << ident->identifier.TokenLiteral << "'\n";
            }
        }
    }

    if (valueType == DataType::UNKNOWN)
    {
        valueType = inferNodeDataType(retStmt->return_value.get());
        std::cout << "[SEMANTIC LOG] Inferred type for return value: " << dataTypetoString(valueType) << "\n";
    }

    if (auto nullLit = dynamic_cast<NullLiteral *>(retStmt->return_value.get()))
    {
        if (!currentFunction->isNullable)
        {
            logSemanticErrors("Cannot return 'null' for non-nullable type '" +
                                  dataTypetoString(currentFunction->returnType) + "'",
                              node->token.line, node->token.column);
            return;
        }
    }
    else if (valueType == DataType::GENERIC && currentFunction->returnType == DataType::GENERIC)
    {
        if (genericName != currentFunction->returnGenericName)
        {
            logSemanticErrors("Return value generic type '" + genericName +
                                  "' does not match expected '" + currentFunction->returnGenericName + "'",
                              node->token.line, node->token.column);
            return;
        }
    }
    else if (!isTypeCompatible(currentFunction->returnType, valueType))
    {
        logSemanticErrors("Return value type '" + dataTypetoString(valueType) +
                              "' does not match '" + dataTypetoString(currentFunction->returnType) + "'",
                          node->token.line, node->token.column);
        return;
    }

    metaData[retStmt] = SymbolInfo{valueType, genericName, isValueNullable};
}

void Semantics::walkFunctionStatement(Node *node)
{
    auto funcStmt = dynamic_cast<FunctionStatement *>(node);
    if (!funcStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analyzing function statement " << funcStmt->toString();
    // Unwrapping whatever is stored in the function statement and walking it
    walker(funcStmt->funcExpr.get());
}

void Semantics::walkFunctionExpression(Node *node)
{
    auto funcExpr = dynamic_cast<FunctionExpression *>(node);
    if (!funcExpr)
    {
        logSemanticErrors("Invalid function expression", node->token.line, node->token.column);
        return;
    }
    std::cout << "[SEMANTIC LOG] Analyzing function expression: " << funcExpr->toString() << "\n";

    std::string funcName = funcExpr->func_key.TokenLiteral;

    // Check for duplicates in global scope
    SymbolInfo *symbol = resolveSymbolInfo(funcName);
    if (symbol && symbol->isDefined)
    {
        logSemanticErrors("Function name '" + funcName + "' already used in global scope", funcExpr->func_key.line, funcExpr->func_key.column);
        return;
    }
    if (symbol && symbol->isDeclaration)
    {
        if (!areSignaturesCompatible(*symbol, funcExpr))
        {
            logSemanticErrors("Function definition for '" + funcName + "' does not match prior declaration in global scope", funcExpr->func_key.line, funcExpr->func_key.column);
            return;
        }
    }

    // Create the initial funcInfo with minimal info for recursion
    SymbolInfo funcInfo;
    funcInfo.isNullable = funcExpr->isNullable;
    funcInfo.isDeclaration = true; // Mark as declared early for recursion
    funcInfo.isDefined = false;
    funcInfo.returnType = DataType::UNKNOWN; // Initially unknown return type
    funcInfo.genericParams.clear();

    // Store generic params
    for (const auto &generic : funcExpr->generic_parameters)
    {
        if (generic.type != TokenType::IDENTIFIER)
        {
            logSemanticErrors("Invalid generic type: " + generic.TokenLiteral, funcExpr->func_key.line, funcExpr->func_key.column);
            return;
        }
        funcInfo.genericParams.push_back(generic.TokenLiteral);
    }

    // Inserting function symbol early for recursion
    symbolTable[0][funcName] = funcInfo;
    currentFunction = funcInfo;
    std::cout << "[SEMANTIC LOG] Set currentFunction for '" << funcName << "' with return type: "
              << dataTypetoString(funcInfo.returnType) << "\n";

    // Pushing new scope for function parameters and body
    symbolTable.push_back({});

    // Walking parameters and storing their info
    std::vector<std::pair<DataType, std::string>> paramTypes;
    for (const auto &param : funcExpr->call)
    {
        auto letStmt = dynamic_cast<LetStatement *>(param.get());
        if (!letStmt)
        {
            logSemanticErrors("Invalid parameter: expected let statement", param.get()->statement.line, param.get()->statement.column);
            symbolTable.pop_back();
            return;
        }
        walkFunctionParameterLetStatement(param.get());
        auto paramInfo = metaData.find(param.get());
        if (paramInfo == metaData.end())
        {
            logSemanticErrors("Parameter '" + letStmt->ident_token.TokenLiteral + "' not analyzed", param.get()->statement.line, param.get()->statement.column);
            symbolTable.pop_back();
            return;
        }
        symbolTable.back()[letStmt->ident_token.TokenLiteral] = paramInfo->second;
        paramTypes.emplace_back(paramInfo->second.symbolDataType, paramInfo->second.genericName);
        std::cout << "[SEMANTIC LOG] Parameter '" << letStmt->ident_token.TokenLiteral
                  << "' stored with type: " << dataTypetoString(paramInfo->second.symbolDataType) << "\n";
    }

    // Processing return type expression
    auto retType = dynamic_cast<ReturnTypeExpression *>(funcExpr->return_type.get());
    if (!retType)
    {
        logSemanticErrors("Unexpected function return type", funcExpr->return_type.get()->expression.line, funcExpr->return_type.get()->expression.column);
        symbolTable.pop_back();
        return;
    }

    DataType returnType = tokenTypeToDataType(retType->expression.type, funcExpr->isNullable);
    std::string returnGenericName;
    if (retType->expression.type == TokenType::IDENTIFIER)
    {
        returnType = DataType::GENERIC;
        returnGenericName = retType->expression.TokenLiteral;
        if (std::find(funcInfo.genericParams.begin(), funcInfo.genericParams.end(),
                      returnGenericName) == funcInfo.genericParams.end())
        {
            logSemanticErrors("Undefined generic type in return: " + returnGenericName, retType->expression.line, retType->expression.column);
            symbolTable.pop_back();
            return;
        }
    }
    else if (returnType == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid return type: " + retType->expression.TokenLiteral, retType->expression.line, retType->expression.column);
        symbolTable.pop_back();
        return;
    }

    // Updating funcInfo with full signature info
    funcInfo.symbolDataType = returnType;
    funcInfo.genericName = returnGenericName;
    funcInfo.returnType = returnType;
    funcInfo.returnGenericName = returnGenericName;
    funcInfo.paramTypes = paramTypes;

    // Updating the symbol table with final function info
    funcInfo.isDefined = true;
    symbolTable[0][funcName] = funcInfo;
    metaData[funcExpr] = funcInfo;

    currentFunction = funcInfo;  // update currentFunction with final info
    std::cout << "[SEMANTIC LOG] Updated currentFunction for '" << funcName << "' with return type: "
              << dataTypetoString(funcInfo.returnType) << "\n";

    // Process the function body block
    auto block = dynamic_cast<BlockExpression *>(funcExpr->block.get());
    if (!block)
    {
        logSemanticErrors("Invalid function body", funcExpr->block.get()->expression.line, funcExpr->block->expression.column);
        symbolTable.pop_back();
        return;
    }
    std::cout << "[SEMANTIC LOG] Processing function block for '" << funcName << "'\n";

    for (const auto &stmt : block->statements)
    {
        std::optional<SymbolInfo> tempFunction = currentFunction; // save before statement
        walker(stmt.get());
        currentFunction = tempFunction; // restore after statement
    }

    // Check if non-void functions have return paths
    if (returnType != DataType::VOID && !hasReturnPath(block))
    {
        logSemanticErrors("Non-void function '" + funcName + "' must have a return value or error", funcExpr->expression.line, funcExpr->expression.column);
        symbolTable.pop_back();
        return;
    }

    // Pop function scope
    symbolTable.pop_back();

    // If there was an outer function context, restore it
    // Assuming you track this elsewhere or null it here
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
    if (!funcDeclrStmt)
    {
        logSemanticErrors("Invalid function declaration statement", node->token.line, node->token.column);
        return;
    }
    std::cout << "[SEMANTIC LOG] Analyzing function declaration statement\n";

    // Getting the function name
    std::string funcName = funcDeclrStmt->function_name->expression.TokenLiteral;

    // Checking if the declaration already exists
    auto symbol = resolveSymbolInfo(funcName);
    if (symbol && (symbol->isDeclaration || symbol->isDefined))
    {
        logSemanticErrors("Already used this name '" + funcName + "'", funcDeclrStmt->statement.line, funcDeclrStmt->statement.column);
        return;
    }

    // Constructing the function signature
    SymbolInfo funcInfo;
    funcInfo.isNullable = funcDeclrStmt->isNullable;
    funcInfo.isDeclaration = true;
    funcInfo.isDefined = false;
    std::vector<std::pair<DataType, std::string>> paramTypes;
    funcInfo.genericParams.clear();

    // Dealing with generic parameters
    for (const auto &generic : funcDeclrStmt->genericParams)
    {
        if (generic.type != TokenType::IDENTIFIER)
        {
            logSemanticErrors("Invalid generic type '" + generic.TokenLiteral + "'", funcDeclrStmt->statement.line, funcDeclrStmt->statement.column);
            return;
        }
        funcInfo.genericParams.push_back(generic.TokenLiteral);
    }

    currentFunction = funcInfo;

    // Dealing with normal parameters
    for (const auto &param : funcDeclrStmt->parameters)
    {
        walkFunctionParameterLetStatement(param.get());
        auto paramInfo = metaData.find(param.get());
        if (paramInfo == metaData.end())
        {
            logSemanticErrors("Parameter '" + dynamic_cast<LetStatement *>(param.get())->ident_token.TokenLiteral + "' not analyzed",
                              param.get()->statement.line, param.get()->statement.column);
            return;
        }
        paramTypes.emplace_back(paramInfo->second.symbolDataType, paramInfo->second.genericName);
    }

    // Processing the return type
    auto retType = dynamic_cast<ReturnTypeExpression *>(funcDeclrStmt->return_type.get());
    if (!retType)
    {
        logSemanticErrors("Unexpected function return type", funcDeclrStmt->return_type.get()->expression.line, funcDeclrStmt->return_type.get()->expression.column);
        return;
    }

    DataType returnType = tokenTypeToDataType(retType->expression.type, funcDeclrStmt->isNullable);
    std::string returnGenericName;

    if (retType->expression.type == TokenType::IDENTIFIER)
    {
        returnType = DataType::GENERIC;
        returnGenericName = retType->expression.TokenLiteral;
        if (std::find(funcInfo.genericParams.begin(), funcInfo.genericParams.end(),
                      returnGenericName) == funcInfo.genericParams.end())
        {
            logSemanticErrors("Undefined generic type in return '" + returnGenericName + "'", retType->expression.line, retType->expression.column);
            return;
        }
    }
    else if (returnType == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid return type '" + retType->expression.TokenLiteral + "'", retType->expression.line, retType->expression.column);
        return;
    }

    funcInfo.symbolDataType = returnType;
    funcInfo.genericName = returnGenericName;
    funcInfo.returnType = returnType;
    funcInfo.returnGenericName = returnGenericName;
    funcInfo.paramTypes = paramTypes;

    currentFunction = funcInfo;

    // Store in global scope
    symbolTable[0][funcName] = funcInfo;
    metaData[funcDeclrStmt] = funcInfo;

    std::cout << "[SEMANTIC LOG] Stored function declaration for '" << funcName << "' with return type: "
              << dataTypetoString(funcInfo.returnType) << "\n";

    currentFunction = std::nullopt;
}

void Semantics::walkFunctionCallExpression(Node *node)
{
    auto funcCall = dynamic_cast<CallExpression *>(node);
    if (!funcCall)
    {
        logSemanticErrors("Invalid function call expression", node->token.line, node->token.column);
        return;
    }

    std::cout << "[SEMANTIC LOG] Analysing function call\n";

    std::string callName = funcCall->function_identifier->expression.TokenLiteral;
    SymbolInfo *callSymbolInfo = resolveSymbolInfo(callName);

    // Check if function exists
    if (!callSymbolInfo)
    {
        logSemanticErrors("Function name '" + callName + "' has not been defined or declared anywhere ",
                          funcCall->expression.line, funcCall->expression.column);
        return;
    }

    // Checking if the function is declared
    if (!callSymbolInfo->isDeclaration)
    {
        logSemanticErrors("Function '" + callName + "' was not declared anywhere",
                          funcCall->expression.line, funcCall->expression.column);
        return;
    }

    // Check if function is defined
    /*
    if (!callSymbolInfo->isDefined)
    {
        logSemanticErrors("Function '" + callName + "' was not defined anywhere",
                          funcCall->expression.line, funcCall->expression.column);
    }
    */

    // Check if call signature matches
    if (!isCallCompatible(*callSymbolInfo, funcCall))
    {
        logSemanticErrors("Function call to '" + callName + "' has incompatible arguments",
                          funcCall->expression.line, funcCall->expression.column);
        return;
    }

    // Store metaData for the call
    SymbolInfo callSymbol;
    callSymbol.symbolDataType = callSymbolInfo->returnType;
    callSymbol.isNullable = callSymbolInfo->isNullable;

    // Handle generic return type
    if (callSymbolInfo->returnType == DataType::GENERIC)
    {
        std::unordered_map<std::string, DataType> genericBindings;
        for (size_t i = 0; i < funcCall->parameters.size(); ++i)
        {
            auto &param = funcCall->parameters[i];
            const auto &expectedType = callSymbolInfo->paramTypes[i];
            DataType argType = DataType::UNKNOWN;

            if (auto nullLit = dynamic_cast<NullLiteral *>(param.get()))
            {
                if (expectedType.first == DataType::NULLABLE_INT ||
                    expectedType.first == DataType::NULLABLE_STR ||
                    expectedType.first == DataType::NULLABLE_BOOLEAN ||
                    expectedType.first == DataType::NULLABLE_FLT ||
                    expectedType.first == DataType::NULLABLE_DOUBLE ||
                    expectedType.first == DataType::NULLABLE_CHAR)
                {
                    argType = expectedType.first;
                }
            }
            else
            {
                argType = inferNodeDataType(param.get());
            }

            if (argType == DataType::UNKNOWN)
            {
                logSemanticErrors("Could not infer type for argument at position " + std::to_string(i + 1),
                                  param->expression.line, param->expression.column);
                return;
            }

            if (expectedType.first == DataType::GENERIC)
            {
                genericBindings[expectedType.second] = argType;
            }
        }

        auto it = genericBindings.find(callSymbolInfo->returnGenericName);
        if (it != genericBindings.end())
        {
            callSymbol.symbolDataType = it->second;
            callSymbol.isNullable = (callSymbolInfo->returnType == DataType::NULLABLE_INT ||
                                     callSymbolInfo->returnType == DataType::NULLABLE_STR ||
                                     callSymbolInfo->returnType == DataType::NULLABLE_BOOLEAN ||
                                     callSymbolInfo->returnType == DataType::NULLABLE_FLT ||
                                     callSymbolInfo->returnType == DataType::NULLABLE_DOUBLE ||
                                     callSymbolInfo->returnType == DataType::NULLABLE_CHAR);
        }
        else
        {
            logSemanticErrors("Could not infer generic return type for call to '" + callName + "'",
                              funcCall->expression.line, funcCall->expression.column);
            return;
        }
    }

    metaData[funcCall] = callSymbol;
    std::cout << "[SEMANTIC LOG] Stored metaData for call to '" << callName
              << "' with return type: " << dataTypetoString(callSymbol.symbolDataType) << "\n";
}