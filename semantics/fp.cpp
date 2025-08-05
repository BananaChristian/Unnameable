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
                                  blockExpr->finalexpr.value().get());
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
                                      blockExpr->finalexpr.value().get());
                }
            }
        }
    }
}

void Semantics::walkReturnStatement(Node *node)
{
    auto retStmt = dynamic_cast<ReturnStatement *>(node);
    if (!retStmt || !currentFunction)
    {
        logSemanticErrors("Invalid return statement", retStmt);
        return;
    }
    std::cout << "[SEMANTIC LOG] Analyzing return statement\n";

    if (!retStmt->return_value && !retStmt->error_val)
    {
        if (currentFunction->returnType != DataType::VOID)
        {
            logSemanticErrors("Non-void function requires a return statement", retStmt);
        }
        return;
    }

    if (retStmt->error_val)
    {
        std::cout << "[SEMANTIC LOG] Return with error statement\n";
        auto errorStmt = dynamic_cast<ErrorStatement *>(retStmt->error_val.get());
        if (!errorStmt)
        {
            logSemanticErrors("Invalid error value in return", retStmt->error_val.get());
        }
        walker(retStmt->error_val.get());
        return;
    }

    if (!retStmt->return_value)
    {
        if (!currentFunction->isNullable)
        {
            logSemanticErrors("Non-void function requires a return value", retStmt);
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
                              node);
            return;
        }
    }
    else if (valueType == DataType::GENERIC && currentFunction->returnType == DataType::GENERIC)
    {
        if (genericName != currentFunction->returnGenericName)
        {
            logSemanticErrors("Return value generic type '" + genericName +
                                  "' does not match expected '" + currentFunction->returnGenericName + "'",
                              node);
            return;
        }
    }
    else if (!isTypeCompatible(currentFunction->returnType, valueType))
    {
        logSemanticErrors("Return value type '" + dataTypetoString(valueType) +
                              "' does not match '" + dataTypetoString(currentFunction->returnType) + "'",
                          node);
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
        logSemanticErrors("Invalid function expression", node);
        return;
    }
    std::cout << "[SEMANTIC LOG] Analyzing function expression: " << funcExpr->toString() << "\n";

    std::string funcName = funcExpr->func_key.TokenLiteral;
    SymbolInfo *symbol = resolveSymbolInfo(funcName);
    if (symbol)
    {
        if (symbol->isDefined)
        {
            logSemanticErrors("Function '" + funcName + "' already defined", funcExpr);
            return;
        }
        if (symbol->isDeclaration)
        {
            if (!areSignaturesCompatible(*symbol, funcExpr))
            {
                logSemanticErrors("Function definition for '" + funcName + "' does not match prior declaration", funcExpr);
                return;
            }
        }
    }

    //symbolTable.push_back({});

    SymbolInfo funcInfo;
    funcInfo.isNullable = funcExpr->isNullable;
    funcInfo.isDeclaration = false;
    funcInfo.isDefined = true;
    std::vector<std::pair<DataType, std::string>> paramTypes;
    funcInfo.genericParams.clear();

    // Populate generic parameters
    for (const auto &generic : funcExpr->generic_parameters)
    {
        if (generic.type != TokenType::IDENTIFIER)
        {
            logSemanticErrors("Invalid generic type: " + generic.TokenLiteral, funcExpr);
            return;
        }
        funcInfo.genericParams.push_back(generic.TokenLiteral);
    }

    // Process parameters
    for (const auto &param : funcExpr->call)
    {
        auto letStmt = dynamic_cast<LetStatement *>(param.get());
        if (!letStmt)
        {
            logSemanticErrors("Invalid parameter: expected let statement", param.get());
            continue;
        }
        // Set currentFunction before processing parameters
        currentFunction = funcInfo;
        walkFunctionParameterLetStatement(param.get());
        auto paramInfo = metaData.find(param.get());
        if (paramInfo == metaData.end())
        {
            logSemanticErrors("Parameter '" + letStmt->ident_token.TokenLiteral + "' not analyzed", param.get());
            continue;
        }
        paramTypes.emplace_back(paramInfo->second.symbolDataType, paramInfo->second.genericName);
        std::cout << "[SEMANTIC LOG] Parameter '" << letStmt->ident_token.TokenLiteral
                  << "' stored with type: " << dataTypetoString(paramInfo->second.symbolDataType) << "\n";
    }

    // Processing return type
    auto retType = dynamic_cast<ReturnTypeExpression *>(funcExpr->return_type.get());
    if (!retType)
    {
        logSemanticErrors("Unexpected function return type", funcExpr->return_type.get());
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
            logSemanticErrors("Undefined generic type in return: " + returnGenericName, retType);
            return;
        }
    }
    else if (returnType == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid return type: " + retType->expression.TokenLiteral, retType);
        return;
    }
    funcInfo.symbolDataType = returnType;
    funcInfo.genericName = returnGenericName;
    funcInfo.returnType = returnType;
    funcInfo.returnGenericName = returnGenericName;
    funcInfo.paramTypes = paramTypes;

    // Set currentFunction with complete funcInfo
    currentFunction = funcInfo;

    // Process block
    auto block = dynamic_cast<BlockExpression *>(funcExpr->block.get());
    if (!block)
    {
        logSemanticErrors("Invalid function body", funcExpr->block.get());
        return;
    }
    std::cout << "[SEMANTIC LOG] Processing function block for '" << funcName << "'\n";
    walker(block);

    if (returnType != DataType::VOID && !hasReturnPath(block))
    {
        logSemanticErrors("Non-void function '" + funcName + "' must have a return value or error", funcExpr);
    }

    symbolTable.back()[funcName] = funcInfo;
    metaData[funcExpr] = funcInfo;
    //symbolTable.pop_back();
    std::cout << "SECOND CURRENT INFO DATA TYPE :" << dataTypetoString(currentFunction->symbolDataType) << "\n";
    currentFunction = std::nullopt;
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
        return;
    std::cout << "[SEMANTIC LOG] Analyzing function declaration statement\n";
    // Getting the function name
    std::string funcName = funcDeclrStmt->function_name->expression.TokenLiteral;
    // Checking if the declaration already exists
    auto symbol = resolveSymbolInfo(funcName);
    if (symbol)
    {
        logSemanticErrors("Already used this name '" + funcName, funcDeclrStmt);
        return;
    }

    // Creating a new scope for the function parameters
    symbolTable.push_back({});

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
            logSemanticErrors("Invalid generic type '" + generic.TokenLiteral + "'", funcDeclrStmt);
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
        }
        paramTypes.emplace_back(paramInfo->second.symbolDataType, paramInfo->second.genericName);
    }

    // Processing the return type
    auto retType = dynamic_cast<ReturnTypeExpression *>(funcDeclrStmt->return_type.get());
    if (!retType)
    {
        logSemanticErrors("Unexpected function return type", funcDeclrStmt->return_type.get());
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
            logSemanticErrors("Undefined generic type in return '" + returnGenericName + "'", retType);
            return;
        }
    }
    else if (returnType == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid return type '" + retType->expression.TokenLiteral + "'", retType);
        return;
    }

    funcInfo.symbolDataType = returnType;
    funcInfo.genericName = returnGenericName;
    funcInfo.returnType = returnType;
    funcInfo.returnGenericName = returnGenericName;
    funcInfo.paramTypes = paramTypes;

    currentFunction = funcInfo;

    symbolTable.back()[funcName] = funcInfo;
    metaData[funcDeclrStmt] = funcInfo;

    symbolTable.pop_back();
    currentFunction = std::nullopt;
}