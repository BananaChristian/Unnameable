#include "semantics.hpp"
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
    }

    if (retStmt->error_val)
    {
        std::cout << "[SEMANTIC LOG] Return with error statement\n";
        // Optionally validating error_val as ErrorStatement
        auto errorStmt = dynamic_cast<ErrorStatement *>(retStmt->error_val.get());
        if (!errorStmt)
        {
            logSemanticErrors("Invalid error value in return", retStmt->error_val.get());
        }
        walker(retStmt->error_val.get());
        return; // Error is always valid
    }

    if (!retStmt->return_value)
    {
        if (!currentFunction->isNullable)
        {
            logSemanticErrors("Non-void function requires a return value", retStmt);
        }
    }

    DataType valueType = inferNodeDataType(retStmt->return_value.get());
    if (auto nullLit = dynamic_cast<NullLiteral *>(retStmt->return_value.get()))
    {
        if (!currentFunction->isNullable)
        {
            logSemanticErrors("Cannot return 'null' for non-nullable type '" +
                                  dataTypetoString(currentFunction->returnType) + "'",
                              node);
        }
    }
    else if (!isTypeCompatible(currentFunction->returnType, valueType))
    {
        logSemanticErrors("Return value type '" + dataTypetoString(valueType) + "' does not match '" + dataTypetoString(currentFunction->returnType) + "'",
                          retStmt->return_value.get());
    }
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
        return;
    std::cout << "[SEMANTIC LOG] Analyzing function expression " << funcExpr->toString();
    // Getting the function's name and checking it already used in the current scope
    std::string funcName = funcExpr->func_key.TokenLiteral;
    SymbolInfo *symbol = resolveSymbolInfo(funcName);
    if (symbol)
    {
        logSemanticErrors("Already used this name '" + funcName, funcExpr);
        return;
    }
    // Dealing with the function parameters
    // First we shall need to create a local scope for the function
    symbolTable.push_back({});
    // Building the function signature
    SymbolInfo funcInfo;
    funcInfo.isNullable = funcExpr->isNullable;
    std::vector<DataType> paramTypes;
    // Now we handle the parameters
    auto &funcParams = funcExpr->call;
    // Since it is a vector we for loop it as we analyse each let statement inside
    for (const auto &param : funcParams)
    {
        // I will call the specific walker for let statements as this is all that is allowed inside this field
        walkLetStatement(param.get());
        auto paramInfo = metaData.find(param.get());
        if (paramInfo == metaData.end())
        {
            logSemanticErrors("Parameter '" + param->statement.TokenLiteral + "' not analyzed", param.get());
            continue;
        }
        paramTypes.push_back(paramInfo->second.symbolDataType);
    }

    // After we deal with the parameters we must handle the return type of that functions
    auto retType = dynamic_cast<ReturnTypeExpression *>(funcExpr->return_type.get());
    if (!retType)
    {
        logSemanticErrors("Unexpected function return type", retType);
        return;
    }
    // Getting the return type data type with its nullability
    bool isRetNullable = funcExpr->isNullable;
    auto retDataType = tokenTypeToDataType(retType->expression.type, isRetNullable);
    if (retDataType == DataType::UNKNOWN)
    {
        logSemanticErrors("Invalid return type: " + retType->expression.TokenLiteral, retType);
        return;
    }
    funcInfo.symbolDataType = retDataType;
    funcInfo.returnType = retDataType;
    funcInfo.paramTypes = paramTypes;

    // Dealing with the block expression
    currentFunction = funcInfo;
    auto block = dynamic_cast<BlockExpression *>(funcExpr->block.get());
    walker(block);

    // Checking the return paths and skipping for void
    if (retDataType != DataType::VOID && !hasReturnPath(block))
    {
        logSemanticErrors("Non-void function '" + funcName + "' must have a return value or error", funcExpr);
    }

    symbolTable.back()[funcName] = funcInfo;
    metaData[funcExpr] = funcInfo;
    symbolTable.pop_back();
    currentFunction = std::nullopt;
}