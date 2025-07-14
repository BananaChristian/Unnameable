#include <iostream>
#include "semantics.hpp"
#include "ast.hpp"

#define CPPREST_FORCE_REBUILD

Semantics::Semantics()
{
    symbolTable.push_back({});
    registerAnalyzerFunctions();
};

// Main walker function
void Semantics::analyzer(Node *node)
{
    if (!node)
    {
        return;
    }
    std::cout << "Analyzing AST node: " << node->toString() << "\n";
    // std::cout << "Type from map: " << typeid(ReturnStatement).name() << "\n";
    std::cout << "Type at runtime: " << typeid(*node).name() << "\n";

    auto analyzerIt = analyzerFunctionsMap.find(typeid(*node));
    if (analyzerIt != analyzerFunctionsMap.end())
    {
        (this->*analyzerIt->second)(node);
    }
    else
    {
        std::cout << "Failed to find analyzer for node: " << node->toString() << "\n";
        std::cout << "Actual runtime type: " << typeid(*node).name() << "\n";
    }
}

void Semantics::preDeclareFunctions(std::vector<std::unique_ptr<Node>> &program)
{
    std::cout << "[SEMANTIC LOG]: Pre declaring all functions\n";
    if (symbolTable.empty())
    {
        symbolTable.push_back({});
    }

    for (const auto &node : program)
    {
        if (auto funcStmt = dynamic_cast<FunctionStatement *>(node.get()))
        {
            auto funcExpr = dynamic_cast<FunctionExpression *>(funcStmt->funcExpr.get());
            if (!funcExpr)
                continue;
            std::cout << "[DEBUG] Function key literal: '" << funcExpr->func_key.TokenLiteral << "'\n";

            TypeSystem returnType = analyzeReturnTypeExpression(funcExpr->return_type.get());

            std::vector<TypeSystem> paramTypes;
            for (const auto &param : funcExpr->call)
            {
                paramTypes.push_back(inferExpressionType(param.get()));
            }

            symbolTable.back()[funcExpr->func_key.TokenLiteral] = Symbol{
                .nodeName = funcExpr->func_key.TokenLiteral,
                .nodeType = returnType,
                .parameterTypes = paramTypes,
                .kind = SymbolKind::FUNCTION,
                .isMutable = false,
                .isConstant = false,
                .scopeDepth = (int)symbolTable.size() - 1,
            };

            std::cout << "[PREDECLARE]: Inserted '" << funcExpr->func_key.TokenLiteral
                      << "' into scope level " << symbolTable.size() - 1 << "\n";
        }
    }
}

// WALKING FUNCTIONS FOR DIFFERENT NODES
void Semantics::analyzeErrorStatement(Node *node)
{
    auto errStmt = dynamic_cast<ErrorStatement *>(node);
    if (!errStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing error statement\n";
    auto errExpr = errStmt->errorExpr.get();
    analyzer(errExpr);
}

void Semantics::analyzeExpressionStatement(Node *node)
{
    auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
    if (!exprStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing expression statement\n";
    analyzer(exprStmt->expression.get());
}

void Semantics::analyzeErrorExpression(Node *node)
{
    auto errExpr = dynamic_cast<ErrorExpression *>(node);
    if (!errExpr)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing error expression\n";
    auto errInfo = errExpr->err_message.get();
    analyzer(errInfo);
}

void Semantics::analyzeReturnStatement(Node *node)
{
    auto returnStmt = dynamic_cast<ReturnStatement *>(node);
    if (!returnStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing return statement node\n";
    if (returnTypeStack.empty())
    {
        logError("Return statement not inside a function", returnStmt);
        return;
    }

    auto expectedType = returnTypeStack.back();

    if (!expectedType.has_value())
    {
        logError("No return type found in stack", returnStmt);
    }
    if (!returnStmt->return_value) // no return value expression
    {
        if (expectedType.value() != TypeSystem::VOID)
        {
            logError("Return with no value in function ", returnStmt);
        }
    }
    else // has a return expression
    {
        TypeSystem actualType = inferExpressionType(returnStmt->return_value.get());
        if (actualType != expectedType.value())
        {
            logError("Type mismatch in return ", returnStmt);
        }
    }
}

void Semantics::analyzeFunctionStatement(Node *node)
{
    auto funcStmt = dynamic_cast<FunctionStatement *>(node);
    if (!funcStmt)
    {
        std::cerr << "[ERROR] Node is not a FunctionStatement!\n";
        return;
    }
    auto exprBasePtr = funcStmt->funcExpr.get(); // Expression*
    auto funcExpr = dynamic_cast<FunctionExpression *>(exprBasePtr);
    if (!funcExpr)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing function statement node: " << funcExpr->toString() << "\n";
    auto retType = funcExpr->return_type.get();
    if (!retType)
    {
        logError("Function missing return type ", funcExpr);
        return;
    }

    TypeSystem retTypeSystem = analyzeReturnTypeExpression(retType);
    returnTypeStack.push_back(retTypeSystem);

    auto &funcCall = funcExpr->call;
    std::vector<TypeSystem> paramTypes;
    TypeSystem callType;
    for (const auto &call : funcCall)
    {
        auto callNode = call.get();
        callType = inferExpressionType(callNode);
        paramTypes.push_back(callType);
        analyzer(call.get());
    }

    symbolTable.back()[funcExpr->func_key.TokenLiteral] = Symbol{
        .nodeName = funcExpr->func_key.TokenLiteral,
        .nodeType = retTypeSystem,
        .parameterTypes = paramTypes,
        .kind = SymbolKind::FUNCTION,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };
    symbolTable.push_back({});

    auto funcBlock = funcExpr->block.get();
    if (!funcBlock)
        return;
    analyzer(funcBlock);
    if (retTypeSystem == TypeSystem::VOID)
    {
        auto blockStmt = dynamic_cast<BlockStatement *>(funcBlock);
        if (blockStmt)
        {
            for (const auto &stmt : blockStmt->statements)
            {
                if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get()))
                {
                    if (retStmt->return_value) // If return has a value → error
                    {
                        logError("Void function '" + funcExpr->func_key.TokenLiteral + "' cannot return a value", retStmt);
                    }
                }
            }
        }
    }
    std::cout << "returnTypeStack size: " << returnTypeStack.size() << "\n";
    returnTypeStack.pop_back();
    symbolTable.pop_back();
}

void Semantics::analyzeFunctionExpression(Node *node)
{
    auto funcExpr = dynamic_cast<FunctionExpression *>(node);
    if (!funcExpr)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing function expression node: " << funcExpr->toString() << "\n";
    auto retType = funcExpr->return_type.get();
    if (!retType)
    {
        logError("Function missing return type ", funcExpr);
        return;
    }

    TypeSystem retTypeSystem = analyzeReturnTypeExpression(retType);
    returnTypeStack.push_back(retTypeSystem);

    auto &funcCall = funcExpr->call;
    std::vector<TypeSystem> paramTypes;
    TypeSystem callType;
    for (const auto &call : funcCall)
    {
        auto callNode = call.get();
        callType = inferExpressionType(callNode);
        paramTypes.push_back(callType);
        analyzer(call.get());
    }

    symbolTable.back()[funcExpr->func_key.TokenLiteral] = Symbol{
        .nodeName = funcExpr->func_key.TokenLiteral,
        .nodeType = retTypeSystem,
        .parameterTypes = paramTypes,
        .kind = SymbolKind::FUNCTION,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };
    symbolTable.push_back({});

    auto funcBlock = funcExpr->block.get();
    if (!funcBlock)
        return;
    analyzer(funcBlock);
    if (retTypeSystem == TypeSystem::VOID)
    {
        auto blockStmt = dynamic_cast<BlockStatement *>(funcBlock);
        if (blockStmt)
        {
            for (const auto &stmt : blockStmt->statements)
            {
                if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get()))
                {
                    if (retStmt->return_value) // If return has a value → error
                    {
                        logError("Void function '" + funcExpr->func_key.TokenLiteral + "' cannot return a value", retStmt);
                    }
                }
            }
        }
    }

    std::cout << "returnTypeStack size: " << returnTypeStack.size() << "\n";
    returnTypeStack.pop_back();
    symbolTable.pop_back();
}

void Semantics::analyzeFunctionCallExpression(Node *node)
{
    auto callExp = dynamic_cast<CallExpression *>(node);
    if (!callExp)
        return;

    std::cout << "[SEMANTIC LOGS]: Analyzing call expression " << callExp->toString() << "\n";

    auto funcIdent = dynamic_cast<Identifier *>(callExp->function_identifier.get());
    if (!funcIdent)
    {
        logError("Function call target is not an identifier", node);
        return;
    }

    std::cout << "[DEBUG] Function call target token literal: '" << funcIdent->token.TokenLiteral << "'\n";

    std::string funcName = funcIdent->token.TokenLiteral;
    auto symbol = resolveSymbol(funcName);
    if (!symbol)
    {
        logError("Function '" + funcName + "' not declared", node);
        return;
    }

    if (symbol->parameterTypes.size() != callExp->parameters.size())
    {
        logError("Mismatched number of arguments", node);
        return;
    }

    for (size_t i = 0; i < callExp->parameters.size(); ++i)
    {
        analyzer(callExp->parameters[i].get());
        auto argType = inferExpressionType(callExp->parameters[i].get());

        if (argType != symbol->parameterTypes[i])
        {
            logError("Type mismatch in argument " + std::to_string(i), callExp->parameters[i].get());
        }
    }

    annotations[callExp] = SemanticInfo{
        .nodeType = symbol->nodeType,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };
}

void Semantics::analyzeIdentifierExpression(Node *node)
{
    auto identExp = dynamic_cast<Identifier *>(node);
    if (!identExp)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing function statement node: " << identExp->toString() << "\n";
    auto identExpName = identExp->token.TokenLiteral;
    auto symbol = resolveSymbol(identExpName);

    if (!symbol)
    {
        logError("Use of undeclared identifier ", identExp);
        annotations[identExp] = SemanticInfo{
            .nodeType = TypeSystem::UNKNOWN,
            .isMutable = false,
            .isConstant = false,
            .scopeDepth = (int)symbolTable.size() - 1,
        };
        return;
    }
    auto identType = symbol->nodeType;

    annotations[identExp] = SemanticInfo{
        .nodeType = identType,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };
}

void Semantics::analyzeForStatement(Node *node)
{

    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        return;
    symbolTable.push_back({});
    std::cout << "[SEMANTIC LOG]: Analyzing for loop node " << forStmt->toString() << "\n";
    auto forInit = forStmt->initializer.get();
    if (!forInit)
        return;
    analyzer(forInit);
    auto forCond = forStmt->condition.get();
    TypeSystem forCondType;
    if (forCond)
    {
        forCondType = inferExpressionType(forCond);
        std::cout << "[SEMANTIC LOG]: For loop condition type " << TypeSystemString(forCondType) << "\n";
        if (forCondType != TypeSystem::BOOLEAN)
        {
            logError("For loop condition is not a boolean", forStmt);
        }
    }

    auto forStep = forStmt->step.get();
    if (!forStep)
        return;
    analyzer(forStep);

    auto forBlock = forStmt->body.get();
    if (!forBlock)
        return;
    analyzer(forBlock);

    annotations[forStmt] = SemanticInfo{
        .nodeType = TypeSystem::UNKNOWN,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};

    symbolTable.pop_back();
}

void Semantics::analyzeWhileStatement(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing while statement node " << whileStmt->toString() << "\n";
    auto whileCond = whileStmt->condition.get();
    TypeSystem condType;
    if (whileCond)
    {
        analyzer(whileCond);
        condType = inferExpressionType(whileCond);
        std::cout << "While condition type:" << TypeSystemString(condType) << "\n";
        if (condType != TypeSystem::BOOLEAN)
        {
            logError("While condition type must be a boolean", whileCond);
        }
    }
    // Analyzing content of the while block
    auto blockStmt = whileStmt->loop.get();
    if (blockStmt)
    {
        analyzeBlockStatements(blockStmt);
    }

    annotations[whileStmt] = SemanticInfo{
        .nodeType = condType,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeIfStatements(Node *node)
{
    auto ifNode = dynamic_cast<ifStatement *>(node);
    if (!ifNode)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing if statement" << ifNode->toString() << "\n";
    if (ifNode->condition)
    {
        analyzer(ifNode->condition.get());
        auto condType = inferExpressionType(ifNode->condition.get());
        std::cout << "Condition Type: " << TypeSystemString(condType) << "\n";
        if (condType != TypeSystem::BOOLEAN)
        {
            logError("If condition must be boolean type", ifNode->condition.get());
        }
    }
    std::cout << "Now analyzing if statement conditions\n";
    if (ifNode->if_result)
    {
        analyzeBlockStatements(ifNode->if_result.get());
    }

    if (ifNode->elseif_condition.has_value() && ifNode->elseif_condition)
    {
        std::cout << "[SEMANTIC LOG]: Analyzing else-if condition\n";
        analyzer(ifNode->elseif_condition.value().get());
        auto elseifcondType = inferExpressionType(ifNode->elseif_condition.value().get());

        if (elseifcondType != TypeSystem::BOOLEAN)
        {
            logError("If condition must be boolean type ", ifNode->elseif_condition.value().get());
        }

        if (ifNode->elseif_result.has_value() && ifNode->elseif_result)
        {
            std::cout << "[SEMANTIC LOG]: Analyzing else-if block\n";
            analyzeBlockStatements(ifNode->elseif_result.value().get());
        }
    }

    if (ifNode->else_result.has_value() && ifNode->else_result)
    {
        std::cout << "[SEMANTIC LOG]: Analyzing else block\n";
        analyzeBlockStatements(ifNode->else_result.value().get());
    }

    annotations[ifNode] = SemanticInfo{
        .nodeType = TypeSystem::BOOLEAN,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };
}

void Semantics::analyzeBlockStatements(Node *node)
{
    symbolTable.push_back({});
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
        return;
    auto &stmts = blockStmt->statements;
    for (const auto &stmt : stmts)
    {
        std::cout << "Analyzing statement in statement block: " << stmt->toString() << "\n";
        std::cout << "Analyzing statement in statement block: " << stmt->toString()
                  << " | Actual type: " << typeid(*stmt).name() << "\n";
        analyzer(stmt.get());
    }
    symbolTable.pop_back();
}

void Semantics::analyzeLetStatements(Node *node)
{
    std::cout << "[SEMANTIC LOG]: Analyzing let statement: " << dynamic_cast<LetStatement *>(node)->toString() << "\n";
    std::cout << "[DEBUG]: Current symbol table size: " << symbolTable.size() << "\n";
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;
    std::string declaredTypeStr = letStmt->data_type_token.TokenLiteral; // Getting the data type of the variable
    std::string varName = letStmt->ident_token.TokenLiteral;             // Getting the variable name

    TypeSystem varType = mapTypeStringToTypeSystem(declaredTypeStr); // Getting the type of the variable

    // Checking if the user provided a variable after using auto if not we get the error early
    if (!letStmt->value && declaredTypeStr == "auto")
    {
        logError("Cannot use 'auto' without initialization in variable ", letStmt);
    }

    // Analysing the assigned expressions value if it exists
    if (letStmt->value)
    {
        analyzer(letStmt->value.get());
        TypeSystem exprType = TypeSystem::UNKNOWN;
        auto it = annotations.find(letStmt->value.get());
        std::string customType = "";
        if (it != annotations.end())
        {
            exprType = it->second.nodeType;
            customType = it->second.customTypeName;
        }

        if (varType == TypeSystem::UNKNOWN)
        {
            // Allowing type inference based on the value of the variable if the keyword used is auto
            if (declaredTypeStr == "auto")
            {
                varType = exprType;
                if (varType == TypeSystem::UNKNOWN)
                {
                    std::cerr << "[SEMANTIC ERROR]: Type inference failed, could not infer type for unkown type for variable" << varName << "\n";
                }
            }
            else
            {
                // No 'auto' and no valid type — reject this statement
                std::cerr << "[SEMANTIC ERROR]: variable '" << varName << "' has no valid type and 'auto' keyword was not used.\n";
            }
        }
        else
        {
            if (exprType != TypeSystem::UNKNOWN && exprType != varType)
            {
                std::cerr << "[SEMANTIC ERROR]: Type mismatch: variable '" << varName << "' declared as '" << declaredTypeStr << "' but assigned value of different type\n";
            }
        }
    }

    Symbol sym{
        .nodeName = varName,
        .nodeType = varType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};

    annotations[letStmt] = SemanticInfo{
        .nodeType = varType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};

    symbolTable.back()[varName] = sym;
    std::cout << "[DEBUG] Inserted '" << varName << "' into scope 0\n";
}

void Semantics::analyzeAssignmentStatement(Node *node)
{
    std::cout << "[SEMANTIC LOG] Analyzing Assignment statement: " << dynamic_cast<AssignmentStatement *>(node)->ident_token.TokenLiteral << "\n";
    auto stmtNode = dynamic_cast<AssignmentStatement *>(node);
    if (!stmtNode)
        return;
    // Getting the datatype of x by walking the scope stack to see if it was stored somewhere
    auto identifierName = stmtNode->ident_token.TokenLiteral;
    auto identSymbol = resolveSymbol(identifierName);
    if (!identSymbol)
    {
        std::cerr << "[SEMANTIC ERROR]: Variable '" << identifierName << "' not declared.\n";
        return;
    }
    auto identType = identSymbol->nodeType;

    auto valueType = inferExpressionType(stmtNode->value.get());
    if (identType != valueType)
    {
        std::cerr << "[SEMANTIC ERROR]: Type mismatch: " << identifierName << " doesnt match " << TypeSystemString(valueType) << "\n";
        return;
    }

    annotations[stmtNode] = SemanticInfo{
        .nodeType = identType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeIntegerLiteral(Node *node)
{
    std::cout << "[SEMANTIC LOG] Analyzing IntegerLiteral: " << dynamic_cast<IntegerLiteral *>(node)->int_token.TokenLiteral << "\n";
    if (!node)
        return;

    IntegerLiteral *intNode = dynamic_cast<IntegerLiteral *>(node);
    if (!intNode)
    {
        std::cerr << "[SEMANTIC ERROR]: Failed to analyze integer node received wrong node" << "\n";
        return;
    }
    std::string intName = intNode->int_token.TokenLiteral;
    TypeSystem intType = TypeSystem::INTEGER;

    annotations[intNode] = SemanticInfo{
        .nodeType = intType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeFloatLiteral(Node *node)
{
    std::cout << "[SEMANTIC LOG] Analyzing FloatLiteral: " << dynamic_cast<FloatLiteral *>(node)->float_token.TokenLiteral << "\n";
    if (!node)
        return;
    FloatLiteral *fltNode = dynamic_cast<FloatLiteral *>(node);
    if (!fltNode)
    {
        std::cerr << "[SEMANTIC ERROR]: Failed to analyze float node recieved";
        return;
    }
    std::string fltName = fltNode->float_token.TokenLiteral;
    TypeSystem fltType = TypeSystem::FLOAT;

    annotations[fltNode] = SemanticInfo{
        .nodeType = fltType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeBlockExpression(Node *node)
{
    auto blockExpr = dynamic_cast<BlockExpression *>(node);
    // Analyze all statements inside the block
    for (auto &stmt : blockExpr->statements)
    {
        analyzer(stmt.get()); // Dispatch to statement analyzer
    }
    // If you have finalexpr (expression after statements), analyze that too
    if (blockExpr->finalexpr)
    {
        analyzer(blockExpr->finalexpr.value().get());
    }
}

void Semantics::analyzeStringLiteral(Node *node)
{
    if (!node)
        return;
    StringLiteral *strNode = dynamic_cast<StringLiteral *>(node);
    std::cout << "[SEMANTIC LOG]: Analyzing string node: " << strNode->string_token.TokenLiteral << "\n";
    if (!strNode)
    {
        std::cout << "Failed to analyze string node\n";
        return;
    }
    TypeSystem strType = TypeSystem::STRING;
    annotations[strNode] = SemanticInfo{
        .nodeType = strType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}
void Semantics::analyzeBooleanLiteral(Node *node)
{
    if (!node)
        return;
    BooleanLiteral *boolNode = dynamic_cast<BooleanLiteral *>(node);
    std::cout << "[SEMANTIC LOG]: Analyzing boolean node: " << boolNode->boolean_token.TokenLiteral << "\n";
    if (!boolNode)
    {
        std::cout << "Failed to analyze boolean node\n";
        return;
    }
    TypeSystem boolType = TypeSystem::BOOLEAN;
    annotations[boolNode] = SemanticInfo{
        .nodeType = boolType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeCharLiteral(Node *node)
{
    if (!node)
        return;
    CharLiteral *charNode = dynamic_cast<CharLiteral *>(node);
    std::cout << "[SEMANTIC LOG]: Analyzing char node: " << charNode->char_token.TokenLiteral << "\n";
    if (!charNode)
    {
        std::cout << "Failed to analyze char node\n";
        return;
    }
    TypeSystem charType = TypeSystem::CHAR;
    annotations[charNode] = SemanticInfo{
        .nodeType = charType,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

void Semantics::analyzeInfixExpression(Node *node)
{
    std::cout << "[SEMANTIC LOG]: Analyzing infix node\n";
    auto infixNode = dynamic_cast<InfixExpression *>(node);
    if (!infixNode)
        return;
    TypeSystem leftType = inferExpressionType(infixNode->left_operand.get());
    TypeSystem rightType = inferExpressionType(infixNode->right_operand.get());

    TypeSystem resultType = resultOf(infixNode->operat.type, leftType, rightType);

    if (!infixNode)
        return;
    annotations[infixNode] = SemanticInfo{
        .nodeType = resultType,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};
}

// HELPER FUNCTIONS
// Functions registers analyzer functions for different nodes
void Semantics::registerAnalyzerFunctions()
{
    analyzerFunctionsMap[typeid(LetStatement)] = &Semantics::analyzeLetStatements;
    analyzerFunctionsMap[typeid(IntegerLiteral)] = &Semantics::analyzeIntegerLiteral;
    analyzerFunctionsMap[typeid(FloatLiteral)] = &Semantics::analyzeFloatLiteral;
    analyzerFunctionsMap[typeid(StringLiteral)] = &Semantics::analyzeStringLiteral;
    analyzerFunctionsMap[typeid(CharLiteral)] = &Semantics::analyzeCharLiteral;
    analyzerFunctionsMap[typeid(BooleanLiteral)] = &Semantics::analyzeBooleanLiteral;
    analyzerFunctionsMap[typeid(InfixExpression)] = &Semantics::analyzeInfixExpression;
    analyzerFunctionsMap[typeid(PrefixExpression)] = &Semantics::analyzeInfixExpression;
    analyzerFunctionsMap[typeid(AssignmentStatement)] = &Semantics::analyzeAssignmentStatement;
    analyzerFunctionsMap[typeid(ifStatement)] = &Semantics::analyzeIfStatements;
    analyzerFunctionsMap[typeid(ForStatement)] = &Semantics::analyzeForStatement;
    analyzerFunctionsMap[typeid(WhileStatement)] = &Semantics::analyzeWhileStatement;
    analyzerFunctionsMap[typeid(BlockStatement)] = &Semantics::analyzeBlockStatements;
    analyzerFunctionsMap[typeid(Identifier)] = &Semantics::analyzeIdentifierExpression;
    analyzerFunctionsMap[typeid(FunctionStatement)] = &Semantics::analyzeFunctionStatement;
    analyzerFunctionsMap[typeid(CallExpression)] = &Semantics::analyzeFunctionCallExpression;
    analyzerFunctionsMap[typeid(ReturnStatement)] = &Semantics::analyzeReturnStatement;
    analyzerFunctionsMap[typeid(BlockExpression)] = &Semantics::analyzeBlockExpression;
    analyzerFunctionsMap[typeid(FunctionExpression)] = &Semantics::analyzeFunctionExpression;
    analyzerFunctionsMap[typeid(ErrorStatement)] = &Semantics::analyzeErrorStatement;
    analyzerFunctionsMap[typeid(ErrorExpression)] = &Semantics::analyzeErrorExpression;
    analyzerFunctionsMap[typeid(DataStatement)] = &Semantics::analyzeDataStatementBlock;
    analyzerFunctionsMap[typeid(BehaviorStatement)] = &Semantics::analyzeBehaviorStatementBlock;
    analyzerFunctionsMap[typeid(ComponentStatement)] = &Semantics::analyzeComponentStatementBlock;
    analyzerFunctionsMap[typeid(FieldAccessExpression)] = &Semantics::analyzeFieldAccessExpression;
    analyzerFunctionsMap[typeid(NewComponentExpression)] = &Semantics::analyzeNewComponentExpression;
    analyzerFunctionsMap[typeid(ExpressionStatement)] = &Semantics::analyzeExpressionStatement;
}

// Function maps the type string to the respective type system
TypeSystem Semantics::mapTypeStringToTypeSystem(const std::string &typeStr)
{
    if (typeStr == "int")
        return TypeSystem::INTEGER;
    if (typeStr == "float")
        return TypeSystem::FLOAT;
    if (typeStr == "string")
        return TypeSystem::STRING;
    if (typeStr == "char")
        return TypeSystem::CHAR;
    if (typeStr == "bool")
        return TypeSystem::BOOLEAN;
    return TypeSystem::UNKNOWN;
}

// Type inference helper function
TypeSystem Semantics::inferExpressionType(Node *node)
{
    if (!node)
        return TypeSystem::UNKNOWN;
    if (auto inLit = dynamic_cast<IntegerLiteral *>(node))
    {
        return TypeSystem::INTEGER;
    }

    if (auto fltLit = dynamic_cast<FloatLiteral *>(node))
    {
        return TypeSystem::FLOAT;
    }

    if (auto strLit = dynamic_cast<StringLiteral *>(node))
    {
        return TypeSystem::STRING;
    }

    if (auto chrLit = dynamic_cast<CharLiteral *>(node))
    {
        return TypeSystem::CHAR;
    }

    if (auto boolLit = dynamic_cast<BooleanLiteral *>(node))
    {
        return TypeSystem::BOOLEAN;
    }

    if (auto ident = dynamic_cast<Identifier *>(node))
    {
        std::string name = ident->identifier.TokenLiteral;
        for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it)
        {
            auto symIt = it->find(name);
            if (symIt != it->end())
            {
                return symIt->second.nodeType;
            }
        }
        std::cerr << "Semantic error: identifier '" << name << "' not declared\n";
        return TypeSystem::UNKNOWN;
    }

    if (auto infix = dynamic_cast<InfixExpression *>(node))
    {
        TypeSystem leftType = inferExpressionType(infix->left_operand.get());
        TypeSystem rightType = inferExpressionType(infix->right_operand.get());

        TokenType operatType = infix->operat.type;

        return resultOf(operatType, leftType, rightType);
    }

    if (auto prefix = dynamic_cast<PrefixExpression *>(node))
    {
        return resultOfUnary(prefix->operat.type, inferExpressionType(prefix->operand.get()));
    }

    if (auto funcExpr = dynamic_cast<FunctionExpression *>(node))
    {
        TypeSystem retType = analyzeReturnTypeExpression(funcExpr->return_type.get());

        if (retType == TypeSystem::VOID)
        {
            logError("Cannot assign a void function definition to a variable", funcExpr);
            return TypeSystem::VOID;
        }

        return retType;
    }

    if (auto callExpr = dynamic_cast<CallExpression *>(node))
    {
        auto ident = dynamic_cast<Identifier *>(callExpr->function_identifier.get());
        if (!ident)
        {
            std::cerr << "[SEMANTIC ERROR]: Call target is not an identifier\n";
            return TypeSystem::UNKNOWN;
        }

        std::string funcName = ident->identifier.TokenLiteral;

        // Search the symbol table for the function
        for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it)
        {
            auto symIt = it->find(funcName);
            if (symIt != it->end() && symIt->second.kind == SymbolKind::FUNCTION)
            {
                return symIt->second.nodeType; // Return function return type
            }
        }

        std::cerr << "[SEMANTIC ERROR]: Function '" << funcName << "' not declared\n";
        return TypeSystem::UNKNOWN;
    }

    return TypeSystem::UNKNOWN;
}

// Function converts the type system to a respective string
std::string Semantics::TypeSystemString(TypeSystem type)
{
    switch (type)
    {
    case TypeSystem::INTEGER:
        return "Type: INTEGER ";
    case TypeSystem::FLOAT:
        return "Type: FLOAT ";
    case TypeSystem::STRING:
        return "Type: STRING ";
    case TypeSystem::CHAR:
        return "Type: CHAR ";
    case TypeSystem::BOOLEAN:
        return "Type: BOOLEAN ";
    default:
        return "Type: UNKOWN ";
    }
}

std::optional<Symbol> Semantics::resolveSymbol(const std::string &name)
{
    for (int i = symbolTable.size() - 1; i >= 0; --i)
    {
        auto &scope = symbolTable[i];
        std::cout << "[DEBUG] Searching for '" << name << "' in scope level " << i << "\n";
        for (auto &[key, val] : scope)
        {
            std::cout << "    >> Key in scope: '" << key << "'\n";
        }
        if (scope.find(name) != scope.end())
        {
            std::cout << "[DEBUG] Found match for '" << name << "'\n";
            return scope[name];
        }
    }
    std::cout << "[DEBUG] No match for '" << name << "'\n";
    return std::nullopt;
}

TypeSystem Semantics::resultOf(TokenType operatorType, TypeSystem leftType, TypeSystem rightType)
{
    if (operatorType == TokenType::AND || operatorType == TokenType::OR)
    {
        if (leftType == TypeSystem::BOOLEAN && rightType == TypeSystem::BOOLEAN)
        {
            return TypeSystem::BOOLEAN;
        }
        else
        {
            return TypeSystem::UNKNOWN;
        }
    }
    bool isComparison = (operatorType == TokenType::GREATER_THAN || operatorType == TokenType::LESS_THAN || operatorType == TokenType::GT_OR_EQ || operatorType == TokenType::LT_OR_EQ || operatorType == TokenType::EQUALS || operatorType == TokenType::NOT_EQUALS);
    if (isComparison)
    {
        if ((leftType != rightType) && !((leftType == TypeSystem::INTEGER && rightType == TypeSystem::FLOAT) || (leftType == TypeSystem::FLOAT && rightType == TypeSystem::INTEGER)))
        {
            return TypeSystem::UNKNOWN;
        }
        return TypeSystem::BOOLEAN;
    }
    else
    {
        if (leftType == rightType)
        {
            return leftType;
        }
        if ((leftType == TypeSystem::INTEGER && rightType == TypeSystem::FLOAT) || (leftType == TypeSystem::FLOAT && rightType == TypeSystem::INTEGER))
        {
            return TypeSystem::FLOAT;
        }
    }

    return TypeSystem::UNKNOWN;
}

TypeSystem Semantics::resultOfUnary(TokenType operatorType, TypeSystem operandType)
{
    if (operatorType == TokenType::BANG)
    {
        if (operandType != TypeSystem::BOOLEAN)
        {
            std::cerr << "[SEMANTIC ERROR]: Cannot apply '!' to type "
                      << TypeSystemString(operandType) << "\n";
            return TypeSystem::UNKNOWN;
        }
        return TypeSystem::BOOLEAN;
    }

    if (operatorType == TokenType::MINUS_MINUS || operatorType == TokenType::PLUS_PLUS)
    {
        if (operandType == TypeSystem::INTEGER || operandType == TypeSystem::FLOAT)
        {
            return operandType; // Avoid repeating return logic
        }

        std::cerr << "[SEMANTIC ERROR]: Cannot apply '++' or '--' to type "
                  << TypeSystemString(operandType) << "\n";
        return TypeSystem::UNKNOWN;
    }

    std::cerr << "[SEMANTIC ERROR]: Unsupported unary operator "
              << static_cast<int>(operatorType)
              << " on type " << TypeSystemString(operandType) << "\n";
    return TypeSystem::UNKNOWN;
}

TypeSystem Semantics::analyzeReturnTypeExpression(Node *node)
{
    auto retExpr = dynamic_cast<ReturnTypeExpression *>(node);
    if (!retExpr)
    {
        return TypeSystem::INVALID;
    }

    TokenType retType = retExpr->typeToken.type;
    if (retType == TokenType::INT)
    {
        return TypeSystem::INTEGER;
    }

    if (retType == TokenType::FLOAT_KEYWORD)
    {
        return TypeSystem::FLOAT;
    }

    if (retType == TokenType::STRING_KEYWORD)
    {
        return TypeSystem::STRING;
    }

    if (retType == TokenType::CHAR_KEYWORD)
    {
        return TypeSystem::CHAR;
    }

    if (retType == TokenType::BOOL_KEYWORD)
    {
        return TypeSystem::BOOLEAN;
    }

    if (retType == TokenType::VOID)
    {
        return TypeSystem::VOID;
    }

    return TypeSystem::UNKNOWN;
}

bool Semantics::blockAlwaysReturns(Node *block)
{
    auto blockNode = dynamic_cast<BlockExpression *>(block);
    if (!blockNode || blockNode->statements.empty())
        return false;

    Node *lastStmt = blockNode->statements.back().get();

    if (auto ret = dynamic_cast<ReturnStatement *>(lastStmt))
    {
        return true;
    }

    if (auto ifStmt = dynamic_cast<ifStatement *>(lastStmt))
    {
        bool thenReturns = false;
        bool elseReturns = false;

        if (auto thenBlock = dynamic_cast<BlockExpression *>(ifStmt->if_result.get()))
        {
            thenReturns = blockAlwaysReturns(thenBlock);
        }

        if (ifStmt->elseif_result.has_value())
        {
            if (auto elseBlock = dynamic_cast<BlockExpression *>(ifStmt->elseif_result.value().get()))
            {
                elseReturns = blockAlwaysReturns(elseBlock);
            }
        }

        return thenReturns && elseReturns;
    }

    return false;
}

// Error logging function
void Semantics::logError(const std::string &message, Node *node)
{
    if (!node)
    {
        std::cerr << "[SEMANTIC ERROR]: " << message << " (at unknown location)\n";
        return;
    }

    std::cerr << "[SEMANTIC ERROR]: " << message
              << " (line: " << node->token.line
              << ", column: " << node->token.column << ")\n";
}
