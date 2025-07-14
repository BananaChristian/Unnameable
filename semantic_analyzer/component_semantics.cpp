#include "semantics.hpp"
#include <algorithm>

#define CPPREST_FORCE_REBUILD

void Semantics::analyzeDataStatementBlock(Node *node)
{
    auto dataStmt = dynamic_cast<DataStatement *>(node);
    if (!dataStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing data block \n";
    std::string blockName = static_cast<Identifier *>(dataStmt->dataBlockName.get())->identifier.TokenLiteral;

    symbolTable.emplace_back();
    std::vector<Symbol> collectedSymbols;
    // Here we have to analyze what is inside the data block now initially I wanted to use the existing analyzer called analyze block statements but I am rethinking this
    // I think I will manually analyze every item inside this vector for the data block
    auto &dataBlockContents = dataStmt->fields;
    for (const auto &field : dataBlockContents)
    {
        // Now I analyze the nodes one by one
        analyzer(field.get());
        auto letStmt = dynamic_cast<LetStatement *>(field.get());
        if (letStmt)
        {
            collectedSymbols.push_back({.nodeName = letStmt->ident_token.TokenLiteral,
                                        .nodeType = mapTypeStringToTypeSystem(letStmt->data_type_token.TokenLiteral),
                                        .parameterTypes = {},
                                        .kind = SymbolKind::VARIABLE,
                                        .isMutable = false,
                                        .isConstant = false,
                                        .scopeDepth = static_cast<int>(symbolTable.size()) - 1});
        }
    }
    sharedDataBlocks[blockName] = std::move(collectedSymbols);
    symbolTable.pop_back();
}

void Semantics::analyzeBehaviorStatementBlock(Node *node)
{
    auto behaviorStmt = dynamic_cast<BehaviorStatement *>(node);
    if (!behaviorStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing behavior block\n";
    std::string behaviorBlockName = static_cast<Identifier *>(behaviorStmt->behaviorBlockName.get())->identifier.TokenLiteral;
    symbolTable.emplace_back();
    std::vector<Symbol> collectedSymbols;
    auto &behaviorData = behaviorStmt->functions;
    for (const auto &methods : behaviorData)
    {
        analyzer(methods.get());
        auto funcStmt = dynamic_cast<FunctionStatement *>(methods.get());
        if (!funcStmt)
            return;

        auto baseExpr = funcStmt->funcExpr.get();
        if (!baseExpr)
            return;

        if (auto funcExpr = dynamic_cast<FunctionExpression *>(baseExpr))
        {
            // Handle full function definition
            collectedSymbols.push_back({.nodeName = funcExpr->func_key.TokenLiteral,
                                        .nodeType = mapTypeStringToTypeSystem(funcExpr->return_type->expression.TokenLiteral),
                                        .parameterTypes = {},
                                        .kind = SymbolKind::FUNCTION,
                                        .isMutable = false,
                                        .isConstant = false,
                                        .scopeDepth = static_cast<int>(symbolTable.size()) - 1});
        }
        else if (auto declExpr = dynamic_cast<FunctionDeclarationExpression *>(baseExpr))
        {
            if (declExpr->funcDeclrStmt)
            {
                auto declExprStmt = static_cast<FunctionDeclaration *>(declExpr->funcDeclrStmt.get());
                collectedSymbols.push_back({.nodeName = declExprStmt->work_keyword_token.TokenLiteral,
                                            .nodeType = mapTypeStringToTypeSystem(declExprStmt->return_type->expression.TokenLiteral),
                                            .parameterTypes = {},
                                            .kind = SymbolKind::FUNCTION,
                                            .isMutable = false,
                                            .isConstant = false,
                                            .scopeDepth = static_cast<int>(symbolTable.size()) - 1});
            }
            else
            {
                std::cerr << "[SEMANTIC ERROR]: Declaration expression missing actual declaration statement\n";
            }
        }

        else
        {
            std::cerr << "[SEMANTIC ERROR]: Unknown function type in behavior block\n";
        }
    }
    sharedBehaviorBlocks[behaviorBlockName] = std::move(collectedSymbols);
    symbolTable.pop_back();
}

void Semantics::analyzeUseStatement(Node *node)
{
    auto useStmt = dynamic_cast<UseStatement *>(node);
    if (!useStmt)
        return;

    std::cout << "[SEMANTIC LOG]: Analyzing use statement\n";

    auto useStmtType = useStmt->kind_token.type;
    std::string blockName;

    // Attempt to extract the block name safely as an identifier
    if (auto idExpr = dynamic_cast<Identifier *>(useStmt->blockName.get()))
    {
        blockName = idExpr->identifier.TokenLiteral;
    }
    else
    {
        logError("Invalid block name expression in use statement", useStmt);
        return;
    }

    auto &currentScope = symbolTable.back();

    // Handling the case where we are importing from a data block
    if (useStmtType == TokenType::DATA)
    {
        auto dataIt = sharedDataBlocks.find(blockName); // Search for the data block in the shared data block map
        if (dataIt == sharedDataBlocks.end())           // If not found, we throw an error
        {
            logError("Use of undeclared data block: " + blockName, useStmt);
            return;
        }

        // If a specific member is being imported from the block (e.g., Foo.bar)
        if (useStmt->functionCallOrData.has_value())
        {
            auto memberExpr = useStmt->functionCallOrData.value().get();

            // Ensure that the member being accessed is a valid identifier
            if (auto memberId = dynamic_cast<Identifier *>(memberExpr))
            {
                const auto &symbols = dataIt->second;

                // Search the symbol vector for a symbol that matches the one the user is trying to import
                auto symIt = std::find_if(symbols.begin(), symbols.end(),
                                          [&](const Symbol &s)
                                          { return s.nodeName == memberId->expression.TokenLiteral; });

                // If the symbol does not exist in the data block, throw an error
                if (symIt == symbols.end())
                {
                    logError("Symbol '" + memberId->expression.TokenLiteral + "' not found in data block '" + blockName + "'", useStmt);
                    return;
                }

                // Check if the symbol already exists in the current scope
                if (currentScope.find(memberId->expression.TokenLiteral) != currentScope.end())
                {
                    logError("Symbol conflict: '" + memberId->expression.TokenLiteral + "' already defined in current scope", useStmt);
                    return;
                }

                // Add the found symbol into the current scope
                currentScope[memberId->expression.TokenLiteral] = *symIt;
            }
            else
            {
                logError("Expected identifier after '.' in use data", useStmt);
            }
        }
        else
        {
            // If no specific member is accessed, we import all symbols from the data block
            for (const auto &sym : dataIt->second)
            {
                currentScope[sym.nodeName] = sym;
            }
        }
    }

    // Handling the case where we are importing from a behavior block
    else if (useStmtType == TokenType::BEHAVIOR)
    {
        auto behaviorIt = sharedBehaviorBlocks.find(blockName);
        if (behaviorIt == sharedBehaviorBlocks.end())
        {
            logError("Use of undeclared behavior block: " + blockName, useStmt);
            return;
        }

        // If the user is trying to call a specific behavior function (e.g., Foo.bar())
        if (useStmt->functionCallOrData.has_value())
        {
            analyzer(useStmt->functionCallOrData.value().get());

            // Check if it's a valid function call expression
            if (auto callExpr = dynamic_cast<CallExpression *>(useStmt->functionCallOrData.value().get()))
            {
                auto calleeExpr = callExpr->function_identifier.get();

                // Ensure the callee is an identifier
                if (auto idExpr = dynamic_cast<Identifier *>(calleeExpr))
                {
                    const std::string funcName = idExpr->identifier.TokenLiteral;
                    const auto &symbols = behaviorIt->second;

                    // Search for the behavior function in the behavior block
                    auto symIt = std::find_if(symbols.begin(), symbols.end(),
                                              [&](const Symbol &s)
                                              { return s.nodeName == funcName; });

                    if (symIt == symbols.end())
                    {
                        logError("Function '" + funcName + "' not found in behavior block '" + blockName + "'", useStmt);
                        return;
                    }

                    // Add the function to the current scope
                    currentScope[funcName] = *symIt;
                }
            }
            else
            {
                // If not a function call, we just import everything
                for (const auto &sym : behaviorIt->second)
                {
                    currentScope[sym.nodeName] = sym;
                }
            }
        }
    }

    // If the use statement type is neither data nor behavior, we throw an error
    else
    {
        logError("Unknown use kind", useStmt);
    }
}

void Semantics::analyzeInitConstructorStatement(Node *node)
{
    auto initStmt = dynamic_cast<InitStatement *>(node);
    if (!initStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing init constructor statement\n";
    auto &arguments = initStmt->constructor_args;
    symbolTable.push_back({});
    for (const auto &argument : arguments)
    {
        analyzer(argument.get());
    }
    auto block = initStmt->block.get();
    analyzer(block);
    annotations[initStmt] = SemanticInfo{
        .nodeType = TypeSystem::VOID,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };
    symbolTable.pop_back();
}

void Semantics::analyzeFieldAccessExpression(Node *node)
{
    auto selfExpr = dynamic_cast<FieldAccessExpression *>(node);
    if (!selfExpr)
        return;

    std::cout << "[SEMANTIC LOG]: Analyzing field access expression\n";
    analyzer(selfExpr->base.get());

    if (auto ident = dynamic_cast<Identifier *>(selfExpr->base.get()))
    {
        if (ident->identifier.TokenLiteral == "self")
        {
            if (!currentComponent)
            {
                logError("'self' used outside component context", selfExpr);
                return;
            }

            std::string fieldName = selfExpr->field.TokenLiteral;
            bool found = false;
            for (const auto &stmt : currentComponent->privateData)
            {
                if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get()))
                {
                    std::string privateFieldName = letStmt->ident_token.TokenLiteral;
                    if (privateFieldName == fieldName)
                    {
                        found = true;
                        annotations[selfExpr] = SemanticInfo{
                            .nodeType = inferExpressionType(letStmt), // assuming you have a type field here
                            .isMutable = false,                       // or however you track mutability
                            .isConstant = false,
                            .scopeDepth = (int)symbolTable.size() - 1,
                        };
                        break;
                    }
                }
            }
            if (!found)
            {
                logError("Field '" + fieldName + "' not found in component '" +
                             static_cast<Identifier *>(currentComponent->component_name.get())->identifier.TokenLiteral + "'",
                         selfExpr);

                annotations[selfExpr] = SemanticInfo{
                    .nodeType = TypeSystem::UNKNOWN,
                    .isMutable = false,
                    .isConstant = false,
                    .scopeDepth = (int)symbolTable.size() - 1,
                };
            }
            return;
        }
    }

    annotations[selfExpr] = SemanticInfo{
        .nodeType = TypeSystem::UNKNOWN,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };
}

void Semantics::analyzeComponentStatementBlock(Node *node)
{
    auto componentStmt = dynamic_cast<ComponentStatement *>(node);
    if (!componentStmt)
        return;

    ComponentStatement *previousComponent = currentComponent;
    currentComponent = componentStmt;

    std::cout << "[SEMANTIC LOG]: Analyzing component statment block\n";
    std::string componentName = static_cast<Identifier *>(componentStmt->component_name.get())->identifier.TokenLiteral;
    symbolTable.push_back({});
    auto &currentScope = symbolTable.back();
    // Dealing with used data imports
    auto &usedDataBlocks = componentStmt->usedDataBlocks;
    for (const auto &dataUse : usedDataBlocks)
    {
        analyzeUseStatement(dataUse.get());
    }
    // Dealing with private attributes if they exist
    auto &privateAttributes = componentStmt->privateData;
    // Checking for the private data
    if (!(privateAttributes.empty()))
    {
        for (const auto &content : privateAttributes)
        {
            analyzer(content.get());
        }
    }

    auto &privateMethods = componentStmt->privateMethods;
    if (!(privateMethods.empty()))
    {
        for (const auto &content : privateMethods)
        {
            analyzer(content.get());
        }
    }

    auto &usedBehaviorBlocks = componentStmt->usedBehaviorBlocks;
    for (const auto &methodUse : usedBehaviorBlocks)
    {
        analyzeUseStatement(methodUse.get());
    }

    if (componentStmt->initConstructor.has_value())
    {
        auto initStmt = componentStmt->initConstructor.value().get();
        analyzeInitConstructorStatement(initStmt);
    }
    components[componentName]=componentStmt;

    annotations[componentStmt] = SemanticInfo{
        .nodeType = TypeSystem::COMPONENT,
        .isMutable = false,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1};

    symbolTable.pop_back();
}

void Semantics::analyzeNewComponentExpression(Node *node){
    auto newExpr = dynamic_cast<NewComponentExpression *>(node);
    if (!newExpr)
        return;
    std::cout << "[SEMANTIC LOG]: Analyzing new component expression\n";

    const std::string &componentName = newExpr->component_name.TokenLiteral;
    auto componentIt=components.find(componentName);

    if(componentIt==components.end()){
        logError("Component '" + componentName + "' not found", newExpr);
        annotations[newExpr] = SemanticInfo{
            .nodeType = TypeSystem::UNKNOWN,
            .customTypeName=componentName,
            .isMutable = false,
            .isConstant = false,
            .scopeDepth = (int)symbolTable.size() - 1,
        };
        return;
    }

    const auto &componentStmt=componentIt->second;

    for (auto &arg : newExpr->arguments) {
        analyzer(arg.get());
    }

    if (componentStmt->initConstructor.has_value()) {
        auto initCtor = static_cast<InitStatement*>(componentStmt->initConstructor.value().get());

        size_t expectedArgs = initCtor->constructor_args.size();
        size_t actualArgs = newExpr->arguments.size();

        if (actualArgs != expectedArgs) {
            logError("Constructor argument count mismatch for component '" + componentName + "'. Expected " +
                         std::to_string(expectedArgs) + ", got " + std::to_string(actualArgs),
                     newExpr);
        }
    }
    else if (!newExpr->arguments.empty()) {
        logError("Component '" + componentName + "' has no constructor but arguments were provided", newExpr);
    }

    annotations[newExpr] = SemanticInfo{
        .nodeType = TypeSystem::COMPONENT, 
        .customTypeName=componentName,
        .isMutable = true,
        .isConstant = false,
        .scopeDepth = (int)symbolTable.size() - 1,
    };

}