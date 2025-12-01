#include "semantics.hpp"

void Semantics::walkGenericStatement(Node *node)
{
    auto genericStmt = dynamic_cast<GenericStatement *>(node);
    if (!genericStmt)
        return;

    std::string blockName = genericStmt->block_name->expression.TokenLiteral;
    int line = genericStmt->block_name->expression.line;
    int col = genericStmt->block_name->expression.column;
    bool hasError = false;

    // Check if the generic name is being used somewhere else in the same scope
    auto existing = resolveSymbolInfo(blockName);

    if (existing)
    {
        logSemanticErrors("Generic block name '" + blockName + "' already in use", line, col);
        hasError = true;
    }

    GenericBluePrint bluePrint;
    bluePrint.name = blockName;

    // Get the type params(order matters)
    for (const auto &typeParam : genericStmt->type_parameters)
    {
        // Extract the name and store it on our vector
        std::string param_name = typeParam.TokenLiteral;
        bluePrint.typeParams.push_back(param_name);
    }

    // build reverse index
    for (int i = 0; i < bluePrint.typeParams.size(); i++)
        bluePrint.typeParamIndex[bluePrint.typeParams[i]] = i;

    // Check if the top level statements in this block are only function statements
    auto blockStmt = dynamic_cast<BlockStatement *>(genericStmt->block.get());
    for (const auto &stmt : blockStmt->statements)
    {
        auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
        if (!fnStmt)
        {
            logSemanticErrors("Must only use function statements as top level in generic blocks", stmt->statement.line, stmt->statement.column);
        }
    }

    // store the original AST subtree
    bluePrint.blockAST = std::move(genericStmt->block);

    // Register in the generic blue print table
    genericMap[blockName] = std::move(bluePrint);

    auto genInfo = std::make_shared<SymbolInfo>();
    genInfo->hasError = hasError;
    genInfo->isGeneric = true;

    symbolTable.back()[blockName] = genInfo;
}

void Semantics::walkInstantiateStatement(Node *node)
{
    auto instStmt = dynamic_cast<InstantiateStatement *>(node);
    if (!instStmt)
        return;

    bool hasError = false;

    // Get the name of the generic we want to instantiate
    auto genericCall = dynamic_cast<GenericCall *>(instStmt->generic_call.get());
    std::string genericName = genericCall->ident->expression.TokenLiteral;
    auto genline = genericCall->ident->expression.line;
    auto gencol = genericCall->ident->expression.column;

    // Extract the type args here and map them to the semantic type system
    std::vector<Token> rawTypes;
    std::vector<ResolvedType> resolvedArgs;
    for (const auto &typeToken : genericCall->args)
    {
        auto type = tokenTypeToResolvedType(typeToken, false);
        // Store the resolved types for later matching to the generic types
        resolvedArgs.push_back(type);
        rawTypes.push_back(typeToken);
    }

    // Get the alias name (Will be appended to the instantiated members)
    std::string aliasName = instStmt->alias.TokenLiteral;

    auto existing = resolveSymbolInfo(aliasName);
    if (existing)
    {
        logSemanticErrors("Alias '" + aliasName + "' already in use", instStmt->alias.line, instStmt->alias.column);
        hasError = true;
    }

    // Search for the generic blue print
    auto bluePrintIt = genericMap.find(genericName);

    if (bluePrintIt == genericMap.end())
    {
        logSemanticErrors("Unknown generic block '" + genericName + "' ", genline, gencol);
        hasError = true;
        return;
    }

    GenericBluePrint &blueprint = bluePrintIt->second;

    // Getting the cloned sub tree
    std::unique_ptr<Node> clonedSubTree(blueprint.blockAST->shallowClone());

    // Check the argument count
    if (genericCall->args.size() != blueprint.typeParams.size())
    {
        logSemanticErrors("Generic '" + genericName + "' requires " + std::to_string(blueprint.typeParams.size()) + " type arguments", genline, gencol);
        hasError = true;
    }

    std::unordered_map<std::string, ResolvedType> paramToType;
    std::unordered_map<std::string, Token> rawTypeMap;

    for (int i = 0; i < blueprint.typeParams.size(); i++)
    {
        paramToType[blueprint.typeParams[i]] = resolvedArgs[i];
        rawTypeMap[blueprint.typeParams[i]] = rawTypes[i];
    }

    // Apply type substitution
    std::cout << "SUBSTITUTING TYPES\n";
    substituteTypes(clonedSubTree.get(), rawTypeMap);

    // Mangle names
    std::cout << "MANGLING NAMES\n";
    mangleGenericName(clonedSubTree.get(), aliasName);

    // Walk the generated subtree
    std::cout << "WALKING CLONED SUB TREE\n";
    walker(clonedSubTree.get());

    // Build the instantion info
    GenericInstantiationInfo instantiationInfo;
    instantiationInfo.aliasName = aliasName;
    instantiationInfo.blueprintName = bluePrintIt->second.name;
    instantiationInfo.paramToType = paramToType;
    instantiationInfo.rawTypeMap = rawTypeMap;
    instantiationInfo.instantiatedAST = std::move(clonedSubTree);

    auto info = std::make_shared<SymbolInfo>();
    info->isInstantiation = true;
    info->hasError = hasError;
    info->instTable = std::move(instantiationInfo);

    symbolTable.back()[aliasName] = info;
    metaData[instStmt] = info;
}

void Semantics::substituteTypes(Node *node, std::unordered_map<std::string, Token> &subMap)
{
    if (auto basic = dynamic_cast<BasicType *>(node))
    {
        // Retrieve the current name(The one in generic)
        std::string genericName = basic->data_token.TokenLiteral;
        // Check the subMap and find the corresponding type token
        if (subMap.count(genericName))
        {
            std::cout << "Subbing type: " << basic->token.TokenLiteral << " to " << subMap.at(genericName).TokenLiteral << "\n";
            basic->data_token = subMap.at(genericName);
            basic->token = subMap.at(genericName);
            basic->expression = subMap.at(genericName);
            std::cout << "New type: " << basic->token.TokenLiteral << "\n";
        }
    }
    else if (auto array = dynamic_cast<ArrayType *>(node))
    {
        substituteTypes(array->innerType.get(), subMap);
    }
    else if (auto ptr = dynamic_cast<PointerType *>(node))
    {
        substituteTypes(ptr->underlyingType.get(), subMap);
    }
    else if (auto ref = dynamic_cast<RefType *>(node))
    {
        substituteTypes(ref->underLyingType.get(), subMap);
    }
    else if (auto ret = dynamic_cast<ReturnType *>(node))
    {
        substituteTypes(ret->returnExpr.get(), subMap);
    }

    // Functions
    if (auto fnStmt = dynamic_cast<FunctionStatement *>(node))
    {
        if (auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get()))
        {
            // Call type sub on the parameters
            for (const auto &param : fnExpr->call)
            {
                substituteTypes(param.get(), subMap);
            }
            // Call type subbing on the return type
            substituteTypes(fnExpr->return_type.get(), subMap);

            // Call type subbing on the body
            substituteTypes(fnExpr->block.get(), subMap);
        }

        if (auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnStmt->funcExpr.get()))
        {
            if (auto fnDeclr = dynamic_cast<FunctionDeclaration *>(fnDeclrExpr->funcDeclrStmt.get()))
            {
                // Call type subbing on the parameters
                for (const auto &param : fnDeclr->parameters)
                {
                    substituteTypes(param.get(), subMap);
                }
                // Call type subbing on the return type
                substituteTypes(fnDeclr->return_type.get(), subMap);
            }
        }
    }

    if (auto blockExpr = dynamic_cast<BlockExpression *>(node))
    {
        for (const auto &stmt : blockExpr->statements)
        {
            substituteTypes(stmt.get(), subMap);
        }
    }

    if (auto blockStmt = dynamic_cast<BlockStatement *>(node))
    {
        for (const auto &stmt : blockStmt->statements)
        {
            substituteTypes(stmt.get(), subMap);
        }
    }

    // Declarations
    if (auto letStmt = dynamic_cast<LetStatement *>(node))
    {
        substituteTypes(letStmt->type.get(), subMap);
    }
}

void Semantics::mangleGenericName(Node *node, std::string aliasName)
{
    if (auto ident = dynamic_cast<Identifier *>(node))
    {
        // Get the original name
        std::string originalName = ident->identifier.TokenLiteral;
        // Mangle the name
        std::string mangledName = aliasName + "_" + originalName;
        // Restore the name back into the ident's name
        ident->identifier.TokenLiteral = mangledName;
        ident->token.TokenLiteral = mangledName;
        ident->expression.TokenLiteral = mangledName;
    }

    if (auto fnStmt = dynamic_cast<FunctionStatement *>(node))
    {
        if (auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get()))
        {
            // Get the name and mangle it
            auto original = fnExpr->func_key.TokenLiteral;
            auto mangled = aliasName + "_" + original;
            fnExpr->func_key.TokenLiteral = mangled;
            fnExpr->token.TokenLiteral = mangled;
            fnExpr->expression.TokenLiteral = mangled;
        }
        if (auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnStmt->funcExpr.get()))
        {
            if (auto fnDeclr = dynamic_cast<FunctionDeclaration *>(fnDeclrExpr->funcDeclrStmt.get()))
            {
                mangleGenericName(fnDeclr->function_name.get(), aliasName);
            }
        }
    }

    if (auto blockStmt = dynamic_cast<BlockStatement *>(node))
    {
        for (const auto &stmt : blockStmt->statements)
        {
            // Just to expose whatever might be in here
            mangleGenericName(stmt.get(), aliasName);
        }
    }
}