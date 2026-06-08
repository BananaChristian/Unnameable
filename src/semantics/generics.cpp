#include "ast.hpp"
#include "semantics.hpp"

void Semantics::walkGenericStatement(Node *node) {
  auto genericStmt = dynamic_cast<GenericStatement *>(node);
  if (!genericStmt)
    return;

  std::string blockName = genericStmt->block_name->expression.TokenLiteral;

  auto existing = resolveSymbolInfo(blockName);
  if (existing)
    logSemanticErrors(ErrorCode::DuplicateName, genericStmt->block_name.get(),
                      {blockName});

  // Create the blueprint on the heap via shared_ptr
  auto bluePrint = std::make_shared<GenericBluePrint>();
  bluePrint->name = blockName;

  for (const auto &typeParam : genericStmt->type_parameters) {
    bluePrint->typeParams.push_back(typeParam.TokenLiteral);
  }

  for (int i = 0; i < static_cast<int>(bluePrint->typeParams.size()); i++)
    bluePrint->typeParamIndex[bluePrint->typeParams[i]] = i;

  auto blockStmt = dynamic_cast<BlockStatement *>(genericStmt->block.get());
  for (const auto &stmt : blockStmt->statements) {
    auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
    auto recordStmt = dynamic_cast<RecordStatement *>(stmt.get());
    auto componentStmt = dynamic_cast<ComponentStatement *>(stmt.get());
    if (!fnStmt && !recordStmt && !componentStmt) {
      logSemanticErrors(ErrorCode::InvalidStmtInGenerics, stmt.get());
    }
  }

  bluePrint->blockAST =
      std::unique_ptr<Node>(genericStmt->block->shallowClone());

  // Store the shared_ptr directly in the payload map
  payload.genericMap[blockName] = bluePrint;

  auto genInfo = std::make_shared<SymbolInfo>();
  genInfo->hasError = hasError;
  genInfo->generic().isGeneric = true;

  payload.symbolTable.back()[blockName] = genInfo;
}

void Semantics::walkInstantiateStatement(Node *node) {
  auto instStmt = dynamic_cast<InstantiateStatement *>(node);
  if (!instStmt)
    return;

  bool isExportable = instStmt->isExportable;
  auto genericCall = dynamic_cast<GenericCall *>(instStmt->generic_call.get());
  std::string genericName = genericCall->ident->expression.TokenLiteral;

  std::vector<Node *> rawTypes;
  std::vector<ResolvedType> resolvedArgs;
  for (const auto &typeExpr : genericCall->args) {
    auto type = inferNodeDataType(typeExpr.get());
    resolvedArgs.push_back(type);
    rawTypes.push_back(typeExpr.get());
  }

  std::string aliasName = instStmt->alias.TokenLiteral;

  auto existing = resolveSymbolInfo(aliasName);
  if (existing) {
    logSemanticErrors(ErrorCode::DuplicateName, instStmt, {aliasName});
  }

  auto bluePrintIt = payload.genericMap.find(genericName);
  if (bluePrintIt == payload.genericMap.end()) {
    logSemanticErrors(ErrorCode::UndefinedVariable, genericCall->ident.get(),
                      {genericName});
    return;
  }

  std::shared_ptr<GenericBluePrint> blueprint = bluePrintIt->second;

  if (genericCall->args.size() != blueprint->typeParams.size()) {
    logSemanticErrors(ErrorCode::ArgumentSizeMismatch, genericCall->ident.get(),
                      {genericName,
                       std::to_string(blueprint->typeParams.size()),
                       std::to_string(genericCall->args.size())});
  }

  std::unique_ptr<Node> clonedSubTree(blueprint->blockAST->shallowClone());

  std::unordered_map<std::string, ResolvedType> paramToType;
  std::unordered_map<std::string, Node *> rawTypeMap;

  for (int i = 0; i < static_cast<int>(blueprint->typeParams.size()); i++) {
    paramToType[blueprint->typeParams[i]] = resolvedArgs[i];
    rawTypeMap[blueprint->typeParams[i]] = rawTypes[i];
  }

  logInternal("Substituting Types ...");
  substituteTypes(clonedSubTree.get(), rawTypeMap);

  logInternal("Mangling name ...");
  mangleGenericName(clonedSubTree.get(), aliasName);

  auto blockStmt = dynamic_cast<BlockStatement *>(clonedSubTree.get());
  for (const auto &stmt : blockStmt->statements) {
    if (auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get())) {
      if (auto fnExpr =
              dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get()))
        fnExpr->isExportable = isExportable;
      if (auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(
              fnStmt->funcExpr.get())) {
        auto fnDeclrStmt = dynamic_cast<FunctionDeclaration *>(
            fnDeclrExpr->funcDeclrStmt.get());
        fnDeclrStmt->isExportable = isExportable;
      }
    }
    if (auto recordStmt = dynamic_cast<RecordStatement *>(stmt.get()))
      recordStmt->isExportable = isExportable;

    if (auto componentStmt = dynamic_cast<ComponentStatement *>(stmt.get()))
      componentStmt->isExportable = isExportable;
  }

  logInternal("Analyzing cloned sub tree ...");
  walker(clonedSubTree.get());

  GenericInstantiationInfo instantiationInfo;
  instantiationInfo.aliasName = aliasName;
  instantiationInfo.blueprintName = blueprint->name; // Use arrow operator
  instantiationInfo.paramToType = paramToType;
  instantiationInfo.rawTypeMap = rawTypeMap;
  instantiationInfo.instantiatedAST = std::move(clonedSubTree);

  auto info = std::make_shared<SymbolInfo>();
  info->generic().isInstantiation = true;
  info->hasError = hasError;
  info->generic().instTable = std::move(instantiationInfo);

  payload.symbolTable.back()[aliasName] = info;
  insertMetaData(instStmt, info);
}

void Semantics::substituteTypes(
    Node *node, std::unordered_map<std::string, Node *> &subMap) {

  if (auto basic = dynamic_cast<BasicType *>(node)) {
    std::string genericName = basic->type_token.TokenLiteral;
    if (subMap.count(genericName)) {
      logInternal("Subbing Type: " + basic->token.TokenLiteral + " to " +
                  subMap.at(genericName)->token.TokenLiteral);
      basic->type_token = subMap.at(genericName)->token;
      basic->token = subMap.at(genericName)->token;
      basic->expression = subMap.at(genericName)->token;
    }
  } else if (auto ret = dynamic_cast<ReturnType *>(node)) {
    if (ret->base_type)
      substituteTypes(ret->base_type.get(), subMap);
  }

  // Functions
  if (auto fnStmt = dynamic_cast<FunctionStatement *>(node)) {
    if (auto fnExpr =
            dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get())) {
      for (const auto &param : fnExpr->parameters)
        substituteTypes(param.get(), subMap);
      substituteTypes(fnExpr->return_type.get(), subMap);
      substituteTypes(fnExpr->block.get(), subMap);
    }

    if (auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(
            fnStmt->funcExpr.get())) {
      if (auto fnDeclr = dynamic_cast<FunctionDeclaration *>(
              fnDeclrExpr->funcDeclrStmt.get())) {
        for (const auto &param : fnDeclr->parameters)
          substituteTypes(param.get(), subMap);
        substituteTypes(fnDeclr->return_type.get(), subMap);
      }
    }
  }

  if (auto blockExpr = dynamic_cast<BlockExpression *>(node)) {
    for (const auto &stmt : blockExpr->statements)
      substituteTypes(stmt.get(), subMap);
  }

  if (auto blockStmt = dynamic_cast<BlockStatement *>(node)) {
    for (const auto &stmt : blockStmt->statements)
      substituteTypes(stmt.get(), subMap);
  }

  // Variables
  if (auto varDecl = dynamic_cast<VariableDeclaration *>(node)) {
    if (varDecl->base_type)
      substituteTypes(varDecl->base_type.get(), subMap);
  }

  // Record statement
  if (auto recordStmt = dynamic_cast<RecordStatement *>(node))
    for (const auto &field : recordStmt->fields)
      substituteTypes(field.get(), subMap);

  // Component Statement
  if (auto compStmt = dynamic_cast<ComponentStatement *>(node)) {
    for (const auto &field : compStmt->fields)
      substituteTypes(field.get(), subMap);

    for (const auto &method : compStmt->methods)
      substituteTypes(method.get(), subMap);

    if (compStmt->initConstructor.has_value()) {
      auto initConstructor = dynamic_cast<InitStatement *>(
          compStmt->initConstructor.value().get());
      for (const auto &param : initConstructor->constructor_args)
        substituteTypes(param.get(), subMap);
      substituteTypes(initConstructor->block.get(), subMap);
    }
  }
}

void Semantics::mangleGenericName(Node *node, std::string aliasName) {
  if (auto ident = dynamic_cast<Identifier *>(node)) {
    // Get the original name
    std::string originalName = ident->identifier.TokenLiteral;
    // Mangle the name
    std::string mangledName = aliasName + "_" + originalName;
    // Restore the name back into the ident's name
    ident->identifier.TokenLiteral = mangledName;
    ident->token.TokenLiteral = mangledName;
    ident->expression.TokenLiteral = mangledName;
  }

  if (auto fnStmt = dynamic_cast<FunctionStatement *>(node)) {
    if (auto fnExpr =
            dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get())) {
      // Get the name and mangle it
      auto original = extractIdentifierName(fnExpr);
      auto mangled = aliasName + "_" + original;
      fnExpr->func_identifier.get()->token.TokenLiteral = mangled;
      fnExpr->token.TokenLiteral = mangled;
      fnExpr->expression.TokenLiteral = mangled;
    }
    if (auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(
            fnStmt->funcExpr.get())) {
      if (auto fnDeclr = dynamic_cast<FunctionDeclaration *>(
              fnDeclrExpr->funcDeclrStmt.get())) {
        mangleGenericName(fnDeclr->function_name.get(), aliasName);
      }
    }
  }

  if (auto blockStmt = dynamic_cast<BlockStatement *>(node)) {
    for (const auto &stmt : blockStmt->statements) {
      // Just to expose whatever might be in here
      mangleGenericName(stmt.get(), aliasName);
    }
  }

  if (auto recordStmt = dynamic_cast<RecordStatement *>(node))
    mangleGenericName(recordStmt->recordName.get(), aliasName);

  if (auto compStmt = dynamic_cast<ComponentStatement *>(node))
    mangleGenericName(compStmt->component_name.get(), aliasName);
}
