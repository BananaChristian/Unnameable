#include <string>

#include "ast.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include "token.hpp"

void Semantics::giveGenericLiteralContext(
    Node *literal, const ResolvedType &contextType,
    const std::shared_ptr<SymbolInfo> &litSym) {
  if (dynamic_cast<INTLiteral *>(literal)) {
    if (isInteger(contextType))
      litSym->type().type = contextType;
    return;
  }
  if (dynamic_cast<FloatLiteral *>(literal)) {
    if (isFloat(contextType))
      litSym->type().type = contextType;
    return;
  }
  if (auto infix = dynamic_cast<InfixExpression *>(literal)) {
    bool leftIsGeneric = isGenericIntOrFloat(infix->left_operand.get());
    bool rightIsGeneric = isGenericIntOrFloat(infix->right_operand.get());

    if (leftIsGeneric) {
      auto leftSym = getSymbolFromMeta(infix->left_operand.get());
      if (leftSym)
        giveGenericLiteralContext(infix->left_operand.get(), contextType,
                                  leftSym);
    }
    if (rightIsGeneric) {
      auto rightSym = getSymbolFromMeta(infix->right_operand.get());
      if (rightSym)
        giveGenericLiteralContext(infix->right_operand.get(), contextType,
                                  rightSym);
    }
    if (litSym)
      litSym->type().type = contextType;
  }
}

void Semantics::enforceDeclarationRules(
    VariableDeclaration *declaration,
    const std::shared_ptr<SymbolInfo> &declInfo) {
  // Set up
  bool isMutable = declaration->mutability == Mutability::MUTABLE;
  bool isConstant = declaration->mutability == Mutability::CONSTANT;
  bool isHeap = declaration->isHeap;
  bool isPersist = declaration->isPersist;
  bool isRestrict = declaration->isRestrict;
  bool isVolatile = declaration->isVolatile;
  bool isExportable = declaration->isExportable;
  bool isPointer = false;
  bool isRef = false;
  bool isArray = false;
  bool isGlobal = isGlobalScope();

  auto initializer = declaration->initializer.get();

  auto base_type = dynamic_cast<BasicType *>(declaration->base_type.get());
  bool isNullable = base_type->isNullable;

  auto type_modifier =
      dynamic_cast<TypeModifier *>(declaration->modified_type.get());
  if (type_modifier) {
    isPointer = type_modifier->isPointer;
    isRef = type_modifier->isReference;
    isArray = type_modifier->isArray;
  }

  const std::string &declName = declaration->var_name->expression.TokenLiteral;

  // Block a name reuse
  if (resolveSymbolInfo(declName)) {
    logSemanticErrors(ErrorCode::DuplicateName, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  // Check the constant
  if (!initializer && isConstant) {
    logSemanticErrors(ErrorCode::ConstMustBeInitialized, declaration,
                      {declName});
  }

  if (isConstant && !isConstLiteral(initializer)) {
    logSemanticErrors(ErrorCode::ConstRequiresLiteral, declaration, {declName});
  }

  if (isConstant && isNullable) {
    logSemanticErrors(ErrorCode::ConstCannotBeNullable, declaration,
                      {declName});
  }

  if (isPersist && isConstant) {
    logSemanticErrors(ErrorCode::ConstCannotBePersist, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  if (isVolatile && isConstant) {
    logSemanticErrors(ErrorCode::ConstCannotBeVolatile, declaration,
                      {declName});
    insertErrorMetaData(declaration);
    return;
  }

  if (declaration->fnPtrMod && isHeap) {
    logSemanticErrors(HeapFnPtrInvalid, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  // Block usage of inbuilt heap qualifier when in freestanding
  if (isHeap && freeStanding) {
    if (!declaration->allocator) {
      logSemanticErrors(ErrorCode::IllegalUseInFreeStanding, declaration,
                        {"heap"});
      insertErrorMetaData(declaration);
      return;
    }
  }

  if (isHeap && isConstant) {
    logSemanticErrors(ConstCannotBeHeap, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  // Persist requires heap
  if (isPersist && !isHeap) {
    logSemanticErrors(ErrorCode::PersistRequiresHeap, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }
  // Rule on exportables
  if (isExportable && !isGlobal) {
    logSemanticErrors(ExportableRequiresGlobal, declaration);
    insertErrorMetaData(declaration);
    return;
  }

  // Global scope rules
  if (isGlobal) {
    if (isHeap) {
      logSemanticErrors(ErrorCode::GlobalHeapVar, declaration, {declName});
      insertErrorMetaData(declaration);
      return;
    }

    if (isArray) {
      // Check array size is constant
      if (type_modifier && !type_modifier->dimensions.empty()) {
        for (auto &dim : type_modifier->dimensions) {
          if (!isConstLiteral(dim.get())) {
            logSemanticErrors(GlobalArraySizeConst, dim.get());
            insertErrorMetaData(declaration);
            return;
          }
        }
      }
    }
  }

  // Assignment sign rule for pointers and references
  if (isPointer || isRef) {
    if (initializer) {
      if (!insideRecord) {
        if (declaration->assign_token.has_value() &&
            declaration->assign_token->type != TokenType::ARROW) {
          std::string kind = isPointer ? "ptr" : "ref";
          logSemanticErrors(ErrorCode::PointerRequiresArrow, declaration,
                            {declName});
        }
      }
    }
  }

  if (isRestrict && isRef) {
    logSemanticErrors(ErrorCode::RestrictOnRefInvalid, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  // Pointer rules
  if (isPointer && !initializer) {
    if (!insideRecord) {
      logSemanticErrors(ErrorCode::PointerMustBeInitialized, declaration,
                        {declName});
    }
  }

  if (isRestrict && !isPointer) {
    logSemanticErrors(ErrorCode::RestrictOnNonPointer, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  // Reference rules
  if (!insideRecord) {
    if (isRef && !initializer) {
      logSemanticErrors(RefMustBeInitialized, declaration, {declName});
    }
  }

  if (isRef && isNullable) {
    logSemanticErrors(RefCannotBeNullable, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  if (isPersist && isRef) {
    logSemanticErrors(RefCannotBePersist, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  if (isVolatile && isRef) {
    logSemanticErrors(RefCannotBeVolatile, declaration, {declName});
    return;
  }

  if (isHeap && isRef) {
    logSemanticErrors(RefCannotBeHeap, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  // Array rules
  if (isArray && !initializer) {
    if (type_modifier && type_modifier->dimensions.empty()) {
      logSemanticErrors(ErrorCode::ArrayMissingDimensions, declaration,
                        {declName});
      insertErrorMetaData(declaration);
      return;
    }
  }

  // Rule on someone who might write void x = whatever
  if (!declaration->fnPtrMod && base_type->token.type == TokenType::VOID) {
    logSemanticErrors(VoidVariableInvalid, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  // Create initial info I will collect it later
  if (declaration->fnPtrMod)
    declInfo->type().isFnPtr = true;

  declInfo->storage().isMutable = isMutable;
  declInfo->storage().isConstant = isConstant;
  declInfo->hasError = hasError;
  declInfo->storage().isVolatile = isVolatile;
  declInfo->storage().isRestrict = isRestrict;
  declInfo->storage().isPersist = isPersist;
  declInfo->type().isRef = isRef;
  declInfo->type().isPointer = isPointer;
  declInfo->storage().isHeap = isHeap;
  declInfo->storage().isGlobal = isGlobal;
  declInfo->type().isArray = isArray;
  declInfo->type().isNullable = isNullable;
  declInfo->isExportable = isExportable;
}

void Semantics::handleNullInitializers(
    VariableDeclaration *declaration,
    const std::shared_ptr<SymbolInfo> &declInfo,
    const std::shared_ptr<SymbolInfo> &nullInfo) {
  const std::string &declName = declaration->var_name->expression.TokenLiteral;
  auto type_modifier =
      dynamic_cast<TypeModifier *>(declaration->modified_type.get());

  if (!declInfo->type().isNullable) {
    logSemanticErrors(ErrorCode::NullToNonNullable, declaration, {declName});
    insertErrorMetaData(declaration);
    return;
  }

  if (declInfo->type().isArray) {
    if (type_modifier->dimensions.empty()) {
      logSemanticErrors(NullToArrayNoDims, declaration);
    }
    return;
  }

  if (declInfo->type().isRef) {
    logSemanticErrors(NullToReference, declaration, {declName});
  }

  // Give the null context
  nullInfo->type().type = declInfo->type().type;

  if (declInfo->storage().isHeap) {
    auto lifetime = createLifeTimeTracker(declaration, nullptr, declInfo);
    declInfo->codegen().ID = lifetime->ID;
    responsibilityTable[declaration] = std::move(lifetime);
  }
  declInfo->hasError = hasError;
  insertMetaData(declaration, declInfo);
  payload.symbolTable.back()[declName] = declInfo;
  // Clear the global error flagger
  hasError = false;
  return;
}

void Semantics::walkVariableDeclaration(Node *node) {
  auto declaration = dynamic_cast<VariableDeclaration *>(node);
  if (!declaration)
    return;

  const std::string &declName = declaration->var_name->expression.TokenLiteral;
  auto initializer = declaration->initializer.get();
  auto type_modifier =
      dynamic_cast<TypeModifier *>(declaration->modified_type.get());

  auto declInfo = std::make_shared<SymbolInfo>();
  enforceDeclarationRules(declaration, declInfo);

  // Get the allocator
  std::string allocType;
  if (declaration->allocator) {
    auto allocator = dynamic_cast<Identifier *>(declaration->allocator.get());
    const std::string &allocatorName = extractIdentifierName(allocator);
    auto allocIt = payload.allocatorMap.find(allocatorName);
    if (allocIt == payload.allocatorMap.end()) {
      logSemanticErrors(ErrorCode::UnknownAllocator, allocator,
                        {allocatorName});
      insertErrorMetaData(declaration);
      return;
    }

    allocType = allocIt->first;
  } else {
    allocType = globalAllocatorName; // The default GPA type
  }
  declInfo->storage().allocType = allocType;

  auto declType = declInfo->type().type.resolvedName;

  ResolvedType declaredType = inferNodeDataType(declaration);
  declInfo->type().type = declaredType;

  if (isCompOrRecordType(declInfo->type().type) && isGlobalScope()) {
    if (customTypeHasHeapFields(declInfo->type().type.resolvedName)) {
      logSemanticErrors(GlobalHeapVar, declaration, {declName});
      insertErrorMetaData(declaration);
      return;
    }
  }

  std::vector<uint64_t> declSizePerDim;
  std::vector<Node *> dynSizePerDim;
  if (declInfo->type().isArray) {
    if (type_modifier) {
      collectDimensions(type_modifier, declSizePerDim, dynSizePerDim);
    }
    declInfo->type().sizePerDimensions = declSizePerDim;
    declInfo->type().dynSizePerDimensions = dynSizePerDim;
  }

  std::shared_ptr<SymbolInfo> initSym = nullptr;
  if (initializer) {
    declInfo->storage().isInitialized = true;
    walker(initializer);
    auto initName = extractIdentifierName(initializer);
    initSym = getSymbolFromMeta(initializer);

    if (!initSym) {
      logSemanticErrors(UndefinedVariable, initializer, {initName});
      insertErrorMetaData(declaration);
      return;
    }

    // If the initializer is erronious dont proceed
    if (initSym->hasError) {
      logSemanticErrors(ErroniousInitializer, initializer);
      insertErrorMetaData(declaration);
      return;
    }

    // Null checks and giving the null literals context
    if (dynamic_cast<NullLiteral *>(initializer)) {
      handleNullInitializers(declaration, declInfo, initSym);
      return;
    }

    // Give generic lits context
    giveGenericLiteralContext(initializer, declInfo->type().type, initSym);

    // Basic Type checks
    auto errorType = ResolvedType::error();

    logInternal("Declared variable type: " + declaredType.resolvedName);
    if (declaredType.kind == DataType::OPAQUE && !declInfo->type().isPointer) {
      errorHandler.addHint("'opaque' can only be used with pointer types")
          .addHint("Example: ptr opaque p")
          .addHint("Remove 'opaque' for non-pointer types");
      logSemanticErrors(OpaqueNonPointer, declaration);
      return;
    }

    // Type check
    if (initializer) {
      bool ignoreCheck = false;
      // References are a special case if I wrote ref i32 x -> y I am guaranteed
      // to type mismatch so we are gonna do a simple dodge
      if (declaredType.isRef()) {
        logInternal("Encountered reference gonna skip a type check");
        ignoreCheck = true;
      }
      if (declaredType.kind == DataType::OPAQUE) {
        if (!initSym->type().type.isPointer()) {
          logSemanticErrors(ErrorCode::OpaqueInitNonPointer, declaration);
        }
      } else if (!ignoreCheck) {
        if (!isTypeCompatible(declaredType, initSym->type().type)) {
          logSemanticErrors(
              TypeMismatch, declaration,
              {declaredType.resolvedName, initSym->type().type.resolvedName});
          insertErrorMetaData(declaration);
          return;
        }
      }
    }

    //  Reference rules
    if (declInfo->type().isRef) {
      if (initSym->type().isNullable) {
        logSemanticErrors(RefToNullable, declaration);
        insertErrorMetaData(declaration);
        return;
      }

      if (!initSym->storage().isHeap && !initSym->storage().isGlobal) {
        logSemanticErrors(ErrorCode::RefToLocal, declaration, {initName});
        insertErrorMetaData(declaration);
        return;
      }

      if (declInfo->storage().isMutable && !initSym->storage().isMutable) {
        logSemanticErrors(MutableRefToImmutable, declaration);
      }

      // A reference is an alias so it inherits the properties of the guy he is
      // bound to
      declInfo->storage().isMutable = initSym->storage().isMutable;
      declInfo->storage().isConstant = initSym->storage().isConstant;

      declInfo->relations().refereeSymbol = initSym;
      initSym->storage().refCount += 1;
    }

    // Pointer rules
    if (declInfo->type().isPointer) {
      declInfo->relations().targetSymbol = initSym;
      initSym->storage().pointerCount += 1;
    }

    // Array rules
    if (declInfo->type().isArray) {
      if (!initSym->type().sizePerDimensions.empty() &&
          !type_modifier->dimensions.empty()) {
        if (declSizePerDim.size() != initSym->type().sizePerDimensions.size()) {
          logSemanticErrors(
              ErrorCode::ArrayDimCountMismatch, declaration,
              {declName, std::to_string(declSizePerDim.size()),
               std::to_string(initSym->type().sizePerDimensions.size())});
          insertErrorMetaData(declaration);
          return;
        }
        declSizePerDim = initSym->type().sizePerDimensions;
        declInfo->type().sizePerDimensions = declSizePerDim;
      } else if (!initSym->type().sizePerDimensions.empty()) {
        // No explicit dimensions, inherit from the literal
        declInfo->type().sizePerDimensions = initSym->type().sizePerDimensions;
      }
    }
  }

  // Baton creation
  LifeTime *targetBaton = nullptr;
  std::unique_ptr<LifeTime> lifetime = nullptr;

  if (declInfo->storage().isHeap)
    lifetime = createLifeTimeTracker(declaration, initializer, declInfo);

  if (lifetime)
    declInfo->codegen().ID = lifetime->ID;

  if (isCompOrRecordType(declInfo->type().type)) {
    logInternal("Triggered field capture");
    executeFieldsCapture(declaration, declInfo->type().type.resolvedName,
                         lifetime);
  }

  if (lifetime)
    responsibilityTable[declaration] = std::move(lifetime);

  // If the declaration itself isnt heap we still need to transfer baton in its
  // initializer
  if (!declInfo->storage().isHeap && initializer) {
    auto idents = digIdentifiers(initializer);
    for (const auto &identifier : idents) {
      auto identSym = getSymbolFromMeta(identifier);
      if (!identSym) {
        logInternal("Failed to get identifier symbol info for '" +
                    extractIdentifierName(identifier) + "' skipping...");
        continue;
      }

      if (!identSym->storage().isHeap) {
        logInternal("Skipping non heap ident transfer");
        continue;
      }

      targetBaton = getBaton(identSym->codegen().ID);
      if (!targetBaton)
        reportDevBug("Failed to get target baton for lifetime ID: " +
                         identSym->codegen().ID,
                     identifier);

      transferResponsibility(nullptr, targetBaton, identSym);
    }
  }

  insertMetaData(declaration, declInfo);
  logInternal("[DEBUG] Stored metadata for " + declName +
              " | isHeap: " + std::to_string(declInfo->storage().isHeap) +
              " | ID: " + declInfo->codegen().ID +
              " | type :" + declInfo->type().type.resolvedName);
  payload.symbolTable.back()[declName] = declInfo;
}
