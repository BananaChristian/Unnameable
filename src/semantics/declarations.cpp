#include "ast.hpp"
#include "semantics.hpp"
#include "token.hpp"
#include <string>

void Semantics::giveGenericLiteralContext(
    Node *literal, const std::shared_ptr<SymbolInfo> &contextSym,
    const std::shared_ptr<SymbolInfo> &litSym) {
  if (dynamic_cast<INTLiteral *>(literal)) {
    if (isInteger(contextSym->type().type))
      litSym->type().type = contextSym->type().type;
  }

  if (dynamic_cast<FloatLiteral *>(literal)) {
    if (isFloat(contextSym->type().type))
      litSym->type().type = contextSym->type().type;
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
  bool isPointer = false;
  bool isRef = false;
  bool isArray = false;

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
    std::string errorMsg =
        "Variable '" + declName + "' cannot be redeclared in the same scope";
    errorHandler.addHint("Variable names must be unique within their scope")
        .addHint("Consider using a different name");
    return;
  }

  // Check the constant
  if (!initializer && isConstant) {
    errorHandler.addHint("Constants must be initialized at declaration")
        .addHint("Example: const i32 MAX_SIZE = 100")
        .addHint("For uninitialized mutable variables, use 'mut'");
    logSemanticErrors("Constant variable '" + declName +
                          "' must be initialized",
                      declaration);
  }

  if (isConstant && !isConstLiteral(initializer)) {
    errorHandler.addHint("Constants require compile-time known values")
        .addHint("Use literals: 42, 3.14, \"hello\", true, etc.")
        .addHint("For runtime immutable values, remove the 'const' keyword");

    logSemanticErrors(
        "Constant '" + declName +
            "' must be initialized with a literal, not a runtime expression",
        declaration);
  }

  if (isConstant && isNullable) {
    errorHandler.addHint("Constants must hold a concrete value, never 'null'")
        .addHint("Remove the '?' suffix to make the type non-nullable")
        .addHint("Example: const i32 VALUE = 42 (not i32?)");

    logSemanticErrors("Constant '" + declName + "' cannot be nullable",
                      declaration);
  }

  if (isPersist && isConstant) {
    errorHandler
        .addHint("'persist' delays baton-system frees on heap variables")
        .addHint("Constants are compile-time values — the baton system never "
                 "touches them")
        .addHint("Remove 'persist', constants manage themselves");
    logSemanticErrors("Constant '" + declName + "' cannot be marked 'persist'",
                      declaration);
    return;
  }

  if (isVolatile && isConstant) {
    errorHandler
        .addHint("'const' means compile-time fixed, 'volatile' means hardware "
                 "can change it")
        .addHint("These are contradictory, pick one")
        .addHint("For read-only hardware registers, use 'volatile' alone");
    logSemanticErrors("Variable '" + declName +
                          "' cannot be both 'volatile' and 'const'",
                      declaration);
    return;
  }

  if (isHeap && isConstant) {
    errorHandler
        .addHint("Constants are compile-time values, heap is runtime dynamic")
        .addHint("Remove 'heap' and let the compiler handle storage")
        .addHint(
            "If you need a large constant, consider a global const instead");
    logSemanticErrors("Constant '" + declName + "' cannot be heap allocated",
                      declaration);
    return;
  }

  // Persist requires heap
  if (isPersist && !isHeap) {
    errorHandler.addHint("'persist' flag only applies to heap allocations")
        .addHint("For example  'persist heap i32 x = 10'");
    logSemanticErrors("Variable '" + declName +
                          "' uses 'persist' without 'heap'",
                      declaration);
    return;
  }

  if (isGlobalScope() && isHeap) {
    logSemanticErrors("Cannot declare a heap variable '" + declName +
                          "' in global scope",
                      declaration);
    return;
  }

  // Assignment sign rule for pointers and references
  if (isPointer || isRef) {
    if (!declaration->assign_token.has_value() ||
        declaration->assign_token->type != TokenType::ARROW) {

      std::string kind = isPointer ? "pointer" : "reference";

      errorHandler
          .addHint(
              kind +
              " variables must be initialized with the arrow operator '->'")
          .addHint("Example: " + kind + " i32 p -> addr x")
          .addHint("The arrow indicates the " + kind +
                   " is being bound to a target")
          .addHint("For regular assignment, use '='");

      logSemanticErrors(kind + " variable '" + declName +
                            "' requires '->' for initialization, not '='",
                        declaration);
    }
  }

  if (isRestrict && isRef) {
    errorHandler
        .addHint(
            "'restrict' is a pointer-only qualifier that indicates no aliasing")
        .addHint("References already guarantee unique access semantics")
        .addHint("Remove 'restrict' or use a pointer instead of a reference");
    logSemanticErrors("Cannot apply 'restrict' to reference variable '" +
                          declName + "'",
                      declaration);
    return;
  }

  // Pointer rules
  if (isPointer && !initializer) {
    errorHandler.addHint("Pointers must point to something, even if it's null")
        .addHint("Initialize with: 'addr variable', 'bitcast<ptr "
                 "T>(0)', or another pointer")
        .addHint("Example: ptr i32 p -> addr x")
        .addHint("Example: ptr i32 p -> bitcast<ptr i32>(0)  # null pointer");

    logSemanticErrors("Pointer variable '" + declName + "' must be initialized",
                      declaration);
  }

  if (isRestrict && !isPointer) {
    errorHandler
        .addHint("'restrict' only applies to pointers, it signals no aliasing")
        .addHint("Remove 'restrict' for non-pointer declarations")
        .addHint("Example of valid usage: restrict ptr i32 p -> addr x");
    logSemanticErrors("'restrict' cannot be applied to non-pointer variable '" +
                          declName + "'",
                      declaration);
    return;
  }

  // Reference rules
  if (!insideRecord || !insideComponent) {
    if (isRef && !initializer) {
      errorHandler.addHint("References must always point to valid memory")
          .addHint("Initialize with: 'addr variable' or another reference")
          .addHint("Example: ref i32 r -> x")
          .addHint("References cannot be null or uninitialized");

      logSemanticErrors("Reference variable '" + declName +
                            "' must be initialized",
                        declaration);
    }
  }

  if (isRef && isNullable) {
    logSemanticErrors("Reference variable '" + declName +
                          "' cannot be nullable",
                      declaration);
    return;
  }

  if (isPersist && isRef) {
    errorHandler.addHint("'persist' only affects heap allocations")
        .addHint("References are aliases with no heap allocation of their own")
        .addHint("Mark the target variable as 'persist' instead");
    logSemanticErrors("Reference '" + declName + "' cannot be marked 'persist'",
                      declaration);
    return;
  }

  if (isVolatile && isRef) {
    errorHandler
        .addHint(
            "'volatile' tells the compiler to always read fresh from memory")
        .addHint("References are aliases, apply 'volatile' to the target "
                 "variable instead")
        .addHint(
            "Example: declare the heap target as volatile, then reference it");
    logSemanticErrors("Reference '" + declName +
                          "' cannot be marked 'volatile'",
                      declaration);
    return;
  }

  if (isHeap && isRef) {
    errorHandler.addHint("References are aliases, they cannot own heap memory")
        .addHint("Remove 'heap', references track their target's lifetime")
        .addHint("If you need heap memory, declare the target as heap instead");
    logSemanticErrors("Reference '" + declName + "' cannot be heap allocated",
                      declaration);
    return;
  }

  // Array rules
  if (isArray && !initializer) {
    if (type_modifier && type_modifier->dimensions.empty()) {
      errorHandler.addHint("Arrays need dimensions or an initializer")
          .addHint("Specify dimensions: arr[10] i32 x")
          .addHint("Or initialize: arr i32 x = [1, 2, 3]");
      logSemanticErrors("Array '" + declName +
                            "' missing dimensions and initializer",
                        declaration);
      return;
    }
  }

  // Create initial info I will collect it later
  declInfo->storage().isMutable = isMutable;
  declInfo->storage().isConstant = isConstant;
  declInfo->hasError = hasError;
  declInfo->storage().isVolatile = isVolatile;
  declInfo->storage().isRestrict = isRestrict;
  declInfo->storage().isPersist = isPersist;
  declInfo->type().isRef = isRef;
  declInfo->type().isPointer = isPointer;
  declInfo->storage().isHeap = isHeap;
  declInfo->type().isArray = isArray;
  declInfo->type().isNullable = isNullable;
}

void Semantics::handleNullInitializers(
    VariableDeclaration *declaration,
    const std::shared_ptr<SymbolInfo> &declInfo,
    const std::shared_ptr<SymbolInfo> &nullInfo) {
  const std::string &declName = declaration->var_name->expression.TokenLiteral;
  auto type_modifier =
      dynamic_cast<TypeModifier *>(declaration->modified_type.get());
  const auto &type = declInfo->type().type;

  if (!declInfo->type().isNullable) {
    errorHandler.addHint("Nullable types use '?' suffix: i32?")
        .addHint("Make the type nullable: " + type.resolvedName + "?")
        .addHint("Or initialize with a non-null value");
    logSemanticErrors("Cannot assign null to non-nullable variable '" +
                          declName + "'",
                      declaration);
    return;
  }

  if (declInfo->type().isArray) {
    if (type_modifier->dimensions.empty()) {
      errorHandler.addHint("Arrays need dimensions: arr[10] i32 x")
          .addHint("Or initialize with array literal: arr i32 x = [1, 2, 3]");
      logSemanticErrors("Cannot assign null to array without dimensions",
                        declaration);
    }
    return;
  }

  if (declInfo->type().isRef) {
    logSemanticErrors(
        "Cannot assign null to a reference variable declaration '" + declName +
            "' of type '" + type.resolvedName + "'",
        declaration);
  }

  // Give the null context
  nullInfo->type().type = declInfo->type().type;

  if (declInfo->storage().isHeap) {
    auto lifetime = createLifeTimeTracker(declaration, nullptr, declInfo);
    declInfo->codegen().ID = lifetime->ID;
    responsibilityTable[declaration] = std::move(lifetime);
  }
  declInfo->hasError = hasError;
  metaData[declaration] = declInfo;
  symbolTable.back()[declName] = declInfo;
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
    auto allocIt = allocatorMap.find(allocatorName);
    if (allocIt == allocatorMap.end()) {
      logSemanticErrors("Unknown allocator type '" + allocatorName + "'",
                        allocator);
      return;
    }

    allocType = allocIt->first;
  } else {
    allocType = "GPA"; // The default GPA type
  }
  declInfo->storage().allocType = allocType;

  auto declType = declInfo->type().type.resolvedName;

  if (initializer) {
    walker(initializer);
    auto initName = extractIdentifierName(initializer);
    auto initSym = getSymbolFromMeta(initializer);

    if (!initSym) {
      errorHandler.addHint("Check spelling or declaration order")
          .addHint("Variables must be declared before use");
      logSemanticErrors("Variable '" + initName + "' is undefined",
                        initializer);
      return;
    }

    // If the initializer is erronious dont proceed
    if (initSym->hasError)
      return;

    ResolvedType declaredType = inferNodeDataType(declaration);
    declInfo->type().type = declaredType;

    // Null checks and giving the null literals context
    if (dynamic_cast<NullLiteral *>(initializer)) {
      handleNullInitializers(declaration, declInfo, initSym);
      return;
    }

    // Give generic lits context
    giveGenericLiteralContext(initializer, declInfo, initSym);

    // Basic Type checks
    auto errorType = ResolvedType::error();

    logInternal("Declared variable type: " + declaredType.resolvedName);
    if (declaredType.kind == DataType::OPAQUE && !declInfo->type().isPointer) {
      errorHandler.addHint("'opaque' can only be used with pointer types")
          .addHint("Example: ptr opaque p")
          .addHint("Remove 'opaque' for non-pointer types");
      logSemanticErrors("'opaque' only applies to pointer declarations",
                        declaration);
      return;
    }

    // Type check
    if (initializer) {
      if (declaredType.kind == DataType::OPAQUE) {
        if (!initSym->type().type.isPointer()) {
          errorHandler
              .addHint("Opaque pointers can only point to other pointers")
              .addHint("Use: ptr opaque p -> addr x")
              .addHint("Make sure the target is a pointer type");
          logSemanticErrors(
              "Cannot initialize opaque pointer with non-pointer type",
              declaration);
        }
      } else if (!isTypeCompatible(declaredType, initSym->type().type)) {
        std::string errorMsg = "Type mismatch in variable declaration";
        errorHandler.addHint("Expected type: " + declaredType.resolvedName)
            .addHint("Got type: " + initSym->type().type.resolvedName)
            .addHint("Check if types match or convert using cast or bitcast");

        logSemanticErrors(errorMsg, declaration);
        return;
      }
    }

    //  Reference rules
    if (declInfo->type().isRef) {
      if (initSym->type().isNullable) {
        errorHandler.addHint("References cannot point to nullable values")
            .addHint("Use 'unwrap' or '?''?' to get the non-nullable value");
        logSemanticErrors("Reference cannot reference nullable value",
                          declaration);
        return;
      }

      if (!initSym->storage().isHeap) {
        errorHandler
            .addHint("References only work with heap or global variables")
            .addHint("Allocate on heap: heap i32 x = 10")
            .addHint("Or use global scope");
        logSemanticErrors("Cannot create reference to local variable '" +
                              initName + "'",
                          declaration);
        return;
      }

      if (declInfo->storage().isMutable && !initSym->storage().isMutable) {
        errorHandler.addHint("Mutable reference requires mutable target")
            .addHint("Make target mutable: mut i32 x = 10");
        logSemanticErrors(
            "Cannot create mutable reference to immutable variable",
            declaration);
      }

      declInfo->relations().refereeSymbol = initSym;
      initSym->storage().refCount += 1;
    }

    // Pointer rules
    if (declInfo->type().isPointer) {
      declInfo->relations().targetSymbol = initSym;
      initSym->storage().pointerCount += 1;
    }

    // Array rules
    if (declInfo->type().isArray && type_modifier &&
        !type_modifier->dimensions.empty()) {
      std::vector<uint64_t> declSizePerDim;
      for (const auto &len : type_modifier->dimensions) {
        walker(len.get());
        if (isIntegerConstant(len.get())) {
          declSizePerDim.push_back(getIntegerConstant(len.get()));
        } else {
          declSizePerDim.push_back(0);
          logInternal("Dynamic dimension for '" + declName + "'");
        }
      }

      // Check dimension mismatch
      if (!initSym->type().sizePerDimensions.empty()) {
        if (declSizePerDim.size() != initSym->type().sizePerDimensions.size()) {
          errorHandler
              .addHint("Expected " + std::to_string(declSizePerDim.size()) +
                       " dimensions")
              .addHint(
                  "Got " +
                  std::to_string(initSym->type().sizePerDimensions.size()) +
                  " dimensions");
          logSemanticErrors("Dimension count mismatch", declaration);
          return;
        }
        declSizePerDim = initSym->type().sizePerDimensions;
      }
      declInfo->type().sizePerDimensions = declSizePerDim;
    }
  }

  // Creating the baton
  if (declInfo->storage().isHeap) {
    LifeTime *targetBaton =
        initializer ? responsibilityTable[initializer].get() : nullptr;
    auto lifetime = createLifeTimeTracker(declaration, targetBaton, declInfo);
    declInfo->codegen().ID = lifetime->ID;
    responsibilityTable[declaration] = std::move(lifetime);
  }

  metaData[declaration] = declInfo;
  logInternal("[DEBUG] Stored metadata for " + declName +
              " | isHeap = " + std::to_string(declInfo->storage().isHeap) +
              " | ID = " + declInfo->codegen().ID);
  hasError = false;
  symbolTable.back()[declName] = declInfo;
}
