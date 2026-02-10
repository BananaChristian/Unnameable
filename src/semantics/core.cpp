#include "ast.hpp"
#include "semantics.hpp"
#include "token.hpp"
#include <cstdint>
#include <memory>
#include <string>
#include <unordered_set>

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"

Semantics::Semantics(Deserializer &deserial, ErrorHandler &handler,
                     bool verbose)
    : deserializer(deserial), errorHandler(handler), verbose(verbose) {
  symbolTable.push_back({});
  registerWalkerFunctions();

  import();
  registerInbuiltAllocatorTypes(); // Register the inbuilt allocators(malloc for
                                   // now)
}

// Main walker function
void Semantics::walker(Node *node) {
  if (!node)
    return;

  logInternal("Analyzing AST node: " + node->toString());

  std::string nodeName = typeid(*node).name();
  logInternal("Type at runtime: " + nodeName);
  auto walkerIt = walkerFunctionsMap.find(typeid(*node));

  if (walkerIt != walkerFunctionsMap.end()) {
    (this->*walkerIt->second)(node);
  } else {
    logInternal("Failed to find analyzer for :" + node->toString());
    logInternal("Actual runtime type: " + nodeName);
    return;
  }
}

// HELPER FUNCTIONS
void Semantics::registerWalkerFunctions() {
  // Walker registration for the native data type literals
  walkerFunctionsMap[typeid(I8Literal)] = &Semantics::walkI8Literal;
  walkerFunctionsMap[typeid(U8Literal)] = &Semantics::walkU8Literal;
  walkerFunctionsMap[typeid(I16Literal)] = &Semantics::walkI16Literal;
  walkerFunctionsMap[typeid(U16Literal)] = &Semantics::walkU16Literal;
  walkerFunctionsMap[typeid(I32Literal)] = &Semantics::walkI32Literal;
  walkerFunctionsMap[typeid(U32Literal)] = &Semantics::walkU32Literal;
  walkerFunctionsMap[typeid(I64Literal)] = &Semantics::walkI64Literal;
  walkerFunctionsMap[typeid(U64Literal)] = &Semantics::walkU64Literal;
  walkerFunctionsMap[typeid(I128Literal)] = &Semantics::walkI128Literal;
  walkerFunctionsMap[typeid(U128Literal)] = &Semantics::walkU128Literal;
  walkerFunctionsMap[typeid(ISIZELiteral)] = &Semantics::walkISIZELiteral;
  walkerFunctionsMap[typeid(USIZELiteral)] = &Semantics::walkUSIZELiteral;
  walkerFunctionsMap[typeid(F32Literal)] = &Semantics::walkF32Literal;
  walkerFunctionsMap[typeid(F64Literal)] = &Semantics::walkF64Literal;
  walkerFunctionsMap[typeid(StringLiteral)] = &Semantics::walkStringLiteral;

  walkerFunctionsMap[typeid(Char8Literal)] = &Semantics::walkChar8Literal;
  walkerFunctionsMap[typeid(Char16Literal)] = &Semantics::walkChar16Literal;
  walkerFunctionsMap[typeid(Char32Literal)] = &Semantics::walkChar32Literal;

  walkerFunctionsMap[typeid(BooleanLiteral)] = &Semantics::walkBooleanLiteral;
  walkerFunctionsMap[typeid(Identifier)] = &Semantics::walkIdentifierExpression;
  walkerFunctionsMap[typeid(AddressExpression)] =
      &Semantics::walkAddressExpression;
  walkerFunctionsMap[typeid(MoveExpression)] = &Semantics::walkMoveExpression;
  walkerFunctionsMap[typeid(DereferenceExpression)] =
      &Semantics::walkDereferenceExpression;
  walkerFunctionsMap[typeid(SizeOfExpression)] =
      &Semantics::walkSizeOfExpression;

  walkerFunctionsMap[typeid(NullLiteral)] = &Semantics::walkNullLiteral;
  walkerFunctionsMap[typeid(CastExpression)] = &Semantics::walkCastExpression;
  walkerFunctionsMap[typeid(BitcastExpression)] =
      &Semantics::walkBitcastExpression;

  // Walker registration for array walker
  walkerFunctionsMap[typeid(ArrayStatement)] = &Semantics::walkArrayStatement;
  walkerFunctionsMap[typeid(ArrayLiteral)] = &Semantics::walkArrayLiteral;
  walkerFunctionsMap[typeid(ArraySubscript)] =
      &Semantics::walkArraySubscriptExpression;

  // Walker registration for let statement and assignment statements
  walkerFunctionsMap[typeid(LetStatement)] = &Semantics::walkLetStatement;
  walkerFunctionsMap[typeid(AssignmentStatement)] =
      &Semantics::walkAssignStatement;
  walkerFunctionsMap[typeid(FieldAssignment)] =
      &Semantics::walkFieldAssignmentStatement;

  // Walker registration for reference statement and pointer statement
  walkerFunctionsMap[typeid(ReferenceStatement)] =
      &Semantics::walkReferenceStatement;
  walkerFunctionsMap[typeid(PointerStatement)] =
      &Semantics::walkPointerStatement;

  // Walker registration for control flow
  walkerFunctionsMap[typeid(ifStatement)] = &Semantics::walkIfStatement;
  walkerFunctionsMap[typeid(SwitchStatement)] = &Semantics::walkSwitchStatement;

  // Loop disruption statements
  walkerFunctionsMap[typeid(BreakStatement)] = &Semantics::walkBreakStatement;
  walkerFunctionsMap[typeid(ContinueStatement)] =
      &Semantics::walkContinueStatement;

  // Walker registration for functions
  walkerFunctionsMap[typeid(FunctionStatement)] =
      &Semantics::walkFunctionStatement;
  walkerFunctionsMap[typeid(FunctionExpression)] =
      &Semantics::walkFunctionExpression;
  walkerFunctionsMap[typeid(FunctionDeclaration)] =
      &Semantics::walkFunctionDeclarationStatement;
  walkerFunctionsMap[typeid(FunctionDeclarationExpression)] =
      &Semantics::walkFunctionDeclarationExpression;
  walkerFunctionsMap[typeid(CallExpression)] =
      &Semantics::walkFunctionCallExpression;
  walkerFunctionsMap[typeid(UnwrapExpression)] =
      &Semantics::walkUnwrapExpression;
  walkerFunctionsMap[typeid(ReturnStatement)] = &Semantics::walkReturnStatement;

  // Walker registration for type expressions
  walkerFunctionsMap[typeid(BasicType)] = &Semantics::walkBasicType;
  walkerFunctionsMap[typeid(ArrayType)] = &Semantics::walkArrayType;

  // Walker registration for loops
  walkerFunctionsMap[typeid(WhileStatement)] = &Semantics::walkWhileStatement;
  walkerFunctionsMap[typeid(ForStatement)] = &Semantics::walkForStatement;

  // Walker registration for blocks
  walkerFunctionsMap[typeid(BlockStatement)] = &Semantics::walkBlockStatement;
  walkerFunctionsMap[typeid(BlockExpression)] = &Semantics::walkBlockExpression;

  // Walker registration for the main expression types
  walkerFunctionsMap[typeid(InfixExpression)] = &Semantics::walkInfixExpression;
  walkerFunctionsMap[typeid(PrefixExpression)] =
      &Semantics::walkPrefixExpression;
  walkerFunctionsMap[typeid(PostfixExpression)] =
      &Semantics::walkPostfixExpression;

  walkerFunctionsMap[typeid(ExpressionStatement)] =
      &Semantics::walkExpressionStatement;

  // Walker registration for the component system
  walkerFunctionsMap[typeid(RecordStatement)] = &Semantics::walkRecordStatement;
  walkerFunctionsMap[typeid(ComponentStatement)] =
      &Semantics::walkComponentStatement;
  walkerFunctionsMap[typeid(NewComponentExpression)] =
      &Semantics::walkNewComponentExpression;
  walkerFunctionsMap[typeid(SelfExpression)] = &Semantics::walkSelfExpression;
  walkerFunctionsMap[typeid(EnumStatement)] = &Semantics::walkEnumStatement;
  walkerFunctionsMap[typeid(InstanceExpression)] =
      &Semantics::walkInstanceExpression;
  walkerFunctionsMap[typeid(MethodCallExpression)] =
      &Semantics::walkMethodCallExpression;

  walkerFunctionsMap[typeid(AllocatorStatement)] =
      &Semantics::walkAllocatorInterface;
  walkerFunctionsMap[typeid(HeapStatement)] = &Semantics::walkHeapStatement;
  walkerFunctionsMap[typeid(SealStatement)] = &Semantics::walkSealStatement;

  // Walker registration for the trace system
  walkerFunctionsMap[typeid(TraceStatement)] = &Semantics::walkTraceStatement;
  // Walker registrartion for generic system
  walkerFunctionsMap[typeid(GenericStatement)] =
      &Semantics::walkGenericStatement;
  walkerFunctionsMap[typeid(InstantiateStatement)] =
      &Semantics::walkInstantiateStatement;
}

ResolvedType Semantics::inferNodeDataType(Node *node) {
  if (!node)
    return ResolvedType{DataType::UNKNOWN, "unknown"};

  if (auto i8Lit = dynamic_cast<I8Literal *>(node))
    return ResolvedType{DataType::I8, "i8"};
  if (auto u8Lit = dynamic_cast<U8Literal *>(node))
    return ResolvedType{DataType::U8, "u8"};
  if (auto i16Lit = dynamic_cast<I16Literal *>(node))
    return ResolvedType{DataType::I16, "i16"};
  if (auto u16Lit = dynamic_cast<U16Literal *>(node))
    return ResolvedType{DataType::U16, "u16"};
  if (auto i32Lit = dynamic_cast<I32Literal *>(node))
    return ResolvedType{DataType::I32, "i32"};
  if (auto u32Lit = dynamic_cast<U32Literal *>(node))
    return ResolvedType{DataType::U32, "u32"};
  if (auto i64Lit = dynamic_cast<I64Literal *>(node))
    return ResolvedType{DataType::I64, "i64"};
  if (auto u64Lit = dynamic_cast<U64Literal *>(node))
    return ResolvedType{DataType::U64, "u64"};
  if (auto i128Lit = dynamic_cast<I128Literal *>(node))
    return ResolvedType{DataType::I128, "i128"};
  if (auto u128Lit = dynamic_cast<U128Literal *>(node))
    return ResolvedType{DataType::U128, "u128"};
  if (auto isizeLit = dynamic_cast<ISIZELiteral *>(node))
    return ResolvedType{DataType::ISIZE, "isize"};
  if (auto usizeLit = dynamic_cast<USIZELiteral *>(node))
    return ResolvedType{DataType::USIZE, "usize"};

  if (auto f32Lit = dynamic_cast<F32Literal *>(node))
    return ResolvedType{DataType::F32, "f32"};
  if (auto f64Lit = dynamic_cast<F64Literal *>(node))
    return ResolvedType{DataType::F64, "f64"};

  if (auto strLit = dynamic_cast<StringLiteral *>(node))
    return ResolvedType{DataType::STRING, "string"};

  if (auto chrLit = dynamic_cast<Char8Literal *>(node))
    return ResolvedType{DataType::CHAR8, "char8"};
  if (auto char16Lit = dynamic_cast<Char16Literal *>(node))
    return ResolvedType{DataType::CHAR16, "char16"};
  if (auto char32Lit = dynamic_cast<Char32Literal *>(node))
    return ResolvedType{DataType::CHAR32, "char32"};

  if (auto boolLit = dynamic_cast<BooleanLiteral *>(node))
    return ResolvedType{DataType::BOOLEAN, "bool"};

  if (auto sizeOfExpr = dynamic_cast<SizeOfExpression *>(node))
    return ResolvedType{DataType::USIZE, "usize"};

  if (auto castExpr = dynamic_cast<CastExpression *>(node)) {
    auto type = castExpr->type.get();
    if (type) {
      auto destinationType = inferNodeDataType(type);
      return destinationType;
    }
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }

  if (auto bitcastExpr = dynamic_cast<BitcastExpression *>(node)) {
    auto type = bitcastExpr->type.get();
    if (type) {
      auto destinationType = inferNodeDataType(type);
      return destinationType;
    }

    return ResolvedType{DataType::ERROR, "error"};
  }

  if (auto arrLit = dynamic_cast<ArrayLiteral *>(node)) {
    if (arrLit->array.empty()) {
      ResolvedType empty;
      empty.isArray = true;
      empty.resolvedName = "[error]";
      empty.kind = DataType::ERROR;
      empty.isNull = false;
      empty.isPointer = false;
      empty.isRef = false;

      return empty;
    }

    // Infer type of first element
    ResolvedType firstType = inferNodeDataType(arrLit->array[0].get());

    // Now verify compatibility of all elements
    for (size_t i = 1; i < arrLit->array.size(); ++i) {
      ResolvedType elemType = inferNodeDataType(arrLit->array[i].get());

      if (!isTypeCompatible(firstType, elemType)) {
        logSemanticErrors("Type mismatch of array member at index '" +
                              std::to_string(i) + "' expected '" +
                              firstType.resolvedName + "' but got '" +
                              elemType.resolvedName + "'",
                          arrLit->expression.line, arrLit->expression.column);

        return ResolvedType{DataType::ERROR, "error", false,
                            false,           false,   false};
      }
    }

    // Build the final array type
    ResolvedType finalType;
    finalType.kind = firstType.kind;
    finalType.isArray = true;
    finalType.isPointer = firstType.isPointer;
    finalType.isRef = firstType.isRef;

    if (firstType.isArray) {
      finalType.resolvedName = firstType.resolvedName;
    } else {
      finalType.resolvedName = "arr[" + firstType.resolvedName + "]";
    }

    return finalType;
  }

  if (auto arrAccess = dynamic_cast<ArraySubscript *>(node)) {
    // Extract the name of the array
    auto arrayName = arrAccess->identifier->expression.TokenLiteral;
    auto line = arrAccess->expression.line;
    auto col = arrAccess->expression.column;

    // Check for its symbolInfo
    auto arrSym = resolveSymbolInfo(arrayName);
    if (!arrSym) {
      logSemanticErrors("Unidentified variable '" + arrayName + "'", line, col);
      return ResolvedType{DataType::ERROR, "error"};
    }

    if (!arrSym->type.isArray) {
      logSemanticErrors("Cannot index into non array type '" +
                            arrSym->type.resolvedName + "'",
                        line, col);
      return ResolvedType{DataType::ERROR, "error"};
    }

    if (arrSym->type.isNull) {
      logSemanticErrors("Cannot index into a nullable array '" + arrayName +
                            "' of type '" + arrSym->type.resolvedName + "'",
                        line, col);
      return ResolvedType{DataType::ERROR, "error"};
    }

    return arrSym->type;
  }

  if (auto selfExpr = dynamic_cast<SelfExpression *>(node)) {
    // Start: find the type of the component containing this method
    if (currentTypeStack.empty() ||
        currentTypeStack.back().type.kind != DataType::COMPONENT)
      return ResolvedType{DataType::UNKNOWN, "unknown"};

    std::string currentTypeName = currentTypeStack.back().typeName;

    // Walk through the chain of fields: pos -> x -> y
    for (const auto &field : selfExpr->fields) {
      auto ident = dynamic_cast<Identifier *>(field.get());
      if (!ident)
        return ResolvedType{DataType::UNKNOWN, "unknown"};

      const std::string fieldName = ident->identifier.TokenLiteral;

      // Look up this type's custom definition
      auto ctIt = customTypesTable.find(currentTypeName);
      if (ctIt == customTypesTable.end())
        return ResolvedType{DataType::UNKNOWN, "unknown"};

      // Find member in this component
      auto &members = ctIt->second->members;
      auto memIt = members.find(fieldName);
      if (memIt == members.end())
        return ResolvedType{DataType::UNKNOWN, "unknown"};

      // Get the member type
      const auto &memberInfo = memIt->second;
      currentTypeName = memberInfo->type.resolvedName; // move deeper

      // Last field: return its full type
      if (&field == &selfExpr->fields.back())
        return memberInfo->type;
    }

    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }

  if (auto instExpr = dynamic_cast<InstanceExpression *>(node)) {
    auto instName = instExpr->blockIdent->expression.TokenLiteral;
    auto line = instExpr->blockIdent->expression.line;
    auto col = instExpr->blockIdent->expression.column;
    auto sym = resolveSymbolInfo(instName);
    if (!sym) {
      logSemanticErrors("Failed to infer type for unidentified identifier '" +
                            instName + "'",
                        line, col);
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    return sym->type;
  }

  // Dealing with the let statement node type
  if (auto letStmt = dynamic_cast<LetStatement *>(node)) {
    auto type = dynamic_cast<BasicType *>(letStmt->type.get());
    auto letStmtDataType = type->data_token;
    return resolvedDataType(letStmtDataType, letStmt);
  }

  if (auto assignStmt = dynamic_cast<AssignmentStatement *>(node)) {
    std::string nameToResolve;

    if (auto selfExpr =
            dynamic_cast<SelfExpression *>(assignStmt->identifier.get())) {
      if (selfExpr->fields.empty()) {
        logSemanticErrors("Invalid 'self' access in assignment",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        return ResolvedType{DataType::UNKNOWN, "unknown"};
      }

      // Grab the LAST field in the chain
      auto lastField = selfExpr->fields.back().get();
      auto ident = dynamic_cast<Identifier *>(lastField);
      if (!ident) {
        logSemanticErrors("Expected identifier in 'self' field chain",
                          assignStmt->identifier->expression.line,
                          assignStmt->identifier->expression.column);
        return ResolvedType{DataType::UNKNOWN, "unknown"};
      }

      nameToResolve = ident->identifier.TokenLiteral;
    } else {
      nameToResolve = assignStmt->identifier->expression.TokenLiteral;
    }
    logInternal("Name being used to resolve inside inferer '" + nameToResolve +
                "'");
    auto assignSymbol = resolveSymbolInfo(nameToResolve);
    auto assignStmtVal = assignStmt->value.get();
    ResolvedType assignStmtValType = inferNodeDataType(assignStmtVal);
    if (!isTypeCompatible(assignSymbol->type, assignStmtValType)) {
      logSemanticErrors("Type mismatch expected '" +
                            assignStmtValType.resolvedName + "' but got '" +
                            assignSymbol->type.resolvedName + "'",
                        assignStmt->identifier->expression.line,
                        assignStmt->identifier->expression.column);
    } else {
      return assignSymbol->type;
    }
  }

  if (auto newExpr = dynamic_cast<NewComponentExpression *>(node)) {
    auto componentName = newExpr->component_name.TokenLiteral;
    int line = newExpr->expression.line;
    int column = newExpr->expression.column;
    auto componentIt = customTypesTable.find(componentName);
    if (componentIt == customTypesTable.end()) {
      logSemanticErrors("Component '" + componentName + "' does not exist",
                        line, column);
      return ResolvedType{DataType::ERROR, "error"};
    }
    return componentIt->second->type;
  }

  if (auto infixExpr = dynamic_cast<InfixExpression *>(node)) {
    return inferInfixExpressionType(infixExpr);
  }

  if (auto prefixExpr = dynamic_cast<PrefixExpression *>(node)) {
    return inferPrefixExpressionType(prefixExpr);
  }

  if (auto postfixExpr = dynamic_cast<PostfixExpression *>(node)) {
    return inferPostfixExpressionType(postfixExpr);
  }

  if (auto nullLit = dynamic_cast<NullLiteral *>(node))
    return {DataType::UNKNOWN, "null"}; // Will be updated based on context

  if (auto ident = dynamic_cast<Identifier *>(node)) {
    std::string name = ident->identifier.TokenLiteral;
    logInternal("Identifier name in the inferer '" + name + "'");
    auto symbol = resolveSymbolInfo(name);
    if (symbol) {
      logInternal("Identifier Data Type '" + symbol->type.resolvedName + "'");
      return symbol->type;
    } else {
      logSemanticErrors("Undefined variable '" + name + "'",
                        ident->expression.line, ident->expression.column);
      return ResolvedType{DataType::ERROR, "error"};
    }
  }

  if (auto derefExpr = dynamic_cast<DereferenceExpression *>(node)) {
    std::string name = extractIdentifierName(derefExpr->identifier.get());
    logInternal("Derefence name given during type resolution");
    auto derefSym = resolveSymbolInfo(name);

    if (!derefSym) {
      logSemanticErrors("Undefined variable '" + name + "'",
                        derefExpr->identifier->expression.line,
                        derefExpr->identifier->expression.column);
      return ResolvedType{DataType::ERROR, "error"};
    }
    // Get the ptr_type
    auto ptrType = derefSym->type;
    // Toggle the isPointer boolean
    ptrType.isPointer = false;
    return isPointerType(ptrType);
  }
  if (auto moveExpr = dynamic_cast<MoveExpression *>(node)) {
    auto innerExpr = moveExpr->expr.get();
    auto type = inferNodeDataType(innerExpr);

    return type;
  }

  if (auto addrExpr = dynamic_cast<AddressExpression *>(node)) {
    auto innerExpr = addrExpr->identifier.get();
    auto identType = inferNodeDataType(innerExpr);
    identType.isPointer = true;
    return isPointerType(identType);
  }

  if (auto basicType = dynamic_cast<BasicType *>(node))
    return tokenTypeToResolvedType(basicType->data_token,
                                   basicType->isNullable);

  if (auto arrRetType = dynamic_cast<ArrayType *>(node)) {
    // Resolve the inner type first like if I have arr[i32] the inner type is
    // i32
    ResolvedType inner = inferNodeDataType(arrRetType->innerType.get());
    // Block void
    if (inner.kind == DataType::VOID) {
      logSemanticErrors("Cannot have a void array type",
                        arrRetType->expression.line,
                        arrRetType->expression.column);
      return ResolvedType{DataType::ERROR, "error", false, false, false, false};
    }

    // Block the inner type from being nullable for example i32?
    if (inner.isNull) {
      logSemanticErrors(
          "The inner type of an array cannot be a nullable type '" +
              inner.resolvedName + "'",
          arrRetType->expression.line, arrRetType->expression.column);
      return ResolvedType{DataType::ERROR, "error", false, false, false, false};
    }

    bool isNullType = false;
    if (arrRetType->isNullable) {
      isNullType = true;
    }

    std::string typeName = "arr[" + inner.resolvedName + "]";
    inner.isArray = true;
    inner.isNull = isNullType;
    inner.resolvedName = typeName;

    return inner;
  }

  if (auto ptrType = dynamic_cast<PointerType *>(node)) {
    ResolvedType inner = inferNodeDataType(ptrType->underlyingType.get());
    if (inner.kind == DataType::VOID) {
      logSemanticErrors("Cannot have a void pointer return type",
                        ptrType->expression.line, ptrType->expression.column);
      return ResolvedType{DataType::ERROR, "error", false, false};
    }
    inner.isPointer = true;
    return isPointerType(inner);
  }

  if (auto refType = dynamic_cast<RefType *>(node)) {
    ResolvedType inner = inferNodeDataType(refType->underLyingType.get());
    if (inner.kind == DataType::VOID) {
      logSemanticErrors("Cannot have a void reference return type",
                        refType->expression.line, refType->expression.column);
      return ResolvedType{DataType::ERROR, "error", false, false, false, false};
      ;
    }
    inner.isRef = true;
    return isRefType(inner);
  }

  if (auto retTypeExpr = dynamic_cast<ReturnType *>(node)) {
    // Just recursively call the inferer for whatever is being nested in
    // here(Basic Type or whatever)
    return inferNodeDataType(retTypeExpr->returnExpr.get());
  }

  if (auto callExpr = dynamic_cast<CallExpression *>(node)) {
    auto symbol = resolveSymbolInfo(
        callExpr->function_identifier->expression.TokenLiteral);
    if (symbol) {
      return symbol->type;
    } else {
      logSemanticErrors(
          "Undefined function name '" +
              callExpr->function_identifier->expression.TokenLiteral + "'",
          callExpr->function_identifier->expression.line,
          callExpr->function_identifier->expression.column);
      return ResolvedType{DataType::ERROR, "error"};
    }
  }

  if (auto metCall = dynamic_cast<MethodCallExpression *>(node)) {
    auto instanceName = metCall->instance->expression.TokenLiteral;
    auto line = metCall->instance->expression.line;
    auto col = metCall->instance->expression.column;

    auto instanceSym = resolveSymbolInfo(instanceName);

    if (!instanceSym) {
      logSemanticErrors("Undefined instance name '" + instanceName + "' ", line,
                        col);
      return ResolvedType{DataType::ERROR, "error"};
    }

    auto call = dynamic_cast<CallExpression *>(metCall->call.get());
    auto callName = call->function_identifier->expression.TokenLiteral;
    auto callLine = call->function_identifier->expression.line;
    auto callCol = call->function_identifier->expression.column;

    // Search using custom types table to get the members
    // Get the type
    auto type = instanceSym->type;
    // Check the customTypes table
    auto typeIt = customTypesTable.find(type.resolvedName);
    auto sealIt = sealTable.find(instanceName);
    if (typeIt != customTypesTable.end()) {
      auto members = typeIt->second->members;
      auto it = members.find(callName);
      if (it == members.end()) {
        logSemanticErrors("Function '" + callName + "' doesnt exist in type '" +
                              type.resolvedName + "'",
                          line, col);
        return ResolvedType{DataType::ERROR, "error"};
      }

      auto memInfo = it->second;
      return memInfo->type;
    } else if (sealIt != sealTable.end()) {
      auto sealFnMap = sealIt->second;
      auto sealFnIt = sealFnMap.find(callName);
      if (sealFnIt == sealFnMap.end()) {
        logSemanticErrors("Function '" + callName + "' doesnt exist in seal '" +
                              instanceName + "'",
                          line, col);
        return ResolvedType{DataType::ERROR, "error"};
      }

      auto sealInfo = sealFnIt->second;
      return sealInfo->type;
    } else {
      logSemanticErrors("Unknown type or seal '" + instanceName + "'", line,
                        col);
      return ResolvedType{DataType::ERROR, "error"};
    }
  }
  if (auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node)) {
    auto line = unwrapExpr->expression.line;
    auto col = unwrapExpr->expression.column;

    auto exprType = inferNodeDataType(unwrapExpr->expr.get());
    exprType.isNull = false;
    auto strippedName = stripOptionalSuffix(exprType.resolvedName);
    exprType.resolvedName = strippedName;

    return exprType;
  }

  if (auto ptrStmt = dynamic_cast<PointerStatement *>(node)) {
    ResolvedType ptrType = inferNodeDataType(ptrStmt->type.get());
    // Update the is pointer flag to true
    ptrType.isPointer = true;
    return isPointerType(ptrType);
  }

  if (auto refStmt = dynamic_cast<ReferenceStatement *>(node)) {
    ResolvedType refType = inferNodeDataType(refStmt->type.get());
    // Update the reference flag to true
    refType.isRef = true;
    return isRefType(refType);
  }

  return {DataType::UNKNOWN, "unknown"};
}

std::string Semantics::extractDeclarationName(Node *node) {
  std::string declName = "<Unknown name>";
  if (auto letStmt = dynamic_cast<LetStatement *>(node)) {
    declName = letStmt->ident_token.TokenLiteral;
  } else if (auto ptrStmt = dynamic_cast<PointerStatement *>(node)) {
    declName = ptrStmt->name->expression.TokenLiteral;
  } else if (auto refStmt = dynamic_cast<ReferenceStatement *>(node)) {
    declName = refStmt->name->expression.TokenLiteral;
  } else if (auto arrStmt = dynamic_cast<ArrayStatement *>(node)) {
    declName = arrStmt->identifier->expression.TokenLiteral;
  }

  return declName;
}

std::string Semantics::extractIdentifierName(Node *node) {
  std::string identName = "<Unsupported node>";
  if (auto call = dynamic_cast<CallExpression *>(node)) {
    auto callIdent =
        dynamic_cast<Identifier *>(call->function_identifier.get());
    identName = callIdent->identifier.TokenLiteral;
    return identName;
  } else if (auto ident = dynamic_cast<Identifier *>(node)) {
    identName = ident->identifier.TokenLiteral;
    return identName;
  } else if (auto metCall = dynamic_cast<MethodCallExpression *>(node)) {
    identName = "<methodfuncName>"; // Place holder for now
    return identName;
  } else if (auto deref = dynamic_cast<DereferenceExpression *>(node)) {
    identName = extractIdentifierName(deref->identifier.get());
    return identName;
  } else if (auto addr = dynamic_cast<AddressExpression *>(node)) {
    identName = extractIdentifierName(addr->identifier.get());
    return identName;
  } else if (auto infix = dynamic_cast<InfixExpression *>(node)) {
    identName = extractIdentifierName(infix->right_operand.get());
    return identName;
  } else if (auto selfExpr = dynamic_cast<SelfExpression *>(node)) {
    if (!selfExpr->fields.empty()) {
      // Get the last field in the chain (e.g., self.a.b.target -> target)
      identName = extractIdentifierName(selfExpr->fields.back().get());
    } else {
      identName = "self"; // Just the component itself
    }
    return identName;
  } else if (auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node)) {
    identName = extractIdentifierName(unwrapExpr->expr.get());
    return identName;
  }

  return identName;
}

ResolvedType Semantics::inferInfixExpressionType(Node *node) {
  auto infixNode = dynamic_cast<InfixExpression *>(node);
  if (!infixNode)
    return {DataType::UNKNOWN, "unknown"};

  // Incase the left side is an identifier
  auto ident = dynamic_cast<Identifier *>(infixNode->left_operand.get());
  std::shared_ptr<SymbolInfo> symbol = nullptr;
  if (ident) {
    symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
  }

  TokenType operatorType = infixNode->operat.type;

  // Special case: scope or dot
  if (operatorType == TokenType::FULLSTOP ||
      operatorType == TokenType::SCOPE_OPERATOR) {
    auto parentType = inferNodeDataType(infixNode->left_operand.get());
    std::string childName = infixNode->right_operand->expression.TokenLiteral;
    auto result =
        resultOfScopeOrDot(operatorType, parentType, childName, infixNode);
    if (result) {
      return result->type;
    } else {
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }
  }

  // --- Regular binary operator ---
  ResolvedType leftType = inferNodeDataType(infixNode->left_operand.get());
  ResolvedType rightType = inferNodeDataType(infixNode->right_operand.get());

  if (operatorType == TokenType::COALESCE) {

    if (!leftType.isNull) {
      logSemanticErrors("Left-hand side of coalesce must be nullable",
                        infixNode->left_operand->expression.line,
                        infixNode->left_operand->expression.column);
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    // Right-hand side type
    rightType = inferNodeDataType(infixNode->right_operand.get());
    ResolvedType baseType = leftType;
    baseType.isNull = false;
    auto strippedName = stripOptionalSuffix(baseType.resolvedName);
    baseType.resolvedName = strippedName;

    if (!isTypeCompatible(baseType, rightType)) {
      logSemanticErrors(
          "Type of fallback in coalesce does not match nullable type '" +
              baseType.resolvedName + "' must match '" +
              rightType.resolvedName + "'",
          ident->expression.line, ident->expression.column);
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    // Result is the underlying type
    return baseType;
  }

  if (ident) {
    auto symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
    if (!symbol) {
      logSemanticErrors("Undefined variable '" +
                            ident->identifier.TokenLiteral + "'",
                        ident->expression.line, ident->expression.column);
      return {DataType::UNKNOWN, "unknown"};
    }
    if (!symbol->isInitialized) {
      logSemanticErrors("Cannot use an uninitialized variable '" +
                            ident->identifier.TokenLiteral + "' in operations",
                        ident->expression.line, ident->expression.column);
      symbol->hasError = true;
      return {DataType::UNKNOWN, "unknown"};
    }
    if (symbol->isDefinitelyNull) {
      logSemanticErrors("Cannot use definitely-null variable '" +
                            ident->identifier.TokenLiteral + "' in operations",
                        ident->expression.line, ident->expression.column);
      symbol->hasError = true;
      return {DataType::UNKNOWN, "unknown"};
    }
  }

  ResolvedType peeledLeft = peelRef(leftType);
  ResolvedType peeledRight = peelRef(rightType);

  auto infixType =
      resultOfBinary(operatorType, peeledLeft, peeledRight, infixNode);
  return infixType; // Return the correct type
}

ResolvedType Semantics::inferPrefixExpressionType(Node *node) {
  auto prefixNode = dynamic_cast<PrefixExpression *>(node);
  if (!prefixNode)
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  auto prefixOperator = prefixNode->operat.type;
  ResolvedType operandType = inferNodeDataType(prefixNode->operand.get());
  return resultOfUnary(prefixOperator, operandType, prefixNode);
}

ResolvedType Semantics::inferPostfixExpressionType(Node *node) {
  auto postfixNode = dynamic_cast<PostfixExpression *>(node);
  if (!postfixNode)
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  ResolvedType operandType = inferNodeDataType(postfixNode->operand.get());
  auto postfixOperator = postfixNode->operator_token.type;
  return resultOfUnary(postfixOperator, operandType, postfixNode);
}

std::shared_ptr<SymbolInfo> Semantics::resultOfScopeOrDot(
    TokenType operatorType, const ResolvedType &parentType,
    const std::string &childName, InfixExpression *infixExpr) {
  if (operatorType != TokenType::FULLSTOP &&
      operatorType != TokenType::SCOPE_OPERATOR)
    return nullptr;

  // If the type is a pointer strip the name and allow for access
  std::string lookUpName = parentType.resolvedName;

  if (parentType.isPointer) {
    lookUpName = stripPtrSuffix(lookUpName);
  } else if (parentType.isRef) {
    lookUpName = stripRefSuffix(lookUpName);
  }

  // Block nullable access
  if (parentType.isNull) {
    logSemanticErrors("Cannot access a nullable type '" + lookUpName + "'",
                      infixExpr->left_operand->expression.line,
                      infixExpr->left_operand->expression.column);
    return nullptr;
  }

  if (operatorType == TokenType::FULLSTOP) {
    if (parentType.kind == DataType::ENUM) {
      logSemanticErrors("Dot operator applied to enum use (::) to access",
                        infixExpr->left_operand->expression.line,
                        infixExpr->left_operand->expression.column);
      return nullptr;
    }

    // Look up the type definition in customTypesTable
    auto typeIt = customTypesTable.find(lookUpName);
    if (typeIt == customTypesTable.end()) {
      logSemanticErrors("Type '" + lookUpName + "' not found",
                        infixExpr->left_operand->expression.line,
                        infixExpr->left_operand->expression.column);
      return nullptr;
    }

    // Look for childName in members
    auto memberIt = typeIt->second->members.find(childName);
    if (memberIt == typeIt->second->members.end()) {
      logSemanticErrors("Type '" + lookUpName + "' has no member '" +
                            childName + "'",
                        infixExpr->right_operand->expression.line,
                        infixExpr->right_operand->expression.column);
      return nullptr;
    }

    auto memberInfo = memberIt->second;
    auto dotResult = std::make_shared<SymbolInfo>();
    dotResult->type = memberInfo->type;
    dotResult->isConstant = memberInfo->isConstant;
    dotResult->isMutable = memberInfo->isMutable;
    dotResult->isInitialized = memberInfo->isInitialised;
    dotResult->isNullable = memberInfo->isNullable;
    dotResult->isPointer = memberInfo->isPointer;
    dotResult->memberIndex = memberInfo->memberIndex;

    return dotResult;

  } else if (operatorType == TokenType::SCOPE_OPERATOR) {
    if (parentType.kind != DataType::ENUM) {
      logSemanticErrors("Scope operator(::) applied to none enum  variable",
                        infixExpr->left_operand->expression.line,
                        infixExpr->left_operand->expression.column);
      return nullptr;
    }

    // Look for the definition in the custom types table
    auto typeIt = customTypesTable.find(lookUpName);
    if (typeIt == customTypesTable.end()) {
      logSemanticErrors("Type '" + lookUpName + "' not found",
                        infixExpr->left_operand->expression.line,
                        infixExpr->left_operand->expression.column);
      return nullptr;
    }

    // Look for the childName in members
    auto memIt = typeIt->second->members.find(childName);
    if (memIt == typeIt->second->members.end()) {
      logSemanticErrors("Type '" + lookUpName + "' does not have a member '" +
                            childName + "'",
                        infixExpr->left_operand->expression.line,
                        infixExpr->left_operand->expression.column);
      return nullptr;
    }

    auto memInfo = memIt->second;
    auto scopeInfo = std::make_shared<SymbolInfo>();
    scopeInfo->type = memInfo->parentType; // This is the actual enum
    scopeInfo->isConstant = memInfo->isConstant;
    scopeInfo->isMutable = memInfo->isMutable;
    scopeInfo->isNullable = memInfo->isNullable;
    scopeInfo->memberIndex = memInfo->memberIndex;

    return scopeInfo;
  }

  return nullptr;
}

ResolvedType Semantics::resultOfBinary(TokenType operatorType,
                                       ResolvedType leftType,
                                       ResolvedType rightType, Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  auto leftLine = infix->left_operand->expression.line;
  auto leftCol = infix->left_operand->expression.column;
  std::string operatorStr = infix->operat.TokenLiteral;

  // Logical operators: &&, ||
  if (operatorType == TokenType::AND || operatorType == TokenType::OR) {
    if (isBoolean(leftType) && isBoolean(rightType))
      return ResolvedType{DataType::BOOLEAN, "boolean"};
    else
      return ResolvedType{DataType::UNKNOWN, "unknown"};
  }

  // The odds of this happening are low unless the parser messed up
  if (operatorType == TokenType::ASSIGN) {
    logSemanticErrors(
        "Cannot use '" + operatorStr +
            "'in binary operations. It is reserved for assignments",
        leftLine, leftCol);
    return ResolvedType{DataType::ERROR, "error"};
  }

  // Comparison operators
  bool isComparison = (operatorType == TokenType::GREATER_THAN ||
                       operatorType == TokenType::GT_OR_EQ ||
                       operatorType == TokenType::LESS_THAN ||
                       operatorType == TokenType::LT_OR_EQ ||
                       operatorType == TokenType::EQUALS ||
                       operatorType == TokenType::NOT_EQUALS);

  if (isComparison) {
    if (leftType.isNull || rightType.isNull) {
      logSemanticErrors("Cannot carry out comparison on types '" +
                            leftType.resolvedName + "' and '" +
                            rightType.resolvedName +
                            "' as one of them is nullable",
                        leftLine, leftCol);
      return ResolvedType{DataType::ERROR, "error"};
    }

    if (leftType.kind == rightType.kind)
      return ResolvedType{DataType::BOOLEAN, "boolean"};
    logSemanticErrors("Cannot compare '" + leftType.resolvedName + "' to '" +
                          rightType.resolvedName + "'",
                      leftLine, leftCol);
    return ResolvedType{DataType::ERROR, "error"};
  }

  // String concatenation(Not complete but I will work on this)
  if (operatorType == TokenType::PLUS && isString(leftType) &&
      isString(rightType)) {
    return ResolvedType{DataType::STRING, "string"};
  }

  // Arithmetic operators: +, -, %, /, *
  bool isArithmetic =
      (operatorType == TokenType::PLUS || operatorType == TokenType::MINUS ||
       operatorType == TokenType::MODULUS ||
       operatorType == TokenType::DIVIDE ||
       operatorType == TokenType::ASTERISK);

  if (isArithmetic) {
    if (leftType.isNull || rightType.isNull) {
      logSemanticErrors("Cannot carry out arithmetic on types '" +
                            leftType.resolvedName + "' and '" +
                            rightType.resolvedName +
                            "' as one of them is nullable",
                        leftLine, leftCol);
      return ResolvedType{DataType::ERROR, "error"};
    }
    if (leftType.isPointer && isInteger(rightType)) {
      if (operatorType == TokenType::PLUS || operatorType == TokenType::MINUS) {
        return leftType; // Location +/- Distance = Location
      }
    }

    // Integer + Pointer = Pointer (Commutative Addition)
    if (isInteger(leftType) && rightType.isPointer) {
      if (operatorType == TokenType::PLUS) {
        return rightType;
      }
    }

    //  Pointer - Pointer = USIZE (Distance between two points)
    if (leftType.isPointer && rightType.isPointer &&
        operatorType == TokenType::MINUS) {
      return ResolvedType{DataType::USIZE, "usize"};
    }
    // Promote mixed int/float combinations
    if ((isInteger(leftType) && isFloat(rightType)) ||
        (isFloat(leftType) && isInteger(rightType))) {
      return ResolvedType{DataType::F32, "f32"};
    }
    // Promote int/double or float/double to double
    if ((isInteger(leftType) && rightType.kind == DataType::F64) ||
        (leftType.kind == DataType::F64 && isInteger(rightType))) {
      return ResolvedType{DataType::F64, "f64"};
    }
    if ((leftType.kind == DataType::F32 && rightType.kind == DataType::F64) ||
        (leftType.kind == DataType::F64 && rightType.kind == DataType::F32)) {
      return ResolvedType{DataType::F64, "f64"};
    }

    if (leftType.kind == rightType.kind) {
      return leftType;
    }

    if (isTypeCompatible(leftType, rightType)) {
      return rightType;
    }

    logSemanticErrors("Type mismatch '" + leftType.resolvedName +
                          "' does not match '" + rightType.resolvedName + "'",
                      leftLine, leftCol);
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }

  bool isBitwise = (operatorType == TokenType::BITWISE_AND ||
                    operatorType == TokenType::BITWISE_OR ||
                    operatorType == TokenType::BITWISE_XOR);

  if (isBitwise) {
    if (isInteger(leftType) && isInteger(rightType)) {
      if (leftType.kind == rightType.kind) {
        return leftType;
      }

      logSemanticErrors("Bitwise mismatch, Cannot use '" + operatorStr +
                            "' on different integer types '" +
                            leftType.resolvedName + "' and '" +
                            rightType.resolvedName + "'",
                        leftLine, leftCol);
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    logSemanticErrors("Bitwise operations cannot be performed on '" +
                          leftType.resolvedName + "' and '" +
                          rightType.resolvedName + "'",
                      leftLine, leftCol);
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }

  bool isShift = (operatorType == TokenType::SHIFT_RIGHT ||
                  operatorType == TokenType::SHIFT_LEFT);
  if (isShift) {
    if (isInteger(leftType) && isInteger(rightType)) {
      return leftType;
    }

    logSemanticErrors(
        "Shift operators require integers on both sides  but got '" +
            leftType.resolvedName + "' and '" + rightType.resolvedName + "'",
        leftLine, leftCol);
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }

  logSemanticErrors("Unknown binary operator '" + operatorStr + "'", leftLine,
                    leftCol);
  return ResolvedType{DataType::UNKNOWN, "unknown"};
}

ResolvedType Semantics::resultOfUnary(TokenType operatorType,
                                      const ResolvedType &operandType,
                                      Node *node) {
  int line;
  int col;
  std::string operatorStr;
  if (auto postfix = dynamic_cast<PostfixExpression *>(node)) {
    line = postfix->expression.line;
    col = postfix->expression.column;
    operatorStr = postfix->operator_token.TokenLiteral;
  } else if (auto prefix = dynamic_cast<PrefixExpression *>(node)) {
    line = prefix->expression.line;
    col = prefix->expression.column;
    operatorStr = prefix->operat.TokenLiteral;
  }

  switch (operatorType) {
  case TokenType::BANG: {
    if (!isBoolean(operandType)) {
      logSemanticErrors("Cannot apply '" + operatorStr + "' to type '" +
                            operandType.resolvedName + "'",
                        line, col);
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }
    return ResolvedType{DataType::BOOLEAN, "bool"};
  }
  case TokenType::MINUS:
  case TokenType::PLUS:
  case TokenType::PLUS_PLUS:
  case TokenType::MINUS_MINUS: {
    if (isInteger(operandType) || isFloat(operandType)) {
      return operandType;
    }

    logSemanticErrors("Cannot apply '" + operatorStr + "' to type '" +
                          operandType.resolvedName + "'",
                      line, col);
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }
  case TokenType::BITWISE_NOT: {
    if (isInteger(operandType)) {
      return operandType;
    }

    logSemanticErrors(
        "Bitwise operator '" + operatorStr +
            "' can only be applied to integer types you provided '" +
            operandType.resolvedName + "'",
        line, col);
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }
  default:
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }
}

std::shared_ptr<SymbolInfo>
Semantics::resolveSymbolInfo(const std::string &name) {
  for (int i = symbolTable.size() - 1; i >= 0; --i) {
    auto &scope = symbolTable[i];
    logInternal("Searching for '" + name + "' in scope level " +
                std::to_string(i));
    for (auto &[key, value] : scope) {
      logInternal(" >> Key in scope '" + key + "'");
    }
    if (scope.find(name) != scope.end()) {
      logInternal("Found match for '" + name + "'");
      return scope[name];
    }
  }

  logInternal("No match for '" + name + "'");
  return nullptr;
}

std::shared_ptr<SymbolInfo>
Semantics::lookUpInCurrentScope(const std::string &name) {
  if (symbolTable.empty()) {
    reportDevBug("Attempted current scope look up on empty symbol table");
    return nullptr;
  }

  // Access the current scope which is the latest scope in the symbol table
  auto &currentScope = symbolTable.back();

  logInternal("Searching for '" + name + "' in current scope level " +
              std::to_string(symbolTable.size() - 1) + "...");

  if (currentScope.find(name) != currentScope.end()) {
    logInternal("Found match for '" + name + "' in current scope");
    return currentScope.at(name);
  }

  logInternal("No match for '" + name + "' in current scope");
  return nullptr;
}

bool Semantics::isGlobalScope() {
  if (symbolTable.size() == 1) {
    return true;
  }
  return false;
}

ResolvedType Semantics::tokenTypeToResolvedType(Token token, bool isNullable) {
  auto makeType = [&](DataType nonNull, const std::string &baseName) {
    if (isNullable) {
      ResolvedType t;
      t.kind = nonNull;
      t.resolvedName = baseName + "?";
      t.isNull = isNullable;
      return t;
    } else
      return ResolvedType{nonNull, baseName};
  };

  switch (token.type) {
  case TokenType::I8_KEYWORD:
    return makeType(DataType::I8, "i8");
  case TokenType::U8_KEYWORD:
    return makeType(DataType::U8, "u8");
  case TokenType::I16_KEYWORD:
    return makeType(DataType::I16, "i16");
  case TokenType::U16_KEYWORD:
    return makeType(DataType::U16, "u16");
  case TokenType::I32_KEYWORD:
    return makeType(DataType::I32, "i32");
  case TokenType::U32_KEYWORD:
    return makeType(DataType::U32, "u32");
  case TokenType::I64_KEYWORD:
    return makeType(DataType::I64, "i64");
  case TokenType::U64_KEYWORD:
    return makeType(DataType::U64, "u64");
  case TokenType::I128_KEYWORD:
    return makeType(DataType::I128, "i128");
  case TokenType::U128_KEYWORD:
    return makeType(DataType::U128, "u128");
  case TokenType::ISIZE_KEYWORD:
    return makeType(DataType::ISIZE, "isize");
  case TokenType::USIZE_KEYWORD:
    return makeType(DataType::USIZE, "usize");

  case TokenType::F32_KEYWORD:
    return makeType(DataType::F32, "f32");
  case TokenType::F64_KEYWORD:
    return makeType(DataType::F64, "f64");
  case TokenType::STRING_KEYWORD:
    return makeType(DataType::STRING, "string");

  case TokenType::CHAR8_KEYWORD:
    return makeType(DataType::CHAR8, "char8");
  case TokenType::CHAR16_KEYWORD:
    return makeType(DataType::CHAR16, "char16");
  case TokenType::CHAR32_KEYWORD:
    return makeType(DataType::CHAR32, "char32");

  case TokenType::BOOL_KEYWORD:
    return makeType(DataType::BOOLEAN, "bool");

  case TokenType::OPAQUE:
    return makeType(DataType::OPAQUE, "opaque");

  case TokenType::VOID:
    return {DataType::VOID, "void"};

  case TokenType::IDENTIFIER: {
    auto parentIt = customTypesTable.find(token.TokenLiteral);
    if (parentIt != customTypesTable.end()) {
      auto parentType = parentIt->second->type;
      parentType.isNull = isNullable;
      return parentType;
    }

    logSemanticErrors("Unknown type '" + token.TokenLiteral + "'", token.line,
                      token.column);
    return {DataType::UNKNOWN, "unknown"};
  }

  default:
    return {DataType::UNKNOWN, "unknown", false, false};
  }
}

bool Semantics::isTypeCompatible(const ResolvedType &expected,
                                 const ResolvedType &actual) {

  // Opaque pointer check
  if (expected.kind == DataType::OPAQUE && expected.isPointer &&
      actual.isPointer) {
    if (!expected.isNull && actual.isNull)
      return false;
    return true;
  }

  // Pointers must match
  if (expected.isPointer != actual.isPointer)
    return false;

  if (expected.isArray != actual.isArray)
    return false;

  // References must match
  if (expected.isRef != actual.isRef)
    return false;

  // Error type can always pass through
  if (actual.kind == DataType::ERROR)
    return false;

  // If kinds differ, types differ.
  if (expected.kind != actual.kind)
    return false;

  // === Nullability Rules ===
  // Assigning nullable to non-nullable is unsafe -> BLOCK
  if (!expected.isNull && actual.isNull)
    return false;

  // Assigning non-nullable to nullable is safe -> ALLOW
  if (expected.isNull && !actual.isNull)
    return true;

  // If both match exactly (both nullable or both non-null)
  if (expected.isNull == actual.isNull)
    return true;

  return false;
}

bool Semantics::hasReturnPath(Node *node) {
  if (currentFunction &&
      currentFunction.value()->returnType.kind == DataType::VOID) {
    return true; // Void functions don't need returns
  }

  if (auto blockStmt = dynamic_cast<BlockStatement *>(node)) {
    for (const auto &stmt : blockStmt->statements) {
      if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get())) {
        if (retStmt->return_value ||
            (currentFunction.value()->isNullable && !retStmt->return_value)) {
          return true; // value, or null return
        }
      }
      if (auto ifStmt = dynamic_cast<ifStatement *>(stmt.get())) {
        auto thenBlock =
            dynamic_cast<BlockStatement *>(ifStmt->if_result.get());
        bool hasThenReturn = thenBlock && hasReturnPath(thenBlock);
        bool hasElseReturn = ifStmt->else_result.has_value() &&
                             hasReturnPath(dynamic_cast<BlockStatement *>(
                                 ifStmt->else_result.value().get()));
        if (hasThenReturn && hasElseReturn) {
          return true;
        }
        bool hasElifReturn = true;
        for (const auto &elif : ifStmt->elifClauses) {
          auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
          if (elifStmt) {
            auto elifBlock =
                dynamic_cast<BlockStatement *>(elifStmt->elif_result.get());
            hasElifReturn &= elifBlock && hasReturnPath(elifBlock);
          }
        }
        if (hasThenReturn && hasElifReturn && hasElseReturn) {
          return true;
        }
      }

      if (auto switchStmt = dynamic_cast<SwitchStatement *>(stmt.get())) {
        return switchReturns(switchStmt);
      }
    }
    return false; // BlockStatement has no finalexpr
  }

  if (auto blockExpr = dynamic_cast<BlockExpression *>(node)) {
    for (const auto &stmt : blockExpr->statements) {
      if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get())) {
        if (retStmt->return_value ||
            (currentFunction.value()->isNullable && !retStmt->return_value)) {
          return true; // Error, value, or null return
        }
      }
      if (auto ifStmt = dynamic_cast<ifStatement *>(stmt.get())) {
        auto thenBlock =
            dynamic_cast<BlockStatement *>(ifStmt->if_result.get());
        bool hasThenReturn = thenBlock && hasReturnPath(thenBlock);
        bool hasElseReturn = ifStmt->else_result.has_value() &&
                             hasReturnPath(dynamic_cast<BlockStatement *>(
                                 ifStmt->else_result.value().get()));
        if (hasThenReturn && hasElseReturn) {
          return true;
        }
        bool hasElifReturn = true;
        for (const auto &elif : ifStmt->elifClauses) {
          auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
          if (elifStmt) {
            auto elifBlock =
                dynamic_cast<BlockStatement *>(elifStmt->elif_result.get());
            hasElifReturn &= elifBlock && hasReturnPath(elifBlock);
          }
        }
        if (hasThenReturn && hasElifReturn && hasElseReturn) {
          return true;
        }
      }

      if (auto switchStmt = dynamic_cast<SwitchStatement *>(stmt.get())) {
        return switchReturns(switchStmt);
      }
    }

    if (blockExpr->finalexpr.has_value()) {
      ResolvedType exprType =
          inferNodeDataType(blockExpr->finalexpr.value().get());
      return exprType.kind == DataType::ERROR ||
             isTypeCompatible(currentFunction.value()->returnType, exprType) ||
             (dynamic_cast<NullLiteral *>(blockExpr->finalexpr.value().get()) &&
              currentFunction.value()->isNullable);
    }
    return false;
  }

  if (auto stmt = dynamic_cast<ExpressionStatement *>(node)) {
    return hasReturnPath(stmt->expression.get());
  }

  return false;
}

bool Semantics::switchReturns(SwitchStatement *sw) {
  if (sw->default_statements.empty())
    return false;

  bool defaultReturns = hasReturnPathList(sw->default_statements);
  if (!defaultReturns)
    return false;

  // Check every case clause
  for (size_t i = 0; i < sw->case_clauses.size(); ++i) {
    auto caseClause = dynamic_cast<CaseClause *>(sw->case_clauses[i].get());

    if (caseClause->body.empty()) {
      // Look ahead for the next body
      bool safelyFallsThrough = false;
      for (size_t j = i + 1; j < sw->case_clauses.size(); ++j) {
        auto nextClause = dynamic_cast<CaseClause *>(sw->case_clauses[j].get());
        if (!nextClause->body.empty()) {
          if (hasReturnPathList(nextClause->body)) {
            safelyFallsThrough = true;
          }
          break;
        }
      }
      // If we didn't find a returning body in later cases, check default
      if (!safelyFallsThrough && !defaultReturns) {
        return false;
      }
      // If it falls through to a valid return, this specific 'i' is safe.
    } else {
      // Normal body check
      if (!hasReturnPathList(caseClause->body)) {
        return false;
      }
    }
  }

  return true;
}

bool Semantics::hasReturnPathList(
    const std::vector<std::unique_ptr<Statement>> &stmts) {
  for (const auto &stmt : stmts) {
    // If the statement is a return, we're good
    if (auto ret = dynamic_cast<ReturnStatement *>(stmt.get()))
      return true;

    // If it's a nested if/switch, check if they are exhaustive
    if (hasReturnPath(stmt.get()))
      return true;
  }
  return false;
}

bool Semantics::areSignaturesCompatible(const SymbolInfo &declInfo,
                                        FunctionExpression *funcExpr) {

  if (declInfo.paramTypes.size() != funcExpr->call.size()) {
    return false;
  }
  for (size_t i = 0; i < declInfo.paramTypes.size(); ++i) {
    auto letStmt = dynamic_cast<LetStatement *>(funcExpr->call[i].get());
    if (!letStmt)
      return false;
    auto type = dynamic_cast<BasicType *>(letStmt->type.get());
    ResolvedType paramType = inferNodeDataType(type);
    std::string paramGenericName =
        type->data_token.type == TokenType::IDENTIFIER
            ? type->data_token.TokenLiteral
            : "";
    // Find declaration's parameter metadata
    bool declParamNullable = false;
    bool isLetNullable = type->isNullable;
    for (const auto &pair : metaData) {
      if (auto declLetStmt = dynamic_cast<LetStatement *>(pair.first)) {
        if (declLetStmt->ident_token.TokenLiteral ==
                letStmt->ident_token.TokenLiteral &&
            pair.second->type.kind == declInfo.paramTypes[i].first.kind &&
            pair.second->genericName == declInfo.paramTypes[i].second) {
          declParamNullable = pair.second->isNullable;
          break;
        }
      }
    }
    if (paramType.kind != declInfo.paramTypes[i].first.kind ||
        paramGenericName != declInfo.paramTypes[i].second ||
        isLetNullable != declParamNullable) {
      return false;
    }
  }

  // Check return type
  auto retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
  if (!retType)
    return false;
  ResolvedType returnType = inferNodeDataType(retType->returnExpr.get());
  std::string returnGenericName =
      retType->expression.type == TokenType::IDENTIFIER
          ? retType->expression.TokenLiteral
          : "";
  return returnType.kind == declInfo.returnType.kind;
}

bool Semantics::signaturesMatchBehaviorDeclaration(
    const std::shared_ptr<MemberInfo> &declMember,
    FunctionExpression *funcExpr) {
  if (!declMember)
    return false;

  if (!funcExpr)
    return false;

  const auto &declParams = declMember->paramTypes;
  if (declParams.size() != funcExpr->call.size())
    return false;

  for (size_t i = 0; i < declMember->paramTypes.size(); ++i) {
    auto letStmt = dynamic_cast<LetStatement *>(funcExpr->call[i].get());
    if (!letStmt)
      return false;
    auto type = dynamic_cast<BasicType *>(letStmt->type.get());
    ResolvedType paramType = inferNodeDataType(type);
    std::string paramGenericName =
        type->data_token.type == TokenType::IDENTIFIER
            ? type->data_token.TokenLiteral
            : "";
    // Find declaration's parameter metadata
    bool declParamNullable = false;
    bool isLetNullable = type->isNullable;
    for (const auto &pair : metaData) {
      if (auto declLetStmt = dynamic_cast<LetStatement *>(pair.first)) {
        if (declLetStmt->ident_token.TokenLiteral ==
                letStmt->ident_token.TokenLiteral &&
            pair.second->type.kind == declMember->paramTypes[i].first.kind &&
            pair.second->genericName == declMember->paramTypes[i].second) {
          declParamNullable = pair.second->isNullable;
          break;
        }
      }
    }
    if (paramType.kind != declMember->paramTypes[i].first.kind ||
        paramGenericName != declMember->paramTypes[i].second ||
        isLetNullable != declParamNullable) {
      return false;
    }
  }

  // Check return type
  auto retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
  if (!retType)
    return false;
  ResolvedType returnType = inferNodeDataType(retType->returnExpr.get());
  std::string returnGenericName =
      retType->expression.type == TokenType::IDENTIFIER
          ? retType->expression.TokenLiteral
          : "";
  return returnType.kind == declMember->returnType.kind;
}

bool Semantics::isMethodCallCompatible(const MemberInfo &memFuncInfo,
                                       CallExpression *callExpr) {
  bool allGood = true;

  auto funcName = callExpr->function_identifier->expression.TokenLiteral;

  if (memFuncInfo.paramTypes.size() != callExpr->parameters.size()) {
    logSemanticErrors("Function '" + funcName + "' call has " +
                          std::to_string(callExpr->parameters.size()) +
                          " arguments, but expects " +
                          std::to_string(memFuncInfo.paramTypes.size()),
                      callExpr->expression.line, callExpr->expression.column);
    return false;
  }

  for (size_t i = 0; i < callExpr->parameters.size(); ++i) {
    auto &param = callExpr->parameters[i];
    const auto &expectedType = memFuncInfo.paramTypes[i].first;
    auto argInfo = metaData[param.get()];
    if (!argInfo)
      continue;

    ResolvedType argType = argInfo->type;

    bool isCompatible = isTypeCompatible(expectedType, argType);
    if (!isCompatible && expectedType.isRef && !argType.isRef) {
      auto ident = dynamic_cast<Identifier *>(param.get());
      auto deref = dynamic_cast<DereferenceExpression *>(param.get());

      if (!(ident || deref)) {
        logSemanticErrors("Cannot pass an expression to a reference parameter",
                          param->expression.line, param->expression.column);
        return false;
      }

      if (expectedType.kind == argType.kind) {
        isCompatible = true;
        argInfo->needsImplicitAddress = true;
      }
    }

    if (argType.kind == DataType::UNKNOWN) {
      logSemanticErrors("Could not infer type for argument " +
                            std::to_string(i + 1),
                        param->expression.line, param->expression.column);
      allGood = false;
      continue;
    }

    // --- Nullability rule ---
    if (auto nullLit = dynamic_cast<NullLiteral *>(param.get())) {
      if (expectedType.isNull) {
        argType = expectedType; // promote null → nullable type
      } else {
        logSemanticErrors("Cannot pass null to non-nullable parameter " +
                              std::to_string(i + 1) + ": expected " +
                              expectedType.resolvedName,
                          param->expression.line, param->expression.column);
        allGood = false;
        continue;
      }
    }

    if (!isCompatible) {
      logSemanticErrors("Call for '" + funcName +
                            "'has a type mismatch in argument " +
                            std::to_string(i + 1) + ", expected '" +
                            expectedType.resolvedName + "' but got '" +
                            argType.resolvedName + "'",
                        param->expression.line, param->expression.column);
      allGood = false;
      continue;
    }
  }

  return allGood;
}

bool Semantics::isCallCompatible(const SymbolInfo &funcInfo,
                                 CallExpression *callExpr) {
  bool allGood = true;

  auto funcName = callExpr->function_identifier->expression.TokenLiteral;

  // Check parameter count
  if (funcInfo.paramTypes.size() != callExpr->parameters.size()) {
    logSemanticErrors("Function '" + funcName + "' call has " +
                          std::to_string(callExpr->parameters.size()) +
                          " arguments, but expected " +
                          std::to_string(funcInfo.paramTypes.size()),
                      callExpr->expression.line, callExpr->expression.column);
    return false;
  }

  for (size_t i = 0; i < callExpr->parameters.size(); ++i) {
    auto &param = callExpr->parameters[i];
    const auto &expectedType = funcInfo.paramTypes[i].first;
    auto argInfo = metaData[param.get()];
    if (!argInfo)
      continue;

    ResolvedType argType = argInfo->type;

    bool isCompatible = isTypeCompatible(expectedType, argType);

    // If the type is a reference
    if (!isCompatible && expectedType.isRef && !argType.isRef) {
      auto ident = dynamic_cast<Identifier *>(param.get());
      auto deref = dynamic_cast<DereferenceExpression *>(param.get());

      if (!(ident || deref)) {
        logSemanticErrors("Cannot pass an expression to a reference parameter",
                          param->expression.line, param->expression.column);
        return false;
      }

      if (expectedType.kind == argType.kind) {
        isCompatible = true;
        argInfo->needsImplicitAddress = true;
      }
    }

    if (argType.kind == DataType::UNKNOWN) {
      logSemanticErrors("Could not infer type for argument " +
                            std::to_string(i + 1),
                        param->expression.line, param->expression.column);
      allGood = false;
      continue;
    }

    // --- Nullability rule ---
    if (auto nullLit = dynamic_cast<NullLiteral *>(param.get())) {
      if (expectedType.isNull) {
        argType = expectedType; // promote null → nullable type
      } else {
        logSemanticErrors("Cannot pass null to non-nullable parameter " +
                              std::to_string(i + 1) + ": expected " +
                              expectedType.resolvedName,
                          param->expression.line, param->expression.column);
        allGood = false;
        continue;
      }
    }

    if (!isCompatible) {
      logSemanticErrors("Call for '" + funcName +
                            "'has a type mismatch in argument " +
                            std::to_string(i + 1) + ", expected '" +
                            expectedType.resolvedName + "' but got '" +
                            argType.resolvedName + "'",
                        param->expression.line, param->expression.column);
      allGood = false;
      continue;
    }
  }
  return allGood;
}

int Semantics::inferLiteralDimensions(ArrayLiteral *lit) {
  if (lit->array.empty())
    return 1; // [] is still 1D, undefined element type

  // If the first element is not an array literal -> dimension = 1
  auto first = lit->array[0].get();
  auto innerArr = dynamic_cast<ArrayLiteral *>(first);
  if (!innerArr)
    return 1;

  // Otherwise: 1 (this level) + dimension of inner
  return 1 + inferLiteralDimensions(innerArr);
}

void Semantics::inferSizePerDimension(ArrayLiteral *lit,
                                      std::vector<int64_t> &sizes) {
  sizes.push_back(lit->array.size());
  if (!lit->array.empty()) {
    if (auto inner = dynamic_cast<ArrayLiteral *>(lit->array[0].get())) {
      inferSizePerDimension(inner, sizes);
    }
  }
}

std::vector<uint64_t> Semantics::getSizePerDimesion(Node *node) {
  std::vector<uint64_t> dims;
  Node *current = node;

  // We keep drilling down as long as we find nested ArrayLiterals
  while (auto arrLit = dynamic_cast<ArrayLiteral *>(current)) {
    // Record the size of the current "container"
    dims.push_back(static_cast<uint64_t>(arrLit->array.size()));
    if (arrLit->array.empty()) {
      break; // Can't determine dimensions of an empty array
    }
    // Move to the first element to see if it's another level deep
    current = arrLit->array[0].get();
  }

  return dims;
}

uint64_t Semantics::getIntegerConstant(Node *node) {
  if (!node)
    return 0;

  auto tryParse = [&](auto *lit) -> uint64_t {
    if (lit) {
      try {
        // stoull handles the string-to-number conversion
        return std::stoull(lit->expression.TokenLiteral);
      } catch (...) {
        return 0; // Handle garbage strings just in case
      }
    }
    return 0;
  };

  if (auto l = dynamic_cast<I8Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<U8Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<I16Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<U16Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<I32Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<U32Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<I64Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<U64Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<I128Literal *>(node))
    return tryParse(l);
  if (auto l = dynamic_cast<U128Literal *>(node))
    return tryParse(l);

  return 0;
}

bool Semantics::isInteger(const ResolvedType &t) {
  static const std::unordered_set<DataType> intTypes = {
      DataType::I8,   DataType::U8,   DataType::I16,   DataType::U16,
      DataType::I32,  DataType::U32,  DataType::I64,   DataType::U64,
      DataType::I128, DataType::U128, DataType::ISIZE, DataType::USIZE};
  return intTypes.count(t.kind) > 0;
}

bool Semantics::isFloat(const ResolvedType &t) {
  return t.kind == DataType::F32 || t.kind == DataType::F64;
}

bool Semantics::isBoolean(const ResolvedType &t) {
  return t.kind == DataType::BOOLEAN;
}

bool Semantics::isString(const ResolvedType &t) {
  return t.kind == DataType::STRING;
}

bool Semantics::isChar(const ResolvedType &t) {
  static const std::unordered_set<DataType> charTypes = {
      DataType::CHAR8, DataType::CHAR16, DataType::CHAR32};
  return charTypes.count(t.kind) > 0;
}

bool Semantics::isLiteral(Node *node) {
  // Integer literals
  auto i8Lit = dynamic_cast<I8Literal *>(node);
  auto u8Lit = dynamic_cast<U8Literal *>(node);
  auto i16Lit = dynamic_cast<I16Literal *>(node);
  auto u16Lit = dynamic_cast<U16Literal *>(node);
  auto i32Lit = dynamic_cast<I32Literal *>(node);
  auto u32Lit = dynamic_cast<U32Literal *>(node);
  auto i64Lit = dynamic_cast<I64Literal *>(node);
  auto u64Lit = dynamic_cast<U64Literal *>(node);
  auto i128Lit = dynamic_cast<I128Literal *>(node);
  auto u128Lit = dynamic_cast<U128Literal *>(node);
  bool isIntLit = (i8Lit || u8Lit || i16Lit || u16Lit || i32Lit || u32Lit ||
                   i64Lit || u64Lit || i128Lit || u128Lit);

  // Float and double literals;
  auto f32Lit = dynamic_cast<F32Literal *>(node);
  auto f64Lit = dynamic_cast<F64Literal *>(node);
  bool isDecLit = (f32Lit || f64Lit);

  // Char literal
  auto char8Lit = dynamic_cast<Char8Literal *>(node);
  auto char16Lit = dynamic_cast<Char16Literal *>(node);
  auto char32Lit = dynamic_cast<Char32Literal *>(node);
  bool isCharLit = (char8Lit || char16Lit || char32Lit);

  // Boolean literal
  auto boolLit = dynamic_cast<BooleanLiteral *>(node);

  // String literal
  auto strLit = dynamic_cast<StringLiteral *>(node);
  auto infix = dynamic_cast<InfixExpression *>(node);

  if (isIntLit || isDecLit || isCharLit || boolLit || strLit || infix)
    return true;
  else
    return false;
}

bool Semantics::isConstLiteral(Node *node) {
  // Integer literals
  auto i8Lit = dynamic_cast<I8Literal *>(node);
  auto u8Lit = dynamic_cast<U8Literal *>(node);
  auto i16Lit = dynamic_cast<I16Literal *>(node);
  auto u16Lit = dynamic_cast<U16Literal *>(node);
  auto i32Lit = dynamic_cast<I32Literal *>(node);
  auto u32Lit = dynamic_cast<U32Literal *>(node);
  auto i64Lit = dynamic_cast<I64Literal *>(node);
  auto u64Lit = dynamic_cast<U64Literal *>(node);
  auto i128Lit = dynamic_cast<I128Literal *>(node);
  auto u128Lit = dynamic_cast<U128Literal *>(node);
  bool isIntLit = (i8Lit || u8Lit || i16Lit || u16Lit || i32Lit || u32Lit ||
                   i64Lit || u64Lit || i128Lit || u128Lit);

  // Float and double literals;
  auto f32Lit = dynamic_cast<F32Literal *>(node);
  auto f64Lit = dynamic_cast<F64Literal *>(node);
  bool isDecLit = (f32Lit || f64Lit);

  // Char literal
  auto char8Lit = dynamic_cast<Char8Literal *>(node);
  auto char16Lit = dynamic_cast<Char16Literal *>(node);
  auto char32Lit = dynamic_cast<Char32Literal *>(node);
  bool isCharLit = (char8Lit || char16Lit || char32Lit);

  // Boolean literal
  auto boolLit = dynamic_cast<BooleanLiteral *>(node);

  // String literal
  auto strLit = dynamic_cast<StringLiteral *>(node);

  if (isIntLit || isDecLit || isCharLit || boolLit || strLit)
    return true;
  else
    return false;
}

bool Semantics::isIntegerConstant(Node *node) {
  auto i8Lit = dynamic_cast<I8Literal *>(node);
  auto u8Lit = dynamic_cast<U8Literal *>(node);
  auto i16Lit = dynamic_cast<I16Literal *>(node);
  auto u16Lit = dynamic_cast<U16Literal *>(node);
  auto i32Lit = dynamic_cast<I32Literal *>(node);
  auto u32Lit = dynamic_cast<U32Literal *>(node);
  auto i64Lit = dynamic_cast<I64Literal *>(node);
  auto u64Lit = dynamic_cast<U64Literal *>(node);
  auto i128Lit = dynamic_cast<I128Literal *>(node);
  auto u128Lit = dynamic_cast<U128Literal *>(node);
  bool isIntLit = (i8Lit || u8Lit || i16Lit || u16Lit || i32Lit || u32Lit ||
                   i64Lit || u64Lit || i128Lit || u128Lit);

  return isIntLit;
}

ResolvedType Semantics::resolvedDataType(Token token, Node *node) {
  logInternal("Resolving Type ....");

  TokenType type = token.type;

  switch (type) {
  case TokenType::I8_KEYWORD:
    return ResolvedType{DataType::I8, "i8"};
  case TokenType::U8_KEYWORD:
    return ResolvedType{DataType::U8, "u8"};

  case TokenType::I16_KEYWORD:
    return ResolvedType{DataType::I16, "i16"};
  case TokenType::U16_KEYWORD:
    return ResolvedType{DataType::U16, "u16"};

  case TokenType::I32_KEYWORD:
    return ResolvedType{DataType::I32, "i32"};
  case TokenType::U32_KEYWORD:
    return ResolvedType{DataType::U32, "u32"};

  case TokenType::I64_KEYWORD:
    return ResolvedType{DataType::I64, "i64"};
  case TokenType::U64_KEYWORD:
    return ResolvedType{DataType::U64, "u64"};

  case TokenType::I128_KEYWORD:
    return ResolvedType{DataType::I128, "i128"};
  case TokenType::U128_KEYWORD:
    return ResolvedType{DataType::U128, "u128"};

  case TokenType::ISIZE_KEYWORD:
    return ResolvedType{DataType::ISIZE, "isize"};
  case TokenType::USIZE_KEYWORD:
    return ResolvedType{DataType::USIZE, "usize"};

  case TokenType::F32_KEYWORD:
    return ResolvedType{DataType::F32, "f32"};
  case TokenType::F64_KEYWORD:
    return ResolvedType{DataType::F64, "f64"};

  case TokenType::STRING_KEYWORD:
    return ResolvedType{DataType::STRING, "string"};

  case TokenType::CHAR8_KEYWORD:
    return ResolvedType{DataType::CHAR8, "char8"};
  case TokenType::CHAR16_KEYWORD:
    return ResolvedType{DataType::CHAR16, "char16"};
  case TokenType::CHAR32_KEYWORD:
    return ResolvedType{DataType::CHAR32, "char32"};

  case TokenType::BOOL_KEYWORD:
    return ResolvedType{DataType::BOOLEAN, "bool"};

  case TokenType::AUTO: {
    auto letStmt = dynamic_cast<LetStatement *>(node);
    auto type = dynamic_cast<BasicType *>(node);
    auto letStmtValue = letStmt->value.get();
    if (!letStmtValue) {
      logSemanticErrors("Cannot infer without a value", type->data_token.line,
                        type->data_token.column);
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }
    auto inferred = inferNodeDataType(letStmtValue);
    return ResolvedType{inferred.kind, inferred.resolvedName};
  }

  // Dealing with custom types now
  case TokenType::IDENTIFIER: {
    logInternal("Resolving custom type ....");
    // Extract the identifier as this how the parser is logging the correct
    // types Case 1 is for let statements
    auto letStmt = dynamic_cast<LetStatement *>(node);
    auto type = dynamic_cast<BasicType *>(node);
    // Extract the custom data type
    auto letStmtType = type->data_token.TokenLiteral;

    // Search for the name in the custom types table
    auto typeIt = customTypesTable.find(letStmtType);
    if (typeIt == customTypesTable.end()) {
      logSemanticErrors("Type '" + letStmtType + "' is unknown",
                        type->data_token.line, type->data_token.column);
      return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    return ResolvedType{typeIt->second->type.kind, typeIt->second->typeName};
  }

  default:
    return ResolvedType{DataType::UNKNOWN, "unknown"};
  }
}

std::string Semantics::stripOptionalSuffix(const std::string &type) {
  std::string result = type;

  // Find the '?' anywhere in the name
  size_t qPos = result.find('?');
  if (qPos != std::string::npos) {
    // Erase just that one character
    result.erase(qPos, 1);
  }

  return result;
}

std::string Semantics::stripPtrSuffix(const std::string &type) {
  // Inline lambda to check if a string ends with a suffix
  auto endsWith = [](const std::string &str,
                     const std::string &suffix) -> bool {
    if (str.length() < suffix.length())
      return false;
    return str.compare(str.length() - suffix.length(), suffix.length(),
                       suffix) == 0;
  };

  auto ptrName = [&](std::string typeName) {
    if (endsWith(typeName, "_ptr"))
      typeName = typeName.substr(0, typeName.size() - 4);

    return typeName;
  };

  return ptrName(type);
}

std::string Semantics::stripRefSuffix(const std::string &type) {
  auto endsWith = [](const std::string &str,
                     const std::string &suffix) -> bool {
    if (str.length() < suffix.length())
      return false;
    return str.compare(str.length() - suffix.length(), suffix.length(),
                       suffix) == 0;
  };

  auto refName = [&](std::string typeName) {
    if (endsWith(typeName, "_ref"))
      typeName = typeName.substr(0, typeName.size() - 4);

    return typeName;
  };

  return refName(type);
}

ResolvedType Semantics::getArrayElementType(ResolvedType &type) {
  auto stripBrackets = [](const std::string &str) -> std::string {
    size_t firstBracket = str.find('[');
    size_t lastBracket = str.rfind(']');

    // Only slice if both brackets exist and there is content between them
    if (firstBracket != std::string::npos && lastBracket != std::string::npos &&
        lastBracket > firstBracket + 1) {
      return str.substr(firstBracket + 1, lastBracket - firstBracket - 1);
    }
    return str; // Fallback to original if it's not actually an array string
  };

  ResolvedType elementType;
  elementType.resolvedName = stripBrackets(type.resolvedName);
  logInternal("Stripped array name: " + elementType.resolvedName);
  elementType.kind = type.kind;
  elementType.isArray = false;

  return elementType;
}

ResolvedType Semantics::isPointerType(ResolvedType t) {
  // Inline lambda to check if a string ends with a suffix
  auto endsWith = [](const std::string &str,
                     const std::string &suffix) -> bool {
    if (str.length() < suffix.length())
      return false;
    return str.compare(str.length() - suffix.length(), suffix.length(),
                       suffix) == 0;
  };

  auto ptrType = [&](DataType baseType, bool isPtr, std::string baseName) {
    if (isPtr) {
      if (!endsWith(baseName, "_ptr"))
        baseName += "_ptr";
    } else {
      if (endsWith(baseName, "_ptr"))
        baseName = baseName.substr(0, baseName.size() - 4);
    }

    ResolvedType type;
    type.resolvedName = baseName;
    type.kind = baseType;
    type.isArray = t.isArray;
    type.isPointer = isPtr;
    type.isNull = t.isNull;
    type.isRef = false; // A pointer cannot be a pointer

    return type;
  };

  return ptrType(t.kind, t.isPointer, t.resolvedName);
}

ResolvedType Semantics::isRefType(ResolvedType t) {
  // Inline lambda to check if a string ends with a suffix
  auto endsWith = [](const std::string &str,
                     const std::string &suffix) -> bool {
    if (str.length() < suffix.length())
      return false;
    return str.compare(str.length() - suffix.length(), suffix.length(),
                       suffix) == 0;
  };

  auto refType = [&](DataType baseType, bool isRef, std::string baseName) {
    if (isRef) {
      if (!endsWith(baseName, "_ref"))
        baseName += "_ref";
    } else {
      if (endsWith(baseName, "_ref"))
        baseName = baseName.substr(0, baseName.size() - 4);
    }

    ResolvedType type;
    type.kind = baseType;
    type.isRef = isRef;
    type.resolvedName = baseName;
    type.isArray = t.isArray;
    type.isPointer = false; // A reference cannot be a pointer

    return type;

    return ResolvedType{baseType, baseName, false, isRef};
  };

  return refType(t.kind, t.isRef, t.resolvedName);
}

ResolvedType Semantics::peelRef(ResolvedType t) {
  if (t.isRef) {
    t.isRef = false;
    t.resolvedName = stripRefSuffix(t.resolvedName);
  }
  return t;
}

std::string Semantics::generateLifetimeID(Node *declarationNode) {
  if (auto letStmt = dynamic_cast<LetStatement *>(declarationNode))
    return "L" + std::to_string(letDeclCount++);
  else if (auto ptrStmt = dynamic_cast<PointerStatement *>(declarationNode))
    return "P" + std::to_string(ptrDeclCount++);

  return "NO ID";
}

std::unique_ptr<LifeTime>
Semantics::createLifeTimeTracker(Node *declarationNode, LifeTime *targetBaton,
                                 const std::shared_ptr<SymbolInfo> &declSym) {
  logInternal("Creating lifetime baton...");
  auto lifetime = std::make_unique<LifeTime>();
  lifetime->ID = generateLifetimeID(declarationNode);
  lifetime->isResponsible = true;

  if (declSym->targetSymbol && targetBaton)
    transferResponsibility(lifetime.get(), targetBaton, declSym->targetSymbol);

  logInternal("Created baton: " + lifetime->ID +
              " for Node: " + declarationNode->toString());
  return lifetime;
}

void Semantics::transferResponsibility(
    LifeTime *currentBaton, LifeTime *targetBaton,
    const std::shared_ptr<SymbolInfo> &targetSym) {
  logInternal("Disarming target briefcase " + targetBaton->ID + " into " +
              currentBaton->ID);
  auto targetPointerCount = targetSym->pointerCount;
  // Check what happens to the pointer count if we subtract 1 from it
  if ((targetPointerCount - 1) == 0) {
    targetBaton->isResponsible = false;
    currentBaton->dependents[targetBaton->ID] = targetSym;

    for (auto const &[id, sym] : targetBaton->dependents) {
      currentBaton->dependents[id] = sym;
    }
  }
}

Node *Semantics::queryForLifeTimeBaton(const std::string &familyID) {
  for (const auto &[node, baton] : responsibilityTable) {
    // If the node has the briefcase
    if (baton) {
      // Check if we are in the same family
      if (familyID == baton->ID) {
        // If we are please tell me the node holding it
        return node;
      }
      continue;
    }
    continue;
  }
  return nullptr;
}

// This function enforces the pointing rules(Can only point upwards)
bool Semantics::validateLatticeMove(const StorageType &holderType,
                                    const StorageType &targetType) {
  if (holderType > targetType) {
    return false;
  }
  return true;
}

StorageType Semantics::determineTier(const std::shared_ptr<SymbolInfo> &sym) {
  // Just retreive the storage policy
  return sym->storage;
}

std::string Semantics::storageStr(const StorageType &storage) {
  switch (storage) {
  case StorageType::GLOBAL:
    return "global";
  case StorageType::SAGE:
    return "sage";
  case StorageType::HEAP:
    return "heap";
  case StorageType::STACK:
    return "stack";
  default:
    return "none";
  }
}

void Semantics::popScope() {
  auto &scope = symbolTable.back();
  for (auto &[name, sym] : scope) {
    // If the symbol we come across is a reference and it is referencing validly
    if (sym->isRef && sym->refereeSymbol) {
      logInternal("Initial refCount :" +
                  std::to_string(sym->refereeSymbol->refCount));
      if (sym->refereeSymbol->refCount > 0) {
        sym->refereeSymbol->refCount -= 1;
        logInternal("Ref '" + name +
                    "' relseased its target the refCount now " +
                    std::to_string(sym->refereeSymbol->refCount));
      }
    } else if (sym->isPointer && sym->targetSymbol &&
               sym->storage == StorageType::STACK) {
      logInternal("Initial pointer count: " +
                  std::to_string(sym->targetSymbol->pointerCount));
      if (sym->targetSymbol->pointerCount > 0) {
        sym->targetSymbol->pointerCount -= 1;
        logInternal("Stack Pointer '" + name +
                    "' realeased its target the pointer count is now " +
                    std::to_string(sym->targetSymbol->pointerCount));
      }
    }
  }
  symbolTable.pop_back();
}

std::shared_ptr<SymbolInfo> Semantics::getSymbolFromMeta(Node *node) {
  auto it = metaData.find(node);
  if (it == metaData.end()) {
    return nullptr;
  }

  return it->second;
}

void Semantics::logSemanticErrors(const std::string &message, int tokenLine,
                                  int tokenColumn) {
  hasFailed = true;
  CompilerError error;
  error.level = ErrorLevel::SEMANTIC;
  error.line = tokenLine;
  error.col = tokenColumn;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);
}

void Semantics::reportDevBug(const std::string &message) {
  hasFailed = true;
  std::cerr << COLOR_RED << "[INTERNAL COMPILER ERROR] " << COLOR_RESET
            << message << "\n";
}

void Semantics::logInternal(const std::string &message) {
  if (verbose) {
    std::cout << message << "\n";
  }
}

bool Semantics::failed() { return hasFailed; }
