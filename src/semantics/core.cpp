#include <cstdint>
#include <cstdlib>
#include <memory>
#include <string>
#include <unordered_set>

#include "ast.hpp"
#include "semantics.hpp"
#include "token.hpp"

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"

Semantics::Semantics(Deserializer& deserial, ErrorHandler& handler, bool verbose)
    : errorHandler(handler), deserializer(deserial), verbose(verbose) {
    symbolTable.push_back({});
    registerWalkerFunctions();

    import();
    registerInbuiltAllocatorTypes();  // Register the inbuilt allocators(malloc for
                                      // now)
}

// Main walker function
void Semantics::walker(Node* node) {
    if (!node) return;

    logInternal("Analyzing AST node: " + node->toString());

    std::string nodeName = typeid(*node).name();
    auto walkerIt = walkerFunctionsMap.find(typeid(*node));

    if (walkerIt != walkerFunctionsMap.end()) {
        logInternal("Reset walker error state flag");
        hasError = false;
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
    walkerFunctionsMap[typeid(INTLiteral)] = &Semantics::walkINTLiteral;
    walkerFunctionsMap[typeid(F32Literal)] = &Semantics::walkF32Literal;
    walkerFunctionsMap[typeid(F64Literal)] = &Semantics::walkF64Literal;
    walkerFunctionsMap[typeid(FloatLiteral)] = &Semantics::walkFloatLiteral;
    walkerFunctionsMap[typeid(StringLiteral)] = &Semantics::walkStringLiteral;
    walkerFunctionsMap[typeid(FStringLiteral)] = &Semantics::walkFStringLiteral;

    walkerFunctionsMap[typeid(Char8Literal)] = &Semantics::walkChar8Literal;
    walkerFunctionsMap[typeid(Char16Literal)] = &Semantics::walkChar16Literal;
    walkerFunctionsMap[typeid(Char32Literal)] = &Semantics::walkChar32Literal;

    walkerFunctionsMap[typeid(BooleanLiteral)] = &Semantics::walkBooleanLiteral;
    walkerFunctionsMap[typeid(Identifier)] = &Semantics::walkIdentifierExpression;
    walkerFunctionsMap[typeid(AddressExpression)] = &Semantics::walkAddressExpression;
    walkerFunctionsMap[typeid(DereferenceExpression)] = &Semantics::walkDereferenceExpression;
    walkerFunctionsMap[typeid(SizeOfExpression)] = &Semantics::walkSizeOfExpression;

    walkerFunctionsMap[typeid(NullLiteral)] = &Semantics::walkNullLiteral;
    walkerFunctionsMap[typeid(CastExpression)] = &Semantics::walkCastExpression;
    walkerFunctionsMap[typeid(BitcastExpression)] = &Semantics::walkBitcastExpression;

    walkerFunctionsMap[typeid(ArrayLiteral)] = &Semantics::walkArrayLiteral;
    walkerFunctionsMap[typeid(ArraySubscript)] = &Semantics::walkArraySubscriptExpression;

    // Walker registration for let statement and assignment statements
    walkerFunctionsMap[typeid(VariableDeclaration)] = &Semantics::walkVariableDeclaration;
    walkerFunctionsMap[typeid(AssignmentStatement)] = &Semantics::walkAssignStatement;
    walkerFunctionsMap[typeid(FieldAssignment)] = &Semantics::walkFieldAssignmentStatement;

    // Walker registration for control flow
    walkerFunctionsMap[typeid(ifStatement)] = &Semantics::walkIfStatement;
    walkerFunctionsMap[typeid(SwitchStatement)] = &Semantics::walkSwitchStatement;

    // Loop disruption statements
    walkerFunctionsMap[typeid(BreakStatement)] = &Semantics::walkBreakStatement;
    walkerFunctionsMap[typeid(ContinueStatement)] = &Semantics::walkContinueStatement;

    // Walker registration for functions
    walkerFunctionsMap[typeid(FunctionStatement)] = &Semantics::walkFunctionStatement;
    walkerFunctionsMap[typeid(FunctionExpression)] = &Semantics::walkFunctionExpression;
    walkerFunctionsMap[typeid(FunctionDeclaration)] = &Semantics::walkFunctionDeclarationStatement;
    walkerFunctionsMap[typeid(FunctionDeclarationExpression)] =
        &Semantics::walkFunctionDeclarationExpression;
    walkerFunctionsMap[typeid(CallExpression)] = &Semantics::walkFunctionCallExpression;
    walkerFunctionsMap[typeid(UnwrapExpression)] = &Semantics::walkUnwrapExpression;
    walkerFunctionsMap[typeid(ReturnStatement)] = &Semantics::walkReturnStatement;

    // Walker registration for type expressions
    walkerFunctionsMap[typeid(BasicType)] = &Semantics::walkBasicType;
    // Walker registration for loops
    walkerFunctionsMap[typeid(WhileStatement)] = &Semantics::walkWhileStatement;
    walkerFunctionsMap[typeid(ForStatement)] = &Semantics::walkForStatement;

    // Walker registration for blocks
    walkerFunctionsMap[typeid(BlockStatement)] = &Semantics::walkBlockStatement;
    walkerFunctionsMap[typeid(BlockExpression)] = &Semantics::walkBlockExpression;

    // Walker registration for the main expression types
    walkerFunctionsMap[typeid(InfixExpression)] = &Semantics::walkInfixExpression;
    walkerFunctionsMap[typeid(PrefixExpression)] = &Semantics::walkPrefixExpression;
    walkerFunctionsMap[typeid(PostfixExpression)] = &Semantics::walkPostfixExpression;

    walkerFunctionsMap[typeid(ExpressionStatement)] = &Semantics::walkExpressionStatement;

    // Walker registration for the component system
    walkerFunctionsMap[typeid(RecordStatement)] = &Semantics::walkRecordStatement;
    walkerFunctionsMap[typeid(ComponentStatement)] = &Semantics::walkComponentStatement;
    walkerFunctionsMap[typeid(NewComponentExpression)] = &Semantics::walkNewComponentExpression;
    walkerFunctionsMap[typeid(SelfExpression)] = &Semantics::walkSelfExpression;
    walkerFunctionsMap[typeid(EnumStatement)] = &Semantics::walkEnumStatement;
    walkerFunctionsMap[typeid(InstanceExpression)] = &Semantics::walkInstanceExpression;
    walkerFunctionsMap[typeid(MethodCallExpression)] = &Semantics::walkMethodCallExpression;

    walkerFunctionsMap[typeid(AllocatorStatement)] = &Semantics::walkAllocatorInterface;
    walkerFunctionsMap[typeid(SealStatement)] = &Semantics::walkSealStatement;

    // Walker registration for the trace system
    walkerFunctionsMap[typeid(TraceStatement)] = &Semantics::walkTraceStatement;
    // Walker registrartion for generic system
    walkerFunctionsMap[typeid(GenericStatement)] = &Semantics::walkGenericStatement;
    walkerFunctionsMap[typeid(InstantiateStatement)] = &Semantics::walkInstantiateStatement;
}

ResolvedType Semantics::inferNodeDataType(Node* node) {
    auto errorType = ResolvedType::error();
    auto unknownType = ResolvedType::unknown();

    if (!node) return unknownType;

    if (dynamic_cast<I8Literal*>(node)) return ResolvedType::makeBase(DataType::I8, "i8");
    if (dynamic_cast<U8Literal*>(node)) return ResolvedType::makeBase(DataType::U8, "u8");
    if (dynamic_cast<I16Literal*>(node)) return ResolvedType::makeBase(DataType::I16, "i16");
    if (dynamic_cast<U16Literal*>(node)) return ResolvedType::makeBase(DataType::U16, "u16");
    if (dynamic_cast<I32Literal*>(node)) return ResolvedType::makeBase(DataType::I32, "i32");
    if (dynamic_cast<U32Literal*>(node)) return ResolvedType::makeBase(DataType::U32, "u32");
    if (dynamic_cast<I64Literal*>(node)) return ResolvedType::makeBase(DataType::I64, "i64");
    if (dynamic_cast<U64Literal*>(node)) return ResolvedType::makeBase(DataType::U64, "u64");
    if (dynamic_cast<I128Literal*>(node)) return ResolvedType::makeBase(DataType::I128, "i128");
    if (dynamic_cast<U128Literal*>(node)) return ResolvedType::makeBase(DataType::U128, "u128");
    if (dynamic_cast<ISIZELiteral*>(node)) return ResolvedType::makeBase(DataType::ISIZE, "isize");
    if (dynamic_cast<USIZELiteral*>(node)) return ResolvedType::makeBase(DataType::USIZE, "usize");
    if (dynamic_cast<INTLiteral*>(node)) {
        // Make it default to i32 but it can be changed based off integer context
        return ResolvedType::makeBase(DataType::I32, "i32");
    }

    if (dynamic_cast<F32Literal*>(node)) return ResolvedType::makeBase(DataType::F32, "f32");
    if (dynamic_cast<F64Literal*>(node)) return ResolvedType::makeBase(DataType::F64, "f64");
    if (dynamic_cast<FloatLiteral*>(node)) return ResolvedType::makeBase(DataType::F32, "f32");

    if (dynamic_cast<StringLiteral*>(node))
        return ResolvedType::makeBase(DataType::STRING, "string");
    if (dynamic_cast<FStringLiteral*>(node))
        return ResolvedType::makeBase(DataType::STRING, "string");

    if (dynamic_cast<Char8Literal*>(node)) return ResolvedType::makeBase(DataType::CHAR8, "char8");
    if (dynamic_cast<Char16Literal*>(node))
        return ResolvedType::makeBase(DataType::CHAR16, "char16");
    if (dynamic_cast<Char32Literal*>(node))
        return ResolvedType::makeBase(DataType::CHAR32, "char32");

    if (dynamic_cast<BooleanLiteral*>(node))
        return ResolvedType::makeBase(DataType::BOOLEAN, "bool");

    if (dynamic_cast<SizeOfExpression*>(node))
        return ResolvedType::makeBase(DataType::USIZE, "usize");

    if (auto castExpr = dynamic_cast<CastExpression*>(node)) {
        auto type = castExpr->type.get();
        if (type) {
            auto destinationType = inferNodeDataType(type);
            return destinationType;
        }
        return unknownType;
    }

    if (auto bitcastExpr = dynamic_cast<BitcastExpression*>(node)) {
        auto type = bitcastExpr->type.get();
        if (type) {
            auto destinationType = inferNodeDataType(type);
            logInternal(
                "Bitcast dest type: modifier=" +
                std::to_string(static_cast<int>(destinationType.modifier)) +
                " kind=" + std::to_string((int)destinationType.kind) +
                " resolvedName=" + destinationType.resolvedName + " innerType=" +
                (destinationType.innerType ? destinationType.innerType->resolvedName : "null"));
            return destinationType;
        }

        return errorType;
    }

    if (auto arrLit = dynamic_cast<ArrayLiteral*>(node)) {
        if (arrLit->array.empty()) {
            return makeArrayType(errorType, 0, false);
        }

        ResolvedType firstType = inferNodeDataType(arrLit->array[0].get());

        for (size_t i = 1; i < arrLit->array.size(); ++i) {
            ResolvedType elemType = inferNodeDataType(arrLit->array[i].get());
            if (!isTypeCompatible(firstType, elemType)) {
                errorHandler.addHint("All array elements must be the same type")
                    .addHint("Expected type: " + firstType.resolvedName)
                    .addHint("Got type: " + elemType.resolvedName);
                logSemanticErrors(
                    "Type mismatch of array member at index '" + std::to_string(i) + "'", arrLit);
                return errorType;
            }
        }

        return makeArrayType(firstType, arrLit->array.size(), false);
    }

    if (auto arrAccess = dynamic_cast<ArraySubscript*>(node)) {
        auto arrayName = arrAccess->identifier->expression.TokenLiteral;
        auto arrSym = resolveSymbolInfo(arrayName);

        if (!arrSym) {
            logSemanticErrors("Unidentified variable '" + arrayName + "'",
                              arrAccess->identifier.get());
            return errorType;
        }

        if (!arrSym->type().type.isArray()) {
            errorHandler.addHint("Only array types can be subscripted with '[]'")
                .addHint("Declare as: arr[N] i32 " + arrayName)
                .addHint("Or initialize with an array literal");
            logSemanticErrors(
                "Cannot index into non-array type '" + arrSym->type().type.resolvedName + "'",
                arrAccess->identifier.get());
            return errorType;
        }

        if (arrSym->type().type.isNull) {
            errorHandler.addHint("Unwrap the nullable array before indexing")
                .addHint("Use '?''?' or 'unwrap' to get the non-nullable value");
            logSemanticErrors("Cannot index into nullable array '" + arrayName + "'",
                              arrAccess->identifier.get());
            return errorType;
        }

        // Return the element type — unwrap one array level
        if (!arrSym->type().type.innerType) return errorType;
        return *arrSym->type().type.innerType;
    }

    if (auto selfExpr = dynamic_cast<SelfExpression*>(node)) {
        // Start: find the type of the component containing this method
        if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
            return unknownType;

        std::string currentTypeName = currentTypeStack.back().typeName;

        // Walk through the chain of fields: pos -> x -> y
        for (const auto& field : selfExpr->fields) {
            auto ident = dynamic_cast<Identifier*>(field.get());
            if (!ident) return unknownType;

            const std::string fieldName = ident->identifier.TokenLiteral;

            // Look up this type's custom definition
            auto ctIt = customTypesTable.find(currentTypeName);
            if (ctIt == customTypesTable.end()) return unknownType;

            // Find member in this component
            auto& members = ctIt->second->members;
            auto memIt = members.find(fieldName);
            if (memIt == members.end()) return unknownType;

            // Get the member type
            const auto& memberInfo = memIt->second;
            currentTypeName = memberInfo->type.resolvedName;  // move deeper

            // Last field: return its full type
            if (&field == &selfExpr->fields.back()) return memberInfo->type;
        }

        return unknownType;
    }

    if (auto instExpr = dynamic_cast<InstanceExpression*>(node)) {
        auto instName = instExpr->blockIdent->expression.TokenLiteral;

        auto sym = resolveSymbolInfo(instName);
        if (!sym) {
            logSemanticErrors("Failed to infer type for unidentified identifier '" + instName + "'",
                              instExpr->blockIdent.get());
            return errorType;
        }

        return sym->type().type;
    }

    // Dealing with the variable declaration
    if (auto varDecl = dynamic_cast<VariableDeclaration*>(node)) {
        auto baseType = inferDeclarationBaseType(varDecl);
        return resolveTypeWithModifier(varDecl->modified_type.get(), baseType);
    }

    if (auto assignStmt = dynamic_cast<AssignmentStatement*>(node)) {
        std::string nameToResolve;

        if (auto selfExpr = dynamic_cast<SelfExpression*>(assignStmt->identifier.get())) {
            if (selfExpr->fields.empty()) {
                logSemanticErrors("Invalid 'self' access in assignment",
                                  assignStmt->identifier.get());
                return errorType;
            }

            // Grab the LAST field in the chain
            auto lastField = selfExpr->fields.back().get();
            auto ident = dynamic_cast<Identifier*>(lastField);
            if (!ident) {
                logSemanticErrors("Expected identifier in 'self' field chain",
                                  assignStmt->identifier.get());
                return errorType;
            }

            nameToResolve = ident->identifier.TokenLiteral;
        } else {
            nameToResolve = assignStmt->identifier->expression.TokenLiteral;
        }
        logInternal("Name being used to resolve inside inferer '" + nameToResolve + "'");
        auto assignSymbol = resolveSymbolInfo(nameToResolve);
        auto assignStmtVal = assignStmt->value.get();
        ResolvedType assignStmtValType = inferNodeDataType(assignStmtVal);
        if (!isTypeCompatible(assignSymbol->type().type, assignStmtValType)) {
            logSemanticErrors("Type mismatch expected '" + assignStmtValType.resolvedName +
                                  "' but got '" + assignSymbol->type().type.resolvedName + "'",
                              assignStmt->identifier.get());
        } else {
            return assignSymbol->type().type;
        }
    }

    if (auto newExpr = dynamic_cast<NewComponentExpression*>(node)) {
        auto componentName = newExpr->component_name.TokenLiteral;
        auto componentIt = customTypesTable.find(componentName);
        if (componentIt == customTypesTable.end()) {
            logSemanticErrors("Component '" + componentName + "' does not exist", newExpr);
            return errorType;
        }
        return componentIt->second->type;
    }

    if (auto infixExpr = dynamic_cast<InfixExpression*>(node)) {
        return inferInfixExpressionType(infixExpr);
    }

    if (auto prefixExpr = dynamic_cast<PrefixExpression*>(node)) {
        return inferPrefixExpressionType(prefixExpr);
    }

    if (auto postfixExpr = dynamic_cast<PostfixExpression*>(node)) {
        return inferPostfixExpressionType(postfixExpr);
    }

    if (dynamic_cast<NullLiteral*>(node))
        return ResolvedType::null();  // Will be updated based on context

    if (auto ident = dynamic_cast<Identifier*>(node)) {
        std::string name = ident->identifier.TokenLiteral;
        logInternal("Identifier name in the inferer '" + name + "'");
        auto symbol = resolveSymbolInfo(name);
        if (symbol) {
            logInternal("Identifier Data Type '" + symbol->type().type.resolvedName + "'");
            return symbol->type().type;
        } else {
            logSemanticErrors("Undefined variable '" + name + "'", ident);
            return errorType;
        }
    }

    if (auto derefExpr = dynamic_cast<DereferenceExpression*>(node)) {
        std::string name = extractIdentifierName(derefExpr->identifier.get());
        auto derefSym = resolveSymbolInfo(name);
        if (!derefSym) {
            logSemanticErrors("Undefined variable '" + name + "'", derefExpr->identifier.get());
            return errorType;
        }
        // Must actually be a pointer to deref
        if (!derefSym->type().type.isPointer()) {
            errorHandler.addHint("Only pointer types can be dereferenced")
                .addHint("Use 'addr' to get a pointer first")
                .addHint("Example: ptr i32 p -> addr x, then deref p");
            logSemanticErrors("Cannot dereference non-pointer variable '" + name + "'",
                              derefExpr->identifier.get());
            return errorType;
        }
        // Unwrap one pointer level return what it points to
        if (!derefSym->type().type.innerType) return errorType;
        return *derefSym->type().type.innerType;
    }

    if (auto addrExpr = dynamic_cast<AddressExpression*>(node)) {
        auto innerType = inferNodeDataType(addrExpr->identifier.get());
        return makePointerType(innerType, false);
    }

    if (auto basicType = dynamic_cast<BasicType*>(node))
        return tokenTypeToResolvedType(basicType->data_token, basicType->isNullable);

    if (auto retType = dynamic_cast<ReturnType*>(node)) {
        if (retType->isVoid) return ResolvedType::makeBase(DataType::VOID, "void");

        auto baseType = inferNodeDataType(retType->base_type.get());
        return resolveTypeWithModifier(retType->modified_type.get(), baseType);
    }

    if (auto callExpr = dynamic_cast<CallExpression*>(node)) {
        auto symbol = resolveSymbolInfo(callExpr->function_identifier->expression.TokenLiteral);
        if (symbol) {
            return symbol->type().type;
        } else {
            logSemanticErrors("Undefined function name '" +
                                  callExpr->function_identifier->expression.TokenLiteral + "'",
                              callExpr->function_identifier.get());
            return errorType;
        }
    }

    if (auto metCall = dynamic_cast<MethodCallExpression*>(node)) {
        auto instanceName = metCall->instance->expression.TokenLiteral;

        auto instanceSym = resolveSymbolInfo(instanceName);

        if (!instanceSym) {
            logSemanticErrors("Undefined instance name '" + instanceName + "' ",
                              metCall->instance.get());
            return errorType;
        }

        auto call = dynamic_cast<CallExpression*>(metCall->call.get());
        auto callName = call->function_identifier->expression.TokenLiteral;

        // Search using custom types table to get the members
        // Get the type
        auto type = instanceSym->type().type;
        // Check the customTypes table
        auto typeIt = customTypesTable.find(type.resolvedName);
        auto sealIt = sealTable.find(instanceName);
        if (typeIt != customTypesTable.end()) {
            auto members = typeIt->second->members;
            auto it = members.find(callName);
            if (it == members.end()) {
                logSemanticErrors(
                    "Function '" + callName + "' doesnt exist in type '" + type.resolvedName + "'",
                    call->function_identifier.get());
                return errorType;
            }

            auto memInfo = it->second;
            return memInfo->type;
        } else if (sealIt != sealTable.end()) {
            auto sealFnMap = sealIt->second;
            auto sealFnIt = sealFnMap.find(callName);
            if (sealFnIt == sealFnMap.end()) {
                logSemanticErrors(
                    "Function '" + callName + "' doesnt exist in seal '" + instanceName + "'",
                    call->function_identifier.get());
                return errorType;
            }

            auto sealInfo = sealFnIt->second;
            return sealInfo->type().type;
        } else {
            logSemanticErrors("Unknown type or seal '" + instanceName + "'",
                              call->function_identifier.get());
            return errorType;
        }
    }
    if (auto unwrapExpr = dynamic_cast<UnwrapExpression*>(node)) {
        auto exprType = inferNodeDataType(unwrapExpr->expr.get());
        exprType.isNull = false;
        auto strippedName = getBaseTypeName(exprType);
        exprType.resolvedName = strippedName;

        return exprType;
    }

    return unknownType;
}

ResolvedType Semantics::resolveTypeWithModifier(Node* modifier, const ResolvedType& base) {
    if (!modifier) return base;

    auto tyMod = dynamic_cast<TypeModifier*>(modifier);
    if (!tyMod) return base;

    // Recursively resolve inner first
    ResolvedType inner = base;
    if (tyMod->inner_modifier) inner = resolveTypeWithModifier(tyMod->inner_modifier.get(), base);

    // Build outer layer wrapping inner
    ResolvedType outer;
    outer.isNull = base.isNull;
    outer.kind = base.kind;
    outer.innerType = std::make_shared<ResolvedType>(inner);

    if (tyMod->isPointer) {
        outer.modifier = Modifier::POINTER;
        outer.resolvedName = "ptr<" + inner.resolvedName + ">";
    } else if (tyMod->isReference) {
        outer.modifier = Modifier::REFERENCE;
        outer.resolvedName = "ref<" + inner.resolvedName + ">";
    } else if (tyMod->isArray) {
        outer.modifier = Modifier::ARRAY;
        // Grab dimension if constant
        if (!tyMod->dimensions.empty()) {
            if (isIntegerConstant(tyMod->dimensions[0].get()))
                outer.arraySize = getIntegerConstant(tyMod->dimensions[0].get());
        }
        outer.resolvedName = "arr[" + (outer.arraySize ? std::to_string(outer.arraySize) : "?") +
                             "]<" + inner.resolvedName + ">";
    }

    return outer;
}

std::string Semantics::extractDeclarationName(Node* node) {
    std::string declName = "<Unknown name>";
    if (auto decl = dynamic_cast<VariableDeclaration*>(node))
        declName = decl->var_name->expression.TokenLiteral;

    return declName;
}

std::string Semantics::extractIdentifierName(Node* node) {
    std::string identName = "<Unsupported node>";
    if (auto call = dynamic_cast<CallExpression*>(node)) {
        auto callIdent = dynamic_cast<Identifier*>(call->function_identifier.get());
        identName = callIdent->identifier.TokenLiteral;
        return identName;
    } else if (auto ident = dynamic_cast<Identifier*>(node)) {
        identName = ident->identifier.TokenLiteral;
        return identName;
    } else if (dynamic_cast<MethodCallExpression*>(node)) {
        identName = "<methodfuncName>";  // Place holder for now
        return identName;
    } else if (auto deref = dynamic_cast<DereferenceExpression*>(node)) {
        identName = extractIdentifierName(deref->identifier.get());
        return identName;
    } else if (auto addr = dynamic_cast<AddressExpression*>(node)) {
        identName = extractIdentifierName(addr->identifier.get());
        return identName;
    } else if (auto infix = dynamic_cast<InfixExpression*>(node)) {
        identName = extractIdentifierName(infix->right_operand.get());
        return identName;
    } else if (auto selfExpr = dynamic_cast<SelfExpression*>(node)) {
        if (!selfExpr->fields.empty()) {
            // Get the last field in the chain (e.g., self.a.b.target -> target)
            identName = extractIdentifierName(selfExpr->fields.back().get());
        } else {
            identName = "self";  // Just the component itself
        }
        return identName;
    } else if (auto unwrapExpr = dynamic_cast<UnwrapExpression*>(node)) {
        identName = extractIdentifierName(unwrapExpr->expr.get());
        return identName;
    } else if (auto arrIndex = dynamic_cast<ArraySubscript*>(node)) {
        identName = extractIdentifierName(arrIndex->identifier.get());
        return identName;
    } else if (auto prefix = dynamic_cast<PrefixExpression*>(node)) {
        identName = extractIdentifierName(prefix->operand.get());
        return identName;
    } else if (auto postfix = dynamic_cast<PostfixExpression*>(node)) {
        identName = extractIdentifierName(postfix->operand.get());
        return identName;
    }

    return identName;
}

ResolvedType Semantics::inferInfixExpressionType(Node* node) {
    auto infixNode = dynamic_cast<InfixExpression*>(node);
    if (!infixNode) return ResolvedType::unknown();

    // Incase the left side is an identifier
    auto ident = dynamic_cast<Identifier*>(infixNode->left_operand.get());
    std::shared_ptr<SymbolInfo> symbol = nullptr;
    if (ident) {
        symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
    }

    TokenType operatorType = infixNode->operat.type;

    // Special case: scope or dot
    if (operatorType == TokenType::FULLSTOP || operatorType == TokenType::SCOPE_OPERATOR) {
        auto parentType = inferNodeDataType(infixNode->left_operand.get());
        std::string childName = extractIdentifierName(infixNode->right_operand.get());
        auto result = resultOfScopeOrDot(operatorType, parentType, childName, infixNode);
        if (result) {
            return result->type().type;
        } else {
            return ResolvedType::error();
        }
    }

    // --- Regular binary operator ---
    ResolvedType leftType = inferNodeDataType(infixNode->left_operand.get());
    ResolvedType rightType = inferNodeDataType(infixNode->right_operand.get());

    if (operatorType == TokenType::COALESCE) {
        if (!leftType.isNull) {
            logSemanticErrors("Left-hand side of coalesce must be nullable",
                              infixNode->left_operand.get());
            return ResolvedType::error();
        }

        // Right-hand side type
        rightType = inferNodeDataType(infixNode->right_operand.get());
        ResolvedType baseType = leftType;
        baseType.isNull = false;
        auto strippedName = getBaseTypeName(baseType);
        baseType.resolvedName = strippedName;
        //If the right operand is a generic int or float give it context
        if(dynamic_cast<INTLiteral*>(infixNode->right_operand.get())||dynamic_cast<FloatLiteral*>(infixNode->right_operand.get())){
            rightType=baseType;
        }

        if (!isTypeCompatible(baseType, rightType)) {
            logSemanticErrors("Type of fallback in coalesce does not match nullable type '" +
                                  baseType.resolvedName + "' must match '" +
                                  rightType.resolvedName + "'",
                              ident);
            return ResolvedType::error();
        }

        // Result is the underlying type
        return baseType;
    }

    if (ident) {
        auto symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
        if (!symbol) {
            logSemanticErrors("Undefined variable '" + ident->identifier.TokenLiteral + "'", ident);
            return ResolvedType::error();
        }
        if (symbol->type().isDefinitelyNull) {
            logSemanticErrors("Cannot use definitely-null variable '" +
                                  ident->identifier.TokenLiteral + "' in operations",
                              ident);
            symbol->hasError = true;
            return ResolvedType::error();
        }
    }

    ResolvedType peeledLeft = peelRef(leftType);
    ResolvedType peeledRight = peelRef(rightType);

    auto infixType = resultOfBinary(operatorType, peeledLeft, peeledRight, infixNode);
    return infixType;  // Return the correct type
}

ResolvedType Semantics::inferPrefixExpressionType(Node* node) {
    auto prefixNode = dynamic_cast<PrefixExpression*>(node);
    if (!prefixNode) return ResolvedType::error();
    auto prefixOperator = prefixNode->operat.type;
    ResolvedType operandType = inferNodeDataType(prefixNode->operand.get());
    return resultOfUnary(prefixOperator, operandType, prefixNode);
}

ResolvedType Semantics::inferPostfixExpressionType(Node* node) {
    auto postfixNode = dynamic_cast<PostfixExpression*>(node);
    if (!postfixNode) return ResolvedType::error();
    ResolvedType operandType = inferNodeDataType(postfixNode->operand.get());
    auto postfixOperator = postfixNode->operator_token.type;
    return resultOfUnary(postfixOperator, operandType, postfixNode);
}

std::shared_ptr<SymbolInfo> Semantics::resultOfScopeOrDot(TokenType operatorType,
                                                          const ResolvedType& parentType,
                                                          const std::string& childName,
                                                          InfixExpression* infixExpr) {
    if (operatorType != TokenType::FULLSTOP && operatorType != TokenType::SCOPE_OPERATOR)
        return nullptr;

    // If the type is a pointer strip the name and allow for access
    std::string lookUpName = getBaseTypeName(parentType);

    // Block nullable access
    if (parentType.isNull) {
        logSemanticErrors("Cannot access a nullable type '" + lookUpName + "'",
                          infixExpr->left_operand.get());
        return nullptr;
    }

    if (operatorType == TokenType::FULLSTOP) {
        if (parentType.kind == DataType::ENUM) {
            logSemanticErrors("Dot operator applied to enum use (::) to access",
                              infixExpr->left_operand.get());
            return nullptr;
        }

        // Look up the type definition in customTypesTable
        auto typeIt = customTypesTable.find(lookUpName);
        if (typeIt == customTypesTable.end()) {
            logSemanticErrors("Type '" + lookUpName + "' not found", infixExpr->left_operand.get());
            return nullptr;
        }

        // Look for childName in members
        auto memberIt = typeIt->second->members.find(childName);
        if (memberIt == typeIt->second->members.end()) {
            logSemanticErrors("Type '" + lookUpName + "' has no member '" + childName + "'",
                              infixExpr->right_operand.get());
            return nullptr;
        }

        auto memberInfo = memberIt->second;
        auto dotResult = std::make_shared<SymbolInfo>();
        dotResult->type().type = memberInfo->type;
        dotResult->storage().isConstant = memberInfo->isConstant;
        dotResult->storage().isMutable = memberInfo->isMutable;
        dotResult->storage().isInitialized = memberInfo->isInitialised;
        dotResult->type().isNullable = memberInfo->isNullable;
        dotResult->type().isPointer = memberInfo->isPointer;
        dotResult->type().memberIndex = memberInfo->memberIndex;

        return dotResult;

    } else if (operatorType == TokenType::SCOPE_OPERATOR) {
        if (parentType.kind != DataType::ENUM) {
            logSemanticErrors("Scope operator(::) applied to none enum  variable",
                              infixExpr->left_operand.get());
            return nullptr;
        }

        // Look for the definition in the custom types table
        auto typeIt = customTypesTable.find(lookUpName);
        if (typeIt == customTypesTable.end()) {
            logSemanticErrors("Type '" + lookUpName + "' not found", infixExpr->left_operand.get());
            return nullptr;
        }

        // Look for the childName in members
        auto memIt = typeIt->second->members.find(childName);
        if (memIt == typeIt->second->members.end()) {
            logSemanticErrors(
                "Type '" + lookUpName + "' does not have a member '" + childName + "'",
                infixExpr->left_operand.get());
            return nullptr;
        }

        auto memInfo = memIt->second;
        auto scopeInfo = std::make_shared<SymbolInfo>();
        scopeInfo->type().type = memInfo->parentType;  // This is the actual enum
        scopeInfo->storage().isConstant = memInfo->isConstant;
        scopeInfo->storage().isMutable = memInfo->isMutable;
        scopeInfo->type().isNullable = memInfo->isNullable;
        scopeInfo->type().memberIndex = memInfo->memberIndex;

        return scopeInfo;
    }

    return nullptr;
}

ResolvedType Semantics::resultOfBinary(TokenType operatorType, ResolvedType leftType,
                                       ResolvedType rightType, Node* node) {
    auto infix = dynamic_cast<InfixExpression*>(node);
    std::string operatorStr = infix->operat.TokenLiteral;

    // Logical operators: &&, ||
    if (operatorType == TokenType::AND || operatorType == TokenType::OR) {
        if (isBoolean(leftType) && isBoolean(rightType))
            return ResolvedType::makeBase(DataType::BOOLEAN, "boolean");
        else
            return ResolvedType::error();
    }

    // The odds of this happening are low unless the parser messed up
    if (operatorType == TokenType::ASSIGN) {
        logSemanticErrors(
            "Cannot use '" + operatorStr + "'in binary operations. It is reserved for assignments",
            infix->left_operand.get());
        return ResolvedType::error();
    }

    // Comparison operators
    bool isComparison =
        (operatorType == TokenType::GREATER_THAN || operatorType == TokenType::GT_OR_EQ ||
         operatorType == TokenType::LESS_THAN || operatorType == TokenType::LT_OR_EQ ||
         operatorType == TokenType::EQUALS || operatorType == TokenType::NOT_EQUALS);

    if (isComparison) {
        if (leftType.isNull || rightType.isNull) {
            logSemanticErrors("Cannot carry out comparison on types '" + leftType.resolvedName +
                                  "' and '" + rightType.resolvedName +
                                  "' as one of them is nullable",
                              infix->left_operand.get());
            return ResolvedType::error();
        }

        if (leftType.kind == rightType.kind)
            return ResolvedType::makeBase(DataType::BOOLEAN, "boolean");
        logSemanticErrors(
            "Cannot compare '" + leftType.resolvedName + "' to '" + rightType.resolvedName + "'",
            infix->left_operand.get());
        return ResolvedType::error();
    }

    // String concatenation(Not complete but I will work on this)
    if (operatorType == TokenType::PLUS && isString(leftType) && isString(rightType)) {
        return ResolvedType::makeBase(DataType::STRING, "string");
    }

    // Arithmetic operators: +, -, %, /, *
    bool isArithmetic = (operatorType == TokenType::PLUS || operatorType == TokenType::MINUS ||
                         operatorType == TokenType::MODULUS || operatorType == TokenType::DIVIDE ||
                         operatorType == TokenType::ASTERISK);

    if (isArithmetic) {
        if (leftType.isNull || rightType.isNull) {
            logSemanticErrors("Cannot carry out arithmetic on types '" + leftType.resolvedName +
                                  "' and '" + rightType.resolvedName +
                                  "' as one of them is nullable",
                              infix->left_operand.get());
            return ResolvedType::error();
        }
        if (leftType.isPointer() && isInteger(rightType)) {
            if (operatorType == TokenType::PLUS || operatorType == TokenType::MINUS) {
                return leftType;  // Location +/- Distance = Location
            }
        }

        // Integer + Pointer = Pointer (Commutative Addition)
        if (isInteger(leftType) && rightType.isPointer()) {
            if (operatorType == TokenType::PLUS) {
                return rightType;
            }
        }

        //  Pointer - Pointer = USIZE (Distance between two points)
        if (leftType.isPointer() && rightType.isPointer() && operatorType == TokenType::MINUS) {
            return ResolvedType::makeBase(DataType::USIZE, "usize");
        }
        // Promote mixed integer/float combinations
        if ((isInteger(leftType) && isFloat(rightType)) ||
            (isFloat(leftType) && isInteger(rightType))) {
            return ResolvedType::makeBase(DataType::F32, "f32");
        }
        // Promote int/double or float/double to double
        if ((isInteger(leftType) && rightType.kind == DataType::F64) ||
            (leftType.kind == DataType::F64 && isInteger(rightType))) {
            return ResolvedType::makeBase(DataType::F64, "f64");
        }
        if ((leftType.kind == DataType::F32 && rightType.kind == DataType::F64) ||
            (leftType.kind == DataType::F64 && rightType.kind == DataType::F32)) {
            return ResolvedType::makeBase(DataType::F64, "f64");
        }

        if (leftType.kind == rightType.kind) {
            return leftType;
        }

        if (isTypeCompatible(leftType, rightType)) {
            return rightType;
        }

        logSemanticErrors("Type mismatch '" + leftType.resolvedName + "' does not match '" +
                              rightType.resolvedName + "'",
                          infix->left_operand.get());
        return ResolvedType::error();
    }

    bool isBitwise =
        (operatorType == TokenType::BITWISE_AND || operatorType == TokenType::BITWISE_OR ||
         operatorType == TokenType::BITWISE_XOR);

    if (isBitwise) {
        if (isInteger(leftType) && isInteger(rightType)) {
            if (leftType.kind == rightType.kind) {
                return leftType;
            }

            logSemanticErrors("Bitwise mismatch, Cannot use '" + operatorStr +
                                  "' on different integer types '" + leftType.resolvedName +
                                  "' and '" + rightType.resolvedName + "'",
                              infix->left_operand.get());
            return ResolvedType::error();
        }

        logSemanticErrors("Bitwise operations cannot be performed on '" + leftType.resolvedName +
                              "' and '" + rightType.resolvedName + "'",
                          infix->left_operand.get());
        return ResolvedType::error();
    }

    bool isShift =
        (operatorType == TokenType::SHIFT_RIGHT || operatorType == TokenType::SHIFT_LEFT);
    if (isShift) {
        if (isInteger(leftType) && isInteger(rightType)) {
            return leftType;
        }

        logSemanticErrors("Shift operators require integers on both sides  but got '" +
                              leftType.resolvedName + "' and '" + rightType.resolvedName + "'",
                          infix->left_operand.get());
        return ResolvedType::unknown();
    }

    logSemanticErrors("Unknown binary operator '" + operatorStr + "'", infix->left_operand.get());
    return ResolvedType::error();
}

ResolvedType Semantics::resultOfUnary(TokenType operatorType, const ResolvedType& operandType,
                                      Node* node) {
    std::string operatorStr;
    if (auto postfix = dynamic_cast<PostfixExpression*>(node))
        operatorStr = postfix->operator_token.TokenLiteral;
    else if (auto prefix = dynamic_cast<PrefixExpression*>(node))
        operatorStr = prefix->operat.TokenLiteral;

    switch (operatorType) {
        case TokenType::BANG: {
            if (isBoolean(operandType) || operandType.isPointer()) {
                return ResolvedType::makeBase(DataType::BOOLEAN, "bool");
            }
            logSemanticErrors(
                "Cannot apply '" + operatorStr + "' to type '" + operandType.resolvedName + "'",
                node);
            return ResolvedType::error();
        }
        case TokenType::MINUS:
        case TokenType::PLUS:
        case TokenType::PLUS_PLUS:
        case TokenType::MINUS_MINUS: {
            if (isInteger(operandType) || isFloat(operandType)) {
                return operandType;
            }

            logSemanticErrors(
                "Cannot apply '" + operatorStr + "' to type '" + operandType.resolvedName + "'",
                node);
            return ResolvedType::error();
        }
        case TokenType::BITWISE_NOT: {
            if (isInteger(operandType)) {
                return operandType;
            }

            logSemanticErrors("Bitwise operator '" + operatorStr +
                                  "' can only be applied to integer types you provided '" +
                                  operandType.resolvedName + "'",
                              node);
            return ResolvedType::error();
        }
        default:
            return ResolvedType::unknown();
    }
}

std::shared_ptr<SymbolInfo> Semantics::resolveSymbolInfo(const std::string& name) {
    for (int i = symbolTable.size() - 1; i >= 0; --i) {
        auto& scope = symbolTable[i];
        logInternal("Searching for '" + name + "' in scope level " + std::to_string(i));
        for (auto& [key, value] : scope) {
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

std::shared_ptr<SymbolInfo> Semantics::lookUpInCurrentScope(const std::string& name) {
    if (symbolTable.empty()) {
        reportDevBug("Attempted current scope look up on empty symbol table", nullptr);
        return nullptr;
    }

    // Access the current scope which is the latest scope in the symbol table
    auto& currentScope = symbolTable.back();

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
    auto makeType = [&](DataType nonNull, const std::string& baseName) {
        if (isNullable) {
            ResolvedType t;
            t.kind = nonNull;
            t.resolvedName = baseName + "?";
            t.isNull = isNullable;
            return t;
        } else
            return ResolvedType::makeBase(nonNull, baseName);
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
            return ResolvedType::makeBase(DataType::VOID, "void");

        case TokenType::IDENTIFIER: {
            auto parentIt = customTypesTable.find(token.TokenLiteral);
            if (parentIt != customTypesTable.end()) {
                auto parentType = parentIt->second->type;
                parentType.isNull = isNullable;
                return parentType;
            }

            logSpecialErrors("Unknown type '" + token.TokenLiteral + "'", token.line, token.column);
            return ResolvedType::unknown();
        }
        default:
            return ResolvedType::unknown();
    }
}

bool Semantics::isTypeCompatible(const ResolvedType& expected, const ResolvedType& actual) {
    // Error type always fails, don't silently pass broken types through
    if (actual.kind == DataType::ERROR || expected.kind == DataType::ERROR) return false;

    //Special case (a string, ptr<u8> and ptr<char8> are the same so just let it slide)
    if (actual.isBase() && actual.kind == DataType::STRING && 
        expected.isPointer() && expected.innerType) {
        if (expected.innerType->isBase() && 
            (expected.innerType->kind == DataType::U8 || 
             expected.innerType->kind == DataType::CHAR8)) {
            return true;
        }
    }
    
    // string -> ptr<char8> (if you have CHAR8 type)
    if (actual.isBase() && actual.kind == DataType::STRING && 
        expected.isPointer() && expected.innerType &&
        expected.innerType->isBase() && expected.innerType->kind == DataType::CHAR8) {
        return true;
    }

    // Opaque pointer,accepts any pointer
    if (expected.isBase() && expected.kind == DataType::OPAQUE) {
        // Only applies when expected is a bare opaque — not when recursing
        // into inner types of a ptr<opaque> comparison
        if (actual.isBase() && actual.kind == DataType::OPAQUE)
            return true;  // both are bare opaque, they match
        if (!actual.isPointer()) return false;
        if (!expected.isNull && actual.isNull) return false;
        return true;
    }

    // Nullability rules
    // Non-nullable cannot accept nullable
    if (!expected.isNull && actual.isNull) return false;
    // Nullable can accept non-nullable, safe widening
    // Both matching is fine too, handled by falling through

    // Modifier must match at this level
    if (expected.modifier != actual.modifier) return false;

    // Base case, both are concrete types
    if (expected.isBase() && actual.isBase()) {
        if (expected.kind != actual.kind) return false;
        // Custom types — compare by name
        if (expected.kind == DataType::COMPONENT || expected.kind == DataType::RECORD ||
            expected.kind == DataType::ENUM) {
            return expected.resolvedName == actual.resolvedName;
        }
        return true;
    }

    // Array check size compatibility then recurse
    if (expected.isArray() && actual.isArray()) {
        // If expected has a fixed size, actual must match
        // 0 means dynamic — dynamic is compatible with anything
        if (expected.arraySize != 0 && actual.arraySize != 0) {
            if (expected.arraySize != actual.arraySize) return false;
        }
        // Recurse into element type
        if (!expected.innerType || !actual.innerType) return false;
        return isTypeCompatible(*expected.innerType, *actual.innerType);
    }

    // Pointer, recurse into what they point to
    if ((expected.isPointer() && actual.isPointer())) {
        if (!expected.innerType || !actual.innerType) return false;
        return isTypeCompatible(*expected.innerType, *actual.innerType);
    }

    // Handle references they should be compatible if their targets are compatible
    if (expected.isRef() && actual.isRef()) {
        if (!expected.innerType || !actual.innerType) return false;
        return isTypeCompatible(*expected.innerType, *actual.innerType);
    }

    return false;
}

bool Semantics::hasReturnPath(Node* node) {
    if (currentFunction && currentFunction.value()->func().returnType.kind == DataType::VOID) {
        return true;  // Void functions don't need returns
    }

    if (auto blockStmt = dynamic_cast<BlockStatement*>(node)) {
        return hasReturnPathInBlock(blockStmt->statements);
    }

    if (auto blockExpr = dynamic_cast<BlockExpression*>(node)) {
        if (hasReturnPathInBlock(blockExpr->statements)) {
            return true;
        }

        if (blockExpr->finalexpr.has_value()) {
            ResolvedType exprType = inferNodeDataType(blockExpr->finalexpr.value().get());
            return exprType.kind == DataType::ERROR ||
                   isTypeCompatible(currentFunction.value()->func().returnType, exprType) ||
                   (dynamic_cast<NullLiteral*>(blockExpr->finalexpr.value().get()) &&
                    currentFunction.value()->type().isNullable);
        }
        return false;
    }

    if (auto stmt = dynamic_cast<ExpressionStatement*>(node)) {
        return hasReturnPath(stmt->expression.get());
    }

    return false;
}

bool Semantics::hasReturnPathInBlock(const std::vector<std::unique_ptr<Statement>>& statements) {
    for (size_t i = 0; i < statements.size(); ++i) {
        const auto& stmt = statements[i];

        // Check if this statement itself is a return
        if (auto retStmt = dynamic_cast<ReturnStatement*>(stmt.get())) {
            if (retStmt->return_value ||
                (currentFunction.value()->type().isNullable && !retStmt->return_value)) {
                return true;  // Found a return
            }
        }

        // Check if there's a return after this statement
        bool hasReturnAfter = false;
        for (size_t j = i + 1; j < statements.size(); ++j) {
            if (dynamic_cast<ReturnStatement*>(statements[j].get())) {
                hasReturnAfter = true;
                break;
            }
        }

        // Handle if statements
        if (auto ifStmt = dynamic_cast<ifStatement*>(stmt.get())) {
            // If there's a return after this if, we don't need returns inside it
            if (hasReturnAfter) {
                continue;
            }

            // Otherwise, check if all branches return
            if (!ifReturnsInAllPaths(ifStmt)) {
                return false;
            }
            // If all branches return, we're good for this path
            continue;
        }

        // Handle switch statements
        if (auto switchStmt = dynamic_cast<SwitchStatement*>(stmt.get())) {
            // If there's a return after this switch, we don't need returns inside
            // it
            if (hasReturnAfter) {
                continue;
            }

            // Otherwise, check if all paths through switch return
            if (!switchReturnsInAllPaths(switchStmt)) {
                return false;
            }
            // If all paths return, we're good
            continue;
        }
    }

    return false;  // No return found in this block
}

// Helper to check if ALL paths through an if statement return
bool Semantics::ifReturnsInAllPaths(ifStatement* ifStmt) {
    // Check then block
    auto thenBlock = dynamic_cast<BlockStatement*>(ifStmt->if_result.get());
    bool hasThenReturn = thenBlock && hasReturnPath(thenBlock);

    // Check elif blocks
    bool hasElifReturn = true;
    for (const auto& elif : ifStmt->elifClauses) {
        auto elifStmt = dynamic_cast<elifStatement*>(elif.get());
        if (elifStmt) {
            auto elifBlock = dynamic_cast<BlockStatement*>(elifStmt->elif_result.get());
            hasElifReturn &= elifBlock && hasReturnPath(elifBlock);
        }
    }

    // Check else block (if exists)
    bool hasElseReturn = true;
    if (ifStmt->else_result.has_value()) {
        auto elseBlock = dynamic_cast<BlockStatement*>(ifStmt->else_result.value().get());
        hasElseReturn = elseBlock && hasReturnPath(elseBlock);
    } else {
        // No else means there's a path that doesn't return
        hasElseReturn = false;
    }

    return hasThenReturn && hasElifReturn && hasElseReturn;
}

// Helper to check if ALL paths through a switch statement return
bool Semantics::switchReturnsInAllPaths(SwitchStatement* sw) {
    // Default must exist and return in all its paths
    if (sw->default_statements.empty()) return false;
    if (!hasReturnPathInBlock(sw->default_statements)) return false;

    // Check every case clause
    for (size_t i = 0; i < sw->case_clauses.size(); ++i) {
        auto caseClause = dynamic_cast<CaseClause*>(sw->case_clauses[i].get());

        if (caseClause->body.empty()) {
            // Empty body means fall-through - find the next non-empty body
            bool foundReturn = false;
            for (size_t j = i + 1; j < sw->case_clauses.size(); ++j) {
                auto nextClause = dynamic_cast<CaseClause*>(sw->case_clauses[j].get());
                if (!nextClause->body.empty()) {
                    if (hasReturnPathInBlock(nextClause->body)) {
                        foundReturn = true;
                    }
                    break;
                }
            }
            // If no non-empty body found, fall through to default
            if (!foundReturn && !hasReturnPathInBlock(sw->default_statements)) {
                return false;
            }
        } else {
            // Non-empty body must return in all its paths
            if (!hasReturnPathInBlock(caseClause->body)) {
                return false;
            }
        }
    }

    return true;
}

bool Semantics::hasReturnPathList(const std::vector<std::unique_ptr<Statement>>& stmts) {
    for (const auto& stmt : stmts) {
        // If the statement is a return, we're good
        if (dynamic_cast<ReturnStatement*>(stmt.get())) return true;

        // If it's a nested if/switch, check if they are exhaustive
        if (hasReturnPath(stmt.get())) return true;
    }
    return false;
}

bool Semantics::checkParamListCompatibility(
    const std::vector<std::pair<ResolvedType, std::string>>& expectedParams,
    const std::vector<std::unique_ptr<Statement>>& actualParams) {
    if (expectedParams.size() != actualParams.size()) return false;

    for (size_t i = 0; i < expectedParams.size(); ++i) {
        auto param = dynamic_cast<VariableDeclaration*>(actualParams[i].get());
        if (!param) return false;

        auto baseType = inferDeclarationBaseType(param);
        ResolvedType paramType = resolveTypeWithModifier(param->modified_type.get(), baseType);

        const ResolvedType& expectedType = expectedParams[i].first;
        const std::string& expectedGeneric = expectedParams[i].second;

        if (!expectedGeneric.empty()) {
            auto base = dynamic_cast<BasicType*>(param->base_type.get());
            if (!base) return false;
            std::string actualGeneric =
                base->data_token.type == TokenType::IDENTIFIER ? base->data_token.TokenLiteral : "";
            if (actualGeneric != expectedGeneric) return false;
        }

        if (!isTypeCompatible(expectedType, paramType)) return false;
    }
    return true;
}

bool Semantics::areSignaturesCompatible(const SymbolInfo& declInfo, FunctionExpression* funcExpr) {
    if (!checkParamListCompatibility(declInfo.func().paramTypes, funcExpr->call)) return false;
    ResolvedType actualReturn = inferNodeDataType(funcExpr->return_type.get());
    return isTypeCompatible(declInfo.func().returnType, actualReturn);
}

bool Semantics::signaturesMatchBehaviorDeclaration(const std::shared_ptr<MemberInfo>& declMember,
                                                   FunctionExpression* funcExpr) {
    if (!declMember || !funcExpr) return false;
    if (!checkParamListCompatibility(declMember->paramTypes, funcExpr->call)) return false;
    ResolvedType actualReturn = inferNodeDataType(funcExpr->return_type.get());
    return isTypeCompatible(declMember->returnType, actualReturn);
}

bool Semantics::isMethodCallCompatible(const MemberInfo& memFuncInfo, CallExpression* callExpr) {
    bool allGood = true;

    auto funcName = callExpr->function_identifier->expression.TokenLiteral;

    if (memFuncInfo.paramTypes.size() != callExpr->parameters.size()) {
        logSemanticErrors(
            "Function '" + funcName + "' call has " + std::to_string(callExpr->parameters.size()) +
                " arguments, but expects " + std::to_string(memFuncInfo.paramTypes.size()),
            callExpr);
        return false;
    }

    for (size_t i = 0; i < callExpr->parameters.size(); ++i) {
        auto& param = callExpr->parameters[i];
        const auto& expectedType = memFuncInfo.paramTypes[i].first;
        auto argInfo = metaData[param.get()];
        if (!argInfo) continue;

        ResolvedType argType = argInfo->type().type;

        bool isCompatible = isTypeCompatible(expectedType, argType);

        if (argType.kind == DataType::UNKNOWN) {
            logSemanticErrors("Could not infer type for argument " + std::to_string(i + 1),
                              param.get());
            allGood = false;
            continue;
        }

        if (dynamic_cast<NullLiteral*>(param.get())) {
            if (expectedType.isNull) {
                argType = expectedType;  // promote null, nullable type
            } else {
                logSemanticErrors("Cannot pass null to non-nullable parameter " +
                                      std::to_string(i + 1) + ": expected " +
                                      expectedType.resolvedName,
                                  param.get());
                allGood = false;
                continue;
            }
        }

        if (!isCompatible) {
            logSemanticErrors("Call for '" + funcName + "'has a type mismatch in argument " +
                                  std::to_string(i + 1) + ", expected '" +
                                  expectedType.resolvedName + "' but got '" + argType.resolvedName +
                                  "'",
                              param.get());
            allGood = false;
            continue;
        }
    }

    return allGood;
}

bool Semantics::isCallCompatible(const SymbolInfo& funcInfo, CallExpression* callExpr) {
    bool allGood = true;

    auto funcName = callExpr->function_identifier->expression.TokenLiteral;

    // Check parameter count
    if (funcInfo.func().paramTypes.size() != callExpr->parameters.size()) {
        logSemanticErrors(
            "Function '" + funcName + "' call has " + std::to_string(callExpr->parameters.size()) +
                " arguments, but expected " + std::to_string(funcInfo.func().paramTypes.size()),
            callExpr);
        return false;
    }

    for (size_t i = 0; i < callExpr->parameters.size(); ++i) {
        auto& param = callExpr->parameters[i];
        const auto& expectedType = funcInfo.func().paramTypes[i].first;
        auto argInfo = metaData[param.get()];
        logInternal("Arg " + std::to_string(i) + " metaData lookup: " +
                    (argInfo ? "found, type=" + argInfo->type().type.resolvedName : "NOT FOUND"));
        if (!argInfo) continue;

        ResolvedType argType = argInfo->type().type;

        bool isCompatible = isTypeCompatible(expectedType, argType);

        if (argType.kind == DataType::UNKNOWN) {
            logSemanticErrors("Could not infer type for argument " + std::to_string(i + 1),
                              param.get());
            allGood = false;
            continue;
        }

        if (dynamic_cast<NullLiteral*>(param.get())) {
            if (expectedType.isNull) {
                argType = expectedType;
            } else {
                logSemanticErrors("Cannot pass null to non-nullable parameter " +
                                      std::to_string(i + 1) + ": expected " +
                                      expectedType.resolvedName,
                                  param.get());
                allGood = false;
                continue;
            }
        }

        if (!isCompatible) {
            logSemanticErrors("Call for '" + funcName + "'has a type mismatch in argument " +
                                  std::to_string(i + 1) + ", expected '" +
                                  expectedType.resolvedName + "' but got '" + argType.resolvedName +
                                  "'",
                              param.get());
            allGood = false;
            continue;
        }
    }
    return allGood;
}

int Semantics::inferLiteralDimensions(ArrayLiteral* lit) {
    if (lit->array.empty()) return 1;  // [] is still 1D, undefined element type

    // If the first element is not an array literal -> dimension = 1
    auto first = lit->array[0].get();
    auto innerArr = dynamic_cast<ArrayLiteral*>(first);
    if (!innerArr) return 1;

    // Otherwise: 1 (this level) + dimension of inner
    return 1 + inferLiteralDimensions(innerArr);
}

void Semantics::inferSizePerDimension(ArrayLiteral* lit, std::vector<int64_t>& sizes) {
    sizes.push_back(lit->array.size());
    if (!lit->array.empty()) {
        if (auto inner = dynamic_cast<ArrayLiteral*>(lit->array[0].get())) {
            inferSizePerDimension(inner, sizes);
        }
    }
}

std::vector<uint64_t> Semantics::getSizePerDimesion(Node* node) {
    std::vector<uint64_t> dims;
    Node* current = node;

    // We keep drilling down as long as we find nested ArrayLiterals
    while (auto arrLit = dynamic_cast<ArrayLiteral*>(current)) {
        // Record the size of the current "container"
        dims.push_back(static_cast<uint64_t>(arrLit->array.size()));
        if (arrLit->array.empty()) {
            break;  // Can't determine dimensions of an empty array
        }
        // Move to the first element to see if it's another level deep
        current = arrLit->array[0].get();
    }

    return dims;
}

uint64_t Semantics::getIntegerConstant(Node* node) {
    if (!node) return 0;

    auto tryParse = [&](auto* lit) -> uint64_t {
        if (lit) {
            try {
                // stoull handles the string-to-number conversion
                return std::stoull(lit->expression.TokenLiteral);
            } catch (...) {
                return 0;  // Handle garbage strings just in case
            }
        }
        return 0;
    };

    if (auto l = dynamic_cast<I8Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<U8Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<I16Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<U16Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<I32Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<U32Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<I64Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<U64Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<I128Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<U128Literal*>(node)) return tryParse(l);
    if (auto l = dynamic_cast<INTLiteral*>(node)) return tryParse(l);

    return 0;
}

bool Semantics::isInteger(const ResolvedType& t) {
    static const std::unordered_set<DataType> intTypes = {
        DataType::I8,   DataType::U8,   DataType::I16,   DataType::U16,
        DataType::I32,  DataType::U32,  DataType::I64,   DataType::U64,
        DataType::I128, DataType::U128, DataType::ISIZE, DataType::USIZE};
    // The modifiers must be off
    bool isInt = t.isBase() && (intTypes.count(t.kind) > 0);
    return isInt;
}

bool Semantics::isFloat(const ResolvedType& t) {
    return (t.kind == DataType::F32 || t.kind == DataType::F64) && t.isBase();
}

bool Semantics::isBoolean(const ResolvedType& t) {
    return t.kind == DataType::BOOLEAN && t.isBase();
}

bool Semantics::isString(const ResolvedType& t) { return t.kind == DataType::STRING && t.isBase(); }

bool Semantics::isChar(const ResolvedType& t) {
    static const std::unordered_set<DataType> charTypes = {DataType::CHAR8, DataType::CHAR16,
                                                           DataType::CHAR32};
    return t.isBase() && charTypes.count(t.kind) > 0;
}

bool Semantics::isLiteral(Node* node) {
    // Integer literals
    auto i8Lit = dynamic_cast<I8Literal*>(node);
    auto u8Lit = dynamic_cast<U8Literal*>(node);
    auto i16Lit = dynamic_cast<I16Literal*>(node);
    auto u16Lit = dynamic_cast<U16Literal*>(node);
    auto i32Lit = dynamic_cast<I32Literal*>(node);
    auto u32Lit = dynamic_cast<U32Literal*>(node);
    auto i64Lit = dynamic_cast<I64Literal*>(node);
    auto u64Lit = dynamic_cast<U64Literal*>(node);
    auto i128Lit = dynamic_cast<I128Literal*>(node);
    auto u128Lit = dynamic_cast<U128Literal*>(node);
    auto intLit = dynamic_cast<INTLiteral*>(node);
    bool isIntLit = (i8Lit || u8Lit || i16Lit || u16Lit || i32Lit || u32Lit || i64Lit || u64Lit ||
                     i128Lit || u128Lit || intLit);

    // Float and double literals;
    auto f32Lit = dynamic_cast<F32Literal*>(node);
    auto f64Lit = dynamic_cast<F64Literal*>(node);
    auto fltLit = dynamic_cast<FloatLiteral*>(node);
    bool isDecLit = (f32Lit || f64Lit || fltLit);

    // Char literal
    auto char8Lit = dynamic_cast<Char8Literal*>(node);
    auto char16Lit = dynamic_cast<Char16Literal*>(node);
    auto char32Lit = dynamic_cast<Char32Literal*>(node);
    bool isCharLit = (char8Lit || char16Lit || char32Lit);

    // Boolean literal
    auto boolLit = dynamic_cast<BooleanLiteral*>(node);

    // String literal
    auto strLit = dynamic_cast<StringLiteral*>(node);
    auto infix = dynamic_cast<InfixExpression*>(node);

    if (isIntLit || isDecLit || isCharLit || boolLit || strLit || infix)
        return true;
    else
        return false;
}

bool Semantics::isConstLiteral(Node* node) {
    // Integer literals
    auto i8Lit = dynamic_cast<I8Literal*>(node);
    auto u8Lit = dynamic_cast<U8Literal*>(node);
    auto i16Lit = dynamic_cast<I16Literal*>(node);
    auto u16Lit = dynamic_cast<U16Literal*>(node);
    auto i32Lit = dynamic_cast<I32Literal*>(node);
    auto u32Lit = dynamic_cast<U32Literal*>(node);
    auto i64Lit = dynamic_cast<I64Literal*>(node);
    auto u64Lit = dynamic_cast<U64Literal*>(node);
    auto i128Lit = dynamic_cast<I128Literal*>(node);
    auto u128Lit = dynamic_cast<U128Literal*>(node);
    auto isizeLit = dynamic_cast<ISIZELiteral*>(node);
    auto usizeLit = dynamic_cast<USIZELiteral*>(node);
    auto INTlit = dynamic_cast<INTLiteral*>(node);
    bool isIntLit = (i8Lit || u8Lit || i16Lit || u16Lit || i32Lit || u32Lit || i64Lit || u64Lit ||
                     i128Lit || u128Lit || isizeLit || usizeLit || INTlit);

    // Float and double literals;
    auto f32Lit = dynamic_cast<F32Literal*>(node);
    auto f64Lit = dynamic_cast<F64Literal*>(node);
    auto FLTLit = dynamic_cast<FloatLiteral*>(node);
    bool isDecLit = (f32Lit || f64Lit || FLTLit);

    // Char literal
    auto char8Lit = dynamic_cast<Char8Literal*>(node);
    auto char16Lit = dynamic_cast<Char16Literal*>(node);
    auto char32Lit = dynamic_cast<Char32Literal*>(node);
    bool isCharLit = (char8Lit || char16Lit || char32Lit);

    // Boolean literal
    auto boolLit = dynamic_cast<BooleanLiteral*>(node);

    // String literal
    auto strLit = dynamic_cast<StringLiteral*>(node);

    if (isIntLit || isDecLit || isCharLit || boolLit || strLit)
        return true;
    else
        return false;
}

bool Semantics::isIntegerConstant(Node* node) {
    auto i8Lit = dynamic_cast<I8Literal*>(node);
    auto u8Lit = dynamic_cast<U8Literal*>(node);
    auto i16Lit = dynamic_cast<I16Literal*>(node);
    auto u16Lit = dynamic_cast<U16Literal*>(node);
    auto i32Lit = dynamic_cast<I32Literal*>(node);
    auto u32Lit = dynamic_cast<U32Literal*>(node);
    auto i64Lit = dynamic_cast<I64Literal*>(node);
    auto u64Lit = dynamic_cast<U64Literal*>(node);
    auto i128Lit = dynamic_cast<I128Literal*>(node);
    auto u128Lit = dynamic_cast<U128Literal*>(node);
    auto isizeLit = dynamic_cast<ISIZELiteral*>(node);
    auto usizeLit = dynamic_cast<USIZELiteral*>(node);
    auto intLit = dynamic_cast<INTLiteral*>(node);
    bool isIntLit = (i8Lit || u8Lit || i16Lit || u16Lit || i32Lit || u32Lit || i64Lit || u64Lit ||
                     i128Lit || u128Lit || isizeLit || usizeLit || intLit);

    return isIntLit;
}

bool Semantics::isFloatConstant(Node *node){
    auto f32Lit=dynamic_cast<F32Literal*>(node);
    auto f64Lit=dynamic_cast<F64Literal*>(node);
    auto fltLit=dynamic_cast<FloatLiteral*>(node);

    bool isFlt=(f32Lit||f64Lit||fltLit);
    return isFlt;
}

ResolvedType Semantics::inferDeclarationBaseType(VariableDeclaration* declaration) {
    logInternal("Resolving Variable declaration Base Type ....");

    auto baseType = dynamic_cast<BasicType*>(declaration->base_type.get());
    TokenType basetype_tokentype = baseType->data_token.type;
    auto makeType = [&](const ResolvedType& type) {
        return ResolvedType::makeBase(type.kind, type.resolvedName, baseType->isNullable);
    };

    switch (basetype_tokentype) {
        case TokenType::I8_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::I8, "i8"));
        case TokenType::U8_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::U8, "u8"));

        case TokenType::I16_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::I16, "i16"));
        case TokenType::U16_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::U16, "u16"));

        case TokenType::I32_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::I32, "i32"));
        case TokenType::U32_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::U32, "u32"));

        case TokenType::I64_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::I64, "i64"));
        case TokenType::U64_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::U64, "u64"));

        case TokenType::I128_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::I128, "i128"));
        case TokenType::U128_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::U128, "u128"));

        case TokenType::ISIZE_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::ISIZE, "isize"));
        case TokenType::USIZE_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::USIZE, "usize"));

        case TokenType::F32_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::F32, "f32"));
        case TokenType::F64_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::F64, "f64"));

        case TokenType::STRING_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::STRING, "string"));

        case TokenType::CHAR8_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::CHAR8, "char8"));
        case TokenType::CHAR16_KEYWORD:
            return ResolvedType::makeBase(DataType::CHAR16, "char16");
        case TokenType::CHAR32_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::CHAR32, "char32"));
        case TokenType::OPAQUE:
            return makeType(ResolvedType::makeBase(DataType::OPAQUE, "opaque"));

        case TokenType::BOOL_KEYWORD:
            return makeType(ResolvedType::makeBase(DataType::BOOLEAN, "bool"));

        case TokenType::AUTO: {
            auto value = declaration->initializer.get();
            if (!value) {
                logSemanticErrors("Cannot infer without a value", declaration);
                return ResolvedType::unknown();
            }
            auto inferred = inferNodeDataType(value);
            return inferred;
        }

        // Dealing with custom types now
        case TokenType::IDENTIFIER: {
            auto typeName = baseType->data_token.TokenLiteral;
            // Search for the name in the custom types table
            auto typeIt = customTypesTable.find(typeName);
            if (typeIt == customTypesTable.end()) {
                logSemanticErrors("Type '" + typeName + "' is unknown", baseType);
                return ResolvedType::error();
            }

            return makeType(
                ResolvedType::makeBase(typeIt->second->type.kind, typeIt->second->typeName));
        }

        default:
            return ResolvedType::unknown();
    }
}

std::string Semantics::getBaseTypeName(const ResolvedType& type) {
    return type.base().resolvedName;
}

ResolvedType Semantics::getArrayElementType(const ResolvedType& type) {
    if (type.isArray() || type.isPointer()) {
        if (!type.innerType) {
            logInternal("ERROR: Found nested type with no innerType record.");
            return ResolvedType::unknown();
        }

        logInternal("Digging deeper: " + type.resolvedName + " -> " + type.innerType->resolvedName);

        return getArrayElementType(*type.innerType);
    }

    logInternal("Base type found: " + type.resolvedName);
    return type;
}

void Semantics::collectDimensions(TypeModifier* modifier, std::vector<uint64_t>& staticDims,
                                  std::vector<Node*>& dynamicDims) {
    if (!modifier) return;

    // Get dimension for current level (e.g., the '3' in arr[3])
    if (!modifier->dimensions.empty()) {
        for (const auto& len : modifier->dimensions) {
            walker(len.get());
            if (isIntegerConstant(len.get())) {
                logInternal("[COLLECTING STATIC DIMS]");
                staticDims.push_back(getIntegerConstant(len.get()));
            } else {
                logInternal("[COLLECTING DYNAMIC DIMS]");
                dynamicDims.push_back(len.get());
            }
        }

        // Recurse into the inner type
        auto innerModifier = dynamic_cast<TypeModifier*>(modifier->inner_modifier.get());
        collectDimensions(innerModifier, staticDims, dynamicDims);
    }
}

ResolvedType Semantics::makePointerType(const ResolvedType& inner, bool isNull) {
    ResolvedType outer;
    outer.modifier = Modifier::POINTER;
    outer.innerType = std::make_shared<ResolvedType>(inner);
    outer.isNull = isNull;
    outer.resolvedName = "ptr<" + inner.resolvedName + ">";
    return outer;
}

ResolvedType Semantics::makeRefType(const ResolvedType& inner, bool isNull) {
    ResolvedType outer;
    outer.modifier = Modifier::REFERENCE;
    outer.innerType = std::make_shared<ResolvedType>(inner);
    outer.isNull = isNull;
    outer.resolvedName = "ref<" + inner.resolvedName + ">";
    return outer;
}

ResolvedType Semantics::makeArrayType(const ResolvedType& inner, uint64_t size, bool isNull) {
    ResolvedType outer;
    outer.modifier = Modifier::ARRAY;
    outer.innerType = std::make_shared<ResolvedType>(inner);
    outer.arraySize = size;
    outer.isNull = isNull;
    outer.resolvedName =
        "arr[" + (size ? std::to_string(size) : "?") + "]<" + inner.resolvedName + ">";
    return outer;
}

ResolvedType Semantics::peelRef(const ResolvedType& t) {
    if (!t.isRef()) return t;
    if (!t.innerType) return ResolvedType::unknown();
    return *t.innerType;
}

bool Semantics::isIdentInSelf(SelfExpression* self, Identifier* target) {
    for (const auto& ident : self->fields) {
        if (ident.get() == target) {
            return true;
        }
    }

    return false;
}

std::string Semantics::generateLifetimeID(const std::shared_ptr<SymbolInfo>& sym) {
    if (sym->type().isPointer)
        return "P" + std::to_string(ptrDeclCount++);
    else if (sym->type().isArray)
        return "A" + std::to_string(arrDeclCount++);
    else
        return "N" + std::to_string(normalDeclCount++);

    return "NO ID";
}

std::unique_ptr<LifeTime> Semantics::createLifeTimeTracker(
    Node* declarationNode, Node* initializer, const std::shared_ptr<SymbolInfo>& declSym) {
    logInternal("Creating lifetime baton...");
    auto lifetime = std::make_unique<LifeTime>();
    lifetime->ID = generateLifetimeID(declSym);
    lifetime->isResponsible = true;
    lifetime->isAlive = true;
    lifetime->persist = declSym->storage().isPersist;

    auto idents = digIdentifiers(initializer);
    for (const auto& identifier : idents) {
        auto identSym = getSymbolFromMeta(identifier);
        if (!identSym) reportDevBug("Failed to get identifier symbol info", identifier);

        if (!identSym->storage().isHeap) {
            logInternal("Encountered non heap ident skipping...");
            continue;
        }

        auto targetBaton = getBaton(identSym->codegen().ID);
        if (!targetBaton)
            reportDevBug(
                "Failed to get lifetime baton for ident of lifetime ID: " + identSym->codegen().ID,
                identifier);

        transferResponsibility(lifetime.get(), targetBaton, identSym);
    }

    /*if (declSym->relations().targetSymbol && targetBaton) {
        transferResponsibility(lifetime.get(), targetBaton, declSym->relations().targetSymbol);
        }*/

    Node* currentBlock = getCurrentBlock();
    if (currentBlock) {
        bornInBlock[currentBlock].push_back(lifetime->ID);
        logInternal("[BORN] Baton " + lifetime->ID + " born in block: " + currentBlock->toString());
    } else {
        errorHandler.addHint(
            "It seems you have declared a heap variable in an invalid scope the "
            "compiler cannot carry out bunkering because it has failed to figure "
            "out the exact block this declaration was made under");
        logSemanticErrors("Failed to register declaration inside block", declarationNode);
    }

    logInternal("Created baton: " + lifetime->ID + " for Node: " + declarationNode->toString());
    return lifetime;
}

LifeTime* Semantics::getBaton(const std::string& ID) {
    // Primary Attempt: Use the ID to find the ORIGINAL holder node
    Node* holder = queryForLifeTimeBaton(ID);

    if (holder) {
        auto it = responsibilityTable.find(holder);
        if (it != responsibilityTable.end()) {
            return it->second.get();
        }
    }

    // Fallback, Deep Search by ID
    for (auto const& [node, baton] : responsibilityTable) {
        if (baton && baton->ID == ID) {
            logInternal("[BATON GETTER] Found via Deep Search for ID: " + ID);
            return baton.get();
        }
    }

    logInternal("[BATON GETTER] Total Failure: ID " + ID + " exists nowhere.");
    return nullptr;
}

void Semantics::transferResponsibility(LifeTime* currentBaton, LifeTime* targetBaton,
                                       const std::shared_ptr<SymbolInfo>& targetSym) {
    if (!targetBaton) {
        logInternal("[HEIST] Failed to carry out heist due to missing target baton");
        return;
    }

    if (!currentBaton) {
        logInternal(
            "[HEIST] Failed to carry out normal heist a non heap variable might have been "
            "encountered");
        if (targetSym->storage().isHeap) {
            logInternal("[HEIST] Carrying out heist registration");
            heistIDs.insert(targetSym->codegen().ID);
        }
        return;
    }

    logInternal("[HEIST] Robber (Current): " + currentBaton->ID);
    logInternal("[HEIST] Victim (Target): " + targetBaton->ID);
    logInternal("[HEIST] Target Pointer Count: " +
                std::to_string(targetSym->storage().pointerCount));
    
    if (currentBaton->ID == targetBaton->ID) {
        logInternal("[HEIST] SKIPPED: Cannot create self-dependency");
        return;
    }

    if (!targetSym->storage().isHeap) {
        logInternal("[HEIST] The victim is not heap raised dont bother");
        return;
    }

    // Assign the baton pointer count
    targetBaton->ptrCount = targetSym->storage().pointerCount;

    auto targetPointerCount = targetBaton->ptrCount;

    // The moment of truth: Can we rob it?
    if (targetPointerCount <= 2) {
        logInternal("[HEIST] SUCCESS: Robbery approved. Disarming " + targetBaton->ID);

        targetBaton->isResponsible = false;       // The Victim is now impotent
        targetBaton->ownedBy = currentBaton->ID;  // The Master-Slave link

        // The Robber takes the specific target
        currentBaton->dependents[targetBaton->ID] = targetSym;

        // THE LOOT: Robber takes all the victim's existing dependents too
        for (auto const& [id, sym] : targetBaton->dependents) {
            logInternal("[HEIST] COLLECTING LOOT: Robber " + currentBaton->ID + " now owns " + id);
            currentBaton->dependents[id] = sym;
        }
    } else {
        logInternal("[HEIST] FAILURE: Robbery vetoed. Pointer count (" +
                    std::to_string(targetPointerCount) + ") is too high.");
    }

    logInternal("[HEIST] Victim " + targetBaton->ID +
                " isResponsible = " + (targetBaton->isResponsible ? "TRUE" : "FALSE") + " ---");
}

const std::unique_ptr<LifeTime>& Semantics::readBatonInfo(const std::string& batonID) {
    for (const auto& [node, baton] : responsibilityTable) {
        if (baton && baton->ID == batonID) {
            return baton;
        }
    }

    static const std::unique_ptr<LifeTime> nullBaton = nullptr;
    return nullBaton;
}

Node* Semantics::queryForLifeTimeBaton(const std::string& familyID) {
    for (const auto& [node, baton] : responsibilityTable) {
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

Node* Semantics::getCurrentBlock() {
    if (activeBlocks.empty()) return nullptr;

    return activeBlocks.back();
}

bool Semantics::isBornInScope(Node* root, const std::string& ID) {
    auto it = bornInBlock.find(root);
    if (it == bornInBlock.end()) {
        return false;
    }

    for (const std::string& id : it->second) {
        if (id == ID) {
            return true;
        }
    }
    return false;
}

void Semantics::transferBaton(Node* receiver, const std::string& familyID) {
    Node* holder = queryForLifeTimeBaton(familyID);
    if (!holder) {
        reportDevBug("Could not find the baton holder for lifetime ID: " + familyID, receiver);
    }
    std::string identName = "<no name>";
    auto identifiers = digIdentifiers(receiver);
    for (const auto& identifier : identifiers) {
        identName = extractIdentifierName(identifier);
        auto identSym = getSymbolFromMeta(identifier);
        if (!identSym) {
            reportDevBug(
                "Failed to get symbol info for identifer node during baton "
                "transfer '" +
                    identName + "'",
                identifier);
        }

        if (!identSym->storage().isHeap) {
            logInternal("[BATON TRANSFER] The identifier '" + identName +
                        "' is not heap allocated skipping..");
        }
        // Ensure we give the baton to the correct identifier
        if (familyID == identSym->codegen().ID) {
            logInternal("[BATON TRANSFER] Transfering baton of ID: " + familyID + " from node: " +
                        holder->toString() + " to composite node: " + receiver->toString() +
                        " ident: " + identifier->toString());
            responsibilityTable[identifier] = std::move(responsibilityTable[holder]);
        }
    }
}

bool Semantics::isTerminator(Node* stmt) {
    if (dynamic_cast<ReturnStatement*>(stmt)) return true;
    if (dynamic_cast<BreakStatement*>(stmt)) return true;
    if (dynamic_cast<ContinueStatement*>(stmt)) return true;

    return false;
}

std::string Semantics::getTerminatorString(Node* node) {
    std::string terminatorStr;
    if (dynamic_cast<ReturnStatement*>(node)) terminatorStr = "return";
    if (dynamic_cast<BreakStatement*>(node)) terminatorStr = "break";
    if (dynamic_cast<ContinueStatement*>(node)) terminatorStr = "continue";

    return terminatorStr;
}

void Semantics::popScope() {
    auto& scope = symbolTable.back();
    for (auto& [name, sym] : scope) {
        // If the symbol we come across is a reference and it is referencing
        // validly
        if (sym->type().isRef && sym->relations().refereeSymbol) {
            logInternal("Initial refCount :" +
                        std::to_string(sym->relations().refereeSymbol->storage().refCount));
            if (sym->relations().refereeSymbol->storage().refCount > 0) {
                sym->relations().refereeSymbol->storage().refCount -= 1;
                logInternal("Ref '" + name + "' relseased its target the refCount now " +
                            std::to_string(sym->relations().refereeSymbol->storage().refCount));
            }
        } else if (sym->type().isPointer && sym->relations().targetSymbol &&
                   !sym->storage().isHeap) {
            logInternal("Initial pointer count: " +
                        std::to_string(sym->relations().targetSymbol->storage().pointerCount));
            if (sym->relations().targetSymbol->storage().pointerCount > 0) {
                sym->relations().targetSymbol->storage().pointerCount -= 1;
                logInternal("Stack Pointer '" + name +
                            "' realeased its target the pointer count is now " +
                            std::to_string(sym->relations().targetSymbol->storage().pointerCount));
            }
        }
    }
    symbolTable.pop_back();
}

std::shared_ptr<SymbolInfo> Semantics::getSymbolFromMeta(Node* node) {
    auto it = metaData.find(node);
    if (it == metaData.end()) {
        return nullptr;
    }

    return it->second;
}

std::vector<Identifier*> Semantics::digIdentifiers(Node* node) {
    std::vector<Identifier*> results;

    if (!node) return results;

    // Base case: node is already an identifier
    if (auto* ident = dynamic_cast<Identifier*>(node)) {
        results.push_back(ident);
        return results;
    }

    // AddressExpression (addr b)
    if (auto* addr = dynamic_cast<AddressExpression*>(node)) {
        return digIdentifiers(addr->identifier.get());
    }

    // DereferenceExpression (deref p)
    if (auto* deref = dynamic_cast<DereferenceExpression*>(node)) {
        return digIdentifiers(deref->identifier.get());
    }

    // SelfExpression (self.field)
    if (auto* self = dynamic_cast<SelfExpression*>(node)) {
        for (auto& field : self->fields) {
            auto fieldIds = digIdentifiers(field.get());
            results.insert(results.end(), fieldIds.begin(), fieldIds.end());
        }
        return results;
    }

    // Subscript expression
    if (auto* arrSub = dynamic_cast<ArraySubscript*>(node)) {
        if (auto* ident = dynamic_cast<Identifier*>(arrSub->identifier.get()))
            results.push_back(ident);

        for (auto& lens : arrSub->index_exprs) {
            auto lenIds = digIdentifiers(lens.get());
            results.insert(results.end(), lenIds.begin(), lenIds.end());
        }
        return results;
    }

    // NewComponentExpression (new Player(args))
    if (auto* newComp = dynamic_cast<NewComponentExpression*>(node)) {
        for (auto& arg : newComp->arguments) {
            auto argIds = digIdentifiers(arg.get());
            results.insert(results.end(), argIds.begin(), argIds.end());
        }
        return results;
    }

    // CallExpression (func(x, y))
    if (auto* call = dynamic_cast<CallExpression*>(node)) {
        for (auto& arg : call->parameters) {
            auto argIds = digIdentifiers(arg.get());
            results.insert(results.end(), argIds.begin(), argIds.end());
        }
        return results;
    }

    // UnwrapExpression (unwrap x)
    if (auto* unwrap = dynamic_cast<UnwrapExpression*>(node)) {
        return digIdentifiers(unwrap->expr.get());
    }

    // MethodCallExpression (instance.method())
    if (auto* method = dynamic_cast<MethodCallExpression*>(node)) {
        auto instanceIds = digIdentifiers(method->instance.get());
        auto callIds = digIdentifiers(method->call.get());
        results.insert(results.end(), instanceIds.begin(), instanceIds.end());
        results.insert(results.end(), callIds.begin(), callIds.end());
        return results;
    }

    // InstanceExpression (Type { fields })
    if (auto* instance = dynamic_cast<InstanceExpression*>(node)) {
        if (instance->blockIdent) {
            auto blockIds = digIdentifiers(instance->blockIdent.get());
            results.insert(results.end(), blockIds.begin(), blockIds.end());
        }
        for (auto& field : instance->fields) {
            auto fieldIds = digIdentifiers(field.get());
            results.insert(results.end(), fieldIds.begin(), fieldIds.end());
        }
        return results;
    }

    // CastExpression (cast<i32>(x))
    if (auto* cast = dynamic_cast<CastExpression*>(node)) {
        if (cast->expr) {
            auto exprIds = digIdentifiers(cast->expr.get());
            results.insert(results.end(), exprIds.begin(), exprIds.end());
        }
        return results;
    }

    // BitcastExpression (bitcast<ptr>(x))
    if (auto* bitcast = dynamic_cast<BitcastExpression*>(node)) {
        if (bitcast->expr) {
            auto exprIds = digIdentifiers(bitcast->expr.get());
            results.insert(results.end(), exprIds.begin(), exprIds.end());
        }
        return results;
    }

    // PrefixExpression (!x, -y)
    if (auto* prefix = dynamic_cast<PrefixExpression*>(node)) {
        return digIdentifiers(prefix->operand.get());
    }

    // PostfixExpression (i++, j--)
    if (auto* postfix = dynamic_cast<PostfixExpression*>(node)) {
        return digIdentifiers(postfix->operand.get());
    }

    // FString literal
    if (auto* fStr = dynamic_cast<FStringLiteral*>(node)) {
        for (const auto& seg : fStr->segments) {
            for (const auto& value : seg.values) {
                auto ids = digIdentifiers(value.get());
                results.insert(results.end(), ids.begin(), ids.end());
            }
        }
        return results;
    }

    // InfixExpression (x + y, a.next, b.prev)
    if (auto* infix = dynamic_cast<InfixExpression*>(node)) {
        auto leftIds = digIdentifiers(infix->left_operand.get());
        auto rightIds = digIdentifiers(infix->right_operand.get());
        results.insert(results.end(), leftIds.begin(), leftIds.end());
        results.insert(results.end(), rightIds.begin(), rightIds.end());
        return results;
    }

    // ArrayLiteral ([1, 2, 3])
    if (auto* arrayLit = dynamic_cast<ArrayLiteral*>(node)) {
        for (auto& item : arrayLit->array) {
            auto itemIds = digIdentifiers(item.get());
            results.insert(results.end(), itemIds.begin(), itemIds.end());
        }
        return results;
    }

    // ArraySubscript (arr[5])
    if (auto* subscript = dynamic_cast<ArraySubscript*>(node)) {
        if (subscript->identifier) {
            auto identIds = digIdentifiers(subscript->identifier.get());
            results.insert(results.end(), identIds.begin(), identIds.end());
        }
        for (auto& index : subscript->index_exprs) {
            auto indexIds = digIdentifiers(index.get());
            results.insert(results.end(), indexIds.begin(), indexIds.end());
        }
        return results;
    }

    // STATEMENTS
    // ExpressionStatement
    if (auto* exprStmt = dynamic_cast<ExpressionStatement*>(node)) {
        return digIdentifiers(exprStmt->expression.get());
    }

    // AssignmentStatement (x = y)
    if (auto* assign = dynamic_cast<AssignmentStatement*>(node)) {
        auto identIds = digIdentifiers(assign->identifier.get());
        auto valueIds = digIdentifiers(assign->value.get());
        results.insert(results.end(), identIds.begin(), identIds.end());
        results.insert(results.end(), valueIds.begin(), valueIds.end());
        return results;
    }

    // FieldAssignment (a.next = addr b)
    if (auto* fieldAssign = dynamic_cast<FieldAssignment*>(node)) {
        auto lhsIds = digIdentifiers(fieldAssign->lhs_chain.get());
        auto valueIds = digIdentifiers(fieldAssign->value.get());
        results.insert(results.end(), lhsIds.begin(), lhsIds.end());
        results.insert(results.end(), valueIds.begin(), valueIds.end());
        return results;
    }

    // ReturnStatement
    if (auto* ret = dynamic_cast<ReturnStatement*>(node)) {
        if (ret->return_value) {
            return digIdentifiers(ret->return_value.get());
        }
        return results;
    }

    // TraceStatement
    if (auto* trace = dynamic_cast<TraceStatement*>(node)) {
        for (const auto& expr : trace->arguments) {
            auto idents = digIdentifiers(expr.get());
            results.insert(results.end(), idents.begin(), idents.end());
        }
        return results;
    }
    return results;
}

void Semantics::logSpecialErrors(const std::string& message, int line, int col) {
    hasFailed = true;
    CompilerError error;
    error.level = ErrorLevel::SEMANTIC;
    error.line = line;
    error.col = col;
    error.message = message;
    error.hints = {};

    errorHandler.report(error);
}

void Semantics::logSemanticErrors(const std::string& message, Node* contextNode) {
    auto tokenLine = 0;
    auto tokenColumn = 0;
    if (contextNode) {
        tokenLine = contextNode->token.line;
        tokenColumn = contextNode->token.column;
    }
    hasFailed = true;
    hasError = true;
    CompilerError error;
    error.level = ErrorLevel::SEMANTIC;
    error.line = tokenLine;
    error.col = tokenColumn;
    error.message = message;
    error.tokenLength = errorHandler.getTokenLength(contextNode);
    error.hints = {};

    errorHandler.report(error);
}

void Semantics::reportDevBug(const std::string& message, Node* contextNode) {
    int line = 0;
    int col = 0;
    if (contextNode) {
        line = contextNode->token.line;
        col = contextNode->token.column;
    }

    CompilerError error;
    error.level = ErrorLevel::INTERNAL;
    error.line = line;
    error.col = col;
    error.message = message;
    error.tokenLength = errorHandler.getTokenLength(contextNode);
    error.hints = {};

    errorHandler.report(error);

    std::abort();
}

void Semantics::logInternal(const std::string& message) {
    if (verbose) {
        std::cout << message << "\n";
    }
}

bool Semantics::failed() { return hasFailed; }
