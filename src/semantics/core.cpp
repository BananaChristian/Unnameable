#include "semantics.hpp"
#include "ast.hpp"
#include <fstream>
#include <algorithm>
#include <unordered_set>

#define CPPREST_FORCE_REBUILD

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"

Semantics::Semantics(Deserializer &deserial, std::string &file) : deserializer(deserial), fileName(file), errorHandler(file)
{
    symbolTable.push_back({});
    registerWalkerFunctions();

    importSeals();                   // Import seals
    importComponents();              // Import components
    importComponentInits();          // Import component inits
    importDataBlocks();              // Import data
    registerInbuiltAllocatorTypes(); // Register the inbuilt allocators(malloc for now)
}

// Main walker function
void Semantics::walker(Node *node)
{
    if (!node)
        return;

    std::cout << "Analyzing AST node: " << node->toString() << "\n";
    std::cout << "Type at runtime: " << typeid(*node).name() << "\n";
    auto walkerIt = walkerFunctionsMap.find(typeid(*node));

    if (walkerIt != walkerFunctionsMap.end())
    {
        (this->*walkerIt->second)(node);
    }
    else
    {
        std::cerr << "[SEMANTIC LOG]: Failed to find analyzer for: " << node->toString() << "\n";
        std::cout << "Actual runtime type: " << typeid(*node).name() << "\n";
    }
}

// HELPER FUNCTIONS
void Semantics::registerWalkerFunctions()
{
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
    walkerFunctionsMap[typeid(FloatLiteral)] = &Semantics::walkFloatLiteral;
    walkerFunctionsMap[typeid(DoubleLiteral)] = &Semantics::walkDoubleLiteral;
    walkerFunctionsMap[typeid(StringLiteral)] = &Semantics::walkStringLiteral;

    walkerFunctionsMap[typeid(Char8Literal)] = &Semantics::walkChar8Literal;
    walkerFunctionsMap[typeid(Char16Literal)] = &Semantics::walkChar16Literal;
    walkerFunctionsMap[typeid(Char32Literal)] = &Semantics::walkChar32Literal;

    walkerFunctionsMap[typeid(BooleanLiteral)] = &Semantics::walkBooleanLiteral;
    walkerFunctionsMap[typeid(Identifier)] = &Semantics::walkIdentifierExpression;
    walkerFunctionsMap[typeid(AddressExpression)] = &Semantics::walkAddressExpression;
    walkerFunctionsMap[typeid(DereferenceExpression)] = &Semantics::walkDereferenceExpression;

    walkerFunctionsMap[typeid(NullLiteral)] = &Semantics::walkNullLiteral;

    // Walker registration for array walker
    walkerFunctionsMap[typeid(ArrayStatement)] = &Semantics::walkArrayStatement;
    walkerFunctionsMap[typeid(ArrayLiteral)] = &Semantics::walkArrayLiteral;
    walkerFunctionsMap[typeid(ArraySubscript)] = &Semantics::walkArraySubscriptExpression;

    // Walker registration for let statement and assignment statements
    walkerFunctionsMap[typeid(LetStatement)] = &Semantics::walkLetStatement;
    walkerFunctionsMap[typeid(AssignmentStatement)] = &Semantics::walkAssignStatement;
    walkerFunctionsMap[typeid(FieldAssignment)] = &Semantics::walkFieldAssignmentStatement;
    walkerFunctionsMap[typeid(EachStatement)] = &Semantics::walkEachStatement;

    // Walker registration for reference statement and pointer statement
    walkerFunctionsMap[typeid(ReferenceStatement)] = &Semantics::walkReferenceStatement;
    walkerFunctionsMap[typeid(PointerStatement)] = &Semantics::walkPointerStatement;

    // Walker registration for control flow
    walkerFunctionsMap[typeid(ifStatement)] = &Semantics::walkIfStatement;
    walkerFunctionsMap[typeid(SwitchStatement)] = &Semantics::walkSwitchStatement;
    walkerFunctionsMap[typeid(CaseClause)] = &Semantics::walkCaseStatement;

    // Loop disruption statements
    walkerFunctionsMap[typeid(BreakStatement)] = &Semantics::walkBreakStatement;
    walkerFunctionsMap[typeid(ContinueStatement)] = &Semantics::walkContinueStatement;

    // Walker registration for functions
    walkerFunctionsMap[typeid(FunctionStatement)] = &Semantics::walkFunctionStatement;
    walkerFunctionsMap[typeid(FunctionExpression)] = &Semantics::walkFunctionExpression;
    walkerFunctionsMap[typeid(FunctionDeclaration)] = &Semantics::walkFunctionDeclarationStatement;
    walkerFunctionsMap[typeid(FunctionDeclarationExpression)] = &Semantics::walkFunctionDeclarationExpression;
    walkerFunctionsMap[typeid(CallExpression)] = &Semantics::walkFunctionCallExpression;
    walkerFunctionsMap[typeid(UnwrapExpression)] = &Semantics::walkUnwrapExpression;
    walkerFunctionsMap[typeid(ReturnStatement)] = &Semantics::walkReturnStatement;

    // Walker registration for type expressions
    walkerFunctionsMap[typeid(BasicType)] = &Semantics::walkBasicType;
    walkerFunctionsMap[typeid(ArrayType)] = &Semantics::walkArrayType;

    // Walker registration for return and error statements
    walkerFunctionsMap[typeid(ErrorStatement)] = &Semantics::walkErrorStatement;

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
    walkerFunctionsMap[typeid(DataStatement)] = &Semantics::walkDataStatement;
    walkerFunctionsMap[typeid(BehaviorStatement)] = &Semantics::walkBehaviorStatement;
    walkerFunctionsMap[typeid(ComponentStatement)] = &Semantics::walkComponentStatement;
    walkerFunctionsMap[typeid(NewComponentExpression)] = &Semantics::walkNewComponentExpression;
    walkerFunctionsMap[typeid(SelfExpression)] = &Semantics::walkSelfExpression;
    walkerFunctionsMap[typeid(EnumClassStatement)] = &Semantics::walkEnumClassStatement;
    walkerFunctionsMap[typeid(InstanceExpression)] = &Semantics::walkInstanceExpression;
    walkerFunctionsMap[typeid(MethodCallExpression)] = &Semantics::walkMethodCallExpression;

    walkerFunctionsMap[typeid(AllocatorStatement)] = &Semantics::walkAllocatorInterface;
    walkerFunctionsMap[typeid(DheapStatement)] = &Semantics::walkDheapStatement;
    walkerFunctionsMap[typeid(SealStatement)] = &Semantics::walkSealStatement;

    // Walker registration for the shout system
    walkerFunctionsMap[typeid(ShoutStatement)] = &Semantics::walkShoutStatement;

    // Wlaker registration for the qualify statement
    walkerFunctionsMap[typeid(QualifyStatement)] = &Semantics::walkQualifyStatement;
    // Walker registrartion for generic system
    walkerFunctionsMap[typeid(GenericStatement)] = &Semantics::walkGenericStatement;
    walkerFunctionsMap[typeid(InstantiateStatement)] = &Semantics::walkInstantiateStatement;
}

ResolvedType Semantics::inferNodeDataType(Node *node)
{
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

    if (auto fltLit = dynamic_cast<FloatLiteral *>(node))
        return ResolvedType{DataType::FLOAT, "float"};
    if (auto dbLit = dynamic_cast<DoubleLiteral *>(node))
        return ResolvedType{DataType::DOUBLE, "double"};

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

    if (auto arrLit = dynamic_cast<ArrayLiteral *>(node))
    {
        // Empty literal → known to be array, unknown inner type
        if (arrLit->array.empty())
        {
            auto unknownInner = std::make_shared<ResolvedType>(
                ResolvedType{DataType::UNKNOWN, "unknown", false, false, false, false});

            ResolvedType result;
            result.kind = DataType::UNKNOWN;
            result.isArray = true;
            result.innerType = unknownInner;
            result.resolvedName = "arr[" + unknownInner->resolvedName + "]";
            return result;
        }

        // Infer type of first element
        ResolvedType firstType = inferNodeDataType(arrLit->array[0].get());

        // Wrap it: array literal ALWAYS produces an array type
        auto innerPtr = std::make_shared<ResolvedType>(firstType);

        // Now verify compatibility of all elements
        for (size_t i = 1; i < arrLit->array.size(); ++i)
        {
            ResolvedType elemType = inferNodeDataType(arrLit->array[i].get());

            if (!isTypeCompatible(firstType, elemType))
            {
                logSemanticErrors(
                    "Type mismatch of array member at index '" + std::to_string(i) +
                        "' expected '" + firstType.resolvedName +
                        "' but got '" + elemType.resolvedName + "'",
                    arrLit->expression.line,
                    arrLit->expression.column);

                return ResolvedType{
                    DataType::UNKNOWN, "unknown", false, false, false, false};
            }
        }

        // Build the final array type
        ResolvedType finalType;
        finalType.kind = firstType.kind;
        finalType.isArray = true;
        finalType.innerType = innerPtr;

        // Pretty print name (non-essential)
        finalType.resolvedName = "arr[" + firstType.resolvedName + "]";

        return finalType;
    }

    if (auto arrAccess = dynamic_cast<ArraySubscript *>(node))
    {
        // Extract the name of the array
        auto arrayName = arrAccess->identifier->expression.TokenLiteral;
        auto line = arrAccess->expression.line;
        auto col = arrAccess->expression.column;

        // Check for its symbolInfo
        auto arrSym = resolveSymbolInfo(arrayName);
        if (!arrSym)
        {
            logSemanticErrors("Unidentified variable '" + arrayName + "'", line, col);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        // Get the array statement info
        ResolvedType currentType = arrSym->type;
        int accessCount = arrAccess->index_exprs.size();

        for (int i = 0; i < accessCount; ++i)
        {
            ResolvedType idxType = inferNodeDataType(arrAccess->index_exprs[i].get());
            if (idxType.kind != DataType::I32)
            {
                logSemanticErrors("Array index must be of type i32", line, col);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            if (!currentType.isArray || !currentType.innerType)
            {
                logSemanticErrors("Too many indices for array '" + arrayName + "'", line, col);
                return ResolvedType{DataType::ERROR, "unknown"};
            }
            currentType = *currentType.innerType; // peel one layer
        }

        return currentType;
    }

    if (auto errExpr = dynamic_cast<ErrorExpression *>(node))
        return ResolvedType{DataType::ERROR, "error"};

    if (auto selfExpr = dynamic_cast<SelfExpression *>(node))
    {
        // Start: find the type of the component containing this method
        if (currentTypeStack.empty() || currentTypeStack.back().type.kind != DataType::COMPONENT)
            return ResolvedType{DataType::UNKNOWN, "unknown"};

        std::string currentTypeName = currentTypeStack.back().typeName;

        // Walk through the chain of fields: pos -> x -> y
        for (const auto &field : selfExpr->fields)
        {
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

    if (auto instExpr = dynamic_cast<InstanceExpression *>(node))
    {
        auto instName = instExpr->blockIdent->expression.TokenLiteral;
        auto line = instExpr->blockIdent->expression.line;
        auto col = instExpr->blockIdent->expression.column;
        auto sym = resolveSymbolInfo(instName);
        if (!sym)
        {
            logSemanticErrors("Failed to infer type for unidentified identifier '" + instName + "'", line, col);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        return sym->type;
    }

    // Dealing with the let statement node type
    if (auto letStmt = dynamic_cast<LetStatement *>(node))
    {
        auto type = dynamic_cast<BasicType *>(letStmt->type.get());
        auto letStmtDataType = type->data_token;
        return resolvedDataType(letStmtDataType, letStmt);
    }

    if (auto assignStmt = dynamic_cast<AssignmentStatement *>(node))
    {
        std::string nameToResolve;

        if (auto selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
        {
            if (selfExpr->fields.empty())
            {
                logSemanticErrors("Invalid 'self' access in assignment",
                                  assignStmt->identifier->expression.line,
                                  assignStmt->identifier->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            // Grab the LAST field in the chain
            auto lastField = selfExpr->fields.back().get();
            auto ident = dynamic_cast<Identifier *>(lastField);
            if (!ident)
            {
                logSemanticErrors("Expected identifier in 'self' field chain",
                                  assignStmt->identifier->expression.line,
                                  assignStmt->identifier->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            nameToResolve = ident->identifier.TokenLiteral;
        }
        else
        {
            nameToResolve = assignStmt->identifier->expression.TokenLiteral;
        }
        std::cout << "NAME BEING USED TO RESOLVE INSIDE INFERER: " << nameToResolve << "\n";
        auto assignSymbol = resolveSymbolInfo(nameToResolve);
        auto assignStmtVal = assignStmt->value.get();
        ResolvedType assignStmtValType = inferNodeDataType(assignStmtVal);
        if (!isTypeCompatible(assignSymbol->type, assignStmtValType))
        {
            logSemanticErrors("Type mismatch expected '" + assignStmtValType.resolvedName + "' but got '" + assignSymbol->type.resolvedName + "'", assignStmt->identifier->expression.line, assignStmt->identifier->expression.column);
        }
        else
        {
            return assignSymbol->type;
        }
    }

    if (auto newExpr = dynamic_cast<NewComponentExpression *>(node))
    {
        auto componentName = newExpr->component_name.TokenLiteral;
        int line = newExpr->expression.line;
        int column = newExpr->expression.column;
        auto componentIt = customTypesTable.find(componentName);
        if (componentIt == customTypesTable.end())
        {
            logSemanticErrors("Component '" + componentName + "' does not exist", line, column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        return componentIt->second->type;
    }

    if (auto infixExpr = dynamic_cast<InfixExpression *>(node))
    {
        return inferInfixExpressionType(infixExpr);
    }

    if (auto prefixExpr = dynamic_cast<PrefixExpression *>(node))
    {
        return inferPrefixExpressionType(prefixExpr);
    }

    if (auto postfixExpr = dynamic_cast<PostfixExpression *>(node))
    {
        return inferPostfixExpressionType(postfixExpr);
    }

    if (auto nullLit = dynamic_cast<NullLiteral *>(node))
        return {DataType::UNKNOWN, "null"}; // Will be updated based on context

    if (auto ident = dynamic_cast<Identifier *>(node))
    {
        std::string name = ident->identifier.TokenLiteral;
        std::cout << "NAME BEING USED IN IDENTIFIER INFERER: " << name << "\n";

        auto symbol = resolveSymbolInfo(name);
        if (symbol)
        {
            std::cout << "IDENTIFIER DATA TYPE: " << symbol->type.resolvedName << "\n";
            return symbol->type;
        }
        else
        {
            logSemanticErrors(
                "Undefined variable '" + name + "'",
                ident->expression.line,
                ident->expression.column);
            return {DataType::UNKNOWN, "unknown"};
        }
    }

    if (auto derefExpr = dynamic_cast<DereferenceExpression *>(node))
    {
        std::string name = extractIdentifierName(derefExpr->identifier.get());
        std::cout << "DEREF NAME BEING GIVEN DURING TYPE RESOLUTION: " << name << "\n";
        auto derefSym = resolveSymbolInfo(name);

        if (!derefSym)
        {
            logSemanticErrors("Undefined variable '" + name + "'", derefExpr->identifier->expression.line, derefExpr->identifier->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        // Get the ptr_type
        auto ptrType = derefSym->type;
        // Toggle the isPointer boolean
        ptrType.isPointer = false;
        return isPointerType(ptrType);
    }

    if (auto addrExpr = dynamic_cast<AddressExpression *>(node))
    {
        auto identType = inferNodeDataType(addrExpr->identifier.get());
        identType.isPointer = true;
        return isPointerType(identType);
    }

    if (auto basicType = dynamic_cast<BasicType *>(node))
        return tokenTypeToResolvedType(basicType->data_token, basicType->isNullable);

    if (auto arrRetType = dynamic_cast<ArrayType *>(node))
    {
        // Resolve the inner type first
        ResolvedType inner = inferNodeDataType(arrRetType->innerType.get());
        if (inner.kind == DataType::VOID)
        {
            logSemanticErrors("Cannot have a void array type", arrRetType->expression.line, arrRetType->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown", false, false, false, false};
        }
        return inner;
    }

    if (auto ptrType = dynamic_cast<PointerType *>(node))
    {
        ResolvedType inner = inferNodeDataType(ptrType->underlyingType.get());
        if (inner.kind == DataType::VOID)
        {
            logSemanticErrors("Cannot have a void pointer return type", ptrType->expression.line, ptrType->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown", false, false};
        }
        inner.isPointer = true;
        return isPointerType(inner);
    }

    if (auto refType = dynamic_cast<RefType *>(node))
    {
        ResolvedType inner = inferNodeDataType(refType->underLyingType.get());
        if (inner.kind == DataType::VOID)
        {
            logSemanticErrors("Cannot have a void reference return type", refType->expression.line, refType->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown", false, false, false};
        }
        inner.isRef = true;
        return isRefType(inner);
    }

    if (auto retTypeExpr = dynamic_cast<ReturnType *>(node))
    {
        // Just recursively call the inferer for whatever is being nested in here(Basic Type or whatever)
        return inferNodeDataType(retTypeExpr->returnExpr.get());
    }

    if (auto callExpr = dynamic_cast<CallExpression *>(node))
    {
        auto symbol = resolveSymbolInfo(callExpr->function_identifier->expression.TokenLiteral);
        if (symbol)
        {
            return symbol->type;
        }
        else
        {
            logSemanticErrors("Undefined function name '" + callExpr->function_identifier->expression.TokenLiteral + "'", callExpr->function_identifier->expression.line, callExpr->function_identifier->expression.column);
            return {DataType::UNKNOWN, "unknown"};
        }
    }

    if (auto metCall = dynamic_cast<MethodCallExpression *>(node))
    {
        auto instanceName = metCall->instance->expression.TokenLiteral;
        auto line = metCall->instance->expression.line;
        auto col = metCall->instance->expression.column;

        auto instanceSym = resolveSymbolInfo(instanceName);

        if (!instanceSym)
        {
            logSemanticErrors("Undefined instance name '" + instanceName + "' ", line, col);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
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
        if (typeIt != customTypesTable.end())
        {
            auto members = typeIt->second->members;
            auto it = members.find(callName);
            if (it == members.end())
            {
                logSemanticErrors("Function '" + callName + "' doesnt exist in type '" + type.resolvedName + "'", line, col);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            auto memInfo = it->second;
            return memInfo->type;
        }
        else if (sealIt != sealTable.end())
        {
            auto sealFnMap = sealIt->second;
            auto sealFnIt = sealFnMap.find(callName);
            if (sealFnIt == sealFnMap.end())
            {
                logSemanticErrors("Function '" + callName + "' doesnt exist in seal '" + instanceName + "'", line, col);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            auto sealInfo = sealFnIt->second;
            return sealInfo->type;
        }
        else
        {
            logSemanticErrors("Unknown type or seal '" + instanceName + "'", line, col);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
    }
    if (auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node))
    {
        auto line = unwrapExpr->expression.line;
        auto col = unwrapExpr->expression.column;
        // Get the contained call
        auto call = dynamic_cast<CallExpression *>(unwrapExpr->call.get());
        auto metCall = dynamic_cast<MethodCallExpression *>(unwrapExpr->call.get());
        std::string funcName = "<unidentified>";
        if (call)
        {
            funcName = call->function_identifier->expression.TokenLiteral;
        }
        else if (metCall)
        {
            auto innerCall = dynamic_cast<CallExpression *>(metCall->call.get());
            funcName = innerCall->function_identifier->expression.TokenLiteral;
        }

        // Resolve the symbol
        auto sym = resolveSymbolInfo(funcName);
        if (!sym)
        {
            logSemanticErrors("Undefined '" + funcName + "'", line, col);
            return ResolvedType{DataType::UNKNOWN, "unknown", false, false};
        }
        // Get the type
        auto unwrapType = sym->type;
        // Toggle the null flag
        unwrapType.isNull = false;
        auto finalType = unwrapType;
        return finalType;
    }

    if (auto ptrStmt = dynamic_cast<PointerStatement *>(node))
    {
        ResolvedType ptrType = inferNodeDataType(ptrStmt->type.get());
        // Update the is pointer flag to true
        ptrType.isPointer = true;
        return isPointerType(ptrType);
    }

    if (auto refStmt = dynamic_cast<ReferenceStatement *>(node))
    {
        ResolvedType refType = inferNodeDataType(refStmt->type.get());
        // Update the reference flag to true
        refType.isRef = true;
        return isRefType(refType);
    }

    return {DataType::UNKNOWN, "unknown"};
}

std::string Semantics::extractIdentifierName(Node *node)
{
    if (auto call = dynamic_cast<CallExpression *>(node))
    {
        auto callIdent = dynamic_cast<Identifier *>(call->function_identifier.get());
        auto identName = callIdent->identifier.TokenLiteral;
        return identName;
    }
    else if (auto ident = dynamic_cast<Identifier *>(node))
    {
        auto identName = ident->identifier.TokenLiteral;
        return identName;
    }
    else if (auto metCall = dynamic_cast<MethodCallExpression *>(node))
    {
        auto identName = "<methodfuncName>";
        return identName;
    }
    else if (auto deref = dynamic_cast<DereferenceExpression *>(node))
    {
        auto identName = extractIdentifierName(deref->identifier.get());
        return identName;
    }
    else if (auto addr = dynamic_cast<AddressExpression *>(node))
    {
        auto identName = extractIdentifierName(addr->identifier.get());
        return identName;
    }
    else
    {
        auto identName = "<Unsupported node>";
        return identName;
    }
}

ResolvedType Semantics::inferInfixExpressionType(Node *node)
{
    auto infixNode = dynamic_cast<InfixExpression *>(node);
    if (!infixNode)
        return {DataType::UNKNOWN, "unknown"};

    // Incase the left side is an identifier
    auto ident = dynamic_cast<Identifier *>(infixNode->left_operand.get());
    std::shared_ptr<SymbolInfo> symbol = nullptr;
    if (ident)
    {
        symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
    }

    TokenType operatorType = infixNode->operat.type;

    // Special case: scope or dot
    if (operatorType == TokenType::FULLSTOP || operatorType == TokenType::SCOPE_OPERATOR)
    {
        std::string parentName = infixNode->left_operand->expression.TokenLiteral;
        std::string childName = infixNode->right_operand->expression.TokenLiteral;
        return resultOfScopeOrDot(operatorType, parentName, childName, infixNode);
    }

    // --- Regular binary operator ---
    ResolvedType leftType = inferNodeDataType(infixNode->left_operand.get());
    ResolvedType rightType = inferNodeDataType(infixNode->right_operand.get());

    if (operatorType == TokenType::COALESCE)
    {
        if (!ident)
        {
            logSemanticErrors("Left-hand side of coalesce must be an identifier", ident->expression.line, ident->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        if (!symbol)
        {
            logSemanticErrors("Undefined variable '" + ident->identifier.TokenLiteral + "'", ident->expression.line, ident->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        if (!symbol->isNullable)
        {
            logSemanticErrors("Left-hand side of coalesce must be nullable", ident->expression.line, ident->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        // Right-hand side type
        rightType = inferNodeDataType(infixNode->right_operand.get());
        ResolvedType underlyingType = symbol->type;

        if (!isTypeCompatible(underlyingType, rightType))
        {
            logSemanticErrors("Type of fallback in coalesce does not match nullable type", ident->expression.line, ident->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        // Result is the underlying type
        return underlyingType;
    }

    if (ident)
    {
        auto symbol = resolveSymbolInfo(ident->identifier.TokenLiteral);
        if (!symbol)
        {
            logSemanticErrors("Undefined variable '" + ident->identifier.TokenLiteral + "'",
                              ident->expression.line, ident->expression.column);
            return {DataType::UNKNOWN, "unknown"};
        }
        if (!symbol->isInitialized)
        {
            logSemanticErrors("Cannot use an uninitialized variable '" + ident->identifier.TokenLiteral + "' in operations",
                              ident->expression.line, ident->expression.column);
            symbol->hasError = true;
            return {DataType::UNKNOWN, "unknown"};
        }
        if (symbol->isDefinitelyNull)
        {
            logSemanticErrors("Cannot use definitely-null variable '" + ident->identifier.TokenLiteral + "' in operations",
                              ident->expression.line, ident->expression.column);
            symbol->hasError = true;
            return {DataType::UNKNOWN, "unknown"};
        }
    }

    auto infixType = resultOfBinary(operatorType, leftType, rightType);
    if (infixType.kind == DataType::UNKNOWN)
    {
        logSemanticErrors("Infix type issue between '" + leftType.resolvedName + "' and '" + rightType.resolvedName + "'", infixNode->left_operand->expression.line, infixNode->left_operand->expression.column);
        return infixType; // Return the unknown type
    }

    return infixType; // Return the correct type
}

ResolvedType Semantics::inferPrefixExpressionType(Node *node)
{
    auto prefixNode = dynamic_cast<PrefixExpression *>(node);
    if (!prefixNode)
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    std::cout << "[SEMANTIC LOG] Infering prefix type\n";
    auto prefixOperator = prefixNode->operat.type;
    ResolvedType operandType = inferNodeDataType(prefixNode->operand.get());
    return resultOfUnary(prefixOperator, operandType);
}

ResolvedType Semantics::inferPostfixExpressionType(Node *node)
{
    auto postfixNode = dynamic_cast<PostfixExpression *>(node);
    if (!postfixNode)
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    ResolvedType operandType = inferNodeDataType(postfixNode->operand.get());
    auto postfixOperator = postfixNode->operator_token.type;
    return resultOfUnary(postfixOperator, operandType);
}

ResolvedType Semantics::resultOfScopeOrDot(TokenType operatorType, const std::string &parentName, const std::string &childName, InfixExpression *infixExpr)
{
    std::cout << "INSIDE SCOPE RESOLVER FOR INFIX\n";

    if (operatorType != TokenType::FULLSTOP && operatorType != TokenType::SCOPE_OPERATOR)
        return ResolvedType{DataType::UNKNOWN, "unknown"};

    // First, check if parentName is a **variable in current scope**
    auto varSymbol = resolveSymbolInfo(parentName);
    if (varSymbol)
    {
        auto varType = varSymbol->type;
        // Block cases where the parentName is an actual type too
        if (varType.resolvedName == parentName)
        {
            logSemanticErrors("Must instantiate type '" + varType.resolvedName + "' to access its members", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        if (operatorType == TokenType::FULLSTOP)
        {
            if (varType.kind == DataType::ENUM)
            {
                logSemanticErrors("Dot operator applied to non-component variable", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            // Look up the type definition in customTypesTable
            auto typeIt = customTypesTable.find(varType.resolvedName);
            if (typeIt == customTypesTable.end())
            {
                logSemanticErrors("Component type '" + varType.resolvedName + "' not found", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            // Look for childName in members
            auto memberIt = typeIt->second->members.find(childName);
            if (memberIt == typeIt->second->members.end())
            {
                logSemanticErrors("Component '" + varType.resolvedName + "' has no member '" + childName + "'", infixExpr->right_operand->expression.line, infixExpr->right_operand->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            return memberIt->second->type;
        }
        else if (operatorType == TokenType::SCOPE_OPERATOR)
        {
            if (varType.kind != DataType::ENUM)
            {
                logSemanticErrors("Scope operator(::) applied to none enum  variable", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            // Look for the definition in the custom types table
            auto typeIt = customTypesTable.find(varType.resolvedName);
            if (typeIt == customTypesTable.end())
            {
                logSemanticErrors("Type '" + varType.resolvedName + "' not found", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            // Look for the childName in members
            auto memIt = typeIt->second->members.find(childName);
            if (memIt == typeIt->second->members.end())
            {
                logSemanticErrors("Type '" + varType.resolvedName + "' does not have a member '" + childName + "'", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
                return ResolvedType{DataType::UNKNOWN, "unknown"};
            }

            if (varType.kind == DataType::ENUM)
            {
                return memIt->second->parentType;
            }
            return memIt->second->type;
        }
    }

    // If parentName is not a variable, treat it as a **type lookup** (this case will only work for use statements and enums)
    auto typeIt = customTypesTable.find(parentName);
    if (typeIt == customTypesTable.end())
    {
        logSemanticErrors("Parent name '" + parentName + "' does not exist", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    if (operatorType == TokenType::SCOPE_OPERATOR)
    {
        if (!(typeIt->second->type.kind == DataType::ENUM))
        {
            logSemanticErrors("Only use :: to access members of enums", infixExpr->left_operand->expression.line, infixExpr->left_operand->expression.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
    }

    // Look for the member in the type
    auto memberIt = typeIt->second->members.find(childName);
    if (memberIt == typeIt->second->members.end())
    {
        logSemanticErrors("Type '" + parentName + "' does not have member '" + childName + "'", infixExpr->right_operand->expression.line, infixExpr->right_operand->expression.column);
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    return memberIt->second->type;
}

ResolvedType Semantics::resultOfBinary(TokenType operatorType, ResolvedType leftType, ResolvedType rightType)
{
    // Logical operators: &&, ||
    if (operatorType == TokenType::AND || operatorType == TokenType::OR)
    {
        if (isBoolean(leftType) && isBoolean(rightType))
            return ResolvedType{DataType::BOOLEAN, "boolean"};
        else
            return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    if (operatorType == TokenType::ASSIGN)
    {
        std::cerr << COLOR_RED << "[SEMANTIC ERROR]" << COLOR_RESET << "Cannot use '=' in binary operations as it only for assignments\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    // Comparison operators
    bool isComparison = (operatorType == TokenType::GREATER_THAN ||
                         operatorType == TokenType::GT_OR_EQ ||
                         operatorType == TokenType::LESS_THAN ||
                         operatorType == TokenType::LT_OR_EQ ||
                         operatorType == TokenType::EQUALS ||
                         operatorType == TokenType::NOT_EQUALS);

    if (isComparison)
    {
        if (leftType.kind == rightType.kind)
            return ResolvedType{DataType::BOOLEAN, "boolean"};

        std::cerr << COLOR_RED << "[SEMANTIC ERROR]" << COLOR_RESET << "Cannot compare " << leftType.resolvedName << " and " << rightType.resolvedName << "\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    // String concatenation
    if (operatorType == TokenType::PLUS && isString(leftType) && isString(rightType))
    {
        return ResolvedType{DataType::STRING, "string"};
    }

    // Arithmetic operators: +, -, %, /, *
    bool isArithmetic = (operatorType == TokenType::PLUS ||
                         operatorType == TokenType::MINUS ||
                         operatorType == TokenType::MODULUS ||
                         operatorType == TokenType::DIVIDE ||
                         operatorType == TokenType::ASTERISK);

    if (isArithmetic)
    {
        // Promote mixed int/float combinations
        if ((isInteger(leftType) && isFloat(rightType)) || (isFloat(leftType) && isInteger(rightType)))
        {
            return ResolvedType{DataType::FLOAT, "float"};
        }
        // Promote int/double or float/double to double
        if ((isInteger(leftType) && rightType.kind == DataType::DOUBLE) || (leftType.kind == DataType::DOUBLE && isInteger(rightType)))
        {
            return ResolvedType{DataType::DOUBLE, "double"};
        }
        if ((leftType.kind == DataType::FLOAT && rightType.kind == DataType::DOUBLE) ||
            (leftType.kind == DataType::DOUBLE && rightType.kind == DataType::FLOAT))
        {
            return ResolvedType{DataType::DOUBLE, "double"};
        }

        if (leftType.kind == rightType.kind)
        {
            return leftType;
        }

        if (isTypeCompatible(leftType, rightType))
        {
            return rightType;
        }

        std::cerr << COLOR_RED << "[SEMANTIC ERROR]" << COLOR_RESET << " Type mismatch '" << leftType.resolvedName << "' does not match '" << rightType.resolvedName << "'" << "\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }

    std::cerr << COLOR_RED << "[SEMANTIC ERROR]" << COLOR_RESET << "Unknown binary operator: " << TokenTypeToLiteral(operatorType) << " with types "
              << leftType.resolvedName << " and " << rightType.resolvedName << "\n";
    return ResolvedType{DataType::UNKNOWN, "unknown"};
}

ResolvedType Semantics::resultOfUnary(TokenType operatorType, const ResolvedType &operandType)
{
    switch (operatorType)
    {
    case TokenType::BANG:
        if (!isBoolean(operandType))
        {
            std::cerr << "[SEMANTIC ERROR] Cannot apply '!' to type " << operandType.resolvedName << "\n";
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        return ResolvedType{DataType::BOOLEAN, "bool"};

    case TokenType::MINUS:
    case TokenType::PLUS:
    case TokenType::PLUS_PLUS:
    case TokenType::MINUS_MINUS:
        if (isInteger(operandType) || isFloat(operandType))
            return operandType;
        std::cerr << COLOR_RED << "[SEMANTIC ERROR]" << COLOR_RESET << "Cannot apply " << TokenTypeToLiteral(operatorType) << " to " << operandType.resolvedName << "\n";
        return ResolvedType{DataType::UNKNOWN, "unknown"};

    default:
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }
}

std::shared_ptr<SymbolInfo> Semantics::resolveSymbolInfo(const std::string &name)
{
    for (int i = symbolTable.size() - 1; i >= 0; --i)
    {
        auto &scope = symbolTable[i];
        std::cout << "[SEMANTIC LOG] Searching for '" << name << "' in scope level " << i << "\n";
        for (auto &[key, value] : scope)
        {
            std::cout << "    >> Key in scope: '" << key << "'\n";
        }
        if (scope.find(name) != scope.end())
        {
            std::cout << "[SEMANTIC LOG] Found match for '" << name << "'\n";
            return scope[name];
        }
    }
    std::cout << "[SEMANTIC LOG] No match for '" << name << "'\n";
    return nullptr;
}

std::shared_ptr<SymbolInfo> Semantics::lookUpInCurrentScope(const std::string &name)
{
    if (symbolTable.empty())
    {
        std::cout << "[SEMANTIC LOG] ERROR: Attempted current scope lookup on empty symbol table.\n";
        return nullptr;
    }

    // Access the current scope which is the latest scope in the symbol table
    auto &currentScope = symbolTable.back();

    std::cout << "[SEMANTIC LOG] Searching for '" << name << "' in CURRENT scope level "
              << symbolTable.size() - 1 << " only.\n";

    if (currentScope.find(name) != currentScope.end())
    {
        std::cout << "[SEMANTIC LOG] Found match for '" << name << "' in current scope.\n";
        return currentScope.at(name); // Use .at() or [] for retrieval
    }

    std::cout << "[SEMANTIC LOG] No match for '" << name << "' in current scope.\n";
    return nullptr;
}

bool Semantics::isGlobalScope()
{
    if (symbolTable.size() == 1)
    {
        return true;
    }
    return false;
}

ResolvedType Semantics::tokenTypeToResolvedType(Token token, bool isNullable)
{
    auto makeType = [&](DataType nonNull, const std::string &baseName)
    {
        if (isNullable)
            return ResolvedType{nonNull, baseName + "?", false, isNullable};
        else
            return ResolvedType{nonNull, baseName};
    };

    switch (token.type)
    {
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

    case TokenType::FLOAT_KEYWORD:
        return makeType(DataType::FLOAT, "float");
    case TokenType::DOUBLE_KEYWORD:
        return makeType(DataType::DOUBLE, "double");
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

    case TokenType::VOID:
        return {DataType::VOID, "void"};

    case TokenType::IDENTIFIER:
    {
        auto [parentName, childName] = splitScopedName(token.TokenLiteral);
        auto parentIt = customTypesTable.find(parentName);
        if (parentIt != customTypesTable.end())
        {
            if (!childName.empty())
            {
                logSemanticErrors("Type name must be a single identifier; scoped access (:: or .) is not allowed here.",
                                  token.line, token.column);
                return {DataType::UNKNOWN, "unknown"};
            }

            auto parentType = parentIt->second->type;
            parentType.isNull = isNullable;
            return parentType;
        }

        logSemanticErrors("Unknown type identifier '" + token.TokenLiteral + "'", token.line, token.column);
        return {DataType::UNKNOWN, "unknown"};
    }

    default:
        return {DataType::UNKNOWN, "unknown", false, false};
    }
}

bool Semantics::isTypeCompatible(const ResolvedType &expected, const ResolvedType &actual)
{
    // Pointers must match
    if (expected.isPointer != actual.isPointer)
        return false;

    if (expected.isArray || actual.isArray)
    {
        if (expected.isArray != actual.isArray)
            return false;
        // one is array, one is not

        // If either inner type is missing → mismatch
        if (!expected.innerType || !actual.innerType)
            return false;

        // Recurse on inner types
        return isTypeCompatible(*expected.innerType, *actual.innerType);
    }

    // References must match
    if (expected.isRef != actual.isRef)
        return false;

    // Error type can always pass through
    if (actual.kind == DataType::ERROR)
        return true;

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

bool Semantics::hasReturnPath(Node *node)
{
    if (currentFunction && currentFunction.value()->returnType.kind == DataType::VOID)
    {
        return true; // Void functions don't need returns
    }

    if (auto blockStmt = dynamic_cast<BlockStatement *>(node))
    {
        for (const auto &stmt : blockStmt->statements)
        {
            if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get()))
            {
                if (retStmt->error_val || retStmt->return_value ||
                    (currentFunction.value()->isNullable && !retStmt->return_value))
                {
                    return true; // Error, value, or null return
                }
            }
            if (auto ifStmt = dynamic_cast<ifStatement *>(stmt.get()))
            {
                auto thenBlock = dynamic_cast<BlockStatement *>(ifStmt->if_result.get());
                bool hasThenReturn = thenBlock && hasReturnPath(thenBlock);
                bool hasElseReturn = ifStmt->else_result.has_value() &&
                                     hasReturnPath(dynamic_cast<BlockStatement *>(
                                         ifStmt->else_result.value().get()));
                if (hasThenReturn && hasElseReturn)
                {
                    return true;
                }
                bool hasElifReturn = true;
                for (const auto &elif : ifStmt->elifClauses)
                {
                    auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
                    if (elifStmt)
                    {
                        auto elifBlock = dynamic_cast<BlockStatement *>(elifStmt->elif_result.get());
                        hasElifReturn &= elifBlock && hasReturnPath(elifBlock);
                    }
                }
                if (hasThenReturn && hasElifReturn && hasElseReturn)
                {
                    return true;
                }
            }
        }
        return false; // BlockStatement has no finalexpr
    }

    if (auto blockExpr = dynamic_cast<BlockExpression *>(node))
    {
        for (const auto &stmt : blockExpr->statements)
        {
            if (auto retStmt = dynamic_cast<ReturnStatement *>(stmt.get()))
            {
                if (retStmt->error_val || retStmt->return_value ||
                    (currentFunction.value()->isNullable && !retStmt->return_value))
                {
                    return true; // Error, value, or null return
                }
            }
            if (auto ifStmt = dynamic_cast<ifStatement *>(stmt.get()))
            {
                auto thenBlock = dynamic_cast<BlockStatement *>(ifStmt->if_result.get());
                bool hasThenReturn = thenBlock && hasReturnPath(thenBlock);
                bool hasElseReturn = ifStmt->else_result.has_value() &&
                                     hasReturnPath(dynamic_cast<BlockStatement *>(
                                         ifStmt->else_result.value().get()));
                if (hasThenReturn && hasElseReturn)
                {
                    return true;
                }
                bool hasElifReturn = true;
                for (const auto &elif : ifStmt->elifClauses)
                {
                    auto elifStmt = dynamic_cast<elifStatement *>(elif.get());
                    if (elifStmt)
                    {
                        auto elifBlock = dynamic_cast<BlockStatement *>(elifStmt->elif_result.get());
                        hasElifReturn &= elifBlock && hasReturnPath(elifBlock);
                    }
                }
                if (hasThenReturn && hasElifReturn && hasElseReturn)
                {
                    return true;
                }
            }
        }
        if (blockExpr->finalexpr.has_value())
        {
            ResolvedType exprType = inferNodeDataType(blockExpr->finalexpr.value().get());
            return exprType.kind == DataType::ERROR ||
                   isTypeCompatible(currentFunction.value()->returnType, exprType) ||
                   (dynamic_cast<NullLiteral *>(blockExpr->finalexpr.value().get()) &&
                    currentFunction.value()->isNullable);
        }
        return false;
    }

    return false;
}

bool Semantics::areSignaturesCompatible(const SymbolInfo &declInfo, FunctionExpression *funcExpr)
{

    if (declInfo.paramTypes.size() != funcExpr->call.size())
    {
        return false;
    }
    for (size_t i = 0; i < declInfo.paramTypes.size(); ++i)
    {
        auto letStmt = dynamic_cast<LetStatement *>(funcExpr->call[i].get());
        if (!letStmt)
            return false;
        auto type = dynamic_cast<BasicType *>(letStmt->type.get());
        ResolvedType paramType = inferNodeDataType(type);
        std::string paramGenericName = type->data_token.type == TokenType::IDENTIFIER ? type->data_token.TokenLiteral : "";
        // Find declaration's parameter metadata
        bool declParamNullable = false;
        bool isLetNullable = type->isNullable;
        for (const auto &pair : metaData)
        {
            if (auto declLetStmt = dynamic_cast<LetStatement *>(pair.first))
            {
                if (declLetStmt->ident_token.TokenLiteral == letStmt->ident_token.TokenLiteral &&
                    pair.second->type.kind == declInfo.paramTypes[i].first.kind &&
                    pair.second->genericName == declInfo.paramTypes[i].second)
                {
                    declParamNullable = pair.second->isNullable;
                    break;
                }
            }
        }
        if (paramType.kind != declInfo.paramTypes[i].first.kind ||
            paramGenericName != declInfo.paramTypes[i].second ||
            isLetNullable != declParamNullable)
        {
            return false;
        }
    }

    // Check return type
    auto retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
    if (!retType)
        return false;
    ResolvedType returnType = inferNodeDataType(retType->returnExpr.get());
    std::string returnGenericName = retType->expression.type == TokenType::IDENTIFIER ? retType->expression.TokenLiteral : "";
    return returnType.kind == declInfo.returnType.kind;
}

bool Semantics::signaturesMatchBehaviorDeclaration(const std::shared_ptr<MemberInfo> &declMember, FunctionExpression *funcExpr)
{
    if (!declMember)
        return false;

    if (!funcExpr)
        return false;

    const auto &declParams = declMember->paramTypes;
    if (declParams.size() != funcExpr->call.size())
        return false;

    for (size_t i = 0; i < declMember->paramTypes.size(); ++i)
    {
        auto letStmt = dynamic_cast<LetStatement *>(funcExpr->call[i].get());
        if (!letStmt)
            return false;
        auto type = dynamic_cast<BasicType *>(letStmt->type.get());
        ResolvedType paramType = inferNodeDataType(type);
        std::string paramGenericName = type->data_token.type == TokenType::IDENTIFIER ? type->data_token.TokenLiteral : "";
        // Find declaration's parameter metadata
        bool declParamNullable = false;
        bool isLetNullable = type->isNullable;
        for (const auto &pair : metaData)
        {
            if (auto declLetStmt = dynamic_cast<LetStatement *>(pair.first))
            {
                if (declLetStmt->ident_token.TokenLiteral == letStmt->ident_token.TokenLiteral &&
                    pair.second->type.kind == declMember->paramTypes[i].first.kind &&
                    pair.second->genericName == declMember->paramTypes[i].second)
                {
                    declParamNullable = pair.second->isNullable;
                    break;
                }
            }
        }
        if (paramType.kind != declMember->paramTypes[i].first.kind ||
            paramGenericName != declMember->paramTypes[i].second ||
            isLetNullable != declParamNullable)
        {
            return false;
        }
    }

    // Check return type
    auto retType = dynamic_cast<ReturnType *>(funcExpr->return_type.get());
    if (!retType)
        return false;
    ResolvedType returnType = inferNodeDataType(retType->returnExpr.get());
    std::string returnGenericName = retType->expression.type == TokenType::IDENTIFIER ? retType->expression.TokenLiteral : "";
    return returnType.kind == declMember->returnType.kind;
}

bool Semantics::isMethodCallCompatible(const MemberInfo &memFuncInfo, CallExpression *callExpr)
{
    bool allGood = true;

    auto funcName = callExpr->function_identifier->expression.TokenLiteral;

    if (memFuncInfo.paramTypes.size() != callExpr->parameters.size())
    {
        logSemanticErrors("Function '" + funcName + "' call has " + std::to_string(callExpr->parameters.size()) +
                              " arguments, but expects " + std::to_string(memFuncInfo.paramTypes.size()),
                          callExpr->expression.line, callExpr->expression.column);
        return false;
    }

    for (size_t i = 0; i < callExpr->parameters.size(); ++i)
    {
        auto &param = callExpr->parameters[i];
        const auto &expectedType = memFuncInfo.paramTypes[i]; // pair<ResolvedType, string>
        ResolvedType argType = inferNodeDataType(param.get());

        if (argType.kind == DataType::UNKNOWN)
        {
            logSemanticErrors("Could not infer type for argument " + std::to_string(i + 1),
                              param->expression.line, param->expression.column);
            allGood = false;
            continue;
        }

        // --- Nullability rule ---
        if (auto nullLit = dynamic_cast<NullLiteral *>(param.get()))
        {
            if (expectedType.first.isNull)
            {
                argType = expectedType.first; // promote null → nullable type
            }
            else
            {
                logSemanticErrors("Cannot pass null to non-nullable parameter " +
                                      std::to_string(i + 1) + ": expected " + expectedType.first.resolvedName,
                                  param->expression.line, param->expression.column);
                allGood = false;
                continue;
            }
        }

        // --- Type strictness ---
        if (argType.kind != expectedType.first.kind)
        {
            logSemanticErrors("Call for '" + funcName + "'has a type mismatch in argument " + std::to_string(i + 1) +
                                  ", expected '" + expectedType.first.resolvedName +
                                  "' but got '" + argType.resolvedName + "'",
                              param->expression.line, param->expression.column);
            allGood = false;
            continue;
        }

        // --- Incase of Generics  ---
        if (argType.resolvedName != expectedType.first.resolvedName)
        {
            logSemanticErrors(
                "Argument type mismatch, expected '" + expectedType.first.resolvedName +
                    "' but got '" + argType.resolvedName + "' in call for '" + funcName + "'",
                param->expression.line, param->expression.column);
            allGood = false;
            continue;
        }
    }

    return allGood;
}

bool Semantics::isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr)
{
    bool allGood = true;

    auto funcName = callExpr->function_identifier->expression.TokenLiteral;

    // 1. Check parameter count
    if (funcInfo.paramTypes.size() != callExpr->parameters.size())
    {
        logSemanticErrors("Function '" + funcName + "' call has " + std::to_string(callExpr->parameters.size()) +
                              " arguments, but expected " + std::to_string(funcInfo.paramTypes.size()),
                          callExpr->expression.line, callExpr->expression.column);
        return false;
    }

    for (size_t i = 0; i < callExpr->parameters.size(); ++i)
    {
        auto &param = callExpr->parameters[i];
        const auto &expectedType = funcInfo.paramTypes[i]; // pair<ResolvedType, string>
        ResolvedType argType = inferNodeDataType(param.get());

        if (argType.kind == DataType::UNKNOWN)
        {
            logSemanticErrors("Could not infer type for argument " + std::to_string(i + 1),
                              param->expression.line, param->expression.column);
            allGood = false;
            continue;
        }

        // --- Nullability rule ---
        if (auto nullLit = dynamic_cast<NullLiteral *>(param.get()))
        {
            if (expectedType.first.isNull)
            {
                argType = expectedType.first; // promote null → nullable type
            }
            else
            {
                logSemanticErrors("Cannot pass null to non-nullable parameter " +
                                      std::to_string(i + 1) + ": expected " + expectedType.first.resolvedName,
                                  param->expression.line, param->expression.column);
                allGood = false;
                continue;
            }
        }

        // --- Type strictness ---
        if (argType.kind != expectedType.first.kind)
        {
            logSemanticErrors("Call for '" + funcName + "'has a type mismatch in argument " + std::to_string(i + 1) +
                                  ", expected '" + expectedType.first.resolvedName +
                                  "' but got '" + argType.resolvedName + "'",
                              param->expression.line, param->expression.column);
            allGood = false;
            continue;
        }

        // --- Incase of Generics  ---
        if (argType.resolvedName != expectedType.first.resolvedName)
        {
            logSemanticErrors(
                "Argument type mismatch, expected '" + expectedType.first.resolvedName +
                    "' but got '" + argType.resolvedName + "' in call for '" + funcName + "'",
                param->expression.line, param->expression.column);
            allGood = false;
            continue;
        }
    }
    return allGood;
}

int Semantics::inferLiteralDimensions(ArrayLiteral *lit)
{
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

void Semantics::inferSizePerDimension(ArrayLiteral *lit, std::vector<int64_t> &sizes)
{
    sizes.push_back(lit->array.size());
    if (!lit->array.empty())
    {
        if (auto inner = dynamic_cast<ArrayLiteral *>(lit->array[0].get()))
        {
            inferSizePerDimension(inner, sizes);
        }
    }
}

ArrayTypeInfo Semantics::getArrayTypeInfo(Node *node)
{
    auto arrLit = dynamic_cast<ArrayLiteral *>(node);
    if (!arrLit)
        return ArrayTypeInfo{ResolvedType{DataType::UNKNOWN, "unknown", false, false, false, false}, 0, {}};

    // Underlying element type = type of the deepest element
    Node *cur = node;
    while (auto inner = dynamic_cast<ArrayLiteral *>(cur))
        cur = inner->array[0].get();

    ResolvedType underlying = inferNodeDataType(cur);

    int dim = inferLiteralDimensions(arrLit);
    std::vector<int64_t> sizePerDim;
    inferSizePerDimension(arrLit, sizePerDim);

    return ArrayTypeInfo{underlying, dim, sizePerDim};
}

void Semantics::logSemanticErrors(const std::string &message, int tokenLine, int tokenColumn)
{
    CompilerError error;
    error.level = ErrorLevel::SEMANTIC;
    error.line = tokenLine;
    error.col = tokenColumn;
    error.message = message;
    error.hints = {};

    errorHandler.report(error);
}

bool Semantics::isInteger(const ResolvedType &t)
{
    static const std::unordered_set<DataType> intTypes = {
        DataType::I8, DataType::U8, DataType::I16, DataType::U16, DataType::I32, DataType::U32,
        DataType::I64, DataType::U64, DataType::I128, DataType::U128, DataType::ISIZE, DataType::USIZE};
    return intTypes.count(t.kind) > 0;
}

bool Semantics::isFloat(const ResolvedType &t)
{
    return t.kind == DataType::FLOAT || t.kind == DataType::DOUBLE;
}

bool Semantics::isBoolean(const ResolvedType &t)
{
    return t.kind == DataType::BOOLEAN;
}

bool Semantics::isString(const ResolvedType &t)
{
    return t.kind == DataType::STRING;
}

bool Semantics::isChar(const ResolvedType &t)
{
    static const std::unordered_set<DataType> charTypes = {
        DataType::CHAR8,
        DataType::CHAR16,
        DataType::CHAR32};
    return charTypes.count(t.kind) > 0;
}

ResolvedType Semantics::resolvedDataType(Token token, Node *node)
{
    std::cout << "INSIDE TYPE RESOLVER\n";
    TokenType type = token.type;

    switch (type)
    {
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

    case TokenType::FLOAT_KEYWORD:
        return ResolvedType{DataType::FLOAT, "float"};
    case TokenType::DOUBLE_KEYWORD:
        return ResolvedType{DataType::DOUBLE, "double"};

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

    case TokenType::AUTO:
    {
        auto letStmt = dynamic_cast<LetStatement *>(node);
        auto type = dynamic_cast<BasicType *>(node);
        auto letStmtValue = letStmt->value.get();
        if (!letStmtValue)
        {
            logSemanticErrors("Cannot infer without a value", type->data_token.line, type->data_token.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }
        auto inferred = inferNodeDataType(letStmtValue);
        return ResolvedType{inferred.kind, inferred.resolvedName};
    }

    // Dealing with custom types now
    case TokenType::IDENTIFIER:
    {
        std::cout << "INSIDE CUSTOM TYPE RESOLVER\n";
        // Extract the identifier as this how the parser is logging the correct types
        // Case 1 is for let statements
        auto letStmt = dynamic_cast<LetStatement *>(node);
        auto type = dynamic_cast<BasicType *>(node);
        // Extract the custom data type
        auto letStmtType = type->data_token.TokenLiteral;

        // Search for the name in the custom types table
        auto typeIt = customTypesTable.find(letStmtType);
        if (typeIt == customTypesTable.end())
        {
            logSemanticErrors("Type '" + letStmtType + "' is unknown", type->data_token.line, type->data_token.column);
            return ResolvedType{DataType::UNKNOWN, "unknown"};
        }

        return ResolvedType{typeIt->second->type.kind, typeIt->second->typeName};
    }

    default:
        return ResolvedType{DataType::UNKNOWN, "unknown"};
    }
}

ResolvedType Semantics::isPointerType(ResolvedType t)
{
    // Inline lambda to check if a string ends with a suffix
    auto endsWith = [](const std::string &str, const std::string &suffix) -> bool
    {
        if (str.length() < suffix.length())
            return false;
        return str.compare(str.length() - suffix.length(), suffix.length(), suffix) == 0;
    };

    auto ptrType = [&](DataType baseType, bool isPtr, std::string baseName)
    {
        if (isPtr)
        {
            if (!endsWith(baseName, "_ptr"))
                baseName += "_ptr";
        }
        else
        {
            if (endsWith(baseName, "_ptr"))
                baseName = baseName.substr(0, baseName.size() - 4);
        }

        return ResolvedType{baseType, baseName, isPtr};
    };

    return ptrType(t.kind, t.isPointer, t.resolvedName);
}

ResolvedType Semantics::isRefType(ResolvedType t)
{
    // Inline lambda to check if a string ends with a suffix
    auto endsWith = [](const std::string &str, const std::string &suffix) -> bool
    {
        if (str.length() < suffix.length())
            return false;
        return str.compare(str.length() - suffix.length(), suffix.length(), suffix) == 0;
    };

    auto refType = [&](DataType baseType, bool isRef, std::string baseName)
    {
        if (isRef)
        {
            if (!endsWith(baseName, "_ref"))
                baseName += "_ref";
        }
        else
        {
            if (endsWith(baseName, "_ref"))
                baseName = baseName.substr(0, baseName.size() - 4);
        }

        return ResolvedType{baseType, baseName, false, isRef};
    };

    return refType(t.kind, t.isRef, t.resolvedName);
}

ResolvedType Semantics::makeArrayType(const ResolvedType &t, int dimensionCount)
{
    ResolvedType out = t; // base element type

    for (int i = 0; i < dimensionCount; i++)
    {
        ResolvedType arr;
        arr.isArray = true;
        arr.kind = t.kind;                                   // always base element type
        arr.innerType = std::make_shared<ResolvedType>(out); // wrap previous
        arr.resolvedName = "arr[" + out.resolvedName + "]";

        out = arr;
    }

    return out;
}

std::pair<std::string, std::string> Semantics::splitScopedName(const std::string &fullName)
{
    std::cout << "Splitting name\n";
    size_t pos = fullName.find("::");

    if (pos == std::string::npos)
        pos = fullName.find('.');

    if (pos == std::string::npos)
    {
        // no scope operator just a plain type
        return {fullName, ""};
    }
    std::string parent = fullName.substr(0, pos);
    std::string child = fullName.substr(pos + (fullName[pos] == ':' ? 2 : 1)); // Skipping :: if not .
    std::cout << "Name has been split into '" + parent + "' and '" + child + "'\n";
    return {parent, child};
}

void Semantics::popScope()
{
    auto &scope = symbolTable.back();
    for (auto &[name, sym] : scope)
    {
        // If the symbol we come across is a reference and it is referencing validly
        if (sym->isRef && sym->refereeSymbol)
        {
            std::cout << "Initial refCount: " << sym->refereeSymbol->refCount << "\n";
            if (sym->refereeSymbol->refCount > 0)
            {
                sym->refereeSymbol->refCount -= 1;
                std::cout << "[SEMANTIC LOG]: Ref '" << name
                          << "' released -> target '"
                          << "' refCount now " << sym->refereeSymbol->refCount << "\n";
            }
        }
    }
    std::cout << "[SCOPE EXITED]\n";
    symbolTable.pop_back();
}