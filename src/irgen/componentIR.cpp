#include "irgen.hpp"

void IRGenerator::generateRecordStatement(Node* node) {
    auto recordStmt = dynamic_cast<RecordStatement*>(node);
    if (!recordStmt) return;

    auto sym = semantics.getSymbolFromMeta(recordStmt);
    if (!sym) reportDevBug("Miising record symbol info", recordStmt->recordName.get());

    std::string blockName = recordStmt->recordName->expression.TokenLiteral;

    // Get the struct
    llvm::StructType* structTy = llvmCustomTypes[blockName];
    if (!structTy) {
        // Fallback if not pre-declared
        structTy = llvm::StructType::create(context, blockName);
        llvmCustomTypes[blockName] = structTy;
    }

    if (structTy->isOpaque()) {
        std::vector<llvm::Type*> memberTypes;

        // Create a temporary vector to hold types in the correct order
        memberTypes.resize(sym->members.size());

        for (auto& pair : sym->members) {
            std::shared_ptr<MemberInfo> info = pair.second;
            llvm::Type* ty = getLLVMType(info->type);
            memberTypes[info->memberIndex] = ty;
        }
        structTy->setBody(memberTypes);
    }
    sym->codegen().llvmType = structTy;

    logInternal("Defined Type Body for: " + blockName);
}

llvm::Value* IRGenerator::generateInstanceExpression(Node* node) {
    auto instExpr = dynamic_cast<InstanceExpression*>(node);
    if (!instExpr) return nullptr;

    std::string instName = instExpr->blockIdent->expression.TokenLiteral;

    llvm::StructType* structTy = llvmCustomTypes[instName];
    if (!structTy) reportDevBug("Unknown record type: " + instName, instExpr->blockIdent.get());

    if (!funcBuilder.GetInsertBlock())
        reportDevBug("Cannot create instance for '" + instName + "' in global scope",
                     instExpr->blockIdent.get());

    llvm::Value* instancePtr = funcBuilder.CreateAlloca(structTy, nullptr, instName + "_inst");

    auto typeInfoIt = semantics.customTypesTable.find(instName);
    if (typeInfoIt == semantics.customTypesTable.end())
        reportDevBug("Missing custom type info for '" + instName + "'", instExpr->blockIdent.get());

    auto& typeInfo = typeInfoIt->second;

    // Map user provided field assignments by name
    std::unordered_map<std::string, AssignmentStatement*> userInits;
    for (auto& fieldStmt : instExpr->fields) {
        if (auto assign = dynamic_cast<AssignmentStatement*>(fieldStmt.get()))
            userInits[assign->identifier->expression.TokenLiteral] = assign;
    }

    auto instanceSym = semantics.getSymbolFromMeta(instExpr);
    bool isInstanceVolatile = instanceSym && instanceSym->storage().isVolatile;

    // Iterate through definition to ensure total field coverage
    for (auto const& [memberName, info] : typeInfo->members) {
        llvm::Value* memberPtr =
            funcBuilder.CreateStructGEP(structTy, instancePtr, info->memberIndex, memberName);

        llvm::Value* finalVal = nullptr;

        // User provided a value for this field
        auto it = userInits.find(memberName);
        if (it != userInits.end()) {
            finalVal = generateExpression(it->second->value.get());
        } else {
            // Fallback to declaration default or zero
            // LetStatement is dead — fields are VariableDeclaration now
            auto varDecl = dynamic_cast<VariableDeclaration*>(info->node);
            if (varDecl && varDecl->initializer) {
                finalVal = generateExpression(varDecl->initializer.get());
            } else {
                finalVal = llvm::Constant::getNullValue(getLLVMType(info->type));
            }
        }

        if (!finalVal)
            reportDevBug("Failed to resolve value for field '" + memberName + "'", info->node);

        auto* storeInst = funcBuilder.CreateStore(finalVal, memberPtr);
        if (isInstanceVolatile || info->isVolatile) storeInst->setVolatile(true);
    }

    // Return the loaded struct value
    auto* finalLoad = funcBuilder.CreateLoad(structTy, instancePtr, instName + "_val");
    if (isInstanceVolatile) finalLoad->setVolatile(true);

    return finalLoad;
}

void IRGenerator::generateInitFunction(Node* node, ComponentStatement* component) {
    auto* initStmt = dynamic_cast<InitStatement*>(node);
    if (!initStmt || !component) return;

    isGlobalScope = false;

    llvm::BasicBlock* oldInsertPoint = funcBuilder.GetInsertBlock();
    const std::string componentName = component->component_name->expression.TokenLiteral;

    auto ctIt = componentTypes.find(componentName);
    if (ctIt == componentTypes.end())
        throw std::runtime_error("Component type not registered: " + componentName);

    llvm::StructType* structTy = llvm::dyn_cast<llvm::StructType>(ctIt->second);
    if (!structTy) throw std::runtime_error("LLVM type is not a struct for: " + componentName);

    // FUNCTION SETUP
    std::vector<llvm::Type*> llvmParamTypes = {structTy->getPointerTo()};  // self
    for (auto& arg : initStmt->constructor_args)
        llvmParamTypes.push_back(getLLVMType(semantics.inferNodeDataType(arg.get())));

    auto* funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), llvmParamTypes, false);
    auto* initFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                            componentName + "_init", module.get());
    currentFunction = initFunc;

    auto* entryBlock = llvm::BasicBlock::Create(context, "entry", initFunc);
    funcBuilder.SetInsertPoint(entryBlock);

    auto argIt = initFunc->arg_begin();
    llvm::Argument* selfArg = &*argIt++;
    selfArg->setName(componentName + ".self_arg");

    llvm::Type* selfPtrType = structTy->getPointerTo();
    llvm::AllocaInst* selfAlloca =
        funcBuilder.CreateAlloca(selfPtrType, nullptr, componentName + ".self_ptr_addr");

    funcBuilder.CreateStore(selfArg, selfAlloca);

    // Ensure Alloca is at the beginning
    if (selfAlloca->getParent() != entryBlock ||
        selfAlloca->getNextNonDebugInstruction() != nullptr)
        ;

    // METADATA REGISTRATION
    auto sym = semantics.getSymbolFromMeta(component);
    if (!sym) reportDevBug("Failed to get component symbol info", component);

    // Store the ALLOCA address as the canonical self pointer for this function
    llvm::Value* prevInstance = sym->codegen().llvmValue;
    sym->codegen().llvmValue = selfAlloca;  // Use selfAlloca (the ptr* to the self ptr)

    currentComponent = component;
    currentComponentInstance = selfAlloca;  // Use selfAlloca
    currentFunctionSelfMap[currentFunction] = selfAlloca;

    // CONSTRUCTOR ARGUMENTS (Store to Alloca)
    for (auto& arg : initStmt->constructor_args) {
        llvm::Value* argVal = &*argIt++;
        auto argName = arg->statement.TokenLiteral;

        // Ensure argVal is not a temporary value being reused in a dangerous way
        if (!llvm::isa<llvm::Argument>(argVal))
            std::cerr << "[IR WARN] Constructor arg '" << argName
                      << "' is not a clean Argument!.\n";  // GUARD

        llvm::AllocaInst* alloca = funcBuilder.CreateAlloca(argVal->getType(), nullptr, argName);
        funcBuilder.CreateStore(argVal, alloca);

        auto argSym = semantics.getSymbolFromMeta(arg.get());
        if (!argSym) reportDevBug("Missing ctor argument symbol info", arg.get());

        argSym->codegen().llvmValue = alloca;
    }

    // Body Gen
    if (initStmt->block) generateBlockStatement(initStmt->block.get());

    llvm::BasicBlock* currentBlock = funcBuilder.GetInsertBlock();

    if (!currentBlock->getTerminator()) {
        llvm::IRBuilder<> terminatorBuilder(currentBlock);
        terminatorBuilder.CreateRetVoid();
    }

    // Cleanup
    sym->codegen().llvmValue = prevInstance;
    currentComponentInstance = nullptr;
    currentComponent = nullptr;
    isGlobalScope = true;
    currentFunction = nullptr;
    if (oldInsertPoint) funcBuilder.SetInsertPoint(oldInsertPoint);

    logInternal("Finished generating '" + componentName + "_init'");
}

void IRGenerator::generateComponentFunctionStatement(Node* node, const std::string& compName) {
    auto* fnExpr = dynamic_cast<FunctionExpression*>(node);
    if (!fnExpr) return;

    llvm::BasicBlock* oldInsertPoint = funcBuilder.GetInsertBlock();
    std::string funcName;

    if (auto* expr = dynamic_cast<FunctionExpression*>(fnExpr)) {
        isGlobalScope = false;
        auto funcSym = semantics.getSymbolFromMeta(expr);
        if (!funcSym) reportDevBug("Failed to find function symbol info", expr);

        funcName = compName + "_" + expr->func_key.TokenLiteral;

        llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
        if (funcSym->isExportable) linkage = llvm::Function::ExternalLinkage;

        // Collect parameter types (first is %this)
        llvm::Type* thisPtrType = llvmCustomTypes[compName]->getPointerTo();
        std::vector<llvm::Type*> paramTypes = {thisPtrType};

        for (auto& p : expr->call) {
            auto it = semantics.metaData.find(p.get());
            if (it == semantics.metaData.end())
                throw std::runtime_error("Missing parameter metadata for: " +
                                         p->statement.TokenLiteral);
            paramTypes.push_back(getLLVMType(it->second->type().type));
        }

        // Return type
        ResolvedType retType = semantics.inferNodeDataType(expr->return_type.get());
        llvm::FunctionType* fnType =
            llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

        // Create or fetch function
        llvm::Function* fn = module->getFunction(funcName);
        if (!fn) fn = llvm::Function::Create(fnType, linkage, funcName, module.get());

        currentFunction = fn;
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", fn);
        funcBuilder.SetInsertPoint(entry);

        // Map %this (first argument)
        llvm::Argument& thisArg = *fn->arg_begin();
        thisArg.setName(compName + ".self");
        llvm::AllocaInst* selfAlloca =
            funcBuilder.CreateAlloca(thisPtrType, nullptr, compName + ".self_ptr");
        funcBuilder.CreateStore(&thisArg, selfAlloca);
        funcSym->codegen().llvmValue = selfAlloca;
        currentFunctionSelfMap[currentFunction] = selfAlloca;

        // Map user parameters
        auto argIter = std::next(fn->arg_begin());  // skip %this
        for (auto& p : expr->call) {
            auto paramNode = p.get();
            auto paramSym = semantics.getSymbolFromMeta(paramNode);
            if (!paramSym) reportDevBug("Failed to find param symbol info", paramNode);

            llvm::Type* paramTy = getLLVMType(paramSym->type().type);
            llvm::AllocaInst* alloca =
                funcBuilder.CreateAlloca(paramTy, nullptr, p->statement.TokenLiteral);
            funcBuilder.CreateStore(&(*argIter), alloca);
            paramSym->codegen().llvmValue = alloca;

            ++argIter;
        }

        // Generate function body
        generateExpression(expr->block.get());

        llvm::BasicBlock* finalBlock = funcBuilder.GetInsertBlock();

        // If the function is void
        bool isVoidFunction = funcSym->func().returnType.kind == DataType::VOID;

        if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator())) {
            if (isVoidFunction) {
                // Inject 'ret void' for void functions that fell off the end.
                funcBuilder.CreateRetVoid();
                std::cout << "INJECTED missing 'ret void' terminator.\n";
            } else {
                // Non-void function finished without a return.
                // This is a semantic failure, but we terminate for LLVM stability.
                funcBuilder.CreateUnreachable();
            }
        }
        isGlobalScope = true;
        funcBuilder.ClearInsertionPoint();

        currentFunction = nullptr;
    }

    // FunctionDeclarationExpression: declaration only error out incase somehow
    // the semantics allowed them through
    else if (dynamic_cast<FunctionDeclarationExpression*>(fnExpr)) {
        throw std::runtime_error("Function declarations are prohibited inside components");
    }

    if (oldInsertPoint) funcBuilder.SetInsertPoint(oldInsertPoint);
}

llvm::Value* IRGenerator::generateMethodCallExpression(Node* node) {
    auto metCall = dynamic_cast<MethodCallExpression*>(node);
    if (!metCall) reportDevBug("Invalid method call expression node", node);

    if (isGlobalScope) reportDevBug("Cannot use calls in global scope", metCall);

    auto metSym = semantics.getSymbolFromMeta(metCall);

    // Check if there are semantic errors
    if (!metSym) reportDevBug("Semantic error detected on component method call", metCall);

    // Something like p1.test()
    // Split the call
    auto instance = dynamic_cast<Identifier*>(metCall->instance.get());
    std::string instanceName = instance->identifier.TokenLiteral;
    llvm::Value* result;
    auto sealIt = semantics.sealTable.find(instanceName);

    // Getting the raw function name as stored in the node
    auto call = dynamic_cast<CallExpression*>(metCall->call.get());
    std::string callName = call->function_identifier->expression.TokenLiteral;

    if (sealIt != semantics.sealTable.end()) {
        // Retrive the function we wish to call
        auto sealFnMap = sealIt->second;
        auto sealFnIt = sealFnMap.find(callName);
        if (sealFnIt == sealFnMap.end()) {
            reportDevBug(
                "Function '" + callName + "' does not exist in seal '" + instanceName + "'",
                metCall);
        }

        call->function_identifier->expression.TokenLiteral = instanceName + "_" + callName;
        call->function_identifier->token.TokenLiteral = instanceName + "_" + callName;

        result = generateCallExpression(call);
    } else {
        // Getting the metaData for the instance
        auto instanceIt = semantics.metaData.find(instance);
        if (instanceIt == semantics.metaData.end()) {
            throw std::runtime_error("Could not find instance metaData");
        }

        auto instanceSym = instanceIt->second;

        llvm::Value* objectPtr = nullptr;
        // Get the llvm value for the instance
        if (instanceSym->codegen().llvmValue) {
            objectPtr = instanceSym->codegen().llvmValue;
        } else {
            objectPtr = generateExpression(instance);
        }

        // Get the component type of the object
        std::string compName = instanceSym->type().type.resolvedName;

        // Resolve the function name
        std::string funcName = compName + "_" + callName;

        // Fetch the function from the module
        llvm::Function* targetFunc = module->getFunction(funcName);
        if (!targetFunc) {
            throw std::runtime_error("Undefined method '" + callName + "' from component '" +
                                     compName + "'");
        }

        // Prepare the arguments
        std::vector<llvm::Value*> args;
        args.push_back(objectPtr);  // The implicit self

        // Offset of 1 is passed inorder to ignore the self paremeter getting
        // injected
        std::vector<llvm::Value*> userArgs = prepareArguments(targetFunc, call->parameters, 1);

        args.insert(args.end(), userArgs.begin(), userArgs.end());

        // Call the function
        result = funcBuilder.CreateCall(targetFunc, args);
    }
    return result;
}

void IRGenerator::generateComponentStatement(Node* node) {
    auto* compStmt = dynamic_cast<ComponentStatement*>(node);
    if (!compStmt) return;

    auto sym = semantics.getSymbolFromMeta(compStmt);
    if (!sym) reportDevBug("Failed to find component symbol info", compStmt);

    const std::string compName = compStmt->component_name->expression.TokenLiteral;
    currentComponent = compStmt;

    llvm::StructType* structTy = llvmCustomTypes[compName];
    if (!structTy) {
        // Fallback for safety, though declareCustomTypes should have handled this
        structTy = llvm::StructType::create(context, compName);
        llvmCustomTypes[compName] = structTy;
    }

    componentTypes[compName] = structTy;
    sym->codegen().llvmType = structTy;

    std::vector<llvm::Type*> memberTypes;
    std::vector<FunctionExpression*> functionExpressions;
    std::unordered_map<std::string, unsigned> memberIndexMap;

    size_t dataMemberCount = 0;
    for (auto const& [name, info] : sym->members) {
        if (!info->isFunction) {
            dataMemberCount++;
        }
    }
    memberTypes.resize(dataMemberCount);

    for (const auto& [memberName, info] : sym->members) {
        if (!info->node) continue;

        if (auto* funcExpr = dynamic_cast<FunctionExpression*>(info->node)) {
            functionExpressions.push_back(funcExpr);
            continue;
        }

        // Handle data member
        llvm::Type* memberTy = getLLVMType(info->type);
        if (!memberTy)
            throw std::runtime_error("Unknown LLVM type for member '" + memberName + "'");

        // Use the semantic index to place it correctly in the struct
        memberTypes[info->memberIndex] = memberTy;
    }

    if (structTy->isOpaque()) {
        structTy->setBody(memberTypes);
        std::cout << "[IRGEN] Finalized body for component: " << compName << "\n";
    }

    for (const auto& func : functionExpressions) {
        // Generate functions now since the body is done being set
        generateComponentFunctionStatement(func, compName);
    }
    // INIT HANDLING
    if (compStmt->initConstructor)
        generateInitFunction(compStmt->initConstructor.value().get(), compStmt);

    currentComponent = nullptr;
}

void IRGenerator::generateEnumStatement(Node* node) {
    auto enumStmt = dynamic_cast<EnumStatement*>(node);
    if (!enumStmt) return;

    // Getting the enum symbolInfo
    auto enumInfo = semantics.getSymbolFromMeta(enumStmt);
    if (!enumInfo) reportDevBug("Fail to find enum symbol info", enumStmt);

    auto enumTypeInfo = semantics.customTypesTable[enumInfo->type().type.resolvedName];

    // Creating a struct for book keeping
    llvm::StructType* enumStruct =
        llvm::StructType::create(context, enumInfo->type().type.resolvedName);
    llvmCustomTypes[enumInfo->type().type.resolvedName] = enumStruct;
}

void IRGenerator::generateInstantiateStatement(Node* node) {
    auto instStmt = dynamic_cast<InstantiateStatement*>(node);

    if (!instStmt) reportDevBug("Invalid instantiation statement", node);

    auto sym = semantics.getSymbolFromMeta(instStmt);
    if (!sym) reportDevBug("Failed to find instantion symbol info", instStmt);

    const auto& instTable = sym->generic().instTable;

    if (instTable.has_value()) {
        auto block = dynamic_cast<BlockStatement*>(instTable->instantiatedAST.get());
        for (const auto& stmt : block->statements) {
            generateStatement(stmt.get());
        }
    }
}

void IRGenerator::generateSealStatement(Node* node) {
    auto sealStmt = dynamic_cast<SealStatement*>(node);
    if (!sealStmt) return;

    auto sealSym = semantics.getSymbolFromMeta(sealStmt);
    if (!sealSym) reportDevBug("Failed to find seal symbol info", sealStmt);

    auto block = dynamic_cast<BlockStatement*>(sealStmt->block.get());
    // Call the generator on the functions themselves
    for (const auto& stmt : block->statements) {
        std::cout << "Looping through seal stmts :" + stmt->toString() << "\n";
        generateStatement(stmt.get());
    }
}
