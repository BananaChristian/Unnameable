#include "irgen.hpp"

llvm::Value *IRGenerator::getOrCreateGlobalDataBlock(DataStatement *dataStmt)
{
    auto it = semantics.metaData.find(dataStmt);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Missing data block metaData");

    // Already created
    if (it->second->llvmValue)
        return it->second->llvmValue;

    auto structTy = llvmCustomTypes[it->second->type.resolvedName];
    // Create a global alloca (or global variable if truly global)
    llvm::GlobalVariable *gv = new llvm::GlobalVariable(
        *module,
        structTy,
        false, // isConstant
        llvm::GlobalValue::ExternalLinkage,
        llvm::Constant::getNullValue(structTy),
        it->second->type.resolvedName);

    it->second->llvmValue = gv;
    return gv;
}

void IRGenerator::generateDataStatement(Node *node)
{
    auto dataStmt = dynamic_cast<DataStatement *>(node);
    if (!dataStmt)
    {
        std::cout << "Failed on the data statement\n";
        return;
    }

    auto it = semantics.metaData.find(dataStmt);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Missing data block metaData");
    }

    auto blockName = it->second->type.resolvedName;

    std::vector<llvm::Type *> memberTypes;
    std::vector<std::string> memberNames;

    for (const auto &member : it->second->members)
    {
        auto &typeKind = member.second->type;
        if (member.second->isHeap)
        {
            // Instead of raw type, store as a pointer
            llvm::Type *heapPtrTy = getLLVMType(typeKind)->getPointerTo();
            memberTypes.push_back(heapPtrTy);
        }
        else
        {
            llvm::Type *type = getLLVMType(typeKind);
            memberTypes.push_back(type);
        }
        memberNames.push_back(member.first);
    }

    llvm::StructType *structTy = llvm::StructType::create(context, memberTypes, blockName);
    it->second->llvmType = structTy;
    llvmCustomTypes[blockName] = structTy;

    if (llvmGlobalDataBlocks.find(blockName) == llvmGlobalDataBlocks.end())
    {
        llvm::AllocaInst *globalAlloc = builder.CreateAlloca(structTy, nullptr, blockName + "_global");
        llvmGlobalDataBlocks[blockName] = globalAlloc;
        it->second->llvmValue = globalAlloc;

        int idx = 0;
        for (auto &[memberName, info] : it->second->members)
        {
            // skip functions
            if (info->node && dynamic_cast<FunctionStatement *>(info->node))
            {
                idx++;
                continue;
            }

            llvm::Value *memberPtr = builder.CreateStructGEP(structTy, globalAlloc, idx, memberName);
            if (!memberPtr)
                throw std::runtime_error("Failed to compute member GEP for init: " + memberName);

            llvm::Type *elemTy = getLLVMType(info->type);
            if (!elemTy)
                throw std::runtime_error("Failed to get element type for init: " + memberName);

            if (info->isHeap)
            {
                llvm::PointerType *elemPtrTy = elemTy->getPointerTo();
                llvm::Constant *nullPtr = llvm::ConstantPointerNull::get(elemPtrTy);
                builder.CreateStore(nullPtr, memberPtr);
            }
            else
            {
                if (elemTy->isPointerTy() || elemTy->isIntegerTy() || elemTy->isFloatingPointTy())
                {
                    builder.CreateStore(llvm::Constant::getNullValue(elemTy), memberPtr);
                }
            }

            info->llvmValue = memberPtr;
            info->llvmType = elemTy;

            idx++;
        }
    }
}

void IRGenerator::generateBehaviorStatement(Node *node)
{
    auto behaviorStmt = dynamic_cast<BehaviorStatement *>(node);
    if (!behaviorStmt)
        return;

    auto it = semantics.metaData.find(behaviorStmt);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Missing behavior block metaData");
    }

    auto &name = it->second->type.resolvedName;

    // Generating IR for each of the member function expressions or declarations
    for (const auto &[funcName, funcInfo] : it->second->members)
    {
        generateFunctionStatement(funcInfo->node);
    }

    std::vector<llvm::Type *> memberTypes;
    for (const auto &[memberName, memberInfo] : it->second->members)
    {
        // Getting the function
        llvm::Function *func = module.get()->getFunction(memberName);
        if (!func)
        {
            throw std::runtime_error("Function not generated: " + memberName);
        }
        // Getting the function type
        llvm::Type *funcType = func->getType()->getPointerTo();
        // Storing the function types
        memberTypes.push_back(funcType);
    }

    // Creating the struct type
    llvm::StructType *structTy = llvm::StructType::create(context, memberTypes, name);
    it->second->llvmType = structTy;
}

void IRGenerator::generateInitFunction(Node *node, ComponentStatement *component)
{
    auto initStmt = dynamic_cast<InitStatement *>(node);
    if (!initStmt || !component)
        return;

    llvm::BasicBlock *oldInsertPoint = builder.GetInsertBlock();

    const std::string componentName = component->component_name->expression.TokenLiteral;

    auto ctIt = componentTypes.find(componentName);
    if (ctIt == componentTypes.end())
        throw std::runtime_error("Component type not registered: " + componentName);

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(ctIt->second);
    if (!structTy)
        throw std::runtime_error("Component llvm type is not a StructType for: " + componentName);

    // Building parameter types: first param is 'self' pointer (pointer to struct)
    std::vector<llvm::Type *> llvmParamTypes;
    llvmParamTypes.push_back(structTy->getPointerTo()); // self

    for (auto &arg : initStmt->constructor_args)
    {
        llvmParamTypes.push_back(getLLVMType(semantics.inferNodeDataType(arg.get())));
    }

    auto funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), llvmParamTypes, false);
    auto initFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                           componentName + "_init", module.get());

    auto entryBlock = llvm::BasicBlock::Create(context, "entry", initFunc);
    builder.SetInsertPoint(entryBlock);

    // Map args: first arg is 'self'
    auto argIt = initFunc->arg_begin();
    llvm::Value *selfArg = &*argIt++;
    selfArg->setName("self");

    // Create a stack slot for self to make it consistent with normal functions
    llvm::Value *selfAlloca = builder.CreateAlloca(structTy->getPointerTo(), nullptr, "self.addr");
    builder.CreateStore(selfArg, selfAlloca);

    // When looking up self in generateSelfExpression, load from here
    llvm::Value *selfPtr = builder.CreateLoad(structTy->getPointerTo(), selfAlloca, "self.ptr");

    // Save into metaData
    auto compIt = semantics.metaData.find(component);
    if (compIt == semantics.metaData.end())
        throw std::runtime_error("Missing component metaData in init generation for: " + componentName);

    llvm::Value *prevInstance = compIt->second->llvmValue;
    compIt->second->llvmValue = selfPtr;

    std::cout << "[IR INIT] meta->llvmValue set to selfArg for component: " << componentName << "\n";

    // A transient IRGenerator field if you use one
    currentComponentInstance = selfArg;
    currentComponent = component;

    // Map remaining constructor args into local variables if you need them
    std::vector<llvm::Value *> constructorArgs;
    for (auto &arg : initStmt->constructor_args)
    {
        llvm::Value *argVal = &*argIt++;
        auto argAlloca = builder.CreateAlloca(argVal->getType(), nullptr, "ctor.arg");
        builder.CreateStore(argVal, argAlloca);
        auto metaIt = semantics.metaData.find(arg.get());
        if (metaIt == semantics.metaData.end())
        {
            throw std::runtime_error("Init arg metaData does not exist");
        }
        metaIt->second->llvmValue = argAlloca;
    }

    // Generate the init body — inside this call, generateSelfExpression will find meta->llvmValue/selfArg
    if (initStmt->block)
    {
        generateBlockStatement(initStmt->block.get());
    }

    builder.CreateRetVoid();

    // Clean up and restoring
    compIt->second->llvmValue = prevInstance;
    currentComponentInstance = nullptr;
    currentComponent = nullptr;

    if (oldInsertPoint)
        builder.SetInsertPoint(oldInsertPoint);

    std::cout << "[IR INIT] Finished init generation for: " << componentName << "\n";
}

void IRGenerator::generateComponentFunctionStatement(Node *node, const std::string &compName)
{
    auto fnStmt = dynamic_cast<FunctionStatement *>(node);
    if (!fnStmt)
        return;

    llvm::BasicBlock *oldInsertPoint = builder.GetInsertBlock();
    auto fnExpr = fnStmt->funcExpr.get();
    std::string funcName;
    if (auto expr = dynamic_cast<FunctionExpression *>(fnExpr))
    {
        auto exprIt = semantics.metaData.find(expr);
        if (exprIt == semantics.metaData.end())
            throw std::runtime_error("Component function metaData not found");

        if (exprIt->second->hasError)
        {
            std::cout << "FUNCTION NODE WITH ERROR:" << expr->toString() << "\n";
            throw std::runtime_error("Component function has semantic errors");
        }
        funcName = expr->func_key.TokenLiteral;

        llvm::Type *thisPtrType = llvmCustomTypes[compName]->getPointerTo();
        std::vector<llvm::Type *> paramTypes;
        paramTypes.push_back(thisPtrType); // hidden 'this' pointer

        for (auto &p : expr->call)
        {
            auto it = semantics.metaData.find(p.get());
            if (it == semantics.metaData.end())
                throw std::runtime_error("Missing parameter metadata");

            paramTypes.push_back(getLLVMType(it->second->type));
        }

        auto fnRetType = semantics.inferNodeDataType(expr->return_type.get());
        llvm::FunctionType *fnType = llvm::FunctionType::get(getLLVMType(fnRetType), paramTypes, false);

        llvm::Function *fn = module->getFunction(funcName);
        if (!fn)
            fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, funcName, module.get());

        currentFunction = fn;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
        builder.SetInsertPoint(entry);

        // Map 'this' pointer correctly (no &!)
        llvm::Argument &thisArg = *fn->arg_begin();
        exprIt->second->llvmValue = &thisArg;

        // Map user arguments
        auto argIter = fn->arg_begin(); // points to %this
        ++argIter;
        for (auto &p : expr->call)
        {
            auto pIt = semantics.metaData.find(p.get());
            if (pIt == semantics.metaData.end())
                throw std::runtime_error("Missing parameter metadata");

            llvm::AllocaInst *alloca = builder.CreateAlloca(getLLVMType(pIt->second->type), nullptr, p->statement.TokenLiteral);
            builder.CreateStore(&(*argIter), alloca); // argument value stored to local alloca
            pIt->second->llvmValue = alloca;

            ++argIter;
        }

        // Generate function body
        generateExpression(expr->block.get());
    }
    else if (auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnExpr))
    {
        auto declrStmt = declrExpr->funcDeclrStmt.get();
        auto fnDecl = dynamic_cast<FunctionDeclaration *>(declrStmt);
        if (!fnDecl)
            return;

        // Getting the function name
        funcName = fnDecl->function_name->expression.TokenLiteral;

        llvm::Type *thisPtrType = llvmCustomTypes[compName]->getPointerTo();
        std::vector<llvm::Type *> paramTypes;
        paramTypes.push_back(thisPtrType); // 'this' pointer is added to to the parameters

        auto declrIt = semantics.metaData.find(fnDecl);
        if (declrIt == semantics.metaData.end())
        {
            throw std::runtime_error("Missing component function declaration meta data");
        }

        for (const auto &param : fnDecl->parameters)
        {
            auto it = semantics.metaData.find(param.get());
            if (it == semantics.metaData.end())
            {
                throw std::runtime_error("Missing function declaration parameter meta data");
            }
            paramTypes.push_back(getLLVMType(it->second->type));
        }

        auto retType = declrIt->second->returnType;
        llvm::FunctionType *fnType = llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

        llvm::Function *declaredFunc = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, funcName, module.get());
    }

    if (oldInsertPoint)
        builder.SetInsertPoint(oldInsertPoint);
}

void IRGenerator::generateComponentStatement(Node *node)
{
    auto compStmt = dynamic_cast<ComponentStatement *>(node);
    if (!compStmt)
        return;

    std::cout << "Generating IR for component statement" << node->toString() << "\n";
    auto it = semantics.metaData.find(compStmt);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Missing component metaData");

    auto sym = it->second;
    const std::string compName = compStmt->component_name->expression.TokenLiteral;
    currentComponent = compStmt;

    // Create empty struct type for data fields only
    llvm::StructType *structTy = llvm::StructType::create(context, compName);
    componentTypes[compName] = structTy;
    llvmCustomTypes[compName] = structTy; // Register type in LLVM custom type map
    it->second->llvmType = structTy;

    // Collect only non-function members for struct
    std::vector<llvm::Type *> memberTypes;
    std::unordered_map<std::string, unsigned> llvmMemberIndices;
    unsigned idx = 0;
    for (const auto &[memberName, info] : it->second->members)
    {
        std::cout << "Dealing with memberTypes\n";
        llvm::Type *memberType = nullptr;

        if (info->node && dynamic_cast<FunctionStatement *>(info->node))
        {
            // Generate method IR separately
            auto funcStmt = static_cast<FunctionStatement *>(info->node);

            // Inject '%this' pointer if function belongs to component
            if (currentComponent)
            {
                llvm::Function *func = module->getFunction(compName + "_" + memberName);
                if (!func)
                {
                    generateComponentFunctionStatement(funcStmt, compName); // pass compName so %this is first param
                    func = module->getFunction(memberName);
                    if (!func)
                        throw std::runtime_error("Failed to generate method: " + memberName);
                }
            }
            continue; // skip adding to struct body
        }

        // Regular data member
        memberType = getLLVMType(info->type);
        if (!memberType)
            throw std::runtime_error("Null member type for '" + memberName + "'");

        memberTypes.push_back(memberType);
        llvmMemberIndices[memberName] = idx++;
    }

    // Finalize struct body
    structTy->setBody(memberTypes);

    // Generate init constructor if it exists
    if (compStmt->initConstructor.has_value())
    {
        generateInitFunction(compStmt->initConstructor.value().get(), compStmt);
    }

    currentComponent = nullptr;
}

void IRGenerator::generateEnumClassStatement(Node *node)
{
    auto enumStmt = dynamic_cast<EnumClassStatement *>(node);
    if (!enumStmt)
        return;

    // Getting the enum statement name
    const std::string &enumName = enumStmt->enum_identifier->expression.TokenLiteral;

    // Retrieve the info from metaData
    auto enumIt = semantics.metaData.find(enumStmt);
    if (enumIt == semantics.metaData.end())
        throw std::runtime_error("No existing metaData for enum class statement");

    // Getting the enum symbolInfo
    auto enumInfo = enumIt->second;
    auto enumTypeInfo = semantics.customTypesTable[enumInfo->type.resolvedName];

    // Creating a struct for book keeping
    llvm::StructType *enumStruct = llvm::StructType::create(context, enumInfo->type.resolvedName);
    llvmCustomTypes[enumInfo->type.resolvedName] = enumStruct;

    std::cout << "[IRGEN LOG] Declared enum class " << enumInfo->type.resolvedName << " with members:\n";
    for (auto &[memberName, member] : enumTypeInfo.members)
    {
        // llvm::ConstantInt will be generated  when a member is actually used in an expression
    }
}

llvm::Value *IRGenerator::generateNewComponentExpression(Node *node)
{
    auto newExpr = dynamic_cast<NewComponentExpression *>(node);
    if (!newExpr)
        throw std::runtime_error("Invalid new component expression ");

    auto compName = newExpr->component_name.TokenLiteral;
    auto line = newExpr->expression.line;
    auto col = newExpr->expression.column;

    auto exprMetaIt = semantics.metaData.find(newExpr);
    if (exprMetaIt == semantics.metaData.end())
        throw std::runtime_error("Undefined component '" + compName + "'");

    // Look up the struct type for this component
    auto compTypeIt = componentTypes.find(compName);
    if (compTypeIt == componentTypes.end())
        throw std::runtime_error("Component '" + compName + "' does not exist");

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(compTypeIt->second);

    // Allocate on the stack
    llvm::Value *instance = builder.CreateAlloca(structTy, nullptr, compName + "_instance");

    // Call the init function if it exists
    llvm::Function *initFn = module->getFunction(compName + "_init");
    if (initFn)
    {
        std::vector<llvm::Value *> initArgs;
        initArgs.push_back(instance); // self ptr

        for (auto &arg : newExpr->arguments)
            initArgs.push_back(generateExpression(arg.get()));

        builder.CreateCall(initFn, initArgs);
    }

    return instance;
}
