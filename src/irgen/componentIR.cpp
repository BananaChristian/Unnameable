#include "irgen.hpp"

void IRGenerator::generateDataStatement(Node *node)
{
    auto dataStmt = dynamic_cast<DataStatement *>(node);
    if (!dataStmt)
        return;

    auto it = semantics.metaData.find(dataStmt);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Missing data block metaData");

    auto &meta = *it->second;
    std::string blockName = meta.type.resolvedName;

    // --- Build LLVM struct type  ---
    std::vector<llvm::Type *> memberTypes;
    for (auto &pair : meta.members)
    {
        std::shared_ptr<MemberInfo> info = pair.second;
        llvm::Type *ty = info->isHeap ? getLLVMType(info->type)->getPointerTo() : getLLVMType(info->type);
        if (!ty)
            throw std::runtime_error("Unknown member type for " + pair.first);
        memberTypes.push_back(ty);
    }

    llvm::StructType *structTy = llvm::StructType::create(context, memberTypes, blockName);
    meta.llvmType = structTy;
    llvmCustomTypes[blockName] = structTy;

    // Global type placeholder (zeroinit)
    llvm::Constant *initStruct = llvm::Constant::getNullValue(structTy);
    llvm::GlobalVariable *gv = new llvm::GlobalVariable(
        *module,
        structTy,
        false,
        llvm::GlobalValue::ExternalLinkage,
        initStruct,
        blockName);
    llvmGlobalDataBlocks[blockName] = gv;
    meta.llvmValue = gv;
}

llvm::Value *IRGenerator::generateInstanceExpression(Node *node)
{
    auto instExpr = dynamic_cast<InstanceExpression *>(node);
    if (!instExpr)
        return nullptr;

    std::string instName = instExpr->blockIdent->expression.TokenLiteral;
    llvm::StructType *structTy = llvmCustomTypes[instName];
    if (!structTy)
        throw std::runtime_error("Unknown struct type: " + instName);

    if (!funcBuilder.GetInsertBlock())
        throw std::runtime_error("Cannot create an instance for '" + instName + "' in the global scope");

    // 1) single stack slot to build the struct in
    llvm::Value *instancePtr = funcBuilder.CreateAlloca(structTy, nullptr, instName + "_inst");

    // 2) default-initialize each member directly into instancePtr (use source member order!)
    auto typeInfoIt = semantics.customTypesTable.find(instName);
    if (typeInfoIt == semantics.customTypesTable.end())
        throw std::runtime_error("Missing custom type info for " + instName);
    auto &typeInfo = typeInfoIt->second;

    for (auto &kv : typeInfo.members)
    {
        const std::string &memberName = kv.first;
        auto &info = kv.second;
        unsigned idx = info->memberIndex; // must be set during semantic phase

        llvm::Value *memberPtr = funcBuilder.CreateStructGEP(structTy, instancePtr, idx, memberName);
        if (info->isHeap)
        {
            llvm::PointerType *pTy = getLLVMType(info->type)->getPointerTo();
            funcBuilder.CreateStore(llvm::ConstantPointerNull::get(pTy), memberPtr);
        }
        else
        {
            llvm::Type *elemTy = getLLVMType(info->type);
            funcBuilder.CreateStore(llvm::Constant::getNullValue(elemTy), memberPtr);
        }
    }

    // 3) apply explicit initializers from instance expression
    for (auto &fieldStmt : instExpr->fields)
    {
        auto assign = dynamic_cast<AssignmentStatement *>(fieldStmt.get());
        if (!assign)
            continue;
        std::string fieldName = assign->identifier->expression.TokenLiteral;
        auto memIt = typeInfo.members.find(fieldName);
        if (memIt == typeInfo.members.end())
            throw std::runtime_error("No such field in " + instName + ": " + fieldName);
        unsigned idx = memIt->second->memberIndex;
        llvm::Value *fieldPtr = funcBuilder.CreateStructGEP(structTy, instancePtr, idx, fieldName);

        // IMPORTANT: generate *value* for RHS. It must produce a type matching element type.
        llvm::Value *rhsVal = generateExpression(assign->value.get());
        if (!rhsVal)
            throw std::runtime_error("Failed to generate RHS for field " + fieldName);

        // If needed, bitcast or convert (ensure rhsVal type == element type)
        funcBuilder.CreateStore(rhsVal, fieldPtr);
    }

    // 4) load struct value and return (value semantics)
    llvm::Value *loaded = funcBuilder.CreateLoad(structTy, instancePtr, instName + "_val");
    return loaded;
}

void IRGenerator::generateInitFunction(Node *node, ComponentStatement *component)
{
    auto *initStmt = dynamic_cast<InitStatement *>(node);
    if (!initStmt || !component)
        return;

    llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();
    const std::string componentName = component->component_name->expression.TokenLiteral;

    auto ctIt = componentTypes.find(componentName);
    if (ctIt == componentTypes.end())
        throw std::runtime_error("Component type not registered: " + componentName);

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(ctIt->second);
    if (!structTy)
        throw std::runtime_error("LLVM type is not a struct for: " + componentName);

    // Build parameter types
    std::vector<llvm::Type *> llvmParamTypes = {structTy->getPointerTo()}; // self
    for (auto &arg : initStmt->constructor_args)
        llvmParamTypes.push_back(getLLVMType(semantics.inferNodeDataType(arg.get())));

    auto *funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), llvmParamTypes, false);
    auto *initFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                            componentName + "_init", module.get());

    auto *entryBlock = llvm::BasicBlock::Create(context, "entry", initFunc);
    funcBuilder.SetInsertPoint(entryBlock);

    // Self pointer setup
    auto argIt = initFunc->arg_begin();
    llvm::Value *selfPtr = &*argIt++;
    selfPtr->setName("self");

    // Register self in metadata
    auto compIt = semantics.metaData.find(component);
    if (compIt == semantics.metaData.end())
        throw std::runtime_error("Missing component metaData for: " + componentName);

    llvm::Value *prevInstance = compIt->second->llvmValue;
    compIt->second->llvmValue = selfPtr;

    currentComponent = component;
    currentComponentInstance = selfPtr;

    // Store constructor args
    for (auto &arg : initStmt->constructor_args)
    {
        llvm::Value *argVal = &*argIt++;
        auto argName = arg->statement.TokenLiteral;
        llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(argVal->getType(), nullptr, argName);
        funcBuilder.CreateStore(argVal, alloca);

        auto metaIt = semantics.metaData.find(arg.get());
        if (metaIt == semantics.metaData.end())
            throw std::runtime_error("Missing metaData for ctor argument: " + argName);

        metaIt->second->llvmValue = alloca;
    }

    // Generate body
    if (initStmt->block)
        generateBlockStatement(initStmt->block.get());

    funcBuilder.CreateRetVoid();

    // Cleanup
    compIt->second->llvmValue = prevInstance;
    currentComponentInstance = nullptr;
    currentComponent = nullptr;
    if (oldInsertPoint)
        funcBuilder.SetInsertPoint(oldInsertPoint);

    std::cout << "[IR INIT] Finished generating " << componentName << "_init\n";
}

void IRGenerator::generateComponentFunctionStatement(Node *node, const std::string &compName)
{
    auto *fnExpr = dynamic_cast<FunctionExpression *>(node);
    if (!fnExpr)
        return;

    llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();
    std::string funcName;

    // FunctionExpression: definition with body
    if (auto *expr = dynamic_cast<FunctionExpression *>(fnExpr))
    {
        auto exprIt = semantics.metaData.find(expr);
        if (exprIt == semantics.metaData.end())
            throw std::runtime_error("Missing metadata for component function");
        if (exprIt->second->hasError)
            throw std::runtime_error("Semantic error in function: " + expr->func_key.TokenLiteral);

        funcName = compName + "_" + expr->func_key.TokenLiteral;

        // Collect parameter types (first is %this)
        llvm::Type *thisPtrType = llvmCustomTypes[compName]->getPointerTo();
        std::vector<llvm::Type *> paramTypes = {thisPtrType};

        for (auto &p : expr->call)
        {
            auto it = semantics.metaData.find(p.get());
            if (it == semantics.metaData.end())
                throw std::runtime_error("Missing parameter metadata for: " + p->statement.TokenLiteral);
            paramTypes.push_back(getLLVMType(it->second->type));
        }

        // Return type
        ResolvedType retType = semantics.inferNodeDataType(expr->return_type.get());
        llvm::FunctionType *fnType =
            llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

        // Create or fetch function
        llvm::Function *fn = module->getFunction(funcName);
        if (!fn)
            fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, funcName, module.get());

        currentFunction = fn;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
        funcBuilder.SetInsertPoint(entry);

        // Map %this (first argument)
        llvm::Argument &thisArg = *fn->arg_begin();
        thisArg.setName("self");
        exprIt->second->llvmValue = &thisArg;

        // Map user parameters
        auto argIter = std::next(fn->arg_begin()); // skip %this
        for (auto &p : expr->call)
        {
            auto paramNode = p.get();
            auto pIt = semantics.metaData.find(paramNode);
            if (pIt == semantics.metaData.end())
                throw std::runtime_error("Missing parameter metaData");

            llvm::Type *paramTy = getLLVMType(pIt->second->type);
            llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(paramTy, nullptr, p->statement.TokenLiteral);
            funcBuilder.CreateStore(&(*argIter), alloca);
            pIt->second->llvmValue = alloca;

            ++argIter;
        }

        // Generate function body
        generateExpression(expr->block.get());
    }

    // FunctionDeclarationExpression: declaration only error out incase somehow the semantics allowed them through
    else if (auto *declExpr = dynamic_cast<FunctionDeclarationExpression *>(fnExpr))
    {
        throw std::runtime_error("Function declarations are prohibited inside components");
    }

    if (oldInsertPoint)
        funcBuilder.SetInsertPoint(oldInsertPoint);
}

void IRGenerator::generateComponentStatement(Node *node)
{
    auto *compStmt = dynamic_cast<ComponentStatement *>(node);
    if (!compStmt)
        return;

    std::cout << "Generating IR for component: " << compStmt->component_name->expression.TokenLiteral << "\n";

    auto it = semantics.metaData.find(compStmt);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Missing component metaData for " + compStmt->component_name->expression.TokenLiteral);

    auto sym = it->second;
    const std::string compName = compStmt->component_name->expression.TokenLiteral;
    currentComponent = compStmt;

    // STRUCT CREATION
    llvm::StructType *structTy = llvm::StructType::create(context, compName);
    componentTypes[compName] = structTy;
    llvmCustomTypes[compName] = structTy;
    sym->llvmType = structTy;

    std::vector<llvm::Type *> memberTypes;
    std::unordered_map<std::string, unsigned> memberIndexMap;

    unsigned index = 0;
    for (const auto &[memberName, info] : sym->members)
    {
        if (!info->node)
            continue;

        if (auto *funcExpr = dynamic_cast<FunctionExpression *>(info->node))
        {
            // Inject `%this` param if function belongs to component
            generateComponentFunctionStatement(funcExpr, compName);
            continue;
        }

        // Handle normal data member
        llvm::Type *memberTy = getLLVMType(info->type);
        if (!memberTy)
            throw std::runtime_error("Unknown LLVM type for member '" + memberName + "' in component " + compName);

        memberTypes.push_back(memberTy);
        memberIndexMap[memberName] = index++;
    }

    // Finalize struct definition
    structTy->setBody(memberTypes);

    // INIT HANDLING
    if (compStmt->initConstructor)
        generateInitFunction(compStmt->initConstructor.value().get(), compStmt);

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
        throw std::runtime_error("Invalid new component expression");

    const std::string &compName = newExpr->component_name.TokenLiteral;

    auto exprMetaIt = semantics.metaData.find(newExpr);
    if (exprMetaIt == semantics.metaData.end())
        throw std::runtime_error("Undefined component '" + compName + "'");
    if (exprMetaIt->second->hasError)
        throw std::runtime_error("Semantic Error detected");

    // Retrieve component struct type
    auto compTypeIt = componentTypes.find(compName);
    if (compTypeIt == componentTypes.end())
        throw std::runtime_error("Component '" + compName + "' does not exist");

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(compTypeIt->second);
    if (!structTy)
        throw std::runtime_error("Component type '" + compName + "' is not a struct");

    // Allocate on stack (for now)
    llvm::Value *instancePtr = funcBuilder.CreateAlloca(structTy, nullptr, compName + ".inst");

    // Zero initialize (optional safety)
    funcBuilder.CreateStore(llvm::Constant::getNullValue(structTy), instancePtr);

    // Check for init
    if (llvm::Function *initFn = module->getFunction(compName + "_init"))
    {
        std::vector<llvm::Value *> initArgs;
        initArgs.push_back(instancePtr); // self pointer

        for (auto &arg : newExpr->arguments)
        {
            llvm::Value *val = generateExpression(arg.get());
            if (!val)
                throw std::runtime_error("Failed to generate argument for new " + compName);
            initArgs.push_back(val);
        }

        funcBuilder.CreateCall(initFn, initArgs);
    }

    // Return the pointer
    return instancePtr;
}
