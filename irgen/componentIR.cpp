#include "irgen.hpp"

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
        auto &typeKind = member.second.type.kind;
        llvm::Type *type = getLLVMType(typeKind);
        memberTypes.push_back(type);
        memberNames.push_back(member.first);
    }

    llvm::StructType *structTy = llvm::StructType::create(context, memberTypes, blockName);
    it->second->llvmType = structTy;
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
        generateFunctionStatement(funcInfo.node);
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
        llvmParamTypes.push_back(getLLVMType(semantics.inferNodeDataType(arg.get()).kind));
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

    std::cout << "[IR INIT] selfArg: " << selfArg << " (type: ";
    selfArg->getType()->print(llvm::outs());
    std::cout << ")\n";

    // Store the component instance pointer into the semantic meta so generateSelfExpression can find it
    auto metaIt = semantics.metaData.find(component);
    if (metaIt == semantics.metaData.end())
    {
        throw std::runtime_error("Missing component metaData in init generation for: " + componentName);
    }

    // Save previous value to restore later 
    llvm::Value *prevInstance = metaIt->second->llvmValue;
    metaIt->second->llvmValue = selfArg;
    std::cout << "[IR INIT] meta->llvmValue set to selfArg for component: " << componentName << "\n";

    // A transient IRGenerator field if you use one
    currentComponentInstance = selfArg;
    currentComponent = component;

    // Map remaining constructor args into local variables if you need them
    std::vector<llvm::Value *> constructorArgs;
    for (auto &arg : initStmt->constructor_args)
    {
        llvm::Value *argVal = &*argIt++;
        constructorArgs.push_back(argVal);
    }

    // Generate the init body — inside this call, generateSelfExpression will find meta->llvmValue/selfArg
    if (initStmt->block)
    {
        generateBlockStatement(initStmt->block.get());
    }

    builder.CreateRetVoid();

    // Clean up and restoring
    metaIt->second->llvmValue = prevInstance;
    currentComponentInstance = nullptr;
    currentComponent = nullptr;

    std::cout << "[IR INIT] Finished init generation for: " << componentName << "\n";
}

void IRGenerator::generateComponentStatement(Node *node)
{
    auto compStmt = dynamic_cast<ComponentStatement *>(node);
    if (!compStmt)
        return;

    std::cout << "Generating IR for component statement" << node->toString() << "\n";
    auto it = semantics.metaData.find(compStmt);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Missing component metaData");
    }
    auto sym = it->second;
    const std::string compName = compStmt->component_name->expression.TokenLiteral;
    currentComponent = compStmt;

    // Creating an empty struct type and store immediately
    llvm::StructType *structTy = llvm::StructType::create(context, compName);
    componentTypes[compName] = structTy;
    it->second->llvmType = structTy;

    //Collecting member types
    std::vector<llvm::Type *> memberTypes;
    for (const auto &[memberName, info] : it->second->members)
    {
        std::cout << "Dealing with memberTypes\n";
        llvm::Type *memberType = nullptr;

        if (info.node)
        {
            if (auto funcStmt = dynamic_cast<FunctionStatement *>(info.node))
            {
                llvm::Function *func = module->getFunction(memberName);
                if (!func)
                {
                    generateFunctionStatement(funcStmt);
                    func = module->getFunction(memberName);
                    if (!func)
                    {
                        throw std::runtime_error("Failed to generate function: " + memberName);
                    }
                }
                memberType = func->getType()->getPointerTo();
            }
        }
        else
        {
            memberType = getLLVMType(info.type.kind);
        }

        if (!memberType)
            throw std::runtime_error("Null member type for: " + memberName);

        memberTypes.push_back(memberType);
    }

    //Setting the body of the struct
    structTy->setBody(memberTypes);

    //Generate init AFTER type is registered
    if (compStmt->initConstructor.has_value())
    {
        generateInitFunction(compStmt->initConstructor.value().get(), compStmt);
    }

    currentComponent = nullptr;
}
