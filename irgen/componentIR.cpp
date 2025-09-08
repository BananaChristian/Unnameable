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

void IRGenerator::generateInitFunction(Node *node)
{
    auto initStmt = dynamic_cast<InitStatement *>(node);
    if (!initStmt)
        return;
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
    const std::string compName = sym->type.resolvedName;

    std::vector<llvm::Type *> memberTypes;
    for (const auto &[memberName, info] : it->second->members)
    {
        std::cout << "Dealing with memberTypes\n";
        llvm::Type *memberType;
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
                memberTypes.push_back(memberType);
            }
        }
        else
        {
            memberType = getLLVMType(info.type.kind);
            memberTypes.push_back(memberType);
        }
    }

    llvm::StructType *structTy = llvm::StructType::create(context, memberTypes, compName);

    it->second->llvmType = structTy;
}
