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