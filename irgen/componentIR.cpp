#include "irgen.hpp"

void IRGenerator::generateDataStatement(Node *node)
{
    auto dataStmt = dynamic_cast<DataStatement *>(node);
    if (!dataStmt)
    {
        std::cout << "Failed on the data statement\n";
        return;
    }

    std::cout << "Generating IR for data statement: " << node->toString() << "\n";

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

    llvm::StructType *structTy = llvm::StructType::create(context, memberTypes,blockName);
    it->second->llvmType = structTy;
}