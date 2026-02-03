#include "irgen.hpp"

void IRGenerator::registerAllocators() {
  for (const auto &allocPair : semantics.allocatorMap) {
    auto handle = allocPair.second;

    // Allocator function
    const std::string &allocName = handle.allocateName;
    auto allocateSym = handle.allocatorSymbol;

    std::vector<llvm::Type *> allocParams;
    for (const auto &param : allocateSym->paramTypes) {
      allocParams.push_back(getLLVMType(param.first));
    }

    llvm::Type *allocateRetType = getLLVMType(allocateSym->returnType);

    llvm::FunctionType *allocfnType =
        llvm::FunctionType::get(allocateRetType, allocParams, false);

    llvm::Function::Create(allocfnType, llvm::Function::ExternalLinkage,
                           allocName, module.get());

    // Free function
    const std::string &freeName = handle.freeName;
    auto freeSym = handle.freeSymbol;

    std::vector<llvm::Type *> freeParams;
    for (const auto &param : freeSym->paramTypes) {
      freeParams.push_back(getLLVMType(param.first));
    }

    llvm::Type *freeRetType = getLLVMType(freeSym->returnType);

    llvm::FunctionType *freefnType =
        llvm::FunctionType::get(freeRetType, freeParams, false);

    llvm::Function::Create(freefnType, llvm::Function::ExternalLinkage,
                           freeName, module.get());
  }
}

void IRGenerator::generateAllocatorInterface(Node *node) {
  auto allocStmt = dynamic_cast<AllocatorStatement *>(node);
  if (!allocStmt)
    return;

  generateStatement(allocStmt->block.get());
}
