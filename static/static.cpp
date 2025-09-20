#include "llvm/IR/Module.h"
#include "llvm/IR/DataLayout.h"
#include "static.hpp"

Static::Static(Semantics &sem, IRGenerator &ir) : semantics(sem), irgen(ir)
{
    registerAnalyzerFunctions();
}

void Static::analyze(Node *node)
{
    auto it = analyzerFuncsMap.find(typeid(*node));
    if (it == analyzerFuncsMap.end())
    {
        std::cout << "[STATIC ERROR] Failed to find static analyzer for " << node->toString() << "\n";
    }

    (this->*it->second)(node);
}

// Registration functions
void Static::registerAnalyzerFunctions()
{
    analyzerFuncsMap[typeid(IntegerLiteral)] = &Static::analyzeLetStatement;
    analyzerFuncsMap[typeid(Identifier)] = &Static::analyzeIdentifier;
    analyzerFuncsMap[typeid(AssignmentStatement)] = &Static::analyzeAssignmentStatement;
    analyzerFuncsMap[typeid(LetStatement)] = &Static::analyzeLetStatement;
    analyzerFuncsMap[typeid(FunctionStatement)] = &Static::analyzeFunctionStatement;
    analyzerFuncsMap[typeid(FunctionExpression)] = &Static::analyzeFunctionExpression;
    analyzerFuncsMap[typeid(FunctionDeclarationExpression)] = &Static::analyzeFunctionDeclarationExpression;
    analyzerFuncsMap[typeid(DataStatement)] = &Static::analyzeDataStatement;
    analyzerFuncsMap[typeid(BehaviorStatement)] = &Static::analyzeBehaviorStatement;
    analyzerFuncsMap[typeid(ComponentStatement)] = &Static::analyzeComponentStatement;
}

// Identifier expression analyzer
void Static::analyzeIdentifier(Node *node)
{
    auto identExpr = dynamic_cast<Identifier *>(node);
    if (!identExpr)
        return;

    // Getting the alloca value from the metaData
    auto identIt = semantics.metaData.find(identExpr);
    if (identIt == semantics.metaData.end())
    {
        std::cout << "Missing identifier metadata\n";
        return;
    }

    auto identVal = identIt->second->llvmValue;

    llvm::Type *identType = identVal->getType();

    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();

    uint64_t size = DL.getTypeAllocSize(identType);
    total_size += size;
}

// Assignment statement
void Static::analyzeAssignmentStatement(Node *node)
{
    auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    // Getting the value from metaData
    auto assignIt = semantics.metaData.find(assignStmt);
    if (assignIt == semantics.metaData.end())
    {
        std::cout << "Failed to find metaData for assign statement\n";
        return;
    }

    auto assignVal = assignIt->second->llvmValue;

    llvm::Type *assignType = assignVal->getType();

    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();

    uint64_t size = DL.getTypeAllocSize(assignType);

    total_size += size;
}

// Let statement analyzer
void Static::analyzeLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
        return;

    // Get the alloca value
    auto letIt = semantics.metaData.find(letStmt);
    if (letIt == semantics.metaData.end())
    {
        std::cout << "Could not find let statement meta data\n";
        return;
    }

    // Retrieve the llvm value from the meta data
    auto letVal = letIt->second->llvmValue;

    // Get the corresponding llvm type of this value
    llvm::Type *letType = letVal->getType();

    // Get the corresponding size LLVM would assign to it
    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();

    uint64_t size = DL.getTypeAllocSize(letType);

    total_size += size;
}

void Static::analyzeDataStatement(Node *node)
{
    auto dataStmt = dynamic_cast<DataStatement *>(node);

    if (!dataStmt)
        return;

    auto dataIt = semantics.metaData.find(dataStmt);
    if (dataIt == semantics.metaData.end())
    {
        std::cout << "Missing data block metaData\n";
        return;
    }

    auto structTy = dataIt->second->llvmType;
    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();
    uint64_t size = DL.getTypeAllocSize(structTy);
    dataIt->second->componentSize = size;
    total_size += size;
}

void Static::analyzeFunctionStatement(Node *node)
{
    auto funcStmt = dynamic_cast<FunctionStatement *>(node);
    if (!funcStmt)
        return;

    // Since this also just nests function expressions or declaration expression
    auto funcExpr = funcStmt->funcExpr.get();
    if (auto fnExpr = dynamic_cast<FunctionExpression *>(funcExpr))
    {
        analyzeFunctionExpression(fnExpr);
    }
    if (auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(funcExpr))
    {
        analyzeFunctionDeclarationExpression(fnDeclrExpr);
    }
}

void Static::analyzeFunctionExpression(Node *node)
{
    auto funcExpr = dynamic_cast<FunctionExpression *>(node);
    if (!funcExpr)
        return;

    const std::string &fnName = funcExpr->func_key.TokenLiteral;
    llvm::Function *func = irgen.getLLVMModule().getFunction(fnName);

    llvm::Type *ptrType = func->getType()->getPointerTo();

    if (!func)
    {
        std::cout << "LLVM could not resolve function '" + fnName + "'\n";
        return;
    }

    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();
    uint64_t size = DL.getTypeAllocSize(ptrType);
    total_size += size;
}

void Static::analyzeFunctionDeclarationExpression(Node *node)
{
    auto funcDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(node);
    if (!funcDeclrExpr)
        return;

    // This is just a nester for the function declaration statement
    auto funcDeclr = dynamic_cast<FunctionDeclaration *>(funcDeclrExpr->funcDeclrStmt.get());
    if (funcDeclr)
    {
        const std::string &fnName = funcDeclr->function_name->expression.TokenLiteral;
        llvm::Function *func = irgen.getLLVMModule().getFunction(fnName);
        llvm::Type *ptrType = func->getType()->getPointerTo();

        const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();
        uint64_t size = DL.getTypeAllocSize(ptrType);
        total_size += size;
    }
}

void Static::analyzeBehaviorStatement(Node *node)
{
    auto behaviorStmt = dynamic_cast<BehaviorStatement *>(node);
    if (!behaviorStmt)
        return;

    auto behaviorIt = semantics.metaData.find(behaviorStmt);
    if (behaviorIt == semantics.metaData.end())
    {
        std::cout << "Missing behavior block metaData";
        return;
    }

    auto structTy = behaviorIt->second->llvmType;
    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();
    uint64_t size = DL.getTypeAllocSize(structTy);
    behaviorIt->second->componentSize = size;
    total_size += size;
}

void Static::analyzeComponentStatement(Node *node)
{
    auto componentStmt = dynamic_cast<ComponentStatement *>(node);
    if (!componentStmt)
        return;

    auto it = semantics.metaData.find(componentStmt);
    if (it == semantics.metaData.end())
    {
        std::cout << "Missing component metaData";
        return;
    }

    auto structTy = it->second->llvmType;
    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();
    uint64_t size = DL.getTypeAllocSize(structTy);
    it->second->componentSize = size;
    total_size += size;
}

void Static::dumpTotal()
{
    std::cout << "Total size: " << static_cast<unsigned long long>(total_size) << " bytes \n";
}
