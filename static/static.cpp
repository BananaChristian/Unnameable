#include "llvm/IR/Module.h"
#include "llvm/IR/DataLayout.h"
#include "static.hpp"

Static::Static(Semantics &sem, IRGenerator &ir) : semantics(sem), irgen(ir)
{
    registerAnalyzerFunctions();
}

void Static::analyze(Node *node)
{
    auto it = analyzerFuncsMap.find(typeid(node));
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
    analyzerFuncsMap[typeid(UseStatement)] = &Static::analyzeUseStatement;
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

    // Since this is more of a container just recursively call the analyzer
    for (const auto &member : dataStmt->fields)
    {
        analyze(member.get()); // This will tabulate the total of all the members
    }
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

    if (func)
    {
        llvm::Type *ptrType = func->getType();
        const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();
        uint64_t size = DL.getTypeAllocSize(ptrType);
        total_size += size;
    }
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
        llvm::Type *ptrType = func->getType();

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

    // This is basically a functions wrapper so we call the generator to handle that
    for (const auto &method : behaviorStmt->functions)
    {
        analyze(method.get());
    }
}

void Static::analyzeUseStatement(Node *node)
{
    auto useStmt = dynamic_cast<UseStatement *>(node);

    if (!useStmt)
        return;

    // Getting the type of block we are importing from
    Token kind_token = useStmt->kind_token;
    // Getting the block name expression
    auto blockNameExpr = useStmt->blockNameOrCall.get();
    const std::string &blockName = blockNameExpr->expression.TokenLiteral;
    auto [parentName, childName] = semantics.splitScopedName(blockName);

    const llvm::DataLayout &DL = irgen.getLLVMModule().getDataLayout();

    // Case 1: Data statement
    if (kind_token.type == TokenType::DATA)
    {
        // Getting the data block name
        auto it = semantics.metaData.find(blockNameExpr);
        if (it == semantics.metaData.end())
        {
            std::cout << "Data block with name '" + blockName + "' not found \n";
            return;
        }

        // Case a:Mass import
        if (childName.empty())
        {
            auto sym = it->second;
            auto &members = sym->members;
            for (const auto &[memberName, memInfo] : members)
            {
                // Getting the data type of the member
                auto type = memInfo.type.kind;
                // Getting the corresponding llvm type
                llvm::Type *memType = irgen.getLLVMType(type);

                uint64_t size = DL.getTypeAllocSize(memType);
                total_size += size;
            }
        }
        else
        {
            // Case b: Singular import(Fall back if we have the child name and parent name)
            auto parentIt = semantics.customTypesTable.find(parentName);
            if (parentIt == semantics.customTypesTable.end())
            {
                std::cout << "Could not find '" + parentName + "'\n";
                return;
            }
            auto members = parentIt->second.members;
            auto memberIt = members.find(childName);
            if (memberIt == members.end())
            {
                std::cout << "Could not find '" + childName + "'\n";
                return;
            }

            auto memberType = memberIt->second.type.kind;

            llvm::Type *llvmMemType = irgen.getLLVMType(memberType);

            uint64_t size = DL.getTypeAllocSize(llvmMemType);
            total_size += size;
        }
    }

    // Case 2: Behavior statement
}
