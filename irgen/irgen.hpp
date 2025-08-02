#ifndef IRGENERATOR_HPP
#define IRGENERATOR_HPP

#include <memory>
#include <map>
#include <typeindex>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include "semantics/semantics.hpp"

#define CPPREST_FORCE_REBUILD

class Node;

struct LoopBlocks
{
    llvm::BasicBlock *condBB;
    llvm::BasicBlock *afterBB;
};

class IRGenerator
{
public:
    IRGenerator(Semantics &semantics);

    using generatorFunctions = void (IRGenerator::*)(Node *node);
    using expressionGenerators = llvm::Value *(IRGenerator::*)(Node * node);

    void generate(const std::vector<std::unique_ptr<Node>> &program);
    void dumpIR();

    std::unordered_map<std::type_index, generatorFunctions> generatorFunctionsMap;
    std::unordered_map<std::type_index, expressionGenerators> expressionGeneratorsMap;
    std::vector<LoopBlocks> loopBlocksStack;

private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::map<std::string, llvm::Value *> namedValues;
    Semantics &semantics;

    // GENERATOR FUNCTIONS FOR STATEMENTS
    void generateLetStatement(Node *node);
    void generateExpressionStatement(Node *node);
    void generateAssignmentStatement(Node *node);
    void generateWhileStatement(Node *node);
    void generateForStatement(Node *node);
    void generateIfStatement(Node *node);
    void generateBreakStatement(Node *node);
    void generateContinueStatement(Node *node);
    void generateBlockStatement(Node *node);

    // GENERATOR FUNCTIONS FOR EXPRESSIONS
    llvm::Value *generateInfixExpression(Node *node);
    llvm::Value *generatePrefixExpression(Node *node);
    llvm::Value *generatePostfixExpression(Node *node);
    llvm::Value *generateStringLiteral(Node *node);
    llvm::Value *generateCharLiteral(Node *node);
    llvm::Value *generateBooleanLiteral(Node *node);
    llvm::Value *generateFloatLiteral(Node *node);
    llvm::Value *generateDoubleLiteral(Node *node);
    llvm::Value *generateIntegerLiteral(Node *node);
    llvm::Value *generateNullLiteral(NullLiteral *nullLit, DataType type);
    llvm::Value *generateIdentifierExpression(Node *node);

    // HELPER FUNCTIONS
    void registerGeneratorFunctions();
    void registerExpressionGeneratorFunctions();
    llvm::Value *generateExpression(Node *node);
    void generateStatement(Node *node);
    llvm::Type *getLLVMType(DataType type);
    char decodeCharLiteral(const std::string &literal);
};

#endif