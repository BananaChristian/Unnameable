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

class Node;

class IRGenerator
{
public:
    IRGenerator(Semantics &semantics);

    using generatorFunctions = void (IRGenerator::*)(Node *node);

    void generate(const std::vector<std::unique_ptr<Node>> &program);
    void dumpIR();

    std::unordered_map<std::type_index, generatorFunctions> generatorFunctionsMap;

private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::map<std::string, llvm::Value *> namedValues;
    Semantics &semantics;

    // GENERATOR FUNCTIONS
    void generateLetStatement(Node *node);
    void generateExpressionStatement(Node *node);

    // HELPER FUNCTIONS
    void registerGeneratorFunctions();
    llvm::Value *generateExpression(Node *node);
    llvm::Type *getLLVMType(DataType type);
};

#endif