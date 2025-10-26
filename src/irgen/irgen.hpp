#ifndef IRGENERATOR_HPP
#define IRGENERATOR_HPP

#include <memory>
#include <map>
#include <typeindex>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include "llvm/IR/Type.h"
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include "semantics/semantics.hpp"

#define CPPREST_FORCE_REBUILD

class Node;

struct LoopBlocks
{
    llvm::BasicBlock *condBB;
    llvm::BasicBlock *stepBB;
    llvm::BasicBlock *afterBB;
};

struct AddressAndPendingFree
{
    llvm::Value *address;
    llvm::CallInst *pendingFree;
};

class IRGenerator
{
public:
    IRGenerator(Semantics &semantics, size_t totalHeapSize);

    using generatorFunctions = void (IRGenerator::*)(Node *node);
    using expressionGenerators = llvm::Value *(IRGenerator::*)(Node * node);

    void generate(const std::vector<std::unique_ptr<Node>> &program);
    void dumpIR();

    llvm::Module &getLLVMModule();
    llvm::Type *getLLVMType(ResolvedType type);

    std::unordered_map<std::type_index, generatorFunctions> generatorFunctionsMap;
    std::unordered_map<std::type_index, expressionGenerators> expressionGeneratorsMap;
    std::unordered_map<std::string, llvm::StructType *> componentTypes;
    std::unordered_map<std::string, llvm::StructType *> llvmCustomTypes;
    std::unordered_map<std::string, llvm::Value *> llvmGlobalDataBlocks;
    std::unordered_map<std::string, unsigned> llvmStructIndices;

    ComponentStatement *currentComponent = nullptr;
    llvm::Value *currentComponentInstance;
    llvm::Function *currentFunction;
    std::vector<LoopBlocks> loopBlocksStack;

    bool isSageInitCalled = false;
    bool isSageDestroyCalled = false;

    bool emitObjectFile(const std::string &outPath);
    void generateSageDestroyCall();

private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> globalBuilder; // THis is the global builder
    llvm::IRBuilder<> funcBuilder;   // This is the builder for functions
    std::unique_ptr<llvm::Module> module;
    Semantics &semantics;
    size_t totalHeapSize;

    llvm::BasicBlock *heapInitFnEntry = nullptr;

    // GENERATOR FUNCTIONS FOR STATEMENTS
    void generateLetStatement(Node *node);
    void generateReferenceStatement(Node *node);
    void generatePointerStatement(Node *node);
    void generateExpressionStatement(Node *node);
    void generateAssignmentStatement(Node *node);
    void generateFieldAssignmentStatement(Node *node);
    void generateWhileStatement(Node *node);
    void generateForStatement(Node *node);
    void generateIfStatement(Node *node);
    void generateBreakStatement(Node *node);
    void generateContinueStatement(Node *node);
    void generateReturnStatement(Node *node);
    void generateBlockStatement(Node *node);
    void generateFunctionStatement(Node *node);
    void generateFunctionDeclaration(Node *node);
    // Special case because this is wrapped in the function statement but actaully doesnt evaluate to anything;
    void generateFunctionDeclarationExpression(Node *node);
    // Component system
    void generateDataStatement(Node *node);
    void generateBehaviorStatement(Node *node);
    void generateInitFunction(Node *node, ComponentStatement *component);
    void generateComponentStatement(Node *node);
    void generateComponentFunctionStatement(Node *node, const std::string &compName);

    // Enum class system
    void generateEnumClassStatement(Node *node);

    void generateShoutStatement(Node *node);

    // GENERATOR FUNCTIONS FOR EXPRESSIONS
    llvm::Value *generateInfixExpression(Node *node);
    llvm::Value *generatePrefixExpression(Node *node);
    llvm::Value *generatePostfixExpression(Node *node);
    llvm::Value *generateStringLiteral(Node *node);
    llvm::Value *generateCharLiteral(Node *node);
    llvm::Value *generateChar16Literal(Node *node);
    llvm::Value *generateChar32Literal(Node *node);
    llvm::Value *generateBooleanLiteral(Node *node);
    llvm::Value *generateFloatLiteral(Node *node);
    llvm::Value *generateDoubleLiteral(Node *node);

    llvm::Value *generateShortLiteral(Node *node);
    llvm::Value *generateUnsignedShortLiteral(Node *node);
    llvm::Value *generateIntegerLiteral(Node *node);
    llvm::Value *generateUnsignedIntegerLiteral(Node *node);
    llvm::Value *generateLongLiteral(Node *node);
    llvm::Value *generateUnsignedLongLiteral(Node *node);
    llvm::Value *generateExtraLiteral(Node *node);
    llvm::Value *generateUnsignedExtraLiteral(Node *node);

    llvm::Value *generateNullLiteral(NullLiteral *nullLit, DataType type);
    llvm::Value *generateIdentifierExpression(Node *node);
    llvm::Value *generateAddressExpression(Node *node);
    llvm::Value *generateDereferenceExpression(Node *node);

    llvm::Value *generateBlockExpression(Node *node);
    llvm::Value *generateFunctionExpression(Node *node);
    llvm::Value *generateCallExpression(Node *node);

    llvm::Value *generateSelfExpression(Node *node);
    llvm::Value *generateNewComponentExpression(Node *node);
    llvm::Value *generateInstanceExpression(Node *node);

    // HELPER FUNCTIONS
    void registerGeneratorFunctions();
    void registerExpressionGeneratorFunctions();
    llvm::Value *generateExpression(Node *node);
    AddressAndPendingFree generateIdentifierAddress(Node *node);
    void generateStatement(Node *node);
    void shoutRuntime(llvm::Value *val, ResolvedType type);
    char *unnitoa(int val, char *buf);
    char decodeCharLiteral(const std::string &literal);
    void generateSageInitCall();
    uint16_t decodeChar16Literal(const std::string &literal);
    uint32_t decodeChar32Literal(const std::string &literal);
    bool isIntegerType(DataType dt);
    bool isSignedInteger(DataType dt);
    bool inFunction();
    bool currentBlockIsTerminated();
    unsigned getIntegerBitWidth(DataType dt);

    void generateGlobalHeapLet(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, const std::string &letName);
    void generateGlobalScalarLet(std::shared_ptr<SymbolInfo> sym, const std::string &letName, Expression *value);
    llvm::Value *generateComponentInit(LetStatement *letStmt, llvm::StructType *structTy);
    // Helper for allocating heap storage (shared logic for components and scalars)
    llvm::Value *allocateHeapStorage(std::shared_ptr<SymbolInfo> sym, const std::string &letName, llvm::StructType *structTy);
    // Helper for initializing component members
    void initializeComponentMembers(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, const std::string &letName, llvm::Value *storage, llvm::StructType *structTy);
    void freeHeapStorage(uint64_t size, const std::string &letName);
};

#endif