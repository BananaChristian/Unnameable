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
    using addressGenerators = llvm::Value *(IRGenerator::*)(Node * node);

    void generate(const std::vector<std::unique_ptr<Node>> &program);
    void dumpIR();

    llvm::Module &getLLVMModule();
    llvm::Type *getLLVMType(ResolvedType type);
    llvm::Type *lowerFunctionType(const ResolvedType &type);

    std::unordered_map<std::type_index, generatorFunctions> generatorFunctionsMap;
    std::unordered_map<std::type_index, expressionGenerators> expressionGeneratorsMap;
    std::unordered_map<std::type_index, addressGenerators> addressGeneratorsMap;
    std::unordered_map<std::string, llvm::StructType *> componentTypes;
    std::unordered_map<std::string, llvm::StructType *> llvmCustomTypes;
    std::unordered_map<std::string, llvm::Value *> llvmGlobalDataBlocks;
    std::unordered_map<std::string, unsigned> llvmStructIndices;
    std::unordered_map<llvm::Function *, llvm::AllocaInst *> currentFunctionSelfMap;

    ComponentStatement *currentComponent = nullptr;
    llvm::Value *currentComponentInstance;
    llvm::Function *currentFunction;
    std::vector<LoopBlocks> loopBlocksStack;

    bool isSageInitCalled = false;
    bool isSageDestroyCalled = false;

    bool isGlobalScope = true;

    bool emitObjectFile(const std::string &outPath);
    void generateSageDestroyCall();

private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> globalBuilder; // THis is the global builder
    llvm::IRBuilder<> funcBuilder;   // This is the builder for functions
    std::unique_ptr<llvm::Module> module;
    Semantics &semantics;
    size_t totalHeapSize;
    bool mainMarker = false;

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
    void generateInitFunction(Node *node, ComponentStatement *component);
    void generateComponentStatement(Node *node);
    void generateComponentFunctionStatement(Node *node, const std::string &compName);

    void generateInstantiateStatement(Node *node);
    void generateSealStatement(Node *node);
    // Array statement generator
    void generateArrayStatement(Node *node);
    // Enum class system
    void generateEnumClassStatement(Node *node);
    // Shout statement generator
    void generateShoutStatement(Node *node);

    void generateQualifyStatement(Node *node);

    // GENERATOR FUNCTIONS FOR EXPRESSIONS
    llvm::Value *generateInfixExpression(Node *node);
    llvm::Value *generatePrefixExpression(Node *node);
    llvm::Value *generatePostfixExpression(Node *node);
    llvm::Value *generateStringLiteral(Node *node);
    llvm::Value *generateChar8Literal(Node *node);
    llvm::Value *generateChar16Literal(Node *node);
    llvm::Value *generateChar32Literal(Node *node);
    llvm::Value *generateBooleanLiteral(Node *node);
    llvm::Value *generateFloatLiteral(Node *node);
    llvm::Value *generateDoubleLiteral(Node *node);
    llvm::Value *generateArrayLiteral(Node *node);

    llvm::Value *generateI8Literal(Node *node);
    llvm::Value *generateU8Literal(Node *node);
    llvm::Value *generateI16Literal(Node *node);
    llvm::Value *generateU16Literal(Node *node);
    llvm::Value *generateI32Literal(Node *node);
    llvm::Value *generateU32Literal(Node *node);
    llvm::Value *generateI64Literal(Node *node);
    llvm::Value *generateU64Literal(Node *node);
    llvm::Value *generateI128Literal(Node *node);
    llvm::Value *generateU128Literal(Node *node);
    llvm::Value *generateISIZELiteral(Node *node);
    llvm::Value *generateUSIZELiteral(Node *node);

    llvm::Value *generateNullLiteral(NullLiteral *nullLit, DataType type);
    llvm::Value *generateIdentifierExpression(Node *node);
    llvm::Value *generateAddressExpression(Node *node);
    llvm::Value *generateDereferenceExpression(Node *node);

    llvm::Value *generateBlockExpression(Node *node);
    llvm::Value *generateFunctionExpression(Node *node);
    llvm::Value *generateCallExpression(Node *node);
    llvm::Value *generateUnwrapCallExpression(Node *node);

    llvm::Value *generateSelfExpression(Node *node);
    llvm::Value *generateInstanceExpression(Node *node);
    llvm::Value *generateMethodCallExpression(Node *node);
    llvm::Value *generateArraySubscriptExpression(Node *node);

    // GENERATOR FUNCTIONS FOR ADRRESSES(L VALUES)
    llvm::Value *generateSelfAddress(Node *node);
    llvm::Value *generateCallAddress(Node *node);
    llvm::Value *generateDereferenceAddress(Node *node);
    llvm::Value *generateArraySubscriptAddress(Node *node);

    // HELPER FUNCTIONS
    void registerGeneratorFunctions();
    void registerExpressionGeneratorFunctions();
    void registerAddressGeneratorFunctions();
    llvm::Value *generateExpression(Node *node);
    llvm::Value *generateAddress(Node *node);
    AddressAndPendingFree generateIdentifierAddress(Node *node);
    void generateStatement(Node *node);
    void shoutRuntime(llvm::Value *val, ResolvedType type);
    void declareImportedSeals();
    void declareImportedTypes();
    void declareCustomTypes();
    void declareImportedInit(const std::string &typeName);
    char *const_unnitoa(__int128 val, char *buf);
    char decodeCharLiteral(const std::string &literal);
    void generateSageInitCall();
    uint16_t decodeChar16Literal(const std::string &literal);
    uint32_t decodeChar32Literal(const std::string &literal);
    bool isIntegerType(DataType dt);
    bool isSignedInteger(DataType dt);
    bool currentBlockIsTerminated();
    unsigned getIntegerBitWidth(DataType dt);

    void finalizeTypeBody(const std::string &typeName, const std::shared_ptr<CustomTypeInfo> &typeInfo, std::string category);
    void declareImportedComponentMethods(const std::string &funcName, const std::string &typeName, const std::shared_ptr<MemberInfo> &memberInfo);

    llvm::GlobalVariable *createGlobalArrayConstant(llvm::Constant *constantArray);

    // For normal variables
    void generateGlobalHeapLet(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, const std::string &letName);
    void generateGlobalScalarLet(std::shared_ptr<SymbolInfo> sym, const std::string &letName, Expression *value);
    // For components
    llvm::Value *generateComponentInit(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, llvm::StructType *structTy, bool isHeap);
    void generateGlobalComponentHeapInit(LetStatement *letStmt, std::shared_ptr<SymbolInfo> sym, const std::string &letName, llvm::StructType *structType);
    // Helper for allocating heap storage (shared logic for components and scalars)
    llvm::Value *allocateHeapStorage(std::shared_ptr<SymbolInfo> sym, const std::string &letName, llvm::StructType *structTy);
    void freeHeapStorage(uint64_t size, uint64_t alignSize, const std::string &letName);
};

#endif