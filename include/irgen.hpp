#pragma once

#include <llvm-18/llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <memory>
#include <typeindex>
#include "ast.hpp"
#include "audit.hpp"
#include "llvm/IR/Type.h"

enum class Classification{
    INTEGER,//Pass in integer registers(GPR)
    SSE, //Pass in SSE registers
    SSEUP, //Pass in upper half of SSE register
    X87, //Pass in x87 FPU stack(rare)
    COMPLEX_X87, //Complex numbers
    MEMORY, //Pass by reference (>16 bytes or complex)
    NO_CLASS
};

struct CoercionInfo{
    llvm::Type *coercedType;
    std::vector<Classification> classes;//Classification for each 8-byte word
    bool isMemory;
};

struct JumpTarget {
    llvm::BasicBlock *breakTarget;
    llvm::BasicBlock *continueTarget;
    Node *target;
};

// Optimization levels
enum class OptLevel {
    NONE,       // --debug    (-O0)
    BASIC,      // --basic    (-O1)
    STANDARD,   // --release  (-O2)
    AGGRESSIVE  // --aggressive (-O3)
};

class IRGenerator {
    OptLevel optLevel;

   public:
    IRGenerator(Semantics &semantics, ErrorHandler &handler, Auditor &auditor, size_t totalHeapSize,
                bool isVerbose, OptLevel optLevel);

    using generatorFunctions = void (IRGenerator::*)(Node *node);
    using expressionGenerators = llvm::Value *(IRGenerator::*)(Node *node);
    using addressGenerators = llvm::Value *(IRGenerator::*)(Node *node);

    void generate(const std::vector<std::unique_ptr<Node>> &program);
    void dumpIR();

    llvm::Module &getLLVMModule();
    llvm::Type *getLLVMType(const ResolvedType &type);

    std::unordered_map<std::type_index, generatorFunctions> generatorFunctionsMap;
    std::unordered_map<std::type_index, expressionGenerators> expressionGeneratorsMap;
    std::unordered_map<std::type_index, addressGenerators> addressGeneratorsMap;
    std::unordered_map<std::string, llvm::StructType *> componentTypes;
    std::unordered_map<std::string,llvm::StructType*> recordTypes;
    std::unordered_map<std::string, llvm::StructType *> llvmCustomTypes;
    std::unordered_map<std::string, unsigned> llvmStructIndices;
    std::unordered_map<llvm::Function *, llvm::AllocaInst *> currentFunctionSelfMap;

    ComponentStatement *currentComponent = nullptr;
    llvm::Value *currentComponentInstance;
    llvm::Function *currentFunction;
    std::vector<JumpTarget> jumpStack;

    bool isGlobalScope = true;

    bool emitObjectFile(const std::string &outPath);

   private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> funcBuilder;  // This is the builder for functions
    std::unique_ptr<llvm::Module> module;
    const llvm::DataLayout *layout;
    Semantics &semantics;
    ErrorHandler &errorHandler;
    Auditor &auditor;
    size_t totalHeapSize;
    bool isVerbose;
    bool inhibitCleanUp = false;

    CoercionInfo classifyStruct(llvm::StructType *structTy);
    void classifyWord(uint64_t offset, uint64_t size, llvm::Type* type, std::vector<Classification>& classes);
    Classification mergeClassifications(Classification c1,Classification c2);
    llvm::Type *createCoercedType(const std::vector<Classification>&classes,uint64_t size);

    struct FunctionCoercion{
        std::vector<llvm::Type*>originalParamTypes;
        std::vector<llvm::Type*>coercedParamTypes;
        std::vector<CoercionInfo> paramCoercion;
        bool hasSRet;
        CoercionInfo returnCoercion;
    };
    std::unordered_map<llvm::Function*,FunctionCoercion> functionCoercionMap;

    //Coercion helper
    llvm::Value *coerceArgument(llvm::Value *arg,const CoercionInfo &info);
    llvm::Value *coerceReturn(llvm::Value *retVal,const CoercionInfo &info);

    void setupTargetLayout();

    // GENERATOR FUNCTIONS FOR STATEMENTS
    void generateVariableDeclaration(Node *node);
    void generateExpressionStatement(Node *node);

    void generateAssignmentStatement(Node *node);
    void generateFieldAssignmentStatement(Node *node);
    void generatePointerAssignmentStatement(AssignmentStatement *assignStmt);

    void generateWhileStatement(Node *node);
    void generateForStatement(Node *node);
    void generateIfStatement(Node *node);
    void generateSwitchStatement(Node *node);
    void generateBreakStatement(Node *node);
    void generateContinueStatement(Node *node);
    void generateReturnStatement(Node *node);
    void generateBlockStatement(Node *node);
    void generateFunctionStatement(Node *node);
    void generateFunctionDeclaration(Node *node);
    // Special case because this is wrapped in the function statement but actaully
    // doesnt evaluate to anything;
    void generateFunctionDeclarationExpression(Node *node);
    // Component system
    void generateRecordStatement(Node *node);
    void generateInitFunction(Node *node, ComponentStatement *component);
    void generateComponentStatement(Node *node);
    void generateComponentFunctionStatement(Node *node, const std::string &compName);

    void generateInstantiateStatement(Node *node);
    void generateSealStatement(Node *node);

    // Enum class system
    void generateEnumStatement(Node *node);
    // Trace statement generator
    void generateTraceStatement(Node *node);
    void generateAllocatorInterface(Node *node);

    // GENERATOR FUNCTIONS FOR EXPRESSIONS
    llvm::Value *generateInfixExpression(Node *node);
    llvm::Value *generatePrefixExpression(Node *node);
    llvm::Value *generatePostfixExpression(Node *node);
    llvm::Value *generateStringLiteral(Node *node);
    llvm::Value *generateFStringLiteral(Node *node);
    llvm::Value *generateChar8Literal(Node *node);
    llvm::Value *generateChar16Literal(Node *node);
    llvm::Value *generateChar32Literal(Node *node);
    llvm::Value *generateBooleanLiteral(Node *node);
    llvm::Value *generateF32Literal(Node *node);
    llvm::Value *generateF64Literal(Node *node);
    llvm::Value *generateFloatLiteral(Node *node);
    llvm::Value *generateArrayLiteral(Node *node);
    llvm::Value *generateSizeOfExpression(Node *node);

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
    llvm::Value *generateINTLiteral(Node *node);
    llvm::Value *generateNullLiteral(Node *node);

    llvm::Value *generateIdentifierExpression(Node *node);
    llvm::Value *generateAddressExpression(Node *node);
    llvm::Value *generateDereferenceExpression(Node *node);

    llvm::Value *generateBlockExpression(Node *node);
    llvm::Value *generateFunctionExpression(Node *node);
    llvm::Value *generateCallExpression(Node *node);
    llvm::Value *generateUnwrapExpression(Node *node);

    llvm::Value *generateSelfExpression(Node *node);
    llvm::Value *generateInstanceExpression(Node *node);
    llvm::Value *generateMethodCallExpression(Node *node);
    llvm::Value *generateArraySubscriptExpression(Node *node);

    llvm::Value *generateCastExpression(Node *node);
    llvm::Value *generateBitcastExpression(Node *node);

    // GENERATOR FUNCTIONS FOR ADRRESSES(L VALUES)
    llvm::Value *generateSelfAddress(Node *node);
    llvm::Value *generateCallAddress(Node *node);
    llvm::Value *generateDereferenceAddress(Node *node);
    llvm::Value *generateArraySubscriptAddress(Node *node);
    llvm::Value *generateIdentifierAddress(Node *node);
    llvm::Value *generateInfixAddress(Node *node);

    // HELPER FUNCTIONS
    void registerGeneratorFunctions();
    void registerExpressionGeneratorFunctions();
    void registerAddressGeneratorFunctions();

    llvm::Value *generateExpression(Node *node);
    llvm::Value *generateAddress(Node *node);

    void generateStatement(Node *node);
    void traceRuntime(llvm::Value *val, ResolvedType type);
    void declareImportedSeals();
    void declareImportedTypes();
    void declareImportedFunctions();
    void declareImportedVariables();
    void declareCustomTypes();
    void registerAllocators();
    void declareImportedInit(const std::string &typeName);
    char *const_unnitoa(__int128 val, char *buf);
    char decodeCharLiteral(const std::string &literal);
    uint32_t decodeUTF8ToCodePoint(const std::string &literal);
    uint16_t decodeChar16Literal(const std::string &literal);
    uint32_t decodeChar32Literal(const std::string &literal);
    uint32_t convertIntTypeToWidth(const DataType &dt);
    bool isIntegerType(const DataType &dt);
    bool isSignedInteger(const DataType &dt);
    bool isDynamicArrayLiteral(ArrayLiteral *literal);
    bool currentBlockIsTerminated();
    bool isUnsigned(const ResolvedType &type);
    unsigned getIntegerBitWidth(DataType dt);

    void finalizeTypeBody(const std::string &typeName,
                          const std::shared_ptr<CustomTypeInfo> &typeInfo, std::string category);
    void declareImportedComponentMethods(const std::string &funcName, const std::string &typeName,
                                         const std::shared_ptr<MemberInfo> &memberInfo);
    llvm::Value *coerceToBoolean(llvm::Value *val, Node *expr);
    llvm::GlobalVariable *createGlobalArrayConstant(llvm::Constant *constantArray);
    llvm::Value *generateStaticArray(ArrayLiteral *lit);
    void emitDynamicInitialization(ArrayLiteral *arrLit, llvm::Value *ptr, llvm::Type *baseTy,
                                   int &index);
    size_t getFlatCount(ArrayLiteral *arrLit);
    llvm::Type *getArrayBaseType(const ResolvedType &type);

    // Infix helpers
    bool ArithmeticOrBitwiseOperator(TokenType op);
    bool ComparisonOperator(TokenType op);
    llvm::Value *handleArithmeticAndBitwise(InfixExpression *infix, llvm::Value *left,
                                            llvm::Value *right,
                                            const std::shared_ptr<SymbolInfo> &leftSym,
                                            const std::shared_ptr<SymbolInfo> &rightSym);
    llvm::Value *handleComparison(InfixExpression *infix, llvm::Value *left, llvm::Value *right,
                                  const std::shared_ptr<SymbolInfo> &leftSym,
                                  const std::shared_ptr<SymbolInfo> &rightSym);
    llvm::Value *handleCoalesce(InfixExpression *infix, llvm::Value *leftStruct);
    llvm::Value *handleLogical(InfixExpression *infix, llvm::Value *left,
                               const std::shared_ptr<SymbolInfo> &leftSym,
                               const std::shared_ptr<SymbolInfo> &rightSym);
    llvm::Value *handleMemberAccess(InfixExpression *infix, llvm::Value *left,
                                    const std::shared_ptr<SymbolInfo> &leftSym,
                                    const std::shared_ptr<SymbolInfo> &rightSym);
    llvm::Value *handleEnumAccess(InfixExpression *infix);

    std::vector<llvm::Value *> prepareArguments(
        llvm::Function *func, const std::vector<std::unique_ptr<Expression>> &params,
        size_t offset,const FunctionCoercion& coercion);
    // For normal variables
    void generateGlobalScalarLet(std::shared_ptr<SymbolInfo> sym, const std::string &name,
                                 Node *value);
    llvm::Value *generateScalarStorage(VariableDeclaration *decl, std::shared_ptr<SymbolInfo> sym,
                                       const std::string &name);
    llvm::Value *generateArrayStorage(VariableDeclaration *decl, std::shared_ptr<SymbolInfo> sym,
                                      const std::string &name);
    llvm::Value *generateReferenceStorage(VariableDeclaration *decl,
                                          std::shared_ptr<SymbolInfo> sym, const std::string &name);
    llvm::Value *generateComponentStorage(VariableDeclaration *decl,
                                          std::shared_ptr<SymbolInfo> sym, const std::string &name);
    llvm::Constant *generateGlobalRecordDefaults(const std::string &typeName);
    // For components
    llvm::Value *generateComponentInit(VariableDeclaration *declaration,
                                       std::shared_ptr<SymbolInfo> sym, llvm::StructType *structTy,
                                       bool isHeap);
    bool isComponentType(const std::string &name);
    bool isRecordType(const std::string &name);
    // Helper for allocating heap storage
    llvm::Value *allocateDynamicHeapStorage(std::shared_ptr<SymbolInfo> sym,
                                            const std::string &letName);
    llvm::Value *allocateRuntimeHeap(std::shared_ptr<SymbolInfo> sym, llvm::Value *size,
                                     const std::string &varName);
    void executePhysicalFree(const std::shared_ptr<SymbolInfo> &sym);
    void freeDynamicHeapStorage(const std::string &allocatorType, llvm::Value *toFree);
    void emitCleanup(Node *contextNode);
    void emitDeclarationClean(Node *contextNode);
    void freeForeigners(Node *block);
    void freeNatives(Node *block);
    void emitBlockCleanUp(Node *block);
    void flattenArrayLiteral(ArrayLiteral *arrLit, std::vector<llvm::Constant *> &flatElems,
                             llvm::Type *&baseType);
    llvm::Value *generateIntegerLiteral(const std::string &literalStr, uint32_t bitWidth,
                                        bool isSigned);
    llvm::Value *stringizeValue(llvm::Value *val, const ResolvedType &type);
    llvm::Function *getOrDeclareStrlen();
    llvm::Function *getOrDeclareWrite();
    llvm::Function *getOrDeclareUnnitoa();
    llvm::Function *getOrDeclareUnnftoa();
    llvm::Function *getOrDeclareStrcat();
    llvm::Function *getOrDeclareUnniptoa();
    llvm::Value *calculateFStringSize(FStringLiteral *fStr);
    void reportDevBug(const std::string &message, Node *contextNode);
    void logInternal(const std::string &message);
};
