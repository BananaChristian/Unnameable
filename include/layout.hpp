#include <llvm-18/llvm/IR/DataLayout.h>
#include <typeindex>
#include <unordered_map>
#include "ast.hpp"
#include "semantics.hpp"
#include "irgen.hpp"

class Layout
{
public:
    Semantics &semantics;
    llvm::LLVMContext &context;
    std::unique_ptr<llvm::Module> tempModule;

    Layout(Semantics &sem, llvm::LLVMContext &ctx);

    using calculateSizeFns = void (Layout::*)(Node *node);
    std::unordered_map<std::type_index, calculateSizeFns> calculatorFnsMap;

    std::unordered_map<std::string, llvm::StructType *> typeMap;

    uint64_t totalHeapSize = 0;

    // Calculator driver
    void calculatorDriver(Node *node);

private:
    const llvm::DataLayout *layout;
    // Component calculators
    void calculateLetStatementSize(Node *node);
    void calculateArrayStatementSize(Node *node);
    void calculateDheapStatementSize(Node *node);
    void calculatePointerStatementSize(Node *node);
    void calculateWhileStatementSize(Node *node);
    void calculateForStatementSize(Node *node);
    void calculateIfStatementSize(Node *node);
    void calculateElifStatementSize(Node *node);
    void calculateBlockStatementMembersSize(Node *node);
    void calculateFunctionStatement(Node *node);
    void calculateFunctionExpression(Node *node);
    void calculateBlockExpression(Node *node);
    void calculateRecordStatement(Node *node);
    void calculateSealStatement(Node *node);
    void calculateComponentStatement(Node *node);
    void calculateInstantiateStatement(Node *node);
    void calculateAllocatorInterfaceSize(Node *node);

    // HELPERS
    void declareAllCustomTypes();
    void registerImportedTypes();
    void registerComponentCalculatorFns();
    void logPrestaticError(const std::string &message, int line, int col);
    llvm::Type *getLLVMType(ResolvedType type);
};