#include <typeindex>
#include <unordered_map>
#include "ast.hpp"
#include "semantics/semantics.hpp"
#include "irgen/irgen.hpp"

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
    // Component calculators
    void calculateLetStatementSize(Node *node);
    void calculateWhileStatementSize(Node *node);
    void calculateForStatementSize(Node *node);
    void calculateIfStatementSize(Node *node);
    void calculateElifStatementSize(Node *node);
    void calculateBlockStatementMembersSize(Node *node);
    void calculateFunctionStatement(Node *node);
    void calculateFunctionExpression(Node *node);
    void calculateBlockExpression(Node *node);
    void calculateDataStatement(Node *node);
    void calculateSealStatement(Node *node);
    void calculateComponentStatement(Node *node);
    void calculateInstantiateStatement(Node *node);
    // HELPERS
    void registerComponentCalculatorFns();
    void logPrestaticError(const std::string &message, int line, int col);
    llvm::Type *getLLVMType(ResolvedType type);
};