#include <typeindex>
#include <unordered_map>
#include "ast.hpp"
#include "semantics/semantics.hpp"
#include "irgen/irgen.hpp"

class Prestatic
{
public:
    Semantics &semantics;
    llvm::LLVMContext &context;
    std::unique_ptr<llvm::Module> tempModule;

    Prestatic(Semantics &sem, llvm::LLVMContext &ctx);

    using calculateSizeFns = void (Prestatic::*)(Node *node);
    std::unordered_map<std::type_index, calculateSizeFns> calculatorFnsMap;

    // Calculator driver
    void calculatorDriver(Node *node);

private:
    // Component calculators
    void calculateLetStatementSize(Node *node);
    // HELPERS
    void registerComponentCalculatorFns();
    void logPrestaticError(const std::string &message, int line, int col);
    llvm::Type *getLLVMType(ResolvedType type);
};