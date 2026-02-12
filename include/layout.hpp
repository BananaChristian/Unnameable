#include "ast.hpp"
#include "errors.hpp"
#include "irgen.hpp"
#include "semantics.hpp"
#include <llvm-18/llvm/IR/DataLayout.h>
#include <typeindex>
#include <unordered_map>

class Layout {
public:
  Semantics &semantics;
  llvm::LLVMContext &context;
  std::unique_ptr<llvm::Module> tempModule;
  ErrorHandler &errorHandler;

  Layout(Semantics &sem, llvm::LLVMContext &ctx, ErrorHandler &errorHandler,
         bool verbose);

  using calculateSizeFns = void (Layout::*)(Node *node);
  std::unordered_map<std::type_index, calculateSizeFns> calculatorFnsMap;

  std::unordered_map<std::string, llvm::StructType *> typeMap;

  uint64_t totalHeapSize = 0;

  // Calculator driver
  void calculatorDriver(Node *node);
  bool failed();

private:
  bool hasFailed = false;
  bool verbose = false;

  const llvm::DataLayout *layout;
  // Component calculators
  void calculateLetStatementSize(Node *node);
  void calculateArrayStatementSize(Node *node);
  void calculateHeapStatementSize(Node *node);
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
  uint64_t countFlattenedElements(Node *node);

  // Loggers
  void logLayoutError(const std::string &message, int line, int col);
  void reportDevBug(const std::string &message);
  void logInternal(const std::string &message);

  llvm::Type *getLLVMType(ResolvedType type);
};
