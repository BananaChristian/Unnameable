#include "layout.hpp"
#include "ast.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include <cstdint>
#include <llvm-18/llvm/IR/DerivedTypes.h>
#include <memory>
#include <string>

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_BOLD "\033[1m"

Layout::Layout(Semantics &sem, llvm::LLVMContext &ctx, ErrorHandler &handler,
               bool verbose)
    : semantics(sem), context(ctx), errorHandler(handler), verbose(verbose) {
  tempModule = std::make_unique<llvm::Module>("prestatic_temp", context);
  registerComponentCalculatorFns();

  // Use the target’s data layout string (can be copied from final module
  // settings)
  tempModule->setDataLayout("e-m:e-i64:64-f80:128-n8:16:32:64-S128");

  this->layout = &tempModule->getDataLayout();

  declareAllCustomTypes();

  registerImportedTypes();
}

// Calculator driver
void Layout::calculatorDriver(Node *node) {
  if (!node) {
    reportDevBug("Invalid node");
    return;
  }

  auto it = calculatorFnsMap.find(typeid(*node));
  if (it == calculatorFnsMap.end()) {
    logInternal("No calculator found for node '" + node->toString() +
                "' , Ignoring ...");
    return;
  }

  (this->*it->second)(node);
}

void Layout::registerComponentCalculatorFns() {
  calculatorFnsMap[typeid(VariableDeclaration)] =
      &Layout::calculateVariableDeclarationSize;
  calculatorFnsMap[typeid(WhileStatement)] =
      &Layout::calculateWhileStatementSize;
  calculatorFnsMap[typeid(ForStatement)] = &Layout::calculateForStatementSize;
  calculatorFnsMap[typeid(ifStatement)] = &Layout::calculateIfStatementSize;
  calculatorFnsMap[typeid(elifStatement)] = &Layout::calculateElifStatementSize;
  calculatorFnsMap[typeid(BlockStatement)] =
      &Layout::calculateBlockStatementMembersSize;
  calculatorFnsMap[typeid(FunctionStatement)] =
      &Layout::calculateFunctionStatement;
  calculatorFnsMap[typeid(FunctionExpression)] =
      &Layout::calculateFunctionExpression;
  calculatorFnsMap[typeid(BlockExpression)] = &Layout::calculateBlockExpression;
  calculatorFnsMap[typeid(RecordStatement)] = &Layout::calculateRecordStatement;
  calculatorFnsMap[typeid(ComponentStatement)] =
      &Layout::calculateComponentStatement;
  calculatorFnsMap[typeid(InstantiateStatement)] =
      &Layout::calculateInstantiateStatement;
  calculatorFnsMap[typeid(SealStatement)] = &Layout::calculateSealStatement;
  calculatorFnsMap[typeid(AllocatorStatement)] =
      &Layout::calculateAllocatorInterfaceSize;
  calculatorFnsMap[typeid(CaseClause)] = &Layout::calculateCaseClause;
  calculatorFnsMap[typeid(SwitchStatement)] = &Layout::calculateSwitchStatement;
}

// Independent calculators
void Layout::calculateVariableDeclarationSize(Node *node) {
  auto declaration = dynamic_cast<VariableDeclaration *>(node);
  if (!declaration)
    return;

  const std::string &name = declaration->var_name->expression.TokenLiteral;
  auto type_modifier =
      dynamic_cast<TypeModifier *>(declaration->modified_type.get());
  uint64_t compSize = 0;
  llvm::Align compAlignment;

  auto sym = semantics.getSymbolFromMeta(declaration);
  if (!sym)
    reportDevBug("Failed to find declaration '" + name + "' symbol info");

  if (!sym->isHeap)
    return;

  llvm::Type *type = nullptr;
  if (sym->isArray) {
    uint64_t totalElements = 0;
    uint64_t totalLiteralElements = 0;
    uint64_t totalByteSize = 0;
    auto elementType = semantics.getArrayElementType(sym->type);
    auto elemLLVMty = getLLVMType(elementType);
    uint64_t elementSize = layout->getTypeAllocSize(elemLLVMty);
    llvm::Align elementAlign = layout->getABITypeAlign(elemLLVMty);

    // If the user provided the length
    bool isConstantLength = true;
    if (!type_modifier->dimensions.empty()) {
      totalElements = 1; // Start at 1 for multiplication
      for (const auto &sizeNode : type_modifier->dimensions) {
        if (semantics.isIntegerConstant(sizeNode.get())) {
          uint64_t dimSize = semantics.getIntegerConstant(sizeNode.get());
          totalElements *= dimSize;
        } else {
          totalElements = 0;
          isConstantLength = false;
          break; // If one dimension is dynamic, the whole compile-time size is
                 // unknown
        }
      }
    }

    // If the user gave a literal
    auto initializer = declaration->initializer.get();
    if (declaration->initializer) {
      if (dynamic_cast<ArrayLiteral *>(initializer))
        totalLiteralElements = countFlattenedElements(initializer);
      else if (dynamic_cast<NullLiteral *>(initializer))
        totalLiteralElements = totalElements;
      else {
        if (auto unwrap = dynamic_cast<UnwrapExpression *>(initializer))
          initializer = unwrap->expr.get();

        auto initSym = semantics.getSymbolFromMeta(initializer);
        if (!initSym)
          reportDevBug("Could not find initializer symbol info");

        totalLiteralElements = initSym->componentSize / elementSize;
      }
    }

    if (!type_modifier->dimensions.empty() && isConstantLength) {
      if (declaration->initializer && totalElements != totalLiteralElements) {
        logLayoutError("Size mismatch between expected size '" +
                           std::to_string(totalElements) +
                           "' elements and declared size '" +
                           std::to_string(totalLiteralElements) +
                           "' elements for array '" + name + "'",
                       declaration);
      }
    } else {
      totalElements = totalLiteralElements;
    }

    logInternal("Total Array Element size :" + std::to_string(totalElements));
    totalByteSize = totalElements * elementSize;
    logInternal("Total Array Byte size :" + std::to_string(totalByteSize) +
                " bytes");

    sym->componentSize = totalByteSize;
    sym->alignment = elementAlign;
    return;
  }

  if (sym->isPointer || sym->isRef)
    type = llvm::PointerType::get(context, 0);
  else
    type = getLLVMType(sym->type);

  compSize = layout->getTypeAllocSize(type);
  compAlignment = layout->getABITypeAlign(type);

  logInternal("Allocation size :" + std::to_string(compSize) + " bytes");
  sym->alignment = compAlignment;
  sym->componentSize = compSize;
}

void Layout::calculateBlockStatementMembersSize(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt)
    return;

  for (const auto &stmt : blockStmt->statements) {
    calculatorDriver(stmt.get());
  }
}

void Layout::calculateForStatementSize(Node *node) {
  auto forStmt = dynamic_cast<ForStatement *>(node);
  if (!forStmt)
    return;

  calculatorDriver(forStmt->body.get());
}

void Layout::calculateWhileStatementSize(Node *node) {
  auto whileStmt = dynamic_cast<WhileStatement *>(node);
  if (!whileStmt)
    return;

  auto content = whileStmt->loop.get();

  calculatorDriver(content);
}

void Layout::calculateIfStatementSize(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt)
    return;

  calculatorDriver(ifStmt->if_result.get());
  if (!ifStmt->elifClauses.empty()) {
    for (const auto &elif : ifStmt->elifClauses) {
      calculatorDriver(elif.get());
    }
  }
  if (ifStmt->else_result.has_value()) {
    calculatorDriver(ifStmt->else_result.value().get());
  }
}

void Layout::calculateElifStatementSize(Node *node) {
  auto elifStmt = dynamic_cast<elifStatement *>(node);
  if (!elifStmt)
    return;

  calculatorDriver(elifStmt->elif_result.get());
}

void Layout::calculateSwitchStatement(Node *node) {
  auto switchStmt = dynamic_cast<SwitchStatement *>(node);
  if (!switchStmt)
    return;

  for (const auto &clause : switchStmt->case_clauses)
    calculatorDriver(clause.get());

  for (const auto &defs : switchStmt->default_statements)
    calculatorDriver(defs.get());
}

void Layout::calculateCaseClause(Node *node) {
  auto caseClause = dynamic_cast<CaseClause *>(node);
  if (!caseClause)
    return;

  for (const auto &stmt : caseClause->body)
    calculatorDriver(stmt.get());
}

void Layout::calculateFunctionStatement(Node *node) {
  auto funcStmt = dynamic_cast<FunctionStatement *>(node);
  if (!funcStmt)
    return;

  calculatorDriver(funcStmt->funcExpr.get());
}

void Layout::calculateFunctionExpression(Node *node) {
  auto funcExpr = dynamic_cast<FunctionExpression *>(node);
  if (!funcExpr)
    return;

  calculatorDriver(funcExpr->block.get());
}

void Layout::calculateBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr)
    return;

  if (!blockExpr->statements.empty()) {
    for (const auto &stmt : blockExpr->statements) {
      calculatorDriver(stmt.get());
    }
  }

  if (blockExpr->finalexpr.has_value()) {
    calculatorDriver(blockExpr->finalexpr.value().get());
  }
}

void Layout::calculateInstantiateStatement(Node *node) {
  auto instStmt = dynamic_cast<InstantiateStatement *>(node);

  if (!instStmt)
    return;

  auto sym = semantics.getSymbolFromMeta(instStmt);
  if (!sym)
    return;

  // Get the instantiation info
  const auto &instTable = sym->instTable;

  if (instTable.has_value()) {
    // Call the calculator driver on the cloned AST
    calculatorDriver(instTable->instantiatedAST.get());
  }
}

void Layout::calculateRecordStatement(Node *node) {
  auto recordStmt = dynamic_cast<RecordStatement *>(node);
  if (!recordStmt)
    return;

  auto recordName = recordStmt->recordName->expression.TokenLiteral;

  auto dataMeta = semantics.metaData.find(recordStmt);
  if (dataMeta == semantics.metaData.end()) {
    reportDevBug("Could not find record '" + recordName + "' metaData");
    return;
  }

  auto dataSym = dataMeta->second;
  if (!dataSym) {
    reportDevBug("Could not find record symbolInfo '" + recordName + "'");
    return;
  }

  std::vector<llvm::Type *> fieldTypes;

  for (const auto &[key, value] : dataSym->members) {
    llvm::Type *fieldType = getLLVMType(value->type);
    fieldTypes.push_back(fieldType);
  }

  llvm::StructType *structTy = llvm::StructType::create(context, recordName);
  structTy->setBody(fieldTypes, /*isPacked*/ false);

  typeMap[recordName] = structTy;
}

void Layout::calculateComponentStatement(Node *node) {
  auto compStmt = dynamic_cast<ComponentStatement *>(node);
  if (!compStmt)
    return;

  auto compName = compStmt->component_name->expression.TokenLiteral;

  auto compMeta = semantics.metaData.find(compStmt);
  if (compMeta == semantics.metaData.end()) {
    reportDevBug("Could not find component metaData for component'" + compName +
                 "'");
    return;
  }

  auto compSym = compMeta->second;
  if (!compSym) {
    reportDevBug("Could not find component symbolInfo '" + compName + "'");
    return;
  }

  // Creating a component sketch
  std::vector<llvm::Type *> fieldTypes;

  for (const auto &[key, value] : compSym->members) {
    // Each member has a ResolvedType, get its LLVM type
    llvm::Type *fieldType = getLLVMType(value->type);
    fieldTypes.push_back(fieldType);
  }

  // Creating an empty struct type for data field types
  auto *structTy = llvm::cast<llvm::StructType>(typeMap[compName]);
  if (!structTy->isOpaque())
    return; // already defined

  structTy->setBody(fieldTypes, false);

  // Calculating for the private methods
  for (const auto &method : compStmt->methods) {
    logInternal("Inside private method calculation");
    calculatorDriver(method.get());
  }
}

void Layout::calculateSealStatement(Node *node) {
  auto sealStmt = dynamic_cast<SealStatement *>(node);
  if (!sealStmt)
    return;

  calculatorDriver(sealStmt->block.get());
}

llvm::Type *Layout::getLLVMType(const ResolvedType &type) {
  // Pointer and Reference,both are opaque ptrs in LLVM
  if (type.isPointer() || type.isRef()) {
    llvm::Type *ptrTy = llvm::PointerType::get(context, 0);
    if (type.isNull) {
      return llvm::StructType::get(
          context, {
                       llvm::Type::getInt1Ty(context), // is_present flag
                       ptrTy                           // the pointer
                   });
    }
    return ptrTy;
  }

  // Array, pointer to element type
  if (type.isArray()) {
    if (!type.innerType)
      throw std::runtime_error("Array type has no inner element type");
    llvm::Type *elemTy = getLLVMType(*type.innerType);
    llvm::Type *arrTy = llvm::PointerType::get(elemTy, 0);
    if (type.isNull) {
      return llvm::StructType::get(context,
                                   {llvm::Type::getInt1Ty(context), arrTy});
    }
    return arrTy;
  }

  llvm::Type *baseType = nullptr;
  switch (type.kind) {
  case DataType::I8:
  case DataType::U8:
  case DataType::CHAR8:
    baseType = llvm::Type::getInt8Ty(context);
    break;

  case DataType::I16:
  case DataType::U16:
  case DataType::CHAR16:
    baseType = llvm::Type::getInt16Ty(context);
    break;

  case DataType::I32:
  case DataType::U32:
  case DataType::CHAR32:
    baseType = llvm::Type::getInt32Ty(context);
    break;

  case DataType::I64:
  case DataType::U64:
    baseType = llvm::Type::getInt64Ty(context);
    break;

  case DataType::I128:
  case DataType::U128:
    baseType = llvm::Type::getInt128Ty(context);
    break;

  case DataType::ISIZE:
  case DataType::USIZE:
    baseType = tempModule->getDataLayout().getIntPtrType(context);
    break;

  case DataType::BOOLEAN:
    baseType = llvm::Type::getInt1Ty(context);
    break;

  case DataType::F32:
    baseType = llvm::Type::getFloatTy(context);
    break;

  case DataType::F64:
    baseType = llvm::Type::getDoubleTy(context);
    break;

  case DataType::STRING:
  case DataType::OPAQUE:
    // Both are just opaque pointers at the base level
    baseType = llvm::PointerType::get(context, 0);
    break;

  case DataType::VOID:
    baseType = llvm::Type::getVoidTy(context);
    break;

  case DataType::RECORD:
  case DataType::COMPONENT: {
    if (type.resolvedName.empty())
      throw std::runtime_error(
          "Custom type requested but resolvedName is empty");
    auto it = typeMap.find(type.resolvedName);
    if (it == typeMap.end())
      throw std::runtime_error("Layout requested for unknown custom type '" +
                               type.resolvedName + "'");
    baseType = it->second;
    break;
  }

  case DataType::ENUM: {
    auto enumIt = semantics.customTypesTable.find(type.resolvedName);
    if (enumIt == semantics.customTypesTable.end())
      throw std::runtime_error("Layout requested for unknown enum '" +
                               type.resolvedName + "'");
    // Recurse on the underlying type
    baseType = getLLVMType(ResolvedType::makeBase(
        enumIt->second->underLyingType, type.resolvedName));
    break;
  }

  case DataType::ERROR:
  case DataType::GENERIC:
  case DataType::UNKNOWN:
    throw std::runtime_error(
        "Unsupported or unknown data type in getLLVMType: " +
        type.resolvedName);
  }

  // Nullable base type, wrap in { i1, T }
  if (type.isNull) {
    return llvm::StructType::get(context,
                                 {llvm::Type::getInt1Ty(context), baseType});
  }

  return baseType;
}

void Layout::calculateAllocatorInterfaceSize(Node *node) {
  auto allocStmt = dynamic_cast<AllocatorStatement *>(node);
  if (!allocStmt)
    return;

  // Just call the driver on whatever is inside
  calculatorDriver(allocStmt->block.get());
}

void Layout::declareAllCustomTypes() {
  logInternal("Declaring all custom type");
  for (const auto &[name, info] : semantics.customTypesTable) {
    auto typeIt = typeMap.find(name);
    if (typeIt == typeMap.end())
      typeMap[name] = llvm::StructType::create(context, name);
  }
}

void Layout::registerImportedTypes() {
  logInternal("Registering component type");
  for (const auto &compTypesPair : semantics.ImportedComponentTable) {
    const auto &[compName, typeInfo] = compTypesPair;
    const auto &members = typeInfo->members;
    std::vector<llvm::Type *> fieldTypes;

    for (const auto &memberPair : members) {
      const auto &[memberName, memInfo] = memberPair;
      if (memInfo->isFunction)
        continue;

      llvm::Type *fieldType = getLLVMType(memInfo->type);
      fieldTypes.push_back(fieldType);
    }

    auto *structTy = llvm::cast<llvm::StructType>(typeMap[compName]);
    if (!structTy->isOpaque())
      return; // already defined

    structTy->setBody(fieldTypes, false);
  }

  for (const auto &dataTypesPair : semantics.ImportedRecordTable) {
    const auto &[dataName, typeInfo] = dataTypesPair;
    const auto &members = typeInfo->members;

    std::vector<llvm::Type *> fieldTypes;

    for (const auto &memberPair : members) {
      const auto &[memberName, memInfo] = memberPair;
      llvm::Type *fieldType = getLLVMType(memInfo->type);
      fieldTypes.push_back(fieldType);
    }

    auto *structTy = llvm::cast<llvm::StructType>(typeMap[dataName]);
    if (!structTy->isOpaque())
      return; // already defined

    structTy->setBody(fieldTypes, false);
  }
}

uint64_t Layout::countFlattenedElements(Node *node) {
  if (auto arrLit = dynamic_cast<ArrayLiteral *>(node)) {
    uint64_t total = 0;
    for (auto &element : arrLit->array) {
      total += countFlattenedElements(element.get());
    }
    return total;
  }
  // If it's not an array literal, it's a leaf (i32, f32, etc.)
  return 1;
}

void Layout::logLayoutError(const std::string &message, Node *contextNode) {
  auto tokenLine = 0;
  auto tokenColumn = 0;
  if (contextNode) {
    tokenLine = contextNode->token.line;
    tokenColumn = contextNode->token.column;
  }
  hasFailed = true;
  CompilerError error;
  error.level = ErrorLevel::LAYOUT;
  error.line = tokenLine;
  error.col = tokenColumn;
  error.message = message;
  error.tokenLength = errorHandler.getTokenLength(contextNode);
  error.hints = {};

  errorHandler.report(error);
}

void Layout::reportDevBug(const std::string &message) {
  hasFailed = true;
  std::cerr << COLOR_RED << "[INTERNAL COMPILER ERROR]: " << COLOR_RESET
            << message << "\n";
  std::abort();
}

void Layout::logInternal(const std::string &message) {
  if (verbose) {
    std::cout << message << "\n";
  }
}

bool Layout::failed() { return hasFailed; }
