#include "layout.hpp"
#include "ast.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include <cstdint>
#include <llvm-18/llvm/IR/DerivedTypes.h>
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
  calculatorFnsMap[typeid(LetStatement)] = &Layout::calculateLetStatementSize;
  calculatorFnsMap[typeid(ArrayStatement)] =
      &Layout::calculateArrayStatementSize;
  calculatorFnsMap[typeid(PointerStatement)] =
      &Layout::calculatePointerStatementSize;
  calculatorFnsMap[typeid(DheapStatement)] =
      &Layout::calculateDheapStatementSize;
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
}

// Independent calculators
void Layout::calculateLetStatementSize(Node *node) {
  auto letStmt = dynamic_cast<LetStatement *>(node);
  auto type = dynamic_cast<BasicType *>(letStmt->type.get());

  if (!letStmt)
    return;

  int line = type->data_token.line;
  int col = type->data_token.column;

  const std::string &name = letStmt->ident_token.TokenLiteral;

  uint64_t compSize = 0;
  llvm::Align compAlignment;

  // Getting the let statement metaData
  auto letMeta = semantics.metaData.find(letStmt);
  if (letMeta == semantics.metaData.end()) {
    reportDevBug("Could not find let statement metaData for '" + name + "'");
    return;
  }

  // Getting if it is heap allocated
  if (!letStmt->isHeap && !letStmt->isDheap) {
    compSize = 0;
    return;
  }

  // Getting the symbolInfo
  auto letSym = letMeta->second;

  // Get the type stub
  llvm::Type *letType = getLLVMType(letSym->type);

  compSize = layout->getTypeAllocSize(letType);
  compAlignment = layout->getABITypeAlign(letType);

  letSym->alignment = compAlignment;
  letSym->componentSize = compSize;

  totalHeapSize += compSize;
}

void Layout::calculateArrayStatementSize(Node *node) {
  auto arrStmt = dynamic_cast<ArrayStatement *>(node);
  if (!arrStmt)
    return;

  const std::string &name = arrStmt->identifier->expression.TokenLiteral;
  uint64_t totalElements = 0;
  uint64_t totalLiteralElements = 0;
  uint64_t totalByteSize = 0;

  logInternal("Calculating layout for array '" + name + "' ....");

  auto arrMeta = semantics.metaData.find(arrStmt);
  if (arrMeta == semantics.metaData.end()) {
    reportDevBug("Could not find array statement metaData for '" + name + "'");
    return;
  }

  auto arrSym = arrMeta->second;

  // Get the element type
  auto elementType = semantics.getArrayElementType(arrSym->type);
  auto elemLLVMty = getLLVMType(elementType);
  uint64_t elementSize = layout->getTypeAllocSize(elemLLVMty);
  llvm::Align elementAlign = layout->getABITypeAlign(elemLLVMty);

  // If the user provided the length
  bool isConstantLength = true;
  if (!arrStmt->lengths.empty()) {
    totalElements = 1; // Start at 1 for multiplication
    for (const auto &sizeNode : arrStmt->lengths) {
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

  // If the user gave a iteral
  if (arrStmt->array_content) {
    if (auto arrLit =
            dynamic_cast<ArrayLiteral *>(arrStmt->array_content.get())) {
      totalLiteralElements = countFlattenedElements(arrLit);
    } else if (auto nullLit =
                   dynamic_cast<NullLiteral *>(arrStmt->array_content.get())) {
      totalLiteralElements = totalElements;
    } else if (auto unwrap = dynamic_cast<UnwrapExpression *>(
                   arrStmt->array_content.get())) {
      uint64_t sourceByteSize =
          semantics.metaData[unwrap->expr.get()]->componentSize;

      totalLiteralElements = sourceByteSize / elementSize;
      logInternal("Total Unwrap array byte size :" +
                  std::to_string(sourceByteSize));

      logInternal("Total Array elements :" +
                  std::to_string(totalLiteralElements));
    } else {
      auto litMeta = semantics.metaData[arrStmt->array_content.get()];
      if (!litMeta) {
        reportDevBug("Could not find array literal metaData");
        return;
      }
      totalLiteralElements = litMeta->componentSize / elementSize;
      logInternal("Dynamic Array literal size: " +
                  std::to_string(totalLiteralElements));
    }

    // If the length was also added check compatibility
    if (!arrStmt->lengths.empty() && isConstantLength) {
      if (totalElements != totalLiteralElements) {
        logLayoutError("Size mismatch between expected size '" +
                           std::to_string(totalElements) +
                           "' elements and declared size '" +
                           std::to_string(totalLiteralElements) +
                           "' elements for array '" + name + "'",
                       arrStmt->identifier->expression.line,
                       arrStmt->identifier->expression.column);
      }
    } else {
      totalElements = totalLiteralElements;
    }
  }

  logInternal("Element type: '" + elementType.resolvedName +
              "'Element Size: " + std::to_string(elementSize));

  logInternal("Total Array Element size :" + std::to_string(totalElements));
  totalByteSize = totalElements * elementSize;
  logInternal("Total Array Byte size :" + std::to_string(totalByteSize) +
              " bytes");

  // Store results for the IR Generator
  arrSym->componentSize = totalByteSize;
  arrSym->alignment = elementAlign;

  //  Pool Addition
  if (arrSym->isHeap) {
    totalHeapSize += totalByteSize;
    logInternal("SAGE Array '" + name + "' -> Total Elements (" +
                std::to_string(totalElements) + ")*elementSize (" +
                std::to_string(elementSize) + ")" + " = " +
                std::to_string(totalByteSize) + " bytes");

    logInternal("Current Global SAGE Pool (totalHeapSize): " +
                std::to_string(totalHeapSize) + " bytes");
  } else if (arrSym->isDheap) {
    logInternal("Dheap Array '" + name + "' Calculated: " +
                std::to_string(totalByteSize) + " bytes (skipped pool)");
  }
}

void Layout::calculatePointerStatementSize(Node *node) {
  auto ptrStmt = dynamic_cast<PointerStatement *>(node);
  if (!ptrStmt)
    return;

  auto ptrMeta = semantics.metaData.find(ptrStmt);
  if (ptrMeta == semantics.metaData.end()) {
    reportDevBug("Failed to find pointer statement metaData");
    return;
  }

  auto ptrSym = ptrMeta->second;
  if (!ptrSym->isHeap && !ptrSym->isDheap)
    return;

  llvm::Type *ptrType = llvm::PointerType::get(context, 0);

  uint64_t compSize = layout->getTypeAllocSize(ptrType);
  llvm::Align compAlignment = layout->getABITypeAlign(ptrType);

  ptrSym->alignment = compAlignment;
  ptrSym->componentSize = compSize;

  totalHeapSize += compSize;
}

void Layout::calculateDheapStatementSize(Node *node) {
  auto dheapStmt = dynamic_cast<DheapStatement *>(node);
  if (!dheapStmt)
    return;

  // Just gonna call the driver on whatever is wrapped
  calculatorDriver(dheapStmt->stmt.get());
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

  auto line = instStmt->instantiate_token.line;
  auto col = instStmt->instantiate_token.column;

  // Extract the semantic metaData(It has the instantiation info)
  auto it = semantics.metaData.find(instStmt);
  if (it == semantics.metaData.end()) {
    reportDevBug("Failed to find instantiation statement metaData");
    return;
  }

  auto sym = it->second;

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
  auto line = recordStmt->recordName->expression.line;
  auto col = recordStmt->recordName->expression.column;

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
  auto line = compStmt->component_name->expression.line;
  auto col = compStmt->component_name->expression.column;

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
  for (const auto &method : compStmt->privateMethods) {
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

llvm::Type *Layout::getLLVMType(ResolvedType type) {
  llvm::Type *baseType = nullptr;

  switch (type.kind) {
  case DataType::I8: {
    baseType = llvm::Type::getInt8Ty(context);
    break;
  }
  case DataType::U8: {
    baseType = llvm::Type::getInt8Ty(context);
    break;
  }
  case DataType::I16: {
    baseType = llvm::Type::getInt16Ty(context);
    break;
  }
  case DataType::U16: {
    baseType = llvm::Type::getInt16Ty(context);
    break;
  }
  case DataType::I32: {
    baseType = llvm::Type::getInt32Ty(context);
    break;
  }
  case DataType::U32: {
    baseType = llvm::Type::getInt32Ty(context);
    break;
  }
  case DataType::I64: {
    baseType = llvm::Type::getInt64Ty(context);
    break;
  }
  case DataType::U64: {
    baseType = llvm::Type::getInt64Ty(context);
    break;
  }
  case DataType::I128: {
    baseType = llvm::Type::getInt128Ty(context);
    break;
  }
  case DataType::U128: {
    baseType = llvm::Type::getInt128Ty(context);
    break;
  }
  case DataType::ISIZE:
  case DataType::USIZE: {
    // We ask the module's DataLayout for the integer type that matches a
    // pointer. 'module' must be accessible here.
    baseType = tempModule->getDataLayout().getIntPtrType(context);
    break;
  }
  case DataType::BOOLEAN: {
    baseType = llvm::Type::getInt1Ty(context);
    break;
  }
  case DataType::CHAR8: {
    baseType = llvm::Type::getInt8Ty(context);
    break;
  }
  case DataType::CHAR16: {
    baseType = llvm::Type::getInt16Ty(context);
    break;
  }
  case DataType::CHAR32: {
    baseType = llvm::Type::getInt32Ty(context);
    break;
  }
  case DataType::F32: {
    baseType = llvm::Type::getFloatTy(context);
    break;
  }
  case DataType::F64: {
    baseType = llvm::Type::getDoubleTy(context);
    break;
  }

  case DataType::STRING: {
    baseType = llvm::PointerType::get(context, 0);
    break;
  }
  case DataType::VOID: {
    baseType = llvm::Type::getVoidTy(context);
    break;
  }
  case DataType::OPAQUE: {
    baseType = llvm::PointerType::get(context, 0);
    break;
  }

  case DataType::RECORD:
  case DataType::COMPONENT: {
    if (type.resolvedName.empty())
      throw std::runtime_error(
          "Custom type requested but resolvedName is empty");

    std::string lookUpName = type.resolvedName;
    if (type.isPointer)
      lookUpName = semantics.stripPtrSuffix(type.resolvedName);
    else if (type.isRef)
      lookUpName = semantics.stripRefSuffix(type.resolvedName);

    auto it = typeMap.find(lookUpName);
    if (it != typeMap.end())
      baseType = it->second;
    else
      throw std::runtime_error("Layout requested for unknown custom type '" +
                               lookUpName + "'");
    break;
  }

  case DataType::ENUM: {
    auto enumInfo = semantics.customTypesTable[type.resolvedName];
    baseType = getLLVMType({enumInfo->underLyingType, ""});
    break;
  }

  case DataType::ERROR:
  case DataType::GENERIC:
  case DataType::UNKNOWN:
    throw std::runtime_error(
        "Unsupported or unknown data type encountered in getLLVMType");
  }

  llvm::Type *finalType = baseType;

  if (type.isArray)
    finalType = llvm::PointerType::get(baseType, 0);

  if (type.kind != DataType::OPAQUE) {
    if (type.isPointer || type.isRef) {
      finalType = llvm::PointerType::get(baseType, 0);
    }
  }

  if (type.isNull) {
    // This creates { i1, ptr } or { i1, i32 }
    std::vector<llvm::Type *> fields = {
        llvm::Type::getInt1Ty(context), // Flag
        finalType                       // Payload (could be i32 OR ptr)
    };
    return llvm::StructType::get(context, fields);
  }

  return finalType;
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

void Layout::logLayoutError(const std::string &message, int line, int col) {
  hasFailed = true;

  CompilerError error;
  error.level = ErrorLevel::LAYOUT;
  error.line = line;
  error.col = col;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);
}

void Layout::reportDevBug(const std::string &message) {
  hasFailed = true;
  std::cerr << COLOR_RED << "[INTERNAL COMPILER ERROR]: " << COLOR_RESET
            << message << "\n";
}

void Layout::logInternal(const std::string &message) {
  if (verbose) {
    std::cout << message << "\n";
  }
}

bool Layout::failed() { return hasFailed; }
