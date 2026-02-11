#include "llvm/TargetParser/Host.h"
#include <llvm-18/llvm/IR/Constants.h>
#include <llvm-18/llvm/IR/DataLayout.h>
#include <llvm-18/llvm/IR/DerivedTypes.h>
#include <llvm-18/llvm/IR/Instruction.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <inttypes.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/CodeGen/TargetPassConfig.h>

#include "ast.hpp"
#include "errors.hpp"
#include "irgen.hpp"
#include "semantics.hpp"

#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

IRGenerator::IRGenerator(Semantics &semantics, ErrorHandler &handler,
                         size_t totalHeap, bool isVerbose)
    : semantics(semantics), errorHandler(handler), totalHeapSize(totalHeap),
      isVerbose(isVerbose), context(), funcBuilder(context),
      module(std::make_unique<llvm::Module>("unnameable", context)) {
  setupTargetLayout();

  registerGeneratorFunctions();
  registerExpressionGeneratorFunctions();
  registerAddressGeneratorFunctions();

  declareCustomTypes();
  declareImportedTypes();
  declareImportedSeals();
  registerAllocators();

  // Declare external allocator functions so IR can call them
  llvm::FunctionType *initType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),    // returns void
      {llvm::Type::getInt64Ty(context)}, // takes size_t (64-bit int)
      false);

  llvm::PointerType *i8PtrTy =
      llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);

  llvm::FunctionType *allocType = llvm::FunctionType::get(
      i8PtrTy, // returns void*
      {
          llvm::Type::getInt64Ty(context), // takes size_t for component size
          llvm::Type::getInt64Ty(context)  // takes size_t for alignment
      },
      false);

  llvm::FunctionType *destroyType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context), {}, false); // void()

  // Add them to the module
  module->getOrInsertFunction("sage_init", initType);
  module->getOrInsertFunction("sage_alloc", allocType);
  module->getOrInsertFunction("sage_destroy", destroyType);
}

// MAIN GENERATOR FUNCTION
void IRGenerator::generate(const std::vector<std::unique_ptr<Node>> &program) {
  // Generate program body
  for (const auto &node : program) {
    generateStatement(node.get());
  }
}

// MAIN GENERATOR FUNCTION FOR EXPRESSION
//  Main Expression generator helper function
llvm::Value *IRGenerator::generateExpression(Node *node) {
  if (!node) {
    throw std::runtime_error(
        "Null node sent to the expression generator dispatcher");
  }

  std::cout << "[DISPATCH] Node Type: " << typeid(*node).name()
            << " | Value: " << node->toString() << "\n";

  auto exprIt = expressionGeneratorsMap.find(typeid(*node));
  if (exprIt == expressionGeneratorsMap.end()) {
    reportDevBug("Could not find expression type IR generator for: " +
                     node->toString(),
                 node->token.line, node->token.column);
  }

  return (this->*exprIt->second)(node);
}

// Main L-value generator helper functions
llvm::Value *IRGenerator::generateAddress(Node *node) {
  if (!node) {
    throw std::runtime_error(
        "Null node sent to the address generator dispatcher");
  }

  auto addrIt = addressGeneratorsMap.find(typeid(*node));
  if (addrIt == addressGeneratorsMap.end()) {
    errorHandler.addHint(
        "The address generator(L-value) for this node does not exist "
        "in the address generator functions map");
    reportDevBug("Could not find address generator for " + node->toString(),
                 node->token.line, node->token.column);
  }

  return (this->*addrIt->second)(node);
}

// GENERATOR FUNCTIONS
void IRGenerator::generateStatement(Node *node) {
  auto generatorIt = generatorFunctionsMap.find(typeid(*node));
  if (generatorIt == generatorFunctionsMap.end()) {
    return;
  }
  (this->*generatorIt->second)(node);
}

// Expression statement IR generator function
void IRGenerator::generateExpressionStatement(Node *node) {
  auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
  if (!exprStmt) {
    throw std::runtime_error("Invalid expression statement node");
  }
  generateExpression(exprStmt->expression.get());
}

// Assignment statement IR generator function
void IRGenerator::generateAssignmentStatement(Node *node) {
  auto *assignStmt = dynamic_cast<AssignmentStatement *>(node);
  if (!assignStmt)
    return;

  if (isGlobalScope) {
    errorHandler.addHint(
        "Semantics failed to guard an assignment in the global scope");
    reportDevBug("Executable statements are not allowed at global scope",
                 assignStmt->statement.line, assignStmt->statement.column);
  }

  // targetPtr is the STACK location of the variable (e.g., the slot holding the
  // ptr to y's heap)
  llvm::Value *targetPtr = generateAddress(assignStmt->identifier.get());
  if (!targetPtr) {
    reportDevBug("Failed to get L-Value address for the LHS expression",
                 assignStmt->identifier->expression.line,
                 assignStmt->identifier->expression.column);
  }

  llvm::Value *initValue = nullptr;
  llvm::Value *activeGhost = nullptr;
  auto valSym = semantics.getSymbolFromMeta(assignStmt->value.get());

  // RHS Evaluation & Ghost Escrow Deployment
  if (valSym->isHeap) {
    Node *valHolder = semantics.queryForLifeTimeBaton(valSym->ID);
    if (valHolder == assignStmt->value.get()) {
      logInternal(
          "The RHS of the assignment is holding its baton - Deploying Ghost");
      activeGhost = allocateDynamicHeapStorage(valSym, "ghost_escrow");
      this->ghostStorage = activeGhost;
      initValue = generateExpression(assignStmt->value.get());
      this->ghostStorage = nullptr;
    } else {
      initValue = generateExpression(assignStmt->value.get());
    }
  } else {
    initValue = generateExpression(assignStmt->value.get());
  }

  if (!initValue) {
    reportDevBug("Failed to generate R-Value for assignment",
                 assignStmt->value->expression.line,
                 assignStmt->value->expression.column);
  }

  auto assignMeta = semantics.metaData[assignStmt];
  if (!assignMeta) {
    reportDevBug("Failed to get assignment statement metaData",
                 assignStmt->statement.line, assignStmt->statement.column);
  }

  // Handling Move Expressions (Pointer Takeover)
  if (auto moveVal = dynamic_cast<MoveExpression *>(assignStmt->value.get())) {
    logInternal("Handling move assignment");
    // In a move, we DO overwrite the stack slot to point to the new memory
    funcBuilder.CreateStore(initValue, targetPtr);
    return;
  }

  // Handling Deep Copy (Value Migration)
  bool isHuge = (assignMeta->type.kind == DataType::COMPONENT ||
                 assignMeta->type.kind == DataType::RECORD) &&
                (!assignMeta->type.isPointer);

  if (assignMeta->type.isArray || isHuge) {
    uint64_t byteSize = assignMeta->componentSize;
    llvm::Type *elemLLVMTy =
        getLLVMType(assignMeta->type.isArray
                        ? semantics.getArrayElementType(assignMeta->type)
                        : assignMeta->type);
    llvm::Align align = layout->getABITypeAlign(elemLLVMTy);

    funcBuilder.CreateMemCpy(targetPtr, align, initValue, align,
                             funcBuilder.getInt64(byteSize));
  } else {
    // SCALAR CASE
    if (assignMeta->storage == StorageType::HEAP) {
      llvm::Value *actualData = funcBuilder.CreateLoad(
          getLLVMType(assignMeta->type), initValue, "snatched_val");

      // Store the value into y's existing heap space
      funcBuilder.CreateStore(actualData, targetPtr);
    } else {
      // Standard stack-to-stack assignment
      funcBuilder.CreateStore(initValue, targetPtr);
    }
  }

  // Ghost Decommissioning
  if (activeGhost) {
    freeDynamicHeapStorage(valSym->allocType, activeGhost);
    logInternal("Ghost Box safely settled and destroyed");
  }

  logInternal("Ended assignment generation");
}
void IRGenerator::generateFieldAssignmentStatement(Node *node) {
  auto *fieldStmt = dynamic_cast<FieldAssignment *>(node);

  llvm::Value *lhsAddress = generateInfixAddress(fieldStmt->lhs_chain.get());

  llvm::Value *rhsVal = generateExpression(fieldStmt->value.get());

  auto meta = semantics.metaData[fieldStmt];
  if (meta->type.isArray) {
    auto elementType = semantics.getArrayElementType(meta->type);
    llvm::Type *elemLLVMType = getLLVMType(elementType);
    llvm::Align align = layout->getABITypeAlign(elemLLVMType);

    llvm::Value *destSlab = funcBuilder.CreateLoad(
        funcBuilder.getPtrTy(), lhsAddress, "field_slab_ptr");

    funcBuilder.CreateMemCpy(destSlab, align, rhsVal, align,
                             funcBuilder.getInt64(meta->componentSize));
  } else {
    funcBuilder.CreateStore(rhsVal, lhsAddress);
  }

  // A free was happening here pause it as I study the system
  /*{for (auto pendingFree : lhs.pendingFrees)
      funcBuilder.Insert(pendingFree); // This finally puts the free into the
     IR}*/
}

void IRGenerator::generateBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt) {
    reportDevBug("Invalid block statement", node->token.line,
                 node->token.column);
  }

  logInternal("Generating block statement with '" +
              std::to_string(blockStmt->statements.size()) + "' statements");
  for (const auto &stmt : blockStmt->statements) {
    auto currentBlock = funcBuilder.GetInsertBlock();
    if (currentBlock && currentBlock->getTerminator()) {
      break;
    }

    generateStatement(stmt.get());

    currentBlock = funcBuilder.GetInsertBlock();
    if (currentBlock && currentBlock->getTerminator()) {
      break;
    }
  }
}

void IRGenerator::generateTraceStatement(Node *node) {
  auto traceStmt = dynamic_cast<TraceStatement *>(node);

  if (!traceStmt) {
    reportDevBug("Invalid trace statement node", node->token.line,
                 node->token.column);
  }

  if (isGlobalScope) {
    errorHandler.addHint("Semantics failed to guard against this");
    reportDevBug("Executable statements must not be in global scope",
                 traceStmt->trace_keyword.line,
                 traceStmt->trace_keyword.column);
  }

  // Getting the semantic type of the val
  auto it = semantics.metaData.find(traceStmt->expr.get());
  if (it == semantics.metaData.end()) {
    errorHandler.addHint(
        "Semantics failed to register metadata for the trace expression");
    reportDevBug("Missing metadata for shout expression",
                 traceStmt->expr->expression.line,
                 traceStmt->expr->expression.column);
  }

  // Getting the symbol
  auto exprSym = it->second;
  if (!exprSym) {
    errorHandler.addHint("The semantics registered the metadata but didnt "
                         "provide the symbol info");
    reportDevBug("No symbol information was found for the trace expression",
                 traceStmt->expr->expression.line,
                 traceStmt->expr->expression.column);
  }

  // Getting the type
  ResolvedType type = exprSym->type;

  // Call the expression generation n the expression
  auto val = generateExpression(traceStmt->expr.get());
  if (!val) {
    errorHandler.addHint("The expression generator failed");
    reportDevBug("Failed to get value for the trace expression",
                 traceStmt->expr->expression.line,
                 traceStmt->expr->expression.column);
  }

  // Call the traceRuntime this is the one who actually prints
  traceRuntime(val, type);
  // Emit the free if we were holding some memory hostage and this is the
  // lastUse
  emitCleanup(traceStmt, exprSym);
}

llvm::Value *IRGenerator::generateBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr) {
    errorHandler.addHint("Sent a wrong node to the block expression generator "
                         "could be a malformed AST");
    reportDevBug("Invalid block expression", node->token.line,
                 node->token.column);
  }

  for (const auto &stmts : blockExpr->statements) {
    // Check if the current block is already terminated by a return or branch
    if (funcBuilder.GetInsertBlock()->getTerminator()) {
      logInternal("Skipping statement- block terminated");
      break;
    }
    generateStatement(stmts.get());
  }

  // A block expression should not return an llvm::Value directly.
  return nullptr;
}

// HELPER FUNCTIONS
llvm::Type *IRGenerator::getLLVMType(ResolvedType type) {
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
    baseType = module->getDataLayout().getIntPtrType(context);
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
    if (type.resolvedName.empty()) {
      errorHandler.addHint(
          "The semantics probably stored a type whose type name is empty");
      reportDevBug("Custom type requested but the resolved name is empty", 0,
                   0);
    }

    std::string lookUpName = type.resolvedName;
    if (type.isPointer) {
      lookUpName = semantics.stripPtrSuffix(lookUpName);
    } else {
      lookUpName = semantics.stripRefSuffix(lookUpName);
    }

    auto it = llvmCustomTypes.find(lookUpName);
    if (it != llvmCustomTypes.end())
      baseType = it->second;
    else {
      errorHandler.addHint(
          "The type '" + lookUpName +
          "' was not stoed in the custom type map used by the IRGenerator");
      reportDevBug(
          "IRgenerator requested for unknown type '" + lookUpName + "'", 0, 0);
    }
    break;
  }

  case DataType::ENUM: {
    auto enumInfo = semantics.customTypesTable[type.resolvedName];
    baseType = getLLVMType({enumInfo->underLyingType, ""});
    break;
  }

  case DataType::ERROR:
  case DataType::GENERIC:
  case DataType::UNKNOWN: {
    errorHandler.addHint(
        "Semantics failed to guard against unknown error leaks");
    reportDevBug("Type '" + type.resolvedName + "' is unknown", 0, 0);
  }
  }

  llvm::Type *finalType = baseType;

  if (type.isArray) {
    finalType = llvm::PointerType::get(baseType, 0);
  }

  // If the data type aint opaque (I dont want ptr ptr)
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

// Registering generator functions for statements
void IRGenerator::registerGeneratorFunctions() {
  generatorFunctionsMap[typeid(LetStatement)] =
      &IRGenerator::generateLetStatement;
  generatorFunctionsMap[typeid(HeapStatement)] =
      &IRGenerator::generateHeapStatement;
  generatorFunctionsMap[typeid(ReferenceStatement)] =
      &IRGenerator::generateReferenceStatement;
  generatorFunctionsMap[typeid(PointerStatement)] =
      &IRGenerator::generatePointerStatement;
  generatorFunctionsMap[typeid(ExpressionStatement)] =
      &IRGenerator::generateExpressionStatement;
  generatorFunctionsMap[typeid(AssignmentStatement)] =
      &IRGenerator::generateAssignmentStatement;
  generatorFunctionsMap[typeid(FieldAssignment)] =
      &IRGenerator::generateFieldAssignmentStatement;
  generatorFunctionsMap[typeid(WhileStatement)] =
      &IRGenerator::generateWhileStatement;
  generatorFunctionsMap[typeid(ForStatement)] =
      &IRGenerator::generateForStatement;
  generatorFunctionsMap[typeid(ifStatement)] =
      &IRGenerator::generateIfStatement;
  generatorFunctionsMap[typeid(SwitchStatement)] =
      &IRGenerator::generateSwitchStatement;
  generatorFunctionsMap[typeid(BreakStatement)] =
      &IRGenerator::generateBreakStatement;
  generatorFunctionsMap[typeid(ContinueStatement)] =
      &IRGenerator::generateContinueStatement;
  generatorFunctionsMap[typeid(BlockStatement)] =
      &IRGenerator::generateBlockStatement;
  generatorFunctionsMap[typeid(FunctionStatement)] =
      &IRGenerator::generateFunctionStatement;
  generatorFunctionsMap[typeid(ReturnStatement)] =
      &IRGenerator::generateReturnStatement;
  generatorFunctionsMap[typeid(FunctionDeclaration)] =
      &IRGenerator::generateFunctionDeclaration;
  // Special case
  generatorFunctionsMap[typeid(FunctionDeclarationExpression)] =
      &IRGenerator::generateFunctionDeclarationExpression;
  // Component system
  generatorFunctionsMap[typeid(RecordStatement)] =
      &IRGenerator::generateRecordStatement;
  generatorFunctionsMap[typeid(ComponentStatement)] =
      &IRGenerator::generateComponentStatement;
  generatorFunctionsMap[typeid(EnumStatement)] =
      &IRGenerator::generateEnumStatement;

  generatorFunctionsMap[typeid(ArrayStatement)] =
      &IRGenerator::generateArrayStatement;
  generatorFunctionsMap[typeid(TraceStatement)] =
      &IRGenerator::generateTraceStatement;
  generatorFunctionsMap[typeid(InstantiateStatement)] =
      &IRGenerator::generateInstantiateStatement;
  generatorFunctionsMap[typeid(SealStatement)] =
      &IRGenerator::generateSealStatement;
  generatorFunctionsMap[typeid(AllocatorStatement)] =
      &IRGenerator::generateAllocatorInterface;
}

void IRGenerator::registerAddressGeneratorFunctions() {
  addressGeneratorsMap[typeid(SelfExpression)] =
      &IRGenerator::generateSelfAddress;
  addressGeneratorsMap[typeid(CallExpression)] =
      &IRGenerator::generateCallAddress;
  addressGeneratorsMap[typeid(ArraySubscript)] =
      &IRGenerator::generateArraySubscriptAddress;
  addressGeneratorsMap[typeid(DereferenceExpression)] =
      &IRGenerator::generateDereferenceAddress;
  addressGeneratorsMap[typeid(Identifier)] =
      &IRGenerator::generateIdentifierAddress;
  addressGeneratorsMap[typeid(InfixExpression)] =
      &IRGenerator::generateInfixAddress;
}

void IRGenerator::registerExpressionGeneratorFunctions() {
  expressionGeneratorsMap[typeid(InfixExpression)] =
      &IRGenerator::generateInfixExpression;
  expressionGeneratorsMap[typeid(PrefixExpression)] =
      &IRGenerator::generatePrefixExpression;
  expressionGeneratorsMap[typeid(PostfixExpression)] =
      &IRGenerator::generatePostfixExpression;
  expressionGeneratorsMap[typeid(StringLiteral)] =
      &IRGenerator::generateStringLiteral;
  expressionGeneratorsMap[typeid(Char8Literal)] =
      &IRGenerator::generateChar8Literal;
  expressionGeneratorsMap[typeid(Char16Literal)] =
      &IRGenerator::generateChar16Literal;
  expressionGeneratorsMap[typeid(Char32Literal)] =
      &IRGenerator::generateChar32Literal;
  expressionGeneratorsMap[typeid(BooleanLiteral)] =
      &IRGenerator::generateBooleanLiteral;
  expressionGeneratorsMap[typeid(I8Literal)] = &IRGenerator::generateI8Literal;
  expressionGeneratorsMap[typeid(U8Literal)] = &IRGenerator::generateU8Literal;
  expressionGeneratorsMap[typeid(I16Literal)] =
      &IRGenerator::generateI16Literal;
  expressionGeneratorsMap[typeid(U16Literal)] =
      &IRGenerator::generateU16Literal;
  expressionGeneratorsMap[typeid(I32Literal)] =
      &IRGenerator::generateI32Literal;
  expressionGeneratorsMap[typeid(U32Literal)] =
      &IRGenerator::generateU32Literal;
  expressionGeneratorsMap[typeid(I64Literal)] =
      &IRGenerator::generateI64Literal;
  expressionGeneratorsMap[typeid(U64Literal)] =
      &IRGenerator::generateU64Literal;
  expressionGeneratorsMap[typeid(I128Literal)] =
      &IRGenerator::generateI128Literal;
  expressionGeneratorsMap[typeid(U128Literal)] =
      &IRGenerator::generateU128Literal;
  expressionGeneratorsMap[typeid(ISIZELiteral)] =
      &IRGenerator::generateISIZELiteral;
  expressionGeneratorsMap[typeid(USIZELiteral)] =
      &IRGenerator::generateUSIZELiteral;
  expressionGeneratorsMap[typeid(F32Literal)] =
      &IRGenerator::generateF32Literal;
  expressionGeneratorsMap[typeid(F64Literal)] =
      &IRGenerator::generateF64Literal;
  expressionGeneratorsMap[typeid(ArrayLiteral)] =
      &IRGenerator::generateArrayLiteral;
  expressionGeneratorsMap[typeid(NullLiteral)] =
      &IRGenerator::generateNullLiteral;
  expressionGeneratorsMap[typeid(Identifier)] =
      &IRGenerator::generateIdentifierExpression;
  expressionGeneratorsMap[typeid(SizeOfExpression)] =
      &IRGenerator::generateSizeOfExpression;
  expressionGeneratorsMap[typeid(CastExpression)] =
      &IRGenerator::generateCastExpression;
  expressionGeneratorsMap[typeid(BitcastExpression)] =
      &IRGenerator::generateBitcastExpression;
  expressionGeneratorsMap[typeid(AddressExpression)] =
      &IRGenerator::generateAddressExpression;
  expressionGeneratorsMap[typeid(DereferenceExpression)] =
      &IRGenerator::generateDereferenceExpression;
  expressionGeneratorsMap[typeid(MoveExpression)] =
      &IRGenerator::generateMoveExpression;
  expressionGeneratorsMap[typeid(BlockExpression)] =
      &IRGenerator::generateBlockExpression;
  expressionGeneratorsMap[typeid(CallExpression)] =
      &IRGenerator::generateCallExpression;
  expressionGeneratorsMap[typeid(UnwrapExpression)] =
      &IRGenerator::generateUnwrapExpression;
  expressionGeneratorsMap[typeid(MethodCallExpression)] =
      &IRGenerator::generateMethodCallExpression;
  expressionGeneratorsMap[typeid(SelfExpression)] =
      &IRGenerator::generateSelfExpression;
  expressionGeneratorsMap[typeid(InstanceExpression)] =
      &IRGenerator::generateInstanceExpression;
  expressionGeneratorsMap[typeid(ArraySubscript)] =
      &IRGenerator::generateArraySubscriptExpression;
}

char IRGenerator::decodeCharLiteral(const std::string &literal) {
  if (literal.length() == 3 && literal.front() == '\'' &&
      literal.back() == '\'') {
    return literal[1];
  } else if (literal.length() == 4 && literal.front() == '\'' &&
             literal.back() == '\'' && literal[1] == '\\') {
    switch (literal[2]) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case '\\':
      return '\\';
    case '\'':
      return '\'';
    case '\"':
      return '\"';
    case 'r':
      return '\r';
    case '0':
      return '\0';
    default: {
      reportDevBug("Unknown escape sequence in char literal '" + literal + "'",
                   0, 0);
    }
    }
  }

  reportDevBug("Invalid char literal '" + literal + "'", 0, 0);
  return '\t';
}

uint16_t IRGenerator::decodeChar16Literal(const std::string &literal) {
  // Example formats: 'A', '\n', '\u1234' (unicode escape)

  if (literal.length() == 3 && literal.front() == '\'' &&
      literal.back() == '\'') {
    return static_cast<uint16_t>(literal[1]);
  } else if (literal.length() == 8 && literal.substr(0, 2) == "'\\u" &&
             literal.back() == '\'') {
    std::string hex = literal.substr(3, 4); // 4 hex digits
    return static_cast<uint16_t>(std::stoi(hex, nullptr, 16));
  } else if (literal.length() == 4 && literal.front() == '\'' &&
             literal.back() == '\'' && literal[1] == '\\') {
    // Simple escapes like '\n'
    switch (literal[2]) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case '\\':
      return '\\';
    case '\'':
      return '\'';
    case '\"':
      return '\"';
    case 'r':
      return '\r';
    case '0':
      return '\0';
    default:
      throw std::runtime_error("Unknown escape sequence in char16 literal: " +
                               literal);
    }
  }
  throw std::runtime_error("Invalid char16 literal: " + literal);
}

// Decodes UTF-32 char32 literals (returns uint32_t)
uint32_t IRGenerator::decodeChar32Literal(const std::string &literal) {
  // Example formats: 'A', '\U0001F600' (unicode escape for emoji)

  if (literal.length() == 3 && literal.front() == '\'' &&
      literal.back() == '\'') {
    return static_cast<uint32_t>(literal[1]);
  } else if (literal.length() == 12 && literal.substr(0, 2) == "'\\U" &&
             literal.back() == '\'') {
    std::string hex = literal.substr(3, 8); // 8 hex digits
    return static_cast<uint32_t>(std::stoul(hex, nullptr, 16));
  } else if (literal.length() == 4 && literal.front() == '\'' &&
             literal.back() == '\'' && literal[1] == '\\') {
    // Simple escapes like '\n'
    switch (literal[2]) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case '\\':
      return '\\';
    case '\'':
      return '\'';
    case '\"':
      return '\"';
    case 'r':
      return '\r';
    case '0':
      return '\0';
    default:
      throw std::runtime_error("Unknown escape sequence in char32 literal: " +
                               literal);
    }
  }
  throw std::runtime_error("Invalid char32 literal: " + literal);
}

llvm::Value *IRGenerator::coerceToBoolean(llvm::Value *val, Node *exprNode) {
  if (!val)
    return nullptr;

  auto it = semantics.metaData.find(exprNode);
  if (it == semantics.metaData.end()) {
    errorHandler.addHint("Semantics did not register the condition metadata");
    reportDevBug("Could not find condition expression metadata",
                 exprNode->token.line, exprNode->token.column);
    return nullptr;
  }

  auto sym = it->second;

  // Handle Nullable Boxes {i1, T}
  if (sym->isNullable) {
    // If we have a pointer to the box (from an alloca or global), load the
    // struct first
    if (val->getType()->isPointerTy()) {
      // Check if it's a pointer to a struct, not just any pointer
      llvm::Type *boxType = getLLVMType(sym->type);
      val = funcBuilder.CreateLoad(boxType, val, "nullable.load");
    }
    // The boolean 'is_present' is always at index 0
    return funcBuilder.CreateExtractValue(val, {0}, "is_present");
  }

  // Handle Booleans (i1)
  if (val->getType()->isIntegerTy(1)) {
    return val;
  }

  // Handle Raw Pointers
  if (val->getType()->isPointerTy()) {
    // Pointers are compared against the null pointer constant
    llvm::Value *nullPtr = llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(val->getType()));
    return funcBuilder.CreateICmpNE(val, nullPtr, "ptr.not_null");
  }

  // Handle Scalars (i8, i32, i64, etc.)
  if (val->getType()->isIntegerTy()) {
    return funcBuilder.CreateICmpNE(
        val, llvm::ConstantInt::get(val->getType(), 0), "coerce.bool");
  }

  reportDevBug("Attempted to coerce a non-nullable aggregate to boolean",
               exprNode->token.line, exprNode->token.column);
  return funcBuilder.getInt1(false);
}
bool IRGenerator::isIntegerType(DataType dt) {
  switch (dt) {
  case DataType::I8:
  case DataType::U8:
  case DataType::I16:
  case DataType::U16:
  case DataType::I32:
  case DataType::U32:
  case DataType::I64:
  case DataType::U64:
  case DataType::I128:
  case DataType::U128:
  case DataType::ISIZE:
  case DataType::USIZE:
    return true;
  default:
    return false;
  }
}

bool IRGenerator::isSignedInteger(DataType dt) {
  switch (dt) {
  case DataType::I8:
  case DataType::I16:
  case DataType::I32:
  case DataType::I64:
  case DataType::I128:
  case DataType::ISIZE:
    return true;
  default:
    return false;
  }
}

bool IRGenerator::isUnsigned(const ResolvedType &type) {
  auto dt = type.kind;
  switch (dt) {
  case DataType::U8:
  case DataType::U16:
  case DataType::U32:
  case DataType::U64:
  case DataType::U128:
  case DataType::USIZE:
    return true;
  default:
    return false;
  }
}

unsigned IRGenerator::getIntegerBitWidth(DataType dt) {
  switch (dt) {
  case DataType::I8:
  case DataType::U8:
    return 8;
  case DataType::I16:
  case DataType::U16:
    return 16;
  case DataType::I32:
  case DataType::U32:
    return 32;
  case DataType::I64:
  case DataType::U64:
    return 64;
  case DataType::I128:
  case DataType::U128:
    return 128;
  default:
    return 0; // Not an integer type
  }
}

char *IRGenerator::const_unnitoa(__int128 val, char *buf) {
  char temp[64]; // Enough for 128-bit decimal digits (max ~39 digits).
  int i = 0;
  int neg = 0;

  if (val == 0) {
    buf[0] = '0';
    buf[1] = '\0';
    return buf;
  }

  if (val < 0) {
    neg = 1;
    val = -val;
  }

  // Extract digits into temp (reversed)
  while (val > 0) {
    __int128 digit = val % 10;
    val /= 10;
    temp[i++] = '0' + (int)digit;
  }

  if (neg)
    temp[i++] = '-';

  // Reverse into buf
  int j = 0;
  while (i > 0)
    buf[j++] = temp[--i];

  buf[j] = '\0';
  return buf;
}

void IRGenerator::traceRuntime(llvm::Value *val, ResolvedType type) {
  if (!val) {
    reportDevBug("shout! called with a null value", 0, 0);
  }

  auto &ctx = module->getContext();
  auto i32Ty = llvm::IntegerType::getInt32Ty(ctx);
  auto i8Ty = llvm::IntegerType::getInt8Ty(ctx);
  auto i128Ty = llvm::IntegerType::get(module->getContext(), 128);
  auto i8PtrTy = llvm::PointerType::get(i8Ty, 0);
  auto i64Ty = llvm::IntegerType::getInt64Ty(ctx);

  llvm::Function *unnitoaFn = module->getFunction("unnitoa");
  if (!unnitoaFn) {
    llvm::FunctionType *unnitoaTy = llvm::FunctionType::get(
        i8PtrTy,           // returns char*
        {i128Ty, i8PtrTy}, // (__int128 value, char* buffer)
        false);

    unnitoaFn = llvm::Function::Create(
        unnitoaTy, llvm::GlobalValue::ExternalLinkage, "unnitoa", *module);
  }

  auto printString = [&](llvm::Value *strVal) {
    if (!strVal) {
      reportDevBug("printString received value", 0, 0);
    }

    // Ensure 'write' exists
    llvm::Function *writeFn = module->getFunction("write");
    if (!writeFn) {
      auto writeType =
          llvm::FunctionType::get(i64Ty, {i32Ty, i8PtrTy, i64Ty}, false);
      writeFn = llvm::Function::Create(
          writeType, llvm::Function::ExternalLinkage, "write", module.get());
    }

    // Ensure 'strlen' exists
    llvm::Function *strlenFn = module->getFunction("strlen");
    if (!strlenFn) {
      auto strlenType = llvm::FunctionType::get(i64Ty, {i8PtrTy}, false);
      strlenFn = llvm::Function::Create(
          strlenType, llvm::Function::ExternalLinkage, "strlen", module.get());
    }

    auto fd = llvm::ConstantInt::get(i32Ty, 1); // stdout

    // print main string
    auto len = funcBuilder.CreateCall(strlenFn, {strVal});
    funcBuilder.CreateCall(writeFn, {fd, strVal, len});

    // print newline
    llvm::Value *newline = funcBuilder.CreateGlobalStringPtr("\n");
    auto newLen = funcBuilder.CreateCall(strlenFn, {newline});
    funcBuilder.CreateCall(writeFn, {fd, newline, newLen});
  };

  auto printInt = [&](llvm::Value *intVal) {
    char buf[20];

    if (auto constInt = llvm::dyn_cast<llvm::ConstantInt>(intVal)) {
      // Compile-time constant
      const_unnitoa(static_cast<int>(constInt->getSExtValue()), buf);
      llvm::Value *strVal = funcBuilder.CreateGlobalStringPtr(buf);
      printString(strVal);
    } else if (intVal->getType()->isIntegerTy(32)) {
      logInternal("Branching to SSA print");
      // Promote to i128 for printing

      llvm::Value *int128 =
          funcBuilder.CreateIntCast(intVal, i128Ty, /*isSigned=*/true);

      // Allocate buffer
      llvm::Value *bufAlloca = funcBuilder.CreateAlloca(
          llvm::ArrayType::get(i8Ty, 64), nullptr, "int_buf");
      llvm::Value *bufPtr = funcBuilder.CreatePointerCast(bufAlloca, i8PtrTy);

      // Call the new universal printer
      funcBuilder.CreateCall(unnitoaFn, {int128, bufPtr});
      printString(bufPtr);
    }

    else {
      reportDevBug("Unsupported integer value in shout! ", 0, 0);
    }
  };

  auto printPointer = [&](llvm::Value *ptrVal) {
    llvm::Value *addrInt = funcBuilder.CreatePtrToInt(ptrVal, i128Ty);

    llvm::Value *bufAlloca = funcBuilder.CreateAlloca(
        llvm::ArrayType::get(i8Ty, 64), nullptr, "ptr_buf");
    llvm::Value *bufPtr = funcBuilder.CreatePointerCast(bufAlloca, i8PtrTy);

    funcBuilder.CreateCall(unnitoaFn, {addrInt, bufPtr});
    printString(bufPtr);
  };

  // If the expression is a pointer
  if (type.isPointer) {
    printPointer(val);
    return;
  }

  if (type.kind == DataType::I32) {
    printInt(val);
  } else if (type.kind == DataType::STRING) {
    printString(val);
  } else {
    throw std::runtime_error("shout! only supports i32 and string for now");
  }
}

void IRGenerator::freeDynamicHeapStorage(const std::string &allocatorType,
                                         llvm::Value *toFree) {
  auto it = semantics.allocatorMap.find(allocatorType);
  if (it == semantics.allocatorMap.end())
    return;

  auto deallocatorName = it->second.freeName;
  llvm::Function *deallocFunc = module->getFunction(deallocatorName);

  llvm::Value *castPtr = funcBuilder.CreatePointerCast(
      toFree, deallocFunc->getFunctionType()->getParamType(0));

  funcBuilder.CreateCall(deallocFunc, {castPtr});
}

void IRGenerator::executePhysicalFree(const std::shared_ptr<SymbolInfo> &sym) {
  if (!sym || !sym->llvmValue)
    return;

  logInternal("  [Metal] Generating  deallocation for: " + sym->ID);

  auto it = semantics.allocatorMap.find(sym->allocType);
  if (it == semantics.allocatorMap.end())
    return;

  auto deallocatorName = it->second.freeName;
  llvm::Function *deallocFunc = module->getFunction(deallocatorName);

  // The Assembly-level logic
  if (!sym->llvmValue)
    return;

  llvm::Value *castPtr = funcBuilder.CreatePointerCast(
      sym->llvmValue, deallocFunc->getFunctionType()->getParamType(0));

  funcBuilder.CreateCall(deallocFunc, {castPtr});
}

llvm::Value *IRGenerator::handleSnatchedMove(llvm::Value *sourceAddr,
                                             std::shared_ptr<SymbolInfo> sym,
                                             Node *node) {
  logInternal("Executing Snatched Move Helper");

  llvm::Align align = layout->getABITypeAlign(sym->llvmType);
  uint64_t byteSize = sym->componentSize;

  // Move from source to the established Escrow
  funcBuilder.CreateMemCpy(ghostStorage, align, sourceAddr, align,
                           funcBuilder.getInt64(byteSize));

  // The source dies now. Its duty is done.
  emitCleanup(node, sym);

  // Return the escrow address so the assignment knows where the data is
  return ghostStorage;
}

void IRGenerator::emitCleanup(Node *contextNode,
                              const std::shared_ptr<SymbolInfo> &contextSym) {
  auto it = semantics.responsibilityTable.find(contextNode);
  if (it == semantics.responsibilityTable.end())
    return;

  auto &baton = it->second;
  if (!baton || !baton->isResponsible)
    return;

  // Trigger Logic (Counts)
  if (contextSym->pointerCount == 0 && contextSym->refCount == 0) {
    // Free the depenedents first
    logInternal("Freeing '" + std::to_string(baton->dependents.size()) +
                "' dependents...");
    for (const auto &[id, depSym] : baton->dependents) {
      executePhysicalFree(depSym);
    }

    // Free the leader
    if (contextSym->isHeap) {
      logInternal("  [Leader] deallocation for: " + contextSym->ID);
      executePhysicalFree(contextSym);
    }

    // Defuse to avoid issues
    baton->isResponsible = false;
  }
}

llvm::Value *IRGenerator::generateIntegerLiteral(const std::string &literalStr,
                                                 uint32_t bitWidth,
                                                 bool isSigned) {
  int base = 10;
  std::string cleanStr = literalStr;
  if (literalStr.size() > 2) {
    if (literalStr[1] == 'x' || literalStr[1] == 'X') {
      base = 16;
      cleanStr = literalStr.substr(2);
    } else if (literalStr[1] == 'b' || literalStr[1] == 'B') {
      base = 2;
      cleanStr = literalStr.substr(2);
    }
  }

  // Create a WIDE APInt to hold the value safely during the check.
  // Using a 129 bits so a 128-bit number can't "accidentally" overflow it.
  llvm::APInt wideVal(129, cleanStr, base);

  // Perform the Range Check
  if (isSigned) {
    // Create the min/max bounds for the target bitWidth
    llvm::APInt minBound = llvm::APInt::getSignedMinValue(bitWidth).sext(129);
    llvm::APInt maxBound = llvm::APInt::getSignedMaxValue(bitWidth).sext(129);

    // slt = Signed Less Than, sgt = Signed Greater Than
    if (wideVal.slt(minBound) || wideVal.sgt(maxBound)) {
      throw std::runtime_error("Overflow: Value out of range for signed i" +
                               std::to_string(bitWidth));
    }
  } else {
    llvm::APInt maxBound = llvm::APInt::getMaxValue(bitWidth).zext(129);

    // ult = Unsigned Less Than, ugt = Unsigned Greater Than
    if (wideVal.ugt(maxBound)) {
      throw std::runtime_error("Overflow: Value out of range for unsigned u" +
                               std::to_string(bitWidth));
    }
  }

  // Shrink the wide value down to the actual target size
  return llvm::ConstantInt::get(context, wideVal.trunc(bitWidth));
}

llvm::GlobalVariable *
IRGenerator::createGlobalArrayConstant(llvm::Constant *constantArray) {
  // The type of the constant must be a valid ArrayType (which can be nested)
  llvm::Type *constantType = constantArray->getType();

  llvm::ArrayType *arrayTy =
      llvm::cast<llvm::ArrayType>(constantArray->getType());

  if (!arrayTy) {
    // If the constant is NOT an ArrayType, something is fundamentally wrong
    // with the output of generateArrayLiteral.
    throw std::runtime_error(
        "Attempted to create a global array constant from a non-array type.");
  }

  // 2. Create the Global Variable
  llvm::GlobalVariable *globalArray =
      new llvm::GlobalVariable(*module, // The owning module
                               arrayTy, // The type of the global variable
                               true,    // IsConstant (Read-only)
                               llvm::GlobalValue::PrivateLinkage, // Linkage
                               constantArray, // The initializer constant value
                               "array.init"   // Name
      );
  // Set alignment for safety
  globalArray->setAlignment(
      llvm::MaybeAlign(arrayTy->getPrimitiveSizeInBits() / 8));
  return globalArray;
}

void IRGenerator::dumpIR() { module->print(llvm::outs(), nullptr); }

bool IRGenerator::currentBlockIsTerminated() {
  llvm::BasicBlock *bb = funcBuilder.GetInsertBlock();
  return bb && bb->getTerminator();
}

llvm::Module &IRGenerator::getLLVMModule() { return *module; }

void IRGenerator::generateSageInitCall() {
  // Safety check just in case
  if (isSageInitCalled)
    return;

  llvm::FunctionType *funcType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);

  // --- Call sage_init upfront ---
  llvm::Function *initFunc = module->getFunction("sage_init");
  if (!initFunc) {
    llvm::FunctionType *initType =
        llvm::FunctionType::get(llvm::Type::getVoidTy(context),
                                {llvm::Type::getInt64Ty(context)}, false);

    initFunc = llvm::Function::Create(initType, llvm::Function::ExternalLinkage,
                                      "sage_init", module.get());
  }

  funcBuilder.CreateCall(
      initFunc,
      {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), totalHeapSize)});
  logInternal("Initializing sage");
  isSageInitCalled =
      true; // Toggle this to true to mark that sage init was called
}

void IRGenerator::generateSageDestroyCall() {
  llvm::Function *destroyFunc = module->getFunction("sage_destroy");
  if (!destroyFunc) {
    llvm::FunctionType *destroyType =
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false);

    destroyFunc =
        llvm::Function::Create(destroyType, llvm::Function::ExternalLinkage,
                               "sage_destroy", module.get());
  }

  funcBuilder.CreateCall(destroyFunc, {});
  logInternal("Destroying sage");
  isSageDestroyCalled =
      true; // Toggle this to true to mark that sage destroy was called
}

void IRGenerator::setupTargetLayout() {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();

  std::string targetTripleStr = llvm::sys::getDefaultTargetTriple();
  module->setTargetTriple(targetTripleStr);

  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);
  if (!target)
    throw std::runtime_error("Target not found");

  llvm::TargetOptions opt;
  auto targetMachine = target->createTargetMachine(targetTripleStr, "generic",
                                                   "", opt, llvm::Reloc::PIC_);

  module->setDataLayout(targetMachine->createDataLayout());
  this->layout = &module->getDataLayout();
}

bool IRGenerator::emitObjectFile(const std::string &filename) {
  // Initialization
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  // Target Setup
  std::string targetTripleStr = llvm::sys::getDefaultTargetTriple();

  module->setTargetTriple(targetTripleStr);

  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);
  if (!target) {
    llvm::errs() << "Failed to find target: " << error << "\n";
    return false;
  }

  llvm::TargetOptions opt;

  std::optional<llvm::Reloc::Model> RM = llvm::Reloc::PIC_;

  auto targetMachine = target->createTargetMachine(targetTripleStr,
                                                   "generic", // CPU name
                                                   "",        // Features string
                                                   opt, RM);

  if (!targetMachine) {
    llvm::errs() << "Failed to create TargetMachine\n";
    return false;
  }

  std::error_code EC;

  const auto FileType = llvm::CodeGenFileType::ObjectFile;

  llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);
  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message() << "\n";
    return false;
  }

  llvm::legacy::PassManager pass;

  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    llvm::errs() << "TargetMachine can't emit object file\n";
    return false;
  }

  pass.run(*module);
  dest.flush();

  return true;
}

void IRGenerator::reportDevBug(const std::string &message, int line, int col) {
  CompilerError error;

  error.level = ErrorLevel::INTERNAL;
  error.line = line;
  error.col = col;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);

  std::abort();
}

void IRGenerator::logInternal(const std::string &message) {
  if (isVerbose) {
    std::cout << message << "\n";
  }
}
