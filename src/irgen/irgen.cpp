#include "llvm/TargetParser/Host.h"
#include <llvm-18/llvm/IR/Constants.h>
#include <llvm-18/llvm/IR/DataLayout.h>
#include <llvm-18/llvm/IR/DerivedTypes.h>
#include <llvm-18/llvm/IR/Instruction.h>
#include <llvm-18/llvm/IR/Type.h>
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
#include "audit.hpp"
#include "errors.hpp"
#include "irgen.hpp"
#include "semantics.hpp"

#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

IRGenerator::IRGenerator(Semantics &semantics, ErrorHandler &handler,
                         Auditor &auditor, size_t totalHeap, bool isVerbose,
                         OptLevel optLevel)
    : optLevel(optLevel), context(), funcBuilder(context),
      module(std::make_unique<llvm::Module>("unnameable", context)),
      semantics(semantics), errorHandler(handler), auditor(auditor),
      totalHeapSize(totalHeap), isVerbose(isVerbose) {
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
  auto exprIt = expressionGeneratorsMap.find(typeid(*node));
  if (exprIt == expressionGeneratorsMap.end()) {
    reportDevBug("Could not find expression type IR generator for: " +
                     node->toString(),
                 node);
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
                 node);
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

void IRGenerator::generatePointerAssignmentStatement(
    AssignmentStatement *assignStmt) {
  auto assignSym = semantics.getSymbolFromMeta(assignStmt);
  if (!assignSym)
    return;

  logInternal("Inside the pointer reassignment");
  inhibitCleanUp = true;
  llvm::Value *targetPtr = generateAddress(assignStmt->identifier.get());
  llvm::Value *newAddr = generateExpression(assignStmt->value.get());

  auto *storeInst = funcBuilder.CreateStore(newAddr, targetPtr);
  if (assignSym->isVolatile) {
    storeInst->setVolatile(true);
  }
  inhibitCleanUp = false;

  emitCleanup(assignStmt);
}

// Assignment statement IR generator function
void IRGenerator::generateAssignmentStatement(Node *node) {
  auto *assignStmt = dynamic_cast<AssignmentStatement *>(node);
  if (!assignStmt)
    return;

  auto assignSym = semantics.getSymbolFromMeta(assignStmt);

  if (assignSym->isPointer) {
    generatePointerAssignmentStatement(assignStmt);
    return;
  }

  inhibitCleanUp = true; // Set the inhibitor to block the child from triggering
                         // a free before we use the value
  // targetPtr is the STACK location of the variable (e.g., the slot holding the
  // ptr to y's heap)

  llvm::Value *targetPtr = generateAddress(assignStmt->identifier.get());
  if (!targetPtr) {
    reportDevBug("Failed to get L-Value address for the LHS expression",
                 assignStmt);
  }

  llvm::Value *initValue = nullptr;
  auto valSym = semantics.getSymbolFromMeta(assignStmt->value.get());

  // RHS Evaluation
  initValue = generateExpression(assignStmt->value.get());

  if (!initValue) {
    reportDevBug("Failed to generate R-Value for assignment", assignStmt);
  }

  auto assignMeta = semantics.metaData[assignStmt];
  if (!assignMeta) {
    reportDevBug("Failed to get assignment statement metaData", assignStmt);
  }

  // Handling Move Expressions (Pointer Takeover)
  if (dynamic_cast<MoveExpression *>(assignStmt->value.get())) {
    logInternal("Handling move assignment");
    // In a move, we DO overwrite the stack slot to point to the new memory
    auto *storeInst = funcBuilder.CreateStore(initValue, targetPtr);
    if (assignSym->isVolatile) {
      storeInst->setVolatile(true);
    }
    inhibitCleanUp = false;

    emitCleanup(assignStmt);
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
    // MemCpy is not a volatile operation
  } else {
    // SCALAR CASE
    llvm::StoreInst *storeInst = nullptr;

    if (assignMeta->isHeap) {
      llvm::Value *valueToStore = initValue;
      if (valSym->isAddress) {
        valueToStore = funcBuilder.CreateLoad(getLLVMType(assignMeta->type),
                                              initValue, "loaded_val");
        // The load itself might need to be volatile if reading from volatile
        if (valSym->isVolatile) {
          if (auto *load = llvm::dyn_cast<llvm::LoadInst>(valueToStore)) {
            load->setVolatile(true);
          }
        }
      }
      // Store the value into y's existing heap space
      storeInst = funcBuilder.CreateStore(valueToStore, targetPtr);
    } else {
      // Standard stack-to-stack assignment
      storeInst = funcBuilder.CreateStore(initValue, targetPtr);
    }

    // Mark store as volatile if LHS is volatile
    if (assignSym->isVolatile && storeInst) {
      storeInst->setVolatile(true);
    }
  }

  inhibitCleanUp = false;
  emitCleanup(assignStmt);
  logInternal("Ended assignment generation");
}

void IRGenerator::generateFieldAssignmentStatement(Node *node) {
  auto *fieldStmt = dynamic_cast<FieldAssignment *>(node);
  if (!fieldStmt)
    return;

  // Get all the symbols we need
  auto fieldSym = semantics.getSymbolFromMeta(fieldStmt);
  auto lhsSym = semantics.getSymbolFromMeta(fieldStmt->lhs_chain.get());
  auto valSym = semantics.getSymbolFromMeta(fieldStmt->value.get());

  if (!fieldSym || !lhsSym || !valSym) {
    reportDevBug("Missing symbols in field assignment", node);
    return;
  }

  logInternal("Generating field assignment");

  // INHIBIT cleanup during RHS evaluation
  inhibitCleanUp = true;

  // Get the address of the field (LHS)
  llvm::Value *fieldAddress = generateInfixAddress(fieldStmt->lhs_chain.get());
  if (!fieldAddress) {
    reportDevBug("Failed to get field address", fieldStmt);
    return;
  }

  // POINTER ASSIGNMENT (ptr field = addr something)
  if (fieldSym->isPointer) {
    llvm::Value *newAddr = generateExpression(fieldStmt->value.get());
    auto *storeInst = funcBuilder.CreateStore(newAddr, fieldAddress);
    if (fieldSym->isVolatile) {
      storeInst->setVolatile(true);
    }

    inhibitCleanUp = false;
    emitCleanup(fieldStmt);

    return;
  }

  // Generate RHS value (could be complex)
  llvm::Value *rhsValue = generateExpression(fieldStmt->value.get());

  // CASE 2: MOVE EXPRESSION (field = move something)
  if (dynamic_cast<MoveExpression *>(fieldStmt->value.get())) {
    logInternal("Handling move assignment to field");
    auto *storeInst = funcBuilder.CreateStore(rhsValue, fieldAddress);
    if (fieldSym->isVolatile) {
      storeInst->setVolatile(true);
    }

    inhibitCleanUp = false;
    emitCleanup(fieldStmt);
    return;
  }

  //  DEEP COPY (large structs/arrays)
  bool isHuge = (fieldSym->type.kind == DataType::COMPONENT ||
                 fieldSym->type.kind == DataType::RECORD) &&
                (!fieldSym->type.isPointer);

  if (fieldSym->type.isArray || isHuge) {
    logInternal("Handling deep copy to field");
    uint64_t byteSize = fieldSym->componentSize;
    llvm::Type *elemLLVMTy = getLLVMType(
        fieldSym->type.isArray ? semantics.getArrayElementType(fieldSym->type)
                               : fieldSym->type);
    llvm::Align align = layout->getABITypeAlign(elemLLVMTy);

    // For array fields, we might need to load the field's address differently
    if (fieldSym->type.isArray) {
      llvm::Value *fieldBasePtr = funcBuilder.CreateLoad(
          funcBuilder.getPtrTy(), fieldAddress, "field_array_base");
      if (fieldSym->isVolatile) {
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(fieldBasePtr)) {
          load->setVolatile(true);
        }
      }
      funcBuilder.CreateMemCpy(fieldBasePtr, align, rhsValue, align,
                               funcBuilder.getInt64(byteSize));
    } else {
      // Regular struct copy
      funcBuilder.CreateMemCpy(fieldAddress, align, rhsValue, align,
                               funcBuilder.getInt64(byteSize));
    }

    inhibitCleanUp = false;
    emitCleanup(fieldStmt);
    return;
  }

  // SCALAR ASSIGNMENT (normal case)
  llvm::StoreInst *storeInst = nullptr;

  if (fieldSym->isHeap) {
    // Field points to heap memory
    llvm::Value *valueToStore = rhsValue;
    if (valSym->isAddress) {
      // Need to load the actual value from the address
      valueToStore = funcBuilder.CreateLoad(getLLVMType(fieldSym->type),
                                            rhsValue, "loaded_field_val");
      if (valSym->isVolatile) {
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(valueToStore)) {
          load->setVolatile(true);
        }
      }
    }
    storeInst = funcBuilder.CreateStore(valueToStore, fieldAddress);
  } else {
    // Regular stack assignment
    storeInst = funcBuilder.CreateStore(rhsValue, fieldAddress);
  }

  if (fieldSym->isVolatile && storeInst) {
    storeInst->setVolatile(true);
  }

  // RELEASE inhibitor and CLEANUP
  inhibitCleanUp = false;
  emitCleanup(fieldStmt);

  logInternal("Ended field assignment generation");
}

void IRGenerator::generateBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt) {
    reportDevBug("Invalid block statement", node);
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
    reportDevBug("Invalid trace statement node", node);
  }

  auto traceSym = semantics.getSymbolFromMeta(traceStmt);

  // Getting the type
  ResolvedType type = traceSym->type;

  inhibitCleanUp = true;
  // Call the expression generation n the expression
  auto val = generateExpression(traceStmt->expr.get());
  if (!val) {
    errorHandler.addHint("The expression generator failed");
    reportDevBug("Failed to get value for the trace expression", traceStmt);
  }

  // Call the traceRuntime this is the one who actually prints
  traceRuntime(val, type);

  inhibitCleanUp = false;
  emitCleanup(traceStmt);
}

llvm::Value *IRGenerator::generateBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr) {
    errorHandler.addHint("Sent a wrong node to the block expression generator "
                         "could be a malformed AST");
    reportDevBug("Invalid block expression", blockExpr);
  }

  for (const auto &stmts : blockExpr->statements) {
    // Check if the current block is already terminated by a return or branch
    if (funcBuilder.GetInsertBlock()->getTerminator()) {
      logInternal("Skipping statement- block terminated");
      break;
    }
    generateStatement(stmts.get());
  }

  // After generating all statements, check if we have a terminator
  llvm::BasicBlock *currentBlock = funcBuilder.GetInsertBlock();
  llvm::Instruction *terminator = currentBlock ? currentBlock->getTerminator() : nullptr;
  
  if (terminator) {
    // Save the terminator (return) position
    llvm::ReturnInst *ret = llvm::dyn_cast<llvm::ReturnInst>(terminator);
    if (ret) {
      // Remove the terminator temporarily
      terminator->removeFromParent();
      
      // Insert cleanup at the current insertion point (which is before where the return was)
      emitBlockCleanUp(blockExpr);
      
      // Re-insert the return
      funcBuilder.Insert(ret);
    } else {
      // Other terminator (branch, etc.) - just add cleanup before it
      emitBlockCleanUp(blockExpr);
    }
  } else {
    // No terminator, just add cleanup
    emitBlockCleanUp(blockExpr);
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
      reportDevBug("Custom type requested but the resolved name is empty",
                   nullptr);
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
      reportDevBug("IRgenerator requested for unknown type '" + lookUpName +
                       "'",
                   nullptr);
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
    reportDevBug("Type '" + type.resolvedName + "' is unknown", nullptr);
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
  if (literal.empty())
    return '\0';
  // Just return the first byte of the already-decoded UTF-8 string
  return literal[0];
}

uint32_t IRGenerator::decodeUTF8ToCodePoint(const std::string &literal) {
  if (literal.empty())
    return 0;

  unsigned char first = static_cast<unsigned char>(literal[0]);
  if (first < 0x80)
    return first; // Simple ASCII

  // Reassemble the bits
  uint32_t cp = 0;
  size_t len = literal.length();

  if ((first & 0xE0) == 0xC0 && len >= 2) {
    cp =
        ((first & 0x1F) << 6) | (static_cast<unsigned char>(literal[1]) & 0x3F);
  } else if ((first & 0xF0) == 0xE0 && len >= 3) {
    cp = ((first & 0x0F) << 12) |
         ((static_cast<unsigned char>(literal[1]) & 0x3F) << 6) |
         (static_cast<unsigned char>(literal[2]) & 0x3F);
  } else if ((first & 0xF8) == 0xF0 && len >= 4) {
    cp = ((first & 0x07) << 18) |
         ((static_cast<unsigned char>(literal[1]) & 0x3F) << 12) |
         ((static_cast<unsigned char>(literal[2]) & 0x3F) << 6) |
         (static_cast<unsigned char>(literal[3]) & 0x3F);
  }
  return cp;
}

uint16_t IRGenerator::decodeChar16Literal(const std::string &literal) {
  return static_cast<uint16_t>(decodeUTF8ToCodePoint(literal));
}

uint32_t IRGenerator::decodeChar32Literal(const std::string &literal) {
  return decodeUTF8ToCodePoint(literal);
}

llvm::Value *IRGenerator::coerceToBoolean(llvm::Value *val, Node *exprNode) {
  if (!val)
    return nullptr;

  auto it = semantics.metaData.find(exprNode);
  if (it == semantics.metaData.end()) {
    errorHandler.addHint("Semantics did not register the condition metadata");
    reportDevBug("Could not find condition expression metadata", exprNode);
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
               exprNode);
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
    reportDevBug("shout! called with a null value", nullptr);
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
      reportDevBug("printString received value", nullptr);
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
      reportDevBug("Unsupported integer value in shout! ", nullptr);
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

void IRGenerator::freeForeigners(Node *block) {
  auto &map = auditor.foreignersToFree;

  logInternal("\n[FOREINER-CLEANUP] >>> Entering Foreigner Cleanup for "
              "block at line: " +
              block->toString());
  logInternal("[LOOP KEY] " + std::to_string((uintptr_t)block));

  auto it = map.find(block);
  if (it == map.end()) {
    logInternal(
        "  [SKIP] No foreigners registered for this block. Basket is empty.");
    return;
  }

  logInternal("  [BASKET-FOUND] Found " + std::to_string(it->second.size()) +
              " batons to process.");

  std::set<std::string> freedBatons;

  for (auto &[baton, contextSym] : it->second) {
    if (!baton)
      continue;

    if (freedBatons.count(baton->ID)) {
      logInternal("  [SKIP] Already freed: " + baton->ID);
      continue;
    }

    logInternal("  [INSPECTING] Baton ID: " + baton->ID);

    if (!contextSym) {
      logInternal("    [ERROR] Metadata missing for holder of " + baton->ID);
      continue;
    }

    logInternal(
        "    [TERMINAL-REACHED] Commencing deallocation sequence for: " +
        contextSym->ID);

    // Clear out dependents (skip if already freed)
    if (!baton->dependents.empty()) {
      logInternal("    [DEPS] Freeing " +
                  std::to_string(baton->dependents.size()) + " dependents...");
      for (const auto &[depID, depSym] : baton->dependents) {
        if (freedBatons.count(depID)) {
          logInternal("      -> Skipping (already freed): " + depID);
          continue;
        }
        logInternal("      -> Killing Dependent: " + depID);
        executePhysicalFree(depSym);
        freedBatons.insert(depID);
      }
    }

    // Kill the leader
    if (contextSym->isHeap) {
      logInternal("    [LEADER] Emitting physical free for: " + contextSym->ID);
      executePhysicalFree(contextSym);
    }

    // Mark as dead
    baton->isResponsible = false;
    baton->isAlive = false;
    freedBatons.insert(baton->ID);
    logInternal("    [SUCCESS] Baton " + baton->ID +
                " is now officially history.");
  }

  map.erase(block);
  logInternal("[FOREINER-CLEANUP] <<< Finished processing foreigners.\n");
}

void IRGenerator::freeNatives(Node *block) {
  auto &map = auditor.nativesToFree;

  logInternal("\n[NATIVE-CLEANUP] >>> Entering Native Cleanup for block "
              "at line: " +
              block->toString());
  logInternal("[LOOP KEY] " + std::to_string((uintptr_t)block));

  auto it = map.find(block);
  if (it == map.end()) {
    logInternal(
        "  [SKIP] No natives registered for this loop. Basket is empty.");
    return;
  }

  logInternal("  [BASKET-FOUND] Found " + std::to_string(it->second.size()) +
              " batons to process.");

  std::set<std::string> freedBatons;

  for (auto &[baton, contextSym] : it->second) {
    if (!baton)
      continue;

    if (freedBatons.count(baton->ID)) {
      logInternal("  [SKIP] Already freed: " + baton->ID);
      continue;
    }

    logInternal("  [INSPECTING] Baton ID: " + baton->ID);

    if (!contextSym) {
      logInternal("    [ERROR] Metadata missing for holder of " + baton->ID);
      continue;
    }

    logInternal(
        "    [TERMINAL-REACHED] Commencing deallocation sequence for: " +
        contextSym->ID);

    // Clear out dependents (skip if they're already freed)
    if (!baton->dependents.empty()) {
      logInternal("    [DEPS] Freeing " +
                  std::to_string(baton->dependents.size()) + " dependents...");
      for (const auto &[depID, depSym] : baton->dependents) {
        if (freedBatons.count(depID)) {
          logInternal("      -> Skipping (already freed): " + depID);
          continue;
        }
        logInternal("      -> Killing Dependent: " + depID);
        executePhysicalFree(depSym);
        freedBatons.insert(depID);
      }
    }

    // Kill the leader
    if (contextSym->isHeap) {
      logInternal("    [LEADER] Emitting physical free for: " + contextSym->ID);
      executePhysicalFree(contextSym);
    }

    // Mark as dead
    baton->isResponsible = false;
    baton->isAlive = false;
    freedBatons.insert(baton->ID);
    logInternal("    [SUCCESS] Baton " + baton->ID +
                " is now officially history.");
  }

  map.erase(block);
  logInternal("[NATIVE-CLEANUP] <<< Finished processing natives.\n");
}

void IRGenerator::emitBlockCleanUp(Node *block) {
  if (!block)
    return;

  logInternal("[IR-BUNKER-CLEANUP] Processing cleanup for block at: " +
              std::to_string((uintptr_t)block));
  freeNatives(block);
  freeForeigners(block);
}

void IRGenerator::executePhysicalFree(const std::shared_ptr<SymbolInfo> &sym) {
  if (!sym) {
    logInternal("  [EXEC-FREE-FAIL] Symbol is null.");
    return;
  }

  logInternal("  [EXEC-FREE] ID: " + sym->ID +
              " | HasLLVM: " + (sym->llvmValue ? "True" : "False") +
              " | IsHeap: " + (sym->isHeap ? "True" : "False"));

  if (!sym->llvmValue || !sym->isHeap)
    return;

  logInternal("  [Deallocating] Generating  deallocation for: " + sym->ID);

  auto it = semantics.allocatorMap.find(sym->allocType);
  if (it == semantics.allocatorMap.end()) {
    logInternal(
        "  [EXEC-FREE-FAIL] No allocator mapping for type allocator type '" +
        sym->allocType + "'");
    return;
  }

  auto deallocatorName = it->second.freeName;
  llvm::Function *deallocFunc = module->getFunction(deallocatorName);

  if (!deallocFunc) {
    logInternal("  [EXEC-FREE-FAIL] Could not find LLVM function: " +
                deallocatorName);
    return;
  }

  logInternal("  [EMITTING-CALL] @ " + deallocatorName + " for " + sym->ID);

  llvm::Value *castPtr = funcBuilder.CreatePointerCast(
      sym->llvmValue, deallocFunc->getFunctionType()->getParamType(0));

  funcBuilder.CreateCall(deallocFunc, {castPtr});
}

void IRGenerator::emitCleanup(Node *contextNode) {
  if (inhibitCleanUp) {
    logInternal("[CLEANUP-CHECK] Cleanup inhibited");
    return;
  }
  logInternal("[CLEANUP-CHECK] Node: " + contextNode->toString());
  if (dynamic_cast<Identifier *>(contextNode) != contextNode)
    logInternal("[CLEANUP-CHECK] Composite node detected");

  auto identifiers = semantics.digIdentifiers(contextNode);
  for (const auto &identifier : identifiers) {
    auto it = semantics.responsibilityTable.find(identifier);
    if (it == semantics.responsibilityTable.end()) {
      logInternal("  [SKIP] No baton associated with this node: " +
                  identifier->toString());
      continue;
    }

    auto &baton = it->second;
    if (!baton) {
      logInternal("  [SKIP] Baton pointer is null.");
      return;
    }

    auto identSym = semantics.getSymbolFromMeta(identifier);
    if (!identSym)
      reportDevBug("Failed to get identifier symbol info for '" +
                       semantics.extractIdentifierName(identifier) +
                       "' in node: " + contextNode->toString(),
                   identifier);

    logInternal("  [BATON-STATE] ID: " + identSym->ID +
                " | Responsible: " + (baton->isResponsible ? "T" : "F") +
                " | PtrCount: " + std::to_string(identSym->pointerCount) +
                " | RefCount: " + std::to_string(identSym->refCount));

    if (!baton->isResponsible) {
      logInternal("  [SKIP] Baton is not responsible for memory.");
      return;
    }

    // Trigger Logic (Counts)
    if (identSym->pointerCount == 0 && identSym->refCount == 0) {
      logInternal("  [TRIGGER] Last use detected. Commencing physical free.");
      // Free the depenedents first
      logInternal("  [Dependents] Size: " +
                  std::to_string(baton->dependents.size()));
      for (const auto &[id, depSym] : baton->dependents) {
        logInternal("    [Dep-Free] ID: " + id);
        executePhysicalFree(depSym);
      }

      // Free the leader
      if (identSym->isHeap) {
        logInternal("  [Leader] ID: " + identSym->ID);
        executePhysicalFree(identSym);
      }

      // Defuse to avoid issues
      baton->isResponsible = false;
      logInternal("  [BATON-DEFUSED] Responsibility revoked for: " +
                  identSym->ID);
    } else {
      logInternal("  [HOLD] Baton still has active references/pointers.");
    }
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

  // Convert OptLevel for layout (though layout mostly uses it for ABI
  // decisions)
  llvm::CodeGenOptLevel llvmOptLevel;
  switch (optLevel) {
  case OptLevel::NONE:
    llvmOptLevel = llvm::CodeGenOptLevel::None;
    break;
  case OptLevel::BASIC:
    llvmOptLevel = llvm::CodeGenOptLevel::Less;
    break;
  case OptLevel::STANDARD:
    llvmOptLevel = llvm::CodeGenOptLevel::Default;
    break;
  case OptLevel::AGGRESSIVE:
    llvmOptLevel = llvm::CodeGenOptLevel::Aggressive;
    break;
  default:
    llvmOptLevel = llvm::CodeGenOptLevel::None;
    break;
  }

  llvm::TargetOptions opt;

  // Create target machine WITH optimization level
  std::optional<llvm::Reloc::Model> relocModel = llvm::Reloc::PIC_;
  std::optional<llvm::CodeModel::Model> codeModel = std::nullopt;

  auto targetMachine = target->createTargetMachine(
      targetTripleStr, "generic", "", opt, relocModel, codeModel,
      llvmOptLevel, // Optimization level passed here
      false);       //  Optional unique suffix

  if (!targetMachine)
    throw std::runtime_error("Failed to create TargetMachine");

  module->setDataLayout(targetMachine->createDataLayout());
  this->layout = &module->getDataLayout();
}

bool IRGenerator::emitObjectFile(const std::string &filename) {
  // Initialization
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
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

  // Convert our OptLevel to LLVM optimization level
  llvm::CodeGenOptLevel llvmOptLevel;
  switch (optLevel) {
  case OptLevel::NONE:
    llvmOptLevel = llvm::CodeGenOptLevel::None;
    break;
  case OptLevel::BASIC:
    llvmOptLevel = llvm::CodeGenOptLevel::Less;
    break;
  case OptLevel::STANDARD:
    llvmOptLevel = llvm::CodeGenOptLevel::Default;
    break;
  case OptLevel::AGGRESSIVE:
    llvmOptLevel = llvm::CodeGenOptLevel::Aggressive;
    break;
  default:
    llvmOptLevel = llvm::CodeGenOptLevel::None;
    break;
  }

  // CPU and features using generic for now
  std::string cpu = "generic";
  std::string features = "";

  // Create target machine with optimization level
  std::optional<llvm::Reloc::Model> relocModel = llvm::Reloc::PIC_;
  std::optional<llvm::CodeModel::Model> codeModel = std::nullopt;

  auto targetMachine = target->createTargetMachine(
      targetTripleStr, cpu, features, opt, relocModel, codeModel,
      llvmOptLevel, // Optimization level
      false);       // Optional unique suffix

  if (!targetMachine) {
    llvm::errs() << "Failed to create TargetMachine\n";
    return false;
  }

  // Set data layout from target machine
  module->setDataLayout(targetMachine->createDataLayout());

  // Open output file
  std::error_code EC;
  llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);
  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message() << "\n";
    return false;
  }

  // Create pass manager
  llvm::legacy::PassManager pass;

  // Set file type to object file
  llvm::CodeGenFileType fileType = llvm::CodeGenFileType::ObjectFile;

  // Add passes to emit object file
  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
    llvm::errs() << "TargetMachine can't emit object file\n";
    return false;
  }

  // Run the passes
  pass.run(*module);

  // Flush output
  dest.flush();

  return true;
}

void IRGenerator::reportDevBug(const std::string &message, Node *contextNode) {
  int line = 0;
  int col = 0;
  if (contextNode) {
    line = contextNode->token.line;
    col = contextNode->token.column;
  }

  CompilerError error;
  error.level = ErrorLevel::INTERNAL;
  error.line = line;
  error.col = col;
  error.message = message;
  error.tokenLength = errorHandler.getTokenLength(contextNode);
  error.hints = {};

  errorHandler.report(error);

  std::abort();
}

void IRGenerator::logInternal(const std::string &message) {
  if (isVerbose) {
    std::cout << message << "\n";
  }
}
