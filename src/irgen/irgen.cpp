#include "llvm/TargetParser/Host.h"
#include <llvm-18/llvm/IR/Constants.h>
#include <llvm-18/llvm/IR/DataLayout.h>
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
#include "irgen.hpp"

#include <iostream>
#define CPPREST_FORCE_REBUILD

IRGenerator::IRGenerator(Semantics &semantics, size_t totalHeap)
    : semantics(semantics), totalHeapSize(totalHeap), context(),
      globalBuilder(context), funcBuilder(context),
      module(std::make_unique<llvm::Module>("unnameable", context)) {
  this->layout = &module->getDataLayout();

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

  llvm::FunctionType *freeType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), // returns void
                              false);

  llvm::FunctionType *destroyType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context), {}, false); // void()

  // Add them to the module
  module->getOrInsertFunction("sage_init", initType);
  module->getOrInsertFunction("sage_alloc", allocType);
  module->getOrInsertFunction("sage_free", freeType);
  module->getOrInsertFunction("sage_destroy", destroyType);

  llvm::Function *globalInitFn = llvm::Function::Create(
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), false),
      llvm::GlobalValue::InternalLinkage, "global_init", module.get());

  heapInitFnEntry = llvm::BasicBlock::Create(context, "entry", globalInitFn);
}

// MAIN GENERATOR FUNCTION
void IRGenerator::generate(const std::vector<std::unique_ptr<Node>> &program) {
  // Generate program body
  for (const auto &node : program) {
    generateStatement(node.get());
  }

  // Finish off global heap init if it exists
  if (heapInitFnEntry) {
    llvm::IRBuilder<> globalInitBuilder(heapInitFnEntry);
    globalInitBuilder.CreateRetVoid();
  }

  // Look for the main function if the user qualified the function
  llvm::Function *mainFn = module->getFunction("main");
  if (!mainFn) {
    if (mainMarker) {
      throw std::runtime_error("You marked this file with qualified this file "
                               "to be main but did not define a main function");
    } else {
      // No main expected, skip
      return;
    }
  }

  llvm::BasicBlock &entryBlock = mainFn->getEntryBlock();
  llvm::IRBuilder<> tmpBuilder(&entryBlock, entryBlock.begin());

  llvm::Function *globalHeapFn = module->getFunction("global_init");
  tmpBuilder.CreateCall(globalHeapFn);
}

// MAIN GENERATOR FUNCTION FOR EXPRESSION
//  Main Expression generator helper function
llvm::Value *IRGenerator::generateExpression(Node *node) {
  if (!node) {
    std::cout << "[IRGEN] NULL node!\n";
    return nullptr;
  }

  auto exprIt = expressionGeneratorsMap.find(typeid(*node));
  if (exprIt == expressionGeneratorsMap.end()) {
    throw std::runtime_error("Could not find expression type IR generator: " +
                             node->toString());
  }

  return (this->*exprIt->second)(node);
}

// Main L-value generator helper functions
llvm::Value *IRGenerator::generateAddress(Node *node) {
  if (!node) {
    std::cout << "[IRGEN] Null node! \n";
  }
  auto addrIt = addressGeneratorsMap.find(typeid(*node));
  if (addrIt == addressGeneratorsMap.end()) {
    throw std::runtime_error("Could not find address generator for: " +
                             node->toString());
  }

  return (this->*addrIt->second)(node);
}

// GENERATOR FUNCTIONS
void IRGenerator::generateStatement(Node *node) {
  auto generatorIt = generatorFunctionsMap.find(typeid(*node));
  if (generatorIt == generatorFunctionsMap.end()) {
    std::cout << "Failed to find statement IR generator for : "
              << node->toString() << "\n";
    return;
  }
  (this->*generatorIt->second)(node);
}
// STATEMENT GENERATOR FUNCTIONS
// Reference statement IR generator
void IRGenerator::generateReferenceStatement(Node *node) {
  auto refStmt = dynamic_cast<ReferenceStatement *>(node);
  if (!refStmt)
    throw std::runtime_error("Invalid reference statement");

  const auto &refName = refStmt->referer->expression.TokenLiteral;
  const auto &refereeName =
      semantics.extractIdentifierName(refStmt->referee.get());

  // Lookup metadata
  auto metaIt = semantics.metaData.find(refStmt);
  if (metaIt == semantics.metaData.end())
    throw std::runtime_error("Failed to find reference metaData for '" +
                             refName + "'");

  auto refSym = metaIt->second;
  auto targetSym = refSym->refereeSymbol;
  if (!targetSym)
    throw std::runtime_error("Reference '" + refName +
                             "' has no target symbol");

  if (targetSym->hasError)
    throw std::runtime_error("Semantic error detected on reference target '" +
                             refereeName + "'");

  // Generate the LLVM pointer to the actual value, not the pointer variable
  llvm::Value *targetAddress = nullptr;
  if (targetSym->llvmValue) {
    // If the target has already generated LLVM value, ensure we store the
    // **address**
    if (targetSym->llvmValue->getType()->isPointerTy()) {
      targetAddress = targetSym->llvmValue;
    } else {
      // For scalars, take their address
      targetAddress = funcBuilder.CreateAlloca(targetSym->llvmValue->getType(),
                                               nullptr, refereeName + "_addr");
      funcBuilder.CreateStore(targetSym->llvmValue, targetAddress);
    }
  } else {
    // If LLVM value not yet generated, compute the address via generateAddress
    targetAddress = generateAddress(refStmt->referee.get());
  }

  if (!targetAddress || !targetAddress->getType()->isPointerTy())
    throw std::runtime_error("Failed to resolve LLVM address for reference '" +
                             refName + "'");

  // The reference itself holds the pointer to the target
  refSym->llvmValue = targetAddress;
}

// IR code gen for an if statement
void IRGenerator::generateIfStatement(Node *node) {
  auto ifStmt = dynamic_cast<ifStatement *>(node);
  if (!ifStmt) {
    throw std::runtime_error("Invalid if statement");
  }

  std::cerr << "[IR DEBUG] Generating if statement\n";

  // Generation of condition for the if
  llvm::Value *condVal = generateExpression(ifStmt->condition.get());
  if (!condVal) {
    throw std::runtime_error("Invalid if condition");
  }

  if (!condVal->getType()->isIntegerTy(1)) {
    condVal = funcBuilder.CreateICmpNE(
        condVal, llvm::ConstantInt::get(condVal->getType(), 0), "ifcond.bool");
  }

  // Create basic blocks
  llvm::Function *function = funcBuilder.GetInsertBlock()->getParent();
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(context, "then", function);
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifmerge");

  // Determine the next block (first elif, else, or merge)
  llvm::BasicBlock *nextBB = nullptr;
  if (!ifStmt->elifClauses.empty()) {
    nextBB = llvm::BasicBlock::Create(context, "elif0");
  } else if (ifStmt->else_result.has_value()) {
    nextBB = llvm::BasicBlock::Create(context, "else");
  } else {
    nextBB = mergeBB;
  }

  // Conditional branch for if
  std::cerr << "[IR DEBUG] Creating conditional branch for if\n";
  funcBuilder.CreateCondBr(condVal, thenBB, nextBB);

  // Generate then branch
  funcBuilder.SetInsertPoint(thenBB);
  std::cerr << "[IR DEBUG] Generating then branch\n";
  generateStatement(ifStmt->if_result.get());
  if (!funcBuilder.GetInsertBlock()->getTerminator()) {
    std::cerr << "[IR DEBUG] Adding branch to ifmerge from then\n";
    funcBuilder.CreateBr(mergeBB);
  } else {
    std::cerr
        << "[IR DEBUG] Skipping branch to ifmerge from then due to terminator: "
        << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName()
        << "\n";
  }

  // Generating elif branches
  for (size_t i = 0; i < ifStmt->elifClauses.size(); ++i) {
    function->insert(function->end(), nextBB);
    funcBuilder.SetInsertPoint(nextBB);
    std::cerr << "[IR DEBUG] Generating elif branch " << i << "\n";

    const auto &elifStmt = ifStmt->elifClauses[i];
    auto elif = dynamic_cast<elifStatement *>(elifStmt.get());
    llvm::Value *elifCondVal = generateExpression(elif->elif_condition.get());
    if (!elifCondVal) {
      throw std::runtime_error("Invalid elif condition");
    }
    if (!elifCondVal->getType()->isIntegerTy(1)) {
      elifCondVal = funcBuilder.CreateICmpNE(
          elifCondVal, llvm::ConstantInt::get(elifCondVal->getType(), 0),
          "elifcond.bool");
    }

    llvm::BasicBlock *elifBodyBB = llvm::BasicBlock::Create(
        context, "elif.body" + std::to_string(i), function);
    llvm::BasicBlock *nextElifBB =
        (i + 1 < ifStmt->elifClauses.size())
            ? llvm::BasicBlock::Create(context, "elif" + std::to_string(i + 1))
            : (ifStmt->else_result.has_value()
                   ? llvm::BasicBlock::Create(context, "else")
                   : mergeBB);

    std::cerr << "[IR DEBUG] Creating conditional branch for elif " << i
              << "\n";
    funcBuilder.CreateCondBr(elifCondVal, elifBodyBB, nextElifBB);

    funcBuilder.SetInsertPoint(elifBodyBB);
    std::cerr << "[IR DEBUG] Generating elif body " << i << "\n";
    generateStatement(elif->elif_result.get());
    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      std::cerr << "[IR DEBUG] Adding branch to ifmerge from elif " << i
                << "\n";
      funcBuilder.CreateBr(mergeBB);
    } else {
      std::cerr
          << "[IR DEBUG] Skipping branch " << i
          << " to ifmerge from elif due to terminator "
          << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName()
          << "\n";
    }

    nextBB = nextElifBB;
  }

  // Generate else branch if present
  if (ifStmt->else_result.has_value()) {
    function->insert(function->end(), nextBB);
    funcBuilder.SetInsertPoint(nextBB);
    std::cerr << "[IR DEBUG] Generating else branch\n";
    generateStatement(ifStmt->else_result.value().get());
    if (!funcBuilder.GetInsertBlock()->getTerminator()) {
      std::cerr << "[IR DEBUG] Adding branch to ifmerge from else\n";
      funcBuilder.CreateBr(mergeBB);
    } else {
      std::cerr
          << "[IR DEBUG] Skipping branch to ifmerge from else due to "
             "terminator: "
          << funcBuilder.GetInsertBlock()->getTerminator()->getOpcodeName()
          << "\n";
    }
  }

  // Finalize with merge block
  function->insert(function->end(), mergeBB);
  funcBuilder.SetInsertPoint(mergeBB);
  std::cerr << "[IR DEBUG] Finished generating if statement\n";
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

  if (isGlobalScope)
    throw std::runtime_error(
        "Executable statements are not allowed at global scope");

  llvm::Value *targetPtr = nullptr;
  llvm::CallInst *pendingFree = nullptr;
  llvm::Value *initValue = generateExpression(assignStmt->value.get());
  if (!initValue)
    throw std::runtime_error("Failed to generate IR for assignment value");

  // Check if it's a SelfExpression (self.field)
  if (auto *selfExpr =
          dynamic_cast<SelfExpression *>(assignStmt->identifier.get())) {
    // Generate pointer to the field in the struct
    targetPtr = generateSelfAddress(selfExpr);
    if (!targetPtr)
      throw std::runtime_error("Failed to get pointer for self field");
  }

  // Checking if it is a dereference expression
  else if (auto derefExpr = dynamic_cast<DereferenceExpression *>(
               assignStmt->identifier.get())) {
    std::cout << "Inside Assignment statement deref branch\n";
    targetPtr = generateDereferenceAddress(derefExpr);
    if (!targetPtr)
      throw std::runtime_error(
          "Failed to get pointer for the dereference expression");
  } else if (auto arraySub =
                 dynamic_cast<ArraySubscript *>(assignStmt->identifier.get())) {
    std::cout << "Inside array sub branch\n";
    targetPtr = generateArraySubscriptAddress(arraySub);
    if (!targetPtr) {
      throw std::runtime_error("Failed to get L-value for array sub");
    }
  } else { // Regular variable assignment
    const std::string &varName =
        assignStmt->identifier->expression.TokenLiteral;
    auto metaIt = semantics.metaData.find(assignStmt);
    if (metaIt == semantics.metaData.end())
      throw std::runtime_error("Could not find variable '" + varName +
                               "' metaData");

    auto assignSym = metaIt->second;

    if (!assignSym)
      throw std::runtime_error("Could not find variable '" + varName + "'");

    if (assignSym->hasError)
      throw std::runtime_error("Semantic error detected");

    AddressAndPendingFree addrAndPendingFree =
        generateIdentifierAddress(assignStmt->identifier.get());
    targetPtr = addrAndPendingFree.address;
    for (auto *freeCall : addrAndPendingFree.pendingFrees) {
      pendingFree = funcBuilder.Insert(freeCall);
    }

    if (!targetPtr)
      throw std::runtime_error("No memory allocated for variable '" + varName +
                               "'");

    if (pendingFree) {
      funcBuilder.Insert(pendingFree);
    }
  }
  // Store the value
  funcBuilder.CreateStore(initValue, targetPtr);
  std::cout << "Exited assignment generator\n";
}

void IRGenerator::generateFieldAssignmentStatement(Node *node) {
  auto *fieldStmt = dynamic_cast<FieldAssignment *>(node);
  if (!fieldStmt)
    return;

  // Generate RHS (value to store)
  llvm::Value *rhs = generateExpression(fieldStmt->value.get());
  if (!rhs)
    throw std::runtime_error("Failed to generate RHS IR");

  // Split scoped name -> "Person::age" -> parent = "Person", child = "age"
  auto [parentVarName, childName] =
      semantics.splitScopedName(fieldStmt->assignment_token.TokenLiteral);

  // Resolve parent symbol info (the instance)
  auto parentMetaIt = semantics.metaData.find(fieldStmt);
  if (parentMetaIt == semantics.metaData.end())
    throw std::runtime_error("Field assignment is lacking metaData");

  auto parentVarInfo = parentMetaIt->second;
  if (!parentVarInfo)
    throw std::runtime_error("Unidentified variable '" + parentVarName + "'");

  // Getting the base symbol
  auto baseSym = parentVarInfo->baseSymbol;
  if (!baseSym)
    throw std::runtime_error("Unidentified variable '" + parentVarName + "'");

  std::cout << "Parent type name: " << baseSym->type.resolvedName << "\n";

  if (!baseSym->llvmValue)
    throw std::runtime_error("Unresolved parent instance: " + parentVarName);

  // Resolve parent type and member info
  auto parentTypeName = baseSym->type.resolvedName;
  auto parentIt = semantics.customTypesTable.find(parentTypeName);
  if (parentIt == semantics.customTypesTable.end())
    throw std::runtime_error("Type '" + parentTypeName + "' does not exist");

  auto &members = parentIt->second->members;
  auto childIt = members.find(childName);
  if (childIt == members.end())
    throw std::runtime_error("'" + childName + "' is not a member of '" +
                             parentTypeName + "'");

  // Member metadata and types
  auto memberInfo =
      childIt->second; // whatever structure you use for member metadata
  llvm::StructType *structTy = llvmCustomTypes[parentTypeName];
  unsigned fieldIndex = memberInfo->memberIndex;

  // GEP to the member slot inside this instance (slot type for heap-member is
  // "elem*", so the GEP yields a pointer-to-slot whose type is elem**)
  llvm::Value *fieldPtr = funcBuilder.CreateStructGEP(
      structTy, baseSym->llvmValue, fieldIndex, childName);
  if (!fieldPtr)
    throw std::runtime_error("Failed to compute GEP for member '" + childName +
                             "'");

  if (memberInfo->isHeap) {
    // Element LLVM type (e.g., i32 for int)
    llvm::Type *elemTy = getLLVMType(memberInfo->type);
    if (!elemTy)
      throw std::runtime_error("Failed to get LLVM element type for member '" +
                               childName + "'");

    // ptr-to-elem type (elem*)
    llvm::PointerType *elemPtrTy = elemTy->getPointerTo();

    // Load current heap pointer from the slot: heapPtr = load(elem*, fieldPtr)
    llvm::Value *heapPtr =
        funcBuilder.CreateLoad(elemPtrTy, fieldPtr, childName + "_heap_load");

    // If heapPtr == null -> allocate; otherwise reuse existing pointer.
    llvm::Value *nullPtr = llvm::ConstantPointerNull::get(elemPtrTy);
    llvm::Value *isNull =
        funcBuilder.CreateICmpEQ(heapPtr, nullPtr, childName + "_is_null");

    llvm::Function *parentFn = funcBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *allocBB =
        llvm::BasicBlock::Create(context, childName + "_alloc", parentFn);
    llvm::BasicBlock *contBB =
        llvm::BasicBlock::Create(context, childName + "_cont", parentFn);

    // Cond branch on null
    funcBuilder.CreateCondBr(isNull, allocBB, contBB);

    //  allocBB: call sage_alloc(size), bitcast to elemPtrTy, store into slot,
    //  branch to contBB
    funcBuilder.SetInsertPoint(allocBB);

    llvm::PointerType *i8PtrTy =
        llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
    llvm::FunctionCallee sageAllocFn = module->getOrInsertFunction(
        "sage_alloc", i8PtrTy, llvm::Type::getInt64Ty(context));

    // compute size (use componentSize from semantics if present or DataLayout)
    uint64_t size = module->getDataLayout().getTypeAllocSize(elemTy);

    llvm::Value *sizeArg =
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size);

    llvm::Value *rawAlloc = funcBuilder.CreateCall(sageAllocFn, {sizeArg},
                                                   childName + "_alloc_i8ptr");
    // bitcast i8* -> elem*
    llvm::Value *newHeapPtr = funcBuilder.CreateBitCast(
        rawAlloc, elemPtrTy, childName + "_alloc_ptr");

    // store pointer into the struct slot
    funcBuilder.CreateStore(newHeapPtr, fieldPtr);
    // branch to continuation
    funcBuilder.CreateBr(contBB);

    // --- contBB: reload heapPtr from slot (now definitely non-null)
    funcBuilder.SetInsertPoint(contBB);
    llvm::Value *heapPtr2 =
        funcBuilder.CreateLoad(elemPtrTy, fieldPtr, childName + "_heap");
    // use heapPtr2 for storing rhs
    funcBuilder.CreateStore(rhs, heapPtr2);

    // After the store, if this member's symbol marks this fieldStmt as last
    // use, free it
    if (memberInfo->isHeap &&
        (memberInfo->lastUseNode == fieldStmt ||
         memberInfo->lastUseNode ==
             /* possibly other node pointer */ memberInfo->node)) {
      llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
          "sage_free", llvm::Type::getVoidTy(context),
          llvm::Type::getInt64Ty(context));
      funcBuilder.CreateCall(
          sageFreeFn,
          {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), size)});
    }

    // continue building after contBB (builder is already positioned at contBB
    // end)
  } else {
    // Non-heap: store RHS directly into the field slot
    funcBuilder.CreateStore(rhs, fieldPtr);
  }
}

void IRGenerator::generateBlockStatement(Node *node) {
  auto blockStmt = dynamic_cast<BlockStatement *>(node);
  if (!blockStmt) {
    throw std::runtime_error("Invalid block statement");
  }

  std::cerr << "[IR DEBUG] Generating block statement with "
            << blockStmt->statements.size() << " statements\n";
  for (const auto &stmt : blockStmt->statements) {
    std::cerr << "[IR DEBUG] Processing block statement child of type: "
              << typeid(*stmt).name() << " - " << stmt->toString() << "\n";

    generateStatement(stmt.get());

    auto currentBlock = funcBuilder.GetInsertBlock();
    if (currentBlock) {
      if (currentBlock->getTerminator()) {
        std::cerr << "[IR DEBUG] Terminator found in block statement child: "
                  << currentBlock->getTerminator()->getOpcodeName() << "\n";
        break; // Stop generating further instructions in this block
      }
    } else {
      std::cerr << "[IR DEBUG] No current insert block after statement, "
                   "skipping terminator check\n";
    }
  }
}

void IRGenerator::generateShoutStatement(Node *node) {
  auto shoutStmt = dynamic_cast<ShoutStatement *>(node);

  if (!shoutStmt)
    throw std::runtime_error("Invalid shout statement node");

  if (isGlobalScope)
    throw std::runtime_error("Expected shout to be inside a function");

  // Getting the semantic type of the val
  auto it = semantics.metaData.find(shoutStmt->expr.get());
  if (it == semantics.metaData.end())
    throw std::runtime_error("Missing metaData for shout expression");

  // Getting the symbol
  auto exprSym = it->second;
  if (!exprSym)
    throw std::runtime_error("No symbol info found ");

  // Getting the type
  ResolvedType type = exprSym->type;

  // Call the expression generation n the expression
  auto val = generateExpression(shoutStmt->expr.get());
  if (!val)
    throw std::runtime_error(
        "No llvm value was generated for expression in shout");

  // Call the shoutRuntime this is the one who actually prints
  shoutRuntime(val, type);
}

// EXPRESSION GENERATOR
//  Expression generator functions
llvm::Value *IRGenerator::generateInfixExpression(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  if (!infix)
    throw std::runtime_error("Invalid infix expression");

  auto infixIt = semantics.metaData.find(infix);
  if (infixIt == semantics.metaData.end())
    throw std::runtime_error("Cannot find infix metaData");

  auto infixSym = infixIt->second;
  if (infixSym->hasError)
    throw std::runtime_error("Semantic error detected");

  auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
  auto lhsIdent = dynamic_cast<Identifier *>(infix->left_operand.get());

  if (isGlobalScope && (lhsIdent || rhsIdent)) {
    throw std::runtime_error(
        "Executable statements are not allowed at global scope: '" +
        infix->operat.TokenLiteral + "' at line " +
        std::to_string(infix->operat.line));
  }

  if (infix->operat.type == TokenType::FULLSTOP) {
    // sanity check: member access only inside a function
    if (isGlobalScope)
      throw std::runtime_error("Member access not allowed at global scope");

    llvm::Value *lhsVal = generateExpression(infix->left_operand.get());
    auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
    if (!rhsIdent)
      throw std::runtime_error("Right side of '.' must be an identifier");

    std::string memberName = rhsIdent->expression.TokenLiteral;

    // get metadata for the left operand
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];
    if (!lhsMeta)
      throw std::runtime_error("Missing metadata for struct expression");

    std::string parentTypeName = lhsMeta->type.resolvedName;

    // get struct definition
    auto parentTypeIt = semantics.customTypesTable.find(parentTypeName);
    if (parentTypeIt == semantics.customTypesTable.end())
      throw std::runtime_error("Unknown struct type '" + parentTypeName + "'");

    auto &parentInfo = parentTypeIt->second;
    auto memberIt = parentInfo->members.find(memberName);
    if (memberIt == parentInfo->members.end())
      throw std::runtime_error("No member '" + memberName + "' in type '" +
                               parentTypeName + "'");

    // get member index + type
    unsigned memberIndex = memberIt->second->memberIndex;
    llvm::StructType *structTy = llvmCustomTypes[parentTypeName];
    llvm::Type *memberType = getLLVMType(memberIt->second->type);

    // if lhs is struct by value, we need a pointer to use GEP
    llvm::Value *lhsPtr = lhsVal;
    if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(lhsVal)) {
      lhsPtr = funcBuilder.CreateLoad(structTy->getPointerTo(), gv,
                                      gv->getName() + ".loaded");
    } else if (!lhsVal->getType()->isPointerTy()) {
      llvm::Value *allocaTmp = funcBuilder.CreateAlloca(
          lhsVal->getType(), nullptr, parentTypeName + "_tmp");
      funcBuilder.CreateStore(lhsVal, allocaTmp);
      lhsPtr = allocaTmp;
    }

    llvm::Value *memberPtr =
        funcBuilder.CreateStructGEP(structTy, lhsPtr, memberIndex, memberName);

    // now return by-value load, not a pointer
    llvm::Value *memberVal =
        funcBuilder.CreateLoad(memberType, memberPtr, memberName + "_val");

    // If this is the last use and the component instance was heap raised
    if (lhsMeta->isHeap && lhsMeta->lastUseNode == infix &&
        lhsMeta->refCount == 0) {
      llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
          "sage_free", llvm::Type::getVoidTy(context),
          llvm::Type::getInt64Ty(context));
      funcBuilder.CreateCall(
          sageFreeFn,
          {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                  lhsMeta->componentSize)},
          infix->left_operand->expression.TokenLiteral + "_sage_free");
    }

    return memberVal;
  }

  if (infix->operat.type == TokenType::SCOPE_OPERATOR) {
    std::cout << "TRIGGERED SCOPE OPERATOR GUY\n";

    // "::" is only for enum access
    auto enumName = lhsIdent->expression.TokenLiteral;
    auto leftTypeIt = semantics.customTypesTable.find(enumName);
    if (leftTypeIt == semantics.customTypesTable.end())
      throw std::runtime_error("Unknown enum class '" + enumName + "'");

    auto leftType = leftTypeIt->second->type;

    // If the type isn't an enum reject it
    if (leftType.kind != DataType::ENUM) {
      throw std::runtime_error(
          "The '::' operator is only valid for enum access");
    }

    auto &enumInfo = leftTypeIt->second;

    // RHS must be an identifier (enum member)
    auto memberIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
    if (!memberIdent)
      throw std::runtime_error(
          "Right-hand of '::' must be an enum member identifier");

    auto memberName = memberIdent->expression.TokenLiteral;

    // Find enum member
    auto memIt = enumInfo->members.find(memberName);
    if (memIt == enumInfo->members.end())
      throw std::runtime_error("Enum '" + enumName + "' has no member named '" +
                               memberName + "'");

    uint64_t memberValue = memIt->second->constantValue;

    // Return constant integer
    llvm::Type *llvmEnumTy = getLLVMType(leftType);
    return llvm::ConstantInt::get(llvmEnumTy, memberValue);
  }

  llvm::Value *left = generateExpression(infix->left_operand.get());
  llvm::Value *right = generateExpression(infix->right_operand.get());

  if (!left || !right)
    throw std::runtime_error("Failed to generate IR for infix expression");

  auto it = semantics.metaData.find(node);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Meta data missing for infix node");

  ResolvedType resultType = it->second->type;
  DataType leftType = semantics.metaData[infix->left_operand.get()]->type.kind;
  DataType rightType =
      semantics.metaData[infix->right_operand.get()]->type.kind;

  // Helper lambda for integer type promotion
  auto promoteInt = [&](llvm::Value *val, DataType fromType,
                        DataType toType) -> llvm::Value * {
    unsigned fromBits = getIntegerBitWidth(fromType);
    unsigned toBits = getIntegerBitWidth(toType);
    if (fromBits == 0 || toBits == 0)
      return val; // Not integer type
    if (fromBits == toBits)
      return val; // Same bit width, no promotion needed

    bool fromSigned = isSignedInteger(fromType);
    if (toBits > fromBits) {
      if (fromSigned)
        return funcBuilder.CreateSExt(
            val, llvm::IntegerType::get(context, toBits), "sexttmp");
      else
        return funcBuilder.CreateZExt(
            val, llvm::IntegerType::get(context, toBits), "zexttmp");
    } else {
      // Truncation if needed (rare, probably invalid here)
      return funcBuilder.CreateTrunc(
          val, llvm::IntegerType::get(context, toBits), "trunctmp");
    }
  };

  // Promote operands to widest integer type among left, right, and result
  if (isIntegerType(resultType.kind)) {
    unsigned targetBits = getIntegerBitWidth(resultType.kind);
    left = promoteInt(left, leftType, resultType.kind);
    right = promoteInt(right, rightType, resultType.kind);
  }

  // Handle BOOLEAN logical operators (AND, OR)
  if (infix->operat.type == TokenType::AND ||
      infix->operat.type == TokenType::OR) {
    if (left->getType() != funcBuilder.getInt1Ty())
      left = funcBuilder.CreateICmpNE(
          left, llvm::ConstantInt::get(left->getType(), 0), "boolcastl");
    if (right->getType() != funcBuilder.getInt1Ty())
      right = funcBuilder.CreateICmpNE(
          right, llvm::ConstantInt::get(right->getType(), 0), "boolcastr");

    if (infix->operat.type == TokenType::AND)
      return funcBuilder.CreateAnd(left, right, "andtmp");
    else
      return funcBuilder.CreateOr(left, right, "ortmp");
  }

  // Handle floating point conversions
  if (resultType.kind == DataType::FLOAT) {
    if (isIntegerType(leftType))
      left = funcBuilder.CreateSIToFP(left, llvm::Type::getFloatTy(context),
                                      "inttofloat");
    if (isIntegerType(rightType))
      right = funcBuilder.CreateSIToFP(right, llvm::Type::getFloatTy(context),
                                       "inttofloat");
  } else if (resultType.kind == DataType::DOUBLE) {
    if (isIntegerType(leftType))
      left = funcBuilder.CreateSIToFP(left, llvm::Type::getDoubleTy(context),
                                      "inttodouble");
    if (isIntegerType(rightType))
      right = funcBuilder.CreateSIToFP(right, llvm::Type::getDoubleTy(context),
                                       "inttodouble");
  }

  // Now generate code based on operator and result type
  // Comparison operators - different for signed/unsigned integers
  if (infix->operat.type == TokenType::EQUALS ||
      infix->operat.type == TokenType::NOT_EQUALS ||
      infix->operat.type == TokenType::LESS_THAN ||
      infix->operat.type == TokenType::LT_OR_EQ ||
      infix->operat.type == TokenType::GREATER_THAN ||
      infix->operat.type == TokenType::GT_OR_EQ) {
    if (isIntegerType(leftType) || leftType == DataType::ENUM) {
      if (isIntegerType(rightType) || rightType == DataType::ENUM) {
        bool signedInt = isSignedInteger(resultType.kind);
        switch (infix->operat.type) {
        case TokenType::EQUALS:
          return funcBuilder.CreateICmpEQ(left, right, "cmptmp");
        case TokenType::NOT_EQUALS:
          return funcBuilder.CreateICmpNE(left, right, "cmptmp");
        case TokenType::LESS_THAN:
          return signedInt ? funcBuilder.CreateICmpSLT(left, right, "cmptmp")
                           : funcBuilder.CreateICmpULT(left, right, "cmptmp");
        case TokenType::LT_OR_EQ:
          return signedInt ? funcBuilder.CreateICmpSLE(left, right, "cmptmp")
                           : funcBuilder.CreateICmpULE(left, right, "cmptmp");
        case TokenType::GREATER_THAN:
          return signedInt ? funcBuilder.CreateICmpSGT(left, right, "cmptmp")
                           : funcBuilder.CreateICmpUGT(left, right, "cmptmp");
        case TokenType::GT_OR_EQ:
          return signedInt ? funcBuilder.CreateICmpSGE(left, right, "cmptmp")
                           : funcBuilder.CreateICmpUGE(left, right, "cmptmp");
        default:
          throw std::runtime_error("Unsupported int comparison operator");
        }
      }
    } else if (resultType.kind == DataType::FLOAT ||
               resultType.kind == DataType::DOUBLE) {
      switch (infix->operat.type) {
      case TokenType::EQUALS:
        return funcBuilder.CreateFCmpOEQ(left, right, "fcmptmp");
      case TokenType::NOT_EQUALS:
        return funcBuilder.CreateFCmpONE(left, right, "fcmptmp");
      case TokenType::LESS_THAN:
        return funcBuilder.CreateFCmpOLT(left, right, "fcmptmp");
      case TokenType::LT_OR_EQ:
        return funcBuilder.CreateFCmpOLE(left, right, "fcmptmp");
      case TokenType::GREATER_THAN:
        return funcBuilder.CreateFCmpOGT(left, right, "fcmptmp");
      case TokenType::GT_OR_EQ:
        return funcBuilder.CreateFCmpOGE(left, right, "fcmptmp");
      default:
        throw std::runtime_error("Unsupported float comparison operator");
      }
    } else {
      throw std::runtime_error("Comparison not supported for type '" +
                               resultType.resolvedName + "'");
    }
  }
  // Arithmetic operators
  switch (infix->operat.type) {
  case TokenType::PLUS: {
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];

    if (lhsMeta->type.isPointer) {
      llvm::Type *baseTy = getLLVMType(lhsMeta->targetSymbol->type);
      return funcBuilder.CreateGEP(baseTy, left, right, "ptr_addtmp");
    }

    if (isIntegerType(resultType.kind))
      return funcBuilder.CreateAdd(left, right, "addtmp");
    else
      return funcBuilder.CreateFAdd(left, right, "faddtmp");
  }

  case TokenType::MINUS: {
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];

    if (lhsMeta->type.isPointer) {
      llvm::Type *baseTy = getLLVMType(lhsMeta->targetSymbol->type);

      llvm::Value *negRight = funcBuilder.CreateNeg(right, "neg_offset");
      return funcBuilder.CreateGEP(baseTy, left, negRight, "ptr_subtmp");
    }
    
    if (isIntegerType(resultType.kind))
      return funcBuilder.CreateSub(left, right, "subtmp");
    else
      return funcBuilder.CreateFSub(left, right, "fsubtmp");
  }

  case TokenType::ASTERISK: {
    if (isIntegerType(resultType.kind))
      return funcBuilder.CreateMul(left, right, "multmp");
    else
      return funcBuilder.CreateFMul(left, right, "fmultmp");
  }

  case TokenType::DIVIDE: {
    if (isIntegerType(resultType.kind))
      return isSignedInteger(resultType.kind)
                 ? funcBuilder.CreateSDiv(left, right, "divtmp")
                 : funcBuilder.CreateUDiv(left, right, "divtmp");
    else
      return funcBuilder.CreateFDiv(left, right, "fdivtmp");
  }

  case TokenType::MODULUS: {

    if (isIntegerType(resultType.kind))
      return isSignedInteger(resultType.kind)
                 ? funcBuilder.CreateSRem(left, right, "modtmp")
                 : funcBuilder.CreateURem(left, right, "modtmp");
    else
      throw std::runtime_error(
          "Modulus not supported for FLOAT or DOUBLE at line " +
          std::to_string(infix->operat.line));
  }

  default:
    throw std::runtime_error(
        "Unsupported infix operator: " + infix->operat.TokenLiteral +
        " at line " + std::to_string(infix->operat.line));
  }
}

// Prefix expression generator function
llvm::Value *IRGenerator::generatePrefixExpression(Node *node) {
  std::cout << "Inside prefix generator\n";
  auto prefix = dynamic_cast<PrefixExpression *>(node);
  if (!prefix)
    throw std::runtime_error("Invalid prefix expression");

  if (isGlobalScope)
    throw std::runtime_error(
        "Executable statements arent allowed in the global scope");

  llvm::Value *operand;

  // If the operand is an identifier
  if (auto identOperand = dynamic_cast<Identifier *>(prefix->operand.get())) {
    operand = generateIdentifierAddress(prefix->operand.get()).address;
  }

  // If the operand is a normal literal
  operand = generateExpression(prefix->operand.get());

  if (!operand)
    throw std::runtime_error("Failed to generate IR for prefix operand");

  const std::string &name = prefix->operand->expression.TokenLiteral;
  auto prefixIt = semantics.metaData.find(prefix);
  if (prefixIt == semantics.metaData.end()) {
    throw std::runtime_error("Unknown variable '" + name + "' in prefix");
  }

  ResolvedType resultType = prefixIt->second->type;

  // Helper to check if integer type
  auto isIntType = [&](DataType dt) { return isIntegerType(dt); };

  // Helper to get LLVM type from DataType
  auto getLLVMType = [&](DataType dt) -> llvm::Type * {
    if (dt == DataType::FLOAT)
      return llvm::Type::getFloatTy(context);
    if (dt == DataType::DOUBLE)
      return llvm::Type::getDoubleTy(context);
    if (isIntType(dt)) {
      unsigned bits = getIntegerBitWidth(dt);
      return llvm::Type::getIntNTy(context, bits);
    }
    return nullptr;
  };

  switch (prefix->operat.type) {
  case TokenType::MINUS:
    if (resultType.kind == DataType::FLOAT ||
        resultType.kind == DataType::DOUBLE)
      return funcBuilder.CreateFNeg(operand, llvm::Twine("fnegtmp"));
    else if (isIntType(resultType.kind))
      return funcBuilder.CreateNeg(operand, llvm::Twine("negtmp"));
    else
      throw std::runtime_error("Unsupported type for unary minus");

  case TokenType::BANG:
    // Boolean NOT
    return funcBuilder.CreateNot(operand, llvm::Twine("nottmp"));

  case TokenType::PLUS_PLUS:
  case TokenType::MINUS_MINUS: {
    auto ident = dynamic_cast<Identifier *>(prefix->operand.get());
    if (!ident)
      throw std::runtime_error("Prefix ++/-- must be used on a variable");

    const std::string &name = ident->expression.TokenLiteral;
    auto identIt = semantics.metaData.find(ident);
    if (identIt == semantics.metaData.end()) {
      throw std::runtime_error("Udefined variable '" + name + "'");
    }
    AddressAndPendingFree addrAndFree = generateIdentifierAddress(ident);
    llvm::Value *varPtr = addrAndFree.address;
    for (auto *freeCall : addrAndFree.pendingFrees) {
      funcBuilder.Insert(freeCall);
    }

    if (!varPtr)
      throw std::runtime_error("Null variable pointer for: " +
                               ident->identifier.TokenLiteral);

    // Get the type from resultType instead of getPointerElementType
    llvm::Type *varType = getLLVMType(resultType.kind);
    if (!varType)
      throw std::runtime_error("Invalid type for variable: " +
                               ident->identifier.TokenLiteral);

    llvm::Value *loaded =
        funcBuilder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

    llvm::Value *delta = nullptr;
    if (resultType.kind == DataType::FLOAT ||
        resultType.kind == DataType::DOUBLE) {
      delta = llvm::ConstantFP::get(varType, 1.0);
    } else if (isIntType(resultType.kind)) {
      unsigned bits = getIntegerBitWidth(resultType.kind);
      delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
    } else {
      throw std::runtime_error("Unsupported type for ++/--");
    }

    llvm::Value *updated = nullptr;
    if (prefix->operat.type == TokenType::PLUS_PLUS)
      updated =
          (resultType.kind == DataType::FLOAT ||
           resultType.kind == DataType::DOUBLE)
              ? funcBuilder.CreateFAdd(loaded, delta,
                                       llvm::Twine("preincfptmp"))
              : funcBuilder.CreateAdd(loaded, delta, llvm::Twine("preinctmp"));
    else
      updated =
          (resultType.kind == DataType::FLOAT ||
           resultType.kind == DataType::DOUBLE)
              ? funcBuilder.CreateFSub(loaded, delta,
                                       llvm::Twine("predecfptmp"))
              : funcBuilder.CreateSub(loaded, delta, llvm::Twine("predectmp"));

    funcBuilder.CreateStore(updated, varPtr);

    for (auto *freeCall : addrAndFree.pendingFrees) {
      funcBuilder.Insert(freeCall);
    }

    return updated;
  }

  default:
    throw std::runtime_error(
        "Unsupported prefix operator: " + prefix->operat.TokenLiteral +
        " at line " + std::to_string(prefix->operat.line));
  }
}

llvm::Value *IRGenerator::generatePostfixExpression(Node *node) {
  auto postfix = dynamic_cast<PostfixExpression *>(node);
  if (!postfix)
    throw std::runtime_error("Invalid postfix expression");

  auto identifier = dynamic_cast<Identifier *>(postfix->operand.get());
  if (!identifier)
    throw std::runtime_error("Postfix operand must be a variable");
  auto identName = identifier->identifier.TokenLiteral;
  auto identIt = semantics.metaData.find(identifier);

  AddressAndPendingFree addrAndFree = generateIdentifierAddress(identifier);
  llvm::Value *varPtr = addrAndFree.address;
  for (auto *freeCall : addrAndFree.pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  if (!varPtr)
    throw std::runtime_error("Null variable pointer for: " +
                             identifier->identifier.TokenLiteral);

  auto postfixIt = semantics.metaData.find(postfix);
  if (postfixIt == semantics.metaData.end()) {
    throw std::runtime_error("Variable '" + identName + "' does not exist");
  }

  std::cerr << "[IR-DEBUG] ident lastUse = "
            << (identIt->second->lastUseNode
                    ? typeid(*identIt->second->lastUseNode).name()
                    : "NULL")
            << ", postfix lastUse = "
            << (postfixIt != semantics.metaData.end() &&
                        postfixIt->second->lastUseNode
                    ? typeid(*postfixIt->second->lastUseNode).name()
                    : "NULL")
            << "\n";

  if (postfixIt->second->hasError)
    return nullptr;

  ResolvedType resultType = postfixIt->second->type;

  // Helper to check if integer type
  auto isIntType = [&](DataType dt) { return isIntegerType(dt); };

  // Helper to get LLVM type from DataType
  auto getLLVMType = [&](DataType dt) -> llvm::Type * {
    if (dt == DataType::FLOAT)
      return llvm::Type::getFloatTy(context);
    if (dt == DataType::DOUBLE)
      return llvm::Type::getDoubleTy(context);
    if (isIntType(dt)) {
      unsigned bits = getIntegerBitWidth(dt);
      return llvm::Type::getIntNTy(context, bits);
    }
    return nullptr;
  };

  // Get the type from resultType for CreateLoad
  llvm::Type *varType = getLLVMType(resultType.kind);
  if (!varType)
    throw std::runtime_error("Invalid type for variable: " +
                             identifier->identifier.TokenLiteral);

  llvm::Value *originalValue =
      funcBuilder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

  llvm::Value *delta = nullptr;
  if (resultType.kind == DataType::FLOAT ||
      resultType.kind == DataType::DOUBLE) {
    delta = llvm::ConstantFP::get(varType, 1.0);
  } else if (isIntType(resultType.kind)) {
    unsigned bits = getIntegerBitWidth(resultType.kind);
    delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
  } else {
    throw std::runtime_error("Unsupported type for ++/--");
  }

  llvm::Value *updatedValue = nullptr;

  if (postfix->operator_token.type == TokenType::PLUS_PLUS)
    updatedValue =
        (resultType.kind == DataType::FLOAT ||
         resultType.kind == DataType::DOUBLE)
            ? funcBuilder.CreateFAdd(originalValue, delta, llvm::Twine("finc"))
            : funcBuilder.CreateAdd(originalValue, delta, llvm::Twine("inc"));
  else if (postfix->operator_token.type == TokenType::MINUS_MINUS)
    updatedValue =
        (resultType.kind == DataType::FLOAT ||
         resultType.kind == DataType::DOUBLE)
            ? funcBuilder.CreateFSub(originalValue, delta, llvm::Twine("fdec"))
            : funcBuilder.CreateSub(originalValue, delta, llvm::Twine("dec"));
  else
    throw std::runtime_error("Unsupported postfix operator: " +
                             postfix->operator_token.TokenLiteral +
                             " at line " +
                             std::to_string(postfix->operator_token.line));

  funcBuilder.CreateStore(updatedValue, varPtr);

  for (auto *freeCall : addrAndFree.pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  // Return original value since postfix
  return originalValue;
}

llvm::Value *IRGenerator::generateStringLiteral(Node *node) {
  std::cout << "INSIDE GENERATE IR FOR STRING\n";
  auto strLit = dynamic_cast<StringLiteral *>(node);
  if (!strLit) {
    throw std::runtime_error("Invalid string literal");
  }
  auto it = semantics.metaData.find(strLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("String literal not found in metadata");
  }
  DataType dt = it->second->type.kind;

  if (dt != DataType::STRING) {
    throw std::runtime_error("Type error: Expected STRING");
  }
  std::string raw = strLit->string_token.TokenLiteral;
  llvm::Constant *strConst =
      llvm::ConstantDataArray::getString(context, raw, true);

  auto *globalStr = new llvm::GlobalVariable(*module, strConst->getType(), true,
                                             llvm::GlobalValue::PrivateLinkage,
                                             strConst, ".str");

  // Pointer to first element
  llvm::Constant *zero =
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
  llvm::Constant *indices[] = {zero, zero};
  llvm::Constant *strPtr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      strConst->getType(), globalStr, indices);

  return strPtr;
}

llvm::Value *IRGenerator::generateChar8Literal(Node *node) {
  auto charLit = dynamic_cast<Char8Literal *>(node);
  if (!charLit) {
    throw std::runtime_error("Invalid char literal");
  }
  auto it = semantics.metaData.find(charLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Char literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::CHAR8) {
    throw std::runtime_error("Type error: Expected CHAR for CharLiteral");
  }
  std::string tokenLiteral = charLit->char8_token.TokenLiteral;

  char c = decodeCharLiteral(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                static_cast<uint8_t>(c), false);
}

llvm::Value *IRGenerator::generateChar16Literal(Node *node) {
  auto char16Lit = dynamic_cast<Char16Literal *>(node);
  if (!char16Lit) {
    throw std::runtime_error("Invalid char 16 literal");
  }
  auto it = semantics.metaData.find(char16Lit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Char16 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::CHAR16) {
    throw std::runtime_error("Type error: Expected CHAR16 for Char16Literal");
  }
  std::string tokenLiteral = char16Lit->char16_token.TokenLiteral;
  uint16_t c = decodeCharLiteral(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), c, false);
}

llvm::Value *IRGenerator::generateChar32Literal(Node *node) {
  auto char32Lit = dynamic_cast<Char32Literal *>(node);
  if (!char32Lit) {
    throw std::runtime_error("Invalid char 32 literal");
  }
  auto it = semantics.metaData.find(char32Lit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Char16 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::CHAR16) {
    throw std::runtime_error("Type error: Expected CHAR16 for Char16Literal");
  }
  std::string tokenLiteral = char32Lit->char32_token.TokenLiteral;
  uint32_t c = decodeChar32Literal(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), c, false);
}

llvm::Value *IRGenerator::generateBooleanLiteral(Node *node) {
  auto boolLit = dynamic_cast<BooleanLiteral *>(node);
  if (!boolLit) {
    throw std::runtime_error("Invalid boolean type");
  }
  auto it = semantics.metaData.find(boolLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Boolean literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::BOOLEAN) {
    throw std::runtime_error("Type error: Expected BOOLEAN for BooleanLiteral");
  }

  bool value = (boolLit->boolean_token.TokenLiteral == "true");

  return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), value);
}

llvm::Value *IRGenerator::generateI8Literal(Node *node) {
  auto i8Lit = dynamic_cast<I8Literal *>(node);
  if (!i8Lit)
    throw std::runtime_error("Invalid i8 literal");

  auto it = semantics.metaData.find(node);
  if (it == semantics.metaData.end())
    throw std::runtime_error("i8 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::I8)
    throw std::runtime_error("Type error: Expected i8");

  int16_t value = static_cast<int8_t>(std::stoi(i8Lit->i8_token.TokenLiteral));
  return llvm::ConstantInt::get(context, llvm::APInt(8, value, true));
}

llvm::Value *IRGenerator::generateU8Literal(Node *node) {
  auto u8Lit = dynamic_cast<U8Literal *>(node);
  if (!u8Lit)
    throw std::runtime_error("Invalid u8 literal");

  auto it = semantics.metaData.find(node);
  if (it == semantics.metaData.end())
    throw std::runtime_error("u8 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::U8)
    throw std::runtime_error("Type error: Expected u8");

  int16_t value =
      static_cast<uint16_t>(std::stoi(u8Lit->u8_token.TokenLiteral));
  return llvm::ConstantInt::get(context, llvm::APInt(8, value, true));
}

llvm::Value *IRGenerator::generateI16Literal(Node *node) {
  auto i16Lit = dynamic_cast<I16Literal *>(node);
  if (!i16Lit)
    throw std::runtime_error("Invalid i16 literal");

  auto it = semantics.metaData.find(node);
  if (it == semantics.metaData.end())
    throw std::runtime_error("i16 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::I16)
    throw std::runtime_error("Type error: Expected i16");

  int16_t value =
      static_cast<int16_t>(std::stoi(i16Lit->i16_token.TokenLiteral));
  return llvm::ConstantInt::get(context, llvm::APInt(16, value, true));
}

llvm::Value *IRGenerator::generateU16Literal(Node *node) {
  auto u16Lit = dynamic_cast<U16Literal *>(node);
  if (!u16Lit)
    throw std::runtime_error("Invalid ushort literal");

  auto it = semantics.metaData.find(node);
  if (it == semantics.metaData.end())
    throw std::runtime_error("u16 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::U16)
    throw std::runtime_error("Type error: Expected u16");

  uint16_t value =
      static_cast<uint16_t>(std::stoul(u16Lit->u16_token.TokenLiteral));
  return llvm::ConstantInt::get(context, llvm::APInt(16, value, false));
}

llvm::Value *IRGenerator::generateI32Literal(Node *node) {
  auto i32Lit = dynamic_cast<I32Literal *>(node);
  if (!i32Lit) {
    throw std::runtime_error("Invalid i32 literal");
  }
  auto it = semantics.metaData.find(i32Lit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error(
        "i32 literal not found in metadata at line:" +
        std::to_string(i32Lit->expression.line) +
        " and column: " + std::to_string(i32Lit->expression.column));
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::I32) {
    throw std::runtime_error("Type error: Expected i32");
  }
  int64_t value = std::stoll(i32Lit->i32_token.TokenLiteral);
  return llvm::ConstantInt::get(context, llvm::APInt(32, value, true));
}

llvm::Value *IRGenerator::generateU32Literal(Node *node) {
  auto u32Lit = dynamic_cast<U32Literal *>(node);
  if (!u32Lit)
    throw std::runtime_error("Invalid u32 literal");

  auto it = semantics.metaData.find(u32Lit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Uint literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::U32)
    throw std::runtime_error("Type error: Expected u32");

  uint32_t value =
      static_cast<uint32_t>(std::stoul(u32Lit->u32_token.TokenLiteral));
  return llvm::ConstantInt::get(context, llvm::APInt(32, value, false));
}

llvm::Value *IRGenerator::generateI64Literal(Node *node) {
  auto i64Lit = dynamic_cast<I64Literal *>(node);
  if (!i64Lit)
    throw std::runtime_error("Invalid i64 literal");

  auto it = semantics.metaData.find(i64Lit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("i64 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::I64)
    throw std::runtime_error("Type error: Expected i64 ");

  int64_t value = std::stoll(i64Lit->i64_token.TokenLiteral);
  return llvm::ConstantInt::get(context, llvm::APInt(64, value, true));
}

llvm::Value *IRGenerator::generateU64Literal(Node *node) {
  auto u64Lit = dynamic_cast<U64Literal *>(node);
  if (!u64Lit)
    throw std::runtime_error("Invalid u64 literal");

  auto it = semantics.metaData.find(u64Lit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("u64 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::U64)
    throw std::runtime_error("Type error: Expected u64");

  uint64_t value = std::stoull(u64Lit->u64_token.TokenLiteral);
  return llvm::ConstantInt::get(context, llvm::APInt(64, value, false));
}

llvm::Value *IRGenerator::generateI128Literal(Node *node) {
  auto i128Lit = dynamic_cast<I128Literal *>(node);
  if (!i128Lit)
    throw std::runtime_error("Invalid i128 literal");

  auto it = semantics.metaData.find(i128Lit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("i128 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::I128)
    throw std::runtime_error("Type error: Expected i128");

  llvm::APInt value(128, i128Lit->i128_token.TokenLiteral, 10);
  return llvm::ConstantInt::get(context, value);
}

llvm::Value *IRGenerator::generateU128Literal(Node *node) {
  auto u128Lit = dynamic_cast<U128Literal *>(node);
  if (!u128Lit)
    throw std::runtime_error("Invalid u128 literal");

  auto it = semantics.metaData.find(u128Lit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("u128 literal not found in metadata");

  DataType dt = it->second->type.kind;
  if (dt != DataType::U128)
    throw std::runtime_error("Type error: Expected u128 ");

  llvm::APInt value(128, u128Lit->u128_token.TokenLiteral, 10);
  return llvm::ConstantInt::get(context, value);
}

llvm::Value *IRGenerator::generateISIZELiteral(Node *node) {
  auto isizeLit = dynamic_cast<ISIZELiteral *>(node);
  if (!isizeLit)
    throw std::runtime_error("Invalid isize literal");

  auto it = semantics.metaData.find(isizeLit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("isize literal not found in metadata");

  // Get Native Pointer Width
  unsigned int ptrWidth = module->getDataLayout().getPointerSizeInBits();

  // Parse the string value
  // stoll handles signed 64-bit integers
  int64_t value = std::stoll(isizeLit->isize_token.TokenLiteral);

  // Create the ConstantInt with the DYNAMIC bit width
  return llvm::ConstantInt::get(context, llvm::APInt(ptrWidth, value, true));
}

llvm::Value *IRGenerator::generateUSIZELiteral(Node *node) {
  auto usizeLit = dynamic_cast<USIZELiteral *>(node);
  if (!usizeLit)
    throw std::runtime_error("Invalid usize literal");

  auto it = semantics.metaData.find(usizeLit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("usize literal not found in metadata");

  // Get Native Pointer Width
  unsigned int ptrWidth = module->getDataLayout().getPointerSizeInBits();

  // Parse the string value
  // Using stoull because it handles the largest possible unsigned 64-bit int
  uint64_t value = std::stoull(usizeLit->usize_token.TokenLiteral);

  // Create the ConstantInt with the DYNAMIC bit width
  return llvm::ConstantInt::get(context, llvm::APInt(ptrWidth, value, false));
}

llvm::Value *IRGenerator::generateFloatLiteral(Node *node) {
  auto fltLit = dynamic_cast<FloatLiteral *>(node);
  if (!fltLit) {
    throw std::runtime_error("Invalid float literal");
  }
  auto it = semantics.metaData.find(fltLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Float literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::FLOAT) {
    throw std::runtime_error("Type error: Expected Float for FloatLiteral ");
  }
  float value = std::stof(fltLit->float_token.TokenLiteral);
  return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
}

llvm::Value *IRGenerator::generateDoubleLiteral(Node *node) {
  auto dbLit = dynamic_cast<DoubleLiteral *>(node);
  if (!dbLit) {
    throw std::runtime_error("Invalid double literal");
  }
  auto it =
      semantics.metaData.find(dbLit); // Creating an iterator to find specific
                                      // meta data about the double literal
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Double literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::DOUBLE) {
    throw std::runtime_error("Type error: Expected DOUBLE for DoubleLiteral");
  }
  // Checking if we have metaData about the double literal and if so we check to
  // see if the data type is double
  double value = std::stod(
      dbLit->double_token.TokenLiteral); // Converting the double literal from a
                                         // string to a double
  return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context),
                               value); // Returning double value
}

llvm::Value *IRGenerator::generateArrayLiteral(Node *node) {
  auto arrLit = dynamic_cast<ArrayLiteral *>(node);
  if (!arrLit)
    throw std::runtime_error("Invalid array literal");

  auto it = semantics.metaData.find(arrLit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Array literal not found in metaData");

  auto sym = it->second;

  // Base element type
  llvm::Type *llvmElemTy = nullptr;

  size_t arraySize = arrLit->array.size();
  // llvm::ArrayType *arrayTy = llvm::ArrayType::get(llvmElemTy, arraySize);

  // Create the vector of element constants
  std::vector<llvm::Constant *> elems;
  elems.reserve(arraySize);

  for (size_t i = 0; i < arraySize; ++i) {
    Node *child = arrLit->array[i].get();
    llvm::Value *currentVal = nullptr;

    if (auto nested = dynamic_cast<ArrayLiteral *>(child)) {
      currentVal = generateArrayLiteral(nested);
      // Recursive constant array
      llvm::Constant *nestedConst = llvm::cast<llvm::Constant>(currentVal);
      elems.push_back(nestedConst);

      if (i == 0) {
        llvmElemTy = nestedConst->getType();
      }
    } else {
      currentVal = generateExpression(child);

      auto *constVal = llvm::dyn_cast<llvm::Constant>(currentVal);
      if (!constVal)
        throw std::runtime_error("Array literal element is not constant");

      elems.push_back(constVal);

      if (i == 0) {
        llvmElemTy = constVal->getType();
      }
    }
  }

  if (elems.empty() || !llvmElemTy) {
    throw std::runtime_error("Cannot infer element type for array literal.");
  }

  llvm::ArrayType *arrayTy = llvm::ArrayType::get(llvmElemTy, arraySize);

  // Return a constant LLVM array
  return llvm::ConstantArray::get(arrayTy, elems);
}

llvm::Value *IRGenerator::generateNullLiteral(NullLiteral *nullLit,
                                              DataType type) {
  switch (type) {
  case DataType::STRING:
    return llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0));

  case DataType::I8:
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                  llvm::APInt(8, INT8_MIN, true));

  case DataType::U8:
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                  llvm::APInt(8, 0));

  case DataType::I16:
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),
                                  llvm::APInt(16, INT16_MIN, true));

  case DataType::U16:
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),
                                  llvm::APInt(16, 0));

  case DataType::I32:
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                  llvm::APInt(32, INT32_MIN, true));

  case DataType::U32:
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                  llvm::APInt(32, 0));

  case DataType::I64:
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                  llvm::APInt(64, INT64_MIN, true));

  case DataType::U64:
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                  llvm::APInt(64, 0));

  case DataType::I128:
  case DataType::U128:
    return llvm::ConstantInt::get(llvm::Type::getInt128Ty(context),
                                  llvm::APInt(128, 0));

  case DataType::FLOAT:
    return llvm::ConstantFP::getNaN(llvm::Type::getFloatTy(context));

  case DataType::DOUBLE:
    return llvm::ConstantFP::getNaN(llvm::Type::getDoubleTy(context));

  case DataType::BOOLEAN:
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),
                                  llvm::APInt(1, 0));

  case DataType::CHAR8:
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                  llvm::APInt(8, 0));

  case DataType::CHAR16:
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),
                                  llvm::APInt(16, 0));

  case DataType::CHAR32:
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                  llvm::APInt(32, 0));

  default:
    throw std::runtime_error(
        "Unsupported nullable data type in generateNullLiteral");
  }
}

// Generator function for identifier expression
llvm::Value *IRGenerator::generateIdentifierExpression(Node *node) {
  std::cout << "INSIDE IDENTIFIER GEN\n";
  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr)
    throw std::runtime_error("Invalid identifier expression :" +
                             node->toString());

  const std::string &identName = identExpr->identifier.TokenLiteral;

  // Lookup symbol
  auto metaIt = semantics.metaData.find(identExpr);
  if (metaIt == semantics.metaData.end())
    throw std::runtime_error("Unidentified identifier '" + identName + "'");

  auto sym = metaIt->second;
  if (sym->hasError)
    throw std::runtime_error("Semantic error detected ");

  // Get address and possible pending free
  AddressAndPendingFree addrInfo = generateIdentifierAddress(identExpr);
  llvm::Value *variableAddr = addrInfo.address;
  if (!variableAddr)
    throw std::runtime_error("No llvm address for '" + identName + "'");

  // Component instance -> return pointer to the struct instance (address is
  // already correct)
  auto compIt = componentTypes.find(sym->type.resolvedName);
  if (compIt != componentTypes.end()) {
    return variableAddr;
  }

  // Heap scalar: variableAddr is a T* (runtime pointer). Load T from it.
  if (sym->isHeap) {
    llvm::Type *elemTy = sym->llvmType;
    if (!elemTy)
      throw std::runtime_error("llvmType null for heap scalar '" + identName +
                               "'");

    // variableAddr should be T* (address of object). If not, bitcast it.
    llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
    if (variableAddr->getType() != expectedPtrTy)
      variableAddr = funcBuilder.CreateBitCast(variableAddr, expectedPtrTy,
                                               identName + "_ptr_typed");

    // Load the value
    llvm::Value *loadedVal =
        funcBuilder.CreateLoad(elemTy, variableAddr, identName + "_val");

    // If we prepared a pending free, insert it NOW (after load)
    for (auto *freeCall : addrInfo.pendingFrees) {
      funcBuilder.Insert(freeCall);
    }

    std::cout << "[DEBUG] Returning heap scalar '" << identName
              << "' (lastUse=" << (sym->lastUseNode == identExpr ? "yes" : "no")
              << ")\n";
    return loadedVal;
  }
  if (sym->isDheap) {
    llvm::Type *elemTy = sym->llvmType;
    if (!elemTy)
      throw std::runtime_error("llvmType null for dheap scalar '" + identName +
                               "'");

    // Grab the value from the heap address
    llvm::Value *loadedVal =
        funcBuilder.CreateLoad(elemTy, variableAddr, identName + "_val");

    // Now that the value is safe in a register, we can release the
    // memory
    for (auto *freeCall : addrInfo.pendingFrees) {
      funcBuilder.Insert(freeCall);
    }

    return loadedVal;
  }

  // Non-heap scalar: variableAddr is a pointer to T, just load
  llvm::Type *identType = getLLVMType(sym->type);
  if (!identType)
    throw std::runtime_error("llvmType null for scalar '" + identName + "'");

  // variableAddr should already be a pointer, load from it
  llvm::Value *val =
      funcBuilder.CreateLoad(identType, variableAddr, identName + "_val");

  std::cout << "ENDED IDENTIFIER GEN\n";
  return val;
}

llvm::Value *IRGenerator::generateAddressExpression(Node *node) {
  auto addrExpr = dynamic_cast<AddressExpression *>(node);
  if (!addrExpr)
    throw std::runtime_error("Invalid address expression");

  std::cout << "Inside the address expression generator\n";

  const std::string &name = addrExpr->identifier->expression.TokenLiteral;

  auto metaIt = semantics.metaData.find(addrExpr);
  if (metaIt == semantics.metaData.end())
    throw std::runtime_error("Unidentified address identifier '" + name + "'");

  auto sym = metaIt->second;

  if (sym->hasError)
    throw std::runtime_error("Semantic error detected");

  auto targetSym = sym->targetSymbol;
  llvm::Value *ptr = targetSym->llvmValue;
  if (!ptr)
    throw std::runtime_error("No llvm value was assigned");

  return ptr;
}

llvm::Value *IRGenerator::generateDereferenceExpression(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);

  Node *current = node;
  int derefCount = 0;
  while (auto nested = dynamic_cast<DereferenceExpression *>(current)) {
    derefCount++;
    current = nested->identifier.get();
  }
  auto identNode = dynamic_cast<Identifier *>(current);

  // Get the starting address of the variable
  AddressAndPendingFree out = generateIdentifierAddress(identNode);
  llvm::Value *addr = out.address;
  auto meta = semantics.metaData[node];

  // This gets us the address of 'p' from 'pp'
  addr = funcBuilder.CreateLoad(getLLVMType(meta->derefPtrType), addr,
                                "base_lift");

  for (int i = 0; i < derefCount; i++) {
    llvm::Type *loadTy;
    if (i < derefCount - 1) {
      loadTy = llvm::PointerType::getUnqual(context);
    } else {
      loadTy = getLLVMType(meta->type);
    }
    addr = funcBuilder.CreateLoad(loadTy, addr, "deref_hop");
  }

  for (auto *freeCall : out.pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  return addr;
}

llvm::Value *IRGenerator::generateDereferenceAddress(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);

  Node *current = node;
  int derefCount = 0;
  while (auto nested = dynamic_cast<DereferenceExpression *>(current)) {
    derefCount++;
    current = nested->identifier.get();
  }
  auto identNode = dynamic_cast<Identifier *>(current);

  AddressAndPendingFree out = generateIdentifierAddress(identNode);
  llvm::Value *addr = out.address;
  auto meta = semantics.metaData[node];

  addr = funcBuilder.CreateLoad(getLLVMType(meta->derefPtrType), addr,
                                "base_lift");

  for (int i = 0; i < derefCount - 1; i++) {
    addr = funcBuilder.CreateLoad(llvm::PointerType::getUnqual(context), addr,
                                  "deref_hop_addr");
  }

  for (auto *freeCall : out.pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  return addr;
}

AddressAndPendingFree IRGenerator::generateIdentifierAddress(Node *node) {
  AddressAndPendingFree out{nullptr, {}};

  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr)
    throw std::runtime_error("Invalid identifier expression: " +
                             node->toString());

  const std::string &identName = identExpr->identifier.TokenLiteral;
  auto metaIt = semantics.metaData.find(identExpr);
  if (metaIt == semantics.metaData.end())
    throw std::runtime_error("Unidentified identifier '" + identName + "'");

  auto sym = metaIt->second;
  if (sym->hasError)
    throw std::runtime_error("Semantic error detected ");

  llvm::Value *variablePtr = sym->llvmValue;
  if (!variablePtr)
    throw std::runtime_error("No llvm value for '" + identName + "'");

  // Component instance -> pointer to struct
  auto compIt = componentTypes.find(sym->type.resolvedName);
  if (compIt != componentTypes.end()) {
    out.address = variablePtr; // already a pointer to struct instance
  } else {
    // scalar/heap -> ensure typed pointer
    if (sym->isHeap) {
      std::cout << "The identifier is heap raised\n";
      llvm::Type *elemTy = sym->llvmType;
      if (!elemTy)
        throw std::runtime_error("llvmType null for heap scalar '" + identName +
                                 "'");

      // If sym->llvmValue is a GlobalVariable (the module slot that stores T*),
      // we must load the runtime pointer from it.
      if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(variablePtr)) {
        std::cout << "Inside global heap var path PATH1 \n";
        llvm::PointerType *ptrType = llvm::PointerType::get(
            funcBuilder.getContext(), 0); // Generic ptr type
        llvm::Value *runtimePtr =
            funcBuilder.CreateLoad(ptrType, gv, identName + "_runtime_ptr");
        out.address = runtimePtr; // address usable for subsequent loads/stores
      } else {
        std::cout << "Inside global heap var path PATH2c \n";
        // variablePtr might already be a direct pointer (alloca or
        // bitcast), ensure type matches
        llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
        if (variablePtr->getType() != expectedPtrTy)
          variablePtr = funcBuilder.CreateBitCast(variablePtr, expectedPtrTy,
                                                  identName + "_ptr_typed");
        out.address = variablePtr;
      }
    } else {
      if (variablePtr->getType()->isPointerTy()) {
        std::cout << "Taken raw_ptr path\n";
        out.address = variablePtr;
      } else {
        throw std::runtime_error("Identifier '" + identName +
                                 "' does not have pointer-like llvmValue");
      }
    }
  }

  // Should not free if this needs a post loop free
  if (sym->needsPostLoopFree) {
    return out;
  }
  // Prepare pending free call (create CallInst but don't insert)
  if (sym->isHeap) {
    bool shouldFree = identExpr->isKiller ||
                      ((sym->lastUseNode == identExpr) && (sym->refCount == 0));
    if (shouldFree) {
      std::cout << "[IR DEBUG] Freeing '" << identName
                << "' | popCount discovered: " << sym->popCount << "\n";
      llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
          "sage_free", llvm::Type::getVoidTy(context));

      int weight = (sym->popCount > 0) ? sym->popCount : 1;

      for (int i = 0; i < weight; i++) {
        llvm::CallInst *callInst = llvm::CallInst::Create(sageFreeFn);
        callInst->setCallingConv(llvm::CallingConv::C);
        // Stash it in the vector
        out.pendingFrees.push_back(callInst);
      }
    }
  } else if (sym->isDheap) {
    // Determine if we should trigger a free
    bool shouldFree = identExpr->isKiller ||
                      ((sym->lastUseNode == identExpr) && (sym->refCount == 0));

    if (shouldFree) {
      // Get allocator info
      const std::string &allocatorTypeName = sym->allocType;
      auto it = semantics.allocatorMap.find(allocatorTypeName);

      if (it != semantics.allocatorMap.end()) {
        auto handle = it->second;
        llvm::Function *freeFunc = module->getFunction(handle.freeName);

        if (freeFunc) {
          llvm::Value *ptrToFree = sym->llvmValue;

          // Create the single Call Instruction
          llvm::CallInst *callInst =
              llvm::CallInst::Create(freeFunc, {ptrToFree}, "");
          callInst->setCallingConv(llvm::CallingConv::C);

          out.pendingFrees.push_back(callInst);
        }
      }
    }
  }

  return out;
}

llvm::Value *IRGenerator::generateSelfExpression(Node *node) {
  auto selfExpr = dynamic_cast<SelfExpression *>(node);
  if (!selfExpr)
    throw std::runtime_error("Invalid self expression");

  std::cout << "[IR] Generating IR for self expression: "
            << selfExpr->toString() << "\n";

  const std::string &compName =
      currentComponent->component_name->expression.TokenLiteral;

  // Lookup LLVM struct for top-level component
  llvm::StructType *currentStructTy = nullptr;
  auto it = componentTypes.find(compName);
  if (it == componentTypes.end())
    throw std::runtime_error("Component '" + compName +
                             "' not found in componentTypes");

  currentStructTy = it->second;

  // --- Load 'self' pointer ---
  llvm::AllocaInst *selfAlloca = currentFunctionSelfMap[currentFunction];
  if (!selfAlloca)
    throw std::runtime_error("'self' access outside component method");

  llvm::Value *currentPtr = funcBuilder.CreateLoad(
      currentStructTy->getPointerTo(), selfAlloca, "self_load");

  // --- Semantic chain walk ---
  auto ctIt = semantics.customTypesTable.find(compName);
  if (ctIt == semantics.customTypesTable.end())
    throw std::runtime_error("Component not found in customTypesTable");

  auto currentTypeInfo = ctIt->second;
  std::shared_ptr<MemberInfo> lastMemberInfo = nullptr;

  for (size_t i = 0; i < selfExpr->fields.size(); ++i) {
    auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
    if (!ident)
      throw std::runtime_error("Expected identifier in self chain");

    std::string fieldName = ident->identifier.TokenLiteral;
    std::cout << "[IR] Accessing field: " << fieldName
              << " in type: " << currentTypeInfo->type.resolvedName << "\n";

    auto memIt = currentTypeInfo->members.find(fieldName);
    if (memIt == currentTypeInfo->members.end())
      throw std::runtime_error("Field not found in CustomTypeInfo");

    lastMemberInfo = memIt->second;

    // --- GEP for this field ---
    auto llvmIt = llvmCustomTypes.find(currentTypeInfo->type.resolvedName);
    if (llvmIt == llvmCustomTypes.end())
      throw std::runtime_error("LLVM struct missing for type " +
                               currentTypeInfo->type.resolvedName);

    llvm::StructType *structTy = llvmIt->second;

    currentPtr = funcBuilder.CreateStructGEP(
        structTy, currentPtr, lastMemberInfo->memberIndex, fieldName + "_ptr");

    // --- Drill into nested type if needed ---
    if (lastMemberInfo->type.kind == DataType::COMPONENT ||
        lastMemberInfo->type.kind == DataType::DATABLOCK) {
      auto nestedIt =
          semantics.customTypesTable.find(lastMemberInfo->type.resolvedName);
      if (nestedIt == semantics.customTypesTable.end())
        throw std::runtime_error("Nested type not found in customTypesTable");

      currentTypeInfo = nestedIt->second;
    } else {
      // primitive reached, stop drilling
      currentTypeInfo = nullptr;
    }
  }

  // --- Final load ---
  llvm::Type *finalTy = getLLVMType(lastMemberInfo->type);
  return funcBuilder.CreateLoad(finalTy, currentPtr,
                                selfExpr->fields.back()->toString() + "_val");
}

llvm::Value *IRGenerator::generateSelfAddress(Node *node) {
  auto selfExpr = dynamic_cast<SelfExpression *>(node);
  if (!selfExpr)
    throw std::runtime_error("Invalid self expression");

  std::cout << "[IR] Generating IR for self address: " << selfExpr->toString()
            << "\n";

  const std::string &compName =
      currentComponent->component_name->expression.TokenLiteral;

  // Lookup LLVM struct for top-level component
  llvm::StructType *currentStructTy = nullptr;
  auto it = componentTypes.find(compName);
  if (it == componentTypes.end())
    throw std::runtime_error("Component '" + compName +
                             "' not found in componentTypes");

  currentStructTy = it->second;

  // --- Load 'self' pointer ---
  llvm::AllocaInst *selfAlloca = currentFunctionSelfMap[currentFunction];
  if (!selfAlloca)
    throw std::runtime_error("'self' access outside component method");

  llvm::Value *currentPtr = funcBuilder.CreateLoad(
      currentStructTy->getPointerTo(), selfAlloca, "self_load");

  // --- Semantic chain walk ---
  auto ctIt = semantics.customTypesTable.find(compName);
  if (ctIt == semantics.customTypesTable.end())
    throw std::runtime_error("Component not found in customTypesTable");

  auto currentTypeInfo = ctIt->second;
  std::shared_ptr<MemberInfo> lastMemberInfo = nullptr;

  for (size_t i = 0; i < selfExpr->fields.size(); ++i) {
    auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
    if (!ident)
      throw std::runtime_error("Expected identifier in self chain");

    std::string fieldName = ident->identifier.TokenLiteral;
    std::cout << "[IR] Accessing field: " << fieldName
              << " in type: " << currentTypeInfo->type.resolvedName << "\n";

    auto memIt = currentTypeInfo->members.find(fieldName);
    if (memIt == currentTypeInfo->members.end())
      throw std::runtime_error("Field not found in CustomTypeInfo");

    lastMemberInfo = memIt->second;

    // --- GEP for this field ---
    auto llvmIt = llvmCustomTypes.find(currentTypeInfo->type.resolvedName);
    if (llvmIt == llvmCustomTypes.end())
      throw std::runtime_error("LLVM struct missing for type " +
                               currentTypeInfo->type.resolvedName);

    llvm::StructType *structTy = llvmIt->second;

    currentPtr = funcBuilder.CreateStructGEP(
        structTy, currentPtr, lastMemberInfo->memberIndex, fieldName + "_ptr");

    // --- Drill into nested type if needed ---
    if (lastMemberInfo->type.kind == DataType::COMPONENT ||
        lastMemberInfo->type.kind == DataType::DATABLOCK) {
      auto nestedIt =
          semantics.customTypesTable.find(lastMemberInfo->type.resolvedName);
      if (nestedIt == semantics.customTypesTable.end())
        throw std::runtime_error("Nested type not found in customTypesTable");

      currentTypeInfo = nestedIt->second;
    } else {
      // primitive reached, stop drilling
      currentTypeInfo = nullptr;
    }
  }

  return currentPtr;
}

llvm::Value *IRGenerator::generateSizeOfExpression(Node *node) {
  auto sizeOf = dynamic_cast<SizeOfExpression *>(node);
  if (!sizeOf)
    throw std::runtime_error("Invalid sizeof expression");

  Expression *typeExpr = sizeOf->type.get();
  ResolvedType type = semantics.inferNodeDataType(typeExpr);

  llvm::Type *llvmTY = getLLVMType(type);

  uint64_t size = layout->getTypeAllocSize(llvmTY);
  llvm::IntegerType *usizeTy = layout->getIntPtrType(context);
  return llvm::ConstantInt::get(usizeTy, size);
}

llvm::Value *IRGenerator::generateBlockExpression(Node *node) {
  auto blockExpr = dynamic_cast<BlockExpression *>(node);
  if (!blockExpr)
    throw std::runtime_error("Invalid block expression");

  for (const auto &stmts : blockExpr->statements) {
    // Check if the current block is already terminated by a return or branch
    if (funcBuilder.GetInsertBlock()->getTerminator()) {
      std::cout << "SKIPPING statement - block terminated\n";
      break;
    }
    generateStatement(stmts.get());
  }

  // A block expression should not return an llvm::Value directly.
  return nullptr;
}

llvm::Value *IRGenerator::generateArraySubscriptAddress(Node *node) {
  auto arrExpr = dynamic_cast<ArraySubscript *>(node);
  auto baseName = arrExpr->identifier->expression.TokenLiteral;

  auto arrIt = semantics.metaData.find(arrExpr);
  if (arrIt == semantics.metaData.end())
    throw std::runtime_error("Could not find array index metaData");

  auto baseSym = arrIt->second;
  if (baseSym->hasError)
    throw std::runtime_error("Semantic error detected");

  // Getting the local address
  llvm::Value *ptr =
      generateIdentifierAddress(arrExpr->identifier.get()).address;

  llvm::Type *currentLevelTy = getLLVMType(baseSym->arrayTyInfo.underLyingType);

  std::vector<llvm::Value *> indices;

  // Wrap the type and convert it into an array format
  const auto &dims = baseSym->arrayTyInfo.sizePerDimension;
  for (int i = dims.size() - 1; i >= 0; i--) {
    currentLevelTy = llvm::ArrayType::get(currentLevelTy, dims[i]);
  }

  indices.push_back(funcBuilder.getInt64(0));

  for (const auto &idxExpr : arrExpr->index_exprs) {
    llvm::Value *v = generateExpression(idxExpr.get());
    indices.push_back(
        funcBuilder.CreateIntCast(v, funcBuilder.getInt64Ty(), false));
  }

  return funcBuilder.CreateGEP(currentLevelTy, ptr, indices, "element_ptr");
}

llvm::Value *IRGenerator::generateArraySubscriptExpression(Node *node) {
  auto arrExpr = dynamic_cast<ArraySubscript *>(node);
  if (!arrExpr)
    throw std::runtime_error("Invalid array access expression");

  auto arrMetaIt = semantics.metaData.find(arrExpr);
  if (arrMetaIt == semantics.metaData.end()) {
    throw std::runtime_error("Could not find array subscript metaData");
  }
  auto arrSym = arrMetaIt->second;

  if (arrSym->hasError) {
    throw std::runtime_error("Semantic error was detected in array subscript");
  }

  AddressAndPendingFree addrInfo =
      generateIdentifierAddress(arrExpr->identifier.get());

  llvm::Value *ptr = generateArraySubscriptAddress(node);

  llvm::Type *elemTy = getLLVMType(arrSym->type);
  auto loadedVal = funcBuilder.CreateLoad(elemTy, ptr, "arr_elem_load");

  for (auto *freeCall : addrInfo.pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  return loadedVal;
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
  case DataType::FLOAT: {
    baseType = llvm::Type::getFloatTy(context);
    break;
  }
  case DataType::DOUBLE: {
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

  case DataType::DATABLOCK:
  case DataType::COMPONENT: {
    if (type.resolvedName.empty())
      throw std::runtime_error(
          "Custom type requested but resolvedName is empty");

    auto it = llvmCustomTypes.find(type.resolvedName);
    if (it != llvmCustomTypes.end())
      baseType = it->second;
    else
      throw std::runtime_error("LLVM IR requested for unknown custom type '" +
                               type.resolvedName + "'");
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

  // Wrap in a pointer if isPointer is true
  if (type.isPointer)
    return llvm::PointerType::get(baseType, 0);

  // Wrap in a pointer if isRef is true
  if (type.isRef)
    return llvm::PointerType::get(baseType, 0);

  return baseType;
}

llvm::Type *IRGenerator::lowerFunctionType(const ResolvedType &type) {
  // If the type isnt nullable just use then normal mapper
  if (!type.isNull)
    return getLLVMType(type);

  // If the type is nullable use the hidden struct that holds the success value
  // and error value
  llvm::Type *valueTy =
      getLLVMType({type.kind, type.resolvedName, /*isPtr*/ false,
                   /*isRef*/ false, /*isNull*/ false});

  // The error value has the same type as the value
  llvm::Type *errorTy = valueTy;

  std::vector<llvm::Type *> members = {valueTy, errorTy};
  return llvm::StructType::get(context, members, false); // not packed
}

// Registering generator functions for statements
void IRGenerator::registerGeneratorFunctions() {
  generatorFunctionsMap[typeid(LetStatement)] =
      &IRGenerator::generateLetStatement;
  generatorFunctionsMap[typeid(DheapStatement)] =
      &IRGenerator::generateDheapStatement;
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
  generatorFunctionsMap[typeid(DataStatement)] =
      &IRGenerator::generateDataStatement;
  generatorFunctionsMap[typeid(ComponentStatement)] =
      &IRGenerator::generateComponentStatement;
  generatorFunctionsMap[typeid(EnumClassStatement)] =
      &IRGenerator::generateEnumClassStatement;

  generatorFunctionsMap[typeid(ArrayStatement)] =
      &IRGenerator::generateArrayStatement;
  generatorFunctionsMap[typeid(ShoutStatement)] =
      &IRGenerator::generateShoutStatement;
  generatorFunctionsMap[typeid(QualifyStatement)] =
      &IRGenerator::generateQualifyStatement;
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
  expressionGeneratorsMap[typeid(FloatLiteral)] =
      &IRGenerator::generateFloatLiteral;
  expressionGeneratorsMap[typeid(DoubleLiteral)] =
      &IRGenerator::generateDoubleLiteral;
  expressionGeneratorsMap[typeid(ArrayLiteral)] =
      &IRGenerator::generateArrayLiteral;
  expressionGeneratorsMap[typeid(Identifier)] =
      &IRGenerator::generateIdentifierExpression;
  expressionGeneratorsMap[typeid(SizeOfExpression)] =
      &IRGenerator::generateSizeOfExpression;
  expressionGeneratorsMap[typeid(AddressExpression)] =
      &IRGenerator::generateAddressExpression;
  expressionGeneratorsMap[typeid(DereferenceExpression)] =
      &IRGenerator::generateDereferenceExpression;
  expressionGeneratorsMap[typeid(BlockExpression)] =
      &IRGenerator::generateBlockExpression;
  expressionGeneratorsMap[typeid(CallExpression)] =
      &IRGenerator::generateCallExpression;
  expressionGeneratorsMap[typeid(UnwrapExpression)] =
      &IRGenerator::generateUnwrapCallExpression;
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
    default:
      throw std::runtime_error("Unknown escape sequence in char literal: " +
                               literal);
    }
  }
  throw std::runtime_error("Invalid char literal: " + literal);
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

void IRGenerator::shoutRuntime(llvm::Value *val, ResolvedType type) {
  if (!val)
    throw std::runtime_error("shout! called with null value");

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
    if (!strVal)
      throw std::runtime_error("printString received null llvm::Value");

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
      std::cout << "Branching to SSA print\n";
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
      throw std::runtime_error("Unsupported integer value in shout!");
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

  llvm::IRBuilder<> initBuilder(heapInitFnEntry, heapInitFnEntry->begin());

  initBuilder.CreateCall(
      initFunc,
      {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), totalHeapSize)});
  std::cout << "Calling sage init \n";
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
  std::cout << "Calling sage destroy\n";
  isSageDestroyCalled =
      true; // Toggle this to true to mark that sage destroy was called
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

  std::optional<llvm::Reloc::Model> RM = llvm::Reloc::PIC_;

  auto targetMachine = target->createTargetMachine(targetTripleStr,
                                                   "generic", // CPU name
                                                   "",        // Features string
                                                   opt, RM);

  if (!targetMachine) {
    llvm::errs() << "Failed to create TargetMachine\n";
    return false;
  }

  module->setDataLayout(targetMachine->createDataLayout());

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
