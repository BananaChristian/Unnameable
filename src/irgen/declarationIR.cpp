#include "ast.hpp"
#include "irgen.hpp"
#include <llvm-18/llvm/IR/Attributes.h>
//______________DYNAMIC HEAP WRAP____________________
void IRGenerator::generateHeapStatement(Node *node) {
  auto heapStmt = dynamic_cast<HeapStatement *>(node);
  if (!heapStmt)
    return;

  generateStatement(heapStmt->stmt.get());
}

//______________LET STATEMENT GENERATION_____________
void IRGenerator::generateLetStatement(Node *node) {
  // VALIDATION AND EXTRACTION
  auto *letStmt = dynamic_cast<LetStatement *>(node);
  if (!letStmt) {
    reportDevBug("Invalid variable declaration", node->token.line,
                 node->token.column);
  }

  const std::string letName = letStmt->ident_token.TokenLiteral;
  logInternal("Generating variable declaration for variable '" + letName + "'");
  auto metaIt = semantics.metaData.find(letStmt);
  if (metaIt == semantics.metaData.end()) {
    errorHandler.addHint("Semantics did not register the metaData for '" +
                         letName + "'");
    reportDevBug("No variable declaration metaData for '" + letName + "",
                 letStmt->ident_token.line, letStmt->ident_token.column);
  }

  auto sym = metaIt->second;

  llvm::StructType *structTy = nullptr;
  bool isSage = letStmt->isSage;
  bool isHeap = letStmt->isHeap;
  bool isComponent = false;

  // Detect component type
  auto compIt = componentTypes.find(sym->type.resolvedName);
  if (compIt != componentTypes.end()) {
    isComponent = true;
    structTy = llvm::dyn_cast<llvm::StructType>(
        compIt->second); // Populate the struct type
  }

  // If there's no current insert block, handle global(GLOBAL SCOPE)
  if (isGlobalScope) {
    if (isComponent && isSage) {
      reportDevBug("Cannot sage raise a component in global scope",
                   letStmt->ident_token.line, letStmt->ident_token.column);
    } else if (isSage) {
      reportDevBug("Cannot sage raise in global scope ",
                   letStmt->ident_token.line, letStmt->ident_token.column);
    }

    else {
      auto valExpr = dynamic_cast<Expression *>(letStmt->value.get());
      generateGlobalScalarLet(sym, letName, valExpr);
    }
    return;
  }

  // LOCAL SCOPE
  llvm::Value *storage = nullptr;

  // For non-components: compute init value (scalar or other) this is for non
  // heap
  llvm::Value *initVal = nullptr;

  // Intercept the valiue if it is a move
  if (auto moveExpr = dynamic_cast<MoveExpression *>(letStmt->value.get())) {
    logInternal("Handling Move-Initialization for " + letName);

    // Get the baton (this nulls the source)
    initVal = generateMoveExpression(moveExpr);

    // Create the stack slot (the parking spot for the pointer)
    storage =
        funcBuilder.CreateAlloca(funcBuilder.getPtrTy(), nullptr, letName);

    // Adopt the baton
    funcBuilder.CreateStore(initVal, storage);

    sym->llvmValue = storage;
    sym->llvmType = funcBuilder.getPtrTy();
    return;
  }
  if (!isComponent) {
    if (letStmt->value)
      initVal = generateExpression(letStmt->value.get());
    else
      initVal = llvm::Constant::getNullValue(getLLVMType(sym->type));
  }

  llvm::Value *constructedPtr = nullptr;
  // If it is a component
  if (isComponent) {
    storage = generateComponentInit(
        letStmt, sym, structTy, isHeap); // It will handle its own heap business
    if (!storage) {
      reportDevBug("Component allocation failed for '" + letName + "'",
                   letStmt->ident_token.line, letStmt->ident_token.column);
    }
  } else if (isSage) // Incase the value is sage raised
  {
    // Generate the sage_alloc and allocate the value on the heap
    storage = allocateSageStorage(sym, letName, nullptr);
    funcBuilder.CreateStore(initVal, storage);
  } else if (isHeap) // If it is dynamic heap raise value
  {
    // Create a map on the stack that holds the pointer to the heap memory
    storage = allocateDynamicHeapStorage(sym, letName);
    funcBuilder.CreateStore(initVal, storage);
  } else // If it isnt a component or a heap raised value
  {

    // scalar / normal type
    llvm::Type *varTy = getLLVMType(sym->type);
    llvm::Align varAlign = layout->getABITypeAlign(varTy);

    storage = funcBuilder.CreateAlloca(varTy, nullptr, letName);

    if (auto *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(storage)) {
      allocaInst->setAlignment(varAlign);
    }

    if (initVal) {
      // If the variable is Nullable ({i1, i32}) but the value is just a scalar
      if (sym->type.isNull && !isComponent &&
          !llvm::isa<llvm::StructType>(varTy)) {
        llvm::StructType *stTy = llvm::cast<llvm::StructType>(varTy);
        // Create an undefined struct to start with
        llvm::Value *boxed = llvm::UndefValue::get(stTy);
        // Insert the isPresent flag = TRUE (index 0)
        boxed = funcBuilder.CreateInsertValue(
            boxed, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1),
            0);
        // Insert the actual data value (index 1)
        boxed = funcBuilder.CreateInsertValue(boxed, initVal, 1);
        // Override initVal so we store the whole box
        initVal = boxed;
      }

      auto *storeInst = funcBuilder.CreateStore(initVal, storage);
      storeInst->setAlignment(varAlign);
    } else {
      // If no value, initialize the whole struct to zero (flag=0, value=0)
      auto *storeInst =
          funcBuilder.CreateStore(llvm::Constant::getNullValue(varTy), storage);
      storeInst->setAlignment(varAlign);
    }
  }

  if (!storage) {
    reportDevBug("No storage allocated for '" + letName + "'",
                 letStmt->ident_token.line, letStmt->ident_token.column);
  }

  // Update symbol metadata with storage and type
  sym->llvmValue = storage;
  sym->llvmType = (isComponent && structTy) ? structTy : getLLVMType(sym->type);
  if (!sym->needsPostLoopFree) {
    emitCleanup(letStmt, sym);
  }
}

//__________________________ARRAY STATEMENT GENERATION_______________________
void IRGenerator::generateArrayStatement(Node *node) {
  auto arrStmt = dynamic_cast<ArrayStatement *>(node);
  if (!arrStmt)
    return;

  auto arrIt = semantics.metaData.find(arrStmt);
  if (arrIt == semantics.metaData.end()) {
    reportDevBug("Failed to find array statement metaData",
                 arrStmt->statement.line, arrStmt->statement.column);
  }

  auto arrName = arrStmt->identifier->expression.TokenLiteral;
  auto sym = arrIt->second;

  llvm::Type *elemTy = getLLVMType(semantics.getArrayElementType(sym->type));
  llvm::Value *allocationCount = nullptr;

  if (!arrStmt->lengths.empty()) {
    allocationCount = funcBuilder.getInt64(1);
    bool isConstant = true;

    for (const auto &lenExpr : arrStmt->lengths) {
      llvm::Value *dimSize = generateExpression(lenExpr.get());
      dimSize =
          funcBuilder.CreateIntCast(dimSize, funcBuilder.getInt64Ty(), false);
      allocationCount = funcBuilder.CreateMul(allocationCount, dimSize,
                                              "total_elements_calc");

      if (!semantics.isIntegerConstant(lenExpr.get()))
        isConstant = false;
    }

    // If it was constant, the layout already calculated componentSize.
    // We can just use that to avoid redundant IR math.
    if (isConstant) {
      uint64_t totalElements =
          sym->componentSize / layout->getTypeAllocSize(elemTy);
      allocationCount = funcBuilder.getInt64(totalElements);
    }
  } else if (arrStmt->array_content) {
    // No explicit length, use literal count from layout
    uint64_t totalElements =
        sym->componentSize / layout->getTypeAllocSize(elemTy);
    allocationCount = funcBuilder.getInt64(totalElements);
  } else {
    reportDevBug("Array '" + arrName + "' has no length or literal",
                 arrStmt->statement.line, arrStmt->statement.column);
  }

  llvm::Type *finalVarType = funcBuilder.getPtrTy();
  if (sym->type.isNull) {
    finalVarType = llvm::StructType::get(
        funcBuilder.getContext(),
        {funcBuilder.getInt1Ty(), funcBuilder.getPtrTy()});
  }

  llvm::Value *dataPtr = nullptr;
  if (sym->isSage) {
    dataPtr = allocateSageStorage(sym, arrName, nullptr);
    sym->llvmType = elemTy;
  } else if (sym->isHeap) {
    llvm::Value *byteSize = funcBuilder.CreateMul(
        allocationCount,
        funcBuilder.getInt64(layout->getTypeAllocSize(elemTy)));
    dataPtr = allocateRuntimeHeap(sym, byteSize, arrName);
    sym->llvmType = elemTy;
  } else {
    dataPtr =
        funcBuilder.CreateAlloca(elemTy, allocationCount, arrName + "_data");
  }

  llvm::Value *variableStorage = nullptr;
  if (sym->type.isNull) {
    // Create the box on the stack
    variableStorage =
        funcBuilder.CreateAlloca(finalVarType, nullptr, arrName + "_box");

    // If we have content, set flag to true and store pointer.
    // If no content (and it's nullable), it's effectively null
    // (is_present=false).
    bool isInitialized = (arrStmt->array_content != nullptr);
    bool isExplicitNull = isInitialized && dynamic_cast<NullLiteral *>(
                                               arrStmt->array_content.get());

    llvm::Value *isPresent =
        funcBuilder.getInt1(isInitialized && !isExplicitNull);

    // Create the struct value
    llvm::Value *boxVal = llvm::UndefValue::get(finalVarType);
    boxVal = funcBuilder.CreateInsertValue(boxVal, isPresent, 0);
    boxVal = funcBuilder.CreateInsertValue(boxVal, dataPtr, 1);

    funcBuilder.CreateStore(boxVal, variableStorage);
  } else {
    // Non-nullable: The variable is just the pointer itself
    variableStorage =
        funcBuilder.CreateAlloca(funcBuilder.getPtrTy(), nullptr, arrName);
    funcBuilder.CreateStore(dataPtr, variableStorage);
  }

  if (arrStmt->array_content &&
      !dynamic_cast<NullLiteral *>(arrStmt->array_content.get())) {
    // Get the source pointer (The Steamrollered Global)
    llvm::Value *srcData = generateExpression(arrStmt->array_content.get());

    // Get the real alignment from the element type
    llvm::Align finalAlign = layout->getABITypeAlign(elemTy);

    // Determine the byte count without phantom fields
    llvm::Value *totalBytes = nullptr;
    if (sym->componentSize > 0) {
      // The Accountant already did the work for a constant size
      totalBytes = funcBuilder.getInt64(sym->componentSize);
    } else {
      // It's dynamic (e.g., [var]). Calculate: count * sizeof(T)
      uint64_t elementSize = layout->getTypeAllocSize(elemTy);
      totalBytes = funcBuilder.CreateMul(allocationCount,
                                         funcBuilder.getInt64(elementSize));
    }

    // Emit the MemCpy
    funcBuilder.CreateMemCpy(dataPtr, // Dst: %x_data (or heap ptr)
                             finalAlign,
                             srcData, // Src: @flat_array_literal
                             finalAlign, totalBytes);
  }

  sym->llvmValue = variableStorage;

  if (!sym->needsPostLoopFree) {
    emitCleanup(arrStmt, sym);
  }
}

//_________________POINTER STATEMENT____________________
void IRGenerator::generatePointerStatement(Node *node) {
  auto ptrStmt = dynamic_cast<PointerStatement *>(node);
  if (!ptrStmt) {
    reportDevBug("Invalid pointer statement", node->token.line,
                 node->token.column);
  }

  auto ptrName = ptrStmt->name->expression.TokenLiteral;

  // Getting the pointer metaData
  auto metaIt = semantics.metaData.find(ptrStmt);
  if (metaIt == semantics.metaData.end()) {
    reportDevBug("Missing pointer statement metaData for '" + ptrName + "'",
                 ptrStmt->statement.line, ptrStmt->statement.column);
  }

  auto ptrSym = metaIt->second;

  // Getting the pointer llvm type
  llvm::Type *ptrType = getLLVMType(ptrSym->type);
  if (!ptrType) {
    reportDevBug("Failed to get pointer type for '" + ptrName + "'",
                 ptrStmt->statement.line, ptrStmt->statement.column);
  }

  llvm::Type *ptrStorageType = ptrType->getPointerTo();

  llvm::Value *initVal = nullptr;
  if (ptrStmt->value) {
    initVal = generateExpression(ptrStmt->value.get());
  } else {
    initVal =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrType));
  }

  if (ptrSym->type.isNull && initVal && !initVal->getType()->isStructTy()) {
    llvm::StructType *stTy = llvm::cast<llvm::StructType>(ptrType);

    // Create an undefined struct to start with
    llvm::Value *boxed = llvm::UndefValue::get(stTy);

    // Insert the isPresent flag = TRUE (index 0)
    boxed = funcBuilder.CreateInsertValue(
        boxed, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1), 0);

    // Insert the actual pointer value (index 1)
    boxed = funcBuilder.CreateInsertValue(boxed, initVal, 1);

    // Override initVal so we store the whole box
    initVal = boxed;
  }

  if (!initVal) {
    reportDevBug("No initial value for pointer '" + ptrName + "'",
                 ptrStmt->statement.line, ptrStmt->statement.column);
  }

  llvm::Value *storagePtr = nullptr;
  if (ptrSym->isSage) {
    storagePtr = allocateSageStorage(ptrSym, ptrName, nullptr);
    funcBuilder.CreateStore(initVal, storagePtr);
  } else if (ptrSym->isHeap) {
    storagePtr = allocateDynamicHeapStorage(ptrSym, ptrName);
    funcBuilder.CreateStore(initVal, storagePtr);
  } else if (!funcBuilder.GetInsertBlock()) {
    llvm::Constant *globalInit = llvm::dyn_cast<llvm::Constant>(initVal);
    if (!globalInit) {
      reportDevBug("Global pointer must be constant", ptrStmt->statement.line,
                   ptrStmt->statement.column);
    }

    storagePtr = new llvm::GlobalVariable(*module, ptrType, false,
                                          llvm::GlobalValue::InternalLinkage,
                                          globalInit, ptrName);
  } else {
    storagePtr = funcBuilder.CreateAlloca(ptrType, nullptr, ptrName);
    funcBuilder.CreateStore(initVal, storagePtr);
  }

  ptrSym->llvmValue = storagePtr;
  ptrSym->llvmType = ptrType;

  // Last use clean up for the pointer statement
  if (!ptrSym->needsPostLoopFree) {
    emitCleanup(ptrStmt, ptrSym);
  }
}

// Reference statement IR generator
void IRGenerator::generateReferenceStatement(Node *node) {
  auto refStmt = dynamic_cast<ReferenceStatement *>(node);
  if (!refStmt) {
    reportDevBug("Invalid reference statemnt", node->token.line,
                 node->token.column);
  }

  const auto &refName = refStmt->name->expression.TokenLiteral;
  const auto &refereeName =
      semantics.extractIdentifierName(refStmt->value.get());

  // Lookup metadata
  auto metaIt = semantics.metaData.find(refStmt);
  if (metaIt == semantics.metaData.end()) {
    reportDevBug("Failed to find reference metaData for '" + refName + "'",
                 refStmt->statement.line, refStmt->statement.column);
  }

  auto refSym = metaIt->second;
  auto targetSym = refSym->refereeSymbol;
  if (!targetSym) {
    reportDevBug("Reference '" + refName + "' has no target symbol",
                 refStmt->statement.line, refStmt->statement.column);
  }

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
    targetAddress = generateAddress(refStmt->value.get());
  }

  if (!targetAddress || !targetAddress->getType()->isPointerTy()) {
    reportDevBug("Failed to resolve address for reference '" + refName + "'",
                 refStmt->statement.line, refStmt->statement.column);
  }

  // The reference itself holds the pointer to the target
  refSym->llvmValue = targetAddress;
}

//_______________________HELPERS______________________________________

void IRGenerator::generateGlobalScalarLet(std::shared_ptr<SymbolInfo> sym,
                                          const std::string &letName,
                                          Expression *value) {
  llvm::Type *varType = getLLVMType(sym->type);

  llvm::Constant *init = nullptr;
  if (sym->isInitialized && value) {
    llvm::Value *val = generateExpression(value);
    init = llvm::dyn_cast<llvm::Constant>(val);

    if (!init) {
      reportDevBug("Global '" + letName +
                       "' must be initialized with a constant expression",
                   value->expression.line, value->expression.column);
    }
  } else if (semantics.customTypesTable.count(sym->type.resolvedName)) {
    init = generateGlobalRecordDefaults(sym->type.resolvedName);
  } else {
    init = llvm::Constant::getNullValue(varType);
  }

  auto *g = new llvm::GlobalVariable(
      *module, varType,
      sym->isConstant, // should be true unless semantics has screwed me
      llvm::GlobalValue::InternalLinkage, init, letName);

  sym->llvmValue = g;
  sym->llvmType = varType;
}

llvm::Constant *
IRGenerator::generateGlobalRecordDefaults(const std::string &typeName) {
  llvm::StructType *structTy =
      llvm::cast<llvm::StructType>(llvmCustomTypes[typeName]);
  auto compInfo = semantics.customTypesTable[typeName];

  // We need to fill this vector in the exact order of the struct fields
  std::vector<llvm::Constant *> fieldConstants(compInfo->members.size());

  for (const auto &[name, memInfo] : compInfo->members) {
    auto letNode = dynamic_cast<LetStatement *>(memInfo->node);
    llvm::Constant *fieldInit = nullptr;

    if (letNode && letNode->value) {
      // Field has a default like field1 = 100
      // We must evaluate it as a constant
      fieldInit = llvm::dyn_cast<llvm::Constant>(
          generateExpression(letNode->value.get()));
    }

    if (!fieldInit) {
      // No default? Use the "Recursive Null" (handles nested records/nullables)
      fieldInit = llvm::Constant::getNullValue(getLLVMType(memInfo->type));
    }

    fieldConstants[memInfo->memberIndex] = fieldInit;
  }

  return llvm::ConstantStruct::get(structTy, fieldConstants);
}

llvm::Value *IRGenerator::generateComponentInit(LetStatement *letStmt,
                                                std::shared_ptr<SymbolInfo> sym,
                                                llvm::StructType *structTy,
                                                bool isHeap) {
  std::string letName = letStmt->ident_token.TokenLiteral;

  // Allocate (Even if no 'new' exists)
  llvm::Value *instancePtr = nullptr;
  if (isHeap) {
    if (!isSageInitCalled)
      generateSageInitCall();
    instancePtr = allocateSageStorage(sym, letName, structTy);
  } else {
    const llvm::DataLayout &DL = module->getDataLayout();

    // Use Preferred Alignment
    llvm::Align finalAlign = DL.getPrefTypeAlign(structTy);

    // Create the alloca and assign it to instancePtr
    auto *allocaInst =
        funcBuilder.CreateAlloca(structTy, nullptr, letName + ".stack");
    allocaInst->setAlignment(finalAlign);
    instancePtr = allocaInst; // instancePtr is just the alloca result

    // Use instancePtr for the store
    auto *storeInst = funcBuilder.CreateStore(
        llvm::Constant::getNullValue(structTy), instancePtr);
    storeInst->setAlignment(finalAlign);
  }

  // Check if the initializer exists
  auto newExpr =
      letStmt->value
          ? dynamic_cast<NewComponentExpression *>(letStmt->value.get())
          : nullptr;

  // Apply default field initializers(from the component definition)
  auto compTypeIt = semantics.customTypesTable.find(sym->type.resolvedName);
  if (compTypeIt != semantics.customTypesTable.end()) {
    for (const auto &[name, memInfo] : compTypeIt->second->members) {
      auto letNode = dynamic_cast<LetStatement *>(memInfo->node);
      if (!letNode || !letNode->value)
        continue;

      llvm::Value *initVal = generateExpression(letNode->value.get());
      llvm::Value *fieldPtr = funcBuilder.CreateStructGEP(
          structTy, instancePtr, memInfo->memberIndex, name + "_field");
      funcBuilder.CreateStore(initVal, fieldPtr);
    }
  }

  // Only call init construtor if new was used
  if (newExpr) {
    if (llvm::Function *initFn =
            module->getFunction(sym->type.resolvedName + "_init")) {
      std::vector<llvm::Value *> initArgs;
      initArgs.push_back(instancePtr);
      for (auto &arg : newExpr->arguments) {
        initArgs.push_back(generateExpression(arg.get()));
      }
      funcBuilder.CreateCall(initFn, initArgs);
    }
  }

  return instancePtr;
}

// Sage Heap storage
llvm::Value *IRGenerator::allocateSageStorage(std::shared_ptr<SymbolInfo> sym,
                                              const std::string &letName,
                                              llvm::StructType *structTy) {
  // If sage_init hasnt been called
  if (!isSageInitCalled)
    generateSageInitCall();

  uint64_t allocSize = sym->componentSize;
  uint64_t alignSize = sym->alignment.value();
  llvm::Type *i8PtrTy =
      llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
  llvm::FunctionCallee sageAlloc = module->getOrInsertFunction(
      "sage_alloc", i8PtrTy, llvm::Type::getInt64Ty(context),
      llvm::Type::getInt64Ty(context));

  llvm::Value *rawPtr = funcBuilder.CreateCall(
      sageAlloc,
      {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), allocSize),
       llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), alignSize)},
      letName + "_sage_rawptr");

  llvm::Type *targetPtrTy = structTy ? structTy->getPointerTo()
                                     : getLLVMType(sym->type)->getPointerTo();
  return funcBuilder.CreateBitCast(rawPtr, targetPtrTy, letName + "_sage_ptr");
}

// Dynamic heap storage
llvm::Value *
IRGenerator::allocateDynamicHeapStorage(std::shared_ptr<SymbolInfo> sym,
                                        const std::string &varName) {

  const std::string &allocatorTypeName = sym->allocType;

  auto it = semantics.allocatorMap.find(allocatorTypeName);
  if (it == semantics.allocatorMap.end()) {
    reportDevBug("Unknown allocator type '" + allocatorTypeName + "'", 0, 0);
  }

  auto handle = it->second;
  auto allocatorName = handle.allocateName;

  // Find the function in the module(it was created in registerAllocators)
  llvm::Function *allocFunc = module->getFunction(allocatorName);
  if (!allocFunc) {
    reportDevBug("Function not found for allocator '" + allocatorName + "'", 0,
                 0);
  }

  size_t allocSize = sym->componentSize; // Get the size for the allocation
  llvm::Value *sizeArg =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), allocSize);

  llvm::Value *rawPtr =
      funcBuilder.CreateCall(allocFunc, {sizeArg}, varName + "_heap_raw");

  llvm::Type *baseType = getLLVMType(sym->type);

  return funcBuilder.CreateBitCast(rawPtr, baseType->getPointerTo(),
                                   varName + "_ptr");
}

llvm::Value *IRGenerator::allocateRuntimeHeap(std::shared_ptr<SymbolInfo> sym,
                                              llvm::Value *runtimeSize,
                                              const std::string &varName) {
  const std::string &allocatorTypeName = sym->allocType;
  auto it = semantics.allocatorMap.find(allocatorTypeName);
  auto handle = it->second;

  llvm::Function *allocFunc = module->getFunction(handle.allocateName);

  // Pass the RUNTIME calculated size to the custom allocator
  llvm::Value *rawPtr =
      funcBuilder.CreateCall(allocFunc, {runtimeSize}, varName + "_heap_raw");

  llvm::Type *baseType = getLLVMType(sym->type);
  return funcBuilder.CreateBitCast(rawPtr, baseType->getPointerTo(),
                                   varName + "_ptr");
}
