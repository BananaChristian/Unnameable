#include "irgen.hpp"
#include <llvm-18/llvm/IR/Function.h>

void IRGenerator::generateFunctionStatement(Node *node) {
  auto fnStmt = dynamic_cast<FunctionStatement *>(node);
  if (!fnStmt)
    return;

  llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();

  // Checking what the function statement is holding could be a function
  // expression or a function declaration expression
  auto fnExpr = fnStmt->funcExpr.get();
  // Case where it is a full function expression
  if (auto expr = dynamic_cast<FunctionExpression *>(fnExpr)) {
    generateFunctionExpression(expr);
  }

  if (auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnExpr))
    generateFunctionDeclarationExpression(declrExpr);

  if (oldInsertPoint)
    funcBuilder.SetInsertPoint(oldInsertPoint);
}

void IRGenerator::generateFunctionDeclarationExpression(Node *node) {
  auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(node);
  if (!declrExpr)
    return;

  // This is also just a wrapper for the function declaration expression so let
  // me just call the statement generator
  auto fnDeclr = declrExpr->funcDeclrStmt.get();
  // Call the statement generator for the function declaration statement
  generateFunctionDeclaration(fnDeclr);
}

void IRGenerator::generateFunctionDeclaration(Node *node) {
  auto fnDeclr = dynamic_cast<FunctionDeclaration *>(node);
  if (!fnDeclr)
    return;

  const std::string &fnName = fnDeclr->function_name->expression.TokenLiteral;

  if (module->getFunction(fnName))
    return;

  auto sym = semantics.getSymbolFromMeta(fnDeclr);
  if (!sym)
    reportDevBug("Failed to get function declaration symbol info", fnDeclr);

  llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
  if (sym->isExportable)
    linkage = llvm::Function::ExternalLinkage;

  // Store coercion info for this function
  FunctionCoercion coercion;

  // Coerce parameters
  for (const auto &param : fnDeclr->parameters) {
    auto paramSym = semantics.getSymbolFromMeta(param.get());
    if (!paramSym) {
      reportDevBug("Missing function declaration parameter symbol info",
                   param.get());
    }

    llvm::Type *originalTy = getLLVMType(paramSym->type().type);
    coercion.originalParamTypes.push_back(originalTy);

    llvm::Type *coercedTy = originalTy;
    CoercionInfo paramInfo;
    paramInfo.isMemory = false;
    paramInfo.coercedType = originalTy;

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalTy)) {
      paramInfo = classifyStruct(structTy);
      if (paramInfo.isMemory) {
        // Large struct (>16 bytes) - pass by pointer with 'byval'
        coercedTy = structTy->getPointerTo();
        paramInfo.coercedType = coercedTy;
      } else if (paramInfo.coercedType) {
        coercedTy = paramInfo.coercedType;
      }
    }

    coercion.coercedParamTypes.push_back(coercedTy);
    coercion.paramCoercion.push_back(paramInfo);
  }

  // Coerce return type
  auto retType = sym->func().returnType;
  llvm::Type *originalRetTy = getLLVMType(retType);
  llvm::Type *coercedRetTy = originalRetTy;
  coercion.returnCoercion.isMemory = false;
  coercion.returnCoercion.coercedType = originalRetTy;

  if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalRetTy)) {
    coercion.returnCoercion = classifyStruct(structTy);
    if (coercion.returnCoercion.isMemory) {
      // Large struct return add hidden sret parameter
      // This modifies the function type to have an extra pointer parameter
      coercedRetTy = llvm::Type::getVoidTy(context);
      // Add pointer to coercedParamTypes at beginning
      coercion.coercedParamTypes.insert(coercion.coercedParamTypes.begin(),
                                        structTy->getPointerTo());
      // Add coercion info for the sret parameter
      CoercionInfo sretInfo;
      sretInfo.isMemory = true;
      sretInfo.coercedType = structTy->getPointerTo();
      coercion.paramCoercion.insert(coercion.paramCoercion.begin(), sretInfo);
      coercion.originalParamTypes.insert(coercion.originalParamTypes.begin(),
                                         structTy);
    } else if (coercion.returnCoercion.coercedType) {
      coercedRetTy = coercion.returnCoercion.coercedType;
    }
  }

  // Create function type with coerced parameters
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(coercedRetTy, coercion.coercedParamTypes, false);
  llvm::Function *fn =
      llvm::Function::Create(fnType, linkage, fnName, module.get());

  // Add attributes to parameters
  for (size_t i = 0; i < coercion.paramCoercion.size(); i++) {
    if (coercion.paramCoercion[i].isMemory) {
      fn->addParamAttr(i, llvm::Attribute::ByVal);
    }
  }

  // Store coercion info for call sites
  functionCoercionMap[fn] = coercion;
}

// Generator function for function expression
llvm::Value *IRGenerator::generateFunctionExpression(Node *node) {
  auto fnExpr = dynamic_cast<FunctionExpression *>(node);
  if (!fnExpr)
    reportDevBug("Invalid function expression", fnExpr);

  auto fnName = fnExpr->func_key.TokenLiteral;
  auto funcSym = semantics.getSymbolFromMeta(fnExpr);

  isGlobalScope = false;

  // Store original parameter types and build coercion
  std::vector<llvm::Type *> originalParamTypes;
  std::vector<CoercionInfo> paramCoercion;
  std::vector<llvm::Type *> coercedParamTypes;

  for (auto &p : fnExpr->call) {
    auto paramSym = semantics.getSymbolFromMeta(p.get());
    if (!paramSym)
      reportDevBug("Missing parameter symbol info", p.get());

    llvm::Type *originalTy = getLLVMType(paramSym->type().type);
    originalParamTypes.push_back(originalTy);

    llvm::Type *coercedTy = originalTy;
    CoercionInfo info;
    info.isMemory = false;
    info.coercedType = originalTy;

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalTy)) {
      info = classifyStruct(structTy);
      if (info.isMemory) {
        coercedTy = structTy->getPointerTo();
        info.coercedType = coercedTy;
      } else if (info.coercedType) {
        coercedTy = info.coercedType;
      }
    }

    paramCoercion.push_back(info);
    coercedParamTypes.push_back(coercedTy);
  }

  // Get return type and coerce it
  auto fnRetType = fnExpr->return_type.get();
  auto retType = semantics.inferNodeDataType(fnRetType);
  llvm::Type *originalRetTy = getLLVMType(retType);

  CoercionInfo returnCoercion;
  returnCoercion.isMemory = false;
  returnCoercion.coercedType = originalRetTy;
  llvm::Type *coercedRetTy = originalRetTy;
  bool hasSRet = false;

  if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalRetTy)) {
    returnCoercion = classifyStruct(structTy);
    if (returnCoercion.isMemory) {
      // Large struct return: add hidden sret parameter
      coercedRetTy = llvm::Type::getVoidTy(context);
      hasSRet = true;
      // Insert pointer parameter at beginning
      coercedParamTypes.insert(coercedParamTypes.begin(),
                               structTy->getPointerTo());
      // Add coercion info for sret
      CoercionInfo sretInfo;
      sretInfo.isMemory = true;
      sretInfo.coercedType = structTy->getPointerTo();
      paramCoercion.insert(paramCoercion.begin(), sretInfo);
    } else if (returnCoercion.coercedType) {
      coercedRetTy = returnCoercion.coercedType;
    }
  }

  llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
  if (fnName == "main" || funcSym->isExportable)
    linkage = llvm::Function::ExternalLinkage;

  llvm::FunctionType *funcType =
      llvm::FunctionType::get(coercedRetTy, coercedParamTypes, false);
  llvm::Function *fn = module->getFunction(fnName);

  if (!fn) {
    fn = llvm::Function::Create(funcType, linkage, fnName, module.get());
  } else if (fn->getFunctionType() != funcType) {
    reportDevBug("Function redefinition for '" + fnName +
                     "' with different signature",
                 fnExpr);
  }

  // Store coercion info
  FunctionCoercion coercion;
  coercion.originalParamTypes = originalParamTypes;
  coercion.coercedParamTypes = coercedParamTypes;
  coercion.paramCoercion = paramCoercion;
  coercion.returnCoercion = returnCoercion;
  coercion.hasSRet = hasSRet;
  functionCoercionMap[fn] = coercion;

  currentFunction = fn;

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
  funcBuilder.SetInsertPoint(entry);

  // Bind parameters with original types (not coerced)
  auto argIter = fn->arg_begin();
  size_t paramIdx = 0;

  for (auto &p : fnExpr->call) {
    auto pIt = semantics.metaData.find(p.get());
    if (pIt == semantics.metaData.end()) {
      reportDevBug("Failed to find parameter meta data", p.get());
    }

    llvm::Type *originalTy = originalParamTypes[paramIdx];
    llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(
        originalTy, nullptr, p->statement.TokenLiteral);

    llvm::Value *paramValue = &(*argIter);

    // If parameter was coerced, bitcast back to original type
    if (paramCoercion[paramIdx].coercedType &&
        paramCoercion[paramIdx].coercedType != originalTy) {
      paramValue =
          funcBuilder.CreateBitCast(paramValue, originalTy->getPointerTo());
      paramValue = funcBuilder.CreateLoad(originalTy, paramValue);
    }

    funcBuilder.CreateStore(paramValue, alloca);
    pIt->second->codegen().llvmValue = alloca;

    argIter++;
    paramIdx++;
  }

  llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();
  generateExpression(fnExpr->block.get());

  llvm::BasicBlock *finalBlock = funcBuilder.GetInsertBlock();
  bool isVoidFunction = funcSym->func().returnType.kind == DataType::VOID;

  if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator())) {
    if (isVoidFunction) {
      funcBuilder.CreateRetVoid();
    } else {
      funcBuilder.CreateUnreachable();
    }
  }

  if (oldInsertPoint) {
    funcBuilder.SetInsertPoint(oldInsertPoint);
  } else {
    funcBuilder.ClearInsertionPoint();
  }

  isGlobalScope = true;
  currentFunction = nullptr;
  return fn;
}

void IRGenerator::generateReturnStatement(Node *node) {
  auto retStmt = dynamic_cast<ReturnStatement *>(node);
  if (!retStmt)
    return;

  auto sym = semantics.getSymbolFromMeta(retStmt);
  if (!sym)
    reportDevBug("Failed to get return statement symbol info", retStmt);

  inhibitCleanUp = true;
  llvm::Value *retVal = nullptr;
  if (retStmt->return_value) {
    retVal = generateExpression(retStmt->return_value.get());
  }
  inhibitCleanUp = false;
  emitCleanup(retStmt);

  llvm::Function *currentFunc = funcBuilder.GetInsertBlock()->getParent();
  llvm::Type *retTy = currentFunc->getReturnType();

  if (retTy->isVoidTy()) {
    funcBuilder.CreateRetVoid();
    return;
  }

  if (!retVal) {
    llvm::errs() << "Error: Non-void function missing return value.\n";
    funcBuilder.CreateUnreachable();
    return;
  }

  // Get coercion info for this function
  FunctionCoercion coercion;
  auto coercionIt = functionCoercionMap.find(currentFunc);
  if (coercionIt != functionCoercionMap.end()) {
    coercion = coercionIt->second;
  }

  // Handle nullable types (existing logic)
  if (sym->type().type.isNull) {
    if (!retVal->getType()->isStructTy()) {
      llvm::Value *boxed = llvm::UndefValue::get(retTy);
      bool isNullLiteral =
          (dynamic_cast<NullLiteral *>(retStmt->return_value.get()) != nullptr);
      llvm::Value *presentFlag = llvm::ConstantInt::get(
          llvm::Type::getInt1Ty(context), isNullLiteral ? 0 : 1);
      boxed = funcBuilder.CreateInsertValue(boxed, presentFlag, 0);
      llvm::Type *innerTy = retTy->getStructElementType(1);
      llvm::Value *payload = nullptr;
      if (isNullLiteral) {
        payload = llvm::Constant::getNullValue(innerTy);
      } else {
        payload = retVal;
        if (payload->getType() != innerTy &&
            payload->getType()->isIntegerTy()) {
          payload = funcBuilder.CreateZExtOrTrunc(payload, innerTy);
        }
      }
      boxed = funcBuilder.CreateInsertValue(boxed, payload, 1);
      funcBuilder.CreateRet(boxed);
      return;
    }
  }

  // Apply return coercion if needed
  if (coercion.returnCoercion.coercedType &&
      coercion.returnCoercion.coercedType != retVal->getType()) {
    retVal =
        funcBuilder.CreateBitCast(retVal, coercion.returnCoercion.coercedType);
  }

  llvm::Type *valTy = retVal->getType();

  if (valTy == retTy) {
    funcBuilder.CreateRet(retVal);
    return;
  }

  if (valTy->isPointerTy() && retTy->isPointerTy()) {
    funcBuilder.CreateRet(funcBuilder.CreateBitCast(retVal, retTy));
    return;
  }

  if (valTy->isIntegerTy() && retTy->isIntegerTy()) {
    funcBuilder.CreateRet(funcBuilder.CreateZExtOrTrunc(retVal, retTy));
    return;
  }

  if (!valTy) {
    llvm::errs() << "IRGen Error: retVal->getType() is null!\n";
    funcBuilder.CreateUnreachable();
    return;
  }

  llvm::errs() << "IRGen Error: Return type mismatch. Cannot adapt " << *valTy
               << " to " << *retTy << "\n";
  funcBuilder.CreateUnreachable();
}

llvm::Value *
IRGenerator::generateFnPtrCall(CallExpression *callExpr,
                               const std::shared_ptr<SymbolInfo> &callSym) {

  auto identSym =
      semantics.getSymbolFromMeta(callExpr->function_identifier.get());
  if (!identSym)
    reportDevBug("Failed to get fn pointer sym",
                 callExpr->function_identifier.get());

  llvm::Value *fnPtrAlloca = identSym->codegen().llvmValue;
  llvm::Value *fnPtr = funcBuilder.CreateLoad(
      llvm::PointerType::get(context, 0), fnPtrAlloca, "fnptr");

  const ResolvedType &fnPtrType = identSym->type().type;
  bool wasInhibited = inhibitCleanUp;
  inhibitCleanUp = false;

  // Build param types with coercion
  std::vector<llvm::Type *> paramTypes;
  std::vector<CoercionInfo> paramCoercions;
  for (const auto &paramType : fnPtrType.fnParamTypes) {
    llvm::Type *ty = getLLVMType(paramType);
    CoercionInfo info;
    info.isMemory = false;
    info.coercedType = ty;
    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(ty)) {
      info = classifyStruct(structTy);
      if (info.isMemory)
        ty = structTy->getPointerTo();
      else if (info.coercedType)
        ty = info.coercedType;
    }
    paramCoercions.push_back(info);
    paramTypes.push_back(ty);
  }

  // Build return type with coercion
  llvm::Type *originalRetTy = fnPtrType.fnReturnType
                                  ? getLLVMType(*fnPtrType.fnReturnType)
                                  : llvm::Type::getVoidTy(context);

  CoercionInfo retCoercion;
  retCoercion.isMemory = false;
  retCoercion.coercedType = originalRetTy;
  llvm::Type *coercedRetTy = originalRetTy;
  bool hasSRet = false;

  if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalRetTy)) {
    retCoercion = classifyStruct(structTy);
    if (retCoercion.isMemory) {
      // Large struct — add hidden sret param
      coercedRetTy = llvm::Type::getVoidTy(context);
      hasSRet = true;
      paramTypes.insert(paramTypes.begin(), structTy->getPointerTo());
    } else if (retCoercion.coercedType) {
      coercedRetTy = retCoercion.coercedType;
    }
  }

  llvm::FunctionType *fnTy =
      llvm::FunctionType::get(coercedRetTy, paramTypes, false);

  // Prepare args
  std::vector<llvm::Value *> argsV;

  // Handle sret if needed
  llvm::AllocaInst *sretAlloca = nullptr;
  if (hasSRet) {
    sretAlloca = funcBuilder.CreateAlloca(originalRetTy, nullptr, "sret.tmp");
    argsV.push_back(sretAlloca);
  }

  // Generate args with coercion
  for (size_t i = 0; i < callExpr->parameters.size(); i++) {
    llvm::Value *argVal = generateExpression(callExpr->parameters[i].get());
    if (!argVal)
      reportDevBug("Failed to generate fn pointer argument",
                   callExpr->parameters[i].get());

    const auto &info = paramCoercions[i];
    if (info.isMemory) {
      llvm::AllocaInst *tmp =
          funcBuilder.CreateAlloca(argVal->getType(), nullptr, "byval.tmp");
      funcBuilder.CreateStore(argVal, tmp);
      argVal = tmp;
    } else if (info.coercedType && info.coercedType != argVal->getType()) {
      if (argVal->getType()->isStructTy() || info.coercedType->isIntegerTy())
        argVal = funcBuilder.CreateBitCast(argVal, info.coercedType);
    }

    // Nullable wrapping
    llvm::Type *expectedTy = paramTypes[i + (hasSRet ? 1 : 0)];
    if (expectedTy->isStructTy() && !argVal->getType()->isStructTy()) {
      llvm::Value *wrapped = llvm::UndefValue::get(expectedTy);
      wrapped =
          funcBuilder.CreateInsertValue(wrapped, funcBuilder.getInt1(true), 0);
      wrapped = funcBuilder.CreateInsertValue(wrapped, argVal, 1);
      argVal = wrapped;
    }

    argsV.push_back(argVal);
  }
  inhibitCleanUp = wasInhibited;

  // Emit indirect call
  llvm::Value *call = funcBuilder.CreateCall(fnTy, fnPtr, argsV, "fnptrtmp");

  // Handle return value
  if (hasSRet) {
    // Load from sret alloca
    llvm::Value *result =
        funcBuilder.CreateLoad(originalRetTy, sretAlloca, "sret.val");
    callSym->codegen().llvmValue = result;
    callSym->codegen().llvmType = result->getType();
    for (const auto &arg : callExpr->parameters)
      emitCleanup(arg.get());
    return result;
  }

  if (coercedRetTy->isVoidTy())
    return nullptr;

  // Coerce return back to original type if needed
  llvm::Value *result = call;
  if (retCoercion.coercedType && retCoercion.coercedType != originalRetTy) {
    result = funcBuilder.CreateBitCast(call, originalRetTy);
  }

  callSym->codegen().llvmValue = result;
  callSym->codegen().llvmType = result->getType();

  for (const auto &arg : callExpr->parameters)
    emitCleanup(arg.get());

  return result;
}

llvm::Value *IRGenerator::generateCallExpression(Node *node) {
  auto callExpr = dynamic_cast<CallExpression *>(node);
  if (!callExpr)
    reportDevBug("Invalid call expression", node);

  if (isGlobalScope)
    reportDevBug("Function calls are not allowed at global scope", callExpr);

  const std::string &fnName =
      callExpr->function_identifier->expression.TokenLiteral;

  auto callSym = semantics.getSymbolFromMeta(callExpr);
  if (!callSym)
    reportDevBug("Call expression symbol info does not exist", callExpr);

  if (callSym->type().isFnPtr)
    return generateFnPtrCall(callExpr, callSym);

  llvm::Function *calledFunc = module->getFunction(fnName);
  if (!calledFunc) {
    reportDevBug("Unknown function '" + fnName + "' referenced", callExpr);
  }

  // Get coercion info for this function
  FunctionCoercion coercion;
  auto coercionIt = functionCoercionMap.find(calledFunc);
  if (coercionIt != functionCoercionMap.end()) {
    coercion = coercionIt->second;
  }

  std::vector<llvm::Value *> argsV =
      prepareArguments(calledFunc, callExpr->parameters, 0, coercion);

  // Emit the function call
  llvm::Value *call = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

  // Coerce return value back to original type if needed
  if (coercion.returnCoercion.coercedType &&
      coercion.returnCoercion.coercedType != calledFunc->getReturnType()) {
    // If the return was coerced to a different type, bitcast back
    // We need the original expected type from the call expression
    llvm::Type *expectedRetTy = getLLVMType(callSym->type().type);
    if (call->getType() != expectedRetTy) {
      call = funcBuilder.CreateBitCast(call, expectedRetTy);
    }
  }

  if (calledFunc->getReturnType()->isVoidTy()) {
    return nullptr;
  }

  callSym->codegen().llvmType = call->getType();
  callSym->codegen().llvmValue = call;

  // If the return type is a pointer call the emitter on the entire thing but if
  // it isnt only target the parameters
  if (callSym->type().isPointer) {
    emitCleanup(callExpr);
  } else {
    for (const auto &arg : callExpr->parameters) {
      emitCleanup(arg.get());
    }
  }

  return call;
}

llvm::Value *IRGenerator::generateCallAddress(Node *node) {
  auto callExpr = dynamic_cast<CallExpression *>(node);
  if (!callExpr)
    reportDevBug("Invalid call expression inside address generator", node);

  if (isGlobalScope)
    reportDevBug("Function calls are not allowed in global scope", callExpr);

  const std::string &funcName =
      callExpr->function_identifier->expression.TokenLiteral;

  auto callSym = semantics.getSymbolFromMeta(callExpr);
  if (!callSym)
    reportDevBug("Call expression symbol info not found", callExpr);

  if (callSym->type().isFnPtr) {
    llvm::Value *result = generateFnPtrCall(callExpr, callSym);
    if (!result)
      reportDevBug("Fn pointer call returned void, cannot take address",
                   callExpr);
    // Store result in temp alloca and return address
    llvm::AllocaInst *tmp =
        funcBuilder.CreateAlloca(result->getType(), nullptr, "fnptrcall.addr");
    funcBuilder.CreateStore(result, tmp);
    return tmp;
  }

  llvm::Function *calledFunc = module->getFunction(funcName);
  if (!calledFunc)
    reportDevBug("Unknown function '" + funcName + "' referenced", callExpr);

  // Get coercion info
  FunctionCoercion coercion;
  auto coercionIt = functionCoercionMap.find(calledFunc);
  if (coercionIt != functionCoercionMap.end()) {
    coercion = coercionIt->second;
  }

  std::vector<llvm::Value *> argsV =
      prepareArguments(calledFunc, callExpr->parameters, 0, coercion);

  llvm::Type *retTy = calledFunc->getReturnType();
  if (retTy->isVoidTy())
    reportDevBug("Attempted to take address of a void call result", callExpr);

  // Ensure the temporary alloca is created in the entry block
  llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
  llvm::IRBuilder<> tmpBuilder(&currentFunction->getEntryBlock(),
                               currentFunction->getEntryBlock().begin());

  // Coerced return type might be different from original
  llvm::Type *actualRetTy = retTy;
  if (coercion.returnCoercion.coercedType &&
      coercion.returnCoercion.coercedType != retTy) {
    actualRetTy = coercion.returnCoercion.coercedType;
  }

  llvm::AllocaInst *tmpAlloca =
      tmpBuilder.CreateAlloca(actualRetTy, nullptr, "calltmp.addr");

  llvm::Value *callVal = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

  // Coerce return value if needed before storing
  if (callVal->getType() != actualRetTy) {
    callVal = funcBuilder.CreateBitCast(callVal, actualRetTy);
  }

  funcBuilder.CreateStore(callVal, tmpAlloca);

  return tmpAlloca;
}

std::vector<llvm::Value *> IRGenerator::prepareArguments(
    llvm::Function *func,
    const std::vector<std::unique_ptr<Expression>> &params, size_t offset,
    const FunctionCoercion &coercion) {

  std::vector<llvm::Value *> argsV;
  auto funcTy = func->getFunctionType();

  for (size_t i = 0; i < params.size(); ++i) {
    llvm::Value *argVal = nullptr;

    auto metaIt = semantics.metaData.find(params[i].get());
    if (metaIt != semantics.metaData.end() &&
        metaIt->second->type().needsImplicitAddress) {
      argVal = generateIdentifierAddress(params[i].get());
    } else {
      argVal = generateExpression(params[i].get());
    }

    if (!argVal)
      throw std::runtime_error("Failed to generate argument IR");

    llvm::Type *expectedTy = funcTy->getParamType(i + offset);

    // Apply ABI coercion if this parameter has coercion info
    if (i < coercion.paramCoercion.size()) {
      const auto &paramInfo = coercion.paramCoercion[i];

      if (paramInfo.isMemory) {
        // Pass large struct by pointer
        // Allocate a temporary and store the value
        llvm::AllocaInst *tempAlloca =
            funcBuilder.CreateAlloca(argVal->getType(), nullptr, "byval.tmp");
        funcBuilder.CreateStore(argVal, tempAlloca);
        argVal = tempAlloca;
      } else if (paramInfo.coercedType &&
                 paramInfo.coercedType != argVal->getType()) {
        // Coerce to the expected type (e.g., struct -> integer)
        if (argVal->getType()->isStructTy() &&
            paramInfo.coercedType->isIntegerTy()) {
          argVal = funcBuilder.CreateBitCast(argVal, paramInfo.coercedType);
        } else if (argVal->getType()->isIntegerTy() &&
                   paramInfo.coercedType->isStructTy()) {
          argVal = funcBuilder.CreateBitCast(argVal, paramInfo.coercedType);
        } else if (argVal->getType()->isStructTy() &&
                   (paramInfo.coercedType->isFloatTy() ||
                    paramInfo.coercedType->isDoubleTy())) {
          argVal = funcBuilder.CreateBitCast(argVal, paramInfo.coercedType);
        }
      }
    }

    // Implicit Promotion: T -> T? (nullable wrapper)
    if (expectedTy->isStructTy() && !argVal->getType()->isStructTy()) {
      llvm::Value *wrapped = llvm::UndefValue::get(expectedTy);
      wrapped =
          funcBuilder.CreateInsertValue(wrapped, funcBuilder.getInt1(true), 0);
      wrapped = funcBuilder.CreateInsertValue(wrapped, argVal, 1);
      argVal = wrapped;
    }

    argsV.push_back(argVal);
  }
  return argsV;
}

std::vector<llvm::Value *> IRGenerator::prepareFnPtrArguments(
    const std::vector<ResolvedType> &paramTypes,
    const std::vector<std::unique_ptr<Expression>> &params) {

  std::vector<llvm::Value *> argsV;

  for (size_t i = 0; i < params.size(); i++) {
    llvm::Value *argVal = generateExpression(params[i].get());
    if (!argVal)
      reportDevBug("Failed to generate fn pointer argument", params[i].get());

    llvm::Type *expectedTy = getLLVMType(paramTypes[i]);

    // Struct coercion
    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(expectedTy)) {
      CoercionInfo info = classifyStruct(structTy);
      if (info.isMemory) {
        // Pass by pointer
        llvm::AllocaInst *tempAlloca =
            funcBuilder.CreateAlloca(argVal->getType(), nullptr, "byval.tmp");
        funcBuilder.CreateStore(argVal, tempAlloca);
        argVal = tempAlloca;
      } else if (info.coercedType && info.coercedType != argVal->getType()) {
        argVal = funcBuilder.CreateBitCast(argVal, info.coercedType);
      }
    }

    // Nullable wrapping
    if (expectedTy->isStructTy() && !argVal->getType()->isStructTy()) {
      llvm::Value *wrapped = llvm::UndefValue::get(expectedTy);
      wrapped =
          funcBuilder.CreateInsertValue(wrapped, funcBuilder.getInt1(true), 0);
      wrapped = funcBuilder.CreateInsertValue(wrapped, argVal, 1);
      argVal = wrapped;
    }

    argsV.push_back(argVal);
  }
  return argsV;
}
