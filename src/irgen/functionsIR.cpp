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
  // Getting the function name
  const std::string &fnName = fnDeclr->function_name->expression.TokenLiteral;

  // If the declaration exists just chill my guy
  if (module->getFunction(fnName)) {
    return;
  }

  auto sym = semantics.getSymbolFromMeta(fnDeclr);
  if(!sym)
    reportDevBug("Failed to get function declaration symbol info",fnDeclr);
  

  llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
  if (sym->isExportable)
    linkage = llvm::Function::ExternalLinkage;

  std::vector<llvm::Type *> paramTypes;
  for (const auto &param : fnDeclr->parameters) {
    auto paramSym = semantics.getSymbolFromMeta(param.get());
    if (!paramSym) {
      reportDevBug(
          "Missing function declaration parameter symbol info",param.get());
    }
    paramTypes.push_back(getLLVMType(paramSym->type().type));
  }
  auto retType = sym->func().returnType;
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

  llvm::Function::Create(fnType, linkage, fnName, module.get());
}

// Generator function for function expression
llvm::Value *IRGenerator::generateFunctionExpression(Node *node) {
  auto fnExpr = dynamic_cast<FunctionExpression *>(node);
  if (!fnExpr)
    reportDevBug("Invalid function expression",fnExpr);

  // Getting the function signature
  auto fnName = fnExpr->func_key.TokenLiteral;
  auto funcSym = semantics.getSymbolFromMeta(fnExpr);

  isGlobalScope = false;

  // Building the function type
  std::vector<llvm::Type *> llvmParamTypes;

  for (auto &p : fnExpr->call) {
    // Getting the data type to push it into getLLVMType
    auto paramSym = semantics.getSymbolFromMeta(p.get());
    if (!paramSym) {
      reportDevBug("Missing parameter symbol info",p.get());
    }
    paramSym->codegen().llvmType = getLLVMType(paramSym->type().type);
    llvmParamTypes.push_back(getLLVMType(paramSym->type().type));
  }

  // Getting the function return type
  auto fnRetType = fnExpr->return_type.get();

  // Get the linkage type(internal by default)
  llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;

  // If it is the main function or a function marked as exportable make it
  // public
  if (fnName == "main" || funcSym->isExportable)
    linkage = llvm::Function::ExternalLinkage;

  auto retType = semantics.inferNodeDataType(fnRetType);
  llvm::FunctionType *funcType =
      llvm::FunctionType::get(getLLVMType(retType), llvmParamTypes, false);

  // Look up if the function declaration exists
  llvm::Function *fn = module->getFunction(fnName);
  // If the function declaration exists
  if (!fn) {
    fn = llvm::Function::Create(funcType, linkage, fnName, module.get());
  } else {
    // If the function was declared checking if the return types match
    if (fn->getFunctionType() != funcType) {
      reportDevBug("Function redefinition for '" + fnName +
                               "' with different signature ",fnExpr);
    }
  }

  currentFunction = fn; // Updating the currentFunction pointer

  // Creating the entry block
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
  funcBuilder.SetInsertPoint(entry);

  // Binding parameters to namedValues
  auto argIter = fn->arg_begin();
  for (auto &p : fnExpr->call) {
    // Getting the statement data type
    auto pIt = semantics.metaData.find(p.get());
    if (pIt == semantics.metaData.end()) {
      reportDevBug("Failed to find paremeter meta data", p.get());
    }
    llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(
        getLLVMType(pIt->second->type().type), nullptr, p->statement.TokenLiteral);
    funcBuilder.CreateStore(&(*argIter), alloca);
    pIt->second->codegen().llvmValue = alloca;

    argIter++;
  }

  llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();

  // This will handle whatever is inside the block including the return value
  generateExpression(fnExpr->block.get());

  llvm::BasicBlock *finalBlock = funcBuilder.GetInsertBlock();
  // If the function is void
  bool isVoidFunction = funcSym->func().returnType.kind == DataType::VOID;

  // Does the current block exist and is it not terminated?
  if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator())) {
    if (isVoidFunction) {
      // Inject 'ret void' for void functions that fell off the end.
      funcBuilder.CreateRetVoid();
      logInternal("Injected missing ret void terminator");
    } else {
      // Non-void function finished without a return.
      // This is a semantic failure, but we terminate for LLVM stability.
      funcBuilder.CreateUnreachable();
    }
  }

  if (oldInsertPoint) {
    funcBuilder.SetInsertPoint(oldInsertPoint);
  } else {
    funcBuilder.ClearInsertionPoint();
  }

  isGlobalScope = true;      // Reset the flag
  currentFunction = nullptr; // Set it back to a null pointer
  return fn;
}

void IRGenerator::generateReturnStatement(Node *node) {
  auto retStmt = dynamic_cast<ReturnStatement *>(node);
  if (!retStmt)
    return;

  // Fetch Semantic Metadata
  auto it = semantics.metaData.find(retStmt);
  if (it == semantics.metaData.end()) {
    reportDevBug("Could not find return statement metaData", retStmt);
  }
  inhibitCleanUp = true;
  // Generate the value being returned
  llvm::Value *retVal = nullptr;
  if (retStmt->return_value) {
    retVal = generateExpression(retStmt->return_value.get());
  }

  inhibitCleanUp = false;
  emitCleanup(retStmt);

  // Get Function Signature Info
  llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
  llvm::Type *retTy = currentFunction->getReturnType();

  // Handle Void Returns
  if (retTy->isVoidTy()) {
    funcBuilder.CreateRetVoid();
    return;
  }

  // Safety check: if function expects a value but we have none
  if (!retVal) {
    llvm::errs() << "Error: Non-void function missing return value.\n";
    funcBuilder.CreateUnreachable();
    return;
  }

  if (it->second->type().type.isNull) {
    if (!retVal->getType()->isStructTy()) {

      llvm::Value *boxed = llvm::UndefValue::get(retTy);

      // Check if we are returning 'null' literal
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

  llvm::Type *valTy = retVal->getType();

  if (valTy == retTy) {
    funcBuilder.CreateRet(retVal);
    return;
  }

  // Pointer Bitcasting
  if (valTy->isPointerTy() && retTy->isPointerTy()) {
    funcBuilder.CreateRet(funcBuilder.CreateBitCast(retVal, retTy));
    return;
  }

  // Integer Width Adaptation
  if (valTy->isIntegerTy() && retTy->isIntegerTy()) {
    funcBuilder.CreateRet(funcBuilder.CreateZExtOrTrunc(retVal, retTy));
    return;
  }

  llvm::errs() << "IRGen Error: Return type mismatch. Cannot adapt " << *valTy
               << " to " << *retTy << "\n";
  funcBuilder.CreateUnreachable();
}

llvm::Value *IRGenerator::generateCallExpression(Node *node) {
  auto callExpr = dynamic_cast<CallExpression *>(node);
  if (!callExpr) {
    throw std::runtime_error("Invalid call expression");
  }

  if (isGlobalScope)
    throw std::runtime_error("Function calls are not allowed at global scope");

  // Getting the function name
  const std::string &fnName =
      callExpr->function_identifier->expression.TokenLiteral;

  auto callIt = semantics.metaData.find(callExpr);
  if (callIt == semantics.metaData.end()) {
    reportDevBug("Call expression does not exist", callExpr);
  }

  // Getting the function I want to call
  llvm::Function *calledFunc = module->getFunction(fnName);

  if (!calledFunc) {
    throw std::runtime_error("Unknown function '" + fnName + "'referenced");
  }

  std::vector<llvm::Value *> argsV =
      prepareArguments(calledFunc, callExpr->parameters, 0);

  // Emitting the function call itself
  llvm::Value *call = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

  // Check if the function return type is void
  if (calledFunc->getReturnType()->isVoidTy()) {
    return nullptr;
  }

  emitCleanup(callExpr);
  return call;
}

llvm::Value *IRGenerator::generateCallAddress(Node *node) {
  auto callExpr = dynamic_cast<CallExpression *>(node);
  if (!callExpr)
    throw std::runtime_error(
        "Invalid call expression inside address generator");

  if (isGlobalScope)
    throw std::runtime_error("Function calls are not allowed in global scope");

  const std::string &funcName =
      callExpr->function_identifier->expression.TokenLiteral;

  auto callIt = semantics.metaData.find(callExpr);
  if (callIt == semantics.metaData.end())
    throw std::runtime_error("Call expression metaData not found");

  if (callIt->second->hasError)
    throw std::runtime_error("Semantic error detected in call for '" +
                             funcName + "'");

  llvm::Function *calledFunc = module->getFunction(funcName);
  if (!calledFunc)
    throw std::runtime_error("Unknown function '" + funcName + "' referenced");

  // Generate IR for each argument
  std::vector<llvm::Value *> argsV =
      prepareArguments(calledFunc, callExpr->parameters, 0);

  llvm::Type *retTy = calledFunc->getReturnType();
  if (retTy->isVoidTy())
    throw std::runtime_error("Attempted to take address of a void call result");

  // Ensure the temporary alloca is created in the entry block of the current
  // function.
  llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
  llvm::IRBuilder<> tmpBuilder(&currentFunction->getEntryBlock(),
                               currentFunction->getEntryBlock().begin());
  llvm::AllocaInst *tmpAlloca =
      tmpBuilder.CreateAlloca(retTy, nullptr, "calltmp.addr");

  // Emit the call (value returned into caller)
  llvm::Value *callVal = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

  // Store the returned value into the caller-owned temporary and return its
  // address.
  funcBuilder.CreateStore(callVal, tmpAlloca);

  return tmpAlloca; // pointer to caller-allocated storage containing the call
                    // result
}

std::vector<llvm::Value *> IRGenerator::prepareArguments(
    llvm::Function *func,
    const std::vector<std::unique_ptr<Expression>> &params, size_t offset) {
  std::vector<llvm::Value *> argsV;
  auto funcTy = func->getFunctionType();

  for (size_t i = 0; i < params.size(); ++i) {
    llvm::Value *argVal = nullptr;

    auto metaIt = semantics.metaData.find(params[i].get());
    if (metaIt != semantics.metaData.end() &&
        metaIt->second->type().needsImplicitAddress) {
      // Instead of loading the value, we grab the raw address
      argVal = generateIdentifierAddress(params[i].get());
    } else {
      // Normal behavior like load a 10
      argVal = generateExpression(params[i].get());
    }

    if (!argVal)
      throw std::runtime_error("Failed to generate argument IR");

    llvm::Type *expectedTy = funcTy->getParamType(i + offset);

    // Implicit Promotion: T -> T?
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
