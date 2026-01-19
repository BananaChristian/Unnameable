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
  // Case where it is a function declaration expression(this is the special
  // case)
  if (auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnExpr)) {
    /*This is actually supposed to behave like a statement dispatcher
    That is because it doesnt actually produce a value it is a wrapper and
    doesnt evaluate to anything But with the  way I built the AST for my
    function declaration I have to do it like this kinda shady though But it
    will work*/
    generateFunctionDeclarationExpression(declrExpr);
  }

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

  // Registering the function and its type
  auto declrIt = semantics.metaData.find(fnDeclr);
  if (declrIt == semantics.metaData.end()) {
    throw std::runtime_error("Missing function declaration meta data");
  }

  auto sym = declrIt->second;
  if (sym->hasError)
    throw std::runtime_error("Error detected");

  std::vector<llvm::Type *> paramTypes;
  for (const auto &param : fnDeclr->parameters) {
    auto paramIt = semantics.metaData.find(param.get());
    if (paramIt == semantics.metaData.end()) {
      throw std::runtime_error(
          "Missing function declaration parameter meta data");
    }
    paramTypes.push_back(getLLVMType(paramIt->second->type));
  }
  auto retType = declrIt->second->returnType;
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

  llvm::Function *declaredFunc = llvm::Function::Create(
      fnType, llvm::Function::ExternalLinkage, fnName, module.get());
}

// Generator function for function expression
llvm::Value *IRGenerator::generateFunctionExpression(Node *node) {
  auto fnExpr = dynamic_cast<FunctionExpression *>(node);
  if (!fnExpr)
    throw std::runtime_error("Invalid function expression");

  // If node has an error we wont generate IR
  auto funcIt = semantics.metaData.find(fnExpr);
  if (funcIt == semantics.metaData.end()) {
    throw std::runtime_error("Function expression does not exist");
  }
  if (funcIt->second->hasError) {
    throw std::runtime_error("Error detected");
  }

  // Getting the function signature
  auto fnName = fnExpr->func_key.TokenLiteral;
  auto funcSym = funcIt->second;

  isGlobalScope = false;

  // Building the function type
  std::vector<llvm::Type *> llvmParamTypes;

  for (auto &p : fnExpr->call) {
    // Getting the data type to push it into getLLVMType
    auto paramIt = semantics.metaData.find(p.get());
    if (paramIt == semantics.metaData.end()) {
      throw std::runtime_error("Missing parameter meta data");
    }
    paramIt->second->llvmType = getLLVMType(paramIt->second->type);
    llvmParamTypes.push_back(getLLVMType(paramIt->second->type));
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
      throw std::runtime_error("Function redefinition for '" + fnName +
                               "' with different signature ");
    }
  }

  currentFunction = fn; // Updating the currentFunction pointer
  if (!currentFunction) {
    std::cerr << "Current function pointer updated \n";
  }

  // Creating the entry block
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
  funcBuilder.SetInsertPoint(entry);

  // Binding parameters to namedValues
  auto argIter = fn->arg_begin();
  for (auto &p : fnExpr->call) {
    // Getting the statement data type
    auto pIt = semantics.metaData.find(p.get());
    if (pIt == semantics.metaData.end()) {
      throw std::runtime_error("Failed to find paremeter meta data");
    }
    llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(
        getLLVMType(pIt->second->type), nullptr, p->statement.TokenLiteral);
    funcBuilder.CreateStore(&(*argIter), alloca);
    pIt->second->llvmValue = alloca;

    argIter++;
  }

  llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();

  // This will handle whatever is inside the block including the return value
  generateExpression(fnExpr->block.get());

  llvm::BasicBlock *finalBlock = funcBuilder.GetInsertBlock();

  // If the function is void
  bool isVoidFunction = funcIt->second->returnType.kind == DataType::VOID;

  // CRITICAL CHECK: Does the current block exist and is it not terminated?
  if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator())) {
    if (isVoidFunction) {
      // Inject 'ret void' for void functions that fell off the end.
      funcBuilder.CreateRetVoid();
      std::cout << "INJECTED missing 'ret void' terminator.\n";
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
    throw std::runtime_error("IRGen Error: Return statement missing metadata.");
  }

  // Generate the value being returned
  llvm::Value *retVal = nullptr;
  if (retStmt->return_value) {
    retVal = generateExpression(retStmt->return_value.get());
  }

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

  if (it->second->type.isNull) {
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
    throw std::runtime_error("Call expression does not exist");
  }
  if (callIt->second->hasError)
    throw std::runtime_error("Semantic error detected in '" + fnName +
                             "' call");

  // Getting the function I want to call
  llvm::Function *calledFunc = module->getFunction(fnName);

  if (!calledFunc) {
    throw std::runtime_error("Unknown function '" + fnName + "'referenced");
  }

  std::vector<llvm::Value *> argsV =
      prepareArguments(calledFunc, callExpr->parameters,0);

  // Emitting the function call itself
  llvm::Value *call = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

  // Check if the function return type is void
  if (calledFunc->getReturnType()->isVoidTy()) {
    return nullptr;
  }

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
      prepareArguments(calledFunc, callExpr->parameters,0);

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
    const std::vector<std::unique_ptr<Expression>> &params,size_t offset) {
  std::vector<llvm::Value *> argsV;
  auto funcTy = func->getFunctionType();

  for (size_t i = 0; i < params.size(); ++i) {
    llvm::Value *argVal = nullptr;

    auto metaIt = semantics.metaData.find(params[i].get());
    if (metaIt != semantics.metaData.end() &&
        metaIt->second->needsImplicitAddress) {
      // Instead of loading the value, we grab the raw address
      AddressAndPendingFree addrInfo =
          generateIdentifierAddress(params[i].get());
      argVal = addrInfo.address;
    } else {
      // Normal behavior like load a 10
      argVal = generateExpression(params[i].get());
    }

    if (!argVal)
      throw std::runtime_error("Failed to generate argument IR");

    llvm::Type *expectedTy = funcTy->getParamType(i+offset);

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
