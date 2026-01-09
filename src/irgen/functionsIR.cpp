#include "irgen.hpp"

void IRGenerator::generateFunctionStatement(Node *node)
{
    auto fnStmt = dynamic_cast<FunctionStatement *>(node);
    if (!fnStmt)
        return;

    llvm::BasicBlock *oldInsertPoint = funcBuilder.GetInsertBlock();

    // Checking what the function statement is holding could be a function expression or a function declaration expression
    auto fnExpr = fnStmt->funcExpr.get();
    // Case where it is a full function expression
    if (auto expr = dynamic_cast<FunctionExpression *>(fnExpr))
    {
        generateFunctionExpression(expr);
    }
    // Case where it is a function declaration expression(this is the special case)
    if (auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnExpr))
    {
        /*This is actually supposed to behave like a statement dispatcher
        That is because it doesnt actually produce a value it is a wrapper and doesnt evaluate to anything
        But with the  way I built the AST for my function declaration I have to do it like this kinda shady though
        But it will work*/
        generateFunctionDeclarationExpression(declrExpr);
    }

    if (oldInsertPoint)
        funcBuilder.SetInsertPoint(oldInsertPoint);
}

void IRGenerator::generateFunctionDeclarationExpression(Node *node)
{
    auto declrExpr = dynamic_cast<FunctionDeclarationExpression *>(node);
    if (!declrExpr)
        return;

    // This is also just a wrapper for the function declaration expression so let me just call the statement generator
    auto fnDeclr = declrExpr->funcDeclrStmt.get();
    // Call the statement generator for the function declaration statement
    generateFunctionDeclaration(fnDeclr);
}

void IRGenerator::generateFunctionDeclaration(Node *node)
{
    auto fnDeclr = dynamic_cast<FunctionDeclaration *>(node);
    if (!fnDeclr)
        return;
    // Getting the function name
    const std::string &fnName = fnDeclr->function_name->expression.TokenLiteral;

    // Registering the function and its type
    auto declrIt = semantics.metaData.find(fnDeclr);
    if (declrIt == semantics.metaData.end())
    {
        throw std::runtime_error("Missing function declaration meta data");
    }

    auto sym = declrIt->second;
    if (sym->hasError)
        throw std::runtime_error("Semantic error detected");

    std::vector<llvm::Type *> paramTypes;
    for (const auto &param : fnDeclr->parameters)
    {
        auto it = semantics.metaData.find(param.get());
        if (it == semantics.metaData.end())
        {
            throw std::runtime_error("Missing function declaration parameter meta data");
        }
        paramTypes.push_back(getLLVMType(it->second->type));
    }
    auto retType = declrIt->second->returnType;
    llvm::FunctionType *fnType = llvm::FunctionType::get(getLLVMType(retType), paramTypes, false);

    llvm::Function *declaredFunc = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, fnName, module.get());
}

// Generator function for function expression
llvm::Value *IRGenerator::generateFunctionExpression(Node *node)
{
    auto fnExpr = dynamic_cast<FunctionExpression *>(node);
    if (!fnExpr)
        throw std::runtime_error("Invalid function expression");

    // If node has an error we wont generate IR
    auto funcIt = semantics.metaData.find(fnExpr);
    if (funcIt == semantics.metaData.end())
    {
        throw std::runtime_error("Function expression does not exist");
    }
    if (funcIt->second->hasError)
    {
        throw std::runtime_error("Semantic error detected");
    }

    // Getting the function signature
    auto fnName = fnExpr->func_key.TokenLiteral;

    isGlobalScope = false;

    // Building the function type
    std::vector<llvm::Type *> llvmParamTypes;

    for (auto &p : fnExpr->call)
    {
        // Getting the data type to push it into getLLVMType
        auto it = semantics.metaData.find(p.get());
        if (it == semantics.metaData.end())
        {
            throw std::runtime_error("Missing parameter meta data");
        }
        it->second->llvmType = getLLVMType(it->second->type);
        llvmParamTypes.push_back(getLLVMType(it->second->type));
    }

    // Getting the function return type
    auto fnRetType = fnExpr->return_type.get();

    auto retType = semantics.inferNodeDataType(fnRetType);
    llvm::FunctionType *funcType = llvm::FunctionType::get(getLLVMType(retType), llvmParamTypes, false);

    // Look up if the function declaration exists
    llvm::Function *fn = module->getFunction(fnName);
    // If the function declaration exists
    if (!fn)
    {
        fn = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, fnName, module.get());
    }
    else
    {
        // If the function was declared checking if the return types match
        if (fn->getFunctionType() != funcType)
        {
            throw std::runtime_error("Function redefinition for '" + fnName + "' with different signature ");
        }
    }

    currentFunction = fn; // Updating the currentFunction pointer
    if (!currentFunction)
    {
        std::cerr << "Current function pointer updated \n";
    }

    // Creating the entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
    funcBuilder.SetInsertPoint(entry);

    // Binding parameters to namedValues
    auto argIter = fn->arg_begin();
    for (auto &p : fnExpr->call)
    {
        // Getting the statement data type
        auto pIt = semantics.metaData.find(p.get());
        if (pIt == semantics.metaData.end())
        {
            throw std::runtime_error("Failed to find paremeter meta data");
        }
        llvm::AllocaInst *alloca = funcBuilder.CreateAlloca(getLLVMType(pIt->second->type), nullptr, p->statement.TokenLiteral);
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
    if (finalBlock && (finalBlock->empty() || !finalBlock->getTerminator()))
    {
        if (isVoidFunction)
        {
            // Inject 'ret void' for void functions that fell off the end.
            funcBuilder.CreateRetVoid();
            std::cout << "INJECTED missing 'ret void' terminator.\n";
        }
        else
        {
            // Non-void function finished without a return.
            // This is a semantic failure, but we terminate for LLVM stability.
            funcBuilder.CreateUnreachable();
        }
    }

    if (oldInsertPoint)
    {
        funcBuilder.SetInsertPoint(oldInsertPoint);
    }
    else
    {
        funcBuilder.ClearInsertionPoint();
    }

    isGlobalScope = true;      // Reset the flag
    currentFunction = nullptr; // Set it back to a null pointer
    return fn;
}

void IRGenerator::generateReturnStatement(Node *node)
{
    auto retStmt = dynamic_cast<ReturnStatement *>(node);
    if (!retStmt)
        return;

    // Retrieve the metaData (for error checking)
    auto it = semantics.metaData.find(retStmt);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Missing return statement metaData");
    }

    if (it->second->hasError)
    {
        throw std::runtime_error("Semantic error detected");
    }

    llvm::Value *retVal = nullptr;
    if (retStmt->return_value)
    {
        retVal = generateExpression(retStmt->return_value.get());
    }

    llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
    llvm::Type *retTy = currentFunction->getReturnType();

    // Void return
    if (retTy->isVoidTy())
    {
        if (retVal)
        {
            llvm::errs() << "Warning: returning a value from a void function; value will be ignored\n";
        }
        funcBuilder.CreateRetVoid();
        return;
    }

    // Non-void return: we expect a value
    if (!retVal)
    {
        llvm::errs() << "Return statement missing value for non-void function\n";
        return;
    }

    // If the returned value type doesn't match function return type, try to adapt
    llvm::Type *valTy = retVal->getType();
    if (valTy == retTy)
    {
        funcBuilder.CreateRet(retVal);
        return;
    }

    // If both are pointer types and point to compatible element, bitcast the pointer
    if (valTy->isPointerTy() && retTy->isPointerTy())
    {
        llvm::Value *casted = funcBuilder.CreateBitCast(retVal, retTy);
        funcBuilder.CreateRet(casted);
        return;
    }

    // If returning an aggregate (struct) by value but retVal is a pointer-to-aggregate (unlikely),
    // load and return the aggregate value.
    if (valTy->isPointerTy() && llvm::isa<llvm::StructType>(retTy))
    {
        llvm::Value *loaded = funcBuilder.CreateLoad(retTy, retVal);
        funcBuilder.CreateRet(loaded);
        return;
    }

    // If both are integer types of different widths, extend/truncate.
    if (valTy->isIntegerTy() && retTy->isIntegerTy())
    {
        unsigned vbits = llvm::cast<llvm::IntegerType>(valTy)->getBitWidth();
        unsigned rbits = llvm::cast<llvm::IntegerType>(retTy)->getBitWidth();
        if (vbits < rbits)
            funcBuilder.CreateRet(funcBuilder.CreateSExt(retVal, retTy));
        else if (vbits > rbits)
            funcBuilder.CreateRet(funcBuilder.CreateTrunc(retVal, retTy));
        else
            funcBuilder.CreateRet(retVal);
        return;
    }

    // Fallback types are incompatible — emit an error and try to bitcast if possible.(This wont happen because semantics must have caught it but who knows)
    llvm::errs() << "Return type mismatch: returning '" << *valTy << "' but function expects '" << *retTy << "'\n";

    // Last-chance attempt, try a bitcast if sizes match (risky)
    if (valTy->getPrimitiveSizeInBits() == retTy->getPrimitiveSizeInBits())
    {
        llvm::Value *maybe = funcBuilder.CreateBitCast(retVal, retTy);
        funcBuilder.CreateRet(maybe);
        return;
    }
}

llvm::Value *IRGenerator::generateCallExpression(Node *node)
{
    auto callExpr = dynamic_cast<CallExpression *>(node);
    if (!callExpr)
    {
        throw std::runtime_error("Invalid call expression");
    }

    if (isGlobalScope)
        throw std::runtime_error("Function calls are not allowed at global scope");

    // Getting the function name
    const std::string &fnName = callExpr->function_identifier->expression.TokenLiteral;

    auto callIt = semantics.metaData.find(callExpr);
    if (callIt == semantics.metaData.end())
    {
        throw std::runtime_error("Call expression does not exist");
    }
    if (callIt->second->hasError)
        throw std::runtime_error("Semantic error detected in '" + fnName + "' call");

    // Getting the function I want to call
    llvm::Function *calledFunc = module->getFunction(fnName);

    if (!calledFunc)
    {
        throw std::runtime_error("Unknown function '" + fnName + "'referenced");
    }

    // Generate IR for each argument
    std::vector<llvm::Value *> argsV;
    for (const auto &arg : callExpr->parameters)
    {
        llvm::Value *argVal = generateExpression(arg.get());
        if (!argVal)
        {
            throw std::runtime_error("Argument codegen failed");
        }
        argsV.push_back(argVal);
    }

    // Emitting the function call itself
    llvm::Value *call = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

    // Check if the function return type is void
    if (calledFunc->getReturnType()->isVoidTy())
    {
        return nullptr;
    }

    // If the return is nullable
    if (callIt->second->returnType.isNull)
    {
        // Extract the success value
        return funcBuilder.CreateExtractValue(call, {0}, "success");
    }
    return call;
}

llvm::Value *IRGenerator::generateCallAddress(Node *node)
{
    auto callExpr = dynamic_cast<CallExpression *>(node);
    if (!callExpr)
        throw std::runtime_error("Invalid call expression inside address generator");

    if (isGlobalScope)
        throw std::runtime_error("Function calls are not allowed in global scope");

    const std::string &funcName = callExpr->function_identifier->expression.TokenLiteral;

    auto callIt = semantics.metaData.find(callExpr);
    if (callIt == semantics.metaData.end())
        throw std::runtime_error("Call expression metaData not found");

    if (callIt->second->hasError)
        throw std::runtime_error("Semantic error detected in call for '" + funcName + "'");

    llvm::Function *calledFunc = module->getFunction(funcName);
    if (!calledFunc)
        throw std::runtime_error("Unknown function '" + funcName + "' referenced");

    // Generate IR for each argument
    std::vector<llvm::Value *> argsV;
    for (const auto &arg : callExpr->parameters)
    {
        llvm::Value *argVal = generateExpression(arg.get());
        if (!argVal)
            throw std::runtime_error("Argument codegen failed");
        argsV.push_back(argVal);
    }

    llvm::Type *retTy = calledFunc->getReturnType();
    if (retTy->isVoidTy())
        throw std::runtime_error("Attempted to take address of a void call result");

    // Ensure the temporary alloca is created in the entry block of the current function.
    llvm::Function *currentFunction = funcBuilder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&currentFunction->getEntryBlock(),
                                 currentFunction->getEntryBlock().begin());
    llvm::AllocaInst *tmpAlloca = tmpBuilder.CreateAlloca(retTy, nullptr, "calltmp.addr");

    // Emit the call (value returned into caller)
    llvm::Value *callVal = funcBuilder.CreateCall(calledFunc, argsV, "calltmp");

    // Store the returned value into the caller-owned temporary and return its address.
    funcBuilder.CreateStore(callVal, tmpAlloca);

    return tmpAlloca; // pointer to caller-allocated storage containing the call result
}

llvm::Value *IRGenerator::generateUnwrapCallExpression(Node *node)
{
    auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node);
    if (!unwrapExpr)
        throw std::runtime_error("Invalid unwrap expression call");

    auto unIt = semantics.metaData.find(unwrapExpr);
    if (unIt == semantics.metaData.end())
        throw std::runtime_error("Missing unwrap metaData");

    if (unIt->second->hasError)
        throw std::runtime_error("Semantic error detected");

    if (!unIt->second->returnType.isNull)
        throw std::runtime_error("Cannot unwrap a non-nullable function return");

    auto call = dynamic_cast<CallExpression *>(unwrapExpr->call.get());
    const std::string &fnName = call->function_identifier->expression.TokenLiteral;

    llvm::Function *calledFn = module->getFunction(fnName);

    if (!calledFn)
    {
        throw std::runtime_error("Unknown function '" + fnName + "' referenced");
    }

    // Generate IR for each argument
    std::vector<llvm::Value *> argsV;
    for (const auto &arg : call->parameters)
    {
        llvm::Value *argVal = generateExpression(arg.get());
        if (!argVal)
        {
            throw std::runtime_error("Argument codegen failed");
        }
        argsV.push_back(argVal);
    }

    llvm::Value *raw = funcBuilder.CreateCall(calledFn, argsV, "unwrap_calltmp");

    // Return the error value
    return funcBuilder.CreateExtractValue(raw, {1}, "error");
}