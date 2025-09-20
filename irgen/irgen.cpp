#include "irgen.hpp"
#include "ast.hpp"

#include <iostream>
#define CPPREST_FORCE_REBUILD
IRGenerator::IRGenerator(Semantics &semantics)
    : semantics(semantics), context(), builder(context), module(std::make_unique<llvm::Module>("unnameable", context))
{
    registerGeneratorFunctions();
    registerExpressionGeneratorFunctions();
}

// MAIN GENERATOR FUNCTION
void IRGenerator::generate(const std::vector<std::unique_ptr<Node>> &program)
{
    llvm::FunctionType *funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);
    llvm::Function *mainFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", module.get());
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);

    for (const auto &node : program)
    {
        generateStatement(node.get());
    }

    builder.SetInsertPoint(entry);

    builder.CreateRet(llvm::ConstantInt::get(context, llvm::APInt(32, 0)));
}

// MAIN GENERATOR FUNCTION FOR EXPRESSION
//  Main Expression generator helper function
llvm::Value *IRGenerator::generateExpression(Node *node)
{
    auto exprIt = expressionGeneratorsMap.find(typeid(*node));
    if (exprIt == expressionGeneratorsMap.end())
    {
        throw std::runtime_error("Could not find expression type IR generator: " + node->toString());
    }
    return (this->*exprIt->second)(node);
}

// GENERATOR FUNCTIONS
void IRGenerator::generateStatement(Node *node)
{
    auto generatorIt = generatorFunctionsMap.find(typeid(*node));
    if (generatorIt == generatorFunctionsMap.end())
    {
        std::cout << "Failed to find statement IR generator for : " << node->toString() << "\n";
        return;
    }
    (this->*generatorIt->second)(node);
}
// STATEMENT GENERATOR FUNCTIONS
// Let statement IR generator function
void IRGenerator::generateLetStatement(Node *node)
{
    std::cout << "IR GEN FOR let statements" << node->toString() << "\n";
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
    {
        throw std::runtime_error("Invalid let Statement node");
    }

    const std::string &letName = letStmt->ident_token.TokenLiteral;
    auto letIt = semantics.metaData.find(letStmt);
    if (letIt == semantics.metaData.end())
    {
        throw std::runtime_error("No let statement for variable '" + letName + "'");
    }
    if (letIt->second->hasError)
    {
        return;
    }
    auto letType = letIt->second->type;

    llvm::Type *varType = getLLVMType(letType);
    if (!varType)
    {
        throw std::runtime_error("Invalid type for variable: " + letName);
    }

    if (inFunction())
    {
        // allocate in function entry (recommended)
        llvm::Function *fn = builder.GetInsertBlock()->getParent();
        llvm::IRBuilder<> entryBuilder(&fn->getEntryBlock(), fn->getEntryBlock().begin());
        auto *alloca = entryBuilder.CreateAlloca(varType, nullptr, letName);
        letIt->second->llvmValue = alloca;

        if (letStmt->value)
        {
            llvm::Value *init = generateExpression(letStmt->value.get());
            builder.CreateStore(init, alloca);
        }
    }
    else
    {
        llvm::Constant *initConst = llvm::Constant::getNullValue(varType);
        auto *gv = new llvm::GlobalVariable(
            *module, varType, false,
            llvm::GlobalValue::ExternalLinkage, initConst, letName);
        letIt->second->llvmValue = gv;
    }
}

// While statement IR generator function
void IRGenerator::generateWhileStatement(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
    {
        throw std::runtime_error("Invalid while statement");
    }

    llvm::Function *function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "while.cond", function);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "while.body");
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(context, "while.end");

    std::cerr << "[IR DEBUG] Creating branch to while.cond\n";
    builder.CreateBr(condBB);

    builder.SetInsertPoint(condBB);
    std::cerr << "[IR DEBUG] Generating while condition\n";
    llvm::Value *condVal = generateExpression(whileStmt->condition.get());
    if (!condVal)
    {
        throw std::runtime_error("Invalid while condition");
    }

    if (!condVal->getType()->isIntegerTy(1))
    {
        condVal = builder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "whilecond.bool");
    }

    std::cerr << "[IR DEBUG] Creating conditional branch\n";
    builder.CreateCondBr(condVal, bodyBB, endBB);

    // Append bodyBB to function
    function->insert(function->end(), bodyBB);
    builder.SetInsertPoint(bodyBB);
    std::cerr << "[IR DEBUG] Generating while body\n";
    loopBlocksStack.push_back({condBB, endBB});
    generateStatement(whileStmt->loop.get());
    loopBlocksStack.pop_back();
    std::cerr << "[IR DEBUG] Finished generating while body\n";

    if (!builder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to while.cond\n";
        builder.CreateBr(condBB);
    }
    else
    {
        std::cerr << "[IR WARNING] While body block already has terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // Append endBB to function
    function->insert(function->end(), endBB);
    builder.SetInsertPoint(endBB);
}

// IR code gen for an if statement
void IRGenerator::generateIfStatement(Node *node)
{
    auto ifStmt = dynamic_cast<ifStatement *>(node);
    if (!ifStmt)
    {
        throw std::runtime_error("Invalid if statement");
    }

    std::cerr << "[IR DEBUG] Generating if statement\n";

    // Generation of condition for the if
    llvm::Value *condVal = generateExpression(ifStmt->condition.get());
    if (!condVal)
    {
        throw std::runtime_error("Invalid if condition");
    }

    if (!condVal->getType()->isIntegerTy(1))
    {
        condVal = builder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "ifcond.bool");
    }

    // Create basic blocks
    llvm::Function *function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", function);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifmerge");

    // Determine the next block (first elif, else, or merge)
    llvm::BasicBlock *nextBB = nullptr;
    if (!ifStmt->elifClauses.empty())
    {
        nextBB = llvm::BasicBlock::Create(context, "elif0");
    }
    else if (ifStmt->else_result.has_value())
    {
        nextBB = llvm::BasicBlock::Create(context, "else");
    }
    else
    {
        nextBB = mergeBB;
    }

    // Conditional branch for if
    std::cerr << "[IR DEBUG] Creating conditional branch for if\n";
    builder.CreateCondBr(condVal, thenBB, nextBB);

    // Generate then branch
    builder.SetInsertPoint(thenBB);
    std::cerr << "[IR DEBUG] Generating then branch\n";
    generateStatement(ifStmt->if_result.get());
    if (!builder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to ifmerge from then\n";
        builder.CreateBr(mergeBB);
    }
    else
    {
        std::cerr << "[IR DEBUG] Skipping branch to ifmerge from then due to terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // Generating elif branches
    for (size_t i = 0; i < ifStmt->elifClauses.size(); ++i)
    {
        function->insert(function->end(), nextBB);
        builder.SetInsertPoint(nextBB);
        std::cerr << "[IR DEBUG] Generating elif branch " << i << "\n";

        const auto &elifStmt = ifStmt->elifClauses[i];
        auto elif = dynamic_cast<elifStatement *>(elifStmt.get());
        llvm::Value *elifCondVal = generateExpression(elif->elif_condition.get());
        if (!elifCondVal)
        {
            throw std::runtime_error("Invalid elif condition");
        }
        if (!elifCondVal->getType()->isIntegerTy(1))
        {
            elifCondVal = builder.CreateICmpNE(elifCondVal, llvm::ConstantInt::get(elifCondVal->getType(), 0), "elifcond.bool");
        }

        llvm::BasicBlock *elifBodyBB = llvm::BasicBlock::Create(context, "elif.body" + std::to_string(i), function);
        llvm::BasicBlock *nextElifBB = (i + 1 < ifStmt->elifClauses.size()) ? llvm::BasicBlock::Create(context, "elif" + std::to_string(i + 1))
                                                                            : (ifStmt->else_result.has_value() ? llvm::BasicBlock::Create(context, "else") : mergeBB);

        std::cerr << "[IR DEBUG] Creating conditional branch for elif " << i << "\n";
        builder.CreateCondBr(elifCondVal, elifBodyBB, nextElifBB);

        builder.SetInsertPoint(elifBodyBB);
        std::cerr << "[IR DEBUG] Generating elif body " << i << "\n";
        generateStatement(elif->elif_result.get());
        if (!builder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Adding branch to ifmerge from elif " << i << "\n";
            builder.CreateBr(mergeBB);
        }
        else
        {
            std::cerr << "[IR DEBUG] Skipping branch " << i << " to ifmerge from elif due to terminator " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
        }

        nextBB = nextElifBB;
    }

    // Generate else branch if present
    if (ifStmt->else_result.has_value())
    {
        function->insert(function->end(), nextBB);
        builder.SetInsertPoint(nextBB);
        std::cerr << "[IR DEBUG] Generating else branch\n";
        generateStatement(ifStmt->else_result.value().get());
        if (!builder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Adding branch to ifmerge from else\n";
            builder.CreateBr(mergeBB);
        }
        else
        {
            std::cerr << "[IR DEBUG] Skipping branch to ifmerge from else due to terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
        }
    }

    // Finalize with merge block
    function->insert(function->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    std::cerr << "[IR DEBUG] Finished generating if statement\n";
}

// IR code gen for a for loop
void IRGenerator::generateForStatement(Node *node)
{
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
    {
        throw std::runtime_error("Invalid for statement");
    }

    llvm::Function *function = builder.GetInsertBlock()->getParent();

    // Handle initializer
    if (forStmt->initializer)
    {
        std::cerr << "[IR DEBUG] Generating initializer\n";
        generateStatement(forStmt->initializer.get());
    }

    // Create all necessary blocks
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "loop.cond", function);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "loop.body");
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(context, "loop.step");
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(context, "loop.end");

    // Jump to condition
    std::cerr << "[IR DEBUG] Creating branch to loop.cond\n";
    builder.CreateBr(condBB);

    // Condition block
    builder.SetInsertPoint(condBB);
    std::cerr << "[IR DEBUG] Generating condition\n";
    llvm::Value *condVal = generateExpression(forStmt->condition.get());
    if (!condVal)
    {
        std::cerr << "[IR ERROR] For loop has invalid condition.\n";
        return;
    }

    // Verify condition type in metadata
    auto it = semantics.metaData.find(forStmt->condition.get());
    if (it == semantics.metaData.end() || it->second->type.kind != DataType::BOOLEAN)
    {
        std::cerr << "[IR ERROR] For loop condition must evaluate to boolean.\n";
        return;
    }

    // Promote i32 -> i1 if necessary
    if (!condVal->getType()->isIntegerTy(1))
    {
        if (condVal->getType()->isIntegerTy(32))
        {
            condVal = builder.CreateICmpNE(condVal, llvm::ConstantInt::get(condVal->getType(), 0), "loopcond.bool");
        }
        else
        {
            std::cerr << "[IR ERROR] For loop condition must be boolean or i32\n";
            return;
        }
    }

    std::cerr << "[IR DEBUG] Creating conditional branch\n";
    builder.CreateCondBr(condVal, bodyBB, endBB);

    // Append bodyBB to function
    function->insert(function->end(), bodyBB);
    builder.SetInsertPoint(bodyBB);
    loopBlocksStack.push_back({condBB, endBB});
    std::cerr << "[IR DEBUG] Generating loop body\n";
    generateStatement(forStmt->body.get());
    std::cerr << "[IR DEBUG] Finished generating loop body\n";
    loopBlocksStack.pop_back();

    // Ensure branch to step if no terminator
    if (!builder.GetInsertBlock()->getTerminator())
    {
        std::cerr << "[IR DEBUG] Adding branch to loop.step\n";
        builder.CreateBr(stepBB);
    }
    else
    {
        std::cerr << "[IR WARNING] Loop body block already has terminator: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
    }

    // Append stepBB to function
    function->insert(function->end(), stepBB);
    builder.SetInsertPoint(stepBB);

    // Loop step
    if (forStmt->step)
    {
        std::cerr << "[IR DEBUG] Generating loop step\n";
        generateExpression(forStmt->step.get());
    }

    // Jump back to condition
    std::cerr << "[IR DEBUG] Creating branch to loop.cond\n";
    builder.CreateBr(condBB);

    // Append end block and move builder there
    function->insert(function->end(), endBB);
    builder.SetInsertPoint(endBB);
}

void IRGenerator::generateBreakStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Break statement not inside a loop");
    }

    llvm::BasicBlock *afterBB = loopBlocksStack.back().afterBB;
    std::cerr << "[IR DEBUG] Generating break to " << afterBB->getName().str() << "\n";
    builder.CreateBr(afterBB);
}

void IRGenerator::generateContinueStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Continue statement not inside a loop");
    }

    llvm::BasicBlock *condBB = loopBlocksStack.back().condBB;
    std::cerr << "[IR DEBUG] Generating continue to " << condBB->getName().str() << "\n";
    builder.CreateBr(condBB);
}

// Expression statement IR generator function
void IRGenerator::generateExpressionStatement(Node *node)
{
    auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
    if (!exprStmt)
    {
        throw std::runtime_error("Invalid expression statement node");
    }
    generateExpression(exprStmt->expression.get());
}

// Assignment statement IR generator function
void IRGenerator::generateAssignmentStatement(Node *node)
{
    auto *assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;

    llvm::Value *targetPtr = nullptr;
    llvm::Value *initValue = generateExpression(assignStmt->value.get());
    if (!initValue)
        throw std::runtime_error("Failed to generate IR for assignment value");

    // Check if it's a SelfExpression (self.field)
    if (auto *selfExpr = dynamic_cast<SelfExpression *>(assignStmt->identifier.get()))
    {
        // Generate pointer to the field in the struct
        targetPtr = generateSelfExpression(selfExpr);
        if (!targetPtr)
            throw std::runtime_error("Failed to get pointer for self field");
    }
    else
    {
        // Regular variable assignment
        const std::string &varName = assignStmt->identifier->expression.TokenLiteral;
        auto it = semantics.resolveSymbolInfo(varName);
        if (!it)
            throw std::runtime_error("Could not find variable '" + varName + "'");
        if (it->hasError)
            return;

        targetPtr = it->llvmValue;
        if (!targetPtr)
            throw std::runtime_error("No memory allocated for variable '" + varName + "'");
    }

    // Store the value
    builder.CreateStore(initValue, targetPtr);
}

void IRGenerator::generateFieldAssignmentStatement(Node *node)
{
    auto *fieldStmt = dynamic_cast<FieldAssignment *>(node);
    if (!fieldStmt)
        return;

    // Generate RHS value
    llvm::Value *initValue = generateExpression(fieldStmt->value.get());
    if (!initValue)
        throw std::runtime_error("Failed to generate IR for field assignment value");

    // Parse "Parent.field"
    const std::string &name = fieldStmt->assignment_token.TokenLiteral;
    auto [parentName, childName] = semantics.splitScopedName(name);

    // Lookup parent type
    auto parentIt = semantics.customTypesTable.find(parentName);
    if (parentIt == semantics.customTypesTable.end())
        throw std::runtime_error("Type '" + parentName + "' does not exist");

    auto &members = parentIt->second.members;
    auto childIt = members.find(childName);
    if (childIt == members.end())
        throw std::runtime_error("'" + childName + "' is not a member of '" + parentName + "'");

    // The struct type (already generated in LLVM from earlier)
    llvm::StructType *structTy = llvmCustomTypes[parentName];
    if (!structTy)
        throw std::runtime_error("LLVM StructType for '" + parentName + "' not found");

    // The instance variable of the parent (e.g. "Player p")
    // You need a pointer to the instance. For now, assume semantic analysis put it in symbol table.
    auto parentVarInfo = semantics.resolveSymbolInfo(parentName);
    if (!parentVarInfo || !parentVarInfo->llvmValue)
        throw std::runtime_error("No instance found for '" + parentName + "'");

    llvm::Value *parentPtr = parentVarInfo->llvmValue;

    // Get index of child field
    unsigned fieldIndex = childIt->second.memberIndex;

    // Compute pointer to field: gep parentPtr, 0, fieldIndex
    llvm::Value *fieldPtr = builder.CreateStructGEP(structTy, parentPtr, fieldIndex, childName);

    // Store RHS into field
    builder.CreateStore(initValue, fieldPtr);
}

void IRGenerator::generateBlockStatement(Node *node)
{
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
    {
        throw std::runtime_error("Invalid block statement");
    }

    std::cerr << "[IR DEBUG] Generating block statement with " << blockStmt->statements.size() << " statements\n";
    for (const auto &stmt : blockStmt->statements)
    {
        std::cerr << "[IR DEBUG] Processing block statement child of type: " << typeid(*stmt).name() << " - " << stmt->toString() << "\n";
        generateStatement(stmt.get());
        if (builder.GetInsertBlock()->getTerminator())
        {
            std::cerr << "[IR DEBUG] Terminator found in block statement child: " << builder.GetInsertBlock()->getTerminator()->getOpcodeName() << "\n";
            break;
        }
    }
}

void IRGenerator::generateFunctionStatement(Node *node)
{
    auto fnStmt = dynamic_cast<FunctionStatement *>(node);
    if (!fnStmt)
        return;

    llvm::BasicBlock *oldInsertPoint = builder.GetInsertBlock();

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
        builder.SetInsertPoint(oldInsertPoint);
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
        throw std::runtime_error("Missing declaration meta data");
    }
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

void IRGenerator::generateReturnStatement(Node *node)
{
    auto retStmt = dynamic_cast<ReturnStatement *>(node);
    if (!retStmt)
        return;

    llvm::Value *retVal = nullptr;

    // Generating IR for the return value if it exists
    if (retStmt->return_value)
    {
        retVal = generateExpression(retStmt->return_value.get());
    }

    llvm::Function *currentFunction = builder.GetInsertBlock()->getParent();
    if (retVal)
    {
        // Ensure the return type matches
        if (retVal->getType() != currentFunction->getReturnType())
        {
            llvm::errs() << "Return type mismatch\n";
        }
        builder.CreateRet(retVal);
    }
    else
    {
        // For void functions
        if (currentFunction->getReturnType()->isVoidTy())
            builder.CreateRetVoid();
        else
            llvm::errs() << "Return statement missing value for non-void function\n";
    }
}

// EXPRESSION GENERATOR
//  Expression generator functions
llvm::Value *IRGenerator::generateInfixExpression(Node *node)
{
    auto infix = dynamic_cast<InfixExpression *>(node);
    if (!infix)
        throw std::runtime_error("Invalid infix expression");

    llvm::Value *left = generateExpression(infix->left_operand.get());
    llvm::Value *right = generateExpression(infix->right_operand.get());
    if (!left || !right)
        throw std::runtime_error("Failed to generate IR for infix expression");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Meta data missing for infix node");

    ResolvedType resultType = it->second->type;
    DataType leftType = semantics.metaData[infix->left_operand.get()]->type.kind;
    DataType rightType = semantics.metaData[infix->right_operand.get()]->type.kind;

    // Helper lambda for integer type promotion
    auto promoteInt = [&](llvm::Value *val, DataType fromType, DataType toType) -> llvm::Value *
    {
        unsigned fromBits = getIntegerBitWidth(fromType);
        unsigned toBits = getIntegerBitWidth(toType);
        if (fromBits == 0 || toBits == 0)
            return val; // Not integer type
        if (fromBits == toBits)
            return val; // Same bit width, no promotion needed

        bool fromSigned = isSignedInteger(fromType);
        if (toBits > fromBits)
        {
            if (fromSigned)
                return builder.CreateSExt(val, llvm::IntegerType::get(context, toBits), "sexttmp");
            else
                return builder.CreateZExt(val, llvm::IntegerType::get(context, toBits), "zexttmp");
        }
        else
        {
            // Truncation if needed (rare, probably invalid here)
            return builder.CreateTrunc(val, llvm::IntegerType::get(context, toBits), "trunctmp");
        }
    };

    // Promote operands to widest integer type among left, right, and result
    if (isIntegerType(resultType.kind))
    {
        unsigned targetBits = getIntegerBitWidth(resultType.kind);
        left = promoteInt(left, leftType, resultType.kind);
        right = promoteInt(right, rightType, resultType.kind);
    }

    // Handle BOOLEAN logical operators (AND, OR)
    if (resultType.kind == DataType::BOOLEAN)
    {
        if (infix->operat.type == TokenType::AND || infix->operat.type == TokenType::OR)
        {
            if (left->getType() != builder.getInt1Ty())
                left = builder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0), "boolcastl");
            if (right->getType() != builder.getInt1Ty())
                right = builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0), "boolcastr");

            if (infix->operat.type == TokenType::AND)
                return builder.CreateAnd(left, right, "andtmp");
            else
                return builder.CreateOr(left, right, "ortmp");
        }

        // Comparisons for boolean (should be rare, but...)
        switch (infix->operat.type)
        {
        case TokenType::EQUALS:
            return builder.CreateICmpEQ(left, right, "cmptmp");
        case TokenType::NOT_EQUALS:
            return builder.CreateICmpNE(left, right, "cmptmp");
        default:
            throw std::runtime_error("Unsupported boolean infix operator: " + infix->operat.TokenLiteral);
        }
    }

    // Handle floating point conversions
    if (resultType.kind == DataType::FLOAT)
    {
        if (isIntegerType(leftType))
            left = builder.CreateSIToFP(left, llvm::Type::getFloatTy(context), "inttofloat");
        if (isIntegerType(rightType))
            right = builder.CreateSIToFP(right, llvm::Type::getFloatTy(context), "inttofloat");
    }
    else if (resultType.kind == DataType::DOUBLE)
    {
        if (isIntegerType(leftType))
            left = builder.CreateSIToFP(left, llvm::Type::getDoubleTy(context), "inttodouble");
        if (isIntegerType(rightType))
            right = builder.CreateSIToFP(right, llvm::Type::getDoubleTy(context), "inttodouble");
    }

    // Now generate code based on operator and result type
    // Comparison operators - different for signed/unsigned integers
    auto isCmp = [&](TokenType t)
    {
        return t == TokenType::EQUALS || t == TokenType::NOT_EQUALS ||
               t == TokenType::LESS_THAN || t == TokenType::LT_OR_EQ ||
               t == TokenType::GREATER_THAN || t == TokenType::GT_OR_EQ;
    };

    if (isCmp(infix->operat.type))
    {
        if (isIntegerType(resultType.kind))
        {
            bool signedInt = isSignedInteger(resultType.kind);
            switch (infix->operat.type)
            {
            case TokenType::EQUALS:
                return builder.CreateICmpEQ(left, right, "cmptmp");
            case TokenType::NOT_EQUALS:
                return builder.CreateICmpNE(left, right, "cmptmp");
            case TokenType::LESS_THAN:
                return signedInt ? builder.CreateICmpSLT(left, right, "cmptmp")
                                 : builder.CreateICmpULT(left, right, "cmptmp");
            case TokenType::LT_OR_EQ:
                return signedInt ? builder.CreateICmpSLE(left, right, "cmptmp")
                                 : builder.CreateICmpULE(left, right, "cmptmp");
            case TokenType::GREATER_THAN:
                return signedInt ? builder.CreateICmpSGT(left, right, "cmptmp")
                                 : builder.CreateICmpUGT(left, right, "cmptmp");
            case TokenType::GT_OR_EQ:
                return signedInt ? builder.CreateICmpSGE(left, right, "cmptmp")
                                 : builder.CreateICmpUGE(left, right, "cmptmp");
            default:
                throw std::runtime_error("Unsupported comparison operator");
            }
        }
        else if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
        {
            switch (infix->operat.type)
            {
            case TokenType::EQUALS:
                return builder.CreateFCmpOEQ(left, right, "fcmptmp");
            case TokenType::NOT_EQUALS:
                return builder.CreateFCmpONE(left, right, "fcmptmp");
            case TokenType::LESS_THAN:
                return builder.CreateFCmpOLT(left, right, "fcmptmp");
            case TokenType::LT_OR_EQ:
                return builder.CreateFCmpOLE(left, right, "fcmptmp");
            case TokenType::GREATER_THAN:
                return builder.CreateFCmpOGT(left, right, "fcmptmp");
            case TokenType::GT_OR_EQ:
                return builder.CreateFCmpOGE(left, right, "fcmptmp");
            default:
                throw std::runtime_error("Unsupported comparison operator");
            }
        }
        else
        {
            throw std::runtime_error("Comparison not supported for type '" + resultType.resolvedName + "'");
        }
    }

    // Arithmetic operators
    switch (infix->operat.type)
    {
    case TokenType::PLUS:
        if (isIntegerType(resultType.kind))
            return builder.CreateAdd(left, right, "addtmp");
        else
            return builder.CreateFAdd(left, right, "faddtmp");

    case TokenType::MINUS:
        if (isIntegerType(resultType.kind))
            return builder.CreateSub(left, right, "subtmp");
        else
            return builder.CreateFSub(left, right, "fsubtmp");

    case TokenType::ASTERISK:
        if (isIntegerType(resultType.kind))
            return builder.CreateMul(left, right, "multmp");
        else
            return builder.CreateFMul(left, right, "fmultmp");

    case TokenType::DIVIDE:
        if (isIntegerType(resultType.kind))
            return isSignedInteger(resultType.kind) ? builder.CreateSDiv(left, right, "divtmp")
                                                    : builder.CreateUDiv(left, right, "divtmp");
        else
            return builder.CreateFDiv(left, right, "fdivtmp");

    case TokenType::MODULUS:
        if (isIntegerType(resultType.kind))
            return isSignedInteger(resultType.kind) ? builder.CreateSRem(left, right, "modtmp")
                                                    : builder.CreateURem(left, right, "modtmp");
        else
            throw std::runtime_error("Modulus not supported for FLOAT or DOUBLE at line " +
                                     std::to_string(infix->operat.line));

    default:
        throw std::runtime_error("Unsupported infix operator: " + infix->operat.TokenLiteral +
                                 " at line " + std::to_string(infix->operat.line));
    }
}

// Prefix expression generator function
llvm::Value *IRGenerator::generatePrefixExpression(Node *node)
{
    auto prefix = dynamic_cast<PrefixExpression *>(node);
    if (!prefix)
        throw std::runtime_error("Invalid prefix expression");

    llvm::Value *operand = generateExpression(prefix->operand.get());
    if (!operand)
        throw std::runtime_error("Failed to generate IR for prefix operand");

    const std::string &name = prefix->operand->expression.TokenLiteral;
    auto prefixIt = semantics.metaData.find(prefix);
    if (prefixIt == semantics.metaData.end())
    {
        throw std::runtime_error("Unknown variable '" + name + "' in prefix");
    }

    ResolvedType resultType = prefixIt->second->type;

    // Helper to check if integer type
    auto isIntType = [&](DataType dt)
    {
        return isIntegerType(dt);
    };

    // Helper to get LLVM type from DataType
    auto getLLVMType = [&](DataType dt) -> llvm::Type *
    {
        if (dt == DataType::FLOAT)
            return llvm::Type::getFloatTy(context);
        if (dt == DataType::DOUBLE)
            return llvm::Type::getDoubleTy(context);
        if (isIntType(dt))
        {
            unsigned bits = getIntegerBitWidth(dt);
            return llvm::Type::getIntNTy(context, bits);
        }
        return nullptr;
    };

    switch (prefix->operat.type)
    {
    case TokenType::MINUS:
        if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
            return builder.CreateFNeg(operand, llvm::Twine("fnegtmp"));
        else if (isIntType(resultType.kind))
            return builder.CreateNeg(operand, llvm::Twine("negtmp"));
        else
            throw std::runtime_error("Unsupported type for unary minus");

    case TokenType::BANG:
        // Boolean NOT
        return builder.CreateNot(operand, llvm::Twine("nottmp"));

    case TokenType::PLUS_PLUS:
    case TokenType::MINUS_MINUS:
    {
        auto ident = dynamic_cast<Identifier *>(prefix->operand.get());
        if (!ident)
            throw std::runtime_error("Prefix ++/-- must be used on a variable");

        const std::string &name = ident->expression.TokenLiteral;
        auto identIt = semantics.metaData.find(ident);
        if (identIt == semantics.metaData.end())
        {
            throw std::runtime_error("Udefined variable '" + name + "'");
        }
        llvm::Value *varPtr = identIt->second->llvmValue;
        if (!varPtr)
            throw std::runtime_error("Null variable pointer for: " + ident->identifier.TokenLiteral);

        // Get the type from resultType instead of getPointerElementType
        llvm::Type *varType = getLLVMType(resultType.kind);
        if (!varType)
            throw std::runtime_error("Invalid type for variable: " + ident->identifier.TokenLiteral);

        llvm::Value *loaded = builder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

        llvm::Value *delta = nullptr;
        if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
        {
            delta = llvm::ConstantFP::get(varType, 1.0);
        }
        else if (isIntType(resultType.kind))
        {
            unsigned bits = getIntegerBitWidth(resultType.kind);
            delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
        }
        else
        {
            throw std::runtime_error("Unsupported type for ++/--");
        }

        llvm::Value *updated = nullptr;
        if (prefix->operat.type == TokenType::PLUS_PLUS)
            updated = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                          ? builder.CreateFAdd(loaded, delta, llvm::Twine("preincfptmp"))
                          : builder.CreateAdd(loaded, delta, llvm::Twine("preinctmp"));
        else
            updated = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                          ? builder.CreateFSub(loaded, delta, llvm::Twine("predecfptmp"))
                          : builder.CreateSub(loaded, delta, llvm::Twine("predectmp"));

        builder.CreateStore(updated, varPtr);
        return updated;
    }

    default:
        throw std::runtime_error("Unsupported prefix operator: " + prefix->operat.TokenLiteral +
                                 " at line " + std::to_string(prefix->operat.line));
    }
}

llvm::Value *IRGenerator::generatePostfixExpression(Node *node)
{
    auto postfix = dynamic_cast<PostfixExpression *>(node);
    if (!postfix)
        throw std::runtime_error("Invalid postfix expression");

    auto identifier = dynamic_cast<Identifier *>(postfix->operand.get());
    if (!identifier)
        throw std::runtime_error("Postfix operand must be a variable");
    auto identName = identifier->identifier.TokenLiteral;
    auto identIt = semantics.metaData.find(identifier);
    llvm::Value *varPtr = identIt->second->llvmValue;
    if (!varPtr)
        throw std::runtime_error("Null variable pointer for: " + identifier->identifier.TokenLiteral);

    auto postfixIt = semantics.metaData.find(postfix);
    if (postfixIt == semantics.metaData.end())
    {
        throw std::runtime_error("Variable '" + identName + "' does not exist");
    }

    ResolvedType resultType = postfixIt->second->type;

    // Helper to check if integer type
    auto isIntType = [&](DataType dt)
    { return isIntegerType(dt); };

    // Helper to get LLVM type from DataType
    auto getLLVMType = [&](DataType dt) -> llvm::Type *
    {
        if (dt == DataType::FLOAT)
            return llvm::Type::getFloatTy(context);
        if (dt == DataType::DOUBLE)
            return llvm::Type::getDoubleTy(context);
        if (isIntType(dt))
        {
            unsigned bits = getIntegerBitWidth(dt);
            return llvm::Type::getIntNTy(context, bits);
        }
        return nullptr;
    };

    // Get the type from resultType for CreateLoad
    llvm::Type *varType = getLLVMType(resultType.kind);
    if (!varType)
        throw std::runtime_error("Invalid type for variable: " + identifier->identifier.TokenLiteral);

    llvm::Value *originalValue = builder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

    llvm::Value *delta = nullptr;
    if (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
    {
        delta = llvm::ConstantFP::get(varType, 1.0);
    }
    else if (isIntType(resultType.kind))
    {
        unsigned bits = getIntegerBitWidth(resultType.kind);
        delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
    }
    else
    {
        throw std::runtime_error("Unsupported type for ++/--");
    }

    llvm::Value *updatedValue = nullptr;

    if (postfix->operator_token.type == TokenType::PLUS_PLUS)
        updatedValue = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                           ? builder.CreateFAdd(originalValue, delta, llvm::Twine("finc"))
                           : builder.CreateAdd(originalValue, delta, llvm::Twine("inc"));
    else if (postfix->operator_token.type == TokenType::MINUS_MINUS)
        updatedValue = (resultType.kind == DataType::FLOAT || resultType.kind == DataType::DOUBLE)
                           ? builder.CreateFSub(originalValue, delta, llvm::Twine("fdec"))
                           : builder.CreateSub(originalValue, delta, llvm::Twine("dec"));
    else
        throw std::runtime_error("Unsupported postfix operator: " + postfix->operator_token.TokenLiteral +
                                 " at line " + std::to_string(postfix->operator_token.line));

    builder.CreateStore(updatedValue, varPtr);

    // Return original value since postfix
    return originalValue;
}

llvm::Value *IRGenerator::generateStringLiteral(Node *node)
{
    std::cout << "INSIDE GENERATE IR FOR STRING\n";
    auto strLit = dynamic_cast<StringLiteral *>(node);
    if (!strLit)
    {
        throw std::runtime_error("Invalid string literal");
    }
    auto it = semantics.metaData.find(strLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("String literal not found in metadata");
    }
    DataType dt = it->second->type.kind;

    if (dt != DataType::STRING && dt != DataType::NULLABLE_STR)
    {
        throw std::runtime_error("Type error: Expected STRING or NULLABLE_STR for StringLiteral ");
    }
    std::string raw = strLit->string_token.TokenLiteral;
    llvm::Value *strConst = builder.CreateGlobalStringPtr(raw);
    return strConst;
}

llvm::Value *IRGenerator::generateCharLiteral(Node *node)
{
    auto charLit = dynamic_cast<CharLiteral *>(node);
    if (!charLit)
    {
        throw std::runtime_error("Invalid char literal");
    }
    auto it = semantics.metaData.find(charLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Char literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::CHAR && dt != DataType::NULLABLE_CHAR)
    {
        throw std::runtime_error("Type error: Expected CHAR for CharLiteral");
    }
    std::string tokenLiteral = charLit->char_token.TokenLiteral;

    char c = decodeCharLiteral(tokenLiteral);
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(c), false);
}

llvm::Value *IRGenerator::generateChar16Literal(Node *node)
{
    auto char16Lit = dynamic_cast<Char16Literal *>(node);
    if (!char16Lit)
    {
        throw std::runtime_error("Invalid char 16 literal");
    }
    auto it = semantics.metaData.find(char16Lit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Char16 literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::CHAR16 && dt != DataType::NULLABLE_CHAR16)
    {
        throw std::runtime_error("Type error: Expected CHAR16 for Char16Literal");
    }
    std::string tokenLiteral = char16Lit->char16_token.TokenLiteral;
    uint16_t c = decodeCharLiteral(tokenLiteral);
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), c, false);
}

llvm::Value *IRGenerator::generateChar32Literal(Node *node)
{
    auto char32Lit = dynamic_cast<Char32Literal *>(node);
    if (!char32Lit)
    {
        throw std::runtime_error("Invalid char 32 literal");
    }
    auto it = semantics.metaData.find(char32Lit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Char16 literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::CHAR16 && dt != DataType::NULLABLE_CHAR16)
    {
        throw std::runtime_error("Type error: Expected CHAR32 for Char16Literal");
    }
    std::string tokenLiteral = char32Lit->char32_token.TokenLiteral;
    uint32_t c = decodeChar32Literal(tokenLiteral);
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), c, false);
}

llvm::Value *IRGenerator::generateBooleanLiteral(Node *node)
{
    auto boolLit = dynamic_cast<BooleanLiteral *>(node);
    if (!boolLit)
    {
        throw std::runtime_error("Invalid boolean type");
    }
    auto it = semantics.metaData.find(boolLit);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Boolean literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::BOOLEAN && dt != DataType::NULLABLE_BOOLEAN)
    {
        throw std::runtime_error("Type error: Expected BOOLEAN for BooleanLiteral");
    }

    bool value = (boolLit->boolean_token.TokenLiteral == "true");

    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), value);
}

llvm::Value *IRGenerator::generateShortLiteral(Node *node)
{
    auto shortLit = dynamic_cast<ShortLiteral *>(node);
    if (!shortLit)
        throw std::runtime_error("Invalid short literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Short literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::SHORT_INT && dt != DataType::NULLABLE_SHORT_INT)
        throw std::runtime_error("Type error: Expected SHORT_INT or NULLABLE_SHORT_INT");

    int16_t value = static_cast<int16_t>(std::stoi(shortLit->short_token.TokenLiteral));
    return llvm::ConstantInt::get(context, llvm::APInt(16, value, true));
}

llvm::Value *IRGenerator::generateUnsignedShortLiteral(Node *node)
{
    auto ushortLit = dynamic_cast<UnsignedShortLiteral *>(node);
    if (!ushortLit)
        throw std::runtime_error("Invalid ushort literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("UShort literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::USHORT_INT && dt != DataType::NULLABLE_USHORT_INT)
        throw std::runtime_error("Type error: Expected USHORT_INT or NULLABLE_USHORT_INT");

    uint16_t value = static_cast<uint16_t>(std::stoul(ushortLit->ushort_token.TokenLiteral));
    return llvm::ConstantInt::get(context, llvm::APInt(16, value, false));
}

llvm::Value *IRGenerator::generateIntegerLiteral(Node *node)
{
    auto intLit = dynamic_cast<IntegerLiteral *>(node);
    if (!intLit)
    {
        throw std::runtime_error("Invalid integer literal");
    }
    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Integer literal not found in metadata at line:" + std::to_string(intLit->expression.line) + " and column: " + std::to_string(intLit->expression.column));
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::INTEGER && dt != DataType::NULLABLE_INT)
    {
        throw std::runtime_error("Type error: Expected INTEGER or NULLABLE_INT for IntegerLiteral");
    }
    int64_t value = std::stoll(intLit->int_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(32, value, true));
}

llvm::Value *IRGenerator::generateUnsignedIntegerLiteral(Node *node)
{
    auto uintLit = dynamic_cast<UnsignedIntegerLiteral *>(node);
    if (!uintLit)
        throw std::runtime_error("Invalid uint literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Uint literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::UINTEGER && dt != DataType::NULLABLE_UINT)
        throw std::runtime_error("Type error: Expected UINTEGER or NULLABLE_UINT");

    uint32_t value = static_cast<uint32_t>(std::stoul(uintLit->uint_token.TokenLiteral));
    return llvm::ConstantInt::get(context, llvm::APInt(32, value, false));
}

llvm::Value *IRGenerator::generateLongLiteral(Node *node)
{
    auto longLit = dynamic_cast<LongLiteral *>(node);
    if (!longLit)
        throw std::runtime_error("Invalid long literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Long literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::LONG_INT && dt != DataType::NULLABLE_LONG_INT)
        throw std::runtime_error("Type error: Expected LONG_INT or NULLABLE_LONG_INT");

    int64_t value = std::stoll(longLit->long_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(64, value, true));
}

llvm::Value *IRGenerator::generateUnsignedLongLiteral(Node *node)
{
    auto ulongLit = dynamic_cast<UnsignedLongLiteral *>(node);
    if (!ulongLit)
        throw std::runtime_error("Invalid ulong literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("ULong literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::ULONG_INT && dt != DataType::NULLABLE_ULONG_INT)
        throw std::runtime_error("Type error: Expected ULONG_INT or NULLABLE_ULONG_INT");

    uint64_t value = std::stoull(ulongLit->ulong_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(64, value, false));
}

llvm::Value *IRGenerator::generateExtraLiteral(Node *node)
{
    auto extraLit = dynamic_cast<ExtraLiteral *>(node);
    if (!extraLit)
        throw std::runtime_error("Invalid extra (128-bit) literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("Extra literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::EXTRA_INT && dt != DataType::NULLABLE_EXTRA_INT)
        throw std::runtime_error("Type error: Expected EXTRA_INT or NULLABLE_EXTRA_INT");

    // Use APInt constructor with string and base 10 for 128-bit
    llvm::APInt value(128, extraLit->extra_token.TokenLiteral, 10);
    return llvm::ConstantInt::get(context, value);
}

llvm::Value *IRGenerator::generateUnsignedExtraLiteral(Node *node)
{
    auto uextraLit = dynamic_cast<UnsignedExtraLiteral *>(node);
    if (!uextraLit)
        throw std::runtime_error("Invalid uextra (128-bit unsigned) literal");

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
        throw std::runtime_error("UExtra literal not found in metadata");

    DataType dt = it->second->type.kind;
    if (dt != DataType::UEXTRA_INT && dt != DataType::NULLABLE_UEXTRA_INT)
        throw std::runtime_error("Type error: Expected UEXTRA_INT or NULLABLE_UEXTRA_INT");

    llvm::APInt value(128, uextraLit->uextra_token.TokenLiteral, 10);
    return llvm::ConstantInt::get(context, value);
}

llvm::Value *IRGenerator::generateFloatLiteral(Node *node)
{
    auto fltLit = dynamic_cast<FloatLiteral *>(node);
    if (!fltLit)
    {
        throw std::runtime_error("Invalid float literal");
    }
    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Float literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::FLOAT && dt != DataType::NULLABLE_FLT)
    {
        throw std::runtime_error("Type error: Expected Float or NULLABLE_FLT for FloatLiteral ");
    }
    float value = std::stof(fltLit->float_token.TokenLiteral);
    return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
}

llvm::Value *IRGenerator::generateDoubleLiteral(Node *node)
{
    auto dbLit = dynamic_cast<DoubleLiteral *>(node);
    if (!dbLit)
    {
        throw std::runtime_error("Invalid double literal");
    }
    auto it = semantics.metaData.find(node); // Creating an iterator to find specific meta data about the double literal
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Double literal not found in metadata");
    }
    DataType dt = it->second->type.kind;
    if (dt != DataType::DOUBLE && dt != DataType::NULLABLE_DOUBLE)
    {
        throw std::runtime_error("Type error: Expected DOUBLE for DoubleLiteral");
    }
    // Checking if we have metaData about the double literal and if so we check to see if the data type is double
    double value = std::stod(dbLit->double_token.TokenLiteral);            // Converting the double literal from a string to a double
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), value); // Returning double value
}

llvm::Value *IRGenerator::generateNullLiteral(NullLiteral *nullLit, DataType type)
{
    switch (type)
    {
    case DataType::NULLABLE_STR:
        return llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0));

    case DataType::NULLABLE_INT:
        // Using minimum signed int as null marker
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, INT32_MIN, true));

    case DataType::NULLABLE_SHORT_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, INT16_MIN, true));

    case DataType::NULLABLE_USHORT_INT:
        // Using zero as null for unsigned
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, 0));

    case DataType::NULLABLE_UINT:
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));

    case DataType::NULLABLE_LONG_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, INT64_MIN, true));

    case DataType::NULLABLE_ULONG_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0));

    case DataType::NULLABLE_EXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
        return llvm::ConstantInt::get(llvm::Type::getInt128Ty(context), llvm::APInt(128, 0));

    case DataType::NULLABLE_FLT:
        return llvm::ConstantFP::getNaN(llvm::Type::getFloatTy(context));

    case DataType::NULLABLE_DOUBLE:
        return llvm::ConstantFP::getNaN(llvm::Type::getDoubleTy(context));

    case DataType::NULLABLE_BOOLEAN:
        return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), llvm::APInt(1, 0));

    case DataType::NULLABLE_CHAR:
        return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0));

    case DataType::NULLABLE_CHAR16:
        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), llvm::APInt(16, 0));

    case DataType::NULLABLE_CHAR32:
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));

    default:
        throw std::runtime_error("Unsupported nullable data type in generateNullLiteral");
    }
}

// Generator function for identifier expression
llvm::Value *IRGenerator::generateIdentifierExpression(Node *node)
{
    auto identExpr = dynamic_cast<Identifier *>(node);
    if (!identExpr)
    {
        throw std::runtime_error("Invalid identifier expression");
    }

    const std::string &identName = identExpr->identifier.TokenLiteral;
    auto identIt = semantics.metaData.find(identExpr);
    if (identIt == semantics.metaData.end())
    {
        throw std::runtime_error("Unidentified identifier '" + identName + "'");
    }
    if (identIt->second->hasError)
    {
        return nullptr;
    }
    llvm::Type *ty = getLLVMType(identIt->second->type);
    // Checking if the identifier has a value
    llvm::Value *variablePtr = identIt->second->llvmValue;

    return builder.CreateLoad(getLLVMType(identIt->second->type), variablePtr, identExpr->identifier.TokenLiteral);
}

llvm::Value *IRGenerator::generateSelfExpression(Node *node)
{
    auto selfExpr = dynamic_cast<SelfExpression *>(node);
    if (!selfExpr)
    {
        throw std::runtime_error("Invalid self expression");
    }

    const std::string &compName = currentComponent->component_name->expression.TokenLiteral;

    auto compIt = componentTypes.find(compName);
    if (compIt == componentTypes.end())
    {
        throw std::runtime_error("Component with name '" + compName + "' not found");
    }

    auto compTy = compIt->second;

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(compTy);

    const std::string &fieldName = selfExpr->field->expression.TokenLiteral;

    auto compMeta = semantics.metaData.find(currentComponent);
    if (compMeta == semantics.metaData.end())
    {
        throw std::runtime_error("Missing component metaData for '" + compName + "'");
    }

    auto memIt = compMeta->second->members.find(fieldName);
    if (memIt == compMeta->second->members.end())
    {
        throw std::runtime_error("Field '" + fieldName + "' not found in component");
    }

    unsigned index = 0;
    for (const auto &[name, info] : compMeta->second->members)
    {
        if (name == fieldName)
            break;
        ++index;
    }

    llvm::Value *componentInstance = compMeta->second->llvmValue;

    llvm::Value *fieldPtr = builder.CreateStructGEP(
        structTy, componentInstance, index, fieldName + "_ptr");

    return fieldPtr;
}

llvm::Value *IRGenerator::generateBlockExpression(Node *node)
{
    auto blockExpr = dynamic_cast<BlockExpression *>(node);
    if (!blockExpr)
        throw std::runtime_error("Invalid block expression");

    for (const auto &stmts : blockExpr->statements)
    {
        // Check if the current block is already terminated by a return or branch
        if (builder.GetInsertBlock()->getTerminator())
        {
            std::cout << "SKIPPING statement - block terminated\n";
            break;
        }
        generateStatement(stmts.get());
    }

    // A block expression should not return an llvm::Value directly.
    return nullptr;
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
        return nullptr; // If it has an error we just stop IR generation
    }

    // Getting the function signature
    auto fnName = fnExpr->func_key.TokenLiteral;

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

    // Creating the entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
    builder.SetInsertPoint(entry);

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
        llvm::AllocaInst *alloca = builder.CreateAlloca(getLLVMType(pIt->second->type), nullptr, p->statement.TokenLiteral);
        builder.CreateStore(&(*argIter), alloca);
        pIt->second->llvmValue = alloca;

        argIter++;
    }

    // This will handle whatever is inside the block including the return value
    generateExpression(fnExpr->block.get());
    return fn;
}

llvm::Value *IRGenerator::generateCallExpression(Node *node)
{
    auto callExpr = dynamic_cast<CallExpression *>(node);
    if (!callExpr)
    {
        throw std::runtime_error("Invalid call expression");
    }

    auto callIt = semantics.metaData.find(callExpr);
    if (callIt == semantics.metaData.end())
    {
        throw std::runtime_error("Call expression does not exist");
    }
    if (callIt->second->hasError)
    {
        return nullptr; // If it has an error we just stop IR generation
    }

    // Getting the function name
    const std::string &fnName = callExpr->function_identifier->expression.TokenLiteral;
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
    llvm::Value *call = builder.CreateCall(calledFunc, argsV, "calltmp");

    // Check if the function return type is void
    if (calledFunc->getReturnType()->isVoidTy())
    {
        return nullptr;
    }
    return call;
}

// HELPER FUNCTIONS
llvm::Type *IRGenerator::getLLVMType(ResolvedType type)
{
    switch (type.kind)
    {
    case DataType::SHORT_INT:
    case DataType::NULLABLE_SHORT_INT:
        return llvm::Type::getInt16Ty(context);

    case DataType::USHORT_INT:
    case DataType::NULLABLE_USHORT_INT:
        return llvm::Type::getInt16Ty(context);

    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
        return llvm::Type::getInt32Ty(context);

    case DataType::UINTEGER:
    case DataType::NULLABLE_UINT:
        return llvm::Type::getInt32Ty(context);

    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
        return llvm::Type::getInt64Ty(context);

    case DataType::ULONG_INT:
    case DataType::NULLABLE_ULONG_INT:
        return llvm::Type::getInt64Ty(context);

    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
        return llvm::Type::getInt128Ty(context);

    case DataType::UEXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
        return llvm::Type::getInt128Ty(context);

    case DataType::BOOLEAN:
    case DataType::NULLABLE_BOOLEAN:
        return llvm::Type::getInt1Ty(context);

    case DataType::CHAR:
    case DataType::NULLABLE_CHAR:
        return llvm::Type::getInt8Ty(context);

    case DataType::CHAR16:
    case DataType::NULLABLE_CHAR16:
        return llvm::Type::getInt16Ty(context);

    case DataType::CHAR32:
    case DataType::NULLABLE_CHAR32:
        return llvm::Type::getInt32Ty(context);

    case DataType::FLOAT:
    case DataType::NULLABLE_FLT:
        return llvm::Type::getFloatTy(context);

    case DataType::DOUBLE:
    case DataType::NULLABLE_DOUBLE:
        return llvm::Type::getDoubleTy(context);

    case DataType::STRING:
    case DataType::NULLABLE_STR:
        return llvm::PointerType::get(context, 0);

    case DataType::VOID:
        return llvm::Type::getVoidTy(context);

    case DataType::DATABLOCK:
    case DataType::COMPONENT:
    {
        if (type.resolvedName.empty())
            throw std::runtime_error("Custom type requested but resolvedName is empty");

        auto it = llvmCustomTypes.find(type.resolvedName);
        if (it != llvmCustomTypes.end())
            return it->second;

        // Error if the type hasn’t been registered yet
        throw std::runtime_error("LLVM IR requested for unknown custom type '" + type.resolvedName + "'");
    }

    case DataType::ERROR:
    case DataType::GENERIC:
    case DataType::UNKNOWN:
        throw std::runtime_error("Unsupported or unknown data type encountered in getLLVMType");

    default:
        return nullptr;
    }
}

// Registering generator functions for statements
void IRGenerator::registerGeneratorFunctions()
{
    generatorFunctionsMap[typeid(LetStatement)] = &IRGenerator::generateLetStatement;
    generatorFunctionsMap[typeid(ExpressionStatement)] = &IRGenerator::generateExpressionStatement;
    generatorFunctionsMap[typeid(AssignmentStatement)] = &IRGenerator::generateAssignmentStatement;
    generatorFunctionsMap[typeid(FieldAssignment)] = &IRGenerator::generateFieldAssignmentStatement;
    generatorFunctionsMap[typeid(WhileStatement)] = &IRGenerator::generateWhileStatement;
    generatorFunctionsMap[typeid(ForStatement)] = &IRGenerator::generateForStatement;
    generatorFunctionsMap[typeid(ifStatement)] = &IRGenerator::generateIfStatement;
    generatorFunctionsMap[typeid(BreakStatement)] = &IRGenerator::generateBreakStatement;
    generatorFunctionsMap[typeid(ContinueStatement)] = &IRGenerator::generateContinueStatement;
    generatorFunctionsMap[typeid(BlockStatement)] = &IRGenerator::generateBlockStatement;
    generatorFunctionsMap[typeid(FunctionStatement)] = &IRGenerator::generateFunctionStatement;
    generatorFunctionsMap[typeid(ReturnStatement)] = &IRGenerator::generateReturnStatement;
    generatorFunctionsMap[typeid(FunctionDeclaration)] = &IRGenerator::generateFunctionDeclaration;
    // Special case
    generatorFunctionsMap[typeid(FunctionDeclarationExpression)] = &IRGenerator::generateFunctionDeclarationExpression;
    // Component system
    generatorFunctionsMap[typeid(DataStatement)] = &IRGenerator::generateDataStatement;
    generatorFunctionsMap[typeid(BehaviorStatement)] = &IRGenerator::generateBehaviorStatement;
    generatorFunctionsMap[typeid(ComponentStatement)] = &IRGenerator::generateComponentStatement;
}

void IRGenerator::registerExpressionGeneratorFunctions()
{
    expressionGeneratorsMap[typeid(InfixExpression)] = &IRGenerator::generateInfixExpression;
    expressionGeneratorsMap[typeid(PrefixExpression)] = &IRGenerator::generatePrefixExpression;
    expressionGeneratorsMap[typeid(PostfixExpression)] = &IRGenerator::generatePostfixExpression;
    expressionGeneratorsMap[typeid(StringLiteral)] = &IRGenerator::generateStringLiteral;
    expressionGeneratorsMap[typeid(CharLiteral)] = &IRGenerator::generateCharLiteral;
    expressionGeneratorsMap[typeid(Char16Literal)] = &IRGenerator::generateChar16Literal;
    expressionGeneratorsMap[typeid(Char32Literal)] = &IRGenerator::generateChar32Literal;
    expressionGeneratorsMap[typeid(BooleanLiteral)] = &IRGenerator::generateBooleanLiteral;
    expressionGeneratorsMap[typeid(ShortLiteral)] = &IRGenerator::generateShortLiteral;
    expressionGeneratorsMap[typeid(UnsignedShortLiteral)] = &IRGenerator::generateUnsignedShortLiteral;
    expressionGeneratorsMap[typeid(IntegerLiteral)] = &IRGenerator::generateIntegerLiteral;
    expressionGeneratorsMap[typeid(UnsignedIntegerLiteral)] = &IRGenerator::generateUnsignedIntegerLiteral;
    expressionGeneratorsMap[typeid(LongLiteral)] = &IRGenerator::generateLongLiteral;
    expressionGeneratorsMap[typeid(UnsignedLongLiteral)] = &IRGenerator::generateUnsignedLongLiteral;
    expressionGeneratorsMap[typeid(ExtraLiteral)] = &IRGenerator::generateExtraLiteral;
    expressionGeneratorsMap[typeid(UnsignedExtraLiteral)] = &IRGenerator::generateUnsignedExtraLiteral;
    expressionGeneratorsMap[typeid(FloatLiteral)] = &IRGenerator::generateFloatLiteral;
    expressionGeneratorsMap[typeid(DoubleLiteral)] = &IRGenerator::generateDoubleLiteral;
    expressionGeneratorsMap[typeid(Identifier)] = &IRGenerator::generateIdentifierExpression;
    expressionGeneratorsMap[typeid(BlockExpression)] = &IRGenerator::generateBlockExpression;
    expressionGeneratorsMap[typeid(CallExpression)] = &IRGenerator::generateCallExpression;
    expressionGeneratorsMap[typeid(SelfExpression)] = &IRGenerator::generateSelfExpression;
}

char IRGenerator::decodeCharLiteral(const std::string &literal)
{
    if (literal.length() == 3 && literal.front() == '\'' && literal.back() == '\'')
    {
        return literal[1];
    }
    else if (literal.length() == 4 && literal.front() == '\'' && literal.back() == '\'' && literal[1] == '\\')
    {
        switch (literal[2])
        {
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
            throw std::runtime_error("Unknown escape sequence in char literal: " + literal);
        }
    }
    throw std::runtime_error("Invalid char literal: " + literal);
}

uint16_t IRGenerator::decodeChar16Literal(const std::string &literal)
{
    // Example formats: 'A', '\n', '\u1234' (unicode escape)

    if (literal.length() == 3 && literal.front() == '\'' && literal.back() == '\'')
    {
        return static_cast<uint16_t>(literal[1]);
    }
    else if (literal.length() == 8 && literal.substr(0, 2) == "'\\u" && literal.back() == '\'')
    {
        std::string hex = literal.substr(3, 4); // 4 hex digits
        return static_cast<uint16_t>(std::stoi(hex, nullptr, 16));
    }
    else if (literal.length() == 4 && literal.front() == '\'' && literal.back() == '\'' && literal[1] == '\\')
    {
        // Simple escapes like '\n'
        switch (literal[2])
        {
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
            throw std::runtime_error("Unknown escape sequence in char16 literal: " + literal);
        }
    }
    throw std::runtime_error("Invalid char16 literal: " + literal);
}

// Decodes UTF-32 char32 literals (returns uint32_t)
uint32_t IRGenerator::decodeChar32Literal(const std::string &literal)
{
    // Example formats: 'A', '\U0001F600' (unicode escape for emoji)

    if (literal.length() == 3 && literal.front() == '\'' && literal.back() == '\'')
    {
        return static_cast<uint32_t>(literal[1]);
    }
    else if (literal.length() == 12 && literal.substr(0, 2) == "'\\U" && literal.back() == '\'')
    {
        std::string hex = literal.substr(3, 8); // 8 hex digits
        return static_cast<uint32_t>(std::stoul(hex, nullptr, 16));
    }
    else if (literal.length() == 4 && literal.front() == '\'' && literal.back() == '\'' && literal[1] == '\\')
    {
        // Simple escapes like '\n'
        switch (literal[2])
        {
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
            throw std::runtime_error("Unknown escape sequence in char32 literal: " + literal);
        }
    }
    throw std::runtime_error("Invalid char32 literal: " + literal);
}

bool IRGenerator::isIntegerType(DataType dt)
{
    switch (dt)
    {
    case DataType::SHORT_INT:
    case DataType::NULLABLE_SHORT_INT:
    case DataType::USHORT_INT:
    case DataType::NULLABLE_USHORT_INT:
    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
    case DataType::UINTEGER:
    case DataType::NULLABLE_UINT:
    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
    case DataType::ULONG_INT:
    case DataType::NULLABLE_ULONG_INT:
    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
    case DataType::UEXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
        return true;
    default:
        return false;
    }
}

bool IRGenerator::isSignedInteger(DataType dt)
{
    switch (dt)
    {
    case DataType::SHORT_INT:
    case DataType::NULLABLE_SHORT_INT:
    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
        return true;
    default:
        return false;
    }
}

unsigned IRGenerator::getIntegerBitWidth(DataType dt)
{
    switch (dt)
    {
    case DataType::SHORT_INT:
    case DataType::NULLABLE_SHORT_INT:
    case DataType::USHORT_INT:
    case DataType::NULLABLE_USHORT_INT:
        return 16;
    case DataType::INTEGER:
    case DataType::NULLABLE_INT:
    case DataType::UINTEGER:
    case DataType::NULLABLE_UINT:
        return 32;
    case DataType::LONG_INT:
    case DataType::NULLABLE_LONG_INT:
    case DataType::ULONG_INT:
    case DataType::NULLABLE_ULONG_INT:
        return 64;
    case DataType::EXTRA_INT:
    case DataType::NULLABLE_EXTRA_INT:
    case DataType::UEXTRA_INT:
    case DataType::NULLABLE_UEXTRA_INT:
        return 128;
    default:
        return 0; // Not an integer type
    }
}

bool IRGenerator::inFunction()
{
    return builder.GetInsertBlock() != nullptr;
}

void IRGenerator::dumpIR()
{
    module->print(llvm::outs(), nullptr);
}

bool IRGenerator::currentBlockIsTerminated()
{
    llvm::BasicBlock *bb = builder.GetInsertBlock();
    return bb && bb->getTerminator();
}

llvm::Module &IRGenerator::getLLVMModule()
{
    return *module;
}
