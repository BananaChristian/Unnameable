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
        std::cout << "Failed to find IR generator for : " << node->toString() << "\n";
        return;
    }
    (this->*generatorIt->second)(node);
}
// STATEMENT GENERATOR FUNCTIONS
// Let statement IR generator function
void IRGenerator::generateLetStatement(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    if (!letStmt)
    {
        throw std::runtime_error("Invalid let Statement node");
    }

    SymbolInfo *symbol = semantics.resolveSymbolInfo(letStmt->ident_token.TokenLiteral);
    if (!symbol)
    {
        throw std::runtime_error("Symbol not found: " + letStmt->ident_token.TokenLiteral);
    }

    llvm::Type *varType = getLLVMType(symbol->symbolDataType);
    if (!varType)
    {
        throw std::runtime_error("Invalid type for variable: " + letStmt->ident_token.TokenLiteral);
    }
    llvm::AllocaInst *alloca = builder.CreateAlloca(varType, nullptr, letStmt->ident_token.TokenLiteral);
    namedValues[letStmt->ident_token.TokenLiteral] = alloca;
    if (letStmt->value)
    {
        llvm::Value *initValue = nullptr;
        if (auto nullLit = dynamic_cast<NullLiteral *>(letStmt->value.get()))
        {
            initValue = generateNullLiteral(nullLit, symbol->symbolDataType);
        }
        else
        {
            initValue = generateExpression(letStmt->value.get());
        }
        if (!initValue)
        {
            throw std::runtime_error("Failed to generate IR for initializer of: " + letStmt->ident_token.TokenLiteral);
        }
        builder.CreateStore(initValue, alloca);
    }
}

// While statement IR generator function
void IRGenerator::generateWhileStatement(Node *node)
{
    llvm::Function *function = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "while.cond", function);
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(context, "while.body");
    llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(context, "while.end");

    builder.CreateBr(condBB);

    builder.SetInsertPoint(condBB);
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    llvm::Value *condVal = generateExpression(whileStmt->condition.get());
    if (!condVal->getType()->isIntegerTy(1))
    {
        std::cerr << "[IR ERROR] While condition must be boolean\n";
    }
    builder.CreateCondBr(condVal, loopBB, afterBB);

    function->getBasicBlockList().push_back(loopBB);
    builder.SetInsertPoint(loopBB);

    generateStatement(whileStmt->loop.get());

    loopBlocksStack.pop_back();
    builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(afterBB);
    builder.SetInsertPoint(afterBB);
}

void IRGenerator::generateBreakStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Break statement not inside a loop");
    }

    llvm::BasicBlock *afterBB = loopBlocksStack.back().afterBB;
    builder.CreateBr(afterBB);

    llvm::BasicBlock *contBB = llvm::BasicBlock::Create(context, "after.break", builder.GetInsertBlock()->getParent());
    builder.SetInsertPoint(contBB);
}

void IRGenerator::generateContinueStatement(Node *node)
{
    if (loopBlocksStack.empty())
    {
        throw std::runtime_error("Continue statement not inside a loop");
    }

    llvm::BasicBlock *condBB = loopBlocksStack.back().condBB;
    builder.CreateBr(condBB);

    llvm::BasicBlock *contBB = llvm::BasicBlock::Create(context, "after.continue", builder.GetInsertBlock()->getParent());
    builder.SetInsertPoint(contBB);
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
    auto assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt)
        return;
    SymbolInfo *symbol = semantics.resolveSymbolInfo(assignStmt->ident_token.TokenLiteral);
    if (!symbol)
    {
        throw std::runtime_error("Symbol '" + assignStmt->ident_token.TokenLiteral + "' not found");
    }

    llvm::Type *varType = getLLVMType(symbol->symbolDataType);
    if (!varType)
    {
        throw std::runtime_error("Invalid type for variable: " + assignStmt->ident_token.TokenLiteral);
    }
    llvm::AllocaInst *alloca = builder.CreateAlloca(varType, nullptr, assignStmt->ident_token.TokenLiteral);
    namedValues[assignStmt->ident_token.TokenLiteral] = alloca;
    if (assignStmt->value)
    {
        llvm::Value *initValue = generateExpression(assignStmt->value.get());
        if (!initValue)
        {
            throw std::runtime_error("Failed to generate IR for assign statement");
        }
        builder.CreateStore(initValue, alloca);
    }
}

// EXPRESSION GENERATOR
//  Expression generator functions
llvm::Value *IRGenerator::generateInfixExpression(Node *node)
{
    auto infix = dynamic_cast<InfixExpression *>(node);
    if (!infix)
    {
        throw std::runtime_error("Invalid infix expression");
    }
    llvm::Value *left = generateExpression(infix->left_operand.get());
    llvm::Value *right = generateExpression(infix->right_operand.get());
    if (!left || !right)
    {
        throw std::runtime_error("Failed to generate IR for infix expression");
    }

    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Meta data missing for infix node");
    }
    DataType resultType = it->second.symbolDataType;
    if (!(resultType == DataType::INTEGER || resultType == DataType::FLOAT || resultType == DataType::DOUBLE || resultType == DataType::BOOLEAN))
    {
        throw std::runtime_error("InfixExpression type must be int, float,or double ");
    }
    DataType leftType = semantics.metaData[infix->left_operand.get()].symbolDataType;
    DataType rightType = semantics.metaData[infix->right_operand.get()].symbolDataType;

    if (resultType == DataType::BOOLEAN)
    {
        if (left->getType() != builder.getInt1Ty())
        {
            left = builder.CreateICmpNE(left, llvm::ConstantInt::get(left->getType(), 0), "boolcastl");
        }
        if (right->getType() != builder.getInt1Ty())
        {
            right = builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0), "boolcastr");
        }

        switch (infix->operat.type)
        {
        case TokenType::EQUALS:
            return leftType == DataType::INTEGER ? builder.CreateICmpEQ(left, right, "cmptmp") : leftType == DataType::FLOAT ? builder.CreateFCmpOEQ(left, right, "fcmptmp")
                                                                                             : leftType == DataType::DOUBLE  ? builder.CreateFCmpOEQ(left, right, "fcmptmp")
                                                                                                                             : throw std::runtime_error("EQUALITY not supported for this type");
        case TokenType::NOT_EQUALS:
            return leftType == DataType::INTEGER ? builder.CreateICmpNE(left, right, "cmptmp") : leftType == DataType::FLOAT ? builder.CreateFCmpONE(left, right, "fcmptmp")
                                                                                             : leftType == DataType::DOUBLE  ? builder.CreateFCmpONE(left, right, "fcmptmp")
                                                                                                                             : throw std::runtime_error("INEQUALITY not supported for this type");
        case TokenType::LESS_THAN:
            return leftType == DataType::INTEGER ? builder.CreateICmpSLT(left, right, "cmptmp") : leftType == DataType::FLOAT ? builder.CreateFCmpOLT(left, right, "fcmptmp")
                                                                                              : leftType == DataType::DOUBLE  ? builder.CreateFCmpOLT(left, right, "fcmptmp")
                                                                                                                              : throw std::runtime_error("LT not supported for this type");
        case TokenType::LT_OR_EQ:
            return leftType == DataType::INTEGER ? builder.CreateICmpSLE(left, right, "cmptmp") : leftType == DataType::FLOAT ? builder.CreateFCmpOLE(left, right, "fcmptmp")
                                                                                              : leftType == DataType::DOUBLE  ? builder.CreateFCmpOLE(left, right, "fcmptmp")
                                                                                                                              : throw std::runtime_error("LE not supported for this type");
        case TokenType::GREATER_THAN:
            return leftType == DataType::INTEGER ? builder.CreateICmpSGT(left, right, "cmptmp") : leftType == DataType::FLOAT ? builder.CreateFCmpOGT(left, right, "fcmptmp")
                                                                                              : leftType == DataType::DOUBLE  ? builder.CreateFCmpOGT(left, right, "fcmptmp")
                                                                                                                              : throw std::runtime_error("GT not supported for this type");
        case TokenType::GT_OR_EQ:
            return leftType == DataType::INTEGER ? builder.CreateICmpSGE(left, right, "cmptmp") : leftType == DataType::FLOAT ? builder.CreateFCmpOGE(left, right, "fcmptmp")
                                                                                              : leftType == DataType::DOUBLE  ? builder.CreateFCmpOGE(left, right, "fcmptmp")
                                                                                                                              : throw std::runtime_error("GE not supported for this type");
        case TokenType::AND:
            return builder.CreateAnd(left, right, "andtmp");
        case TokenType::OR:
            return builder.CreateOr(left, right, "ortmp");
        default:
            throw std::runtime_error("Unsupported infix operator: " + infix->operat.TokenLiteral +
                                     " at line " + std::to_string(infix->operat.line));
        }
    }
    if (resultType == DataType::FLOAT)
    {
        if (leftType == DataType::INTEGER)
        {
            left = builder.CreateSIToFP(left, llvm::Type::getFloatTy(context), "inttofloat");
        }
        if (rightType == DataType::INTEGER)
        {
            right = builder.CreateSIToFP(right, llvm::Type::getFloatTy(context), "inttofloat");
        }
    }
    else if (resultType == DataType::DOUBLE)
    {
        if (leftType == DataType::INTEGER)
        {
            left = builder.CreateSIToFP(left, llvm::Type::getDoubleTy(context), "inttodouble");
        }
        if (rightType == DataType::INTEGER)
        {
            right = builder.CreateSIToFP(right, llvm::Type::getDoubleTy(context), "inttodouble");
        }
    }
    switch (infix->operat.type)
    {
    case TokenType::PLUS:
        return resultType == DataType::INTEGER ? builder.CreateAdd(left, right, "addtmp") : builder.CreateFAdd(left, right, "faddtmp");
    case TokenType::MINUS:
        return resultType == DataType::INTEGER ? builder.CreateSub(left, right, "addtmp") : builder.CreateFSub(left, right, "fsubtmp");
    case TokenType::ASTERISK:
        return resultType == DataType::INTEGER ? builder.CreateMul(left, right, "multmp") : builder.CreateFMul(left, right, "fmultmp");
    case TokenType::DIVIDE:
        return resultType == DataType::INTEGER ? builder.CreateSDiv(left, right, "divtmp") : builder.CreateFDiv(left, right, "fdivtmp");
    case TokenType::MODULUS:
        return resultType == DataType::INTEGER ? builder.CreateSRem(left, right, "modtmp") : throw std::runtime_error("Modulus not supported for FLOAT or DOUBLE at line " + std::to_string(infix->operat.line));
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
    {
        throw std::runtime_error("Invalid prefix expression");
    }
    llvm::Value *operand = generateExpression(prefix->operand.get());
    if (!operand)
    {
        throw std::runtime_error("Failed to generate IR for prefix operand");
    }
    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end())
    {
        throw std::runtime_error("Meta data missing for prefix node");
    }
    DataType resultType = it->second.symbolDataType;
    if (!(resultType == DataType::INTEGER || resultType == DataType::FLOAT || resultType == DataType::DOUBLE))
    {
        throw std::runtime_error("Prefix expression ");
    }
    if (prefix->operat.type == TokenType::MINUS)
    {
        return resultType == DataType::INTEGER ? builder.CreateNeg(operand, "negtmp") : builder.CreateFNeg(operand, "fnegtmp");
    }
    throw std::runtime_error("Unsupported prefix operator: " + prefix->operat.TokenLiteral +
                             " at line " + std::to_string(prefix->operat.line));
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
    DataType dt = it->second.symbolDataType;
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
    DataType dt = it->second.symbolDataType;
    if (dt != DataType::CHAR && dt != DataType::NULLABLE_CHAR)
    {
        throw std::runtime_error("Type error: Expected CHAR for CharLiteral");
    }
    std::string tokenLiteral = charLit->char_token.TokenLiteral;

    char c = decodeCharLiteral(tokenLiteral);
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(c), false);
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
    DataType dt = it->second.symbolDataType;
    if (dt != DataType::BOOLEAN && dt != DataType::NULLABLE_BOOLEAN)
    {
        throw std::runtime_error("Type error: Expected BOOLEAN for BooleanLiteral");
    }

    bool value = (boolLit->boolean_token.TokenLiteral == "true");

    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), value);
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
        throw std::runtime_error("Integer literal not found in metadata");
    }
    DataType dt = it->second.symbolDataType;
    if (dt != DataType::INTEGER && dt != DataType::NULLABLE_INT)
    {
        throw std::runtime_error("Type error: Expected INTEGER or NULLABLE_INT for IntegerLiteral");
    }
    int64_t value = std::stoll(intLit->int_token.TokenLiteral);
    return llvm::ConstantInt::get(context, llvm::APInt(32, value, true));
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
    DataType dt = it->second.symbolDataType;
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
    DataType dt = it->second.symbolDataType;
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
    std::cout << "Submitted data type: " << semantics.dataTypetoString(type) << "\n";

    switch (type)
    {
    case DataType::NULLABLE_STR:
        return llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(context));
    case DataType::NULLABLE_INT:
        return llvm::ConstantInt::get(context, llvm::APInt(32, 1 << 31));
    case DataType::NULLABLE_FLT:
        return llvm::ConstantFP::get(context, llvm::APFloat::getQNaN(llvm::APFloat::IEEEsingle()));
    case DataType::NULLABLE_BOOLEAN:
        return llvm::ConstantInt::get(context, llvm::APInt(1, 2, false));
    case DataType::NULLABLE_DOUBLE:
        return llvm::ConstantFP::get(context, llvm::APFloat::getQNaN(llvm::APFloat::IEEEdouble()));
    case DataType::NULLABLE_CHAR:
        return llvm::ConstantInt::get(context, llvm::APInt(8, 0));
    default:
        throw std::runtime_error("Unsupported null type");
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
    SymbolInfo *symbol = semantics.resolveSymbolInfo(identExpr->identifier.TokenLiteral); // Looking if the identifier name already exists
    if (!symbol)
    {
        throw std::runtime_error("Use of undeclared variable name '" + identExpr->identifier.TokenLiteral + "'");
    }
    // Checking if the identifier has a value
    auto it = namedValues.find(identExpr->identifier.TokenLiteral);
    if (it == namedValues.end())
    {
        throw std::runtime_error("Variable '" + identExpr->identifier.TokenLiteral + "' has no storage (missing alloca?)");
    }
    llvm::Value *variablePtr = it->second;
    return builder.CreateLoad(getLLVMType(symbol->symbolDataType), variablePtr, identExpr->identifier.TokenLiteral);
}

// HELPER FUNCTIONS
llvm::Type *IRGenerator::getLLVMType(DataType type)
{
    switch (type)
    {
    case DataType::INTEGER:
        return llvm::Type::getInt32Ty(context);
    case DataType::FLOAT:
        return llvm::Type::getFloatTy(context);
    case DataType::DOUBLE:
        return llvm::Type::getDoubleTy(context);
    case DataType::BOOLEAN:
        return llvm::Type::getInt1Ty(context);
    case DataType::CHAR:
        return llvm::Type::getInt8Ty(context);
    case DataType::STRING:
        return llvm::Type::getInt8PtrTy(context);
    case DataType::NULLABLE_STR:
        return llvm::Type::getInt8PtrTy(context);
    case DataType::NULLABLE_CHAR:
        return llvm::Type::getInt8Ty(context);
    case DataType::NULLABLE_INT:
        return llvm::Type::getInt32Ty(context);
    case DataType::NULLABLE_FLT:
        return llvm::Type::getFloatTy(context);
    case DataType::NULLABLE_DOUBLE:
        return llvm::Type::getDoubleTy(context);
    case DataType::NULLABLE_BOOLEAN:
        return llvm::Type::getInt1Ty(context);
    case DataType::UNKNOWN:
        throw std::runtime_error("Unknown data type encountered");
    default:
        return nullptr;
    }
}

void IRGenerator::registerGeneratorFunctions()
{
    generatorFunctionsMap[typeid(LetStatement)] = &IRGenerator::generateLetStatement;
    generatorFunctionsMap[typeid(ExpressionStatement)] = &IRGenerator::generateExpressionStatement;
    generatorFunctionsMap[typeid(AssignmentStatement)] = &IRGenerator::generateAssignmentStatement;
    generatorFunctionsMap[typeid(WhileStatement)] = &IRGenerator::generateWhileStatement;
    generatorFunctionsMap[typeid(BreakStatement)] = &IRGenerator::generateBreakStatement;
    generatorFunctionsMap[typeid(ContinueStatement)] = &IRGenerator::generateContinueStatement;
}

void IRGenerator::registerExpressionGeneratorFunctions()
{
    expressionGeneratorsMap[typeid(InfixExpression)] = &IRGenerator::generateInfixExpression;
    expressionGeneratorsMap[typeid(PrefixExpression)] = &IRGenerator::generatePrefixExpression;
    expressionGeneratorsMap[typeid(StringLiteral)] = &IRGenerator::generateStringLiteral;
    expressionGeneratorsMap[typeid(CharLiteral)] = &IRGenerator::generateCharLiteral;
    expressionGeneratorsMap[typeid(BooleanLiteral)] = &IRGenerator::generateBooleanLiteral;
    expressionGeneratorsMap[typeid(IntegerLiteral)] = &IRGenerator::generateIntegerLiteral;
    expressionGeneratorsMap[typeid(FloatLiteral)] = &IRGenerator::generateFloatLiteral;
    expressionGeneratorsMap[typeid(DoubleLiteral)] = &IRGenerator::generateDoubleLiteral;
    expressionGeneratorsMap[typeid(Identifier)] = &IRGenerator::generateIdentifierExpression;
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

void IRGenerator::dumpIR()
{
    module->print(llvm::outs(), nullptr);
}
