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
        auto generatorIt = generatorFunctionsMap.find(typeid(*node));
        if (generatorIt == generatorFunctionsMap.end())
        {
            std::cout << "Failed to find IR generator for : " << node->toString() << "\n";
            return;
        }
        (this->*generatorIt->second)(node.get());
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
        throw std::runtime_error("Unsupported expression type");
    }
    return (this->*exprIt->second)(node);
}

// GENERATOR FUNCTIONS
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
        llvm::Value *initValue = generateExpression(letStmt->value.get());
        if (!initValue)
        {
            throw std::runtime_error("Failed to generate IR for initializer of: " + letStmt->ident_token.TokenLiteral);
        }
        builder.CreateStore(initValue, alloca);
    }
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
    if (!(resultType == DataType::INTEGER || resultType == DataType::FLOAT || resultType == DataType::DOUBLE))
    {
        throw std::runtime_error("InfixExpression type must be int, float,or double ");
    }
    DataType leftType = semantics.metaData[infix->left_operand.get()].symbolDataType;
    DataType rightType = semantics.metaData[infix->right_operand.get()].symbolDataType;

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

llvm::Value *IRGenerator::generateIntegerLiteral(Node *node)
{
    auto intLit = dynamic_cast<IntegerLiteral *>(node);
    if (!intLit)
    {
        throw std::runtime_error("Invalid integer literal");
    }
    auto it = semantics.metaData.find(node);
    if (it == semantics.metaData.end() || it->second.symbolDataType != DataType::INTEGER)
    {
        throw std::runtime_error("Type error: Expected INTEGER for IntegerLiteral");
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
    if (it == semantics.metaData.end() || it->second.symbolDataType != DataType::FLOAT)
    {
        throw std::runtime_error("Type error: Expected float for FloatLiteral");
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
    if (it == semantics.metaData.end() || it->second.symbolDataType != DataType::DOUBLE)
    {
        throw std::runtime_error("Type error: Expected 'double' for DoubleLiteral");
    } // Checking if we have metaData about the double literal and if so we check to see if the data type is double
    double value = std::stod(dbLit->double_token.TokenLiteral);            // Converting the double literal from a string to a double
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), value); // Returning double value
}

llvm::Value *IRGenerator::generateNullLiteral(Node *node)
{
    auto nullLit = dynamic_cast<NullLiteral *>(node);
    if (!nullLit)
    {
        throw std::runtime_error("Invalid null literal");
    }
    auto it = semantics.metaData.find(node);

    DataType type = it->second.symbolDataType;
    std::cout<<"Submitted data type: "<<semantics.dataTypetoString(type)<<"\n";

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
    case DataType::NULLABLE:
        return llvm::Type::getVoidTy(context);
    default:
        return nullptr;
    }
}

void IRGenerator::registerGeneratorFunctions()
{
    generatorFunctionsMap[typeid(LetStatement)] = &IRGenerator::generateLetStatement;
    generatorFunctionsMap[typeid(ExpressionStatement)] = &IRGenerator::generateExpressionStatement;
    generatorFunctionsMap[typeid(AssignmentStatement)] = &IRGenerator::generateAssignmentStatement;
}

void IRGenerator::registerExpressionGeneratorFunctions()
{
    expressionGeneratorsMap[typeid(InfixExpression)] = &IRGenerator::generateInfixExpression;
    expressionGeneratorsMap[typeid(PrefixExpression)] = &IRGenerator::generatePrefixExpression;
    expressionGeneratorsMap[typeid(IntegerLiteral)] = &IRGenerator::generateIntegerLiteral;
    expressionGeneratorsMap[typeid(FloatLiteral)] = &IRGenerator::generateFloatLiteral;
    expressionGeneratorsMap[typeid(DoubleLiteral)] = &IRGenerator::generateDoubleLiteral;
    expressionGeneratorsMap[typeid(Identifier)] = &IRGenerator::generateIdentifierExpression;
    expressionGeneratorsMap[typeid(NullLiteral)] = &IRGenerator::generateNullLiteral;
}

void IRGenerator::dumpIR()
{
    module->print(llvm::outs(), nullptr);
}
