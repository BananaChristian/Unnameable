#include "irgen.hpp"
#include "ast.hpp"

#include <iostream>

IRGenerator::IRGenerator(Semantics &semantics)
    : semantics(semantics), context(), builder(context), module(std::make_unique<llvm::Module>("unnameable", context))
{
    registerGeneratorFunctions();
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

// GENERATOR FUNCTIONS
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
    case DataType::NULLABLE:
        return llvm::Type::getVoidTy(context);
    default:
        return nullptr;
    }
}

// Expression generator helper functions
llvm::Value *IRGenerator::generateExpression(Node *node)
{
    // Generating IR for integer literal
    if (auto intLit = dynamic_cast<IntegerLiteral *>(node))
    {
        auto it = semantics.metaData.find(node);
        if (it == semantics.metaData.end() || it->second.symbolDataType != DataType::INTEGER)
        {
            throw std::runtime_error("Type error: Expected INTEGER for IntegerLiteral");
        }
        int64_t value = std::stoll(intLit->int_token.TokenLiteral);
        return llvm::ConstantInt::get(context, llvm::APInt(32, value, true));
    }

    // Generating IR for float literal
    if (auto fltLit = dynamic_cast<FloatLiteral *>(node))
    {
        auto it = semantics.metaData.find(node);
        if (it == semantics.metaData.end() || it->second.symbolDataType != DataType::FLOAT)
        {
            throw std::runtime_error("Type error: Expected float for FloatLiteral");
        }
        float value = std::stof(fltLit->float_token.TokenLiteral);
        return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
    }

    // Generating IR for double literal
    if (auto dbLit = dynamic_cast<DoubleLiteral *>(node))
    {
        auto it = semantics.metaData.find(node);
        if (it == semantics.metaData.end() || it->second.symbolDataType != DataType::DOUBLE)
        {
            throw std::runtime_error("Type error: Expected 'double' for DoubleLiteral");
        }
        double value = std::stod(dbLit->double_token.TokenLiteral);
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), value);
    }

    // Generating IR for infix expression
    if (auto infix = dynamic_cast<InfixExpression *>(node))
    {
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

    // Generating IR for prefix expression
    if (auto prefix = dynamic_cast<PrefixExpression *>(node))
    {
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
    throw std::runtime_error("Unsupported expression type");
}

void IRGenerator::registerGeneratorFunctions()
{
    generatorFunctionsMap[typeid(LetStatement)] = &IRGenerator::generateLetStatement;
    generatorFunctionsMap[typeid(ExpressionStatement)] = &IRGenerator::generateExpressionStatement;
}

void IRGenerator::dumpIR()
{
    module->print(llvm::outs(), nullptr);
}
