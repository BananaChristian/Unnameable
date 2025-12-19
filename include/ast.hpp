#pragma once
#include "token/token.hpp"
#include <memory>
#include <string>
#include <iostream>
#include <vector>
#include <optional>
#include <utility>

#define CPPREST_FORCE_REBUILD

template <typename T>
std::unique_ptr<T> clonePtr(const std::unique_ptr<T> &ptr)
{
    if (!ptr)
        return nullptr;
    return std::unique_ptr<T>(static_cast<T *>(ptr->shallowClone()));
}

template <typename T>
std::vector<std::unique_ptr<T>> clonePtrVector(const std::vector<std::unique_ptr<T>> &vec)
{
    std::vector<std::unique_ptr<T>> out;
    out.reserve(vec.size());
    for (auto &item : vec)
    {
        out.push_back(std::unique_ptr<T>(static_cast<T *>(item->shallowClone())));
    }
    return out;
}

enum class Mutability
{
    IMMUTABLE,
    MUTABLE,
    CONSTANT
};

// GENERAL AST NODE
struct Node
{
    Token token;

    virtual ~Node() = default;

    virtual std::string toString()
    {
        return "Node: " + token.TokenLiteral;
    };

    virtual Node *shallowClone() const = 0;

    Node(Token t) : token(t) {}
};

// GENERAL EXPRESSION NODE
struct Expression : Node
{
    Token expression;
    std::string toString() override
    {
        return "Expression: " + expression.TokenLiteral;
    }

    Expression *shallowClone() const override
    {
        return new Expression(
            expression);
    }

    Expression(Token expr) : Node(expr), expression(expr) {};
};

// GENERAL STATEMENT NODE
struct Statement : Node
{
    Token statement;
    std::string toString() override
    {
        return "Statement: " + statement.TokenLiteral;
    }

    Statement *shallowClone() const override
    {
        return new Statement(
            statement);
    }
    Statement(Token stmt) : Node(stmt), statement(stmt) {};
};

// Identifier statement node
struct Identifier : Expression
{
    Token identifier;
    std::string toString() override
    {
        return "Identifier Expression: " + identifier.TokenLiteral;
    }

    Identifier *shallowClone() const override
    {
        return new Identifier(
            identifier);
    }
    Identifier(Token ident) : Expression(ident), identifier(ident) {};
};

// Address expression node
struct AddressExpression : Expression
{
    Token addr_token;
    std::unique_ptr<Expression> identifier;

    std::string toString() override
    {
        return "Address Expression: " + addr_token.TokenLiteral + identifier->toString();
    }

    AddressExpression *shallowClone() const override
    {
        return new AddressExpression(
            addr_token,
            clonePtr(identifier));
    }
    AddressExpression(Token addr_t, std::unique_ptr<Expression> ident) : Expression(addr_t), addr_token(addr_t), identifier(std::move(ident)) {};
};

// Dereference expression node
struct DereferenceExpression : Expression
{
    Token deref_token;
    std::unique_ptr<Expression> identifier;

    std::string toString() override
    {
        return "Dereference Expression: " + deref_token.TokenLiteral + " " + identifier->toString();
    }

    DereferenceExpression *shallowClone() const override
    {
        return new DereferenceExpression(
            deref_token,
            clonePtr(identifier));
    }

    DereferenceExpression(Token deref, std::unique_ptr<Expression> ident) : Expression(deref), deref_token(deref), identifier(std::move(ident)) {}
};

struct SelfExpression : Expression
{
    Token self_token; // e.g. self
    std::vector<std::unique_ptr<Expression>> fields;

    std::string toString() override
    {
        std::string fieldStr = "";
        for (const auto &field : fields)
        {
            fieldStr += "." + field->toString();
        }
        return "Self Expression: " + self_token.TokenLiteral + fieldStr;
    }

    SelfExpression(Token self, std::vector<std::unique_ptr<Expression>> fieldExpr)
        : Expression(self), self_token(self), fields(std::move(fieldExpr)) {};
};

struct NewComponentExpression : Expression
{
    Token new_token;      // token for 'new'
    Token component_name; // e.g. 'Player'
    std::vector<std::unique_ptr<Expression>> arguments;

    std::string toString() override
    {
        std::string args_str;
        for (auto &arg : arguments)
        {
            args_str += arg->toString() + ", ";
        }
        return "NewComponentExpression: new " + component_name.TokenLiteral + "(" + args_str + ")";
    }

    NewComponentExpression(Token newTok, Token compName, std::vector<std::unique_ptr<Expression>> args)
        : Expression(newTok), new_token(newTok), component_name(compName), arguments(std::move(args)) {}
};

// Null literal
struct NullLiteral : Expression
{
    Token null_token;
    std::string toString() override
    {
        return "Null Literal: " + null_token.TokenLiteral;
    }

    NullLiteral *shallowClone() const override
    {
        return new NullLiteral(
            null_token);
    }

    NullLiteral(Token null_tok) : Expression(null_tok), null_token(null_tok) {};
};

//Signed 8 bit Integer literal
struct I8Literal:Expression{
    Token i8_token;

    std::string toString() override
    {
        return "i8 Literal: " + i8_token.TokenLiteral;
    }

    I8Literal *shallowClone() const override{
        return new I8Literal(
            i8_token);
    }
    I8Literal(Token i8_t):Expression(i8_t),i8_token(i8_t){}
};

//Unsigned 8 bit Integer literal
struct U8Literal:Expression{
    Token u8_token;

    std::string toString() override
    {
        return "u8 Literal: " + u8_token.TokenLiteral;
    }

    U8Literal *shallowClone() const override{
        return new U8Literal(
            u8_token);
    }
    U8Literal(Token u8_t):Expression(u8_t),u8_token(u8_t){}
};

// Signed 16 bit Integer literal
struct I16Literal : Expression
{
    Token i16_token;
    std::string toString() override
    {
        return "i16 Literal: " + i16_token.TokenLiteral;
    }

    I16Literal *shallowClone() const override
    {
        return new I16Literal(
            i16_token);
    }
    I16Literal(Token i16_t) : Expression(i16_t), i16_token(i16_t) {};
};

// Unsigned 16 bit Ineteger literal
struct U16Literal : Expression
{
    Token u16_token;
    std::string toString() override
    {
        return "u16 Literal: " + u16_token.TokenLiteral;
    }
    U16Literal *shallowClone() const override
    {
        return new U16Literal(
            u16_token);
    }

    U16Literal(Token u16_t) : Expression(u16_t), u16_token(u16_t) {};
};

// Signed 32 bit Integer literal
struct I32Literal : Expression
{
    Token i32_token;
    std::string toString() override
    {
        return "i32 Literal: " + i32_token.TokenLiteral;
    }

    I32Literal *shallowClone() const override
    {
        return new I32Literal(
            i32_token);
    }
    I32Literal(Token i32_t) : Expression(i32_t), i32_token(i32_t) {};
};

// Unsigned 32 bit integer literal
struct U32Literal : Expression
{
    Token u32_token;
    std::string toString() override
    {
        return "u32 Literal: " + u32_token.TokenLiteral;
    }

    U32Literal *shallowClone() const override
    {
        return new U32Literal(
            u32_token);
    }
    U32Literal(Token u32_t) : Expression(u32_t), u32_token(u32_t) {};
};

// Signed 64 bit integer literal
struct I64Literal : Expression
{
    Token i64_token;
    std::string toString() override
    {
        return "i64 Literal: " + i64_token.TokenLiteral;
    }

    I64Literal *shallowClone() const override
    {
        return new I64Literal(
            i64_token);
    }
    I64Literal(Token i64_t) : Expression(i64_t), i64_token(i64_t) {};
};

// Unsigned 64 bit integer literal
struct U64Literal : Expression
{
    Token u64_token;
    std::string toString() override
    {
        return "u64 Literal: " + u64_token.TokenLiteral;
    }

    U64Literal *shallowClone() const override
    {
        return new U64Literal(
            u64_token);
    }

    U64Literal(Token u64_t) : Expression(u64_t), u64_token(u64_t) {};
};

// Signed 128 bit integer literal
struct I128Literal : Expression
{
    Token i128_token;
    std::string toString() override
    {
        return "i128 Literal: " + i128_token.TokenLiteral;
    }

    I128Literal *shallowClone() const override
    {
        return new I128Literal(
            i128_token);
    }
    I128Literal(Token i128_t) : Expression(i128_t), i128_token(i128_t) {};
};

// Unsigned 128 bit integer literal
struct U128Literal : Expression
{
    Token u128_token;
    std::string toString() override
    {
        return "U128 Literal: " + u128_token.TokenLiteral;
    }

    U128Literal *shallowClone() const override
    {
        return new U128Literal(
            u128_token);
    }
    U128Literal(Token uextra_t) : Expression(uextra_t), u128_token(uextra_t) {};
};

// Boolean literal
struct BooleanLiteral : Expression
{
    Token boolean_token;
    std::string toString() override
    {
        return "Boolean Literal: " + boolean_token.TokenLiteral;
    }

    BooleanLiteral *shallowClone() const override
    {
        return new BooleanLiteral(
            boolean_token);
    }
    BooleanLiteral(Token bool_t) : Expression(bool_t), boolean_token(bool_t) {};
};

// Float literal
struct FloatLiteral : Expression
{
    Token float_token;
    std::string toString() override
    {
        return "Float Literal: " + float_token.TokenLiteral;
    }

    FloatLiteral *shallowClone() const override
    {
        return new FloatLiteral(
            float_token);
    }
    FloatLiteral(Token float_t) : Expression(float_t), float_token(float_t) {};
};

// Double literal
struct DoubleLiteral : Expression
{
    Token double_token;
    std::string toString() override
    {
        return "Double Literal: " + double_token.TokenLiteral;
    }

    DoubleLiteral *shallowClone() const override
    {
        return new DoubleLiteral(
            double_token);
    }
    DoubleLiteral(Token double_t) : Expression(double_t), double_token(double_t) {};
};

// 8 bit Char literal
struct Char8Literal : Expression
{
    Token char8_token;
    std::string toString() override
    {
        return "Char8 Literal: " + char8_token.TokenLiteral;
    }

    Char8Literal *shallowClone() const override
    {
        return new Char8Literal(
            char8_token);
    }
    Char8Literal(Token char8_t) : Expression(char8_t), char8_token(char8_t) {};
};

// 16 bit Char literal
struct Char16Literal : Expression
{
    Token char16_token;
    std::string toString() override
    {
        return "Char16 Literal: " + char16_token.TokenLiteral;
    }

    Char16Literal *shallowClone() const override
    {
        return new Char16Literal(
            char16_token);
    }
    Char16Literal(Token char16t) : Expression(char16t), char16_token(char16t) {};
};

// 32 bit Char literal
struct Char32Literal : Expression
{
    Token char32_token;
    std::string toString() override
    {
        return "Char32 Literal: " + char32_token.TokenLiteral;
    }

    Char32Literal *shallowClone() const override
    {
        return new Char32Literal(
            char32_token);
    }
    Char32Literal(Token char32t) : Expression(char32t), char32_token(char32t) {};
};

// String literal
struct StringLiteral : Expression
{
    Token string_token;
    std::string toString() override
    {
        return "String Literal: " + string_token.TokenLiteral;
    }

    StringLiteral *shallowClone() const override
    {
        return new StringLiteral(
            string_token);
    }
    StringLiteral(Token string_t) : Expression(string_t), string_token(string_t) {};
};

// Call expression
struct CallExpression : Expression
{
    std::unique_ptr<Expression> function_identifier;
    std::vector<std::unique_ptr<Expression>> parameters;
    std::string toString() override
    {
        std::string args_str;
        for (size_t i = 0; i < parameters.size(); ++i)
        {
            args_str += parameters[i]->toString();
            if (i < parameters.size() - 1)
                args_str += ", ";
        }
        return "Call Expression: " + function_identifier->toString() + "(" + args_str + ")";
    }

    CallExpression *shallowClone() const override
    {
        return new CallExpression(
            function_identifier->expression,
            clonePtr(function_identifier),
            clonePtrVector(parameters));
    }

    CallExpression(Token tok, std::unique_ptr<Expression> fn_ident, std::vector<std::unique_ptr<Expression>> params) : Expression(tok), function_identifier(std::move(fn_ident)), parameters(std::move(params)) {};
};

// Unwrap call expression
struct UnwrapExpression : Expression
{
    std::unique_ptr<Expression> call;
    std::string toString() override
    {
        std::string callStr = "<no_call>";
        if (call)
        {
            callStr = call->toString();
        }
        return "Unwrap expression: unwrap " + callStr;
    }

    UnwrapExpression(std::unique_ptr<Expression> callExpr) : Expression(callExpr->expression), call(std::move(callExpr)) {};
};

// Method call expression
struct MethodCallExpression : Expression
{
    std::unique_ptr<Expression> instance;
    std::unique_ptr<Expression> call;

    std::string toString() override
    {
        return "Method Call Expression: " + instance->toString() + "." +
               call->toString();
    }

    MethodCallExpression(std::unique_ptr<Expression> inst,
                         std::unique_ptr<Expression> callExpr)
        : Expression(inst->expression),
          instance(std::move(inst)),
          call(std::move(callExpr)) {}
};

// Instance expression
struct InstanceExpression : Expression
{
    std::unique_ptr<Expression> blockIdent;
    std::vector<std::unique_ptr<Statement>> fields;

    std::string toString() override
    {
        std::string args;
        for (const auto &field : fields)
        {
            args += field->toString();
        }
        return "Instance expression: " + blockIdent->toString() + " {" + args + "}";
    }

    InstanceExpression(std::unique_ptr<Expression> ident, std::vector<std::unique_ptr<Statement>> param) : Expression(ident->expression), blockIdent(std::move(ident)), fields(std::move(param)) {};
};

// Function expression struct node
struct FunctionExpression : Expression
{
    bool isExportable;
    Token func_key;
    std::vector<std::unique_ptr<Statement>> call;
    std::unique_ptr<Expression> return_type;
    std::unique_ptr<Expression> block;

    std::string toString() override
    {
        std::string cl = "(";
        for (size_t i = 0; i < call.size(); ++i)
        {
            cl += call[i] ? call[i]->toString() : "<null>";
            if (i < call.size() - 1)
            {
                cl += ", ";
            }
        }
        cl += ")";

        std::string ret_str = return_type ? return_type->toString() : "<no type>";
        std::string block_str = block ? block->toString() : "<no block>";
        std::string exportStr = isExportable ? "export " : "";

        return exportStr + "FunctionExpression: " + func_key.TokenLiteral + " " + "Function parameters: " + cl + " Return type: " + ret_str + " Function block: " + block_str;
    }

    FunctionExpression *shallowClone() const override
    {
        return new FunctionExpression(
            isExportable,
            func_key,
            clonePtrVector(call),
            clonePtr(return_type),
            clonePtr(block));
    }

    FunctionExpression(bool exportable, Token fn, std::vector<std::unique_ptr<Statement>> c, std::unique_ptr<Expression> return_t, std::unique_ptr<Expression> bl) : Expression(fn), func_key(fn), isExportable(exportable), call(std::move(c)), return_type(std::move(return_t)), block(std::move(bl)) {};
};

struct ArrayType : Expression
{
    Token arr_token;                       // 'arr' keyword
    std::unique_ptr<Expression> innerType; // Could be BasicReturnType or another ArrayReturnType
    bool isNullable;                       // Toggle if we see ?

    std::string toString() override
    {
        return "Array Type: " + arr_token.TokenLiteral + "[" + innerType->toString() + "]" + (isNullable ? "?" : "");
    }

    ArrayType *shallowClone() const override
    {
        return new ArrayType(
            arr_token,
            clonePtr(innerType),
            isNullable);
    }

    ArrayType(Token arr, std::unique_ptr<Expression> inner, bool isNull)
        : Expression(arr), arr_token(arr), innerType(std::move(inner)), isNullable(isNull) {}
};

struct BasicType : Expression
{
    Token data_token;        // Basic token like int
    bool isNullable = false; // If we see ? we toggle
    std::string toString() override
    {
        return "Basic Type: " + data_token.TokenLiteral + (isNullable ? "?" : "");
    }

    BasicType *shallowClone() const override
    {
        return new BasicType(
            data_token,
            isNullable);
    }

    BasicType(Token data, bool isNull) : Expression(data), data_token(data), isNullable(isNull) {};
};

struct PointerType : Expression
{
    Token ptr_token;
    std::unique_ptr<Expression> underlyingType;

    std::string toString() override
    {
        return "Pointer Type: " + underlyingType->toString() + "_ptr";
    }

    PointerType *shallowClone() const override
    {
        return new PointerType(
            ptr_token,
            clonePtr(underlyingType));
    }

    PointerType(Token ptr, std::unique_ptr<Expression> type) : Expression(ptr), ptr_token(ptr), underlyingType(std::move(type)) {};
};

struct RefType : Expression
{
    Token ref_token;
    std::unique_ptr<Expression> underLyingType;

    std::string toString() override
    {
        return "Ref type: " + underLyingType->toString() + "_ptr"; // A reference is a pointer dressed in fancy dance clothes
    }

    RefType *shallowClone() const override
    {
        return new RefType(
            ref_token,
            clonePtr(underLyingType));
    }

    RefType(Token ref, std::unique_ptr<Expression> type) : Expression(ref), ref_token(ref), underLyingType(std::move(type)) {};
};

// Return type expression
struct ReturnType : Expression
{
    std::unique_ptr<Expression> returnExpr;
    std::string toString() override
    {
        return "Return Type: " + returnExpr->toString();
    }

    ReturnType *shallowClone() const override
    {
        return new ReturnType(
            clonePtr(returnExpr));
    }

    ReturnType(std::unique_ptr<Expression> retExpr) : Expression(retExpr->expression), returnExpr(std::move(retExpr)) {};
};

// Error expression
struct ErrorExpression : Expression
{
    Token error_token;
    std::unique_ptr<Expression> err_message;
    std::string toString() override
    {
        return "Error expression: " + err_message->toString();
    };

    ErrorExpression(Token err_token, std::unique_ptr<Expression> message) : Expression(err_token), error_token(err_token), err_message(std::move(message)) {};
};

// Prefix expression node for syntax like !true;
struct PrefixExpression : Expression
{
    Token operat;
    std::unique_ptr<Expression> operand;
    std::string toString() override
    {
        return "Prefix Expression: (" + operat.TokenLiteral + operand->toString() + ")";
    }
    PrefixExpression(Token opr, std::unique_ptr<Expression> oprand)
        : Expression(opr), operat(opr), operand(std::move(oprand)) {}
};

// Postfix expression node for syntax like i++
struct PostfixExpression : Expression
{
    std::unique_ptr<Expression> operand;
    Token operator_token;

    std::string toString() override
    {
        return "Postfix Expression: (" + operand->toString() + operator_token.TokenLiteral + ")";
    }

    PostfixExpression *shallowClone() const override
    {
        return new PostfixExpression(
            clonePtr(operand),
            operator_token);
    }

    PostfixExpression(std::unique_ptr<Expression> op, Token operat) : Expression(operat), operand(std::move(op)), operator_token(operat) {};
};

// Infix Expression node for syntax like x+y;
struct InfixExpression : Expression
{
    std::unique_ptr<Expression> left_operand;
    Token operat;
    std::unique_ptr<Expression> right_operand;
    std::string toString() override
    {
        return "Infix Expression: (" + left_operand->toString() + " " + operat.TokenLiteral + " " + right_operand->toString() + ")";
    }

    InfixExpression *shallowClone() const override
    {
        return new InfixExpression(
            clonePtr(left_operand),
            operat,
            clonePtr(right_operand));
    }
    InfixExpression(std::unique_ptr<Expression> left, Token op, std::unique_ptr<Expression> right) : Expression(op), left_operand(std::move(left)), operat(op), right_operand(std::move(right)) {};
};

//-----STATEMENTS----

struct ExpressionStatement : Statement
{
    Token expr;
    std::unique_ptr<Expression> expression;
    std::string toString() override
    {
        if (expression)
        {
            return "Expression statement: " + expression->toString() + ";";
        }
        return ";";
    }

    ExpressionStatement *shallowClone() const override
    {
        return new ExpressionStatement(
            expr,
            clonePtr(expression));
    }
    ExpressionStatement(Token exp, std::unique_ptr<Expression> expr) : Statement(exp), expr(exp), expression(std::move(expr)) {};
};

// Break statement node
struct BreakStatement : Statement
{
    Token break_tok;
    std::string toString()
    {
        return "Break Statement: " + break_tok.TokenLiteral;
    }

    BreakStatement *shallowClone() const override
    {
        return new BreakStatement(
            break_tok);
    }
    BreakStatement(Token break_t) : Statement(break_t), break_tok(break_t) {};
};

// Continue statement struct
struct ContinueStatement : Statement
{
    Token cont_tok;
    std::string toString() override
    {
        return "Continue Statement: " + cont_tok.TokenLiteral;
    }

    ContinueStatement *shallowClone() const override
    {
        return new ContinueStatement(
            cont_tok);
    }
    ContinueStatement(Token cont_t) : Statement(cont_t), cont_tok(cont_t) {};
};

// Allocator interface statement
struct AllocatorStatement : Statement
{
    bool isExportable;
    Token allocator_token;
    std::unique_ptr<Expression> allocator_name;
    std::unique_ptr<Statement> block;

    std::string toString() override
    {
        std::string exportStr = isExportable ? "export " : "";
        std::string blockName = "";
        if (allocator_name)
        {
            blockName = allocator_name->toString() + " ";
        }

        return "Allocator Interface: " + exportStr + blockName + block->toString();
    }

    AllocatorStatement(bool exportable, Token alloc, std::unique_ptr<Expression> name, std::unique_ptr<Statement> blk) : Statement(alloc), allocator_token(alloc), allocator_name(std::move(name)), block(std::move(blk)) {};
};

struct DheapStatement : Statement
{
    Token dheap_token;
    std::unique_ptr<Expression> allocType;
    std::unique_ptr<Statement> stmt;

    std::string toString() override
    {
        std::string allocName = "";
        std::string stmtStr = "";

        if (allocType)
        {
            allocName = " <" + allocType->toString() + ">";
        }

        if (stmt)
        {
            stmtStr = stmt->toString();
        }

        return "DHeap Statement: " + dheap_token.TokenLiteral + allocName + " " + stmtStr;
    }

    DheapStatement(Token d_tok, std::unique_ptr<Expression> alloc, std::unique_ptr<Statement> st) : Statement(d_tok), allocType(std::move(alloc)), stmt(std::move(st)) {};
};

// Seal block statement
struct SealStatement : Statement
{
    bool isExportable;
    Token seal_token;
    std::unique_ptr<Expression> sealName;
    std::unique_ptr<Statement> block;

    std::string toString() override
    {
        std::string exportStr = isExportable ? "export " : "";
        std::string name = "No name";
        if (sealName)
        {
            name = sealName->toString();
        }
        std::string blockStr = "";
        if (block)
        {
            blockStr = block->toString();
        }
        return "Seal Statement: " + exportStr + seal_token.TokenLiteral + " " + name + blockStr;
    }

    SealStatement(bool isExp, Token seal, std::unique_ptr<Expression> name, std::unique_ptr<Statement> blk) : Statement(seal), isExportable(isExp), seal_token(seal), sealName(std::move(name)), block(std::move(blk)) {}
};

// Use statement struct
struct UseStatement : Statement
{
    Token use_token;
    Token kind_token; // "data" or "behavior"
    std::unique_ptr<Expression> blockNameOrCall;

    std::string toString() override
    {
        std::string result = "Use statement: ";
        result += kind_token.TokenLiteral + " ";
        result += blockNameOrCall->toString();
        return result + ";";
    }

    UseStatement(Token useTok, Token kindTok, std::unique_ptr<Expression> name)
        : Statement(useTok), use_token(useTok), kind_token(kindTok), blockNameOrCall(std::move(name)) {}
};

// Data statement struct
struct DataStatement : Statement
{
    bool isExportable;
    Mutability mutability;
    Token data_token;
    std::unique_ptr<Expression> dataBlockName;
    std::vector<std::unique_ptr<Statement>> fields;

    std::string toString() override
    {
        std::string mutStr;
        if (mutability == Mutability::MUTABLE)
        {
            mutStr += "mut ";
        }
        else if (mutability == Mutability::CONSTANT)
        {
            mutStr += "const ";
        }
        std::string exportStr = isExportable ? "export " : "";
        std::string result = exportStr + mutStr + " Data statement: " + dataBlockName->toString() + " {\n";
        for (const auto &field : fields)
        {
            result += "  " + field->toString() + "\n";
        }
        result += "}";
        return result;
    }

    DataStatement(bool exportable, Mutability mut, Token data, std::unique_ptr<Expression> block_name, std::vector<std::unique_ptr<Statement>> data_fields) : Statement(data), isExportable(exportable), mutability(mut), data_token(data), dataBlockName(std::move(block_name)), fields(std::move(data_fields)) {};
};

// Behavior statement struct
struct BehaviorStatement : Statement
{
    bool isExportable;
    Token behavior_token;
    std::unique_ptr<Expression> behaviorBlockName;
    std::vector<std::unique_ptr<Statement>> functions;
    std::string toString() override
    {
        std::string exportStr = isExportable ? "export " : "";
        std::string result = exportStr + "Behavior statement: " + behaviorBlockName->toString() + " {\n";
        for (const auto &func : functions)
        {
            result += "  " + func->toString() + "\n";
        }
        result += "}";
        return result;
    }

    BehaviorStatement(bool exportable, Token behavior, std::unique_ptr<Expression> behavior_name, std::vector<std::unique_ptr<Statement>> funcs) : Statement(behavior), isExportable(exportable), behavior_token(behavior), behaviorBlockName(std::move(behavior_name)), functions(std::move(funcs)) {};
};

// Init statement
struct InitStatement : Statement
{
    Token init_token;
    std::vector<std::unique_ptr<Statement>> constructor_args;
    std::unique_ptr<Statement> block;

    std::string toString() override
    {
        std::string args;
        for (auto &arguments : constructor_args)
        {
            args += arguments->toString();
        }
        return "Init Statement: " + init_token.TokenLiteral + " (" + args + ") " + block->toString();
    }

    InitStatement(Token init, std::vector<std::unique_ptr<Statement>> args, std::unique_ptr<Statement> block_content) : Statement(init), constructor_args(std::move(args)), block(std::move(block_content)) {};
};

// Component statement struct
struct ComponentStatement : Statement
{
    bool isExportable;
    Token component_token;
    std::unique_ptr<Expression> component_name;

    std::vector<std::unique_ptr<Statement>> privateData;
    std::vector<std::unique_ptr<Statement>> privateMethods;

    std::vector<std::unique_ptr<Statement>> usedDataBlocks;
    std::vector<std::unique_ptr<Statement>> usedBehaviorBlocks;

    std::optional<std::unique_ptr<Statement>> initConstructor;

    std::string toString() override
    {
        std::string exportStr = isExportable ? "export " : "";
        std::string result = exportStr + "Component Statement: " + component_name->toString() + " {\n";
        // Private data
        for (const auto &field : privateData)
        {
            if (!field)
            {
                std::cout << "Encountered null field\n";
            }
            else
            {
                result += " " + field->toString() + "\n";
            }
        }

        // Private methods
        for (const auto &method : privateMethods)
        {
            if (!method)
            {
                std::cout << "Encountered null method\n";
            }
            else
            {
                result += "  " + method->toString() + "\n";
            }
        }

        // Used data blocks
        for (const auto &use : usedDataBlocks)
        {
            result += "  " + use->toString() + "\n";
        }

        // Used behavior blocks
        for (const auto &use : usedBehaviorBlocks)
        {
            result += "  " + use->toString() + "\n";
        }

        // Init constructor
        if (initConstructor.has_value())
        {
            result += " " + (*initConstructor)->toString();
        }

        result += "}";
        return result;
    }

    ComponentStatement(
        bool exportable,
        Token component,
        std::unique_ptr<Expression> name,
        std::vector<std::unique_ptr<Statement>> private_data,
        std::vector<std::unique_ptr<Statement>> private_methods,
        std::vector<std::unique_ptr<Statement>> used_data_blocks,
        std::vector<std::unique_ptr<Statement>> used_behavior_blocks,
        std::optional<std::unique_ptr<Statement>> init)
        : Statement(component),
          isExportable(exportable),
          component_token(component),
          component_name(std::move(name)),
          privateData(std::move(private_data)),
          privateMethods(std::move(private_methods)),
          usedDataBlocks(std::move(used_data_blocks)),
          usedBehaviorBlocks(std::move(used_behavior_blocks)),
          initConstructor(std::move(init)) {}
};

// Error statement
struct ErrorStatement : Statement
{
    Token errorStmt_token;
    std::unique_ptr<Expression> errorExpr;
    std::string toString() override
    {
        std::string errorStr = "<empty>";
        if (errorExpr)
        {
            errorStr = errorExpr->toString();
        }
        return "Error statement: error! " + errorStr;
    };

    ErrorStatement(Token err, std::unique_ptr<Expression> errExpr) : Statement(err), errorExpr(std::move(errExpr)) {};
};

// Reference Statament node
struct ReferenceStatement : Statement
{
    Token ref_token;
    Mutability mutability;
    std::unique_ptr<Expression> type;
    std::unique_ptr<Expression> referer;
    std::unique_ptr<Expression> referee;

    std::string toString() override
    {
        std::string mutStr = "";
        if (mutability == Mutability::MUTABLE)
        {
            mutStr += "mut ";
        }
        else if (mutability == Mutability::CONSTANT)
        {
            mutStr += "const ";
        }

        std::string typeStr = "";
        if (type)
        {
            typeStr += type->toString();
        }

        std::string valueStr = "";
        if (referee)
        {
            valueStr += " -> " + referee->toString();
        }

        return "Reference Statement: " + ref_token.TokenLiteral + " " + mutStr + typeStr + " " + referer->toString() + valueStr;
    };

    ReferenceStatement *shallowClone() const override
    {
        return new ReferenceStatement(
            ref_token,
            mutability,
            clonePtr(type),
            clonePtr(referer),
            clonePtr(referee));
    }

    ReferenceStatement(
        Token ref,
        Mutability mut,
        std::unique_ptr<Expression> data_type,
        std::unique_ptr<Expression> identifier,
        std::unique_ptr<Expression> value) : Statement(ref), mutability(mut), type(std::move(data_type)), referer(std::move(identifier)), referee(std::move(value)) {};
};

// Pointer statement node
struct PointerStatement : Statement
{
    Token ptr_token;
    Mutability mutability;
    std::unique_ptr<Expression> type;
    std::unique_ptr<Expression> name;
    std::unique_ptr<Expression> value;

    std::string toString() override
    {
        std::string mutStr = "";
        if (mutability == Mutability::MUTABLE)
        {
            mutStr += "mut ";
        }
        else if (mutability == Mutability::CONSTANT)
        {
            mutStr += "const ";
        }

        std::string typeStr = "";
        if (type)
        {
            typeStr += type->toString();
        }

        std::string valueStr = "";
        if (value)
        {
            valueStr += " -> " + value->toString();
        }
        return "Pointer Statement: " + ptr_token.TokenLiteral + " " + mutStr + typeStr + " " + name->toString() + valueStr;
    };

    PointerStatement *shallowClone() const override
    {
        return new PointerStatement(
            ptr_token,
            mutability,
            clonePtr(type),
            clonePtr(name),
            clonePtr(value));
    }

    PointerStatement(
        Token ptr,
        Mutability mut,
        std::unique_ptr<Expression> data_type,
        std::unique_ptr<Expression> identifier,
        std::unique_ptr<Expression> val) : Statement(ptr), ptr_token(ptr), mutability(mut), type(std::move(data_type)), name(std::move(identifier)), value(std::move(val)) {};
};

// Let statement node
struct LetStatement : Statement
{
    bool isHeap;
    Mutability mutability;
    std::unique_ptr<Expression> type;
    Token ident_token;
    std::optional<Token> assign_token;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        std::string mut_str = "";
        std::string nullStr = "";
        std::string heapStr = "";
        std::string typeStr = "Failed";
        if (isHeap)
        {
            heapStr = "heap ";
        }

        if (type)
        {
            typeStr = type->toString();
        }

        if (mutability == Mutability::MUTABLE)
            mut_str = "mut";
        else if (mutability == Mutability::CONSTANT)
            mut_str = "const ";

        std::string result = "Let Statement: (" + heapStr + mut_str + "Data Type: " + typeStr + nullStr + " Variable name: " + ident_token.TokenLiteral;

        if (value)
        {
            result += " Value: " + value->toString();
        }
        else
        {
            result += " Value: <uninitialized>";
        }

        result += ")";
        return result;
    }

    LetStatement *shallowClone() const
    {
        return new LetStatement(
            isHeap,
            mutability,
            clonePtr(type),
            ident_token,
            assign_token,
            clonePtr(value));
    }

    LetStatement(bool heap, Mutability muta, std::unique_ptr<Expression> data_t, const Token &ident_t, const std::optional<Token> &assign_t, std::unique_ptr<Expression> val) : isHeap(heap),
                                                                                                                                                                                mutability(muta),
                                                                                                                                                                                type(std::move(data_t)),
                                                                                                                                                                                ident_token(ident_t),
                                                                                                                                                                                assign_token(assign_t),
                                                                                                                                                                                Statement(ident_t),
                                                                                                                                                                                value(std::move(val)) {};
};

struct AssignmentStatement : Statement
{
    std::unique_ptr<Expression> identifier;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        return "Assignment statement: (Variable: " + identifier->toString() + " Value: " + value->toString() + ")";
    };

    AssignmentStatement *shallowClone() const override
    {
        return new AssignmentStatement(
            clonePtr(identifier),
            clonePtr(value));
    }
    AssignmentStatement(std::unique_ptr<Expression> ident, std::unique_ptr<Expression> val) : Statement(ident->token), identifier(std::move(ident)), value(std::move(val)) {};
};

struct FieldAssignment : Statement
{
    Token assignment_token;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        return "Field Assignment: (Variable: " + assignment_token.TokenLiteral + " Value: " + value->toString() + ")";
    }

    FieldAssignment(Token ident, std::unique_ptr<Expression> val) : Statement(ident), assignment_token(ident), value(std::move(val)) {};
};

// Signal statement node
struct SignalStatement : Statement
{
    Token signal_token;
    std::unique_ptr<Expression> identifier;
    std::unique_ptr<Statement> tstart;
    std::unique_ptr<Expression> func_arg;

    std::string toString() override
    {
        std::string result = "Signal Statement: " + signal_token.TokenLiteral + " " + identifier->toString() + "=" + tstart->toString() + "(" + func_arg->toString() + ")";
        return result;
    }

    SignalStatement(Token signal, std::unique_ptr<Expression> ident, std::unique_ptr<Statement> thread_st, std::unique_ptr<Expression> arg) : Statement(signal), identifier(std::move(ident)), tstart(std::move(thread_st)), func_arg(move(arg)) {};
};

// Start statement
struct StartStatement : Statement
{
    Token start_tok;
    std::string toString() override
    {
        return "Start Statement: " + start_tok.TokenLiteral;
    }
    StartStatement(Token start) : Statement(start), start_tok(std::move(start)) {};
};

// Wait statement
struct WaitStatement : Statement
{
    Token wait_token;
    std::unique_ptr<Expression> arg;
    std::string toString() override
    {
        return "Wait Statement: " + wait_token.TokenLiteral + "(" + arg->toString() + ")";
    };
    WaitStatement(Token wait, std::unique_ptr<Expression> a) : Statement(wait), arg(std::move(a)) {};
};

// Return statement node
struct ReturnStatement : Statement
{
    Token return_stmt;
    std::unique_ptr<Expression> return_value;
    std::unique_ptr<Statement> error_val;
    std::string toString() override
    {
        return "Return Statement: ( Token: " + return_stmt.TokenLiteral + " Value: " +
               (return_value ? return_value->toString() : "void") + "), Error: " + (error_val ? error_val->toString() : "no error return");
    }

    ReturnStatement *shallowClone() const override
    {
        return new ReturnStatement(
            return_stmt,
            clonePtr(return_value),
            clonePtr(error_val));
    }
    ReturnStatement(Token ret, std::unique_ptr<Expression> ret_val, std::unique_ptr<Statement> err) : Statement(ret), return_stmt(ret), return_value(std::move(ret_val)), error_val(std::move(err)) {};
};

// Elif statement node
struct elifStatement : Statement
{
    Token elif_token;
    std::unique_ptr<Expression> elif_condition;
    std::unique_ptr<Statement> elif_result;

    std::string toString() override
    {
        return "elifStatement: " + elif_token.TokenLiteral + "(" + elif_condition->toString() + ") {" + elif_result->toString() + "}";
    }

    elifStatement *shallowClone() const override
    {
        return new elifStatement(
            elif_token,
            clonePtr(elif_condition),
            clonePtr(elif_result));
    }

    elifStatement(Token token, std::unique_ptr<Expression> condition, std::unique_ptr<Statement> result) : Statement(token),
                                                                                                           elif_token(token),
                                                                                                           elif_condition(std::move(condition)),
                                                                                                           elif_result(std::move(result)) {};
};

// If statement node
struct ifStatement : Statement
{
    Token if_stmt;
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> if_result;

    std::vector<std::unique_ptr<Statement>> elifClauses;

    std::optional<Token> else_stmt;
    std::optional<std::unique_ptr<Statement>> else_result;

    std::string toString() override
    {
        std::string result = "IfStatement:\n";

        if (condition)
            result += "  if (" + condition->toString() + ") {\n";
        else
            result += "  if (<null condition>) {\n";

        if (if_result)
            result += "    " + if_result->toString() + "\n";
        else
            result += "    <null if_result>\n";

        result += "  }\n";

        if (!elifClauses.empty())
        {
            for (const auto &elifs : elifClauses)
            {
                result += elifs->toString();
            }
        }

        // ELSE block
        if (else_stmt.has_value())
        {
            result += "  else {\n";

            if (else_result.has_value() && *else_result)
                result += "    " + (*else_result)->toString() + "\n";
            else
                result += "    <null else_result>\n";

            result += "  }\n";
        }

        return result;
    }

    ifStatement *shallowClone() const override
    {
        return new ifStatement(
            if_stmt,
            clonePtr(condition),
            clonePtr(if_result),
            clonePtrVector(elifClauses),
            else_stmt,
            clonePtr(else_result.value()));
    }

    ifStatement(Token if_st, std::unique_ptr<Expression> condition_e, std::unique_ptr<Statement> if_r,
                std::vector<std::unique_ptr<Statement>> elifStmts,
                std::optional<Token> else_st, std::optional<std::unique_ptr<Statement>> else_r) : Statement(if_st),
                                                                                                  if_stmt(if_st), condition(std::move(condition_e)), if_result(std::move(if_r)),
                                                                                                  elifClauses(std::move(elifStmts)),
                                                                                                  else_stmt(else_st), else_result(std::move(else_r)) {};
};

// Case clause struct used in switch-case statement
struct CaseClause : Statement
{
    Token case_token;
    std::unique_ptr<Expression> condition;
    std::vector<std::unique_ptr<Statement>> body;

    std::string toString() override
    {
        std::string body_content;
        for (const auto &b : body)
        {
            body_content += "\n    " + b->toString();
        }
        return "case " + condition->toString() + ":" + body_content;
    }

    CaseClause *shallowClone() const override
    {
        return new CaseClause(
            case_token,
            clonePtr(condition),
            clonePtrVector(body));
    }

    CaseClause(Token token, std::unique_ptr<Expression> cond, std::vector<std::unique_ptr<Statement>> stmt)
        : Statement(token), case_token(token), condition(std::move(cond)), body(std::move(stmt)) {}
};

struct SwitchStatement : Statement
{
    Token switch_token;
    std::unique_ptr<Expression> switch_expr;
    // Inside the braces
    // Cases
    std::vector<std::unique_ptr<Statement>> case_clauses;
    // Default case
    Token default_token;
    std::vector<std::unique_ptr<Statement>> default_statements;

    std::string toString() override
    {
        std::string result = "switch (" + switch_expr->toString() + ") {\n";

        // Print all case clauses
        for (const auto &clause : case_clauses)
        {
            result += clause->toString() + "\n";
        }

        // Print default statements if they exist
        if (!default_statements.empty())
        {
            result += "\n  default:\n";
            for (const auto &stmt : default_statements)
            {
                result += "    " + stmt->toString() + "\n";
            }
        }

        result += "}";
        return result;
    }

    SwitchStatement *shallowClone() const override
    {
        return new SwitchStatement(
            switch_token,
            clonePtr(switch_expr),
            clonePtrVector(case_clauses),
            default_token,
            clonePtrVector(default_statements));
    }

    SwitchStatement(Token token, std::unique_ptr<Expression> expr, std::vector<std::unique_ptr<Statement>> cases, Token default_tok, std::vector<std::unique_ptr<Statement>> default_stmts)
        : Statement(token), switch_token(token), switch_expr(std::move(expr)), case_clauses(std::move(cases)), default_token(default_tok), default_statements(std::move(default_stmts)) {}
};

struct EnumMember : Node
{
    Token t;
    std::string enumMember;
    std::unique_ptr<Expression> value;

    std::string toString() override
    {
        std::string val;
        if (value)
        {
            val += "= " + value->toString();
        }
        return "Enum Member: " + enumMember + val;
    }

    EnumMember *shallowClone() const override
    {
        return new EnumMember(
            t,
            enumMember,
            clonePtr(value));
    }

    EnumMember(Token token, std::string member, std::unique_ptr<Expression> val) : Node(token), enumMember(member), value(std::move(val)) {};
};

// Enum class struct
struct EnumClassStatement : Statement
{
    bool isExportable;
    Token enum_token;
    Token class_token;
    std::unique_ptr<Expression> enum_identifier;
    std::optional<Token> int_type;
    std::vector<std::unique_ptr<EnumMember>> enum_content;
    std::string toString()
    {
        std::string exportStr = isExportable ? "export " : "";
        std::string enum_block;
        std::string int_typestr = "";

        if (int_type.has_value())
        {
            int_typestr = " : " + int_type.value().TokenLiteral;
        }

        for (const auto &enum_cont : enum_content)
        {
            enum_block += enum_cont->toString() + ",\n";
        }

        return exportStr + "Enum class statement: " + enum_identifier->toString() + int_typestr + " { " + enum_block + " }";
    }

    EnumClassStatement *shallowClone() const override
    {
        return new EnumClassStatement(
            isExportable,
            enum_token,
            class_token,
            clonePtr(enum_identifier),
            int_type,
            clonePtrVector(enum_content));
    }

    EnumClassStatement(bool exportable, Token enum_tok, Token class_tok, std::unique_ptr<Expression> enum_ident, std::optional<Token> intType, std::vector<std::unique_ptr<EnumMember>> enum_block) : Statement(enum_tok), isExportable(exportable), enum_token(enum_tok),
                                                                                                                                                                                                      class_token(class_tok),
                                                                                                                                                                                                      enum_identifier(std::move(enum_ident)),
                                                                                                                                                                                                      int_type(intType),
                                                                                                                                                                                                      enum_content(std::move(enum_block)) {};
};

struct ForStatement : Statement
{
    Token for_key;
    std::unique_ptr<Statement> initializer; // int i;
    std::unique_ptr<Expression> condition;  // i < 10
    std::unique_ptr<Statement> step;        // i = i + 1
    std::unique_ptr<Statement> body;        // the loop body

    ForStatement *shallowClone() const override
    {
        return new ForStatement(
            for_key,
            clonePtr(initializer),
            clonePtr(condition),
            clonePtr(step),
            clonePtr(body));
    }

    ForStatement(Token for_k,
                 std::unique_ptr<Statement> init,
                 std::unique_ptr<Expression> cond,
                 std::unique_ptr<Statement> step,
                 std::unique_ptr<Statement> body)
        : Statement(for_k),
          for_key(for_k),
          initializer(std::move(init)),
          condition(std::move(cond)),
          step(std::move(step)),
          body(std::move(body)) {};

    std::string toString() override
    {
        std::string out = "ForStatement(\n";
        out += "  Init: " + (initializer ? initializer->toString() : "null") + "\n";
        out += "  Cond: " + (condition ? condition->toString() : "null") + "\n";
        out += "  Step: " + (step ? step->toString() : "null") + "\n";
        out += "  Body: " + (body ? body->toString() : "null") + "\n";
        out += ")";
        return out;
    }
};

struct EachStatement : Statement
{
    Token each_key;
    std::unique_ptr<Statement> iteratorVar;
    std::unique_ptr<Expression> iterable;
    std::unique_ptr<Statement> body;

    EachStatement *shallowClone() const override
    {
        return new EachStatement(
            each_key,
            clonePtr(iteratorVar),
            clonePtr(iterable),
            clonePtr(body));
    }

    EachStatement(Token for_k,
                  std::unique_ptr<Statement> iterVar,
                  std::unique_ptr<Expression> iterable,
                  std::unique_ptr<Statement> body)
        : Statement(for_k),
          each_key(for_k),
          iteratorVar(std::move(iterVar)),
          iterable(std::move(iterable)),
          body(std::move(body)) {}

    std::string toString() override
    {
        std::string out = "EachStatement(\n";
        out += "  Var: " + (iteratorVar ? iteratorVar->toString() : "null") + "\n";
        out += "  Iterable: " + (iterable ? iterable->toString() : "null") + "\n";
        out += "  Body: " + (body ? body->toString() : "null") + "\n";
        out += ")";
        return out;
    }
};

struct WhileStatement : Statement
{
    Token while_key;
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> loop;

    std::string toString() override
    {
        return "While : " + condition->toString() + loop->toString();
    }

    WhileStatement *shallowClone() const override
    {
        return new WhileStatement(
            while_key,
            clonePtr(condition),
            clonePtr(loop));
    }

    WhileStatement(Token while_k, std::unique_ptr<Expression> condition, std::unique_ptr<Statement> l) : Statement(while_k), while_key(while_k), condition(std::move(condition)), loop(std::move(l)) {};
};

// Function Statement
struct FunctionStatement : Statement
{
    Token token;
    std::unique_ptr<Expression> funcExpr;
    std::string toString() override
    {
        return "Function Statement: " + funcExpr->toString();
    }

    FunctionStatement *shallowClone() const override
    {
        return new FunctionStatement(
            token,
            clonePtr(funcExpr));
    }

    FunctionStatement(Token funcStmtTok, std::unique_ptr<Expression> expr) : Statement(funcStmtTok), funcExpr(std::move(expr)) {};
};

// Function declaration statement
struct FunctionDeclaration : Statement
{
    bool isExportable;
    Token func_keyword_token;
    std::unique_ptr<Expression> function_name;
    std::vector<std::unique_ptr<Statement>> parameters;
    std::unique_ptr<Expression> return_type;
    bool isNullable = false;

    std::string toString() override
    {
        std::string exportStr = isExportable ? "export " : "";
        std::string arguments;
        for (auto &param : parameters)
        {
            arguments += param->toString();
        }

        return exportStr + "Function Declaration Statement: " + func_keyword_token.TokenLiteral + " " + (function_name ? function_name->toString() : "[null_name]") + " " + arguments + " " + (return_type ? return_type->toString() : "[no_return]");
    }

    FunctionDeclaration *shallowClone() const override
    {
        return new FunctionDeclaration(
            isExportable,
            func_keyword_token,
            clonePtr(function_name),
            clonePtrVector(parameters),
            clonePtr(return_type));
    }

    FunctionDeclaration(bool exportable, Token func, std::unique_ptr<Expression> identifier, std::vector<std::unique_ptr<Statement>> params, std::unique_ptr<Expression> ret_type) : Statement(func),
                                                                                                                                                                                     isExportable(exportable),
                                                                                                                                                                                     func_keyword_token(func),
                                                                                                                                                                                     function_name(std::move(identifier)),
                                                                                                                                                                                     parameters(std::move(params)),
                                                                                                                                                                                     return_type(std::move(ret_type)) {};
};

// Function Declaration expression
struct FunctionDeclarationExpression : Expression
{
    Token func_token;
    std::unique_ptr<Statement> funcDeclrStmt;
    std::string toString() override
    {
        return "Function Declaration Expression: " + funcDeclrStmt->toString();
    }

    FunctionDeclarationExpression *shallowClone() const override
    {
        return new FunctionDeclarationExpression(
            func_token,
            clonePtr(funcDeclrStmt));
    }

    FunctionDeclarationExpression(Token func, std::unique_ptr<Statement> declaration) : Expression(func), funcDeclrStmt(std::move(declaration)) {};
};

// Block statement
struct BlockStatement : Statement
{
    Token brace;
    std::vector<std::unique_ptr<Statement>> statements;
    std::string toString() override
    {
        std::string out = "Block Statement: { ";
        for (const auto &s : statements)
        {
            if (s)
            {
                out += s->toString();
            }
        }
        out += " }";
        return out;
    }

    BlockStatement *shallowClone() const override
    {
        return new BlockStatement(
            brace,
            clonePtrVector(statements));
    }

    BlockStatement(Token brac, std::vector<std::unique_ptr<Statement>> cont) : Statement(brac), brace(brac), statements(std::move(cont)) {};
};

// GENERICS
// Generic statement node
struct GenericStatement : Statement
{
    Token generic_token;
    std::unique_ptr<Expression> block_name;
    std::vector<Token> type_parameters;
    std::unique_ptr<Statement> block;

    std::string toString() override
    {
        std::string parameters;
        for (const auto &param : type_parameters)
        {
            parameters += param.TokenLiteral + ",";
        }

        if (!parameters.empty() && parameters.back() == ',')
        {
            parameters.pop_back();
        }

        std::string block_str = " { <blueprint stored> }";

        if (block)
        {
            block_str = block->toString();
        }
        return "Generic Statement: " + block_name->toString() + "(" + parameters + ")" + block_str;
    }

    GenericStatement(Token generic, std::unique_ptr<Expression> name, std::vector<Token> types, std::unique_ptr<Statement> content) : Statement(generic), generic_token(generic), block_name(std::move(name)), type_parameters(types), block(std::move(content)) {};
};

// Instatiate statement
struct InstantiateStatement : Statement
{
    Token instantiate_token;
    std::unique_ptr<Expression> generic_call;
    Token as_token;
    Token alias;

    std::string toString() override
    {
        return "Instantiate statement: " + generic_call->toString() + " as " + alias.TokenLiteral;
    }

    InstantiateStatement(Token inst, std::unique_ptr<Expression> call, Token as, Token name) : Statement(inst), instantiate_token(inst), generic_call(std::move(call)), as_token(as), alias(name) {};
};

// Generic call expression
struct GenericCall : Expression
{
    std::unique_ptr<Expression> ident;
    std::vector<Token> args;

    std::string toString() override
    {
        std::string arg;
        for (const auto &param : args)
        {
            arg += param.TokenLiteral + ",";
        };
        return "Generic Call:" + ident->toString() + "(" + arg + ")";
    };
    GenericCall(std::unique_ptr<Expression> name, std::vector<Token> types) : Expression(name->token), ident(std::move(name)), args(types) {};
};

// Array Literal
struct ArrayLiteral : Expression
{
    Token arr_token;
    std::vector<std::unique_ptr<Expression>> array;

    std::string toString() override
    {
        std::string items;
        for (const auto &item : array)
        {
            items += item->toString() + ",";
        }
        if (!items.empty())
            items.pop_back(); // remove last comma
        return "ArrayLiteral: [" + items + "]";
    }

    ArrayLiteral *shallowClone() const override
    {
        return new ArrayLiteral(
            arr_token,
            clonePtrVector(array));
    }

    ArrayLiteral(Token arr_tok, std::vector<std::unique_ptr<Expression>> arr)
        : Expression(arr_tok), arr_token(arr_tok), array(std::move(arr)) {};
};

// Array statement
struct ArrayStatement : Statement
{
    Mutability mutability;
    bool isHeap;
    std::unique_ptr<Expression> arrayType;            // Like arr[int]
    std::vector<std::unique_ptr<Expression>> lengths; // Stuff like [1] or [2][3] it is optional though
    std::unique_ptr<Expression> identifier;
    std::unique_ptr<Expression> array_content;

    std::string toString() override
    {
        std::string heapStr = isHeap ? "heap " : "";
        std::string lenStr = "";
        if (!lengths.empty())
        {
            for (const auto &len : lengths)
            {
                lenStr += "[" + len->toString() + "]";
            }
        }
        std::string mutStr = "";
        if (mutability == Mutability::MUTABLE)
        {
            mutStr += "mut";
        }
        else if (mutability == Mutability::CONSTANT)
        {
            mutStr += "const";
        }

        std::string arrayTypeStr = "<no type>";
        if (arrayType)
            arrayTypeStr = arrayType->toString();

        std::string arrName = "<no name>";
        if (identifier)
        {
            arrName = identifier->toString();
        }

        std::string arrContent = "[]";
        if (array_content)
        {
            arrContent = array_content->toString();
        }

        return "Array Statement: " + heapStr + mutStr + " " + arrayTypeStr + " Lengths: " + lenStr + " " + arrName + " " + arrContent;
    }

    ArrayStatement *shallowClone() const
    {
        return new ArrayStatement(
            isHeap,
            mutability,
            clonePtr(arrayType),
            clonePtrVector(lengths),
            clonePtr(identifier),
            clonePtr(array_content));
    }

    ArrayStatement(
        bool heap,
        Mutability mut,
        std::unique_ptr<Expression> arrayTy,
        std::vector<std::unique_ptr<Expression>> lens,
        std::unique_ptr<Expression> ident,
        std::unique_ptr<Expression> array) : Statement(arrayTy->token), isHeap(heap), mutability(mut), arrayType(std::move(arrayTy)), lengths(std::move(lens)), identifier(std::move(ident)), array_content(std::move(array)) {};
};

// Array Subscript expression
struct ArraySubscript : Expression
{
    std::unique_ptr<Expression> identifier;
    std::vector<std::unique_ptr<Expression>> index_exprs;

    std::string toString() override
    {
        std::string indexStr;
        for (const auto &index : index_exprs)
        {
            indexStr += "[" + index->toString() + "]";
        }
        return "Array Subscript Expression: " + identifier->toString() + indexStr;
    }

    ArraySubscript *shallowClone() const override
    {
        return new ArraySubscript(
            clonePtr(identifier),
            clonePtrVector(index_exprs));
    }

    ArraySubscript(std::unique_ptr<Expression> ident, std::vector<std::unique_ptr<Expression>> ids) : Expression(ident->expression), identifier(std::move(ident)), index_exprs(std::move(ids)) {};
};

// Alias statement
struct AliasStatement : Statement
{
    Token alias_token;
    std::unique_ptr<Expression> aliasName;
    std::unique_ptr<Expression> aliasType;

    std::string toString() override
    {
        return "Alias statement: " + alias_token.TokenLiteral + " " + aliasName->toString() + " = " + aliasType->toString();
    }

    AliasStatement(Token alias, std::unique_ptr<Expression> name, std::unique_ptr<Expression> type) : Statement(alias), aliasName(std::move(name)), aliasType(std::move(type)) {}
};

// Qualify statement
struct QualifyStatement : Statement
{
    Token qualify_key;
    std::unique_ptr<Expression> expr;
    std::string toString() override
    {
        return "Qualify Statement: " + qualify_key.TokenLiteral + " " + expr->toString();
    }

    QualifyStatement(Token qualify, std::unique_ptr<Expression> main) : Statement(qualify), qualify_key(qualify), expr(std::move(main)) {};
};

// Merge statement
struct MergeStatement : Statement
{
    Token merge_key;
    std::unique_ptr<Expression> stringExpr;

    std::string toString() override
    {
        return "Merge Statement: " + merge_key.TokenLiteral + " " + stringExpr->toString();
    }

    MergeStatement(Token merge, std::unique_ptr<Expression> string) : Statement(merge), merge_key(merge), stringExpr(std::move(string)) {};
};

// Import statement
struct ImportStatement : Statement
{
    Token import_key;
    std::unique_ptr<Expression> stringExpr;

    std::string toString() override
    {
        return "Import Statement: " + import_key.TokenLiteral + " " + stringExpr->toString();
    }

    ImportStatement(Token import, std::unique_ptr<Expression> string) : Statement(import), import_key(import), stringExpr(std::move(string)) {};
};

// Link statement
struct LinkStatement : Statement
{
    Token link_key;
    std::unique_ptr<Expression> stringExpr;

    std::string toString() override
    {
        return "Link Statement: " + link_key.TokenLiteral + " " + stringExpr->toString();
    }

    LinkStatement(Token link, std::unique_ptr<Expression> string) : Statement(link), link_key(link), stringExpr(std::move(string)) {};
};

// Shout statement
struct ShoutStatement : Statement
{
    Token shout_key;
    std::unique_ptr<Expression> expr;

    std::string toString() override
    {
        return "Shout Statement: " + shout_key.TokenLiteral + "! " + expr->toString();
    }

    ShoutStatement *shallowClone() const override
    {
        return new ShoutStatement(
            shout_key,
            clonePtr(expr));
    }

    ShoutStatement(Token shout, std::unique_ptr<Expression> expression) : Statement(shout), shout_key(shout), expr(std::move(expression)) {};
};

// BLOCKS
//  Block expression
struct BlockExpression : Expression
{
    Token brace;
    std::vector<std::unique_ptr<Statement>> statements;
    std::optional<std::unique_ptr<Expression>> finalexpr;

    std::string toString() override
    {
        std::string out = "BlockExpression: { \n";
        for (auto &stmt : statements)
        {
            out += stmt->toString() + "\n";
        }
        if (finalexpr.has_value() && finalexpr.value())
        {
            out += "Final Expression: " + finalexpr.value()->toString();
        }
        return out + "}";
    };

    BlockExpression *shallowClone() const override
    {
        auto b = new BlockExpression(brace);
        b->statements = clonePtrVector(statements);
        if (finalexpr.has_value())
        {
            b->finalexpr = clonePtr(finalexpr.value());
        }
        return b;
    }

    BlockExpression(Token lbrace) : Expression(lbrace) {};
};

enum class Precedence
{
    PREC_NONE = 0,
    PREC_ASSIGNMENT, // =
    PREC_COALESCE,   //"??"
    PREC_OR,         // ||
    PREC_AND,        // &&
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // "* /"
    PREC_UNARY,      // "! -"
    PREC_POSTFIX,
    PREC_CALL, // . ()
    PREC_PRIMARY
};