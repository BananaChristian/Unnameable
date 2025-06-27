#pragma once
#include "token/token.hpp"
#include <memory>
#include <string>
#include <vector>
#include <optional>

// GENERAL AST NODE
struct Node
{
    Token token;
    virtual std::string toString()
    {
        return "Node: " + token.TokenLiteral;
    };
};

// GENERAL EXPRESSION NODE
struct Expression : Node
{
    Token expression;
    std::string toString() override
    {
        return "Expression: " + expression.TokenLiteral;
    }
    Expression(Token expr) : expression(expr) {};
};

// GENERAL STATEMENT NODE
struct Statement : Node
{
    Token statement;
    std::string toString() override
    {
        return "Statement: " + statement.TokenLiteral;
    }
    Statement(Token stmt) : statement(stmt) {};
};

// Identifier statement node
struct Identifier : Expression
{
    Token identifier;
    std::string toString() override
    {
        return "Identifier Expression: " + identifier.TokenLiteral;
    }
    Identifier(Token ident) : Expression(ident), identifier(ident) {};
};

// Integer literal
struct IntegerLiteral : Expression
{
    Token int_token;
    std::string toString() override
    {
        return "Integer Literal: " + int_token.TokenLiteral;
    }
    IntegerLiteral(Token int_t) : Expression(int_t), int_token(int_t) {};
};

// Boolean literal
struct BooleanLiteral : Expression
{
    Token boolean_token;
    std::string toString() override
    {
        return "Boolean Literal: " + boolean_token.TokenLiteral;
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
    FloatLiteral(Token float_t) : Expression(float_t), float_token(float_t) {};
};

// Char literal
struct CharLiteral : Expression
{
    Token char_token;
    std::string toString() override
    {
        return "Char Literal: " + char_token.TokenLiteral;
    }
    CharLiteral(Token char_t) : Expression(char_t), char_token(char_t) {};
};

// String literal
struct StringLiteral : Expression
{
    Token string_token;
    std::string toString() override
    {
        return "String Literal: " + string_token.TokenLiteral;
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

    CallExpression(Token tok, std::unique_ptr<Expression> fn_ident, std::vector<std::unique_ptr<Expression>> params) : Expression(tok), function_identifier(std::move(fn_ident)), parameters(std::move(params)) {};
};

// Function expression struct node
struct FunctionExpression : Expression
{
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

        return "FunctionExpression: " + func_key.TokenLiteral + " " +
               "Function parameters: " + cl +
               " Return type: " + ret_str +
               " Function block: " + block_str;
    }

    FunctionExpression(Token fn, std::vector<std::unique_ptr<Statement>> c, std::unique_ptr<Expression> return_t, std::unique_ptr<Expression> bl) : Expression(fn), call(std::move(c)), return_type(std::move(return_t)), block(std::move(bl)) {};
};

// Return type expression
struct ReturnTypeExpression : Expression
{
    Token typeToken;
    std::string toString() override
    {
        return "Type expression: " + typeToken.TokenLiteral;
    }
    ReturnTypeExpression(Token type) : Expression(type), typeToken(type) {};
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
    InfixExpression(std::unique_ptr<Expression> left, Token op, std::unique_ptr<Expression> right) : Expression(op), left_operand(move(left)), operat(op), right_operand(move(right)) {};
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
            return expression->toString() + ";";
        }
        return ";";
    }
    ExpressionStatement(Token exp, std::unique_ptr<Expression> expr) : Statement(exp), expr(exp), expression(move(expr)) {};
};

// Break statement node
struct BreakStatement : Statement
{
    Token break_tok;
    std::string toString()
    {
        return "Break Statement: " + break_tok.TokenLiteral;
    }
    BreakStatement(Token break_t) : Statement(break_t), break_tok(break_t) {};
};

// Continue statement struct
struct ContinueStatement : Statement
{
    Token cont_tok;
    std::string toString()
    {
        return "Continue Statement: " + cont_tok.TokenLiteral;
    }
    ContinueStatement(Token cont_t) : Statement(cont_t), cont_tok(cont_t) {};
};

// Let statement node
struct LetStatement : Statement
{
    bool isFixed = false;
    Token data_type_token;
    Token ident_token;
    std::optional<Token> assign_token;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        std::string isConst = isFixed ? "fixed" : "";
        std::string result = "Let Statement: (" + isConst + " Data Type:" + data_type_token.TokenLiteral +
                             " Variable name: " + ident_token.TokenLiteral;

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

    LetStatement(Token data_t, Token ident_t, std::optional<Token> assign_t, std::unique_ptr<Expression> val, bool fixed = false) : isFixed(fixed), data_type_token(data_t), ident_token(ident_t), assign_token(assign_t), Statement(data_t), value(move(val)) {};
};

struct LetStatementNoType : Statement
{
    Token ident_token;
    std::unique_ptr<Expression> value;
    std::string toString() override
    {
        return "Let Statement no data type: (Variable: " + ident_token.TokenLiteral + " Value: " + value->toString() + ")";
    };
    LetStatementNoType(Token ident, std::unique_ptr<Expression> val) : Statement(ident), ident_token(ident), value(move(val)) {};
};

// Return statement node
struct ReturnStatement : Statement
{
    Token return_stmt;
    std::unique_ptr<Expression> return_value;
    std::string toString() override
    {
        return "Return Statement: ( Token: " + return_stmt.TokenLiteral + " Value: " +
               (return_value ? return_value->toString() : "void") + ")";
    }
    ReturnStatement(Token ret, std::unique_ptr<Expression> ret_val) : Statement(ret), return_stmt(ret), return_value(move(ret_val)) {};
};

// If statement node
struct ifStatement : Statement
{
    Token if_stmt;
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> if_result;

    std::optional<Token> elseif_stmt;
    std::optional<std::unique_ptr<Expression>> elseif_condition;
    std::optional<std::unique_ptr<Statement>> elseif_result;

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

        if (elseif_stmt.has_value())
        {
            result += "  else if (";

            if (elseif_condition.has_value() && *elseif_condition)
                result += (*elseif_condition)->toString();
            else
                result += "<null elseif_condition>";

            result += ") {\n";

            if (elseif_result.has_value() && *elseif_result)
                result += "    " + (*elseif_result)->toString() + "\n";
            else
                result += "    <null elseif_result>\n";

            result += "  }\n";
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

    ifStatement(Token if_st, std::unique_ptr<Expression> condition_e, std::unique_ptr<Statement> if_r,
                std::optional<Token> elseif_st, std::optional<std::unique_ptr<Expression>> elseif_cond, std::optional<std::unique_ptr<Statement>> elseif_r,
                std::optional<Token> else_st, std::optional<std::unique_ptr<Statement>> else_r) : Statement(if_st),
                                                                                                  if_stmt(if_st), condition(move(condition_e)), if_result(move(if_r)),
                                                                                                  elseif_stmt(elseif_st), elseif_condition(move(elseif_cond)), elseif_result(move(elseif_r)),
                                                                                                  else_stmt(else_st), else_result(move(else_r)) {};
};

struct ForStatement : Statement
{
    Token for_key;
    std::unique_ptr<Statement> initializer; // int i;
    std::unique_ptr<Expression> condition;  // i < 10
    std::unique_ptr<Expression> step;       // i = i + 1
    std::unique_ptr<Statement> body;        // the loop body

    ForStatement(Token for_k,
                 std::unique_ptr<Statement> init,
                 std::unique_ptr<Expression> cond,
                 std::unique_ptr<Expression> step,
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

struct WhileStatement : Statement
{
    Token while_key;
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> loop;

    std::string toString() override
    {
        return "While : " + condition->toString() + loop->toString();
    }

    WhileStatement(Token while_k, std::unique_ptr<Expression> condition, std::unique_ptr<Statement> l) : Statement(while_k), while_key(while_k), condition(move(condition)), loop(move(l)) {};
};

// Function statement node
struct FunctionStatement : Statement
{
    Token data_type;                               // The function's return data type
    std::unique_ptr<Expression> ident;             // The functions name
    std::vector<std::unique_ptr<Expression>> args; // The functions parameters

    std::string toString() override
    {
        std::string results;
        std::string arguments;
        for (auto &param : args)
        {
            arguments += param->toString();
        }
        results = "Function Statement: " + data_type.TokenLiteral + " " + ident->toString() + " " + "(" + arguments + ")";
        return results;
    };

    FunctionStatement(Token data, std::unique_ptr<Expression> id, std::vector<std::unique_ptr<Expression>> params) : Statement(data), data_type(data), ident(move(id)), args(move(params)) {};
};

// Block statement
struct BlockStatement : Statement
{
    Token brace;
    std::vector<std::unique_ptr<Statement>> statements;
    std::string toString() override
    {
        std::string out = "{ ";
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
    BlockStatement(Token brac, std::vector<std::unique_ptr<Statement>> cont) : Statement(brace), brace(brac), statements(move(cont)) {}
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
        std::string out = "BlockExpression:\n";
        for (auto &stmt : statements)
        {
            out += stmt->toString() + "\n";
        }
        if (finalexpr.has_value() && finalexpr.value())
        {
            out += "Final Expression: " + finalexpr.value()->toString();
        }
        return out;
    };

    BlockExpression(Token lbrace) : Expression(lbrace) {};
};

enum class Precedence
{
    PREC_NONE = 0,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // ||
    PREC_AND,        // &&
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // "* /"
    PREC_UNARY,      // "! -"
    PREC_CALL,       // . ()
    PREC_PRIMARY
};