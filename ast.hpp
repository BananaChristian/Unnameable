#pragma once
#include "token/token.hpp"
#include <memory>
#include <string>
#include <optional>
using namespace std;

// GENERAL AST NODE
struct Node
{
    Token token;
    virtual string toString()
    {
        return "Node: " + token.TokenLiteral;
    };
};

// GENERAL EXPRESSION NODE
struct Expression : Node
{
    Token expression;
    string toString() override
    {
        return "Expression: " + expression.TokenLiteral;
    }
    Expression(Token expr) : expression(expr) {};
};

// Identifier statement node
struct Identifier : Expression
{
    Token identifier;
    string toString() override
    {
        return "Identifier Expression: " + identifier.TokenLiteral;
    }
    Identifier(Token ident) : Expression(ident), identifier(ident) {};
};

// Integer literal
struct IntegerLiteral : Expression
{
    Token int_token;
    string toString() override
    {
        return "Integer Literal: " + int_token.TokenLiteral;
    }
    IntegerLiteral(Token int_t) : Expression(int_t), int_token(int_t) {};
};

// Boolean literal
struct BooleanLiteral : Expression
{
    Token boolean_token;
    string toString() override
    {
        return "Boolean Literal: " + boolean_token.TokenLiteral;
    }
    BooleanLiteral(Token bool_t) : Expression(bool_t), boolean_token(bool_t) {};
};

// Float literal
struct FloatLiteral : Expression
{
    Token float_token;
    string toString() override
    {
        return "Float Literal: " + float_token.TokenLiteral;
    }
    FloatLiteral(Token float_t) : Expression(float_t), float_token(float_t) {};
};

// Char literal
struct CharLiteral : Expression
{
    Token char_token;
    string toString() override
    {
        return "Char Literal: " + char_token.TokenLiteral;
    }
    CharLiteral(Token char_t) : Expression(char_t), char_token(char_t) {};
};

// String literal
struct StringLiteral : Expression
{
    Token string_token;
    string toString() override
    {
        return "String Literal: " + string_token.TokenLiteral;
    }
    StringLiteral(Token string_t) : Expression(string_t), string_token(string_t) {};
};

// Prefix expression node for syntax like !true;
struct PrefixExpression : Expression
{
    Token operat;
    unique_ptr<Expression> operand;
    string toString() override
    {
        return "Prefix Expression: " + operat.TokenLiteral;
    }
    PrefixExpression(Token opr, unique_ptr<Expression> oprand) : Expression(operat), operand(move(oprand)) {};
};

// Infix Expression node for syntax like x+y;
struct InfixExpression : Expression
{
    unique_ptr<Expression> left_operand;
    Token operat;
    unique_ptr<Expression> right_operand;
    string toString() override
    {
        return "Infix Expression: (" + left_operand->toString() + " " + operat.TokenLiteral + " " + right_operand->toString() + ")";
    }
    InfixExpression(unique_ptr<Expression> left, Token op, unique_ptr<Expression> right) : Expression(op), left_operand(move(left)), operat(op), right_operand(move(right)) {};
};

//-----STATEMENTS----
// GENERAL STATEMENT NODE
struct Statement : Node
{
    Token statement;
    string toString() override
    {
        return "Statement: " + statement.TokenLiteral;
    }
    Statement(Token stmt) : statement(stmt) {};
};

// Let statement node
struct LetStatement : Statement
{
    Token data_type_token;
    Token ident_token;
    optional<Token> assign_token;
    unique_ptr<Expression> value;
    string toString() override
    {
        string result = "Let Statement: ( Data type: " + data_type_token.TokenLiteral +
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

    LetStatement(Token data_t, Token ident_t, optional<Token> assign_t, unique_ptr<Expression> val) : data_type_token(data_t), ident_token(ident_t), assign_token(assign_t), Statement(data_t), value(move(val)) {};
};

struct LetStatementNoType: Statement{
    Token ident_token;
    unique_ptr<Expression> value;
    string toString() override{
        return "Let Statement no data type: (Variable: " + ident_token.TokenLiteral+ " Value: "+ value->toString()+")";
    };
    LetStatementNoType(Token ident,unique_ptr<Expression> val):Statement(ident), ident_token(ident),value(move(val)){};
};

// Return statement node
struct ReturnStatement : Statement
{
    Token return_stmt;
    unique_ptr<Expression> return_value;
    string toString() override
    {
        return "Return Statement: ( Token: " + return_stmt.TokenLiteral + " Value: " +
               (return_value ? return_value->toString() : "void") + ")";
    }
    ReturnStatement(Token ret, unique_ptr<Expression> ret_val) : Statement(ret), return_stmt(ret), return_value(move(ret_val)) {};
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