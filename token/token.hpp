#pragma once
#include <string>
#include <unordered_map>

enum class TokenType{
    //Aritmetic Operators
    ASSIGN,//=
    PLUS,//+
    PLUS_PLUS,//++
    MINUS,//-
    MINUS_MINUS,//--
    ASTERISK,//x
    DIVIDE,///
    MODULUS,//"%"

    //Logical Operators
    EQUALS,//==
    NOT_EQUALS,//"!="
    AND,//&&
    OR,//||
    GREATER_THAN,//>
    LESS_THAN,//<
    GT_OR_EQ,//>=
    LT_OR_EQ,//<=
    SHIFT_RIGHT,//>>
    SHIFT_LEFT,//<<

    //Delimiters
    LBRACE,//{
    RBRACE,//}
    LBRACKET,//[
    RBRACKET,//]
    LPAREN,//(
    RPAREN,//)

    //Punctuation
    SEMICOLON,//;
    COLON,//:
    FULLSTOP,//.
    COMMA,//,
    BANG,//"!"


    IDENTIFIER,
    STRING_KEYWORD,
    FLOAT,
    FLOAT_KEYWORD,
    INTEGER,
    CHAR_KEYWORD,
    BOOL_KEYWORD,

    //Keywords
    FUNCTION,
    AUTO,
    RETURN,
    CONSTANT,
    CAST,
    CLASS,
    SELF,
    PUBLIC,
    PRIVATE,

    //Control flow
    IF,
    ELSE,
    ELSE_IF,
    WHILE,
    FOR,
    BREAK,
    CONTINUE,
    SWITCH,
    CASE,
    DEFAULT,

    //Data types
    INT,
    STRING,
    CHAR,
    VOID,
    TRUE,
    FALSE,
    ARRAY,

    //Memory management keywords
    ZONE,
    UNIQUE,
    MAKE,
    UNSAFE,
    ALLOCATE,
    GC,
    DROP,
    ELEVATE,
    WRITE,
    POINTER,
    READ,

    ILLEGAL,
    END,

};

struct Token{
    std::string TokenLiteral;
    TokenType type;
};

std::string TokenTypeToLiteral(TokenType type);