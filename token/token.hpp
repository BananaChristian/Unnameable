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
    BITWISE_AND,//&
    BITWISE_OR,//&

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
    FLOAT_KEYWORD,
    DOUBLE_KEYWORD,
    INTEGER,
    CHAR_KEYWORD,
    BOOL_KEYWORD,

    //Keywords
    FUNCTION,
    AUTO,
    RETURN,
    CONSTANT,
    CAST,
    COMPONENT,//component key word for classes
    SELF,//self keyword for a component instance
    DATA,//data keyword token for class 
    BEHAVIOR,//behavior keyword token for class method blocks
    USE,//use keyword that allows use of external data and behavior blocks
    INIT,//constructor keyword
    START,//start
    WAIT,//wait
    SIGNAL,//signal
    ERROR,//error
    NEW,//new keyword for a new instance of a component


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
    FLOAT,
    TRUE,
    FALSE,
    DOUBLE,
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
    int line;
    int column;
};

std::string TokenTypeToLiteral(TokenType type);