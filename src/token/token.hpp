#pragma once
#include <string>
#include <unordered_map>

enum class TokenType
{
    // Aritmetic Operators
    ASSIGN,      //=
    PLUS,        //+
    PLUS_PLUS,   //++
    MINUS,       //-
    MINUS_MINUS, //--
    ASTERISK,    // x
    DIVIDE,      ///
    MODULUS,     //"%"

    ARROW, //->
    AT,    //@

    // Logical Operators
    EQUALS,       //==
    NOT_EQUALS,   //"!="
    AND,          //&&
    OR,           //||
    GREATER_THAN, //>
    LESS_THAN,    //<
    GT_OR_EQ,     //>=
    LT_OR_EQ,     //<=
    SHIFT_RIGHT,  //>>
    SHIFT_LEFT,   //<<
    BITWISE_AND,  //&
    BITWISE_OR,   //&

    // Delimiters
    LBRACE,   //{
    RBRACE,   //}
    LBRACKET, //[
    RBRACKET, //]
    LPAREN,   //(
    RPAREN,   //)

    // Punctuation
    SEMICOLON,      //;
    COLON,          //:
    FULLSTOP,       //.
    COMMA,          //,
    BANG,           //"!"
    SCOPE_OPERATOR, //"::"
    QUESTION_MARK,  //"?"
    COALESCE,       //"??"

    IDENTIFIER,
    STRING_KEYWORD,
    FLOAT_KEYWORD,
    DOUBLE_KEYWORD,
    BOOL_KEYWORD,

    // Keywords
    FUNCTION,
    AUTO,
    RETURN,
    CAST,

    ENUM,
    CLASS,

    HEAP,      // Heap keyword that allows a user to manually tell the compiler to allocate on the SAGE heap
    DHEAP,     //Dheap keword that allows a user to use a dynamic heap
    ALLOCATOR,//Allocator keyword

    COMPONENT, // component key word for classes
    SELF,      // self keyword for a component instance
    DATA,      // data keyword token for class
    BEHAVIOR,  // behavior keyword token for class method blocks
    USE,       // use keyword that allows use of external data and behavior blocks
    INIT,      // constructor keyword

    GENERIC, // Generic block keyword
    INSTANTIATE,
    AS,
    ALIAS,

    START,  // start
    WAIT,   // wait
    SIGNAL, // signal
    ERROR,  // error
    NEW,    // new keyword for a new instance of a component
    CONST,  // constant keyword
    MUT,    // mutable keyword
    REF,    // reference keyword
    PTR,    // Pointer keyword
    DEREF,  // Dereference keyword
    ADDR,   // addr keyword
    UNWRAP, // Unwrap keyword

    EXPORT,//Export keyword

    MODULE,  // module keyword
    MERGE,   // Merge keyword
    LINK,    // Link keyword
    QUALIFY, // Qualify keyword

    SCOPE, // Scope keyword

    SHOUT, // Shout keyword

    // Control flow
    IF,
    ELSE,
    ELSE_IF,
    WHILE,
    FOR,
    EACH,
    BREAK,
    CONTINUE,
    SWITCH,
    CASE,
    DEFAULT,

    // Data types
    SHORT, // Signed int 16 bit
    SHORT_KEYWORD,
    USHORT, // Unsigned int 16 bit
    USHORT_KEYWORD,

    INT, // Signed int 32 bit
    INTEGER_KEYWORD,
    UINT, // Unsingned int 32 bit
    UINT_KEYWORD,

    LONG, // Signed int 64 bit
    LONG_KEYWORD,
    ULONG, // Unsigned int 64 bit
    ULONG_KEYWORD,

    EXTRA, // Signed int 128 bit
    EXTRA_KEYWORD,
    UEXTRA, // Unsigned int 128 bit
    UEXTRA_KEYWORD,

    STRING,

    CHAR, // 8 bit char
    CHAR_KEYWORD,
    CHAR16, // 16 bit char
    CHAR16_KEYWORD,
    CHAR32, // 32 bit char
    CHAR32_KEYWORD,

    VOID,
    FLOAT,
    TRUE,
    FALSE,
    DOUBLE,
    ARRAY,

    NULLABLE, //"null"

    ILLEGAL,
    END,

};

std::string TokenTypeToLiteral(TokenType type);

struct Token
{
    std::string TokenLiteral{};
    TokenType type{TokenType::ILLEGAL};
    int line{0};
    int column{0};

    Token() = default;

    Token(const std::string &literal, TokenType t, int l, int c)
        : TokenLiteral(literal), type(t), line(l), column(c)
    {
    }
};
