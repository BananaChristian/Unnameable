#pragma once
#include "token/token.hpp"
#include <vector>
using namespace std;

class Lexer
{
    int currentPosition;
    int nextPosition;
    string input;
    unordered_map<string, TokenType> keywords = {
        {"auto", TokenType::AUTO},
        {"work", TokenType::FUNCTION},
        {"return", TokenType::RETURN},
        {"fixed", TokenType::CONSTANT},
        {"cast", TokenType::CAST},
        {"class", TokenType::CLASS},
        {"self", TokenType::SELF},
        {"public", TokenType::PUBLIC},
        {"private", TokenType::PRIVATE},

        {"if", TokenType::IF},
        {"else", TokenType::ELSE},
        {"elseif", TokenType::ELSE_IF},
        {"while", TokenType::WHILE},
        {"for", TokenType::FOR},
        {"break", TokenType::BREAK},
        {"continue", TokenType::CONTINUE},
        {"switch", TokenType::SWITCH},
        {"case", TokenType::CASE},
        {"default", TokenType::DEFAULT},

        {"int", TokenType::INT},
        {"string", TokenType::STRING_KEYWORD},
        {"float", TokenType::FLOAT_KEYWORD},
        {"void", TokenType::VOID},
        {"char", TokenType::CHAR_KEYWORD},
        {"true", TokenType::TRUE},
        {"false", TokenType::FALSE},
        {"bool",TokenType::BOOL_KEYWORD},
        {"arr", TokenType::ARRAY},

        {"zone", TokenType::ZONE},
        {"unique", TokenType::UNIQUE},
        {"make", TokenType::MAKE},
        {"signal",TokenType::SIGNAL},
        {"start",TokenType::START},
        {"wait",TokenType::WAIT},
        {"unsafe", TokenType::UNSAFE},
        {"alloc", TokenType::ALLOCATE},
        {"gc", TokenType::GC},
        {"free", TokenType::DROP},
        {"elevate", TokenType::ELEVATE},
        {"write", TokenType::WRITE},
        {"pointer", TokenType::POINTER},
        {"read", TokenType::READ},
    };

public:
    Lexer(string &input);
    Token tokenize();
    vector<Token> outputTokens;

    vector<Token> token_list;
    void updateTokenList();

private:
    void advance();
    void skipWhiteSpace();
    char peekChar();
    char currentChar();
    bool isDigit();
    bool isLetter(char character);
    void readComments();
    Token readNumbers();
    Token readIdentifiers();
    Token readString();
    Token readChar();
};