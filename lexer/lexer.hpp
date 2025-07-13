#pragma once
#include "token/token.hpp"
#include <string>
#include <unordered_map>
#include <vector>

class Lexer
{
    int currentPosition;
    int nextPosition;
    std::string input;
    int line=1;
    int column=0;

    std::unordered_map<std::string, TokenType> keywords = {
        {"auto", TokenType::AUTO},
        {"work", TokenType::FUNCTION},
        {"return", TokenType::RETURN},
        {"fixed", TokenType::CONSTANT},
        {"cast", TokenType::CAST},
        {"component", TokenType::COMPONENT},
        {"self", TokenType::SELF},
        {"new",TokenType::NEW},
        {"data", TokenType::DATA},
        {"behavior", TokenType::BEHAVIOR},
        {"use",TokenType::USE},
        {"init",TokenType::INIT},

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
        {"double",TokenType::DOUBLE_KEYWORD},
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
        {"error",TokenType::ERROR},
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
    Lexer(const std::string& sourceCode);
    Token tokenize();
    std::vector<Token> outputTokens;

    std::vector<Token> token_list;
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
    void logError(const std::string& message,int line,int column);
};