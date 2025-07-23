#pragma once
#include "token/token.hpp"
#include <string>
#include <unordered_map>
#include <vector>

class Lexer
{
    size_t currentPosition;
    size_t nextPosition;
    std::string input;
    int line = 1;
    int column = 0;

    std::unordered_map<std::string, TokenType> keywords = {
        {"auto", TokenType::AUTO},
        {"work", TokenType::FUNCTION},
        {"return", TokenType::RETURN},
        {"cast", TokenType::CAST},

        {"enum", TokenType::ENUM},
        {"class", TokenType::CLASS},

        {"component", TokenType::COMPONENT},
        {"self", TokenType::SELF},
        {"new", TokenType::NEW},
        {"data", TokenType::DATA},
        {"behavior", TokenType::BEHAVIOR},
        {"use", TokenType::USE},
        {"init", TokenType::INIT},

        {"if", TokenType::IF},
        {"else", TokenType::ELSE},
        {"elif", TokenType::ELSE_IF},
        {"while", TokenType::WHILE},
        {"for", TokenType::FOR},
        {"break", TokenType::BREAK},
        {"continue", TokenType::CONTINUE},

        {"switch", TokenType::SWITCH},
        {"case", TokenType::CASE},
        {"default", TokenType::DEFAULT},

        {"null", TokenType::NULLABLE},

        {"int", TokenType::INT},
        {"string", TokenType::STRING_KEYWORD},
        {"float", TokenType::FLOAT_KEYWORD},
        {"double", TokenType::DOUBLE_KEYWORD},
        {"void", TokenType::VOID},
        {"char", TokenType::CHAR_KEYWORD},
        {"true", TokenType::TRUE},
        {"false", TokenType::FALSE},
        {"bool", TokenType::BOOL_KEYWORD},
        {"arr", TokenType::ARRAY},

        {"const", TokenType::CONST},
        {"mut", TokenType::MUT},

        {"unique", TokenType::UNIQUE},
        {"make", TokenType::MAKE},
        {"signal", TokenType::SIGNAL},
        {"start", TokenType::START},
        {"error", TokenType::ERROR},
        {"wait", TokenType::WAIT},
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
    Lexer(const std::string &sourceCode);
    Token tokenize();
    std::vector<Token> outputTokens;

    std::vector<Token> token_list;
    void updateTokenList();

private:
    size_t getUTF8CharLength(size_t pos);
    char32_t decodeUTF8(size_t pos);
    void advance();
    void skipWhiteSpace();
    char32_t peekChar();
    char32_t currentChar();
    bool isDigit(char32_t ch);
    void readComments();
    Token readNumbers();
    char convertUnicodeDigit(char32_t ch);
    Token readIdentifiers();
    bool isIdentifierStart(char32_t ch);
    bool isIdentifierContinue(char32_t ch);
    Token readString();
    void appendUTF8(std::string &str, char32_t ch);
    Token readChar();
    void logError(const std::string &message, int line, int column);
};