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
        {"func", TokenType::FUNCTION},
        {"return", TokenType::RETURN},

        {"enum", TokenType::ENUM},
        {"class", TokenType::CLASS},

        {"component", TokenType::COMPONENT},
        {"self", TokenType::SELF},
        {"new", TokenType::NEW},
        {"data", TokenType::DATA},
        {"behavior", TokenType::BEHAVIOR},
        {"use", TokenType::USE},
        {"init", TokenType::INIT},

        {"generic", TokenType::GENERIC},
        {"instantiate", TokenType::INSTANTIATE},
        {"as", TokenType::AS},
        {"alias", TokenType::ALIAS},

        {"if", TokenType::IF},
        {"else", TokenType::ELSE},
        {"elif", TokenType::ELSE_IF},
        {"while", TokenType::WHILE},
        {"for", TokenType::FOR},
        {"each", TokenType::EACH},
        {"break", TokenType::BREAK},
        {"continue", TokenType::CONTINUE},

        {"switch", TokenType::SWITCH},
        {"case", TokenType::CASE},
        {"default", TokenType::DEFAULT},

        {"null", TokenType::NULLABLE},

        {"short", TokenType::SHORT_KEYWORD},
        {"ushort", TokenType::USHORT_KEYWORD},

        {"int", TokenType::INTEGER_KEYWORD},
        {"uint", TokenType::UINT_KEYWORD},

        {"long", TokenType::LONG_KEYWORD},
        {"ulong", TokenType::ULONG_KEYWORD},

        {"extra", TokenType::EXTRA_KEYWORD},
        {"uextra", TokenType::UEXTRA_KEYWORD},

        {"char", TokenType::CHAR_KEYWORD},
        {"char16", TokenType::CHAR16_KEYWORD},
        {"char32", TokenType::CHAR32_KEYWORD},

        {"string", TokenType::STRING_KEYWORD},
        {"float", TokenType::FLOAT_KEYWORD},
        {"double", TokenType::DOUBLE_KEYWORD},
        {"void", TokenType::VOID},

        {"true", TokenType::TRUE},
        {"false", TokenType::FALSE},
        {"bool", TokenType::BOOL_KEYWORD},
        {"arr", TokenType::ARRAY},

        {"const", TokenType::CONST},
        {"mut", TokenType::MUT},

        {"heap", TokenType::HEAP},
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