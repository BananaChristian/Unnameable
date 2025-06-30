#include "token/token.hpp"
#include "lexer.hpp"
#include <iostream>
#include <unordered_map>
#include <string>
using namespace std;

#define CAPTURE_POS       \
    int tokenLine = line; \
    int tokenColumn = column;

Lexer::Lexer(const string &sourceCode) : input(sourceCode), currentPosition(0), nextPosition(1), line(1), column(0) {};

// Lexer advance function
void Lexer::advance()
{
    if (nextPosition < input.length())
    {
        currentPosition = nextPosition;
        if (input[currentPosition] == '\n')
        {
            line++;
            std::cout<<"LINE"<<line<<"\n";
            column = 0;
        }
        else
        {
            std::cout<<"COLUMN"<<column<<"\n";
            column++;
        }
        nextPosition++;
    }
    else
    {
        currentPosition = input.length();
    }
}

char Lexer::peekChar()
{
    if (nextPosition >= input.length())
    {
        return '\0';
    }
    char nextCharacter = input[nextPosition];
    return nextCharacter;
}

char Lexer::currentChar()
{
    char currentCharacter = input[currentPosition];
    return currentCharacter;
}

void Lexer::skipWhiteSpace()
{
    while (true)
    {
        while (currentChar() == ' ' || currentChar() == '\n' || currentChar() == '\t')
        {
            advance();
        }

        if (currentChar() == '#')
        {
            readComments();
            continue;
        }
        break;
    }
}

Token Lexer::readNumbers()
{
    std::string number;
    CAPTURE_POS;
    while (currentChar() >= '0' && currentChar() <= '9')
    {
        number += currentChar();
        advance();
        if (currentChar() == '.')
        {
            number += currentChar();
            advance();
            while (currentChar() >= '0' && currentChar() <= '9')
            {
                number += currentChar();
                advance();
            }
            return Token{number, TokenType::FLOAT, tokenLine, tokenColumn};
        }
    }
    return Token{number, TokenType::INTEGER, tokenLine, tokenColumn};
}

Token Lexer::readIdentifiers()
{
    std::string identifier;
    CAPTURE_POS;
    while (isLetter(currentChar()))
    {
        identifier += currentChar();
        advance();
    }
    TokenType type = keywords.count(identifier) ? keywords[identifier] : TokenType::IDENTIFIER;
    return Token{identifier, type, tokenLine, tokenColumn};
};

bool Lexer::isDigit()
{
    if (currentChar() >= '0' && currentChar() <= '9')
    {
        return true;
    }
    return false;
}

bool Lexer::isLetter(char character)
{
    if ((character >= 'a' && character <= 'z') || (character >= 'A' && character <= 'Z') || character == '_')
    {
        return true;
    }
    return false;
}

void Lexer::readComments()
{
    if (currentChar() == '#')
    {
        advance();
        while (currentChar() != '\0' && currentChar() != '\n')
        {
            advance();
        }
    }
}

Token Lexer::readString()
{
    std::string value;
    CAPTURE_POS;
    advance();

    while (currentChar() != '\0' && currentChar() != '\n')
    {
        if (currentChar() == '"')
        {
            advance();
            return Token{value, TokenType::STRING, tokenLine, tokenColumn};
        }

        if (currentChar() == '\\')
        {
            advance();
            switch (currentChar())
            {
            case 'n':
                value += '\n';
                break;
            case 't':
                value += '\t';
                break;
            case 'r':
                value += '\r';
                break;
            case '\\':
                value += '\\';
                break;
            case '\"':
                value += '\"';
                break;
            case '\'':
                value += '\'';
                break;
            case '0':
                value += '\0';
                break;
            default:
                logError("Invalid escape sequence", tokenLine, tokenColumn);
                return Token{"Invalid escape sequence", TokenType::ILLEGAL, tokenLine, tokenColumn};
            }

            advance();
        }
        else
        {
            value += currentChar();
        }
        advance();
    }
    logError("Unterminated string", tokenLine, tokenColumn);
    return Token{"Unterminated string", TokenType::ILLEGAL, tokenLine, tokenColumn};
}

Token Lexer::readChar()
{
    CAPTURE_POS;
    advance();

    if (currentChar() == '\\')
    {
        advance();
        char escaped = currentChar();
        char unescaped;

        switch (escaped)
        {
        case 'n':
            unescaped = '\n';
            break;
        case 't':
            unescaped = '\t';
            break;
        case 'r':
            unescaped = '\r';
            break;
        case '0':
            unescaped = '\0';
            break;
        case '\'':
            unescaped = '\'';
            break;
        case '\"':
            unescaped = '\"';
            break;
        case '\\':
            unescaped = '\\';
            break;
        default:
            logError("Invalid escape", tokenLine, tokenColumn);
            return Token{"Invalid escape", TokenType::ILLEGAL, tokenLine, tokenColumn};
        }

        advance();
        if (currentChar() != '\'')
        {
            logError("Missing closing quote", tokenLine, tokenColumn);
            return Token{"Missing closing quote", TokenType::ILLEGAL, tokenLine, tokenColumn};
        }

        advance();
        return Token{std::string(1, unescaped), TokenType::CHAR};
    }

    char value = currentChar();
    advance();

    if (currentChar() != '\'')
    {
        return Token{"Missing closing quote", TokenType::ILLEGAL};
    }

    advance();
    return Token{std::string(1, value), TokenType::CHAR};
}

Token Lexer::tokenize()
{
    skipWhiteSpace();
    char character = currentChar();

    if (isDigit())
    {
        return readNumbers();
    }
    else if (isLetter(character))
    {
        return readIdentifiers();
    }
    switch (character)
    {
    case '=':
    {
        CAPTURE_POS;
        if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{"==", TokenType::EQUALS, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"=", TokenType::ASSIGN, tokenLine, tokenColumn};
        }
    }
    case '!':
    {
        CAPTURE_POS;
        if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{"!=", TokenType::NOT_EQUALS, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"!", TokenType::BANG, tokenLine, tokenColumn};
        }
    }
    case '+':
    {
        CAPTURE_POS;
        if (peekChar() == '+')
        {
            advance();
            advance();
            return Token{"++", TokenType::PLUS_PLUS, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"+", TokenType::PLUS, tokenLine, tokenColumn};
        }
    }
    case '-':
    {
        CAPTURE_POS;
        if (peekChar() == '-')
        {
            advance();
            advance();
            return Token{"--", TokenType::MINUS_MINUS, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"-", TokenType::MINUS, tokenLine, tokenColumn};
        }
    }
    case '*':
    {
        CAPTURE_POS;
        advance();
        return Token{"*", TokenType::ASTERISK, tokenLine, tokenColumn};
    }
    case '/':
    {
        CAPTURE_POS;
        advance();
        return Token{"/", TokenType::DIVIDE, tokenLine, tokenColumn};
    }
    case '&':
    {
        CAPTURE_POS;
        if (peekChar() == '&')
        {
            advance();
            advance();
            return Token{"&&", TokenType::AND, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"&", TokenType::BITWISE_AND, tokenLine, tokenColumn};
        }
    }
    case '|':
    {
        CAPTURE_POS;
        if (peekChar() == '|')
        {
            advance();
            advance();
            return Token{"||", TokenType::OR, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"|", TokenType::BITWISE_OR, tokenLine, tokenColumn};
        }
    }
    case '>':
    {
        CAPTURE_POS;
        if (peekChar() == '>')
        {
            advance();
            advance();
            return Token{">>", TokenType::SHIFT_RIGHT, tokenLine, tokenColumn};
        }
        else if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{">=", TokenType::GT_OR_EQ, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{">", TokenType::GREATER_THAN, tokenLine, tokenColumn};
        }
    }
    case '<':
    {
        CAPTURE_POS;
        if (peekChar() == '<')
        {
            advance();
            advance();
            return Token{"<<", TokenType::SHIFT_LEFT, tokenLine, tokenColumn};
        }
        else if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{"<=", TokenType::LT_OR_EQ, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"<", TokenType::LESS_THAN, tokenLine, tokenColumn};
        }
    }
    case '{':
    {
        CAPTURE_POS;
        advance();
        return Token{"{", TokenType::LBRACE, tokenLine, tokenColumn};
    }
    case '}':
    {
        CAPTURE_POS;
        advance();
        return Token{"}", TokenType::RBRACE, tokenLine, tokenColumn};
    }
    case '[':
    {
        CAPTURE_POS;
        advance();
        return Token{"[", TokenType::LBRACKET, tokenLine, tokenColumn};
    }
    case ']':
    {
        CAPTURE_POS;
        advance();
        return Token{"]", TokenType::RBRACKET, tokenLine, tokenColumn};
    }
    case '(':
    {
        CAPTURE_POS;
        advance();
        return Token{"(", TokenType::LPAREN, tokenLine, tokenColumn};
    }
    case ')':
    {
        CAPTURE_POS;
        advance();
        return Token{")", TokenType::RPAREN, tokenLine, tokenColumn};
    }
    case ';':
    {
        CAPTURE_POS;
        advance();
        return Token{";", TokenType::SEMICOLON, tokenLine, tokenColumn};
    }
    case ',':
    {
        CAPTURE_POS;
        advance();
        return Token{",", TokenType::COMMA, tokenLine, tokenColumn};
    }
    case ':':
    {
        CAPTURE_POS;
        advance();
        return Token{":", TokenType::COLON, tokenLine, tokenColumn};
    }
    case '"':
        return readString();
    case '\'':
    {
        return readChar();
    }
    case '\0':
    {
        return Token{"", TokenType::END};
    }
    default:
    {
        CAPTURE_POS;
        advance();
        logError("Unexpected character: ", tokenLine, tokenColumn);
        return Token{string(1, character), TokenType::ILLEGAL, tokenLine, tokenColumn};
    }
    }
};

void Lexer::updateTokenList()
{
    token_list.clear();
    while (true)
    {
        Token tok = tokenize();
        token_list.push_back(tok);
        if (tok.type == TokenType::END)
        {
            break;
        }
    }
}

void Lexer::logError(const std::string &message, int line, int column)
{
    std::cerr << "[TOKEN ERROR]: At line " << line << " column " << column << " : " << message << "\n";
}
