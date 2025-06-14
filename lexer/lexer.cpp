#include "token/token.hpp"
#include "lexer.hpp"
#include <iostream>
#include <unordered_map>
#include <string>
using namespace std;

Lexer::Lexer(string &input_str) : input(input_str), currentPosition(0), nextPosition(1) {};

// Lexer advance function
void Lexer::advance()
{
    if (nextPosition < input.length())
    {
        currentPosition = nextPosition;
        nextPosition++;
    }
    else
    {
        currentPosition = input.length();
    }
}

char Lexer::peekChar()
{
    if(nextPosition>= input.length()){
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
    while (currentChar() == ' ' || currentChar() == '\n' || currentChar() == '\t')
    {
        advance();
    }
}

Token Lexer::readNumbers()
{
    string number;
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
            return Token{number, TokenType::FLOAT};
        }
    }
    return Token{number, TokenType::INTEGER};
}

Token Lexer::readIdentifiers()
{
    string identifier;
    while (isLetter(currentChar()))
    {
        identifier += currentChar();
        advance();
    }
    TokenType type = keywords.count(identifier) ? keywords[identifier] : TokenType::IDENTIFIER;
    return Token{identifier, type};
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

Token Lexer::readString()
{
    std::string value;
    advance();

    while (currentChar() != '\0' && currentChar() != '\n')
    {
        if (currentChar() == '"')
        {
            advance();
            return Token{value, TokenType::STRING};
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
                return Token{"Invalid escape sequence", TokenType::ILLEGAL};
            }
        }
        else
        {
            value += currentChar();
        }
        advance();
    }
    return Token{"Unterminated string", TokenType::ILLEGAL};
}

Token Lexer::readChar()
{
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
            return Token{"Invalid escape", TokenType::ILLEGAL};
        }

        advance();
        if (currentChar() != '\'')
        {
            return Token{"Missing closing quote", TokenType::ILLEGAL};
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
        if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{"==", TokenType::EQUALS};
        }
        else
        {
            advance();
            return Token{"=", TokenType::ASSIGN};
        }
    case '!':
        if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{"!=", TokenType::NOT_EQUALS};
        }
        else
        {
            advance();
            return Token{"!", TokenType::BANG};
        }
    case '+':
        advance();
        return Token{"+", TokenType::PLUS};
    case '-':
        advance();
        return Token{"-", TokenType::MINUS};
    case '*':
        advance();
        return Token{"*", TokenType::ASTERISK};
    case '/':
        advance();
        return Token{"/", TokenType::DIVIDE};
    case '&':
        if (peekChar() == '&')
        {
            advance();
            advance();
            return Token{"&&", TokenType::AND};
        }
    case '|':
        if (peekChar() == '|')
        {
            advance();
            advance();
            return Token{"||", TokenType::OR};
        }
    case '>':
        if (peekChar() == '>')
        {
            advance();
            advance();
            return Token{">>", TokenType::SHIFT_RIGHT};
        }
        else if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{">=", TokenType::GT_OR_EQ};
        }
        else
        {
            advance();
            return Token{">", TokenType::GREATER_THAN};
        }
    case '<':
        if (peekChar() == '<')
        {
            advance();
            advance();
            return Token{"<<", TokenType::SHIFT_LEFT};
        }
        else if (peekChar() == '=')
        {
            advance();
            advance();
            return Token{"<=", TokenType::LT_OR_EQ};
        }
        else
        {
            advance();
            return Token{"<", TokenType::LESS_THAN};
        }
    case '{':
        advance();
        return Token{"{", TokenType::LBRACE};
    case '}':
        advance();
        return Token{"}", TokenType::RBRACE};
    case '[':
        advance();
        return Token{"[", TokenType::LBRACKET};
    case ']':
        advance();
        return Token{"]", TokenType::RBRACKET};
    case '(':
        advance();
        return Token{"(", TokenType::LPAREN};
    case ')':
        advance();
        return Token{")", TokenType::RPAREN};
    case ';':
        advance();
        return Token{";", TokenType::SEMICOLON};
    case ',':
        advance();
        return Token{",", TokenType::COMMA};
    case ':':
        advance();
        return Token{":", TokenType::COLON};
    case '"':
        return readString();
    case '\'':
        return readChar();
    case '\0':
        return Token{"", TokenType::END};
    default:
        advance();
        return Token{string(1, character), TokenType::ILLEGAL};
    }
};

void Lexer::updateTokenList(){
    token_list.clear();
    while(true){
        Token tok=tokenize();
        token_list.push_back(tok);
        if(tok.type==TokenType::END){
            break;
        }
    }
}
