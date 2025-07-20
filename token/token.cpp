#include <iostream>
#include "token.hpp"
using namespace std;

string TokenTypeToLiteral(TokenType type)
{
    switch (type)
    {
    case TokenType::AND:
        return "Token Type: AND";
    case TokenType::ASSIGN:
        return "Token Type: ASSIGN";
    case TokenType::ASTERISK:
        return "Token Type: ASTERISK";
    case TokenType::EQUALS:
        return "Token Type: EQUALS";
    case TokenType::GREATER_THAN:
        return "Token Type: GREATER THAN";
    case TokenType::GT_OR_EQ:
        return "Token Type: GREATER THAN OR EQUAL TO";
    case TokenType::LESS_THAN:
        return "Token Type: LESS THAN";
    case TokenType::LT_OR_EQ:
        return "Token Type: LESS THAN OR EQUAL TO";
    case TokenType::BITWISE_AND:
        return "Token Type: BITWISE AND";
    case TokenType::BITWISE_OR:
        return "Token Type: BITWISE OR";
    case TokenType::PLUS:
        return "Token Type: PLUS";
    case TokenType::PLUS_PLUS:
        return "TokenType: PLUS PLUS";
    case TokenType::MINUS:
        return "Token Type: MINUS";
    case TokenType::MINUS_MINUS:
        return "Token Type: MINUS MINUS";
    case TokenType::DIVIDE:
        return "Token Type: DIVIDE";
    case TokenType::OR:
        return "Token Type: OR";
    case TokenType::SHIFT_RIGHT:
        return "Token Type: SHIFT RIGHT";
    case TokenType::SHIFT_LEFT:
        return "Token Type: SHIFT LEFT";
    case TokenType::LBRACE:
        return "Token Type: LBRACE";
    case TokenType::RBRACE:
        return "Token Type: RBRACE";
    case TokenType::ARROW:
        return "Token Type: ARROW";
    case TokenType::LBRACKET:
        return "Token Type: LBRACKET";
    case TokenType::RBRACKET:
        return "Token Type: RBRACKET";
    case TokenType::LPAREN:
        return "Token Type: LPAREN";
    case TokenType::RPAREN:
        return "Token Type: RPAREN";
    case TokenType::SEMICOLON:
        return "Token Type: SEMICOLON";
    case TokenType::COLON:
        return "Token Type: COLON";
    case TokenType::COMMA:
        return "Token Type: COMMA";
    case TokenType::IDENTIFIER:
        return "Token Type: IDENTIFIER";
    case TokenType::END:
        return "Token Type:END";
    case TokenType::INTEGER:
        return "Token Type:INTEGER";
    case TokenType::FLOAT:
        return "Token Type: FLOAT";
    case TokenType::VOID:
        return "Token Type: VOID";
    case TokenType::FUNCTION:
        return "Token Type: FUNCTION";
    case TokenType::AUTO:
        return "Token Type: AUTO";
    case TokenType::RETURN:
        return "Token Type: RETURN";
    case TokenType::ENUM:
        return "Token Type:ENUM";
    case TokenType::CLASS:
        return "Token Type: CLASS";
    case TokenType::CONST:
        return "Token Type: CONST";
    case TokenType::CAST:
        return "Token Type: CAST";
    case TokenType::COMPONENT:
        return "Token Type: COMPONENT";
    case TokenType::SELF:
        return "Token Type: SELF";
    case TokenType::BEHAVIOR:
        return "Token Type: BEHAVIOR";
    case TokenType::NEW:
        return "Token Type: NEW";
    case TokenType::DATA:
        return "Token Type: DATA";
    case TokenType::USE:
        return "Token Type: USE";
    case TokenType::INIT:
        return "Token Type: INIT";
    case TokenType::START:
        return "Token Type: START";
    case TokenType::WAIT:
        return "Token Type: WAIT";
    case TokenType::SIGNAL:
        return "Tokenn Type: SIGNAL";
    case TokenType::ERROR:
        return "Token Type: ERROR";
    case TokenType::IF:
        return "Token Type: IF";
    case TokenType::ELSE:
        return "Token Type: ELSE";
    case TokenType::ELSE_IF:
        return "Token Type: ELSE IF";
    case TokenType::WHILE:
        return "Token Type: WHILE";
    case TokenType::FOR:
        return "Token Type: FOR";
    case TokenType::SCOPE_OPERATOR:
        return "Token Type: SCOPE OPERATOR";
    case TokenType::BREAK:
        return "Token Type: BREAK";
    case TokenType::CONTINUE:
        return "Token Type: CONTINUE";
    case TokenType::SWITCH:
        return "Token Type: SWITCH";
    case TokenType::CASE:
        return "Token Type: CASE";
    case TokenType::DEFAULT:
        return "Token Type: DEFAULT";
    case TokenType::INT:
        return "Token Type: INT";
    case TokenType::STRING:
        return "Token Type: STRING";
    case TokenType::STRING_KEYWORD:
        return "Token Type: STRING KEYWORD";
    case TokenType::FLOAT_KEYWORD:
        return "Token Type: FLOAT KEYWORD";
    case TokenType::CHAR_KEYWORD:
        return "Token Type: CHAR KEYWORD";
    case TokenType::CHAR:
        return "Token Type: CHAR";
    case TokenType::QUESTION_MARK:
        return "Token Type: QUESTION MARK";
    case TokenType::BOOL_KEYWORD:
        return "Token Type: BOOL_KEYWORD";
    case TokenType::NULLABLE:
        return "Token Type: NULL";
    case TokenType::TRUE:
        return "Token Type: TRUE";
    case TokenType::FALSE:
        return "Token Type: FALSE";
    case TokenType::ARRAY:
        return "Token Type: ARRAY";
    case TokenType::MUT:
        return "Token Type: MUT";
    case TokenType::UNIQUE:
        return "Token Type: UNIQUE";
    case TokenType::MAKE:
        return "Token Type: MAKE";
    case TokenType::UNSAFE:
        return "Token Type: UNSAFE";
    case TokenType::ALLOCATE:
        return "Token Type: ALLOCATE";
    case TokenType::GC:
        return "Token Type: GC";
    case TokenType::DROP:
        return "Token Type: DROP";
    case TokenType::ELEVATE:
        return "Token Type: ELEVATE";
    case TokenType::WRITE:
        return "Token Type: WRITE";
    case TokenType::POINTER:
        return "Token Type: POINTER";
    case TokenType::READ:
        return "Token Type: READ";
    case TokenType::BANG:
        return "Token Type: BANG";
    case TokenType::FULLSTOP:
        return "Token Type: FULLSTOP";
    default:
        return "Token Type: ILLEGAL";
    }
}