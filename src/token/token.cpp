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

    case TokenType::CONST:
        return "Token Type: CONST";
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

    case TokenType::GENERIC:
        return "Token Type: GENERIC";
    case TokenType::INSTANTIATE:
        return "Token Type: INSTANTIATE";
    case TokenType::AS:
        return "Token Type: AS";
    case TokenType::ALIAS:
        return "Token Type:ALIAS";

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
    case TokenType::EACH:
        return "Token Type: EACH";
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

    case TokenType::SHORT:
        return "Token Type: SHORT";
    case TokenType::SHORT_KEYWORD:
        return "Token Type: SHORT KEYWORD";
    case TokenType::USHORT:
        return "TokenType: USHORT";
    case TokenType::USHORT_KEYWORD:
        return "Token Type: USHORT KEYWORD";

    case TokenType::INT:
        return "Token Type: INT";
    case TokenType::INTEGER_KEYWORD:
        return "Token Type: INTEGER KEYWORD";
    case TokenType::UINT:
        return "Token Type: UNINT";
    case TokenType::UINT_KEYWORD:
        return "Token Type: UINT KEYWORD";

    case TokenType::LONG:
        return "TokenType: LONG";
    case TokenType::LONG_KEYWORD:
        return "Token Type: LONG KEYWORD";
    case TokenType::ULONG:
        return "Token Type: ULONG";
    case TokenType::ULONG_KEYWORD:
        return "Token Type: ULONG KEYWORD";

    case TokenType::EXTRA:
        return "Token Type: EXTRA";
    case TokenType::EXTRA_KEYWORD:
        return "TokenType:: EXTRA_KEYWORD";
    case TokenType::UEXTRA:
        return "Token Type: UEXTRA";
    case TokenType::UEXTRA_KEYWORD:
        return "TokenType:: UEXTRA_KEYWORD";

    case TokenType::CHAR:
        return "Token Type: CHAR";
    case TokenType::CHAR_KEYWORD:
        return "Token Type: CHAR KEYWORD";
    case TokenType::CHAR16:
        return "Token Type: CHAR16";
    case TokenType::CHAR16_KEYWORD:
        return "Token Type: CHAR16 KEYWORD";
    case TokenType::CHAR32:
        return "Token Type: CHAR32";
    case TokenType::CHAR32_KEYWORD:
        return "Token Type: CHAR32 KEYWORD";

    case TokenType::STRING:
        return "Token Type: STRING";
    case TokenType::STRING_KEYWORD:
        return "Token Type: STRING KEYWORD";
    case TokenType::FLOAT_KEYWORD:
        return "Token Type: FLOAT KEYWORD";

    case TokenType::QUESTION_MARK:
        return "Token Type: QUESTION MARK";
    case TokenType::COALESCE:
        return "TokenType::COALESCE";

    case TokenType::BOOL_KEYWORD:
        return "Token Type: BOOL_KEYWORD";
    case TokenType::DOUBLE_KEYWORD:
        return "Token Type: DOUBLE_KEYWORD";
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
    case TokenType::REF:
        return "Token Type: REF";
    case TokenType::PTR:
        return "Token Type: PTR";
    case TokenType::DEREF:
        return "Token Type: DEREF";
    case TokenType::MODULUS:
        return "Token Type: MODULUS";
    case TokenType::MODULE:
        return "Token Type: MODULE";
    case TokenType::LINK:
        return "TokenType: LINK";
    case TokenType::MERGE:
        return "Token Type: MERGE";

    case TokenType::SCOPE:
        return "Token Type: SCOPE";
    case TokenType::SHOUT:
        return "Token Type: SHOUT";
    case TokenType::QUALIFY:
        return "Token Type: QUALIFY";
        
    case TokenType::HEAP:
        return "Token Type: HEAP";
    case TokenType::BANG:
        return "Token Type: BANG";
    case TokenType::FULLSTOP:
        return "Token Type: FULLSTOP";
    default:
        return "Token Type: ILLEGAL";
    }
}