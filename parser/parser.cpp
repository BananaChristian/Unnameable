#include "parser.hpp"
#include "ast.hpp"
#include "token/token.hpp"
#include <iostream>
#include <memory>
#include <vector>
using namespace std;

//--------------PARSER CLASS CONSTRUCTOR-------------
Parser::Parser(vector<Token> &tokenInput) : tokenInput(tokenInput), currentPos(0), nextPos(1)
{
    registerInfixFns();
    registerPrefixFns();
    registerKeywordParseFns();
    registerStatementParseFns();
}

// MAIN PARSER FUNCTION
vector<unique_ptr<Node>> Parser::parseProgram()
{
    vector<unique_ptr<Node>> program;

    while (currentPos < tokenInput.size())
    {
        cout << "Parsing token: " << currentToken().TokenLiteral << endl;
        Token current = currentToken();

        if (current.type == TokenType::END)
        {
            break;
        }

        if (KeywordParseFunctionsMap.count(current.type))
        {
            auto stmt = parseKeywords(current.type);
            if (stmt)
            {
                program.push_back(move(stmt));
            }
        }
        else if (current.type == TokenType::IDENTIFIER && nextToken().type == TokenType::ASSIGN)
        {
            auto stmt=parseLetStatement();
            if(stmt){
                program.push_back(move(stmt));
            }
        }else if(current.type==TokenType::RETURN){
            auto stmt=parseReturnStatement();
            if(stmt){
                program.push_back(move(stmt));
            }
        }
        else if (current.type == TokenType::IDENTIFIER)
        {
            auto stmt = parseIdentifier();
            if (stmt)
            {
                program.push_back(move(stmt));
            }
        }
        advance();
    }
    cout << "Parser finished" << endl;
    return program;
}

//------------PARSING FUNCTIONS SECTION----------
//-----------PARSING STATEMENTS----------
// Parsing let statements
unique_ptr<Statement> Parser::parseLetStatement() {
    Token identifier_token = currentToken(); // x
    auto left = make_unique<Identifier>(identifier_token);

    advance(); 
    Token assign_token = currentToken();

    advance();
    auto right = parseExpression(get_precedence(assign_token.type));

    return make_unique<LetStatement>(assign_token, move(left), move(right));
}

unique_ptr<Statement> Parser::parseReturnStatement(){
    Token return_stmt = currentToken();
    advance(); 

    if (currentToken().type == TokenType::SEMICOLON || currentToken().type == TokenType::END) {
        cout << "[DEBUG] Return is void" << endl;
        return make_unique<ReturnStatement>(return_stmt, nullptr); // void return
    }

    cout << "[DEBUG] Parsing return expression token: " << currentToken().TokenLiteral << endl;
    auto return_value = parseExpression(Precedence::PREC_NONE);

    if (!return_value) {
        cout << "[DEBUG] return_value is NULL" << endl;
    } else {
        cout << "[DEBUG] return_value EXISTS: " << currentToken().TokenLiteral << endl;
    }

    if (currentToken().type == TokenType::SEMICOLON) {
        advance();
    }

    return make_unique<ReturnStatement>(return_stmt, move(return_value));
}

// Parsing identifiers
unique_ptr<Expression> Parser::parseIdentifier()
{
    auto ident = make_unique<Identifier>(currentToken());
    advance();
    return ident;
}

// Parsing keywords
unique_ptr<Statement> Parser::parseKeywords(TokenType type)
{
    auto KeywordFnIt = KeywordParseFunctionsMap.find(type);
    if (KeywordFnIt == KeywordParseFunctionsMap.end())
    {
        return nullptr;
    }
    auto keywordfunc = (this->*KeywordFnIt->second)();
    return keywordfunc;
}

// Parsing the bool keyword
unique_ptr<Statement> Parser::parseBoolKeyword()
{
    Token bool_t = currentToken();
    return make_unique<BooleanKeyword>(bool_t);
}

// Parsing the int keyword
unique_ptr<Statement> Parser::parseIntKeyword()
{
    Token int_t = currentToken();
    return make_unique<IntKeyword>(int_t);
}

// Parsing the string keyword
unique_ptr<Statement> Parser::parseStringKeyword()
{
    Token string_t = currentToken();
    return make_unique<StringKeyword>(string_t);
};

// Parsing the float keyword
unique_ptr<Statement> Parser::parseFloatKeyword()
{
    Token float_t = currentToken();
    return make_unique<FloatKeyword>(float_t);
}

// Parsing the auto keyword
unique_ptr<Statement> Parser::parseAutoKeyword()
{
    Token auto_t = currentToken();
    return make_unique<AutoKeyword>(auto_t);
}

//-----------PARSING EXPRESSIONS----------
// Expression parsing section
// Main Expression parsing function
unique_ptr<Expression> Parser::parseExpression(Precedence precedence)
{
    auto PrefixParseFnIt = PrefixParseFunctionsMap.find(currentToken().type);

    if (PrefixParseFnIt == PrefixParseFunctionsMap.end())
    {
        cerr << "[ERROR] No prefix parse function for token: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }

    auto left_expression = (this->*PrefixParseFnIt->second)();
    cout << "[DEBUG] Initial left expression: " << left_expression->toString() << endl;
    
    while (precedence < get_precedence(currentToken().type))
    {
        cout << "[DEBUG] Looping for token: " << currentToken().TokenLiteral << endl;
        auto InfixParseFnIt = InfixParseFunctionsMap.find(currentToken().type);

        if (InfixParseFnIt == InfixParseFunctionsMap.end())
        {
            cout << "[DEBUG] No infix parser found for: " << currentToken().TokenLiteral << endl;
            break;
        }

        left_expression = (this->*InfixParseFnIt->second)(std::move(left_expression));
        cout << "[DEBUG] Updated left expression: " << left_expression->toString() << endl;
        //advance();
    }

    return left_expression;
}

// Inifix parse function definition
unique_ptr<Expression> Parser::parseInfixExpression(unique_ptr<Expression> left)
{
    Token operat = currentToken();
    cout << "[DEBUG] parsing infix with operator: " << operat.TokenLiteral << endl;
    Precedence prec = get_precedence(operat.type);
    advance();
    auto right = parseExpression(prec);
    return make_unique<InfixExpression>(move(left), operat, move(right));
}

// Prefix parse function definition
unique_ptr<Expression> Parser::parsePrefixExpression()
{
    Token operat = currentToken();
    Precedence operatorPrecedence = get_precedence(operat.type);
    advance();
    auto operand = parseExpression(operatorPrecedence);
    return make_unique<PrefixExpression>(operat, move(operand));
}

//Integer literal parse function
unique_ptr<Expression> Parser::parseIntegerLiteral(){
    auto ident= make_unique<Identifier>(currentToken());
    advance();
    return ident;
}

//Boolean literal parse function
unique_ptr<Expression> Parser::parseBooleanLiteral(){
    Token bool_tok=currentToken();
    return make_unique<BooleanLiteral>(bool_tok);
}

//Float literal parse function
unique_ptr<Expression> Parser::parseFloatLiteral(){
    Token float_tok=currentToken();
    return make_unique<FloatLiteral>(float_tok);
}

//Char literal parse function
unique_ptr<Expression> Parser::parseCharLiteral(){
    Token char_tok=currentToken();
    return make_unique<CharLiteral>(char_tok);
}

//String literal parse function
unique_ptr<Expression> Parser::parseStringLiteral(){
    Token string_tok=currentToken();
    return make_unique<StringLiteral>(string_tok);
}

//Grouped expression parse function
unique_ptr<Expression> Parser::parseGroupedExpression(){
    advance();
    auto expr=parseExpression(Precedence::PREC_NONE);
    if(currentToken().type != TokenType::RPAREN){
        cerr << "[ERROR] Expected ')'" << endl;
        return nullptr;
    }
    advance();
    return expr;
}

//----------HELPER FUNCTIONS---------------
// Slider function
void Parser::advance()
{
    if (nextPos < tokenInput.size())
    {
        currentPos = nextPos;
        nextPos++;
    }
}

// Registration functions
// Registering infix functions for a particular token type
void Parser::registerInfixFns()
{
    InfixParseFunctionsMap[TokenType::PLUS] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::MINUS] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::DIVIDE] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::ASTERISK] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::MODULUS] = &Parser::parseInfixExpression;
}

// Registering prefix functions for a particular token type
void Parser::registerPrefixFns()
{
    PrefixParseFunctionsMap[TokenType::INTEGER]=&Parser::parseIntegerLiteral;
    PrefixParseFunctionsMap[TokenType::TRUE]=&Parser::parseBooleanLiteral;
    PrefixParseFunctionsMap[TokenType::FALSE]=&Parser::parseBooleanLiteral;
    PrefixParseFunctionsMap[TokenType::FLOAT]=&Parser::parseFloatLiteral;
    PrefixParseFunctionsMap[TokenType::CHAR]=&Parser::parseCharLiteral;
    PrefixParseFunctionsMap[TokenType::STRING]=&Parser::parseStringLiteral;
    PrefixParseFunctionsMap[TokenType::IDENTIFIER]=&Parser::parseIdentifier;
    PrefixParseFunctionsMap[TokenType::LPAREN]= &Parser::parseGroupedExpression;
    PrefixParseFunctionsMap[TokenType::LBRACE]=&Parser::parseGroupedExpression;
}

// Registering keyword parsing functions
void Parser::registerKeywordParseFns()
{
    KeywordParseFunctionsMap[TokenType::BOOL_KEYWORD] = &Parser::parseBoolKeyword;
    KeywordParseFunctionsMap[TokenType::INT] = &Parser::parseIntKeyword;
    KeywordParseFunctionsMap[TokenType::STRING_KEYWORD] = &Parser::parseStringKeyword;
    KeywordParseFunctionsMap[TokenType::FLOAT_KEYWORD] = &Parser::parseFloatKeyword;
    KeywordParseFunctionsMap[TokenType::AUTO] = &Parser::parseAutoKeyword;
}

// Registering the statement parsing functions
void Parser::registerStatementParseFns()
{
    StatementParseFunctionsMap[TokenType::ASSIGN] = &Parser::parseLetStatement;
    StatementParseFunctionsMap[TokenType::RETURN]=&Parser::parseReturnStatement;
}

// Precedence getting function
Precedence Parser::get_precedence(TokenType type)
{
    Precedence prec = precedence.count(type) ? precedence[type] : Precedence::PREC_NONE;
    return prec;
}

// Current token peeking function
Token Parser::currentToken()
{
    Token current = tokenInput[currentPos];
    return current;
}

// Next token peeking function
Token Parser::nextToken()
{
    Token next = tokenInput[nextPos];
    return next;
}