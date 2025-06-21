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

    precedence = {
        {TokenType::ASSIGN, Precedence::PREC_ASSIGNMENT},
        {TokenType::OR, Precedence::PREC_OR},
        {TokenType::AND, Precedence::PREC_AND},
        {TokenType::EQUALS, Precedence::PREC_EQUALITY},
        {TokenType::NOT_EQUALS, Precedence::PREC_EQUALITY},
        {TokenType::GREATER_THAN, Precedence::PREC_COMPARISON},
        {TokenType::LESS_THAN, Precedence::PREC_COMPARISON},
        {TokenType::GT_OR_EQ, Precedence::PREC_COMPARISON},
        {TokenType::LT_OR_EQ, Precedence::PREC_COMPARISON},
        {TokenType::PLUS, Precedence::PREC_TERM},
        {TokenType::MINUS, Precedence::PREC_TERM},
        {TokenType::ASTERISK, Precedence::PREC_FACTOR},
        {TokenType::DIVIDE, Precedence::PREC_FACTOR},
        {TokenType::BANG, Precedence::PREC_UNARY},
        {TokenType::FULLSTOP, Precedence::PREC_CALL},
        {TokenType::LPAREN,Precedence::PREC_CALL},
        {TokenType::IDENTIFIER, Precedence::PREC_PRIMARY},
    };
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
        if (current.type == TokenType::INT ||
            current.type == TokenType::FLOAT_KEYWORD ||
            current.type == TokenType::STRING_KEYWORD ||
            current.type == TokenType::BOOL_KEYWORD ||
            current.type == TokenType::AUTO)
        {
            auto stmt = parseLetStatementWithType();
            if (stmt)
            {
                program.push_back(move(stmt));
            }
        }
        else if (current.type == TokenType::RETURN)
        {
            auto stmt = parseReturnStatement();
            if (stmt)
            {
                program.push_back(move(stmt));
            }
        }
        else if (current.type == TokenType::IF)
        {
            auto stmt = parseIfStatement();
            if (stmt)
            {
                program.push_back(move(stmt));
            }
        }
        else if (current.type == TokenType::WHILE)
        {
            auto stmt = parseWhileStatement();
            if (stmt)
            {
                program.push_back(move(stmt));
            }
        }
        else if (current.type == TokenType::IDENTIFIER)
        {
            Token peek_token = nextToken();
            if (peek_token.type == TokenType::ASSIGN)
            {
                auto stmt = parseLetStatementWithoutType();
                if (stmt)
                {
                    program.push_back(move(stmt));
                }
            }
            else
            {
                auto expr = parseExpression(Precedence::PREC_NONE);
                if (expr)
                {
                    program.push_back(move(expr));
                }
            }
        }
        advance();
    }
    cout << "Parser finished" << endl;
    return program;
}

//------------PARSING FUNCTIONS SECTION----------
//-----------PARSING STATEMENTS----------
// General statement parser function
unique_ptr<Statement> Parser::parseStatement()
{
    Token current = currentToken();

    if (current.type == TokenType::INT ||
        current.type == TokenType::FLOAT_KEYWORD ||
        current.type == TokenType::STRING_KEYWORD ||
        current.type == TokenType::BOOL_KEYWORD ||
        current.type == TokenType::AUTO)
    {
        return parseLetStatementWithType();
    }
    else if (current.type == TokenType::RETURN)
    {
        return parseReturnStatement();
    }
    else if (current.type == TokenType::IF)
    {
        return parseIfStatement();
    }
    else if (current.type == TokenType::WHILE)
    {
        return parseWhileStatement();
    }
    else if (current.type == TokenType::IDENTIFIER)
    {
        Token peek_token_ = nextToken();
        if (peek_token_.type == TokenType::ASSIGN)
        {
            return parseLetStatementWithoutType();
        }
        else
        {
            auto expr = parseExpression(Precedence::PREC_NONE);
            if (expr)
            {
                if (currentToken().type == TokenType::SEMICOLON)
                {
                    advance();
                }
                return make_unique<ExpressionStatement>(current, move(expr));
            }
            return nullptr;
        }
    }

    cerr << "[ERROR] Unexpected token type at start of statement: " << current.TokenLiteral << endl;
    advance();
    return nullptr;
}

// Parsing let statements with types
unique_ptr<Statement> Parser::parseLetStatementWithoutType()
{
    Token ident_token = currentToken();
    cout << "[DEBUG] Identifier token: " + ident_token.TokenLiteral << endl;
    advance();

    if (currentToken().type != TokenType::ASSIGN)
    {
        cout << "[DEBUG]Expected = after identifier" << endl;
        return nullptr;
    }
    advance();

    unique_ptr<Expression> value = parseExpression(Precedence::PREC_NONE);

    return make_unique<LetStatementNoType>(ident_token, move(value));
}

unique_ptr<Statement> Parser::parseLetStatementWithType()
{
    Token dataType_token = currentToken();
    cout << "[DEBUG] Data type token: " + dataType_token.TokenLiteral << endl;
    advance();

    if (currentToken().type != TokenType::IDENTIFIER)
    {
        cout << "[DEBUG] Expected variable name after data type '"
             << dataType_token.TokenLiteral << "' but got '"
             << currentToken().TokenLiteral << "'" << endl;
        return nullptr;
    }

    Token ident_token = currentToken();
    advance();

    optional<Token> assign_token;
    unique_ptr<Expression> value = nullptr;

    if (currentToken().type == TokenType::ASSIGN)
    {
        assign_token = currentToken();
        cout << "[DEBUG] Encountered assignment token" << endl;
        advance();
        value = parseExpression(Precedence::PREC_NONE);
    }
    else if (currentToken().type == TokenType::SEMICOLON)
    {
        cout << "[DEBUG] Encountered semicolon token" << endl;
    }

    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance();
    }

    return make_unique<LetStatement>(dataType_token, ident_token, assign_token, move(value));
}

// Parsing return statements
unique_ptr<Statement> Parser::parseReturnStatement()
{
    Token return_stmt = currentToken();
    advance();

    if (currentToken().type == TokenType::SEMICOLON || currentToken().type == TokenType::END)
    {
        cout << "[DEBUG] Return is void" << endl;
        return make_unique<ReturnStatement>(return_stmt, nullptr);
    }

    cout << "[DEBUG] Parsing return expression token: " << currentToken().TokenLiteral << endl;
    auto return_value = parseExpression(Precedence::PREC_NONE);

    if (!return_value)
    {
        cout << "[DEBUG] return_value is NULL" << endl;
    }
    else
    {
        cout << "[DEBUG] return_value EXISTS: " << currentToken().TokenLiteral << endl;
    }

    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance();
    }

    return make_unique<ReturnStatement>(return_stmt, move(return_value));
}

// Parse for loops
/*
unique_ptr<Statement> Parser::parseForStatement(){
    Token for_k=currentToken();
    advance();
    auto expr=parseExpression(Precedence::PREC_NONE);

}
*/

// Parsing while statements
unique_ptr<Statement> Parser::parseWhileStatement()
{
    Token while_key = currentToken();
    cout << "WHILE KEYWORD" << endl;
    advance();
    if (currentToken().type != TokenType::LPAREN)
    {
        cout << "[DEBUG] Expected '(' after 'while', got: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }
    advance();
    auto condition = parseExpression(Precedence::PREC_NONE);
    if (currentToken().type != TokenType::RPAREN)
    {
        cout << "[DEBUG] Expected ')' after while condition, got: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }
    advance();
    auto result = parseBlockStatement();
    return make_unique<WhileStatement>(while_key, move(condition), move(result));
}

// Parsing if statements`
unique_ptr<Statement> Parser::parseIfStatement()
{
    Token if_stmt = currentToken();
    advance();

    if (currentToken().type != TokenType::LPAREN)
    {
        cout << "[DEBUG] Expected '(' after 'if', got: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }
    advance();

    auto condition = parseExpression(Precedence::PREC_NONE);
    if (currentToken().type != TokenType::RPAREN)
    {
        cout << "[DEBUG] Expected ')' got: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }
    advance();
    auto if_result = parseBlockStatement();

    optional<Token> elseif_stmt;
    optional<unique_ptr<Expression>> elseif_condition;
    optional<unique_ptr<Statement>> elseif_result;

    if (currentToken().type == TokenType::ELSE_IF)
    {
        elseif_stmt = currentToken();
        advance();

        if (currentToken().type != TokenType::LPAREN)
        {
            cout << "[DEBUG] Expected '(' after 'elseif', got: " << currentToken().TokenLiteral << endl;
            return nullptr;
        }

        elseif_condition = parseGroupedExpression();
        elseif_result = parseBlockStatement();
    }

    optional<Token> else_stmt;
    optional<unique_ptr<Statement>> else_result;

    if (currentToken().type == TokenType::ELSE)
    {
        else_stmt = currentToken();
        advance();

        else_result = parseBlockStatement();
    }

    return make_unique<ifStatement>(
        if_stmt,
        move(condition),
        move(if_result),
        move(elseif_stmt),
        move(elseif_condition),
        move(elseif_result),
        move(else_stmt),
        move(else_result));
}

// Parsing identifiers
unique_ptr<Expression> Parser::parseIdentifier()
{
    auto ident = make_unique<Identifier>(currentToken());
    advance();
    return ident;
}

//-----------PARSING EXPRESSIONS----------
// Expression parsing section
// Main Expression parsing function
unique_ptr<Expression> Parser::parseExpression(Precedence precedence)
{
    auto PrefixParseFnIt = PrefixParseFunctionsMap.find(currentToken().type); // Creating an iterator pointer to loop through the map

    if (PrefixParseFnIt == PrefixParseFunctionsMap.end()) // Checking if the iterator has reached the end of the map
    {
        cerr << "[ERROR] No prefix parse function for token: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }

    auto left_expression = (this->*PrefixParseFnIt->second)(); // Calling the neccesary prefix function after encountering that particular token
    cout << "[DEBUG] Initial left expression: " << left_expression->toString() << endl;

    while (precedence < get_precedence(currentToken().type)) // Looping as long as the precedence is lower than the precedence of the current token
    {
        cout << "[DEBUG] Looping for token: " << currentToken().TokenLiteral << endl;
        auto InfixParseFnIt = InfixParseFunctionsMap.find(currentToken().type); // Creating the iterator pointer for the infix map

        if (InfixParseFnIt == InfixParseFunctionsMap.end()) // Checking if the iterator has reached the end of the map
        {
            cout << "[DEBUG] No infix parser found for: " << currentToken().TokenLiteral << endl;
            break;
        }

        left_expression = (this->*InfixParseFnIt->second)(std::move(left_expression)); // If we find the infix parse function for that token we call the function
        cout << "[DEBUG] Updated left expression: " << left_expression->toString() << endl;
    }

    return left_expression; // Returning the expression that was parsed it can be either prefix or infix
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

// Integer literal parse function
unique_ptr<Expression> Parser::parseIntegerLiteral()
{
    auto ident = make_unique<IntegerLiteral>(currentToken());
    advance();
    return ident;
}

// Boolean literal parse function
unique_ptr<Expression> Parser::parseBooleanLiteral()
{
    Token bool_tok = currentToken();
    return make_unique<BooleanLiteral>(bool_tok);
}

// Float literal parse function
unique_ptr<Expression> Parser::parseFloatLiteral()
{
    Token float_tok = currentToken();
    return make_unique<FloatLiteral>(float_tok);
}

// Char literal parse function
unique_ptr<Expression> Parser::parseCharLiteral()
{
    Token char_tok = currentToken();
    return make_unique<CharLiteral>(char_tok);
}

// String literal parse function
unique_ptr<Expression> Parser::parseStringLiteral()
{
    Token string_tok = currentToken();
    return make_unique<StringLiteral>(string_tok);
}

// Grouped expression parse function
unique_ptr<Expression> Parser::parseGroupedExpression()
{
    Token lparen = currentToken();
    advance();

    if (currentToken().type == TokenType::RPAREN)
    {
        cerr << "[ERROR] Empty grouped expression after '('\n";
        return nullptr;
    }

    auto expr = parseExpression(Precedence::PREC_NONE);
    if (!expr)
    {
        cerr << "[ERROR] Failed to parse expression inside grouped expr.\n";
        return nullptr;
    }

    if (currentToken().type != TokenType::RPAREN)
    {
        cerr << "[ERROR] Expected ')' to close grouped expression, got: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }

    advance();
    return expr;
}

unique_ptr<Expression> Parser::parseCallExpression(unique_ptr<Expression> left)
{
    std::cout << "[DEBUG] Entered parseCallExpression for: " << left->toString() << "\n";
    Token call_token = currentToken(); // We expect a left parenthesis here

    if (call_token.type != TokenType::LPAREN)
    { // Checking if we encounter the left parenthesis after the function name has been declared
        cout << "Expected ( after " << left->toString() << endl;
        return nullptr;
    }

    advance(); // Advancing the pointer to look at what is inside the brackets

    auto args = parseCallArguments(); // Calling the parse call arguments inorder to parse the arguments

    if (currentToken().type != TokenType::RPAREN)
    {
        cerr << "Expected ')' after arguments in function call, but got '"
             << currentToken().TokenLiteral << "'\n";
    }

    advance();

    return make_unique<CallExpression>(call_token, move(left), move(args));
}

vector<unique_ptr<Expression>> Parser::parseCallArguments()
{
    vector<unique_ptr<Expression>> args;
    if (currentToken().type == TokenType::RPAREN)
    {
        return args;
    }

    auto firstArg = parseExpression(Precedence::PREC_NONE);
    if (!firstArg) {
        std::cerr << "Failed to parse first function argument.\n";
        return args;
    }
    args.push_back(std::move(firstArg));

    while (currentToken().type == TokenType::COMMA)
    {
        advance();
        auto arg = parseExpression(Precedence::PREC_NONE);
        if (!arg) {
            std::cerr << "Failed to parse function argument after comma.\n";
            return args;
        }
        args.push_back(std::move(arg));
    }

    if (nextToken().type == TokenType::RPAREN) {
        advance(); // consume RPAREN
    } else {
        std::cerr << "Expected ')' after function arguments but got '" 
                  << nextToken().TokenLiteral << "'\n";
    }

    return args;
}

// Parsing block expressions
unique_ptr<Expression> Parser::parseBlockExpression()
{
    Token lbrace = currentToken();
    cout << "[DEBUG]: Encountered the {" << endl;
    if (lbrace.type != currentToken().type)
    {
        cout << "[ERROR]: Encountered " << currentToken().TokenLiteral << endl;
        return nullptr;
    }
    advance();
    auto block = make_unique<BlockExpression>(lbrace);
    while (currentToken().type != TokenType::RBRACE)
    {
        if (currentToken().type == TokenType::END)
        {
            cout << "[ERROR] Unterminated block experession" << endl;
            return nullptr;
        }

        if (isStatementStart(currentToken()))
        {
            auto stmt = parseStatement();
            if (stmt)
            {
                block->statements.push_back(move(stmt));
            }
            else
            {
                auto expr = parseExpression(Precedence::PREC_NONE);
                if (expr)
                {
                    block->finalexpr = move(expr);
                }
                break;
            }
        }
    }

    if (currentToken().type != TokenType::RBRACE)
    {
        cout << "[ERROR]Expected } got" << currentToken().TokenLiteral << endl;
        return nullptr;
    }

    advance();
    return block;
}

// Parsing block statements
unique_ptr<Statement> Parser::parseBlockStatement()
{
    Token lbrace = currentToken();
    if (lbrace.type != TokenType::LBRACE)
    {
        cerr << "[ERROR] Expected '{' to start block.\n";
        return nullptr;
    }
    advance();
    vector<unique_ptr<Statement>> statements;

    while (currentToken().type != TokenType::RBRACE && currentToken().type != TokenType::END)
    {
        auto stmt = parseStatement();
        if (stmt != nullptr)
        {
            statements.push_back(move(stmt));
        }
        else
        {
            cerr << "[ERROR] Failed to parse statement within block. Skipping token: " << currentToken().TokenLiteral << endl;
            advance();
        }
    }

    if (currentToken().type != TokenType::RBRACE)
    {
        cerr << "[ERROR]Expected } to close block got: " << currentToken().TokenLiteral << endl;
        return nullptr;
    }

    advance();

    return make_unique<BlockStatement>(lbrace, move(statements));
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

// Checking for statement starters
bool Parser::isStatementStart(const Token &token)
{
    if (token.type == TokenType::FUNCTION || token.type == TokenType::RETURN || token.type == TokenType::WHILE)
    {
        return true;
    }
    else
    {
        return false;
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
    InfixParseFunctionsMap[TokenType::GREATER_THAN] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::LESS_THAN] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::GT_OR_EQ] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::LT_OR_EQ] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::LPAREN] = &Parser::parseCallExpression;
}

// Registering prefix functions for a particular token type
void Parser::registerPrefixFns()
{
    PrefixParseFunctionsMap[TokenType::INTEGER] = &Parser::parseIntegerLiteral;
    PrefixParseFunctionsMap[TokenType::TRUE] = &Parser::parseBooleanLiteral;
    PrefixParseFunctionsMap[TokenType::FALSE] = &Parser::parseBooleanLiteral;
    PrefixParseFunctionsMap[TokenType::FLOAT] = &Parser::parseFloatLiteral;
    PrefixParseFunctionsMap[TokenType::CHAR] = &Parser::parseCharLiteral;
    PrefixParseFunctionsMap[TokenType::STRING] = &Parser::parseStringLiteral;
    PrefixParseFunctionsMap[TokenType::IDENTIFIER] = &Parser::parseIdentifier;
    PrefixParseFunctionsMap[TokenType::LPAREN] = &Parser::parseGroupedExpression;
    PrefixParseFunctionsMap[TokenType::LBRACE] = &Parser::parseBlockExpression;
}

// Registering keyword parsing functions
void Parser::registerKeywordParseFns()
{
}

// Registering the statement parsing functions
void Parser::registerStatementParseFns()
{
    StatementParseFunctionsMap[TokenType::ASSIGN] = &Parser::parseLetStatementWithType;
    StatementParseFunctionsMap[TokenType::RETURN] = &Parser::parseReturnStatement;
    StatementParseFunctionsMap[TokenType::IF] = &Parser::parseIfStatement;
    StatementParseFunctionsMap[TokenType::WHILE] = &Parser::parseWhileStatement;
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
