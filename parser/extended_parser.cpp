#include "ast.hpp"
#include "parser.hpp"

#define CPPREST_FORCE_REBUILD

//-----------------------SWITCH STATEMENT SUPPORT---------------------------//
// Parsing case clause
std::unique_ptr<Statement> Parser::parseCaseClause()
{
    std::vector<std::unique_ptr<Statement>> body;
    Token case_token = currentToken();
    if (case_token.type != TokenType::CASE)
    {
        logError("Expected case keyword but got: " + currentToken().TokenLiteral);
    }

    advance();

    auto condition = parseExpression(Precedence::PREC_NONE);

    if (currentToken().type != TokenType::COLON)
    {
        logError("Expected : after case condition but got: " + currentToken().TokenLiteral);
    }

    advance();

    while (currentToken().type != TokenType::CASE && currentToken().type != TokenType::DEFAULT && currentToken().type != TokenType::RBRACE)
    {
        auto stmt = parseStatement();
        if (stmt)
        {
            body.push_back(std::move(stmt));
        }
        else
        {
            advance();
        }
    }

    return std::make_unique<CaseClause>(case_token, std::move(condition), std::move(body));
}

// Parsing switch statement
std::unique_ptr<Statement> Parser::parseSwitchStatement()
{
    std::vector<std::unique_ptr<Statement>> case_clauses;
    std::vector<std::unique_ptr<Statement>> default_body;

    Token default_token;

    Token switch_token = currentToken();
    advance(); // Advancing past the switch keyword
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected ( after switch keyword but got: " + currentToken().TokenLiteral);
    }

    advance();                                                // Consuming the ( token
    auto switchExpr = parseExpression(Precedence::PREC_NONE); // I expect whatever parses this to advance
    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ) but got: " + currentToken().TokenLiteral);
    }

    advance(); // Consume the ) token;
    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected { but got: " + currentToken().TokenLiteral);
    }

    advance(); // Consume the { token;

    while (currentToken().type != TokenType::RBRACE)
    {
        if (currentToken().type == TokenType::CASE)
        {
            auto case_clause = parseCaseClause();
            case_clauses.push_back(std::move(case_clause));
        }
        else if (currentToken().type == TokenType::DEFAULT)
        {
            default_token = currentToken();
            advance(); // Consume DEFAULT
            if (currentToken().type != TokenType::COLON)
            {
                logError("Expected : after default keyword but got: " + currentToken().TokenLiteral);
            }
            advance(); // Consume COLON
            while (currentToken().type != TokenType::CASE &&
                   currentToken().type != TokenType::DEFAULT &&
                   currentToken().type != TokenType::RBRACE)
            {
                auto stmt = parseStatement();
                if (stmt)
                {
                    default_body.push_back(std::move(stmt));
                }
                else
                {
                    advance();
                }
            }
        }
        else
        {
            logError("Unexpected token in switch statement: " + currentToken().TokenLiteral);
            advance();
        }
    }

    if (default_token.type != TokenType::DEFAULT)
    {
        logError("Missing the default case");
        return nullptr;
    }

    advance(); // Consume the } token

    return std::make_unique<SwitchStatement>(
        switch_token,
        std::move(switchExpr),
        std::move(case_clauses),
        default_token,
        std::move(default_body));
}

// Parsing enum class statement
std::unique_ptr<Statement> Parser::parseEnumClassStatement()
{
    std::vector<std::unique_ptr<Expression>> enum_block;

    Token enum_token = currentToken();
    advance(); // Consume the enum keyword token
    Token class_token = currentToken();
    if (class_token.type != TokenType::CLASS)
    {
        logError("Expected keyword class after keyword enum but got: " + currentToken().TokenLiteral);
    }
    advance();                           // Consume the class keyword token
    auto enum_ident = parseIdentifier(); // Parsing the enum class's name

    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected { but got: " + currentToken().TokenLiteral);
    }
    advance(); // Consume the { token

    while (currentToken().type != TokenType::RBRACE)
    {
        auto enumClassStmt = parseExpression(Precedence::PREC_NONE);
        enum_block.push_back(std::move(enumClassStmt));
        if (currentToken().type == TokenType::COMMA)
        {
            advance();
        }
        if(currentToken().type==TokenType::SEMICOLON){
            logError("Please do not use semi colons inside enum class use :");
            advance();
        }
    }

    if (currentToken().type != TokenType::RBRACE)
    {
        logError("Expected } but got: " + currentToken().TokenLiteral);
    }
    advance(); // Consume the } token

    return std::make_unique<EnumClassStatement>(enum_token, class_token, std::move(enum_ident), std::move(enum_block));
}