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
// Parsing enum member node
std::unique_ptr<EnumMember> Parser::parseEnumMember()
{
    // Check if the current token is an identifier
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Enum member must start with an identifier, got: " + TokenTypeToLiteral(currentToken().type));
        return nullptr; // Return nullptr but don't advance to allow recovery in caller
    }

    std::string name = currentToken().TokenLiteral;
    advance(); // Consume the identifier
    std::unique_ptr<Expression> value = nullptr;

    // Handle optional value assignment
    if (currentToken().type == TokenType::ASSIGN)
    {
        advance(); // Consume the assignment token
        if (!isIntegerLiteralType(currentToken().type))
        {
            logError("Enum member value must be an integer, got: " + TokenTypeToLiteral(currentToken().type));
            return nullptr; // Return nullptr without advancing
        }
        // Parse the expression for the integer value
        value = parseExpression(Precedence::PREC_NONE);
        if (!value)
        {
            logError("Failed to parse enum member value");
            return nullptr;
        }
        // No need for manual advance here; parseExpression should handle token consumption
    }

    return std::make_unique<EnumMember>(name, std::move(value));
}

std::unique_ptr<Statement> Parser::parseEnumClassStatement()
{
    std::vector<std::unique_ptr<EnumMember>> enum_block;
    std::optional<Token> int_token;
    Token enum_token = currentToken();
    advance(); // Consume the enum keyword token

    // Check for class keyword
    Token class_token = currentToken();
    if (class_token.type != TokenType::CLASS)
    {
        logError("Expected keyword 'class' after 'enum', got: " + currentToken().TokenLiteral);
        return nullptr;
    }
    advance(); // Consume the class keyword token

    // Parse the enum class's name
    auto enum_ident = parseIdentifier();
    if (!enum_ident)
    {
        logError("Expected identifier for enum class name, got: " + currentToken().TokenLiteral);
        return nullptr;
    }

    // Handle optional underlying type
    if (currentToken().type == TokenType::COLON)
    {
        advance(); // Consume the colon token
        if (!isIntegerType(currentToken().type))
        {
            logError("Expected integer type after colon in enum class, got: " + TokenTypeToLiteral(currentToken().type));
            return nullptr;
        }
        int_token = currentToken();
        advance(); // Consume the integer type token
    }

    // Check for opening brace
    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected '{' for enum class body, got: " + currentToken().TokenLiteral);
        return nullptr;
    }
    advance(); // Consume the { token

    // Parse enum members
    while (currentToken().type != TokenType::RBRACE && currentToken().type != TokenType::END)
    {
        auto enumClassMember = parseEnumMember();
        if (!enumClassMember)
        {
            // Skip to the next comma, closing brace, or end to recover
            while (currentToken().type != TokenType::COMMA &&
                   currentToken().type != TokenType::RBRACE &&
                   currentToken().type != TokenType::END)
            {
                advance();
            }
        }
        else
        {
            enum_block.push_back(std::move(enumClassMember));
        }

        // Handle comma or unexpected tokens
        if (currentToken().type == TokenType::COMMA)
        {
            advance(); // Consume the comma
        }
        else if (currentToken().type == TokenType::SEMICOLON)
        {
            logError("Semicolons are not allowed inside enum class; use commas to separate members");
            advance(); // Consume the semicolon to continue parsing
        }
        else if (currentToken().type != TokenType::RBRACE)
        {
            logError("Expected ',' or '}' after enum member, got: " + currentToken().TokenLiteral);
            // Don't advance here; let the loop check the next token
        }
    }

    // Check for closing brace
    if (currentToken().type != TokenType::RBRACE)
    {
        logError("Expected '}' to close enum class, got: " + currentToken().TokenLiteral);
        return nullptr;
    }
    advance(); // Consume the } token

    // Handle optional semicolon
    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance(); // Consume the semicolon
    }

    return std::make_unique<EnumClassStatement>(enum_token, class_token, std::move(enum_ident), int_token, std::move(enum_block));
}

std::unique_ptr<Statement> Parser::parseGenericStatement()
{
    Token generic_token = currentToken();
    advance(); // Consume the keyword generic

    // Getting the generic name
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected 'identifier' but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    auto genericIdent = parseIdentifier();

    // Dealing with the type parameters
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' but got " + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    std::vector<Token> types;
    while (currentToken().type != TokenType::RPAREN && currentToken().type != TokenType::END)
    {
        advance();
        if (currentToken().type == TokenType::IDENTIFIER)
        {
            types.push_back(currentToken());
        }
        else if (currentToken().type != TokenType::COMMA)
        {
            logError("Unexpected token in generic parameters: " + currentToken().TokenLiteral);
            return nullptr;
        }
        advance();
    }

    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' to close argument list");
        return nullptr;
    }
    advance();

    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected '{' to start init block but got: " + currentToken().TokenLiteral);
        return nullptr;
    }

    auto block = parseBlockStatement();
    advance();

    return std::make_unique<GenericStatement>(generic_token, std::move(genericIdent), types, std::move(block));
}

std::unique_ptr<Statement> Parser::parseInstantiateStatement()
{
    Token instantiate = currentToken();
    advance();

    // Dealing with the generic call
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected 'identifier' for  generic call but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    auto ident = parseIdentifier();
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    std::vector<Token> types;
    while (currentToken().type != TokenType::RPAREN && currentToken().type != TokenType::END)
    {
        advance();
        if (isBasicType(currentToken().type))
        {
            types.push_back(currentToken());
        }
        else if (currentToken().type != TokenType::COMMA)
        {
            logError("Unexpected token in generic call parameters: " + currentToken().TokenLiteral);
        }
        advance();
    }

    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' to close argument list");
        return nullptr;
    }
    auto generic_call = std::make_unique<GenericCall>(std::move(ident), types);

    advance();

    if (currentToken().type != TokenType::AS)
    {
        logError("Expected 'as' but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    Token as = currentToken();
    advance();

    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected 'identifier' but got '" + currentToken().TokenLiteral + "'");
    }
    Token alias = currentToken();
    advance();

    return std::make_unique<InstantiateStatement>(instantiate, std::move(generic_call), as, alias);
}

std::unique_ptr<Expression> Parser::parseArrayLiteral()
{
    Token arr_tok = currentToken();
    advance(); // consume '['

    std::vector<std::unique_ptr<Expression>> array;

    while (currentToken().type != TokenType::RBRACKET &&
           currentToken().type != TokenType::END)
    {
        // Parse element
        auto expr = parseExpression(Precedence::PREC_NONE);
        array.push_back(std::move(expr));

        if (currentToken().type == TokenType::COMMA)
        {
            advance(); // consume ',' and continue
        }
        else if (currentToken().type != TokenType::RBRACKET)
        {
            logError("Unexpected token in array literal: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }

    if (currentToken().type != TokenType::RBRACKET)
    {
        logError("Expected ']' to close array literal");
        return nullptr;
    }
    advance(); // consume ']'

    return std::make_unique<ArrayLiteral>(arr_tok, std::move(array));
}

std::unique_ptr<Statement> Parser::parseArrayStatement()
{
    Token arr_token = currentToken();
    advance(); 

    std::vector<std::unique_ptr<Expression>> lengths;

    while (currentToken().type == TokenType::LBRACKET &&
           currentToken().type != TokenType::END)
    {
        advance(); // consume '['

        if (currentToken().type == TokenType::UINT ||
            currentToken().type == TokenType::ULONG)
        {
            auto len_no = parseExpression(Precedence::PREC_NONE);
            lengths.push_back(std::move(len_no));
        }
        else if (currentToken().type != TokenType::RBRACKET)
        {
            logError("Unexpected token in array length '" + currentToken().TokenLiteral+"' please only use unsigned int(32) or unsigned long(64)");
            return nullptr;
        }

        if (currentToken().type != TokenType::RBRACKET)
        {
            logError("Expected ']' after array length");
            return nullptr;
        }
        advance(); // consume ']'
    }

    // Expect type
    if (!isBasicType(currentToken().type) &&
        currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected array type but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    Token arr_type = currentToken();
    advance();

    // Expect identifier
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected array name but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    auto ident = parseIdentifier();

    std::unique_ptr<Expression> list = nullptr;

    // No initializer
    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance(); // consume ';'
        return std::make_unique<ArrayStatement>(
            arr_token, std::move(lengths), arr_type, std::move(ident), std::move(list));
    }

    // With initializer
    if (currentToken().type != TokenType::ASSIGN)
    {
        logError("Expected '=' but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    advance(); // consume '='

    list = parseArrayLiteral();

    if (currentToken().type != TokenType::SEMICOLON)
    {
        logError("Expected ';' after array initializer");
        return nullptr;
    }
    advance(); // consume ';'

    return std::make_unique<ArrayStatement>(
        arr_token, std::move(lengths), arr_type, std::move(ident), std::move(list));
}

bool Parser::isIntegerType(TokenType type)
{
    switch (type)
    {
    case TokenType::SHORT_KEYWORD:
    case TokenType::USHORT_KEYWORD:
    case TokenType::INTEGER_KEYWORD:
    case TokenType::UINT_KEYWORD:
    case TokenType::LONG_KEYWORD:
    case TokenType::ULONG_KEYWORD:
    case TokenType::EXTRA_KEYWORD:
    case TokenType::UEXTRA_KEYWORD:
        return true;
    default:
        return false;
    }
}

bool Parser::isIntegerLiteralType(TokenType type)
{
    switch (type)
    {
    case TokenType::SHORT:
    case TokenType::USHORT:
    case TokenType::INT:
    case TokenType::UINT:
    case TokenType::LONG:
    case TokenType::ULONG:
    case TokenType::EXTRA:
    case TokenType::UEXTRA:
        return true;
    default:
        return false;
    }
}