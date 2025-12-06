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
    Token memberToken = currentToken();

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

    return std::make_unique<EnumMember>(memberToken, name, std::move(value));
}

std::unique_ptr<Statement> Parser::parseEnumClassStatement()
{
    bool isExportable = false;
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

    return std::make_unique<EnumClassStatement>(isExportable, enum_token, class_token, std::move(enum_ident), int_token, std::move(enum_block));
}

std::unique_ptr<Statement> Parser::parseGenericStatement()
{
    Token generic_token = currentToken();
    advance(); // consume 'generic'

    // name
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected identifier for generic name but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    auto genericIdent = parseIdentifier();

    // type param list
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' after generic name but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    advance(); // consume '('

    std::vector<Token> types;

    while (currentToken().type != TokenType::RPAREN &&
           currentToken().type != TokenType::END)
    {
        if (currentToken().type == TokenType::IDENTIFIER)
        {
            types.push_back(currentToken());
            advance();
        }
        else if (currentToken().type == TokenType::COMMA)
        {
            advance(); // consume comma
        }
        else
        {
            logError("Unexpected token in generic parameters: '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
    }

    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' to close generic parameter list");
        return nullptr;
    }
    advance(); // consume ')'

    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected '{' to start generic block but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    auto block = parseBlockStatement();

    return std::make_unique<GenericStatement>(
        generic_token,
        std::move(genericIdent),
        types,
        std::move(block));
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
    // Expect '('
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' after instantiate identifier");
        return nullptr;
    }
    advance(); // consume '('

    std::vector<Token> types;

    // Parse argument list
    while (currentToken().type != TokenType::RPAREN &&
           currentToken().type != TokenType::END)
    {
        // Must be a type token (basic OR custom)
        if (currentToken().type == TokenType::IDENTIFIER ||
            isBasicType(currentToken().type))
        {
            types.push_back(currentToken());
            advance();
        }
        else
        {
            logError("Unexpected token in generic call: " + currentToken().TokenLiteral);
            return nullptr;
        }

        // If comma → eat it and continue
        if (currentToken().type == TokenType::COMMA)
        {
            advance();
            continue;
        }

        // If not comma, next MUST be ')'
        if (currentToken().type != TokenType::RPAREN)
        {
            logError("Expected ',' or ')' but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
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

std::unique_ptr<Expression> Parser::parseArrayType()
{
    // Check if we have an 'arr' keyword
    if (currentToken().type == TokenType::ARRAY)
    {
        Token arr_token = currentToken();
        advance(); // consume 'arr'

        // Expect '['
        if (currentToken().type != TokenType::LBRACKET)
        {
            logError("Expected '[' after 'arr' keyword but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
        advance(); // consume '['

        // Parse **only a basic type or identifier** as inner type
        std::unique_ptr<Expression> innerType;
        if (isBasicType(currentToken().type) || currentToken().type == TokenType::IDENTIFIER)
        {
            innerType = parseBasicType();
        }
        else
        {
            logError("Arrays can only contain a basic type or custom type, got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }

        // Check for nullability '?'
        bool isNullable = false;
        if (currentToken().type == TokenType::QUESTION_MARK)
        {
            isNullable = true;
            advance(); // consume '?'
        }

        // Expect ']'
        if (currentToken().type != TokenType::RBRACKET)
        {
            logError("Expected ']' to close array type but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
        advance(); // consume ']'

        // Return a node representing this array type
        return std::make_unique<ArrayType>(arr_token, std::move(innerType), isNullable);
    }
    else if (isBasicType(currentToken().type) || currentToken().type == TokenType::IDENTIFIER)
    {
        // Simple basic type
        return parseBasicType();
    }

    logError("Expected array type or basic type but got '" + currentToken().TokenLiteral + "'");
    return nullptr;
}

std::unique_ptr<Statement> Parser::parseArrayStatement(bool isParam)
{
    bool isHeap = false;
    Mutability mutability = Mutability::IMMUTABLE;
    if (currentToken().type == TokenType::MUT)
    {
        mutability = Mutability::MUTABLE;
        advance(); // Consume the 'mut'
    }
    else if (currentToken().type == TokenType::CONST)
    {
        mutability = Mutability::CONSTANT;
        advance(); // Consume the const
    }

    // Parse the array type (arr[...] with basic or custom type inside)
    auto arrTypeNode = parseArrayType();
    if (!arrTypeNode)
        return nullptr;

    // Parse the lengths

    // Optional single dimension [size]
    std::unique_ptr<Expression> lengthExpr = nullptr;
    std::vector<std::unique_ptr<Expression>> lengths;

    while (currentToken().type == TokenType::LBRACKET)
    {
        advance(); // consume '['

        std::unique_ptr<Expression> lengthExpr;
        lengthExpr = parseExpression(Precedence::PREC_NONE);

        if (!lengthExpr)
            return nullptr;

        if (currentToken().type != TokenType::RBRACKET)
        {
            logError("Expected ']' after array length but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
        advance(); // consume ']'

        lengths.push_back(std::move(lengthExpr));
    }

    // Expect identifier
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected array name but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    auto ident = parseIdentifier(); // this consumes the IDENTIFIER

    // Optional initializer
    std::unique_ptr<Expression> items = nullptr;
    if (currentToken().type == TokenType::ASSIGN)
    {
        advance(); // consume '='
        items = parseExpression(Precedence::PREC_NONE);
        if (!items)
            return nullptr;
    }

    // Final semicolon (only for non-parameter mode)
    if (!isParam)
    {
        if (currentToken().type == TokenType::SEMICOLON)
        {
            advance();
        }
        else
        {
            logError("Expected a semicolon but got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }
    else
    {
        // If it's a function parameter, we expect either ')' or ',' next
        if (currentToken().type != TokenType::COMMA && currentToken().type != TokenType::RPAREN)
        {
            logError("Expected ',' or ')' after parameter declaration but got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }

    return std::make_unique<ArrayStatement>(
        isHeap,
        mutability,
        std::move(arrTypeNode),
        std::move(lengths),
        std::move(ident),
        std::move(items));
}

std::unique_ptr<Statement> Parser::parseArrayStatementWrapper()
{
    return parseArrayStatement();
}

std::unique_ptr<Expression> Parser::parseArraySubscript()
{
    auto ident = parseIdentifier();
    std::vector<std::unique_ptr<Expression>> indexes;

    while (currentToken().type == TokenType::LBRACKET)
    {
        advance(); // Consume [
        std::unique_ptr<Expression> index;
        index = parseExpression(Precedence::PREC_NONE);

        if (!index)
            return nullptr;

        if (currentToken().type != TokenType::RBRACKET)
        {
            logError("Expected ']' after index but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
        advance(); // Consume ]
        indexes.push_back(std::move(index));
    }

    return std::make_unique<ArraySubscript>(std::move(ident), std::move(indexes));
}

std::unique_ptr<Statement> Parser::parseAliasStatement()
{
    Token alias = currentToken();

    advance(); // Consume alias token

    auto aliasName = parseIdentifier();
    if (currentToken().type != TokenType::ASSIGN)
    {
        logError("Expected '=' after alias name but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    advance(); // Consume '='

    auto type = parseReturnType();
    if (currentToken().type != TokenType::SEMICOLON)
    {
        logError("Expected ';' after alias statement but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    return std::make_unique<AliasStatement>(alias, std::move(aliasName), std::move(type));
}

std::unique_ptr<Statement> Parser::parseFieldAssignment()
{
    std::unique_ptr<Expression> value = nullptr;
    Token current = currentToken();
    advance(); // Consume for example x
    if (currentToken().type == TokenType::SCOPE_OPERATOR || currentToken().type == TokenType::FULLSTOP)
    {
        std::cout << "SPECIAL IDENTIFIER CASE TRIGGERED\n";
        std::string fieldName = current.TokenLiteral;
        std::string operatorLiteral =
            (currentToken().type == TokenType::SCOPE_OPERATOR) ? "::" : ".";
        fieldName += operatorLiteral;

        advance(); // consume scope or dot operator

        if (currentToken().type != TokenType::IDENTIFIER)
        {
            logError("Expected an identifier after '" + operatorLiteral + "' but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }

        fieldName += currentToken().TokenLiteral;
        current.TokenLiteral = fieldName;

        advance(); // consume second identifier
    }

    if (currentToken().type != TokenType::ASSIGN)
    {
        logError("Expected '=' after identifier in assignment but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    advance(); // consume '='

    // Parse right-hand side expression
    value = parseExpression(Precedence::PREC_NONE);

    return std::make_unique<FieldAssignment>(current, std::move(value));
}

std::unique_ptr<Statement> Parser::parseReferenceStatement(bool isParam)
{
    Mutability mut = Mutability::IMMUTABLE;
    std::unique_ptr<Expression> type;

    Token ref_token = currentToken();
    advance(); // Consume 'ref'

    // Check mutability
    if (currentToken().type == TokenType::MUT)
    {
        mut = Mutability::MUTABLE;
        advance();
    }
    else if (currentToken().type == TokenType::CONST)
    {
        mut = Mutability::CONSTANT;
        advance();
    }

    if (currentToken().type == TokenType::AUTO)
    {
        logError("Do not use 'auto' if u want to infer the type just dont include the type");
        advance(); // Consume auto
    }

    // Parse optional type
    // Only treat as type if followed by another identifier (like "int x")
    if (isBasicType(currentToken().type) || currentToken().type == TokenType::ARRAY ||
        (currentToken().type != TokenType::IDENTIFIER && nextToken().type == TokenType::IDENTIFIER))
    {
        type = parseReturnType();
    }

    // Parse identifier (the referer)
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected reference name but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    std::unique_ptr<Expression> ident = parseIdentifier();

    // Optional initializer
    std::unique_ptr<Expression> value = nullptr;
    if (currentToken().type == TokenType::ARROW)
    {
        advance(); // Consume =>
        value = parseExpression(Precedence::PREC_NONE);
        if (!value)
            return nullptr;
    }

    // Final semicolon (only for non-parameter mode)
    if (!isParam)
    {
        if (currentToken().type == TokenType::SEMICOLON)
        {
            advance();
        }
        else
        {
            logError("Expected a semicolon but got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }
    else
    {
        // If it's a function parameter, we expect either ')' or ',' next
        if (currentToken().type != TokenType::COMMA && currentToken().type != TokenType::RPAREN)
        {
            logError("Expected ',' or ')' after parameter declaration but got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }
    return std::make_unique<ReferenceStatement>(
        ref_token,
        mut,
        std::move(type),
        std::move(ident),
        std::move(value));
}

std::unique_ptr<Statement> Parser::parsePointerStatement(bool isParam)
{
    Mutability mut = Mutability::IMMUTABLE;
    std::unique_ptr<Expression> type;

    Token ptr_token = currentToken();
    advance(); // Consume 'ptr'

    // Check mutability
    if (currentToken().type == TokenType::MUT)
    {
        mut = Mutability::MUTABLE;
        advance();
    }
    else if (currentToken().type == TokenType::CONST)
    {
        mut = Mutability::CONSTANT;
        advance();
    }

    if (currentToken().type == TokenType::AUTO)
    {
        logError("Do not use 'auto' if u want to infer the type just dont include the type");
        advance(); // Consume auto
    }

    // Parse optional type
    // Only treat as type if followed by another identifier (like "int x")
    if (isBasicType(currentToken().type) || currentToken().type == TokenType::ARRAY ||
        (currentToken().type != TokenType::IDENTIFIER && nextToken().type == TokenType::IDENTIFIER))
    {
        type = parseReturnType();
    }

    // Parse identifier (the referer)
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected pointer name but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    std::unique_ptr<Expression> ident = parseIdentifier();

    // Optional initializer
    std::unique_ptr<Expression> value = nullptr;
    if (currentToken().type == TokenType::ARROW)
    {
        advance(); // Consume =>
        value = parseExpression(Precedence::PREC_NONE);
        if (!value)
            return nullptr;
    }

    // Final semicolon (only for non-parameter mode)
    if (!isParam)
    {
        if (currentToken().type == TokenType::SEMICOLON)
        {
            advance();
        }
        else
        {
            logError("Expected a semicolon but got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }
    else
    {
        // If it's a function parameter, we expect either ')' or ',' next
        if (currentToken().type != TokenType::COMMA && currentToken().type != TokenType::RPAREN)
        {
            logError("Expected ',' or ')' after parameter declaration but got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }
    return std::make_unique<PointerStatement>(
        ptr_token,
        mut,
        std::move(type),
        std::move(ident),
        std::move(value));
}

std::unique_ptr<Expression> Parser::parseUnwrapExpression()
{
    advance(); // Consume the unwrap token
    auto callExpr = parseExpression(Precedence::PREC_NONE);

    return std::make_unique<UnwrapExpression>(std::move(callExpr));
}

std::unique_ptr<Statement> Parser::parseQualifyStatement()
{
    Token qualify_token = currentToken();
    advance(); // Consume the qualify token

    std::unique_ptr<Expression> expr = parseIdentifier();

    return std::make_unique<QualifyStatement>(qualify_token, std::move(expr));
}

std::unique_ptr<Statement> Parser::parseMergeStatement()
{
    Token merge_token = currentToken();
    advance(); // Consume the merge token
    auto merged = parseStringLiteral();

    return std::make_unique<MergeStatement>(merge_token, std::move(merged));
}

std::unique_ptr<Statement> Parser::parsePointerStatementWrapper()
{
    return parsePointerStatement();
}

std::unique_ptr<Statement> Parser::parseReferenceStatementWrapper()
{
    return parseReferenceStatement();
}

std::unique_ptr<Statement> Parser::parseShoutStatement()
{
    Token shout = currentToken();
    advance(); // Consume the shout token
    if (currentToken().type != TokenType::BANG)
    {
        logError("Expected '!' but got '" + currentToken().TokenLiteral + "'");
        advance(); // Consume whatever is there
    }
    advance(); // Consume the ! token
    auto expr = parseExpression(Precedence::PREC_NONE);

    return std::make_unique<ShoutStatement>(shout, std::move(expr));
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