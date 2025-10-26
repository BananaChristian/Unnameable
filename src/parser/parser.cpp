#include "parser.hpp"
#include "ast.hpp"
#include "token/token.hpp"
#include <iostream>
#include <memory>
#include <vector>

#define CPPREST_FORCE_REBUILD

//--------------PARSER CLASS CONSTRUCTOR-------------
Parser::Parser(std::vector<Token> &tokenInput) : tokenInput(tokenInput), currentPos(0), nextPos(1)
{
    lastToken = tokenInput.empty() ? Token{"", TokenType::ILLEGAL, 999, 999} : tokenInput[0];
    registerInfixFns();
    registerPrefixFns();
    registerPostfixFns();
    registerStatementParseFns();
}

// MAIN PARSER FUNCTION
std::vector<std::unique_ptr<Node>> Parser::parseProgram()
{
    std::vector<std::unique_ptr<Node>> program;

    while (currentPos < tokenInput.size())
    {
        // Skip standalone semicolons between statements
        if (currentToken().type == TokenType::SEMICOLON)
        {
            advance();
            continue;
        }

        std::cout << "Parsing token: " << currentToken().TokenLiteral << "\n";
        Token current = currentToken();

        if (current.type == TokenType::END)
            break;

        std::unique_ptr<Node> node = parseStatement();

        if (node)
        {
            program.push_back(std::move(node));
        }
        else
        {
            // only advance on null to avoid infinite loop
            advance();
        }
    }

    std::cout << "Parser finished\n";
    return program;
}

//------------PARSING FUNCTIONS SECTION----------
//-----------PARSING STATEMENTS----------
// General statement parser function
std::unique_ptr<Statement> Parser::parseStatement()
{
    Token current = currentToken();
    std::cout << "[DEBUG] parseStatement starting with token: " << current.TokenLiteral << "\n";

    // Handle self.* assignments
    if (current.type == TokenType::SELF)
    {
        Token peek1 = peekToken(1);
        Token peek3 = peekToken(3);
        if (peek3.type == TokenType::ASSIGN)
        {
            return parseAssignmentStatement();
        }
    }

    // Other statement types from map
    auto stmtFnIt = StatementParseFunctionsMap.find(current.type);
    if (stmtFnIt != StatementParseFunctionsMap.end())
    {
        return (this->*stmtFnIt->second)();
    }

    // Fallback expression statement
    auto expr = parseExpression(Precedence::PREC_NONE);
    if (expr)
    {
        if (currentToken().type == TokenType::SEMICOLON)
            advance();
        else
            logError("Expected ';' after expression statement but got '" + currentToken().TokenLiteral + "'");

        return std::make_unique<ExpressionStatement>(current, std::move(expr));
    }

    logError("Unexpected token at start of statement '" + current.TokenLiteral + "'");
    advance();
    return nullptr;
}

// Parsing assignment statements
std::unique_ptr<Statement> Parser::parseAssignmentStatement(bool isParam)
{
    std::cout << "NOW INSIDE ASSIGNEMENT STATEMENT PARSER\n";
    std::unique_ptr<Expression> value = nullptr;
    Token identToken = currentToken();
    bool isQualified = false;

    // self.field = ...
    std::unique_ptr<Expression> lhs;

    if (identToken.type == TokenType::SELF)
    {
        lhs = parseSelfExpression();
    }
    else if (identToken.type == TokenType::DEREF)
    {
        lhs = parseDereferenceExpression();
    }
    else if (identToken.type == TokenType::IDENTIFIER)
    {
        lhs = parseIdentifier(); // consume identifier
    }
    else
    {
        logError("Invalid left-hand side in assignment: '" + identToken.TokenLiteral + "'");
        return nullptr;
    }

    // Expect '=' before parsing the value
    if (currentToken().type != TokenType::ASSIGN)
    {
        logError("Expected '=' after identifier in assignment but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    advance(); // consume '='

    // Parse right-hand side expression
    value = parseExpression(Precedence::PREC_NONE);

    // Consume optional semicolon
    if (!isParam && currentToken().type == TokenType::SEMICOLON)
    {
        advance();
    }
    else if (!isParam)
    {
        logError("Expected ';' after assignment but got '" + currentToken().TokenLiteral + "'");
    }

    if (!lhs)
        return nullptr;

    std::cout << "[LEAVING ASSIGNMENT] Current token: "
              << currentToken().TokenLiteral
              << " (" << (int)currentToken().type << ")\n";

    return std::make_unique<AssignmentStatement>(std::move(lhs), std::move(value));
}

std::unique_ptr<Statement> Parser::parseDereferenceAssignment()
{
    if (peekToken(2).type == TokenType::EQUALS)
        return parseAssignmentStatement();

    // Fall back to the expression guy
    auto expr = parseExpression(Precedence::PREC_NONE);
    if (expr)
    {
        if (currentToken().type == TokenType::SEMICOLON)
            advance();
        else
            logError("Expected ';' after expression statement but got '" + currentToken().TokenLiteral + "'");

        return std::make_unique<ExpressionStatement>(currentToken(), std::move(expr));
    }
    return nullptr;
}

// Parsing let statements
std::unique_ptr<Statement> Parser::parseLetStatementWithType(bool isParam)
{
    bool isHeap = false;
    Mutability mutability = Mutability::IMMUTABLE;
    bool isNullable = false;
    Token mutability_token = currentToken();

    if (mutability_token.type == TokenType::CONST)
    {
        mutability = Mutability::CONSTANT;
        std::cout << "CURRENT TOKEN: " << mutability_token.TokenLiteral << "\n";
        advance();
        mutability_token = currentToken();
    }
    else if (mutability_token.type == TokenType::MUT)
    {
        mutability = Mutability::MUTABLE;
        std::cout << "CURRENT TOKEN: " << mutability_token.TokenLiteral << "\n";
        advance();
        mutability_token = currentToken();
    }

    Token dataType_token = currentToken();
    std::cout << "[DEBUG] Data type token: " + dataType_token.TokenLiteral << "\n";
    advance();

    if (currentToken().type == TokenType::QUESTION_MARK)
    {
        isNullable = true;
        advance();
    }

    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected variable name after data type but got: " + currentToken().TokenLiteral);
        return nullptr;
    }

    Token ident_token = currentToken();
    advance();

    std::optional<Token> assign_token;
    std::unique_ptr<Expression> value = nullptr;

    if (currentToken().type == TokenType::ASSIGN)
    {
        assign_token = currentToken();
        std::cout << "[DEBUG] Encountered assignment token" << "\n";
        advance();
        value = parseExpression(Precedence::PREC_NONE);
    }
    else if (currentToken().type == TokenType::SEMICOLON)
    {
        std::cout << "[DEBUG] Encountered semicolon token" << "\n";
    }

    if (!isParam)
    {
        if (currentToken().type != TokenType::SEMICOLON)
        {
            logError("Expected a semicolon but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
    }
    else
    {
        // If it's a function parameter, we expect either ')' or ',' next
        if (currentToken().type != TokenType::COMMA && currentToken().type != TokenType::RPAREN && currentToken().type != TokenType::BITWISE_OR)
        {
            logError("Expected ',' or ')' after parameter declaration but got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }

    if (mutability == Mutability::CONSTANT)
    {
        if (value == nullptr)
        {
            logError("Uninitialized const variable");
            return nullptr;
        }
    }

    return std::make_unique<LetStatement>(isHeap, mutability, dataType_token, isNullable, ident_token, assign_token, move(value));
}
// Parsing let statements with custom types
std::unique_ptr<Statement> Parser::parseLetStatementWithCustomType(bool isParam)
{
    std::cout << "INSIDE CUSTOM TYPE PARSER\n";
    Mutability mut = Mutability::IMMUTABLE;
    Token data_type_token;
    bool isNullable = false;
    bool isHeap = false;

    // Checking for mutability
    if (currentToken().type == TokenType::MUT)
    {
        mut = Mutability::MUTABLE;
        advance();
    }
    if (currentToken().type == TokenType::CONST)
    {
        mut = Mutability::CONSTANT;
        advance();
    }

    // Checking for the indentifier token
    if (currentToken().type == TokenType::IDENTIFIER)
    {
        data_type_token = currentToken();
        std::string fullTypeName = data_type_token.TokenLiteral;
        advance(); // Consume the identifier token
        while (currentToken().type == TokenType::SCOPE_OPERATOR)
        {
            advance(); // Consume the scope operator
            if (currentToken().type != TokenType::IDENTIFIER)
            {
                logError("Expected an identifier after  '::'");
                return nullptr;
            }
            fullTypeName += "::" + currentToken().TokenLiteral; // append to full name
            advance();                                          // Consume the identifier
        }
        data_type_token.TokenLiteral = fullTypeName; // Overriding the token's literal to allow storage of IDENTIFIER::IDENTIFIER
    }

    // Check for nullability
    if (currentToken().type == TokenType::QUESTION_MARK)
    {
        isNullable = true;
        advance();
    }

    // Check for the variable name
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected variable name after data type");
        return nullptr;
    }

    Token ident_token = currentToken();
    advance(); // Consume the variable name identifier token

    // Checking if we have a value assigned
    std::optional<Token> assign_token;
    std::unique_ptr<Expression> value = nullptr;

    if (currentToken().type == TokenType::ASSIGN)
    {
        assign_token = currentToken();
        std::cout << "[DEBUG] Encountered assignment token" << "\n";
        advance();
        value = parseExpression(Precedence::PREC_NONE);
    }
    else if (currentToken().type == TokenType::SEMICOLON)
    {
        std::cout << "[DEBUG] Encountered semicolon token" << "\n";
    }

    if (!isParam)
    {
        if (currentToken().type == TokenType::SEMICOLON)
        {
            advance();
        }
        else
        {
            logError("Expected a semicolon but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
    }
    else
    {
        // If it's a function parameter, we expect either ')' or ',' next
        if (currentToken().type != TokenType::COMMA && currentToken().type != TokenType::RPAREN)
        {
            logError("Expected ',' or ')' after parameter declaration but got '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
    }

    if (mut == Mutability::CONSTANT)
    {
        if (value == nullptr)
        {
            logError("Uninitialized const variable");
            return nullptr;
        }
    }

    return std::make_unique<LetStatement>(isHeap, mut, data_type_token, isNullable, ident_token, assign_token, std::move(value));
}

std::unique_ptr<Statement> Parser::parseHeapStatement()
{
    advance(); // consume 'heap'

    // Disallow: heap data ...
    if (currentToken().type == TokenType::DATA)
    {
        logError("Cannot use 'heap' before a data block");
        return nullptr;
    }

    // Disallow: heap array ...
    if (currentToken().type == TokenType::ARRAY)
    {
        logError("Cannot use 'heap' before an array declaration");
        return nullptr;
    }

    auto stmt = parseLetStatementCustomOrBasic();
    if (!stmt)
        return nullptr;

    if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get()))
    {
        letStmt->isHeap = true;
    }
    else
    {
        logError("'heap' can only be applied to let-statements");
        return nullptr;
    }

    return stmt;
}

/*Decider on type of let statement: Now the name of this function is confusing initially I wanted it to be the function that decides how to parse let statements.
But I have no choice  but to make it also decide how to parse a function if it was a parameter now I will fix the name in the future but for now let me just continue
*/
std::unique_ptr<Statement> Parser::parseLetStatementDecider()
{
    Token current = currentToken();

    if (isBasicType(current.type) || current.type == TokenType::AUTO)
    {
        return parseLetStatementWithType(true);
    }
    else if (current.type == TokenType::ARRAY)
    {
        return parseArrayStatement(true);
    }
    else if (current.type == TokenType::REF)
    {
        return parseReferenceStatement(true);
    }
    else if (current.type == TokenType::PTR)
    {
        return parsePointerStatement(true);
    }
    else if (current.type == TokenType::DEREF)
    {
        return parseAssignmentStatement(true);
    }
    else if (current.type == TokenType::IDENTIFIER)
    {
        if (nextToken().type == TokenType::SCOPE_OPERATOR)
        {
            return parseLetStatementWithCustomType(true);
        }
        else if (nextToken().type == TokenType::ASSIGN)
        {
            return parseAssignmentStatement(true);
        }
        else
        {
            return parseLetStatementWithCustomType(true);
        }
    }

    std::cerr << "[ERROR]: Failed to decide how to parse parameter variable. Token: " << current.TokenLiteral << "\n";
    return nullptr;
}

// Checker for basic data types
bool Parser::isBasicType(TokenType type)
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
    case TokenType::FLOAT_KEYWORD:
    case TokenType::DOUBLE_KEYWORD:
    case TokenType::CHAR_KEYWORD:
    case TokenType::CHAR16_KEYWORD:
    case TokenType::CHAR32_KEYWORD:
    case TokenType::STRING_KEYWORD:
    case TokenType::BOOL_KEYWORD:
        return true;
    default:
        return false;
    }
}

// Parsing signal statement
std::unique_ptr<Statement> Parser::parseSignalStatement()
{
    Token signal_token = currentToken();
    advance();
    auto ident = parseIdentifier();
    if (currentToken().type != TokenType::ASSIGN)
    {
        logError("Expected an =");
        return nullptr;
    }
    advance(); // Move past the = sign
    if (currentToken().type != TokenType::START)
    {
        logError("Expected a thread starter");
        return nullptr;
    }
    auto start = parseStartStatement();
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected ( after start keyword");
        return nullptr;
    }
    advance();
    auto func_name = parseIdentifier();
    auto func_arg = parseCallExpression(std::move(func_name));
    advance();
    if (currentToken().type != TokenType::SEMICOLON)
    {
        std::cout << "[ERROR]: Expected ; after ) but got->" << currentToken().TokenLiteral << "\n";
        logError("Expected ; after )");
        return nullptr;
    }

    return std::make_unique<SignalStatement>(signal_token, std::move(ident), std::move(start), std::move(func_arg));
}

// Parsing start statement
std::unique_ptr<Statement> Parser::parseStartStatement()
{
    Token start = currentToken();
    advance();
    return std::make_unique<StartStatement>(start);
}

// Parsing wait statement
std::unique_ptr<Statement> Parser::parseWaitStatement()
{
    Token wait = currentToken();
    advance();
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected ( after wait keyword");
        return nullptr;
    }
    advance();
    auto call = parseIdentifier();
    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ) after argument");
        return nullptr;
    }
    advance();
    if (currentToken().type != TokenType::SEMICOLON)
    {
        logError("Expected ; after )");
        return nullptr;
    }
    return std::make_unique<WaitStatement>(wait, std::move(call));
}

// Parsing use statement
std::unique_ptr<Statement> Parser::parseUseStatement()
{
    std::optional<std::unique_ptr<Expression>> functionCallOrData;
    Token use_token = currentToken();
    advance(); // Consume the use keyword token

    Token kind_token;
    if (currentToken().type == TokenType::DATA || currentToken().type == TokenType::BEHAVIOR)
    {
        kind_token = currentToken();
        advance(); // Consume 'data' or 'behavior'
    }
    else
    {
        logError("Expected either 'data' or 'behavior' keyword but got: " + currentToken().TokenLiteral);
    }

    if (currentToken().type == TokenType::FULLSTOP || currentToken().type == TokenType::SCOPE_OPERATOR)
    {
        logError("Expected data block name but you are starting with a fullstop");
    }

    std::unique_ptr<Expression> expr = parseExpression(Precedence::PREC_NONE);

    // Special check to prevent function calls
    auto callExpr = dynamic_cast<CallExpression *>(expr.get());
    if (callExpr)
    {
        logError("Unexpected function call");
        return nullptr;
    }

    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance();
    }
    else
    {
        logError("Expected ';' after use statement");
    }

    return std::make_unique<UseStatement>(
        use_token,
        kind_token,
        std::move(expr));
}

// Parsing behavior statement
std::unique_ptr<Statement> Parser::parseBehaviorStatement()
{
    std::vector<std::unique_ptr<Statement>> functions;
    Token behavior_token = currentToken();
    advance(); // Consuming the behavior keyword token
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected behavior block name");
    }
    auto behavior_name = parseIdentifier();
    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected { to start behavior block");
    }
    advance(); // Consuming the behavior lparen
    while (currentToken().type != TokenType::RBRACE && currentToken().type != TokenType::END)
    {
        auto funcStmt = parseStatement();
        if (!funcStmt)
        {
            logError("Failed to parse function statement");
            continue;
        }
        if (auto func = dynamic_cast<FunctionStatement *>(funcStmt.get()))
        {
            functions.push_back(std::move(funcStmt));
        }
        else
        {
            logError("Only function statements allowed inside behavior block");
        }
    }
    if (currentToken().type != TokenType::RBRACE)
    {
        logError("Expected } to close behavior block");
    }
    advance();

    return std::make_unique<BehaviorStatement>(
        behavior_token,
        std::move(behavior_name),
        std::move(functions));
}

// Parsing data behavior
std::unique_ptr<Statement> Parser::parseDataStatement()
{
    Mutability mutability = Mutability::IMMUTABLE;
    std::vector<std::unique_ptr<Statement>> fields;
    if (currentToken().type == TokenType::MUT)
    {
        mutability = Mutability::MUTABLE;
        std::cout << "MUTABILITY IS MUTABLE\n";
        advance(); // Consuming the mut keyword token
    }
    else if (currentToken().type == TokenType::CONST)
    {
        mutability = Mutability::CONSTANT;
        std::cout << "MUTABILITY IS CONSTANT\n";
        advance(); // Comsume the const keyword
    }
    Token data_token = currentToken();
    advance(); // Consuming the data keyword token
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected data block name");
    }
    auto dataBlockName = parseIdentifier();
    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected { to start data block");
    }

    advance(); // Consuming the LPAREN

    while (currentToken().type != TokenType::RBRACE && currentToken().type != TokenType::END)
    {
        if (currentToken().type == TokenType::SEMICOLON)
        {
            advance();
            continue;
        }
        auto dataStmt = parseStatement();
        if (auto letStmt = dynamic_cast<LetStatement *>(dataStmt.get()))
        {
            TokenType declaredType = letStmt->data_type_token.type;
            if (isBasicType(declaredType) || declaredType == TokenType::IDENTIFIER || declaredType == TokenType::AUTO)
            {
                fields.push_back(std::move(dataStmt));
            }
            else
            {
                logError("Only type declarations are allowed inside data block. Got: " + dataStmt->token.TokenLiteral);
            }
        }
        else
        {
            logError("Only let statements allowed inside data block");
        }
    }
    if (currentToken().type != TokenType::RBRACE)
    {
        logError("Expected } to close data block but got '" + currentToken().TokenLiteral + "'");
    }
    advance();

    return std::make_unique<DataStatement>(
        mutability,
        data_token,
        std::move(dataBlockName),
        std::move(fields));
}

// Parsing instance expression
std::unique_ptr<Expression> Parser::parseInstanceExpression(std::unique_ptr<Expression> left)
{
    std::cout << "INSIDE INSTANCE PARSE\n";

    std::vector<std::unique_ptr<Statement>> args;

    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected '{' but got '" + currentToken().TokenLiteral + "'");
        advance();
        return nullptr;
    }

    advance(); // consume '{'

    //Check if immediately closed
    if (currentToken().type == TokenType::RBRACE)
    {
        advance(); // consume '}'
        return std::make_unique<InstanceExpression>(std::move(left), std::move(args));
    }

    // Parse the first argument
    std::unique_ptr<Statement> firstArg = parseAssignmentStatement(true);
    if (firstArg)
        args.push_back(std::move(firstArg));

    while (currentToken().type == TokenType::COMMA)
    {
        advance(); // consume ','
        std::unique_ptr<Statement> arg = parseAssignmentStatement(true);
        if (!arg)
        {
            logError("Failed to parse field argument after comma.");
            return nullptr;
        }
        args.push_back(std::move(arg));
    }

    if (currentToken().type == TokenType::RBRACE)
    {
        advance(); // consume '}'
    }
    else
    {
        logError("Expected '}' after instance arguments");
        return nullptr;
    }

    return std::make_unique<InstanceExpression>(std::move(left), std::move(args));
}

// Parsing component statements
std::unique_ptr<Statement> Parser::parseComponentStatement()
{
    std::vector<std::unique_ptr<Statement>> privateData; // An empty vector where the private data if created shall all be stored
    std::vector<std::unique_ptr<Statement>> privateMethods;

    std::vector<std::unique_ptr<Statement>> usedDataBlocks;
    std::vector<std::unique_ptr<Statement>> usedBehaviorBlocks;

    std::optional<std::unique_ptr<Statement>> initConstructor;

    Token component_token = currentToken();
    advance(); // Consume the keyword component
    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected component name");
    }
    auto component_name = parseIdentifier();
    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected { but got '" + currentToken().TokenLiteral + "'");
    }
    advance(); // Consuming the LPAREN
    // Now here we are inside the block so we have to parse the stuff inside

    while (true)
    {
        if (currentToken().type == TokenType::RBRACE ||
            currentToken().type == TokenType::END)
        {
            break; // stop parsing component body
        }
        std::unique_ptr<Statement> stmt = nullptr;

        if (isIntegerType(currentToken().type))
        {
            stmt = parseLetStatementWithType(false);
            privateData.push_back(std::move(stmt));
        }

        switch (currentToken().type)
        {
        case TokenType::USE:
            if (nextToken().type == TokenType::BEHAVIOR)
            {
                stmt = parseUseStatement();
                usedBehaviorBlocks.push_back(std::move(stmt));
            }
            else if (nextToken().type == TokenType::DATA)
            {
                stmt = parseUseStatement();
                usedDataBlocks.push_back(std::move(stmt));
            }
            else
            {
                logError("Expected 'use data' or 'use behavior', got '" + nextToken().TokenLiteral + "'");
                advance(); // skip the unexpected token
            }
            break;
        case TokenType::STRING_KEYWORD:
        case TokenType::MUT:
        case TokenType::CONST:
        case TokenType::FLOAT_KEYWORD:
        case TokenType::BOOL_KEYWORD:
        case TokenType::CHAR_KEYWORD:
        case TokenType::CHAR16_KEYWORD:
        case TokenType::CHAR32_KEYWORD:
            stmt = parseLetStatementWithType(false);
            privateData.push_back(std::move(stmt));
            break;
        case TokenType::IDENTIFIER:
        case TokenType::SELF:
            stmt = parseAssignmentStatement(false);
            privateData.push_back(std::move(stmt));
            break;
        case TokenType::FUNCTION:
            stmt = parseFunctionStatement();
            privateMethods.push_back(std::move(stmt));
            break;
        case TokenType::INIT:
            if (initConstructor.has_value())
            {
                logError("Duplicate init constructor in component");
                advance();
                break;
            }
            stmt = parseInitConstructorStatement();
            initConstructor = std::move(stmt);
            break;
        case TokenType::SEMICOLON:
            advance();
            break;

        default:
            logError("Unknown statement inside component block: " + currentToken().TokenLiteral);
            advance();
            continue;
        }
    }
    if (currentToken().type != TokenType::RBRACE)
    {
        logError("Expected } to close component block but got '" + currentToken().TokenLiteral + "'");
    }
    advance(); // Consume the RBRACE

    return std::make_unique<ComponentStatement>(
        component_token,
        std::move(component_name),
        std::move(privateData),
        std::move(privateMethods),
        std::move(usedDataBlocks),
        std::move(usedBehaviorBlocks),
        std::move(initConstructor));
}

std::unique_ptr<Statement> Parser::parseInitConstructorStatement()
{
    Token init_token = currentToken();
    advance(); // Consume 'init'

    std::vector<std::unique_ptr<Statement>> args;

    // Expect LPAREN
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' after 'init', but got: " + currentToken().TokenLiteral);
        return nullptr;
    }
    advance(); // consume '('

    // Parse arguments until RPAREN
    while (currentToken().type != TokenType::RPAREN && currentToken().type != TokenType::END)
    {
        // Parse argument
        auto arg = parseLetStatementDecider();
        if (arg)
        {
            args.push_back(std::move(arg));
        }

        if (currentToken().type == TokenType::COMMA)
        {
            advance(); // consume comma and continue
        }
        else if (currentToken().type != TokenType::RPAREN)
        {
            logError("Expected ',' or ')' in init argument list, got: " + currentToken().TokenLiteral);
            return nullptr;
        }
    }

    // Expect closing RPAREN
    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' to close init argument list");
        return nullptr;
    }
    advance(); // consume ')'

    // Parse the block statement
    if (currentToken().type != TokenType::LBRACE)
    {
        logError("[ERROR] Expected '{' to start init block but got: " + currentToken().TokenLiteral);
        return nullptr;
    }

    auto block = parseBlockStatement();
    return std::make_unique<InitStatement>(init_token, std::move(args), std::move(block));
}

// Parsing function statement
std::unique_ptr<Statement> Parser::parseFunctionStatement()
{
    Token funcToken = currentToken();

    std::unique_ptr<Expression> funcExpr = parseFunctionExpression();
    if (!funcExpr)
    {
        return nullptr;
    }

    return std::make_unique<FunctionStatement>(funcToken, std::move(funcExpr));
}
// Parsing return statements
std::unique_ptr<Statement> Parser::parseReturnStatement()
{
    Token return_stmt = currentToken();
    advance();

    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance();
        return std::make_unique<ReturnStatement>(return_stmt, nullptr, nullptr);
    }
    else if (currentToken().type == TokenType::END)
    {
        logError("Unexpected end of input after return");
        return nullptr;
    }

    std::cout << "[DEBUG] Parsing return expression token: " << currentToken().TokenLiteral << "\n";
    auto return_value = parseExpression(Precedence::PREC_NONE);

    if (!return_value)
    {
        logError("Return Value is NULL");
        return nullptr;
    }

    std::unique_ptr<Statement> error_stmt = nullptr;
    if (currentToken().type == TokenType::COMMA)
    {
        advance();
        error_stmt = parseErrorStatement();
        if (!error_stmt)
        {
            return nullptr;
        }
    }

    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance();
    }

    return std::make_unique<ReturnStatement>(return_stmt, std::move(return_value), std::move(error_stmt));
}

// Parse for loops
std::unique_ptr<Statement> Parser::parseForStatement()
{
    Token forToken = currentToken();
    advance(); // consume 'for'

    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' after 'for'");
        return nullptr;
    }
    advance(); // consume '('

    // Parse initializer (e.g., mut int i = 0)
    auto initializer = parseLetStatementWithType(true);

    if (currentToken().type != TokenType::BITWISE_OR)
    {
        logError("Expected '|' after initializer in for loop");
        return nullptr;
    }
    advance(); // consume first '|'

    // Parse condition
    auto condition = parseExpression(Precedence::PREC_NONE);

    if (currentToken().type != TokenType::BITWISE_OR)
    {
        logError("Expected '|' after condition in for loop");
        return nullptr;
    }
    advance(); // consume second '|'

    // Parse step expression (must be prefix/postfix)
    auto step = parseExpression(Precedence::PREC_NONE);
    if (!(dynamic_cast<PrefixExpression *>(step.get()) ||
          dynamic_cast<PostfixExpression *>(step.get())))
    {
        logError("Step expression must be a prefix or postfix increment/decrement (e.g., ++i, i++)");
        return nullptr;
    }

    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' after step expression");
        return nullptr;
    }
    advance(); // consume ')'

    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected '{' to start for loop block");
        return nullptr;
    }

    auto block = parseBlockStatement();

    return std::make_unique<ForStatement>(
        forToken,
        std::move(initializer),
        std::move(condition),
        std::move(step),
        std::move(block));
}

std::unique_ptr<Statement> Parser::parseEachStatement()
{
    Token each_key = currentToken();
    advance(); // Consume the 'each' token
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' after 'each'");
        return nullptr;
    }
    advance(); // Skip '('

    // Parse the iterator variable (e.g., `auto x`)
    auto iterVar = parseLetStatementDecider(); // could be an identifier or declaration

    if (currentToken().type != TokenType::COLON)
    {
        logError("Expected ':' in each loop but got: " + currentToken().TokenLiteral);
        return nullptr;
    }
    advance(); // skip ':'

    // Parse the iterable (e.g., list)
    auto iterable = parseExpression(Precedence::PREC_NONE);

    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' after iterable expression");
        return nullptr;
    }
    advance(); // skip ')'

    if (currentToken().type != TokenType::LBRACE)
    {
        logError("Expected '{' after foreach loop");
        return nullptr;
    }

    auto body = parseBlockStatement();

    return std::make_unique<EachStatement>(
        each_key,
        std::move(iterVar),
        std::move(iterable),
        std::move(body));
}

// Parsing while statements
std::unique_ptr<Statement> Parser::parseWhileStatement()
{
    Token while_key = currentToken();
    advance();
    if (currentToken().type != TokenType::LPAREN)
    {
        logError(" Expected ')' after keyword while ");
        return nullptr;
    }
    advance();
    auto condition = parseExpression(Precedence::PREC_NONE);
    if (currentToken().type != TokenType::RPAREN)
    {
        logError(" Expected ')' after while condition");
        return nullptr;
    }
    advance();
    auto result = parseBlockStatement();
    return std::make_unique<WhileStatement>(while_key, move(condition), move(result));
}

// Parse break statement
std::unique_ptr<Statement> Parser::parseBreakStatement()
{
    Token break_tok = currentToken();
    advance();
    advance();
    return std::make_unique<BreakStatement>(break_tok);
}

// Parse continue statement
std::unique_ptr<Statement> Parser::parseContinueStatement()
{
    Token cont_tok = currentToken();
    advance();
    advance();
    return std::make_unique<ContinueStatement>(cont_tok);
}

// Parsing elif statements
std::unique_ptr<Statement> Parser::parseElifStatement()
{
    Token elif_stmt = currentToken();
    advance();

    if (currentToken().type != TokenType::LPAREN)
    {
        std::cout << "[DEBUG] Expected '(' after 'elif', got: " << currentToken().TokenLiteral << "\n";
        logError("Expected '(' after 'elseif'");
        return nullptr;
    }

    auto elif_condition = parseGroupedExpression();
    auto elif_result = parseBlockStatement();
    auto elifClause = std::make_unique<elifStatement>(
        elif_stmt,
        std::move(elif_condition),
        std::move(elif_result));

    return elifClause;
}

// Parsing if statements`
std::unique_ptr<Statement> Parser::parseIfStatement()
{
    Token if_stmt = currentToken();
    advance();

    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' after 'if' ");
        return nullptr;
    }
    advance();

    auto condition = parseExpression(Precedence::PREC_NONE);
    if (currentToken().type != TokenType::RPAREN)
    {
        std::cout << "[DEBUG] Expected ')' got: " << currentToken().TokenLiteral << "\n";
        logError("Expected ')' got: ");
        return nullptr;
    }
    advance();
    auto if_result = parseBlockStatement();

    std::vector<std::unique_ptr<Statement>> elifClauses;
    while (currentToken().type == TokenType::ELSE_IF)
    {
        auto elifClause = parseElifStatement();
        elifClauses.push_back(std::move(elifClause));
    }

    std::optional<Token> else_stmt;
    std::optional<std::unique_ptr<Statement>> else_result;

    if (currentToken().type == TokenType::ELSE)
    {
        else_stmt = currentToken();
        advance();

        else_result = parseBlockStatement();
    }

    return std::make_unique<ifStatement>(
        if_stmt,
        std::move(condition),
        std::move(if_result),
        std::move(elifClauses),
        std::move(else_stmt),
        std::move(else_result));
}

// Parsing identifiers
std::unique_ptr<Expression> Parser::parseIdentifier()
{
    auto ident = std::make_unique<Identifier>(currentToken());
    if (!ident)
    {
        logError("Failed to parse identifier '" + currentToken().TokenLiteral + "'");
    }
    advance();
    return ident;
}

// Parsing address expression
std::unique_ptr<Expression> Parser::parseAddressExpression()
{
    Token addr_token = currentToken();
    advance(); // Consume & token

    auto ident = parseIdentifier();

    return std::make_unique<AddressExpression>(addr_token, std::move(ident));
}

// Parsing dereference expression
std::unique_ptr<Expression> Parser::parseDereferenceExpression()
{
    Token deref_token = currentToken();
    advance(); // Consume deref token

    auto ident = parseIdentifier();

    return std::make_unique<DereferenceExpression>(deref_token, std::move(ident));
}

std::unique_ptr<Expression> Parser::parseIdentifierOrArraySubscript()
{
    if (nextToken().type == TokenType::LBRACKET)
    {
        return parseArraySubscript();
    }
    return parseIdentifier();
}

std::unique_ptr<Expression> Parser::parseSelfExpression()
{
    Token self_token = currentToken();
    advance();

    if (currentToken().type != TokenType::FULLSTOP)
    {
        logError("Exprected a . after self but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    advance();

    if (currentToken().type != TokenType::IDENTIFIER)
    {
        logError("Expected an identifer after . but got" + currentToken().TokenLiteral);
        return nullptr;
    }

    // Call the identifier parser here
    auto fieldExpr = parseIdentifier();

    return std::make_unique<SelfExpression>(
        self_token, std::move(fieldExpr));
}

//-----------PARSING EXPRESSIONS----------
// Expression parsing section
// Main Expression parsing function
std::unique_ptr<Expression> Parser::parseExpression(Precedence precedence)
{
    auto PrefixParseFnIt = PrefixParseFunctionsMap.find(currentToken().type); // Creating an iterator pointer to loop through the map

    if (PrefixParseFnIt == PrefixParseFunctionsMap.end()) // Checking if the iterator has reached the end of the map
    {
        // std::cerr << "[ERROR] No prefix parse function for token: " << currentToken().TokenLiteral << "\n";
        logError("No prefix parse function for token '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    auto left_expression = (this->*PrefixParseFnIt->second)(); // Calling the neccesary prefix function after encountering that particular token
    std::cout << "[DEBUG] Initial left expression: " << left_expression->toString() << "\n";

    while (true)
    {
        if (currentToken().type == TokenType::SEMICOLON ||
            currentToken().type == TokenType::RBRACE ||
            currentToken().type == TokenType::RPAREN ||
            currentToken().type == TokenType::COMMA)
        {
            break;
        }
        Precedence currentPrecedence = get_precedence(currentToken().type);

        // If current token is a postfix operator (e.g., ++, --)
        auto PostfixParseFnIt = PostfixParseFunctionsMap.find(currentToken().type);
        if (PostfixParseFnIt != PostfixParseFunctionsMap.end())
        {
            // Only parse if postfix precedence is >= incoming precedence
            if (precedence >= currentPrecedence)
                break;

            std::cout << "[DEBUG] Parsing postfix token: " << currentToken().TokenLiteral << "\n";
            left_expression = (this->*PostfixParseFnIt->second)(std::move(left_expression));
            std::cout << "[DEBUG] Updated left expression (postfix): " << left_expression->toString() << "\n";
            continue;
        }

        // Handle infix operators
        auto InfixParseFnIt = InfixParseFunctionsMap.find(currentToken().type);
        if (InfixParseFnIt == InfixParseFunctionsMap.end())
        {
            std::cout << "[DEBUG] No infix or postfix parser found for: " << currentToken().TokenLiteral << "\n";
            break;
        }

        if (precedence >= currentPrecedence)
            break;

        std::cout << "[DEBUG] Parsing infix token: " << currentToken().TokenLiteral << "\n";
        left_expression = (this->*InfixParseFnIt->second)(std::move(left_expression));
        std::cout << "[DEBUG] Updated left expression (infix): " << left_expression->toString() << "\n";
    }

    return left_expression; // Returning the expression that was parsed it can be either prefix or infix
}

// Inifix parse function definition
std::unique_ptr<Expression> Parser::parseInfixExpression(std::unique_ptr<Expression> left)
{
    Token operat = currentToken();
    std::cout << "[DEBUG] parsing infix with operator: " << operat.TokenLiteral << "\n";
    Precedence prec = get_precedence(operat.type);
    advance();
    auto right = parseExpression(prec);
    return std::make_unique<InfixExpression>(std::move(left), operat, std::move(right));
}

// Prefix parse function definition
std::unique_ptr<Expression> Parser::parsePrefixExpression()
{
    Token operat = currentToken();
    Precedence operatorPrecedence = get_precedence(operat.type);
    advance();
    auto operand = parseExpression(operatorPrecedence);
    return std::make_unique<PrefixExpression>(operat, std::move(operand));
}

// Postfix expression parser function definition
std::unique_ptr<Expression> Parser::parsePostfixExpression(std::unique_ptr<Expression> left)
{
    Token op = currentToken();
    advance(); // Consume ++ or --
    return std::make_unique<PostfixExpression>(std::move(left), op);
}

std::unique_ptr<Expression> Parser::parseNewComponentExpression()
{
    Token new_token = currentToken();
    advance(); // Consuming the new keyword open
    Token component_name = currentToken();
    if (component_name.type != TokenType::IDENTIFIER)
    {
        logError("Expected identifier for component name after new but got: " + component_name.TokenLiteral);
        return nullptr;
    }
    advance(); // Consuming the component name
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected '(' after component name in 'new' expression");
        return nullptr;
    }
    advance(); // Consume the lparen
    auto args = parseCallArguments();

    return std::make_unique<NewComponentExpression>(new_token, component_name, std::move(args));
}

// Null literal parse function
std::unique_ptr<Expression> Parser::parseNullLiteral()
{
    auto ident = std::make_unique<NullLiteral>(currentToken());
    advance();
    return ident;
}

// 16 bit signed integer literal parse function
std::unique_ptr<Expression> Parser::parseShortLiteral()
{
    auto ident = std::make_unique<ShortLiteral>(currentToken());
    advance();
    return ident;
}

// 16 bit unsigned integer literal parse function
std::unique_ptr<Expression> Parser::parseUnsignedShortLiteral()
{
    auto ident = std::make_unique<UnsignedShortLiteral>(currentToken());
    advance();
    return ident;
}

// 32 bit signed Integer literal parse function
std::unique_ptr<Expression> Parser::parseIntegerLiteral()
{
    auto ident = std::make_unique<IntegerLiteral>(currentToken());
    advance();
    return ident;
}

// 32 bit unsigned Integer literal parse function
std::unique_ptr<Expression> Parser::parseUnsignedIntegerLiteral()
{
    auto ident = std::make_unique<UnsignedIntegerLiteral>(currentToken());
    advance();
    return ident;
}

// Signed 64 bit integer parse function
std::unique_ptr<Expression> Parser::parseLongLiteral()
{
    auto ident = std::make_unique<LongLiteral>(currentToken());
    advance();
    return ident;
}

// UnSigned 64 bit integer parse function
std::unique_ptr<Expression> Parser::parseUnsignedLongLiteral()
{
    auto ident = std::make_unique<UnsignedLongLiteral>(currentToken());
    advance();
    return ident;
}

// Signed 128 bit integer parse function
std::unique_ptr<Expression> Parser::parseExtraLiteral()
{
    auto ident = std::make_unique<ExtraLiteral>(currentToken());
    advance();
    return ident;
}

// UnSigned 128 bit integer parse function
std::unique_ptr<Expression> Parser::parseUnsignedExtraLiteral()
{
    auto ident = std::make_unique<UnsignedExtraLiteral>(currentToken());
    advance();
    return ident;
}

// Boolean literal parse function
std::unique_ptr<Expression> Parser::parseBooleanLiteral()
{
    Token bool_tok = currentToken();
    advance();
    return std::make_unique<BooleanLiteral>(bool_tok);
}

// Float literal parse function
std::unique_ptr<Expression> Parser::parseFloatLiteral()
{
    Token float_tok = currentToken();
    advance();
    return std::make_unique<FloatLiteral>(float_tok);
}

// Double literal parse function
std::unique_ptr<Expression> Parser::parseDoubleLiteral()
{
    Token double_tok = currentToken();
    advance();
    return std::make_unique<DoubleLiteral>(double_tok);
}

// 8 bit Char literal parse function
std::unique_ptr<Expression> Parser::parseCharLiteral()
{
    Token char_tok = currentToken();
    advance();
    return std::make_unique<CharLiteral>(char_tok);
}

// 16 bit Char literal parser function
std::unique_ptr<Expression> Parser::parseChar16Literal()
{
    Token char16_token = currentToken();
    advance();
    return std::make_unique<Char16Literal>(char16_token);
}

// 32 bit Char literal parser function
std::unique_ptr<Expression> Parser::parseChar32Literal()
{
    Token char16_token = currentToken();
    advance();
    return std::make_unique<Char32Literal>(char16_token);
}
// String literal parse function
std::unique_ptr<Expression> Parser::parseStringLiteral()
{
    Token string_tok = currentToken();
    advance();
    return std::make_unique<StringLiteral>(string_tok);
}

// Grouped expression parse function
std::unique_ptr<Expression> Parser::parseGroupedExpression()
{
    advance(); // Consume the ( token
    auto expr = parseExpression(Precedence::PREC_NONE);
    if (!expr)
    {
        logError("Empty grouped expression after '('");
        return nullptr;
    }

    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' to close grouped expression but got " + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    advance(); // consume ')'
    return expr;
}

std::unique_ptr<Expression> Parser::parseCallExpression(std::unique_ptr<Expression> left)
{
    std::cout << "[DEBUG] Entered parseCallExpression for: " << left->toString() << "\n";
    Token call_token = currentToken(); // We expect a left parenthesis here

    if (call_token.type != TokenType::LPAREN)
    { // Checking if we encounter the left parenthesis after the function name has been declared
        logError("Expected ( after function name");
        return nullptr;
    }

    advance(); // Advancing the pointer to look at what is inside the brackets

    auto args = parseCallArguments(); // Calling the parse call arguments inorder to parse the arguments

    return std::make_unique<CallExpression>(call_token, std::move(left), std::move(args));
}

// Parsing function call arguments
std::vector<std::unique_ptr<Expression>> Parser::parseCallArguments()
{
    std::vector<std::unique_ptr<Expression>> args;
    if (currentToken().type == TokenType::RPAREN)
    {
        advance(); // Consume the ) token
        return args;
    }

    auto firstArg = parseExpression(Precedence::PREC_NONE);
    if (!firstArg)
    {
        std::cerr << "Failed to parse first function argument.\n";
        return args;
    }
    args.push_back(std::move(firstArg));

    while (currentToken().type == TokenType::COMMA)
    {
        advance();
        auto arg = parseExpression(Precedence::PREC_NONE);
        if (!arg)
        {
            std::cerr << "Failed to parse function argument after comma.\n";
            return args;
        }
        args.push_back(std::move(arg));
    }

    if (currentToken().type == TokenType::RPAREN)
    {
        advance(); // consume ')'
    }
    else
    {
        logError("Expected ')' after function arguments");
    }

    return args;
}

// Parsing basic return type
std::unique_ptr<Expression> Parser::parseBasicType()
{
    Token data_token;
    bool isNullable = false;

    data_token = currentToken();
    advance();
    if (currentToken().type == TokenType::QUESTION_MARK)
    {
        isNullable = true;
        advance(); // Consume the ? if it exists
    }
    return std::make_unique<BasicType>(data_token, isNullable);
}

// Parsing pointer return type
std::unique_ptr<Expression> Parser::parsePointerType()
{
    Token ptr_token = currentToken();
    advance(); // Consume the ptr token
    std::unique_ptr<Expression> type;
    if (isBasicType(currentToken().type) || currentToken().type == TokenType::IDENTIFIER || currentToken().type == TokenType::VOID)
    {
        type = parseBasicType();
    }
    else if (currentToken().type == TokenType::ARRAY)
    {

        type = parseArrayType();
    }
    else
    {
        logError("Expected basic or array return type ");
    }

    return std::make_unique<PointerType>(ptr_token, std::move(type));
}

// Parsing the return type expression
std::unique_ptr<Expression> Parser::parseReturnType()
{
    std::unique_ptr<Expression> expr = nullptr;
    if (isBasicType(currentToken().type) || currentToken().type == TokenType::IDENTIFIER || currentToken().type == TokenType::VOID)
    {
        expr = parseBasicType();
    }
    else if (currentToken().type == TokenType::ARRAY)
    {

        expr = parseArrayType();
    }
    else if (currentToken().type == TokenType::PTR)
    {
        expr = parsePointerType();
    }
    else
    {
        logError("Expected basic or array return type ");
    }

    return std::make_unique<ReturnType>(std::move(expr));
}

// Parsing function expression
std::unique_ptr<Expression> Parser::parseFunctionExpression()
{
    std::cout << "[TEST]Function parser is working\n";
    //--------Dealing with func keyword---------------
    Token func_tok = currentToken(); // The token representing the keyword for functions (func)
    advance();

    //----------Dealing with function name------------
    auto identExpr = parseIdentifier();
    auto identNode = dynamic_cast<Identifier *>(identExpr.get());

    if (!identNode)
    {
        logError("Expected identifier for function name after 'func' but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    Token identToken = identNode->identifier;

    //---Dealing with the call itself
    auto call = parseFunctionParameters(); // We might get some arguments or not so we call the parse call expression

    std::unique_ptr<Expression> return_type = nullptr;
    //--Checking for colons
    if (currentToken().type == TokenType::COLON)
    {
        advance(); // Move past the colon signs
        if (isBasicType(currentToken().type) || currentToken().type == TokenType::IDENTIFIER ||
            currentToken().type == TokenType::VOID || currentToken().type == TokenType::ARRAY || currentToken().type == TokenType::PTR)
        {
            return_type = parseReturnType();
        }

        else
        {
            logError("Unexpected return type '" + currentToken().TokenLiteral + "'");
            return nullptr;
        }
    }

    std::cout << "[DEBUG]: Encountered the " << currentToken().TokenLiteral << "\n";
    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance(); // consume the semicolon
        auto decl = std::make_unique<FunctionDeclaration>(
            func_tok,
            std::move(identExpr),
            std::move(call),
            std::move(return_type));
        return std::make_unique<FunctionDeclarationExpression>(func_tok, std::move(decl));
    }

    auto block = parseBlockExpression(); // Parsing the blocks
    if (!block)
    {
        std::cerr << "[ERROR] Failed to parse function body.\n";
        return nullptr;
    }

    return std::make_unique<FunctionExpression>(identToken, std::move(call), std::move(return_type), std::move(block));
}

// Parsing function paramemters
std::vector<std::unique_ptr<Statement>> Parser::parseFunctionParameters()
{
    std::vector<std::unique_ptr<Statement>> args; // Declaring the empty vector

    // Checking if the current token is the lparen
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected a ( to start a function parameter ");
        return args;
    }

    // Advancing from the lparen
    advance();

    if (currentToken().type == TokenType::RPAREN)
    {
        advance();
        if (currentToken().type != TokenType::COLON)
        {
            logError("Expected ':' after empty parameter list ");
        }

        return args; // Return an empty vector of arguments
    }

    auto firstParam = parseLetStatementDecider(); // Parsing the fisrt parameter i.e 1

    if (!firstParam)
    { // Checking if it failed to parse the 1st parameter
        std::cerr << "Failed to parse first parameter.\n";
        return args;
    }
    args.push_back(std::move(firstParam)); // If its parsed we add it to the vector

    while (currentToken().type == TokenType::COMMA)
    {                                          // If we still have commas
        advance();                             // Advance from the comma to the second parameter
        auto arg = parseLetStatementDecider(); // Parse the second parameter
        if (!arg)
        { // It it fails to parse the second parameter
            std::cerr << "Failed to parse parameter after comma\n";
            return args;
        }
        args.push_back(std::move(arg));
    }

    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected ')' after function parameters ");
        return args;
    }
    advance();

    //--THIS IS WHERE THE FUNCTION HAS TO DEFER FROM PARSE CALL EXPRESSION
    if (currentToken().type != TokenType::COLON)
    {
        logError("Expected : after function closing ");
    }

    return args;
}

// Parsing error expressions
std::unique_ptr<Expression> Parser::parseErrorExpression()
{
    Token err_tok = currentToken();
    advance();
    if (currentToken().type != TokenType::LPAREN)
    {
        logError("Expected ( ");
    }
    advance();
    auto err_message = parseExpression(Precedence::PREC_NONE);
    if (currentToken().type != TokenType::RPAREN)
    {
        logError("Expected )");
    }
    return std::make_unique<ErrorExpression>(err_tok, std::move(err_message));
}

// Parsing block expressions
std::unique_ptr<Expression> Parser::parseBlockExpression()
{
    Token lbrace = currentToken();
    if (lbrace.type != TokenType::LBRACE)
    {
        logError("Expected { after data type but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }
    advance();
    auto block = std::make_unique<BlockExpression>(lbrace);
    while (currentToken().type != TokenType::RBRACE)
    {
        if (currentToken().type == TokenType::END)
        {
            logError("Unterminated block experession");
            return nullptr;
        }

        auto stmt = parseStatement();
        if (stmt)
        {
            block->statements.push_back(std::move(stmt));
        }
    }

    std::cout << "[DEBUG] Block contains " << block->statements.size()
              << " statement(s). Final expression present: " << (block->finalexpr != nullptr) << "\n";

    if (currentToken().type != TokenType::RBRACE)
    {
        logError("Expected } but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    advance();
    return block;
}

// Parsing block statements
std::unique_ptr<Statement> Parser::parseBlockStatement()
{
    Token lbrace = currentToken();
    if (lbrace.type != TokenType::LBRACE)
    {
        logError("[ERROR] Expected '{' to start block but got '" + lbrace.TokenLiteral + "'");
        return nullptr;
    }
    advance();
    std::vector<std::unique_ptr<Statement>> statements;

    while (currentToken().type != TokenType::RBRACE && currentToken().type != TokenType::END)
    {
        if (currentToken().type == TokenType::SEMICOLON)
        {
            advance(); // skip empty statement
            continue;
        }
        auto stmt = parseStatement();
        if (stmt != nullptr)
        {
            statements.push_back(std::move(stmt));
        }
        else
        {
            std::cerr << "[ERROR] Failed to parse statement within block. Skipping token: " << currentToken().TokenLiteral << "\n";
            advance();
        }
    }

    if (currentToken().type != TokenType::RBRACE)
    {
        logError("Expected } to close block but got '" + currentToken().TokenLiteral + "'");
        return nullptr;
    }

    advance();

    if (currentToken().type == TokenType::SEMICOLON)
    {
        advance();
    }

    return std::make_unique<BlockStatement>(lbrace, std::move(statements));
}

//----------HELPER FUNCTIONS---------------
// Slider function
void Parser::advance()
{
    if (nextPos < tokenInput.size())
    {
        lastToken = currentToken();
        currentPos = nextPos;
        std::cout << "Current token :" << currentToken().TokenLiteral << "\n";
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
    InfixParseFunctionsMap[TokenType::GREATER_THAN] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::LESS_THAN] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::GT_OR_EQ] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::LT_OR_EQ] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::FULLSTOP] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::SCOPE_OPERATOR] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::AND] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::OR] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::NOT_EQUALS] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::EQUALS] = &Parser::parseInfixExpression;
    InfixParseFunctionsMap[TokenType::LPAREN] = &Parser::parseCallExpression;
    InfixParseFunctionsMap[TokenType::LBRACE] = &Parser::parseInstanceExpression;
    InfixParseFunctionsMap[TokenType::COALESCE] = &Parser::parseInfixExpression;
}

// Registering prefix functions for a particular token type
void Parser::registerPrefixFns()
{
    PrefixParseFunctionsMap[TokenType::SHORT] = &Parser::parseShortLiteral;
    PrefixParseFunctionsMap[TokenType::USHORT] = &Parser::parseUnsignedShortLiteral;
    PrefixParseFunctionsMap[TokenType::INT] = &Parser::parseIntegerLiteral;
    PrefixParseFunctionsMap[TokenType::UINT] = &Parser::parseUnsignedIntegerLiteral;
    PrefixParseFunctionsMap[TokenType::LONG] = &Parser::parseLongLiteral;
    PrefixParseFunctionsMap[TokenType::ULONG] = &Parser::parseUnsignedLongLiteral;
    PrefixParseFunctionsMap[TokenType::EXTRA] = &Parser::parseExtraLiteral;
    PrefixParseFunctionsMap[TokenType::UEXTRA] = &Parser::parseUnsignedExtraLiteral;

    PrefixParseFunctionsMap[TokenType::CHAR] = &Parser::parseCharLiteral;
    PrefixParseFunctionsMap[TokenType::CHAR16] = &Parser::parseChar16Literal;
    PrefixParseFunctionsMap[TokenType::CHAR32] = &Parser::parseChar32Literal;

    PrefixParseFunctionsMap[TokenType::NULLABLE] = &Parser::parseNullLiteral;
    PrefixParseFunctionsMap[TokenType::TRUE] = &Parser::parseBooleanLiteral;
    PrefixParseFunctionsMap[TokenType::FALSE] = &Parser::parseBooleanLiteral;
    PrefixParseFunctionsMap[TokenType::FLOAT] = &Parser::parseFloatLiteral;
    PrefixParseFunctionsMap[TokenType::DOUBLE] = &Parser::parseDoubleLiteral;
    PrefixParseFunctionsMap[TokenType::STRING] = &Parser::parseStringLiteral;

    PrefixParseFunctionsMap[TokenType::IDENTIFIER] = &Parser::parseIdentifierOrArraySubscript;
    PrefixParseFunctionsMap[TokenType::BITWISE_AND] = &Parser::parseAddressExpression;
    PrefixParseFunctionsMap[TokenType::DEREF] = &Parser::parseDereferenceExpression;
    PrefixParseFunctionsMap[TokenType::NEW] = &Parser::parseNewComponentExpression;
    PrefixParseFunctionsMap[TokenType::SELF] = &Parser::parseSelfExpression;
    PrefixParseFunctionsMap[TokenType::BANG] = &Parser::parsePrefixExpression;
    PrefixParseFunctionsMap[TokenType::MINUS] = &Parser::parsePrefixExpression;
    PrefixParseFunctionsMap[TokenType::LPAREN] = &Parser::parseGroupedExpression;
    PrefixParseFunctionsMap[TokenType::LBRACKET] = &Parser::parseArrayLiteral;
    PrefixParseFunctionsMap[TokenType::LBRACE] = &Parser::parseBlockExpression;
    PrefixParseFunctionsMap[TokenType::PLUS_PLUS] = &Parser::parsePrefixExpression;
    PrefixParseFunctionsMap[TokenType::MINUS_MINUS] = &Parser::parsePrefixExpression;
    PrefixParseFunctionsMap[TokenType::FUNCTION] = &Parser::parseFunctionExpression;
}

// Registering the postfix parse functions
void Parser::registerPostfixFns()
{
    PostfixParseFunctionsMap[TokenType::PLUS_PLUS] = &Parser::parsePostfixExpression;
    PostfixParseFunctionsMap[TokenType::MINUS_MINUS] = &Parser::parsePostfixExpression;
}

// Decider for custom or basic let statements
std::unique_ptr<Statement> Parser::parseLetStatementCustomOrBasic()
{
    if (currentToken().type == TokenType::IDENTIFIER)
    {
        return parseLetStatementWithCustomType();
    }
    return parseLetStatementWithType();
}

// Wrapper function for let statement with basic or custom type
std::unique_ptr<Statement> Parser::parseLetStatementWithTypeWrapper()
{
    if (nextToken().type == TokenType::DATA)
    {
        return parseDataStatement();
    }
    else if (nextToken().type == TokenType::ARRAY)
    {
        return parseArrayStatementWrapper();
    }
    return parseLetStatementCustomOrBasic();
}

// Identifer overloader parser
std::unique_ptr<Statement> Parser::parseIdentifierStatement()
{
    Token current = currentToken();
    std::cout << "IDENTIFIER TOKEN INSIDE IDENTIFIER STATEMENT PARSER: " << current.TokenLiteral << "\n";
    // Helper to peek after full subscript (x[0], x[y[1]], etc.)
    auto peekAfterSubscript = [this](int startOffset = 1) -> Token
    {
        int depth = 0;
        int offset = startOffset;
        while (true)
        {
            Token t = peekToken(offset);
            if (t.type == TokenType::LBRACKET)
                depth++;
            else if (t.type == TokenType::RBRACKET)
            {
                depth--;
                if (depth == 0)
                    return peekToken(offset + 1); // token after closing bracket
            }
            else if (t.type == TokenType::END)
                break;
            offset++;
        }
        return Token{"", TokenType::END};
    };

    Token peek1 = peekToken(1);

    // Array subscript case
    if (peek1.type == TokenType::LBRACKET)
    {
        if (peekAfterSubscript().type == TokenType::ASSIGN)
        {
            std::cout << "SUBSCRIPT ASSIGNMENT DETECTED\n";
            return parseAssignmentStatement();
        }
    }

    // Simple assignment (x = ...)
    if (peek1.type == TokenType::ASSIGN)
    {
        return parseAssignmentStatement();
    }

    // Qualified assignment (x.y = ..., test::field = ...)
    if (peek1.type == TokenType::FULLSTOP || peek1.type == TokenType::SCOPE_OPERATOR)
    {
        Token peek3 = peekToken(3);
        if (peek3.type == TokenType::ASSIGN)
            return parseFieldAssignment();
    }

    // Type declarations (x y)
    if (peek1.type == TokenType::IDENTIFIER)
    {
        return parseLetStatementWithTypeWrapper();
    }

    // Fall back to generic expression statement
    auto expr = parseExpression(Precedence::PREC_NONE);
    if (expr)
    {
        if (currentToken().type == TokenType::SEMICOLON)
            advance();
        else
            logError("Expected ';' after expression statement but got '" + currentToken().TokenLiteral + "'");

        return std::make_unique<ExpressionStatement>(current, std::move(expr));
    }

    logError("Invalid identifier statement");
    return nullptr;
}

std::unique_ptr<Statement> Parser::parseErrorStatement()
{
    Token err_token = currentToken();
    auto errExpr = parseErrorExpression();
    return std::make_unique<ErrorStatement>(err_token, std::move(errExpr));
}

// Registering the statement parsing functions
void Parser::registerStatementParseFns()
{
    StatementParseFunctionsMap[TokenType::RETURN] = &Parser::parseReturnStatement;
    StatementParseFunctionsMap[TokenType::IF] = &Parser::parseIfStatement;
    StatementParseFunctionsMap[TokenType::WHILE] = &Parser::parseWhileStatement;
    StatementParseFunctionsMap[TokenType::FOR] = &Parser::parseForStatement;
    StatementParseFunctionsMap[TokenType::EACH] = &Parser::parseEachStatement;
    StatementParseFunctionsMap[TokenType::BREAK] = &Parser::parseBreakStatement;
    StatementParseFunctionsMap[TokenType::CONTINUE] = &Parser::parseContinueStatement;
    StatementParseFunctionsMap[TokenType::SIGNAL] = &Parser::parseSignalStatement;
    StatementParseFunctionsMap[TokenType::START] = &Parser::parseStartStatement;
    StatementParseFunctionsMap[TokenType::WAIT] = &Parser::parseWaitStatement;
    StatementParseFunctionsMap[TokenType::ALIAS] = &Parser::parseAliasStatement;
    StatementParseFunctionsMap[TokenType::QUALIFY] = &Parser::parseQualifyStatement;
    StatementParseFunctionsMap[TokenType::MERGE] = &Parser::parseMergeStatement;
    StatementParseFunctionsMap[TokenType::SHOUT] = &Parser::parseShoutStatement;

    // For basic types
    StatementParseFunctionsMap[TokenType::SHORT_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::USHORT_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::INTEGER_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::UINT_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::LONG_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::ULONG_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::EXTRA_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::UEXTRA_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;

    StatementParseFunctionsMap[TokenType::CHAR_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::CHAR16_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::CHAR32_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    // For custom types
    StatementParseFunctionsMap[TokenType::IDENTIFIER] = &Parser::parseIdentifierStatement;

    StatementParseFunctionsMap[TokenType::FLOAT_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::DOUBLE_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::STRING_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::BOOL_KEYWORD] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::CONST] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::MUT] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::FUNCTION] = &Parser::parseFunctionStatement;
    StatementParseFunctionsMap[TokenType::AUTO] = &Parser::parseLetStatementWithTypeWrapper;
    StatementParseFunctionsMap[TokenType::ERROR] = &Parser::parseErrorStatement;
    StatementParseFunctionsMap[TokenType::COMPONENT] = &Parser::parseComponentStatement;
    StatementParseFunctionsMap[TokenType::BEHAVIOR] = &Parser::parseBehaviorStatement;
    StatementParseFunctionsMap[TokenType::DATA] = &Parser::parseDataStatement;
    StatementParseFunctionsMap[TokenType::USE] = &Parser::parseUseStatement;
    StatementParseFunctionsMap[TokenType::INIT] = &Parser::parseInitConstructorStatement;
    StatementParseFunctionsMap[TokenType::SWITCH] = &Parser::parseSwitchStatement;
    StatementParseFunctionsMap[TokenType::ENUM] = &Parser::parseEnumClassStatement;

    StatementParseFunctionsMap[TokenType::GENERIC] = &Parser::parseGenericStatement;
    StatementParseFunctionsMap[TokenType::INSTANTIATE] = &Parser::parseInstantiateStatement;

    StatementParseFunctionsMap[TokenType::ARRAY] = &Parser::parseArrayStatementWrapper;
    StatementParseFunctionsMap[TokenType::HEAP] = &Parser::parseHeapStatement;
    StatementParseFunctionsMap[TokenType::REF] = &Parser::parseReferenceStatementWrapper;
    StatementParseFunctionsMap[TokenType::PTR] = &Parser::parsePointerStatementWrapper;
    StatementParseFunctionsMap[TokenType::DEREF] = &Parser::parseDereferenceAssignment;
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

Token Parser::peekToken(int peek)
{
    int steps = currentPos + peek;
    if (steps >= tokenInput.size())
    {
        return Token{"", TokenType::END, 0, 0}; // EOF token
    }
    return tokenInput[steps];
}

// Error logging
void Parser::logError(const std::string &message)
{
    Token token = getErrorToken();
    if (token.line == 0 && token.column == 0)
    {
        std::cerr << "[PANIC]: Logging an uninitialized token! Investigate token flow.\n";
    }
    std::cerr << "[PARSER ERROR]: " << message << " At line: " << token.line << " column: " << token.column << "\n";
    errors.push_back(
        ParseError{
            message,
            token.line,
            token.column});
}

// Generic checker
bool Parser::isGeneric(const std::string &typeName, const std::vector<Token> &genericParams)
{
    for (const auto &param : genericParams)
    {
        if (param.TokenLiteral == typeName && param.type == TokenType::IDENTIFIER)
        {
            return true;
        }
    }
    return false;
}

// Getting the error token
Token Parser::getErrorToken()
{
    if (currentPos >= tokenInput.size())
    {
        if (lastToken.line == 0)
        {
            return Token{"", TokenType::ILLEGAL, 999, 999};
        }
        return lastToken;
    }
    return tokenInput[currentPos - 1];
}

std::shared_ptr<FileUnit> Parser::generateFileUnit()
{
    // Populate the fields final field
    std::vector<std::unique_ptr<Node>> nodes = parseProgram();
    auto fileUnit = std::make_shared<FileUnit>();
    fileUnit->nodes = std::move(nodes);

    // Scan AST for imports and entry qualifier
    for (auto &node : fileUnit->nodes)
    {
        if (auto *mergeStmt = dynamic_cast<MergeStatement *>(node.get()))
        {
            fileUnit->mergers.push_back(mergeStmt->stringExpr->expression.TokenLiteral);
        }
        else if (auto *qualifyStmt = dynamic_cast<QualifyStatement *>(node.get()))
        {
            fileUnit->isEntryFile = true;
        }
    }
    return fileUnit;
}