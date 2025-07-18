#include "token/token.hpp"
#include "ast.hpp"
#include <string>
#include <vector>
#include <map>

#define CPPREST_FORCE_REBUILD

struct ParseError{
    std::string message;
    int line;
    int column;
};

class Parser
{
    std::vector<Token> tokenInput;
    int currentPos;
    int nextPos;

    // Precedence and token type map
    std::map<TokenType, Precedence> precedence{
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
        {TokenType::MINUS_MINUS,Precedence::PREC_UNARY},
        {TokenType::PLUS_PLUS,Precedence::PREC_UNARY},
        {TokenType::FULLSTOP, Precedence::PREC_CALL},
        {TokenType::LPAREN,Precedence::PREC_CALL},
        {TokenType::SCOPE_OPERATOR,Precedence::PREC_CALL},
        {TokenType::IDENTIFIER, Precedence::PREC_PRIMARY},
    };

    Token lastToken;

public:
    // Parser class declaration
    Parser(std::vector<Token> &tokenInput);
    // Main parser program
    std::vector<std::unique_ptr<Node>> parseProgram();

    // Sliding across the token input from the lexer;
    void advance();

    // Functions for registering the functions neccesary for parsing the different tokens
    void registerPrefixFns();
    void registerInfixFns();

    // Function to register statement parsing functions
    void registerStatementParseFns();

    // Function to get the precedence depending on the token type
    Precedence get_precedence(TokenType type);

    using prefixParseFns = std::unique_ptr<Expression> (Parser::*)();
    using infixParseFns = std::unique_ptr<Expression> (Parser::*)(std::unique_ptr<Expression>);

    using stmtParseFns = std::unique_ptr<Statement> (Parser::*)();

    std::map<TokenType, prefixParseFns> PrefixParseFunctionsMap;
    std::map<TokenType, infixParseFns> InfixParseFunctionsMap;
    
    std::map<TokenType, stmtParseFns> StatementParseFunctionsMap;
    std::vector<ParseError> errors;

private:
    //---------------PARSING STATEMENTS--------------------
    // General statement parsing function
    std::unique_ptr<Statement> parseStatement();

    // Parsing let statements with type
    std::unique_ptr<Statement> parseLetStatementWithType(bool isParam=false);

    // Parsing let statements without type
    std::unique_ptr<Statement> parseAssignmentStatement(bool isParam=false);

    //A function to determine whether to parse Let with type or no type
    std::unique_ptr<Statement> parseLetStatementDecider();

    // Parsing if statement
    std::unique_ptr<Statement> parseIfStatement();

    //Parsing switch statement
    std::unique_ptr<Statement> parseSwitchStatement();

    //Parsing enum class statement
    std::unique_ptr<Statement> parseEnumClassStatement();

    //Parsing case clause
    std::unique_ptr<Statement> parseCaseClause();

    //Parsing signal statement
    std::unique_ptr<Statement> parseSignalStatement();

    //Parsing start statement
    std::unique_ptr<Statement> parseStartStatement();

    //Parsing wait statement
    std::unique_ptr<Statement> parseWaitStatement();

    //Parsing the function statement
    std::unique_ptr<Statement> parseFunctionStatement();

    // Parsing return statements
    std::unique_ptr<Statement> parseReturnStatement();

    //Parsing for statement
    std::unique_ptr<Statement> parseForStatement();

    //Parsing while loops
    std::unique_ptr<Statement>parseWhileStatement();

    //Parsing break statement
    std::unique_ptr<Statement> parseBreakStatement();

    //Parsing continue statement
    std::unique_ptr<Statement> parseContinueStatement();

    // Parsing block statements
    std::unique_ptr<Statement> parseBlockStatement();

    //Parsing component statements
    std::unique_ptr<Statement> parseComponentStatement();

    //Parsing constructor statement
    std::unique_ptr<Statement> parseInitConstructorStatement();

    //Parsing use statements
    std::unique_ptr<Statement> parseUseStatement();

    //Parsing behavior statements
    std::unique_ptr<Statement> parseBehaviorStatement();

    //Parsing data statements
    std::unique_ptr<Statement> parseDataStatement();
    
    //--------------PARSING EXPRESSIONS--------------------
    // Main expression parsing function
    std::unique_ptr<Expression> parseExpression(Precedence precedence);

    // Infix expression parsing function
    std::unique_ptr<Expression> parseInfixExpression(std::unique_ptr<Expression> left);

    // Prefix expression parsing function
    std::unique_ptr<Expression> parsePrefixExpression();

    //Error expression
    std::unique_ptr<Expression> parseErrorExpression();

    //Error statement
    std::unique_ptr<Statement> parseErrorStatement();

    //New component expression parse function declaration
    std::unique_ptr<Expression> parseNewComponentExpression();

    // Parsing identifiers
    std::unique_ptr<Expression> parseIdentifier();

    //Parsing for expression
    std::unique_ptr<Expression> parseFunctionExpression();

    // Parsing block expressions
    std::unique_ptr<Expression> parseBlockExpression();

    // Parsing grouped expressions
    std::unique_ptr<Expression> parseGroupedExpression();

    // Parsing data type literals
    // Integer
    std::unique_ptr<Expression> parseIntegerLiteral();

    // Boolean
    std::unique_ptr<Expression> parseBooleanLiteral();

    // Float
    std::unique_ptr<Expression> parseFloatLiteral();

    // Char
    std::unique_ptr<Expression> parseCharLiteral();

    // String
    std::unique_ptr<Expression> parseStringLiteral();

    //Tuples
    std::unique_ptr<Expression> parseTupleExpression();

    //Parsing ++ or --
    std::unique_ptr<Expression> parsePostfixUnary();

    //Call expression parse function
    std::unique_ptr<Expression> parseCallExpression(std::unique_ptr<Expression> left);

    //Parsing call arguments
    std::vector<std::unique_ptr<Expression>> parseCallArguments();

    //Parsing function parameters
    std::vector<std::unique_ptr<Statement>> parseFunctionParameters();


    //HELPER FUNCTIONS
    // Peeking functions
    Token currentToken();
    Token nextToken();

    //Wrapper functions
    std::unique_ptr<Statement> parseLetStatementWithTypeWrapper();

    //Function to select how to parse grouped expression
    std::unique_ptr<Expression> parseGroupedOrTupleExpression();

    //Error logging 
    void logError(const std::string& message);

    //Getting the error token
    Token  getErrorToken();
};