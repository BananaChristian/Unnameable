#include "ast.hpp"
#include "errors.hpp"
#include "token.hpp"
#include <map>
#include <memory>
#include <string>
#include <vector>

#define CPPREST_FORCE_REBUILD

struct FileUnit {
  std::string fileName;
  std::vector<std::unique_ptr<Node>> nodes;
  std::vector<std::string> mergers;
};

class Parser {
  std::vector<Token> tokenInput;
  std::string fileName;
  int currentPos;
  int nextPos;

  // Precedence and token type map
  std::map<TokenType, Precedence> precedence{
      {TokenType::ASSIGN, Precedence::PREC_ASSIGNMENT},
      {TokenType::ARROW, Precedence::PREC_ASSIGNMENT},
      {TokenType::OR, Precedence::PREC_OR},
      {TokenType::AND, Precedence::PREC_AND},
      {TokenType::EQUALS, Precedence::PREC_EQUALITY},
      {TokenType::NOT_EQUALS, Precedence::PREC_EQUALITY},
      {TokenType::GREATER_THAN, Precedence::PREC_COMPARISON},
      {TokenType::LESS_THAN, Precedence::PREC_COMPARISON},
      {TokenType::GT_OR_EQ, Precedence::PREC_COMPARISON},
      {TokenType::LT_OR_EQ, Precedence::PREC_COMPARISON},
      {TokenType::BITWISE_OR, Precedence::PREC_BITWISE_OR},
      {TokenType::BITWISE_XOR, Precedence::PREC_BITWISE_XOR},
      {TokenType::BITWISE_AND, Precedence::PREC_BITWISE_AND},
      {TokenType::SHIFT_LEFT, Precedence::PREC_SHIFT},
      {TokenType::SHIFT_RIGHT, Precedence::PREC_SHIFT},
      {TokenType::PLUS, Precedence::PREC_TERM},
      {TokenType::MINUS, Precedence::PREC_TERM},
      {TokenType::ASTERISK, Precedence::PREC_FACTOR},
      {TokenType::DIVIDE, Precedence::PREC_FACTOR},
      {TokenType::MODULUS, Precedence::PREC_FACTOR},
      {TokenType::BANG, Precedence::PREC_UNARY},
      {TokenType::MINUS_MINUS, Precedence::PREC_UNARY},
      {TokenType::PLUS_PLUS, Precedence::PREC_UNARY},
      {TokenType::ADDR, Precedence::PREC_UNARY},
      {TokenType::DEREF, Precedence::PREC_UNARY},
      {TokenType::UNWRAP, Precedence::PREC_UNARY},
      {TokenType::BITWISE_NOT, Precedence::PREC_UNARY},
      {TokenType::FULLSTOP, Precedence::PREC_CALL},
      {TokenType::LPAREN, Precedence::PREC_CALL},
      {TokenType::SCOPE_OPERATOR, Precedence::PREC_CALL},
      {TokenType::AT, Precedence::PREC_CALL},
      {TokenType::IDENTIFIER, Precedence::PREC_PRIMARY},
      {TokenType::COALESCE, Precedence::PREC_COALESCE},
      {TokenType::LBRACE, Precedence::PREC_CALL},
  };

  Token lastToken;

  ErrorHandler errorHandler;

public:
  // Parser class declaration
  Parser(std::vector<Token> &tokenInput, const std::string &file);
  // Main parser program
  std::vector<std::unique_ptr<Node>> parseProgram();

  // Sliding across the token input from the lexer;
  void advance();

  // Functions for registering the functions neccesary for parsing the different
  // tokens
  void registerPrefixFns();
  void registerInfixFns();
  void registerPostfixFns();

  // Function to register statement parsing functions
  void registerStatementParseFns();

  // Function to get the precedence depending on the token type
  Precedence get_precedence(TokenType type);

  // Generator for the file unit
  std::shared_ptr<FileUnit> generateFileUnit();

  using prefixParseFns = std::unique_ptr<Expression> (Parser::*)();
  using infixParseFns =
      std::unique_ptr<Expression> (Parser::*)(std::unique_ptr<Expression>);
  using postfixParseFns =
      std::unique_ptr<Expression> (Parser::*)(std::unique_ptr<Expression>);

  using stmtParseFns = std::unique_ptr<Statement> (Parser::*)();

  std::map<TokenType, prefixParseFns> PrefixParseFunctionsMap;
  std::map<TokenType, infixParseFns> InfixParseFunctionsMap;
  std::map<TokenType, postfixParseFns> PostfixParseFunctionsMap;

  std::map<TokenType, stmtParseFns> StatementParseFunctionsMap;

private:
  std::vector<std::string> sourceLines;
  //---------------PARSING STATEMENTS--------------------
  // General statement parsing function
  std::unique_ptr<Statement> parseStatement();
  // Parsing let statements
  std::unique_ptr<Statement> parseLetStatement();
  std::unique_ptr<Statement> parseMutStatement();
  std::unique_ptr<Statement> parseConstStatement();
  // Parsing assignment statements
  std::unique_ptr<Statement> parseAssignmentStatement();
  std::unique_ptr<Statement> parseDereferenceAssignment();
  // Parsing heap statements
  std::unique_ptr<Statement> parseHeapStatement();
  std::unique_ptr<Statement> parseDHeapStatement();
  // Parsing allocator statement
  std::unique_ptr<Statement> parseAllocatorStatement();
  // Parsing field assignment such as b.item=10
  std::unique_ptr<Statement> parseFieldAssignment();
  // Parsing reference and pointer statement
  std::unique_ptr<Statement> parseReferenceStatement();
  std::unique_ptr<Statement> parsePointerStatement();
  // Parsing seal statement
  std::unique_ptr<Statement> parseSealStatement();
  // Shout statement parser
  std::unique_ptr<Statement> parseShoutStatement();

  std::unique_ptr<Statement> parseSelfAssignment();
  // Identifer starting statements function
  std::unique_ptr<Statement> parseIdentifierStatement();
  // Parsing if statement
  std::unique_ptr<Statement> parseIfStatement();
  // Parsing elif statement
  std::unique_ptr<Statement> parseElifStatement();
  // Parsing switch statement
  std::unique_ptr<Statement> parseSwitchStatement();
  // Parsing enum member Node
  std::unique_ptr<EnumMember> parseEnumMember();

  // Parsing enum class statement
  std::unique_ptr<Statement> parseEnumClassStatement();

  // Parsing case clause
  std::unique_ptr<Statement> parseCaseClause();

  // Parsing export statement
  std::unique_ptr<Statement> parseExportStatement();

  // Parsing the function statement
  std::unique_ptr<Statement> parseFunctionStatement();

  // Parsing return statements
  std::unique_ptr<Statement> parseReturnStatement();

  // Parsing for statement
  std::unique_ptr<Statement> parseForStatement();

  // Parsing while loops
  std::unique_ptr<Statement> parseWhileStatement();

  // Parsing break statement
  std::unique_ptr<Statement> parseBreakStatement();

  // Parsing continue statement
  std::unique_ptr<Statement> parseContinueStatement();

  // Parsing block statements
  std::unique_ptr<Statement> parseBlockStatement();

  // Parsing component statements
  std::unique_ptr<Statement> parseComponentStatement();

  // Parsing constructor statement
  std::unique_ptr<Statement> parseInitConstructorStatement();

  // Parsing use statements
  std::unique_ptr<Statement> parseUseStatement();

  // Parsing behavior statements
  std::unique_ptr<Statement> parseBehaviorStatement();

  // Parsing record statements
  std::unique_ptr<Statement> parseRecordStatement();

  // Parsing generic statements
  std::unique_ptr<Statement> parseGenericStatement();

  // Parsing instantiate statement
  std::unique_ptr<Statement> parseInstantiateStatement();

  // Parsing array statements
  std::unique_ptr<Statement> parseArrayStatement();

  // Parsing the qualify statement
  std::unique_ptr<Statement> parseQualifyStatement();

  // Parsing the merge statement
  std::unique_ptr<Statement> parseMergeStatement();

  // Parsing the import statement
  std::unique_ptr<Statement> parseImportStatement();

  // Parsing the link statement
  std::unique_ptr<Statement> parseLinkStatement();

  //--------------PARSING EXPRESSIONS--------------------
  // Main expression parsing function
  std::unique_ptr<Expression> parseExpression(Precedence precedence);

  // Infix expression parsing function
  std::unique_ptr<Expression>
  parseInfixExpression(std::unique_ptr<Expression> left);

  // Prefix expression parsing function
  std::unique_ptr<Expression> parsePrefixExpression();

  // Postfix expression parsing function
  std::unique_ptr<Expression>
  parsePostfixExpression(std::unique_ptr<Expression> left);

  // New component expression parse function declaration
  std::unique_ptr<Expression> parseNewComponentExpression();

  // Instance expression parse function
  std::unique_ptr<Expression>
  parseInstanceExpression(std::unique_ptr<Expression> left);

  // Method access call expression parse function
  std::unique_ptr<Expression>
  parseMethodCallExpression(std::unique_ptr<Expression> left);

  // Parsing unwrap expression
  std::unique_ptr<Expression> parseUnwrapExpression();

  // Parsing identifier expression
  std::unique_ptr<Expression> parseIdentifier();

  // Parsing address expression
  std::unique_ptr<Expression> parseAddressExpression();

  // Parsing dereference expression
  std::unique_ptr<Expression> parseDereferenceExpression();

  // Parsing array subscript expression
  std::unique_ptr<Expression> parseArraySubscript();

  // Decider for identifier or array subscript
  std::unique_ptr<Expression> parseIdentifierOrArraySubscript();

  // Parsing self expression
  std::unique_ptr<Expression> parseSelfExpression();

  // Parsing for expression
  std::unique_ptr<Expression> parseFunctionExpression();

  // Parsing for basic return type
  std::unique_ptr<Expression> parseBasicType();

  // Parsing for pointer return type
  std::unique_ptr<Expression> parsePointerType();

  // Parsing for reference type
  std::unique_ptr<Expression> parseRefType();

  // Parsing the return type expression
  std::unique_ptr<Expression> parseReturnType();

  // Parsing block expressions
  std::unique_ptr<Expression> parseBlockExpression();

  // Parsing grouped expressions
  std::unique_ptr<Expression> parseGroupedExpression();

  // Parsing data type literals
  // Signed 8 bit integer
  std::unique_ptr<Expression> parseI8Literal();

  // Unsigned 8 bit integer
  std::unique_ptr<Expression> parseU8Literal();

  // Signed 16 bit integer
  std::unique_ptr<Expression> parseI16Literal();

  // Unsigned 16 bit integer
  std::unique_ptr<Expression> parseU16Literal();

  // Signed 32 bit Integer
  std::unique_ptr<Expression> parseI32Literal();

  // Unsigned 32 bit integer
  std::unique_ptr<Expression> parseU32Literal();

  // Signed 64 bit integer
  std::unique_ptr<Expression> parseI64Literal();

  // UnSigned 64 bit integer
  std::unique_ptr<Expression> parseU64Literal();

  // Signed 128 bit integer
  std::unique_ptr<Expression> parseI128Literal();

  // Unsigned 128 bit integer
  std::unique_ptr<Expression> parseU128Literal();

  // Signed CPU native width integer
  std::unique_ptr<Expression> parseISIZELiteral();

  // Unsigned CPU native width integer
  std::unique_ptr<Expression> parseUSIZELiteral();

  // Boolean
  std::unique_ptr<Expression> parseBooleanLiteral();

  // Float
  std::unique_ptr<Expression> parseFloatLiteral();

  // Double
  std::unique_ptr<Expression> parseDoubleLiteral();

  // 8 bit Char
  std::unique_ptr<Expression> parseChar8Literal();

  // 16 bit Char
  std::unique_ptr<Expression> parseChar16Literal();

  // 32 bit Char
  std::unique_ptr<Expression> parseChar32Literal();

  // Null
  std::unique_ptr<Expression> parseNullLiteral();

  // String
  std::unique_ptr<Expression> parseStringLiteral();

  // Sizeof
  std::unique_ptr<Expression> parseSizeOfExpression();

  // Cast and bitcast expression
  std::unique_ptr<Expression> parseCastExpression();
  std::unique_ptr<Expression> parseBitcastExpression();
  // Array literal
  std::unique_ptr<Expression> parseArrayLiteral();
  // Array type parse function
  std::unique_ptr<Expression> parseArrayType();
  // Call expression parse function
  std::unique_ptr<Expression>
  parseCallExpression(std::unique_ptr<Expression> left);

  std::unique_ptr<Expression>
  parseInfixOrMethodCallExpression(std::unique_ptr<Expression> left);

  // Parsing call arguments
  std::vector<std::unique_ptr<Expression>> parseCallArguments();

  // Parsing function parameters
  std::vector<std::unique_ptr<Statement>> parseFunctionParameters();

  // HELPER FUNCTIONS
  //  Peeking functions
  Token currentToken();
  Token nextToken();
  Token peekToken(int peek);

  // Wrapper functions
  std::unique_ptr<Statement> parseLetStatementWithTypeWrapper();

  // Checker for genericsparseLetStatementWithTypeWrapper
  bool isGeneric(const std::string &typeName,
                 const std::vector<Token> &genericParams);

  // Declaration checker
  bool isDeclaration(Node *node);

  // Checker for basic types
  bool isBasicType(TokenType type);

  // Checker for integer types
  bool isIntegerType(TokenType type);

  // Checker for integer literals
  bool isIntegerLiteralType(TokenType type);

  // Error logging
  void logError(const std::string &message);

  // Getting the error token
  Token getErrorToken();
};
