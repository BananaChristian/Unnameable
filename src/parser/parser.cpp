#include "parser.hpp"
#include "ast.hpp"
#include <iostream>
#include <memory>
#include <vector>

#define CPPREST_FORCE_REBUILD

//--------------PARSER CLASS CONSTRUCTOR-------------
Parser::Parser(std::vector<Token> &tokenInput, const std::string &file)
    : tokenInput(tokenInput), fileName(file), errorHandler(file), currentPos(0),
      nextPos(1) {
  lastToken = tokenInput.empty() ? Token{"", TokenType::ILLEGAL, 999, 999}
                                 : tokenInput[0];
  registerInfixFns();
  registerPrefixFns();
  registerPostfixFns();
  registerStatementParseFns();
}

// MAIN PARSER FUNCTION
std::vector<std::unique_ptr<Node>> Parser::parseProgram() {
  std::vector<std::unique_ptr<Node>> program;

  while (currentPos < tokenInput.size()) {
    // Skip standalone semicolons between statements
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
      continue;
    }

    if (currentToken().type == TokenType::END) {
      std::cout << "[DEBUG] Parser reached END token. Breaking loop.\n";
      break;
    }

    std::cout << "Parsing token: " << currentToken().TokenLiteral << "\n";
    Token current = currentToken();

    if (current.type == TokenType::END)
      break;

    std::unique_ptr<Node> node = parseStatement();

    if (node) {
      program.push_back(std::move(node));
    } else {
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
std::unique_ptr<Statement> Parser::parseStatement() {
  Token current = currentToken();
  std::cout << "[DEBUG] parseStatement starting with token: "
            << current.TokenLiteral << "\n";

  // Handle self.* assignments

  // Other statement types from map
  auto stmtFnIt = StatementParseFunctionsMap.find(current.type);
  if (stmtFnIt != StatementParseFunctionsMap.end()) {
    return (this->*stmtFnIt->second)();
  }

  // Fallback expression statement
  auto expr = parseExpression(Precedence::PREC_NONE);
  if (expr) {
    if (currentToken().type == TokenType::SEMICOLON)
      advance();
    else
      logError("Expected ';' after expression statement but got '" +
               currentToken().TokenLiteral + "'");

    return std::make_unique<ExpressionStatement>(current, std::move(expr));
  }

  advance();
  return nullptr;
}

// _____________DIRECTIVES______________________
std::unique_ptr<Statement> Parser::parseQualifyStatement() {
  Token qualify_token = currentToken();
  advance(); // Consume the qualify token

  std::unique_ptr<Expression> expr = parseIdentifier();

  return std::make_unique<QualifyStatement>(qualify_token, std::move(expr));
}

std::unique_ptr<Statement> Parser::parseMergeStatement() {
  Token merge_token = currentToken();
  advance(); // Consume the merge token
  auto merged = parseStringLiteral();

  return std::make_unique<MergeStatement>(merge_token, std::move(merged));
}

std::unique_ptr<Statement> Parser::parseImportStatement() {
  Token import_token = currentToken();
  advance();

  auto importStr = parseStringLiteral();

  return std::make_unique<ImportStatement>(import_token, std::move(importStr));
}

std::unique_ptr<Statement> Parser::parseLinkStatement() {
  Token link_token = currentToken();
  advance();

  auto linkStr = parseStringLiteral();

  return std::make_unique<LinkStatement>(link_token, std::move(linkStr));
}

std::unique_ptr<Statement> Parser::parseShoutStatement() {
  Token shout = currentToken();
  advance(); // Consume the shout token
  if (currentToken().type != TokenType::BANG) {
    logError("Expected '!' but got '" + currentToken().TokenLiteral + "'");
    advance(); // Consume whatever is there
  }
  advance(); // Consume the ! token
  auto expr = parseExpression(Precedence::PREC_NONE);

  return std::make_unique<ShoutStatement>(shout, std::move(expr));
}

//----------HELPER FUNCTIONS---------------
bool Parser::isIntegerType(TokenType type) {
  switch (type) {
  case TokenType::I8_KEYWORD:
  case TokenType::U8_KEYWORD:
  case TokenType::I16_KEYWORD:
  case TokenType::U16_KEYWORD:
  case TokenType::I32_KEYWORD:
  case TokenType::U32_KEYWORD:
  case TokenType::I64_KEYWORD:
  case TokenType::U64_KEYWORD:
  case TokenType::I128_KEYWORD:
  case TokenType::U128_KEYWORD:
  case TokenType::ISIZE_KEYWORD:
  case TokenType::USIZE_KEYWORD:
    return true;
  default:
    return false;
  }
}

bool Parser::isIntegerLiteralType(TokenType type) {
  switch (type) {
  case TokenType::INT8:
  case TokenType::UINT8:
  case TokenType::INT16:
  case TokenType::UINT16:
  case TokenType::INT32:
  case TokenType::UINT32:
  case TokenType::INT64:
  case TokenType::UINT64:
  case TokenType::INT128:
  case TokenType::UINT128:
  case TokenType::INTSIZE:
  case TokenType::UINTSIZE:
    return true;
  default:
    return false;
  }
}

// Checker for basic data types
bool Parser::isBasicType(TokenType type) {
  switch (type) {
  case TokenType::I8_KEYWORD:
  case TokenType::U8_KEYWORD:
  case TokenType::I16_KEYWORD:
  case TokenType::U16_KEYWORD:
  case TokenType::I32_KEYWORD:
  case TokenType::U32_KEYWORD:
  case TokenType::I64_KEYWORD:
  case TokenType::U64_KEYWORD:
  case TokenType::I128_KEYWORD:
  case TokenType::U128_KEYWORD:
  case TokenType::ISIZE_KEYWORD:
  case TokenType::USIZE_KEYWORD:
  case TokenType::FLOAT_KEYWORD:
  case TokenType::DOUBLE_KEYWORD:
  case TokenType::CHAR8_KEYWORD:
  case TokenType::CHAR16_KEYWORD:
  case TokenType::CHAR32_KEYWORD:
  case TokenType::STRING_KEYWORD:
  case TokenType::BOOL_KEYWORD:
    return true;
  default:
    return false;
  }
}

// Slider function
void Parser::advance() {
  if (nextPos < tokenInput.size()) {
    lastToken = currentToken();
    currentPos = nextPos;
    std::cout << "Current token :" << currentToken().TokenLiteral << "\n";
    nextPos++;
  }
}

// Registration functions
// Registering infix functions for a particular token type
void Parser::registerInfixFns() {
  InfixParseFunctionsMap[TokenType::PLUS] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::MINUS] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::DIVIDE] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::ASTERISK] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::MODULUS] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::GREATER_THAN] =
      &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::LESS_THAN] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::GT_OR_EQ] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::LT_OR_EQ] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::BITWISE_AND]=&Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::BITWISE_OR]=&Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::SHIFT_LEFT]=&Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::SHIFT_RIGHT]=&Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::FULLSTOP] =
      &Parser::parseInfixOrMethodCallExpression;
  InfixParseFunctionsMap[TokenType::SCOPE_OPERATOR] =
      &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::AND] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::OR] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::AT] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::NOT_EQUALS] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::EQUALS] = &Parser::parseInfixExpression;
  InfixParseFunctionsMap[TokenType::LPAREN] = &Parser::parseCallExpression;
  InfixParseFunctionsMap[TokenType::LBRACE] = &Parser::parseInstanceExpression;
  InfixParseFunctionsMap[TokenType::COALESCE] = &Parser::parseInfixExpression;
}

// Registering prefix functions for a particular token type
void Parser::registerPrefixFns() {
  PrefixParseFunctionsMap[TokenType::INT8] = &Parser::parseI8Literal;
  PrefixParseFunctionsMap[TokenType::UINT8] = &Parser::parseU8Literal;
  PrefixParseFunctionsMap[TokenType::INT16] = &Parser::parseI16Literal;
  PrefixParseFunctionsMap[TokenType::UINT16] = &Parser::parseU16Literal;
  PrefixParseFunctionsMap[TokenType::INT32] = &Parser::parseI32Literal;
  PrefixParseFunctionsMap[TokenType::UINT32] = &Parser::parseU32Literal;
  PrefixParseFunctionsMap[TokenType::INT64] = &Parser::parseI64Literal;
  PrefixParseFunctionsMap[TokenType::UINT64] = &Parser::parseU64Literal;
  PrefixParseFunctionsMap[TokenType::INT128] = &Parser::parseI128Literal;
  PrefixParseFunctionsMap[TokenType::UINT128] = &Parser::parseU128Literal;
  PrefixParseFunctionsMap[TokenType::INTSIZE] = &Parser::parseISIZELiteral;
  PrefixParseFunctionsMap[TokenType::UINTSIZE] = &Parser::parseUSIZELiteral;

  PrefixParseFunctionsMap[TokenType::CHAR8] = &Parser::parseChar8Literal;
  PrefixParseFunctionsMap[TokenType::CHAR16] = &Parser::parseChar16Literal;
  PrefixParseFunctionsMap[TokenType::CHAR32] = &Parser::parseChar32Literal;

  PrefixParseFunctionsMap[TokenType::NULLABLE] = &Parser::parseNullLiteral;
  PrefixParseFunctionsMap[TokenType::TRUE] = &Parser::parseBooleanLiteral;
  PrefixParseFunctionsMap[TokenType::FALSE] = &Parser::parseBooleanLiteral;
  PrefixParseFunctionsMap[TokenType::FLOAT] = &Parser::parseFloatLiteral;
  PrefixParseFunctionsMap[TokenType::DOUBLE] = &Parser::parseDoubleLiteral;
  PrefixParseFunctionsMap[TokenType::STRING] = &Parser::parseStringLiteral;
  PrefixParseFunctionsMap[TokenType::SIZEOF] = &Parser::parseSizeOfExpression;

  PrefixParseFunctionsMap[TokenType::IDENTIFIER] =
      &Parser::parseIdentifierOrArraySubscript;
  PrefixParseFunctionsMap[TokenType::ADDR] = &Parser::parseAddressExpression;
  PrefixParseFunctionsMap[TokenType::DEREF] =
      &Parser::parseDereferenceExpression;
  PrefixParseFunctionsMap[TokenType::NEW] =
      &Parser::parseNewComponentExpression;
  PrefixParseFunctionsMap[TokenType::SELF] = &Parser::parseSelfExpression;
  PrefixParseFunctionsMap[TokenType::UNWRAP] = &Parser::parseUnwrapExpression;
  PrefixParseFunctionsMap[TokenType::BANG] = &Parser::parsePrefixExpression;
  PrefixParseFunctionsMap[TokenType::MINUS] = &Parser::parsePrefixExpression;
  PrefixParseFunctionsMap[TokenType::BITWISE_NOT]=&Parser::parsePrefixExpression;
  PrefixParseFunctionsMap[TokenType::LPAREN] = &Parser::parseGroupedExpression;
  PrefixParseFunctionsMap[TokenType::LBRACKET] = &Parser::parseArrayLiteral;
  PrefixParseFunctionsMap[TokenType::LBRACE] = &Parser::parseBlockExpression;
  PrefixParseFunctionsMap[TokenType::PLUS_PLUS] =
      &Parser::parsePrefixExpression;
  PrefixParseFunctionsMap[TokenType::MINUS_MINUS] =
      &Parser::parsePrefixExpression;
  PrefixParseFunctionsMap[TokenType::FUNCTION] =
      &Parser::parseFunctionExpression;
}

// Registering the postfix parse functions
void Parser::registerPostfixFns() {
  PostfixParseFunctionsMap[TokenType::PLUS_PLUS] =
      &Parser::parsePostfixExpression;
  PostfixParseFunctionsMap[TokenType::MINUS_MINUS] =
      &Parser::parsePostfixExpression;
}

// Identifer overloader parser
std::unique_ptr<Statement> Parser::parseIdentifierStatement() {
  Token current = currentToken();
  std::cout << "IDENTIFIER TOKEN INSIDE IDENTIFIER STATEMENT PARSER: "
            << current.TokenLiteral << "\n";

  auto peekAfterSubscript = [this](int startOffset = 1) -> Token {
    int offset = startOffset;

    // Must start on '['
    if (peekToken(offset).type != TokenType::LBRACKET)
      return peekToken(offset);

    while (peekToken(offset).type == TokenType::LBRACKET) {
      int depth = 0;

      // Walk until we close this bracket chain
      while (true) {
        Token t = peekToken(offset);

        if (t.type == TokenType::LBRACKET)
          depth++;
        else if (t.type == TokenType::RBRACKET) {
          depth--;
          if (depth == 0) {
            offset++; // move past the closing ']'
            break;
          }
        } else if (t.type == TokenType::END) {
          return Token{"", TokenType::END, t.line, t.column};
        }

        offset++;
      }
    }

    return peekToken(offset);
  };

  Token peek1 = peekToken(1);
  if (peek1.type == TokenType::LBRACKET) {
    std::cout << "PEEK AFTER SUBSCRIPT :" << peekAfterSubscript().TokenLiteral
              << "\n";
    if (peekAfterSubscript().type == TokenType::ASSIGN) {
      std::cout << "SUBSCRIPT ASSIGNMENT DETECTED\n";
      return parseAssignmentStatement();
    }
  }

  // Simple assignment (x = ...)
  if (peek1.type == TokenType::ASSIGN) {
    std::cout << "Identifier taken assign path\n";
    return parseAssignmentStatement();
  }

  // Cases where there is a custom type like(Type var)
  if (peek1.type == TokenType::IDENTIFIER) {
    std::cout << "Identifier taken let statement path\n";
    if (peekToken(2).type == TokenType::SEMICOLON) {
      return parseLetStatementCustomOrBasic();
    }
    return parseLetStatementCustomOrBasic();
  }

  // Qualified assignment (x.y = ..., test::field = ...)
  if (peek1.type == TokenType::FULLSTOP ||
      peek1.type == TokenType::SCOPE_OPERATOR) {
    Token peek3 = peekToken(3);
    if (peek3.type == TokenType::ASSIGN)
      return parseFieldAssignment();
  }

  // Fall back to generic expression statement
  auto expr = parseExpression(Precedence::PREC_NONE);
  if (expr) {
    if (currentToken().type == TokenType::SEMICOLON)
      advance();
    else
      logError("Expected ';' after expression statement but got '" +
               currentToken().TokenLiteral + "'");

    return std::make_unique<ExpressionStatement>(current, std::move(expr));
  }

  logError("Invalid identifier statement");
  return nullptr;
}

std::unique_ptr<Statement> Parser::parseErrorStatement() {
  Token err_token = currentToken();
  advance(); // Consume the error token

  if (currentToken().type != TokenType::BANG) {
    logError("Expected ! but got " + currentToken().TokenLiteral);
    advance(); // Consume the erronious token
  }
  advance(); // Consume the ! token
  std::unique_ptr<Expression> errExpr = nullptr;

  errExpr = parseExpression(Precedence::PREC_NONE);

  return std::make_unique<ErrorStatement>(err_token, std::move(errExpr));
}

// Registering the statement parsing functions
void Parser::registerStatementParseFns() {
  StatementParseFunctionsMap[TokenType::RETURN] = &Parser::parseReturnStatement;
  StatementParseFunctionsMap[TokenType::IF] = &Parser::parseIfStatement;
  StatementParseFunctionsMap[TokenType::WHILE] = &Parser::parseWhileStatement;
  StatementParseFunctionsMap[TokenType::FOR] = &Parser::parseForStatement;
  StatementParseFunctionsMap[TokenType::BREAK] = &Parser::parseBreakStatement;
  StatementParseFunctionsMap[TokenType::CONTINUE] =
      &Parser::parseContinueStatement;
  StatementParseFunctionsMap[TokenType::QUALIFY] =
      &Parser::parseQualifyStatement;
  StatementParseFunctionsMap[TokenType::MERGE] = &Parser::parseMergeStatement;
  StatementParseFunctionsMap[TokenType::LINK] = &Parser::parseLinkStatement;
  StatementParseFunctionsMap[TokenType::IMPORT] = &Parser::parseImportStatement;
  StatementParseFunctionsMap[TokenType::SHOUT] = &Parser::parseShoutStatement;

  // For basic types
  StatementParseFunctionsMap[TokenType::I8_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::U8_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::I16_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::U16_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::I32_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::U32_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::I64_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::U64_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::I128_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::U128_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::USIZE_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::ISIZE_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;

  StatementParseFunctionsMap[TokenType::CHAR8_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::CHAR16_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::CHAR32_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  // For custom types
  StatementParseFunctionsMap[TokenType::IDENTIFIER] =
      &Parser::parseIdentifierStatement;
  StatementParseFunctionsMap[TokenType::SELF] = &Parser::parseSelfAssignment;

  StatementParseFunctionsMap[TokenType::FLOAT_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::DOUBLE_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::STRING_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::BOOL_KEYWORD] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::CONST] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::MUT] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::FUNCTION] =
      &Parser::parseFunctionStatement;
  StatementParseFunctionsMap[TokenType::AUTO] =
      &Parser::parseLetStatementWithTypeWrapper;
  StatementParseFunctionsMap[TokenType::ERROR] = &Parser::parseErrorStatement;
  StatementParseFunctionsMap[TokenType::COMPONENT] =
      &Parser::parseComponentStatement;
  StatementParseFunctionsMap[TokenType::BEHAVIOR] =
      &Parser::parseBehaviorStatement;
  StatementParseFunctionsMap[TokenType::RECORD] = &Parser::parseRecordStatement;
  StatementParseFunctionsMap[TokenType::USE] = &Parser::parseUseStatement;
  StatementParseFunctionsMap[TokenType::INIT] =
      &Parser::parseInitConstructorStatement;
  StatementParseFunctionsMap[TokenType::SWITCH] = &Parser::parseSwitchStatement;
  StatementParseFunctionsMap[TokenType::ENUM] =
      &Parser::parseEnumClassStatement;

  StatementParseFunctionsMap[TokenType::GENERIC] =
      &Parser::parseGenericStatement;
  StatementParseFunctionsMap[TokenType::INSTANTIATE] =
      &Parser::parseInstantiateStatement;

  StatementParseFunctionsMap[TokenType::ARRAY] =
      &Parser::parseArrayStatementWrapper;
  StatementParseFunctionsMap[TokenType::HEAP] = &Parser::parseHeapStatement;
  StatementParseFunctionsMap[TokenType::DHEAP] = &Parser::parseDHeapStatement;
  StatementParseFunctionsMap[TokenType::ALLOCATOR] =
      &Parser::parseAllocatorStatement;
  StatementParseFunctionsMap[TokenType::SEAL] = &Parser::parseSealStatement;
  StatementParseFunctionsMap[TokenType::EXPORT] = &Parser::parseExportStatement;
  StatementParseFunctionsMap[TokenType::REF] =
      &Parser::parseReferenceStatementWrapper;
  StatementParseFunctionsMap[TokenType::PTR] =
      &Parser::parsePointerStatementWrapper;
  StatementParseFunctionsMap[TokenType::DEREF] =
      &Parser::parseDereferenceAssignment;
}

// Precedence getting function
Precedence Parser::get_precedence(TokenType type) {
  Precedence prec =
      precedence.count(type) ? precedence[type] : Precedence::PREC_NONE;
  return prec;
}

// Current token peeking function
Token Parser::currentToken() {
  Token current = tokenInput[currentPos];
  return current;
}

// Next token peeking function
Token Parser::nextToken() {
  Token next = tokenInput[nextPos];
  return next;
}

Token Parser::peekToken(int peek) {
  int steps = currentPos + peek;
  if (steps >= tokenInput.size()) {
    return Token{"", TokenType::END, 0, 0}; // EOF token
  }
  return tokenInput[steps];
}

bool Parser::isDeclaration(Node *node) {
  bool isDecl = false;
  if (auto letStmt = dynamic_cast<LetStatement *>(node)) {
    isDecl = true;
  } else if (auto ptrStmt = dynamic_cast<PointerStatement *>(node)) {
    isDecl = true;
  } else if (auto arrStmt = dynamic_cast<ArrayStatement *>(node)) {
    isDecl = true;
  } else if (auto refStmt = dynamic_cast<ReferenceStatement *>(node)) {
    isDecl = true;
  }

  return isDecl;
}

// Error logging
void Parser::logError(const std::string &message) {
  Token token = getErrorToken();

  if (token.line == 0 && token.column == 0) {
    std::cerr
        << "[PANIC]: Logging an uninitialized token! Investigate token flow.\n";
  }

  CompilerError error;
  error.level = ErrorLevel::PARSER;
  error.line = token.line;
  error.col = token.column;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);
}

// Generic checker
bool Parser::isGeneric(const std::string &typeName,
                       const std::vector<Token> &genericParams) {
  for (const auto &param : genericParams) {
    if (param.TokenLiteral == typeName && param.type == TokenType::IDENTIFIER) {
      return true;
    }
  }
  return false;
}

// Getting the error token
Token Parser::getErrorToken() {
  if (currentPos >= tokenInput.size()) {
    if (lastToken.line == 0) {
      return Token{"", TokenType::ILLEGAL, 999, 999};
    }
    return lastToken;
  }
  return tokenInput[currentPos - 1];
}

std::shared_ptr<FileUnit> Parser::generateFileUnit() {
  // Populate the fields final field
  std::vector<std::unique_ptr<Node>> nodes = parseProgram();
  auto fileUnit = std::make_shared<FileUnit>();
  fileUnit->nodes = std::move(nodes);

  // Scan AST for imports and entry qualifier
  for (auto &node : fileUnit->nodes) {
    if (auto *mergeStmt = dynamic_cast<MergeStatement *>(node.get())) {
      fileUnit->mergers.push_back(
          mergeStmt->stringExpr->expression.TokenLiteral);
    }
  }
  return fileUnit;
}
