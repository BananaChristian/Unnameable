#include "ast.hpp"
#include "parser.hpp"
#include <memory>

// Parsing function paramemters
std::vector<std::unique_ptr<Statement>> Parser::parseFunctionParameters() {
  std::vector<std::unique_ptr<Statement>> args; // Declaring the empty vector

  // Optional parentheses if no parameters
  bool hasParens = false;
  if (currentToken().type == TokenType::LPAREN) {
    hasParens = true;
    advance(); // move past '('
  }

  // Check for empty parameter list or no parentheses
  if (hasParens && currentToken().type == TokenType::RPAREN) {
    advance(); // move past ')'
    if (currentToken().type != TokenType::COLON) {
      logError("Expected ':' after empty parameter list but got '" +
               currentToken().TokenLiteral + "'",currentToken());
    }
    return args; // empty vector
  }

  // If no LPAREN, and current token is COLON, allow no-parameter function
  if (!hasParens && currentToken().type == TokenType::COLON) {
    return args; // empty vector, no parentheses needed
  }

  // Now we parse the first parameter (if any)
  auto firstParam = parseStatement();
  if (!firstParam) {
    return args;
  }
  args.push_back(std::move(firstParam));

  while (currentToken().type == TokenType::COMMA) {
    advance();
    auto arg = parseStatement();
    if (!arg) {
      return args;
    }
    args.push_back(std::move(arg));
  }

  // If we used parentheses, we must have closing ')'
  if (hasParens && currentToken().type != TokenType::RPAREN) {
    logError("Expected ')' after function parameters but got '" +
             currentToken().TokenLiteral + "'",currentToken());
    return args;
  }
  if (hasParens)
    advance();

  // Check colon after parameter list
  if (currentToken().type != TokenType::COLON) {
    logError("Expected ':' after function declaration",currentToken());
  }

  return args;
}

// Parsing function expression
std::unique_ptr<Expression> Parser::parseFunctionExpression() {
  bool isExportable = false;
  //--------Dealing with func keyword---------------
  Token func_tok =
      currentToken(); // The token representing the keyword for functions (func)
  advance();

  //----------Dealing with function name------------
  auto identExpr = parseIdentifier();
  auto identNode = dynamic_cast<Identifier *>(identExpr.get());

  if (!identNode) {
    logError("Expected identifier for function name after 'func' but got '" +
             currentToken().TokenLiteral + "'",currentToken());
    return nullptr;
  }

  Token identToken = identNode->identifier;

  //---Dealing with the call itself
  auto call = parseFunctionParameters(); // We might get some arguments or not
                                         // so we call the parse call expression

  std::unique_ptr<Expression> return_type = nullptr;
  //--Checking for colons
  if (currentToken().type == TokenType::COLON) {
    advance(); // Move past the colon signs
    if (isBasicType(currentToken().type) ||
        currentToken().type == TokenType::IDENTIFIER ||
        currentToken().type == TokenType::VOID ||
        currentToken().type == TokenType::ARRAY ||
        currentToken().type == TokenType::PTR ||
        currentToken().type == TokenType::REF) {
      return_type = parseReturnType();
    }

    else {
      logError("Unexpected return type '" + currentToken().TokenLiteral + "'",currentToken());
      return nullptr;
    }
  }

  if (currentToken().type == TokenType::SEMICOLON) {
    advance(); // consume the semicolon
  }

  if (currentToken().type != TokenType::LBRACE) {
    auto decl = std::make_unique<FunctionDeclaration>(
        isExportable, func_tok, std::move(identExpr), std::move(call),
        std::move(return_type));

    return std::make_unique<FunctionDeclarationExpression>(func_tok,
                                                           std::move(decl));
  }

  auto block = parseBlockExpression(); // Parsing the blocks
  if (!block) {
    return nullptr;
  }

  return std::make_unique<FunctionExpression>(
      isExportable, identToken, std::move(call), std::move(return_type),
      std::move(block));
}

// Parsing block expressions
std::unique_ptr<Expression> Parser::parseBlockExpression() {
  Token lbrace = currentToken();
  if (lbrace.type != TokenType::LBRACE) {
    logError("Expected { after data type but got '" +
             currentToken().TokenLiteral + "'",currentToken());
    return nullptr;
  }
  advance();
  auto block = std::make_unique<BlockExpression>(lbrace);
  while (currentToken().type != TokenType::RBRACE) {
    if (currentToken().type == TokenType::END) {
      logError("Unterminated block experession",currentToken());
      return nullptr;
    }

    auto stmt = parseStatement();
    if (stmt) {
      block->statements.push_back(std::move(stmt));
    }
  }

  if (currentToken().type != TokenType::RBRACE) {
    logError("Expected } but got '" + currentToken().TokenLiteral + "'",currentToken());
    advance();
    return nullptr;
  }

  advance();
  return block;
}

// Parsing function statement
std::unique_ptr<Statement> Parser::parseFunctionStatement() {
  Token funcToken = currentToken();

  std::unique_ptr<Expression> funcExpr = parseFunctionExpression();
  if (!funcExpr) {
    return nullptr;
  }

  return std::make_unique<FunctionStatement>(funcToken, std::move(funcExpr));
}

// Parsing return statements
std::unique_ptr<Statement> Parser::parseReturnStatement() {
  Token return_stmt = currentToken();
  advance();

  std::unique_ptr<Expression> return_value =
      parseExpression(Precedence::PREC_NONE);

  if (!return_value) {
    return std::make_unique<ReturnStatement>(return_stmt, nullptr);
  }

  return std::make_unique<ReturnStatement>(return_stmt,
                                           std::move(return_value));
}
