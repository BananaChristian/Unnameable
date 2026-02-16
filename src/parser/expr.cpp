#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"
#include <memory>

// Main Expression parsing function
std::unique_ptr<Expression> Parser::parseExpression(Precedence precedence) {
  auto PrefixParseFnIt = PrefixParseFunctionsMap.find(
      currentToken()
          .type); // Creating an iterator pointer to loop through the map

  if (PrefixParseFnIt ==
      PrefixParseFunctionsMap
          .end()) // Checking if the iterator has reached the end of the map
  {
    return nullptr;
  }

  auto left_expression =
      (this->*PrefixParseFnIt
                  ->second)(); // Calling the neccesary prefix function after
                               // encountering that particular token

  while (true) {
    if (currentToken().type == TokenType::SEMICOLON ||
        currentToken().type == TokenType::RBRACE ||
        currentToken().type == TokenType::RPAREN ||
        currentToken().type == TokenType::COMMA) {
      break;
    }
    Precedence currentPrecedence = get_precedence(currentToken().type);

    // If current token is a postfix operator (e.g., ++, --)
    auto PostfixParseFnIt = PostfixParseFunctionsMap.find(currentToken().type);
    if (PostfixParseFnIt != PostfixParseFunctionsMap.end()) {
      // Only parse if postfix precedence is >= incoming precedence
      if (precedence >= currentPrecedence)
        break;

      left_expression =
          (this->*PostfixParseFnIt->second)(std::move(left_expression));

      continue;
    }

    // Handle infix operators
    auto InfixParseFnIt = InfixParseFunctionsMap.find(currentToken().type);
    if (InfixParseFnIt == InfixParseFunctionsMap.end()) {
      break;
    }

    if (precedence >= currentPrecedence)
      break;

    left_expression =
        (this->*InfixParseFnIt->second)(std::move(left_expression));
  }

  return left_expression; // Returning the expression that was parsed it can be
                          // either prefix or infix
}

// Inifix parse function definition
std::unique_ptr<Expression>
Parser::parseInfixExpression(std::unique_ptr<Expression> left) {
  Token operat = currentToken();

  Precedence prec = get_precedence(operat.type);
  advance();
  auto right = parseExpression(prec);
  return std::make_unique<InfixExpression>(std::move(left), operat,
                                           std::move(right));
}

// Prefix parse function definition
std::unique_ptr<Expression> Parser::parsePrefixExpression() {
  Token operat = currentToken();
  Precedence operatorPrecedence = get_precedence(operat.type);
  advance();
  auto operand = parseExpression(operatorPrecedence);
  return std::make_unique<PrefixExpression>(operat, std::move(operand));
}

// Postfix expression parser function definition
std::unique_ptr<Expression>
Parser::parsePostfixExpression(std::unique_ptr<Expression> left) {
  Token op = currentToken();
  advance(); // Consume ++ or --
  return std::make_unique<PostfixExpression>(std::move(left), op);
}

// Parsing address expression
std::unique_ptr<Expression> Parser::parseAddressExpression() {
  Token addr_token = currentToken();
  Precedence operatorPrecedence = get_precedence(addr_token.type);
  advance(); // Consume addr token

  auto ident = parseExpression(operatorPrecedence);

  return std::make_unique<AddressExpression>(addr_token, std::move(ident));
}

// Parsing move expression
std::unique_ptr<Expression> Parser::parseMoveExpression() {
  Token move_token = currentToken();
  Precedence opPrecedence = get_precedence(move_token.type);
  advance(); // Consume the move token

  auto expr = parseExpression(opPrecedence);
  return std::make_unique<MoveExpression>(move_token, std::move(expr));
}

// Parsing dereference expression
std::unique_ptr<Expression> Parser::parseDereferenceExpression() {
  Token deref_token = currentToken();
  Precedence operatorPrecedence = get_precedence(deref_token.type);
  advance(); // Consume deref token
  auto ident = parseExpression(operatorPrecedence);

  return std::make_unique<DereferenceExpression>(deref_token, std::move(ident));
}

// Parsing identifier expression
std::unique_ptr<Expression> Parser::parseIdentifier() {
  bool isKiller = false;
  auto ident = std::make_unique<Identifier>(currentToken(), isKiller);
  if (!ident) {
    logError("Failed to parse identifier '" + currentToken().TokenLiteral + "'",
             currentToken());
  }
  advance();
  return ident;
}

// Parsing array subscript expression
std::unique_ptr<Expression> Parser::parseArraySubscript() {
  auto ident = parseIdentifier();
  std::vector<std::unique_ptr<Expression>> indexes;

  while (currentToken().type == TokenType::LBRACKET) {
    advance(); // Consume [
    std::unique_ptr<Expression> index;
    index = parseExpression(Precedence::PREC_NONE);

    if (!index)
      return nullptr;

    if (currentToken().type != TokenType::RBRACKET) {
      logError("Expected ']' after index but got '" +
                   currentToken().TokenLiteral + "'",
               currentToken());
      return nullptr;
    }
    advance(); // Consume ]
    indexes.push_back(std::move(index));
  }

  return std::make_unique<ArraySubscript>(std::move(ident), std::move(indexes));
}

std::unique_ptr<Expression> Parser::parseIdentifierOrArraySubscript() {
  if (nextToken().type == TokenType::LBRACKET) {
    return parseArraySubscript();
  } else if (nextToken().type == TokenType::LPAREN) {
    return parseCallExpression();
  }
  return parseIdentifier();
}

// Parsing self expression
std::unique_ptr<Expression> Parser::parseSelfExpression() {
  Token self_token = currentToken();
  advance();

  std::vector<std::unique_ptr<Expression>> fields;

  while (currentToken().type == TokenType::FULLSTOP) {
    advance(); // skip '.'

    if (currentToken().type != TokenType::IDENTIFIER) {
      logError("Expected identifier after '.' in self expression but got '" +
                   currentToken().TokenLiteral + "'",
               currentToken());
      return nullptr;
    }

    auto fieldIdent = std::make_unique<Identifier>(currentToken(), false);
    fields.push_back(std::move(fieldIdent));

    advance(); // move past the identifier
  }

  return std::make_unique<SelfExpression>(self_token, std::move(fields));
}

// Parse new component expression
std::unique_ptr<Expression> Parser::parseNewComponentExpression() {
  Token new_token = currentToken();
  advance(); // Consuming the new keyword open
  Token component_name = currentToken();
  if (component_name.type != TokenType::IDENTIFIER) {
    logError("Expected identifier for component name after new but got '" +
                 component_name.TokenLiteral + "'",
             currentToken());
    return nullptr;
  }
  advance(); // Consuming the component name
  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected '(' after component name in 'new' expression but got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }
  advance(); // Consume the lparen
  auto args = parseCallArguments();

  return std::make_unique<NewComponentExpression>(new_token, component_name,
                                                  std::move(args));
}

// Grouped expression parse function
std::unique_ptr<Expression> Parser::parseGroupedExpression() {
  advance(); // Consume the ( token
  auto expr = parseExpression(Precedence::PREC_NONE);
  if (!expr) {
    logError("Empty grouped expression after '('", currentToken());
    return nullptr;
  }

  if (currentToken().type != TokenType::RPAREN) {
    logError("Expected ')' to close grouped expression but got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  advance(); // consume ')'
  return expr;
}

// Parsing the method call expression
std::unique_ptr<Expression>
Parser::parseMethodCallExpression(std::unique_ptr<Expression> left) {
  advance(); // consume FULLSTOP

  auto callExpr = parseCallExpression();

  return std::make_unique<MethodCallExpression>(std::move(left),
                                                std::move(callExpr));
}

// Parsing function call arguments
std::vector<std::unique_ptr<Expression>> Parser::parseCallArguments() {
  std::vector<std::unique_ptr<Expression>> args;
  if (currentToken().type == TokenType::RPAREN) {
    advance(); // Consume the ) token
    return args;
  }

  auto firstArg = parseExpression(Precedence::PREC_NONE);
  if (!firstArg) {
    return args;
  }
  args.push_back(std::move(firstArg));

  while (currentToken().type == TokenType::COMMA) {
    advance();
    auto arg = parseExpression(Precedence::PREC_NONE);

    if (!arg) {
      return args;
    }
    args.push_back(std::move(arg));
  }

  if (currentToken().type == TokenType::RPAREN) {
    advance(); // consume ')'
  } else {
    logError("Expected ')' after function arguments", currentToken());
  }

  return args;
}

// Parsing the call expression
std::unique_ptr<Expression> Parser::parseCallExpression() {
  auto ident = parseIdentifier();

  Token call_token = currentToken(); // We expect a left parenthesis here

  if (call_token.type !=
      TokenType::LPAREN) { // Checking if we encounter the left parenthesis
                           // after the function name has been declared
    logError("Expected ( after function name bug got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    advance();
    return nullptr;
  }

  advance(); // Advancing the pointer to look at what is inside the brackets

  auto args = parseCallArguments(); // Calling the parse call arguments inorder
                                    // to parse the arguments

  return std::make_unique<CallExpression>(call_token, std::move(ident),
                                          std::move(args));
}

// Unwrap call parse
std::unique_ptr<Expression> Parser::parseUnwrapExpression() {
  Token unwrap = currentToken();
  Precedence operatorPrecedence = get_precedence(unwrap.type);
  advance(); // Consume the unwrap token
  std::unique_ptr<Expression> expr = parseExpression(operatorPrecedence);
  if (!expr) {
    logError("Expected an expression after unwrap but got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
  }

  return std::make_unique<UnwrapExpression>(unwrap, std::move(expr));
}

std::unique_ptr<Expression>
Parser::parseInfixOrMethodCallExpression(std::unique_ptr<Expression> left) {
  // Peek ahead by 2
  if (peekToken(2).type == TokenType::LPAREN) {
    // Method call: p1.test(...)
    return parseMethodCallExpression(std::move(left));
  } else {
    // Regular member access: p1.health
    return parseInfixExpression(std::move(left));
  }
}

// Parsing the sizeof expression
std::unique_ptr<Expression> Parser::parseSizeOfExpression() {
  Token sizeOf = currentToken();
  advance(); // Consume the sizeOf token

  if (currentToken().type == TokenType::LESS_THAN) {
    advance(); // Consume the < token
  } else {
    logError("Expected '<' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  std::unique_ptr<Expression> type = nullptr;

  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::IDENTIFIER) {
    type = parseBasicType();
  } else {
    logError("Expected a type but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    advance();
  }

  if (currentToken().type == TokenType::GREATER_THAN) {
    advance();
  } else {
    logError("Expected '>' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  return std::make_unique<SizeOfExpression>(sizeOf, std::move(type));
}

std::unique_ptr<Expression> Parser::parseCastExpression() {
  Token cast = currentToken();
  std::unique_ptr<Expression> type = nullptr;
  std::unique_ptr<Expression> expr = nullptr;

  advance();
  if (currentToken().type != TokenType::LESS_THAN) {
    logError("Expected '<' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }
  advance(); // Consume the < token

  if (isBasicType(currentToken().type)) {
    type = parseBasicType();
  } else {
    if (currentToken().type == TokenType::IDENTIFIER) {
      logError("Expected a built in type but got '" +
                   currentToken().TokenLiteral + "'",
               currentToken());
      advance();
    } else {
      logError("Inavlid cast type '" + currentToken().TokenLiteral + "'",
               currentToken());
      advance();
    }
  }

  if (currentToken().type != TokenType::GREATER_THAN) {
    logError("Expected '>' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }
  advance(); // Consume the > token

  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected '(' but got '" + currentToken().TokenLiteral + "",
             currentToken());
    return nullptr;
  }
  advance(); // Consume the ( token

  expr = parseExpression(Precedence::PREC_NONE);
  if (currentToken().type != TokenType::RPAREN) {
    logError("Expected ')' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }
  advance(); // Consume the )

  return std::make_unique<CastExpression>(cast, std::move(type),
                                          std::move(expr));
}

std::unique_ptr<Expression> Parser::parseBitcastExpression() {
  Token bitcast = currentToken();
  std::unique_ptr<Expression> type = nullptr;
  std::unique_ptr<Expression> expr = nullptr;
  advance();
  if (currentToken().type != TokenType::LESS_THAN) {
    logError("Expected '<' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  advance();
  if (isBasicType(currentToken().type)) {
    type = parseBasicType();
  } else if (currentToken().type == TokenType::PTR) {
    type = parsePointerType();
  } else {
    logError("Invalid bitcast type '" + currentToken().TokenLiteral + "'",
             currentToken());
    advance();
  }

  if (currentToken().type != TokenType::GREATER_THAN) {
    logError("Expected '>' but got '" + currentToken().TokenLiteral + "'",
             currentToken()),
        currentToken();
    return nullptr;
  }
  advance();

  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected '(' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }
  advance();

  expr = parseExpression(Precedence::PREC_NONE);
  if (currentToken().type != TokenType::RPAREN) {
    logError("Expected  ')' but got '" + currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }
  advance(); // Consume the )

  return std::make_unique<BitcastExpression>(bitcast, std::move(type),
                                             std::move(expr));
}
