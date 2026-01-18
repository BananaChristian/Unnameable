#include "parser.hpp"

// Parsing basic return type
std::unique_ptr<Expression> Parser::parseBasicType() {
  Token data_token;
  bool isNullable = false;

  data_token = currentToken();
  advance();
  if (currentToken().type == TokenType::QUESTION_MARK) {
    isNullable = true;
    advance(); // Consume the ? if it exists
  }
  return std::make_unique<BasicType>(data_token, isNullable);
}

// Parsing pointer return type
std::unique_ptr<Expression> Parser::parsePointerType() {
  Token ptr_token = currentToken();
  advance(); // Consume the ptr token
  std::unique_ptr<Expression> type;
  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::IDENTIFIER) {
    type = parseBasicType();
  } else if (currentToken().type == TokenType::ARRAY) {
    type = parseArrayType();
  } else {
    logError("Expected basic or array type for pointers ");
  }

  return std::make_unique<PointerType>(ptr_token, std::move(type));
}

// Parsing the ref type
std::unique_ptr<Expression> Parser::parseRefType() {
  Token ref_token = currentToken();
  advance(); // Consume the ref token

  std::unique_ptr<Expression> type;
  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::IDENTIFIER) {
    type = parseBasicType();
  } else if (currentToken().type == TokenType::ARRAY) {
    type = parseArrayType();
  } else {
    logError("Expected basic or array return for references");
  }

  return std::make_unique<RefType>(ref_token, std::move(type));
}

//Array type
std::unique_ptr<Expression> Parser::parseArrayType() {
  // Check if we have an 'arr' keyword
  if (currentToken().type == TokenType::ARRAY) {
    Token arr_token = currentToken();
    advance(); // consume 'arr'

    // Expect '['
    if (currentToken().type != TokenType::LBRACKET) {
      logError("Expected '[' after 'arr' keyword but got '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }
    advance(); // consume '['

    // Parse **only a basic type or identifier** as inner type
    std::unique_ptr<Expression> innerType;
    if (isBasicType(currentToken().type) ||
        currentToken().type == TokenType::IDENTIFIER) {
      innerType = parseBasicType();
    } else {
      logError("Arrays can only contain a basic type or custom type, got '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }

    // Check for nullability '?'
    bool isNullable = false;
    if (currentToken().type == TokenType::QUESTION_MARK) {
      isNullable = true;
      advance(); // consume '?'
    }

    // Expect ']'
    if (currentToken().type != TokenType::RBRACKET) {
      logError("Expected ']' to close array type but got '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }
    advance(); // consume ']'

    // Return a node representing this array type
    return std::make_unique<ArrayType>(arr_token, std::move(innerType),
                                       isNullable);
  } else if (isBasicType(currentToken().type) ||
             currentToken().type == TokenType::IDENTIFIER) {
    // Simple basic type
    return parseBasicType();
  }

  logError("Expected array type or basic type but got '" +
           currentToken().TokenLiteral + "'");
  return nullptr;
}

// Parsing the return type expression
std::unique_ptr<Expression> Parser::parseReturnType() {
  std::unique_ptr<Expression> expr = nullptr;
  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::IDENTIFIER ||
      currentToken().type == TokenType::VOID) {
    expr = parseBasicType();
  } else if (currentToken().type == TokenType::ARRAY) {

    expr = parseArrayType();
  } else if (currentToken().type == TokenType::PTR) {
    expr = parsePointerType();
  } else if (currentToken().type == TokenType::REF) {
    expr = parseRefType();
  } else {
    logError("Expected basic or array return type ");
  }

  return std::make_unique<ReturnType>(std::move(expr));
}
