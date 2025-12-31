#include "parser.hpp"

// Parsing assignment statements
std::unique_ptr<Statement> Parser::parseAssignmentStatement(bool isParam) {
  std::cout << "NOW INSIDE ASSIGNMENT STATEMENT PARSER\n";
  std::unique_ptr<Expression> value = nullptr;
  Token identToken = currentToken();
  bool isQualified = false;

  // self.field = ...
  std::unique_ptr<Expression> lhs;
  lhs = parseExpression(Precedence::PREC_NONE);

  if (!lhs) {
    logError("Invalid left-hand side in assignment");
    return nullptr;
  }

  if (!(dynamic_cast<Identifier *>(lhs.get()) ||
        dynamic_cast<ArraySubscript *>(lhs.get()) ||
        dynamic_cast<SelfExpression *>(lhs.get()) ||
        dynamic_cast<DereferenceExpression *>(lhs.get()))) {
    logError("Left-hand side of assignment is not assignable");
    return nullptr;
  }

  // Expect '=' before parsing the value
  if (currentToken().type != TokenType::ASSIGN) {
    logError("Expected '=' after identifier in assignment but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // consume '='

  // Parse right-hand side expression
  value = parseExpression(Precedence::PREC_NONE);

  // Consume optional semicolon
  if (!isParam && currentToken().type == TokenType::SEMICOLON) {
    advance();
  } else if (!isParam) {
    logError("Expected ';' after assignment but got '" +
             currentToken().TokenLiteral + "'");
  }

  if (!lhs)
    return nullptr;

  std::cout << "[LEAVING ASSIGNMENT] Current token: "
            << currentToken().TokenLiteral << " (" << (int)currentToken().type
            << ")\n";

  return std::make_unique<AssignmentStatement>(std::move(lhs),
                                               std::move(value));
}

std::unique_ptr<Statement> Parser::parseSelfAssignment() {
  return parseAssignmentStatement();
}

std::unique_ptr<Statement> Parser::parseDereferenceAssignment() {
  auto isDerefAssignment = [&]() -> bool {
    int i = 0;
    // Skip all leading 'deref' tokens
    while (peekToken(i).type == TokenType::DEREF) {
      i++;
    }
    // Skip the identifier
    if (peekToken(i).type == TokenType::IDENTIFIER) {
      i++;
    }
    // Is the next thing an assignment?
    return peekToken(i).type == TokenType::ASSIGN;
  };

  if (isDerefAssignment()) {
    std::cout << "Deep Dereference assignment triggered\n";
    return parseAssignmentStatement();
  }

  // Fall back to the expression guy
  auto expr = parseExpression(Precedence::PREC_NONE);
  if (expr) {
    if (currentToken().type == TokenType::SEMICOLON)
      advance();
    else
      logError("Expected ';' after expression statement but got '" +
               currentToken().TokenLiteral + "'");

    return std::make_unique<ExpressionStatement>(currentToken(),
                                                 std::move(expr));
  }
  return nullptr;
}

std::unique_ptr<Statement> Parser::parseFieldAssignment() {
  std::unique_ptr<Expression> value = nullptr;
  Token current = currentToken();
  advance(); // Consume for example x
  if (currentToken().type == TokenType::SCOPE_OPERATOR ||
      currentToken().type == TokenType::FULLSTOP) {
    std::cout << "SPECIAL IDENTIFIER CASE TRIGGERED\n";
    std::string fieldName = current.TokenLiteral;
    std::string operatorLiteral =
        (currentToken().type == TokenType::SCOPE_OPERATOR) ? "::" : ".";
    fieldName += operatorLiteral;

    advance(); // consume scope or dot operator

    if (currentToken().type != TokenType::IDENTIFIER) {
      logError("Expected an identifier after '" + operatorLiteral +
               "' but got '" + currentToken().TokenLiteral + "'");
      return nullptr;
    }

    fieldName += currentToken().TokenLiteral;
    current.TokenLiteral = fieldName;

    advance(); // consume second identifier
  }

  if (currentToken().type != TokenType::ASSIGN) {
    logError("Expected '=' after identifier in assignment but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // consume '='

  // Parse right-hand side expression
  value = parseExpression(Precedence::PREC_NONE);

  return std::make_unique<FieldAssignment>(current, std::move(value));
}
