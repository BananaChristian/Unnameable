#include "ast.hpp"
#include "parser.hpp"

// Parsing assignment statements
std::unique_ptr<Statement> Parser::parseAssignmentStatement() {
  std::unique_ptr<Expression> value = nullptr;
  Token identToken = currentToken();

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

  if (!lhs)
    return nullptr;

  return std::make_unique<AssignmentStatement>(std::move(lhs),
                                               std::move(value));
}

std::unique_ptr<Statement> Parser::parseSelfAssignment() {
  return parseAssignmentStatement();
}

std::unique_ptr<Statement> Parser::parseDereferenceAssignment() {
  auto lhs = parseExpression(Precedence::PREC_NONE);

  if (lhs && currentToken().type == TokenType::ASSIGN) {
    advance(); // Consume '='
    auto rhs = parseExpression(Precedence::PREC_NONE);
    return std::make_unique<AssignmentStatement>(std::move(lhs),
                                                 std::move(rhs));
  }

  // Fall back to the expression guy
  auto expr = parseExpression(Precedence::PREC_NONE);
  if (expr) {
    return std::make_unique<ExpressionStatement>(currentToken(),
                                                 std::move(expr));
  }
  return nullptr;
}

std::unique_ptr<Statement> Parser::parseFieldAssignment() {
  auto path = parseExpression(Precedence::PREC_NONE);
  if (currentToken().type != TokenType::ASSIGN) {
    logError("Expected '=' after field access chain");
    return nullptr;
  }
  advance();

  auto val = parseExpression(Precedence::PREC_NONE);
  return std::make_unique<FieldAssignment>(std::move(path), std::move(val));
}
