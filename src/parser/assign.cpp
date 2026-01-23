#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"

// Parsing assignment statements
std::unique_ptr<Statement> Parser::parseAssignmentStatement() {
  std::unique_ptr<Expression> value = nullptr;
  Token identToken = currentToken();

  // self.field = ...
  std::unique_ptr<Expression> lhs;
  lhs = parseExpression(Precedence::PREC_NONE);

  auto assignToken = currentToken();
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

  // Expect '=' or '->' before parsing the value
  if ((assignToken.type != TokenType::ASSIGN) &&
      (assignToken.type != TokenType::ARROW)) {
    logError("Expected '=' or '->' after identifier in assignment but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // consume '=' or '->'

  // Parse right-hand side expression
  value = parseExpression(Precedence::PREC_NONE);

  if (!lhs)
    return nullptr;

  return std::make_unique<AssignmentStatement>(std::move(lhs), assignToken,
                                               std::move(value));
}

std::unique_ptr<Statement> Parser::parseSelfAssignment() {
  return parseAssignmentStatement();
}

std::unique_ptr<Statement> Parser::parseDereferenceAssignment() {
  auto lhs = parseExpression(Precedence::PREC_NONE);

  auto assign_token = currentToken();
  bool validAssign = ((assign_token.type == TokenType::ASSIGN) ||
                      (assign_token.type == TokenType::ARROW));

  if (lhs && validAssign) {
    advance(); // Consume '=' or '->'
    auto rhs = parseExpression(Precedence::PREC_NONE);
    return std::make_unique<AssignmentStatement>(std::move(lhs), assign_token,
                                                 std::move(rhs));
  }

  // Fall back to the expression guy
  if (lhs) {
    return std::make_unique<ExpressionStatement>(currentToken(),
                                                 std::move(lhs));
  }
  return nullptr;
}

std::unique_ptr<Statement> Parser::parseFieldAssignment() {
  auto path = parseExpression(Precedence::PREC_NONE);

  auto assign_token = currentToken();
  if ((assign_token.type != TokenType::ASSIGN) &&
      (assign_token.type != TokenType::ARROW)) {
    logError("Expected '=' after field access chain");
    return nullptr;
  }
  advance();

  auto val = parseExpression(Precedence::PREC_NONE);
  return std::make_unique<FieldAssignment>(std::move(path), assign_token,
                                           std::move(val));
}
