#include "parser.hpp"

// Parsing while statements
std::unique_ptr<Statement> Parser::parseWhileStatement() {
  Token while_key = currentToken();
  advance();
  if (currentToken().type != TokenType::LPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),{"')'",currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  advance();
  auto condition = parseExpression(Precedence::PREC_NONE);
  if (currentToken().type != TokenType::RPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),{"')'",currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  advance();
  auto result = parseBlockStatement();
  return std::make_unique<WhileStatement>(while_key, std::move(condition),
                                          std::move(result));
}

// Parse for loops
std::unique_ptr<Statement> Parser::parseForStatement() {
  Token forToken = currentToken();
  advance(); // consume 'for'

  if (currentToken().type != TokenType::LPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),{"'('",currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  advance(); // consume '('

  // Parse initializer (mut i32 i = 0)
  auto initializer = parseStatement();
  advance();

  // Parse condition()
  auto condition = parseExpression(Precedence::PREC_NONE);

  if (currentToken().type != TokenType::SEMICOLON) {
    logError(ErrorCode::UnexpectedToken,
             currentToken(),{"';'",currentToken().TokenLiteral});
    return nullptr;
  }
  advance();

  // Parse step statement
  auto step = parseStatement();

  if (currentToken().type != TokenType::RPAREN) {
    logError(ErrorCode::UnexpectedToken,
             currentToken(),{"')'",currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  advance(); // consume ')'

  if (currentToken().type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),{"'{'",currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }

  auto block = parseBlockStatement();

  return std::make_unique<ForStatement>(forToken, std::move(initializer),
                                        std::move(condition), std::move(step),
                                        std::move(block));
}

// Parse break statement
std::unique_ptr<Statement> Parser::parseBreakStatement() {
  Token break_tok = currentToken();
  advance();
  return std::make_unique<BreakStatement>(break_tok);
}

// Parse continue statement
std::unique_ptr<Statement> Parser::parseContinueStatement() {
  Token cont_tok = currentToken();
  advance();
  return std::make_unique<ContinueStatement>(cont_tok);
}
