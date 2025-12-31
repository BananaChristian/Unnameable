#include "ast.hpp"
#include "parser.hpp"

#define CPPREST_FORCE_REBUILD

//-----------------------SWITCH STATEMENT---------------------------
// Parsing case clause
std::unique_ptr<Statement> Parser::parseCaseClause() {
  std::vector<std::unique_ptr<Statement>> body;
  Token case_token = currentToken();
  if (case_token.type != TokenType::CASE) {
    logError("Expected case keyword but got: " + currentToken().TokenLiteral);
  }

  advance();

  auto condition = parseExpression(Precedence::PREC_NONE);

  if (currentToken().type != TokenType::COLON) {
    logError("Expected : after case condition but got: " +
             currentToken().TokenLiteral);
  }

  advance();

  while (currentToken().type != TokenType::CASE &&
         currentToken().type != TokenType::DEFAULT &&
         currentToken().type != TokenType::RBRACE) {
    auto stmt = parseStatement();
    if (stmt) {
      body.push_back(std::move(stmt));
    } else {
      advance();
    }
  }

  return std::make_unique<CaseClause>(case_token, std::move(condition),
                                      std::move(body));
}

// Parsing switch statement
std::unique_ptr<Statement> Parser::parseSwitchStatement() {
  std::vector<std::unique_ptr<Statement>> case_clauses;
  std::vector<std::unique_ptr<Statement>> default_body;

  Token default_token;

  Token switch_token = currentToken();
  advance(); // Advancing past the switch keyword
  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected ( after switch keyword but got: " +
             currentToken().TokenLiteral);
  }

  advance(); // Consuming the ( token
  auto switchExpr = parseExpression(
      Precedence::PREC_NONE); // I expect whatever parses this to advance
  if (currentToken().type != TokenType::RPAREN) {
    logError("Expected ) but got: " + currentToken().TokenLiteral);
  }

  advance(); // Consume the ) token;
  if (currentToken().type != TokenType::LBRACE) {
    logError("Expected { but got: " + currentToken().TokenLiteral);
  }

  advance(); // Consume the { token;

  while (currentToken().type != TokenType::RBRACE) {
    if (currentToken().type == TokenType::CASE) {
      auto case_clause = parseCaseClause();
      case_clauses.push_back(std::move(case_clause));
    } else if (currentToken().type == TokenType::DEFAULT) {
      default_token = currentToken();
      advance(); // Consume DEFAULT
      if (currentToken().type != TokenType::COLON) {
        logError("Expected : after default keyword but got: " +
                 currentToken().TokenLiteral);
      }
      advance(); // Consume COLON
      while (currentToken().type != TokenType::CASE &&
             currentToken().type != TokenType::DEFAULT &&
             currentToken().type != TokenType::RBRACE) {
        auto stmt = parseStatement();
        if (stmt) {
          default_body.push_back(std::move(stmt));
        } else {
          advance();
        }
      }
    } else {
      logError("Unexpected token in switch statement: " +
               currentToken().TokenLiteral);
      advance();
    }
  }

  if (default_token.type != TokenType::DEFAULT) {
    logError("Missing the default case");
    return nullptr;
  }

  advance(); // Consume the } token

  return std::make_unique<SwitchStatement>(
      switch_token, std::move(switchExpr), std::move(case_clauses),
      default_token, std::move(default_body));
}

//__________________IF STATEMENT________________________
// Parsing elif statements
std::unique_ptr<Statement> Parser::parseElifStatement() {
  Token elif_stmt = currentToken();
  advance();

  if (currentToken().type != TokenType::LPAREN) {
    std::cout << "[DEBUG] Expected '(' after 'elif', got: "
              << currentToken().TokenLiteral << "\n";
    logError("Expected '(' after 'elseif'");
    return nullptr;
  }

  auto elif_condition = parseGroupedExpression();
  auto elif_result = parseBlockStatement();
  auto elifClause = std::make_unique<elifStatement>(
      elif_stmt, std::move(elif_condition), std::move(elif_result));

  return elifClause;
}

// Parsing if statements`
std::unique_ptr<Statement> Parser::parseIfStatement() {
  Token if_stmt = currentToken();
  advance();

  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected '(' after 'if' ");
    return nullptr;
  }
  advance();

  auto condition = parseExpression(Precedence::PREC_NONE);
  if (currentToken().type != TokenType::RPAREN) {
    std::cout << "[DEBUG] Expected ')' got: " << currentToken().TokenLiteral
              << "\n";
    logError("Expected ')' got: ");
    return nullptr;
  }
  advance();
  auto if_result = parseBlockStatement();

  std::vector<std::unique_ptr<Statement>> elifClauses;
  while (currentToken().type == TokenType::ELSE_IF) {
    auto elifClause = parseElifStatement();
    elifClauses.push_back(std::move(elifClause));
  }

  std::optional<Token> else_stmt;
  std::optional<std::unique_ptr<Statement>> else_result;

  if (currentToken().type == TokenType::ELSE) {
    else_stmt = currentToken();
    advance();

    else_result = parseBlockStatement();
  }

  return std::make_unique<ifStatement>(
      if_stmt, std::move(condition), std::move(if_result),
      std::move(elifClauses), std::move(else_stmt), std::move(else_result));
}
