#include "parser.hpp"
#include "token.hpp"

std::unique_ptr<Expression> Parser::parseTypeModifier() {
  Token type_modifier = currentToken();
  std::unique_ptr<Expression> inner_modifier;
  std::vector<std::unique_ptr<Expression>> dims;
  if (type_modifier.type == TokenType::PTR ||
      type_modifier.type == TokenType::REF ||
      type_modifier.type == TokenType::ARRAY) {
    advance(); // Consume the type modifier
    // Man handle arrays
    if (type_modifier.type == TokenType::ARRAY) {
      while (currentToken().type == TokenType::LBRACKET) {
        advance();
        std::unique_ptr<Expression> lengthExpr;
        lengthExpr = parseExpression(Precedence::PREC_NONE);
        if (!lengthExpr) {
          reportDevBug("Failed to parse length expression", currentToken());
          return nullptr;
        }

        if (currentToken().type != TokenType::RBRACKET) {
          logError("Expected ']' after array length but got '" +
                       currentToken().TokenLiteral + "'",
                   currentToken());
          return nullptr;
        }
        advance(); // consume ']'

        dims.push_back(std::move(lengthExpr));
      }
    }
    if (currentToken().type == TokenType::LESS_THAN) {
      advance(); // consume '<'
      inner_modifier = parseTypeModifier();
      if (currentToken().type == TokenType::GREATER_THAN) {
        advance(); // normal case, consume '>'
      } else if (currentToken().type == TokenType::SHIFT_RIGHT) {
        // This is to combat the >> issue create a false > to replace the >>
        // from the lexer
        Token gt_token;
        gt_token.TokenLiteral = ">";
        gt_token.type = TokenType::GREATER_THAN;
        replaceCurrentToken(gt_token);
      } else {
        logError("Expected '>' to close the type modifier grouping but got '" +
                     currentToken().TokenLiteral + "'",
                 currentToken());
        return nullptr;
      }
    }
  }
  bool isArray = type_modifier.type == TokenType::ARRAY;
  bool isPointer = type_modifier.type == TokenType::PTR;
  bool isReference = type_modifier.type == TokenType::REF;

  auto typeExpr =
      std::make_unique<TypeModifier>(isArray, isPointer, isReference);
  typeExpr->dimensions = std::move(dims);
  typeExpr->inner_modifier = std::move(inner_modifier);

  return typeExpr;
}

// Parsing basic type
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

std::unique_ptr<Expression> Parser::parseReturnType() {
  // Void case
  if (currentToken().type == TokenType::VOID) {
    advance();
    return std::make_unique<ReturnType>(); // isVoid constructor
  }

  // Parse modifier if present
  std::unique_ptr<Expression> modifier = nullptr;
  if (currentToken().type == TokenType::PTR ||
      currentToken().type == TokenType::REF ||
      currentToken().type == TokenType::ARRAY) {
    modifier = parseTypeModifier();
  }

  // Parse base type
  if (!isBasicType(currentToken().type) &&
      currentToken().type != TokenType::IDENTIFIER &&
      currentToken().type != TokenType::OPAQUE) {
    logError("Expected base type in return type but got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  auto base = parseBasicType();
  if (!base) {
    logError("Failed to parse base return type", currentToken());
    return nullptr;
  }

  return std::make_unique<ReturnType>(std::move(modifier), std::move(base));
}
