#include "parser.hpp"
#include "token.hpp"
#include <memory>

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
          return nullptr;
        }

        if (currentToken().type != TokenType::RBRACKET) {
          logError(ErrorCode::UnexpectedToken, currentToken(),
                   {"']'", currentToken().TokenLiteral});
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
        logError(ErrorCode::UnexpectedToken, currentToken(),
                 {"'>'", currentToken().TokenLiteral});
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
  Token type_token = currentToken();

  if (currentToken().type != TokenType::IDENTIFIER) {
    advance();
    return std::make_unique<BasicType>(type_token, nullptr, false);
  }

  std::unique_ptr<Expression> type = parseIdentifier(); // Consumes 'I32'

  // For the outer type like saying std::string
  if (currentToken().type == TokenType::SCOPE_OPERATOR) {
    Token scope = currentToken();
    advance(); // Consume
    auto inner = parseIdentifier();
    type = std::make_unique<InfixExpression>(std::move(type), scope,
                                             std::move(inner));
  }

  // Manually handle the '@' safely without letting Pratt hijack it or
  // overshoot!
  if (currentToken().type == TokenType::AT) {
    Token at_tok = currentToken();
    advance(); // Consume '@'

    auto modifier = parseIdentifier(); // Consumes 'Array'
    type = std::make_unique<ComponentAccess>(std::move(type), at_tok,
                                             std::move(modifier));
  }

  bool isNullable = false;
  if (currentToken().type == TokenType::QUESTION_MARK) {
    isNullable = true;
    advance();
  }

  return std::make_unique<BasicType>(type_token, std::move(type), isNullable);
}

std::unique_ptr<Expression> Parser::parseReturnType() {
  // Void case
  if (currentToken().type == TokenType::VOID) {
    advance();
    return std::make_unique<ReturnType>(); // isVoid constructor
  }

  // Parse func modifier if present
  std::unique_ptr<Expression> fnptr_mod;
  if (currentToken().type == TokenType::FN) {
    fnptr_mod = parseFunctionPointerModifier();
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
    logError(ErrorCode::InvalidType, currentToken(),
             {currentToken().TokenLiteral});
    advance();
    return nullptr;
  }

  auto base = parseBasicType();

  return std::make_unique<ReturnType>(std::move(fnptr_mod), std::move(modifier),
                                      std::move(base));
}
