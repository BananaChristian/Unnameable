#include "ast.hpp"
#include "parser.hpp"

// Null literal parse function
std::unique_ptr<Expression> Parser::parseNullLiteral() {
  auto ident = std::make_unique<NullLiteral>(currentToken());
  advance();
  return ident;
}

// 8 bit signed integer literal parse function
std::unique_ptr<Expression> Parser::parseI8Literal() {
  auto ident = std::make_unique<I8Literal>(currentToken());
  advance();
  return ident;
}

// 8 bit unsigned integer literal parse function
std::unique_ptr<Expression> Parser::parseU8Literal() {
  auto ident = std::make_unique<U8Literal>(currentToken());
  advance();
  return ident;
}

// 16 bit signed integer literal parse function
std::unique_ptr<Expression> Parser::parseI16Literal() {
  auto ident = std::make_unique<I16Literal>(currentToken());
  advance();
  return ident;
}

// 16 bit unsigned integer literal parse function
std::unique_ptr<Expression> Parser::parseU16Literal() {
  auto ident = std::make_unique<U16Literal>(currentToken());
  advance();
  return ident;
}

// 32 bit signed Integer literal parse function
std::unique_ptr<Expression> Parser::parseI32Literal() {
  auto ident = std::make_unique<I32Literal>(currentToken());
  advance();
  return ident;
}

// 32 bit unsigned Integer literal parse function
std::unique_ptr<Expression> Parser::parseU32Literal() {
  auto ident = std::make_unique<U32Literal>(currentToken());
  advance();
  return ident;
}

// Signed 64 bit integer parse function
std::unique_ptr<Expression> Parser::parseI64Literal() {
  auto ident = std::make_unique<I64Literal>(currentToken());
  advance();
  return ident;
}

// UnSigned 64 bit integer parse function
std::unique_ptr<Expression> Parser::parseU64Literal() {
  auto ident = std::make_unique<U64Literal>(currentToken());
  advance();
  return ident;
}

// Signed 128 bit integer parse function
std::unique_ptr<Expression> Parser::parseI128Literal() {
  auto ident = std::make_unique<I128Literal>(currentToken());
  advance();
  return ident;
}

// UnSigned 128 bit integer parse function
std::unique_ptr<Expression> Parser::parseU128Literal() {
  auto ident = std::make_unique<U128Literal>(currentToken());
  advance();
  return ident;
}

// Signed CPU Native width bit integer parse function
std::unique_ptr<Expression> Parser::parseISIZELiteral() {
  auto ident = std::make_unique<ISIZELiteral>(currentToken());
  advance();
  return ident;
}

// Unsigned CPU Native width bit integer parse function
std::unique_ptr<Expression> Parser::parseUSIZELiteral() {
  auto ident = std::make_unique<USIZELiteral>(currentToken());
  advance();
  return ident;
}

// Generic integer literal
std::unique_ptr<Expression> Parser::parseINTLiteral() {
  auto ident = std::make_unique<INTLiteral>(currentToken());
  advance();
  return ident;
}

// Boolean literal parse function
std::unique_ptr<Expression> Parser::parseBooleanLiteral() {
  Token bool_tok = currentToken();
  advance();
  return std::make_unique<BooleanLiteral>(bool_tok);
}

// Float literal parse function
std::unique_ptr<Expression> Parser::parseF32Literal() {
  Token f32_tok = currentToken();
  advance();
  return std::make_unique<F32Literal>(f32_tok);
}

// Double literal parse function
std::unique_ptr<Expression> Parser::parseF64Literal() {
  Token f64_tok = currentToken();
  advance();
  return std::make_unique<F64Literal>(f64_tok);
}

// Generic float parse function
std::unique_ptr<Expression> Parser::parseFloatLiteral() {
  Token float_tok = currentToken();
  advance();
  return std::make_unique<FloatLiteral>(float_tok);
}

// 8 bit Char literal parse function
std::unique_ptr<Expression> Parser::parseChar8Literal() {
  Token char8_tok = currentToken();
  advance();
  return std::make_unique<Char8Literal>(char8_tok);
}

// 16 bit Char literal parser function
std::unique_ptr<Expression> Parser::parseChar16Literal() {
  Token char16_token = currentToken();
  advance();
  return std::make_unique<Char16Literal>(char16_token);
}

// 32 bit Char literal parser function
std::unique_ptr<Expression> Parser::parseChar32Literal() {
  Token char16_token = currentToken();
  advance();
  return std::make_unique<Char32Literal>(char16_token);
}

// String literal parse function
std::unique_ptr<Expression> Parser::parseStringLiteral() {
  Token string_tok = currentToken();
  advance();
  return std::make_unique<StringLiteral>(string_tok);
}

std::unique_ptr<Expression> Parser::parseFStringLiteral() {
    Token f_token = currentToken();
    std::string content = f_token.TokenLiteral;
    advance(); 

    std::vector<FStringSegment> segments;
    size_t pos = 0;

    while (pos < content.length()) {
        size_t nextBrace = content.find_first_of("{}", pos);

        // No more braces capture remaining text
        if (nextBrace == std::string::npos) {
            std::string trailing = content.substr(pos);
            if (!trailing.empty()) {
                segments.push_back({std::make_unique<StringLiteral>(
                    Token{trailing, TokenType::STRING, f_token.line, f_token.column}), {}
                });
            }
            break;
        }

        // Handle escaped braces like {{ or }}
        if (nextBrace + 1 < content.length() && content[nextBrace] == content[nextBrace + 1]) {
            // It's an escape! We treat the first brace and everything before it as a StringLiteral
            std::string literalPart = content.substr(pos, nextBrace - pos + 1); 
            segments.push_back({std::make_unique<StringLiteral>(
                Token{literalPart, TokenType::STRING, f_token.line, f_token.column}), {}
            });
            pos = nextBrace + 2; // Skip both braces
            continue;
        }

        // Handle a real hole {expression}
        if (content[nextBrace] == '{') {
            FStringSegment currentSeg;
            
            // Capture text leading up to the hole
            std::string leadingText = content.substr(pos, nextBrace - pos);
            currentSeg.string_part = std::make_unique<StringLiteral>(
                Token{leadingText, TokenType::STRING, f_token.line, f_token.column}
            );

            size_t endBrace = content.find('}', nextBrace);
            if (endBrace != std::string::npos) {
                std::string exprCode = content.substr(nextBrace + 1, endBrace - nextBrace - 1);
                
                if (!exprCode.empty()) {
                    currentSeg.values.push_back(std::make_unique<Identifier>(
                        Token{exprCode, TokenType::IDENTIFIER, f_token.line, f_token.column}
                    ));
                }
                segments.push_back(std::move(currentSeg));
                pos = endBrace + 1;
            } else {
                logError("Missing '}' in f-string", f_token);
                break;
            }
        } else {
            // Handle stray '}' (Treat as literal text)
            segments.push_back({std::make_unique<StringLiteral>(
                Token{content.substr(pos, nextBrace - pos + 1), TokenType::STRING, f_token.line, f_token.column}), {}
            });
            pos = nextBrace + 1;
        }
    }

    return std::make_unique<FStringLiteral>(f_token, std::move(segments));
}

// Array literal parse function
std::unique_ptr<Expression> Parser::parseArrayLiteral() {
  Token arr_tok = currentToken();
  advance(); // consume '['

  std::vector<std::unique_ptr<Expression>> array;

  while (currentToken().type != TokenType::RBRACKET &&
         currentToken().type != TokenType::END) {
    // Parse element
    auto expr = parseExpression(Precedence::PREC_NONE);
    array.push_back(std::move(expr));

    if (currentToken().type == TokenType::COMMA) {
      advance(); // consume ',' and continue
    } else if (currentToken().type != TokenType::RBRACKET) {
      logError("Unexpected token in array literal: " +
                   currentToken().TokenLiteral,
               currentToken());
      return nullptr;
    }
  }

  if (currentToken().type != TokenType::RBRACKET) {
    logError("Expected ']' to close array literal", currentToken());
    return nullptr;
  }
  advance(); // consume ']'

  return std::make_unique<ArrayLiteral>(arr_tok, std::move(array));
}
