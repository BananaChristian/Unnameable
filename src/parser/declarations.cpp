#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"
#include <memory>

std::unique_ptr<Statement> Parser::parseDeclaration() {
  // Collect all modifiers
  bool isSage = false;
  bool isHeap = false;
  bool isMut = false;
  bool isConst = false;
  bool isVolatile = false;
  bool isRestrict = false;

  Token heap_token;
  std::unique_ptr<Expression> allocType = nullptr; // For heap<allocator>
  bool dynamicHeapDecl = false;

  while (true) {
    if (currentToken().type == TokenType::SAGE) {
      if (isSage || isHeap) {
        logError("Multiple storage class specifiers", currentToken());
        return nullptr;
      }
      isSage = true;
      advance();
    } else if (currentToken().type == TokenType::HEAP) {
      if (isSage || isHeap) {
        logError("Multiple storage class specifiers", currentToken());
        return nullptr;
      }
      heap_token = currentToken();

      isHeap = true;
      dynamicHeapDecl = true;

      // Check for optional allocator <Type>
      if (nextToken().type == TokenType::LESS_THAN) {
        advance(); // consume HEAP
        advance(); // consume <
        allocType = parseIdentifier();
        if (currentToken().type != TokenType::GREATER_THAN) {
          logError("Expected '>' after allocator type", currentToken());
          return nullptr;
        }
        advance(); // consume >
      } else {
        advance(); // just consume HEAP
      }
    } else if (currentToken().type == TokenType::MUT) {
      if (isMut || isConst) {
        logError("Multiple mutability specifiers", currentToken());
        return nullptr;
      }
      isMut = true;
      advance();
    } else if (currentToken().type == TokenType::CONST) {
      if (isMut || isConst) {
        logError("Multiple mutability specifiers", currentToken());
        return nullptr;
      }
      isConst = true;
      advance();
    } else if (currentToken().type == TokenType::VOLATILE) {
      isVolatile = true;
      advance();
    } else if (currentToken().type == TokenType::RESTRICT) {
      isRestrict = true;
      advance();
    } else {
      break; // No more modifiers
    }
  }

  // Determine mutability enum from collected flags
  Mutability mutability = Mutability::IMMUTABLE;
  if (isMut && isConst) {
    logError("Variable cannot be both 'mut' and 'const'", currentToken());
    return nullptr;
  }
  if (isMut)
    mutability = Mutability::MUTABLE;
  if (isConst)
    mutability = Mutability::CONSTANT;

  // Now parse the actual declaration based on what's next
  std::unique_ptr<Statement> stmt;

  if (currentToken().type == TokenType::PTR) {
    stmt = parsePointerStatement();
  } else if (currentToken().type == TokenType::REF) {
    stmt = parseReferenceStatement();
  } else if (currentToken().type == TokenType::ARRAY) {
    stmt = parseArrayStatement();
  } else if (isBasicType(currentToken().type) ||
             currentToken().type == TokenType::IDENTIFIER ||
             currentToken().type == TokenType::AUTO) {
    stmt = parseLetStatement();
  } else {
    logError("Expected declaration after modifiers", currentToken());
    return nullptr;
  }

  if (!stmt)
    return nullptr;

  // Apply collected modifiers to the statement
  if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get())) {
    letStmt->isSage = isSage;
    letStmt->isHeap = isHeap;
    letStmt->isVolatile = isVolatile;
    letStmt->isRestrict = isRestrict;
    letStmt->mutability = mutability;
  } else if (auto ptrStmt = dynamic_cast<PointerStatement *>(stmt.get())) {
    ptrStmt->isSage = isSage;
    ptrStmt->isHeap = isHeap;
    ptrStmt->isVolatile = isVolatile;
    ptrStmt->isRestrict = isRestrict;
    ptrStmt->mutability = mutability;
  } else if (auto refStmt = dynamic_cast<ReferenceStatement *>(stmt.get())) {
    refStmt->isSage = isSage;
    refStmt->isHeap = isHeap;
    refStmt->isVolatile = isVolatile;
    refStmt->isRestrict = isRestrict;
    refStmt->mutability = mutability;
  } else if (auto arrStmt = dynamic_cast<ArrayStatement *>(stmt.get())) {
    arrStmt->isSage = isSage;
    arrStmt->isHeap = isHeap;
    arrStmt->isVolatile = isVolatile;
    arrStmt->isRestrict = isRestrict;
    arrStmt->mutability = mutability;
  } else {
    logError("Cannot apply modifiers to this statement type", currentToken());
    return nullptr;
  }

  if (dynamicHeapDecl) {
    return std::make_unique<HeapStatement>(heap_token, std::move(allocType),
                                           std::move(stmt));
  }

  return stmt;
}

std::unique_ptr<Statement> Parser::parseLetStatement() {
  // Parse type
  std::unique_ptr<Expression> type;

  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::IDENTIFIER ||
      currentToken().type == TokenType::AUTO) {
    type = parseBasicType();
    if (!type) {
      logError("Type parse failed for '" + currentToken().TokenLiteral + "'",
               currentToken());
      return nullptr;
    }
  } else {
    logError("Invalid variable declaration type '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  // Parse name
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected variable name after data type but got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  Token ident_token = currentToken();
  advance();

  // Parse optional initializer
  std::optional<Token> assign_token;
  std::unique_ptr<Expression> value = nullptr;

  if (currentToken().type == TokenType::ARROW) {
    logError("Invalid assignment operator for variable declaration expected "
             "'=' but got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  if (currentToken().type == TokenType::ASSIGN) {
    assign_token = currentToken();
    advance();
    value = parseExpression(Precedence::PREC_NONE);
  }

  return std::make_unique<LetStatement>(
      false,                 // isSage
      false,                 // isHeap
      false,                 // isVolatile
      false,                 // isRestrict
      Mutability::IMMUTABLE, // mutability (default)
      std::move(type), ident_token, assign_token, std::move(value));
}

// Array statement parser
std::unique_ptr<Statement> Parser::parseArrayStatement() {
  // Parse the array type (arr[...] with basic or custom type inside)
  auto arrTypeNode = parseArrayType();
  if (!arrTypeNode)
    return nullptr;

  // Parse the dimensions (optional)
  std::vector<std::unique_ptr<Expression>> lengths;

  while (currentToken().type == TokenType::LBRACKET) {
    advance(); // consume '['

    std::unique_ptr<Expression> lengthExpr;
    lengthExpr = parseExpression(Precedence::PREC_NONE);

    if (!lengthExpr)
      return nullptr;

    if (currentToken().type != TokenType::RBRACKET) {
      logError("Expected ']' after array length but got '" +
                   currentToken().TokenLiteral + "'",
               currentToken());
      return nullptr;
    }
    advance(); // consume ']'

    lengths.push_back(std::move(lengthExpr));
  }

  // Parse array name
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected array name but got '" + currentToken().TokenLiteral +
                 "'",
             currentToken());
    return nullptr;
  }
  auto ident = parseIdentifier(); // consumes the IDENTIFIER

  // Parse optional initializer
  std::unique_ptr<Expression> items = nullptr;
  if (currentToken().type == TokenType::ASSIGN) {
    advance(); // consume '='
    items = parseExpression(Precedence::PREC_NONE);
    if (!items)
      return nullptr;
  }

  return std::make_unique<ArrayStatement>(
      false,                 // isSage
      false,                 // isHeap
      Mutability::IMMUTABLE, // mutability (default)
      false,                 // isVolatile
      false, std::move(arrTypeNode), std::move(lengths), std::move(ident),
      std::move(items));
}

// Reference statement parser
std::unique_ptr<Statement> Parser::parseReferenceStatement() {
  std::unique_ptr<Expression> type;
  Token ref_token = currentToken();
  advance(); // Consume 'ref'

  if (currentToken().type == TokenType::AUTO) {
    logError("Do not use 'auto' if u want to infer the type just dont include "
             "the type",
             currentToken());
    advance(); // Consume auto
  }

  // Parse optional type
  // Only treat as type if followed by another identifier (like "i32 x")
  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::ARRAY ||
      (currentToken().type == TokenType::IDENTIFIER &&
       nextToken().type == TokenType::IDENTIFIER)) {
    type = parseReturnType();
  }

  // Parse identifier (the referer)
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected reference name but got '" + currentToken().TokenLiteral +
                 "'",
             currentToken());
    return nullptr;
  }
  std::unique_ptr<Expression> ident = parseIdentifier();

  // Optional initializer
  std::unique_ptr<Expression> value = nullptr;
  if (currentToken().type == TokenType::ARROW) {
    advance(); // Consume ->
    value = parseExpression(Precedence::PREC_NONE);
    if (!value)
      return nullptr;
  }

  return std::make_unique<ReferenceStatement>(
      false,                 // isSage
      false,                 // isHeap
      Mutability::IMMUTABLE, // mutability (default)
      false,                 // isVolatile
      false,                 // isRestrict
      ref_token, std::move(type), std::move(ident), std::move(value));
}

// Pointer statement parser
std::unique_ptr<Statement> Parser::parsePointerStatement() {
  std::unique_ptr<Expression> type;
  Token ptr_token = currentToken();
  advance(); // Consume 'ptr'

  if (currentToken().type == TokenType::AUTO) {
    logError("Do not use 'auto' if you want to infer the type just dont include "
             "the type",
             currentToken());
    advance(); // Consume auto
  }

  // Parse optional type
  // Only treat as type if followed by another identifier (like "i32 x")
  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::ARRAY ||
      currentToken().type == TokenType::OPAQUE ||
      ((currentToken().type == TokenType::IDENTIFIER &&
        nextToken().type == TokenType::IDENTIFIER) ||
       nextToken().type == TokenType::QUESTION_MARK)) {

    type = parseReturnType();
  }

  // Parse identifier (the name)
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected pointer name but got '" + currentToken().TokenLiteral +
                 "'",
             currentToken());
    return nullptr;
  }
  std::unique_ptr<Expression> ident = parseIdentifier();

  // Optional initializer
  std::unique_ptr<Expression> value = nullptr;
  if (currentToken().type == TokenType::ARROW) {
    advance(); // Consume ->
    value = parseExpression(Precedence::PREC_NONE);
    if (!value)
      return nullptr;
  }

  return std::make_unique<PointerStatement>(
      false,                 // isSage
      false,                 // isHeap
      Mutability::IMMUTABLE, // mutability (default)
      false,                 // isVolatile
      false,                 // isRestrict
      ptr_token, std::move(type), std::move(ident), std::move(value));
}
