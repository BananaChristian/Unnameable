#include "parser.hpp"

std::unique_ptr<Statement> Parser::parseLetStatement() {
  bool isHeap = false;
  bool isDheap = false;
  Mutability mutability = Mutability::IMMUTABLE;
  std::unique_ptr<Expression> type;
  bool isNullable = false;

  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::IDENTIFIER ||
      currentToken().type == TokenType::AUTO) {
    type = parseBasicType();
    if (!type) {
      logError("Type parse failed for '" + currentToken().TokenLiteral + "'");
      return nullptr;
    }
  }

  if (currentToken().type == TokenType::QUESTION_MARK) {
    isNullable = true;
    advance();
  }

  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected variable name after data type but got: " +
             currentToken().TokenLiteral);
    return nullptr;
  }

  Token ident_token = currentToken();
  advance();

  std::optional<Token> assign_token;
  std::unique_ptr<Expression> value = nullptr;

  if (currentToken().type == TokenType::ASSIGN) {
    assign_token = currentToken();
    std::cout << "[DEBUG] Encountered assignment token" << "\n";
    advance();
    value = parseExpression(Precedence::PREC_NONE);
  } else if (currentToken().type == TokenType::SEMICOLON) {
    std::cout << "[DEBUG] Encountered semicolon token" << "\n";
  }

  return std::make_unique<LetStatement>(isHeap, isDheap, mutability,
                                        std::move(type), ident_token,
                                        assign_token, std::move(value));
}

std::unique_ptr<Statement> Parser::parseMutStatement() {
  advance(); // Consume the mut
  auto stmt = parseStatement();
  if (!stmt)
    return nullptr;

  if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get()))
    letStmt->mutability = Mutability::MUTABLE;
  else if (auto arrStmt = dynamic_cast<ArrayStatement *>(stmt.get()))
    arrStmt->mutability = Mutability::MUTABLE;
  else if (auto ptrStmt = dynamic_cast<PointerStatement *>(stmt.get()))
    ptrStmt->mutability = Mutability::MUTABLE;
  else if (auto refStmt = dynamic_cast<ReferenceStatement *>(stmt.get()))
    refStmt->mutability = Mutability::MUTABLE;
  else if (auto recordStmt = dynamic_cast<RecordStatement *>(stmt.get()))
    recordStmt->mutability = Mutability::MUTABLE;
  else {
    logError("Applied 'mut' to unsupported statement");
    return nullptr;
  }

  return stmt;
}

std::unique_ptr<Statement> Parser::parseConstStatement() {
  advance(); // Consume the const token
  auto stmt = parseStatement();
  if (!stmt)
    return nullptr;

  if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get()))
    letStmt->mutability = Mutability::CONSTANT;
  else if (auto arrStmt = dynamic_cast<ArrayStatement *>(stmt.get()))
    arrStmt->mutability = Mutability::CONSTANT;
  else if (auto ptrStmt = dynamic_cast<PointerStatement *>(stmt.get()))
    ptrStmt->mutability = Mutability::CONSTANT;
  else if (auto refStmt = dynamic_cast<ReferenceStatement *>(stmt.get()))
    refStmt->mutability = Mutability::CONSTANT;
  else {
    logError("Applied 'const' to unsupported statement");
    return nullptr;
  }

  return stmt;
}

// Parse heap statement
std::unique_ptr<Statement> Parser::parseHeapStatement() {
  advance(); // consume 'heap'

  if (currentToken().type == TokenType::RECORD) {
    logError("Cannot use 'heap' before a record");
    return nullptr;
  }

  auto stmt = parseStatement();
  if (!stmt)
    return nullptr;

  if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get())) {
    if (letStmt->isDheap) {
      logError("Cannot static heap raise an already dynamically heap raised "
               "variable declaration");
      return nullptr;
    }
    letStmt->isHeap = true;
  } else if (auto arrStmt = dynamic_cast<ArrayStatement *>(stmt.get())) {
    if (arrStmt->isDheap) {
      logError("Cannot static heap raise an already dynamically heap raised "
               "array declaration");
      return nullptr;
    }
    arrStmt->isHeap = true;
  } else if (auto ptrStmt = dynamic_cast<PointerStatement *>(stmt.get())) {
    if (ptrStmt->isDheap) {
      logError("Cannot static heap raise an already dynamically heap raised "
               "pointer declaration");
      return nullptr;
    }
    ptrStmt->isHeap = true;
  } else if (auto refStmt = dynamic_cast<ReferenceStatement *>(stmt.get())) {
    if (refStmt->isDheap) {
      logError("Cannot static heap raise an already dynamically heap raised "
               "reference declaration");
      return nullptr;
    }
    refStmt->isHeap = true;
  } else {
    logError("'heap' applied to non supported statement");
    return nullptr;
  }

  return stmt;
}

// Parse dheap statement
std::unique_ptr<Statement> Parser::parseDHeapStatement() {
  Token dheap_token = currentToken();
  advance();

  std::unique_ptr<Expression> allocType;

  if (currentToken().type == TokenType::LESS_THAN) {
    advance(); // Consume < token
    allocType = parseIdentifier();
    if (currentToken().type != TokenType::GREATER_THAN) {
      logError("Expected '>' but got '" + currentToken().TokenLiteral + "'");
      advance(); // Consume the wrong token
    } else {
      advance(); // Just consume the correct token
    }
  }

  std::unique_ptr<Statement> stmt = parseStatement();

  if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get())) {
    if (letStmt->isHeap) {
      logError("Cannot dynamic heap raise an already statically heap raised "
               "variable declaration");
      return nullptr;
    }
    letStmt->isDheap = true;
  } else if (auto arrStmt = dynamic_cast<ArrayStatement *>(stmt.get())) {
    if (arrStmt->isHeap) {
      logError("Cannot dynamic heap raise an already statically heap raised "
               "array declaration");
      return nullptr;
    }
    arrStmt->isDheap = true;
  } else if (auto ptrStmt = dynamic_cast<PointerStatement *>(stmt.get())) {
    if (ptrStmt->isHeap) {
      logError("Cannot dynamic heap raise an already statically heap raised "
               "pointer statement");
      return nullptr;
    }
    ptrStmt->isDheap = true;
  } else if (auto refStmt = dynamic_cast<ReferenceStatement *>(stmt.get())) {
    if (refStmt->isHeap) {
      logError("Cannot dynamic heap raise an already statically heap raised "
               "variable declaration");
      return nullptr;
    }
    refStmt->isDheap = true;
  } else {
    logError("'dheap' applied to non supported statement");
    return nullptr;
  }

  return std::make_unique<DheapStatement>(dheap_token, std::move(allocType),
                                          std::move(stmt));
}

// Array statement parser
std::unique_ptr<Statement> Parser::parseArrayStatement() {
  bool isHeap = false;
  bool isDheap = false;
  Mutability mutability = Mutability::IMMUTABLE;

  // Parse the array type (arr[...] with basic or custom type inside)
  auto arrTypeNode = parseArrayType();
  if (!arrTypeNode)
    return nullptr;

  // Parse the lengths

  // Optional single dimension [size]
  std::unique_ptr<Expression> lengthExpr = nullptr;
  std::vector<std::unique_ptr<Expression>> lengths;

  while (currentToken().type == TokenType::LBRACKET) {
    advance(); // consume '['

    std::unique_ptr<Expression> lengthExpr;
    lengthExpr = parseExpression(Precedence::PREC_NONE);

    if (!lengthExpr)
      return nullptr;

    if (currentToken().type != TokenType::RBRACKET) {
      logError("Expected ']' after array length but got '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }
    advance(); // consume ']'

    lengths.push_back(std::move(lengthExpr));
  }

  // Expect identifier
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected array name but got '" + currentToken().TokenLiteral +
             "'");
    return nullptr;
  }
  auto ident = parseIdentifier(); // this consumes the IDENTIFIER

  // Optional initializer
  std::unique_ptr<Expression> items = nullptr;
  if (currentToken().type == TokenType::ASSIGN) {
    advance(); // consume '='
    items = parseExpression(Precedence::PREC_NONE);
    if (!items)
      return nullptr;
  }

  return std::make_unique<ArrayStatement>(
      isHeap, isDheap, mutability, std::move(arrTypeNode), std::move(lengths),
      std::move(ident), std::move(items));
}

// Reference statement parser
std::unique_ptr<Statement> Parser::parseReferenceStatement() {
  bool isHeap = false;
  bool isDheap = false;
  Mutability mut = Mutability::IMMUTABLE;
  std::unique_ptr<Expression> type;

  Token ref_token = currentToken();
  advance(); // Consume 'ref'

  // Check mutability
  if (currentToken().type == TokenType::MUT) {
    mut = Mutability::MUTABLE;
    advance();
  } else if (currentToken().type == TokenType::CONST) {
    mut = Mutability::CONSTANT;
    advance();
  }

  if (currentToken().type == TokenType::AUTO) {
    logError("Do not use 'auto' if u want to infer the type just dont include "
             "the type");
    advance(); // Consume auto
  }

  // Parse optional type
  // Only treat as type if followed by another identifier (like "int x")
  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::ARRAY ||
      (currentToken().type != TokenType::IDENTIFIER &&
       nextToken().type == TokenType::IDENTIFIER)) {
    type = parseReturnType();
  }

  // Parse identifier (the referer)
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected reference name but got '" + currentToken().TokenLiteral +
             "'");
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

  return std::make_unique<ReferenceStatement>(isHeap, isDheap, ref_token, mut,
                                              std::move(type), std::move(ident),
                                              std::move(value));
}

// Pointer statement parser
std::unique_ptr<Statement> Parser::parsePointerStatement() {
  bool isHeap = false;
  bool isDheap = false;
  Mutability mut = Mutability::IMMUTABLE;
  std::unique_ptr<Expression> type;

  Token ptr_token = currentToken();
  advance(); // Consume 'ptr'

  // Check mutability
  if (currentToken().type == TokenType::MUT) {
    mut = Mutability::MUTABLE;
    advance();
  } else if (currentToken().type == TokenType::CONST) {
    mut = Mutability::CONSTANT;
    advance();
  }

  if (currentToken().type == TokenType::AUTO) {
    logError("Do not use 'auto' if u want to infer the type just dont include "
             "the type");
    advance(); // Consume auto
  }

  // Parse optional type
  // Only treat as type if followed by another identifier (like "int x")
  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::ARRAY ||
      (currentToken().type == TokenType::IDENTIFIER &&
       nextToken().type == TokenType::IDENTIFIER)) {

    type = parseReturnType();
  }

  // Parse identifier (the name)
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected pointer name but got '" + currentToken().TokenLiteral +
             "'");
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

  return std::make_unique<PointerStatement>(isHeap, isDheap, ptr_token, mut,
                                            std::move(type), std::move(ident),
                                            std::move(value));
}


