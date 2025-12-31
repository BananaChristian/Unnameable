#include "parser.hpp"

std::unique_ptr<Statement> Parser::parseLetStatementDecider() {
  Token current = currentToken();

  if (isBasicType(current.type) || current.type == TokenType::AUTO) {
    return parseLetStatementWithType(true);
  } else if (current.type == TokenType::ARRAY) {
    return parseArrayStatement(true);
  } else if (current.type == TokenType::REF) {
    return parseReferenceStatement(true);
  } else if (current.type == TokenType::PTR) {
    return parsePointerStatement(true);
  } else if (current.type == TokenType::DEREF) {
    return parseAssignmentStatement(true);
  } else if (current.type == TokenType::IDENTIFIER) {
    if (nextToken().type == TokenType::ASSIGN) {
      return parseAssignmentStatement(true);
    } else {
      return parseLetStatementWithCustomType(true);
    }
  }

  std::cerr
      << "[ERROR]: Failed to decide how to parse parameter variable. Token: "
      << current.TokenLiteral << "\n";
  return nullptr;
}

// Parsing let statements
std::unique_ptr<Statement> Parser::parseLetStatementWithType(bool isParam) {
  bool isHeap = false;
  bool isDheap = false;
  Mutability mutability = Mutability::IMMUTABLE;
  bool isNullable = false;
  Token mutability_token = currentToken();

  if (mutability_token.type == TokenType::CONST) {
    mutability = Mutability::CONSTANT;
    std::cout << "CURRENT TOKEN: " << mutability_token.TokenLiteral << "\n";
    advance();
    mutability_token = currentToken();
  } else if (mutability_token.type == TokenType::MUT) {
    mutability = Mutability::MUTABLE;
    std::cout << "CURRENT TOKEN: " << mutability_token.TokenLiteral << "\n";
    advance();
    mutability_token = currentToken();
  }

  std::unique_ptr<Expression> type = parseBasicType();

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

  if (!isParam) {
    if (currentToken().type != TokenType::SEMICOLON) {
      logError("Expected a semicolon but got '" + currentToken().TokenLiteral +
               "'");
      return nullptr;
    }
  } else {
    // If it's a function parameter, we expect either ')' or ',' next
    if (currentToken().type != TokenType::COMMA &&
        currentToken().type != TokenType::RPAREN &&
        currentToken().type != TokenType::BITWISE_OR) {
      logError("Expected ',' or ')' after parameter declaration but got: " +
               currentToken().TokenLiteral);
      return nullptr;
    }
  }

  if (mutability == Mutability::CONSTANT) {
    if (value == nullptr) {
      logError("Uninitialized const variable");
      return nullptr;
    }
  }

  return std::make_unique<LetStatement>(isHeap, isDheap, mutability,
                                        std::move(type), ident_token,
                                        assign_token, std::move(value));
}

// Parsing let statements with custom types
std::unique_ptr<Statement>
Parser::parseLetStatementWithCustomType(bool isParam) {
  std::cout << "INSIDE CUSTOM TYPE PARSER\n";
  Mutability mut = Mutability::IMMUTABLE;
  std::unique_ptr<Expression> type;
  bool isHeap = false;
  bool isDheap = false;

  // Checking for mutability
  if (currentToken().type == TokenType::MUT) {
    mut = Mutability::MUTABLE;
    advance();
  }
  if (currentToken().type == TokenType::CONST) {
    mut = Mutability::CONSTANT;
    advance();
  }

  // Checking for the indentifier token
  if (currentToken().type == TokenType::IDENTIFIER) {
    type = parseBasicType();

    if (!type) {
      // A specific error should have been logged inside parseBasicType(), but
      // returning nullptr here prevents using a broken expression object.
      logError("Type parse failed for '" + currentToken().TokenLiteral + "'");
      return nullptr;
    }
  }

  // Check for the variable name
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected variable name after data type");
    return nullptr;
  }

  Token ident_token = currentToken();
  advance(); // Consume the variable name identifier token

  // Checking if we have a value assigned
  std::optional<Token> assign_token = std::nullopt;
  std::unique_ptr<Expression> value = nullptr;

  if (currentToken().type == TokenType::ASSIGN) {
    assign_token = currentToken();
    std::cout << "[DEBUG] Encountered assignment token" << "\n";
    advance();
    value = parseExpression(Precedence::PREC_NONE);
  } else if (currentToken().type == TokenType::SEMICOLON) {
    std::cout << "[DEBUG] Encountered semicolon token" << "\n";
  }

  if (!isParam) {
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
    } else {
      logError("Expected a semicolon but got '" + currentToken().TokenLiteral +
               "'");
      return nullptr;
    }
  } else {
    // If it's a function parameter, we expect either ')' or ',' next
    if (currentToken().type != TokenType::COMMA &&
        currentToken().type != TokenType::RPAREN) {
      logError("Expected ',' or ')' after parameter declaration but got '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }
  }

  if (mut == Mutability::CONSTANT) {
    if (value == nullptr) {
      logError("Uninitialized const variable");
      return nullptr;
    }
  }

  return std::make_unique<LetStatement>(isHeap, isDheap, mut, std::move(type),
                                        ident_token, assign_token,
                                        std::move(value));
}

// Parse heap statement
std::unique_ptr<Statement> Parser::parseHeapStatement() {
  advance(); // consume 'heap'

  if (currentToken().type == TokenType::DATA) {
    logError("Cannot use 'heap' before a data block");
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
std::unique_ptr<Statement> Parser::parseArrayStatement(bool isParam) {
  bool isHeap = false;
  bool isDheap = false;
  Mutability mutability = Mutability::IMMUTABLE;
  if (currentToken().type == TokenType::MUT) {
    mutability = Mutability::MUTABLE;
    advance(); // Consume the 'mut'
  } else if (currentToken().type == TokenType::CONST) {
    mutability = Mutability::CONSTANT;
    advance(); // Consume the const
  }

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

  // Final semicolon (only for non-parameter mode)
  if (!isParam) {
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
    } else {
      logError("Expected a semicolon but got: " + currentToken().TokenLiteral);
      return nullptr;
    }
  } else {
    // If it's a function parameter, we expect either ')' or ',' next
    if (currentToken().type != TokenType::COMMA &&
        currentToken().type != TokenType::RPAREN) {
      logError("Expected ',' or ')' after parameter declaration but got: " +
               currentToken().TokenLiteral);
      return nullptr;
    }
  }

  return std::make_unique<ArrayStatement>(
      isHeap, isDheap, mutability, std::move(arrTypeNode), std::move(lengths),
      std::move(ident), std::move(items));
}

// Reference statement parser
std::unique_ptr<Statement> Parser::parseReferenceStatement(bool isParam) {
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
    advance(); // Consume =>
    value = parseExpression(Precedence::PREC_NONE);
    if (!value)
      return nullptr;
  }

  // Final semicolon (only for non-parameter mode)
  if (!isParam) {
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
    } else {
      logError("Expected a semicolon but got: " + currentToken().TokenLiteral);
      return nullptr;
    }
  } else {
    // If it's a function parameter, we expect either ')' or ',' next
    if (currentToken().type != TokenType::COMMA &&
        currentToken().type != TokenType::RPAREN) {
      logError("Expected ',' or ')' after parameter declaration but got: " +
               currentToken().TokenLiteral);
      return nullptr;
    }
  }
  return std::make_unique<ReferenceStatement>(isHeap, isDheap, ref_token, mut,
                                              std::move(type), std::move(ident),
                                              std::move(value));
}

// Pointer statement parser
std::unique_ptr<Statement> Parser::parsePointerStatement(bool isParam) {
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
      (currentToken().type != TokenType::IDENTIFIER &&
       nextToken().type == TokenType::IDENTIFIER)) {
    type = parseReturnType();
  }

  // Parse identifier (the referer)
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected pointer name but got '" + currentToken().TokenLiteral +
             "'");
    return nullptr;
  }
  std::unique_ptr<Expression> ident = parseIdentifier();

  // Optional initializer
  std::unique_ptr<Expression> value = nullptr;
  if (currentToken().type == TokenType::ARROW) {
    advance(); // Consume =>
    value = parseExpression(Precedence::PREC_NONE);
    if (!value)
      return nullptr;
  }

  // Final semicolon (only for non-parameter mode)
  if (!isParam) {
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
    } else {
      logError("Expected a semicolon but got: " + currentToken().TokenLiteral);
      return nullptr;
    }
  } else {
    // If it's a function parameter, we expect either ')' or ',' next
    if (currentToken().type != TokenType::COMMA &&
        currentToken().type != TokenType::RPAREN) {
      logError("Expected ',' or ')' after parameter declaration but got: " +
               currentToken().TokenLiteral);
      return nullptr;
    }
  }
  return std::make_unique<PointerStatement>(isHeap, isDheap, ptr_token, mut,
                                            std::move(type), std::move(ident),
                                            std::move(value));
}

// Wrappers for the dispatcher
std::unique_ptr<Statement> Parser::parseArrayStatementWrapper() {
  return parseArrayStatement();
}

std::unique_ptr<Statement> Parser::parsePointerStatementWrapper() {
  return parsePointerStatement();
}

std::unique_ptr<Statement> Parser::parseReferenceStatementWrapper() {
  return parseReferenceStatement();
}

// Decider for custom or basic let statements
std::unique_ptr<Statement> Parser::parseLetStatementCustomOrBasic() {
  if (currentToken().type == TokenType::IDENTIFIER) {
    return parseLetStatementWithCustomType();
  }
  return parseLetStatementWithType();
}

// Wrapper function for let statement with basic or custom type
std::unique_ptr<Statement> Parser::parseLetStatementWithTypeWrapper() {
  if (nextToken().type == TokenType::DATA) {
    return parseDataStatement();
  } else if (nextToken().type == TokenType::ARRAY) {
    return parseArrayStatementWrapper();
  }
  return parseLetStatementCustomOrBasic();
}
