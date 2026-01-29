#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"

//_____________Allocator statement_________________
std::unique_ptr<Statement> Parser::parseAllocatorStatement() {
  bool isExportable = false;
  Token allocator_token = currentToken();
  advance();

  std::unique_ptr<Expression> allocator_name = parseIdentifier();

  std::unique_ptr<Statement> block = parseBlockStatement();

  return std::make_unique<AllocatorStatement>(isExportable, allocator_token,
                                              std::move(allocator_name),
                                              std::move(block));
}

//___________Seal statement_________________
std::unique_ptr<Statement> Parser::parseSealStatement() {
  bool isExportable = false;
  Token seal_token = currentToken();
  advance();

  std::unique_ptr<Expression> sealIdent = parseIdentifier();

  if (currentToken().type != TokenType::LBRACE) {
    logError("Expected { but got '" + currentToken().TokenLiteral + "'");
    return nullptr;
  }

  std::unique_ptr<Statement> block = parseBlockStatement();

  return std::make_unique<SealStatement>(
      isExportable, seal_token, std::move(sealIdent), std::move(block));
}

//______________GENERICS ________________
std::unique_ptr<Statement> Parser::parseGenericStatement() {
  Token generic_token = currentToken();
  advance(); // consume 'generic'

  // name
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected identifier for generic name but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  auto genericIdent = parseIdentifier();

  // type param list
  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected '(' after generic name but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // consume '('

  std::vector<Token> types;

  while (currentToken().type != TokenType::RPAREN &&
         currentToken().type != TokenType::END) {
    if (currentToken().type == TokenType::IDENTIFIER) {
      types.push_back(currentToken());
      advance();
    } else if (currentToken().type == TokenType::COMMA) {
      advance(); // consume comma
    } else {
      logError("Unexpected token in generic parameters: '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }
  }

  if (currentToken().type != TokenType::RPAREN) {
    logError("Expected ')' to close generic parameter list");
    return nullptr;
  }
  advance(); // consume ')'

  if (currentToken().type != TokenType::LBRACE) {
    logError("Expected '{' to start generic block but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }

  auto block = parseBlockStatement();

  return std::make_unique<GenericStatement>(
      generic_token, std::move(genericIdent), types, std::move(block));
}

std::unique_ptr<Statement> Parser::parseInstantiateStatement() {
  Token instantiate = currentToken();
  advance();

  // Dealing with the generic call
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected 'identifier' for  generic call but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }

  auto ident = parseIdentifier();
  // Expect '('
  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected '(' after instantiate identifier");
    return nullptr;
  }
  advance(); // consume '('

  std::vector<Token> types;

  // Parse argument list
  while (currentToken().type != TokenType::RPAREN &&
         currentToken().type != TokenType::END) {
    // Must be a type token (basic OR custom)
    if (currentToken().type == TokenType::IDENTIFIER ||
        isBasicType(currentToken().type)) {
      types.push_back(currentToken());
      advance();
    } else {
      logError("Unexpected token in generic call: " +
               currentToken().TokenLiteral);
      return nullptr;
    }

    // If comma → eat it and continue
    if (currentToken().type == TokenType::COMMA) {
      advance();
      continue;
    }

    // If not comma, next MUST be ')'
    if (currentToken().type != TokenType::RPAREN) {
      logError("Expected ',' or ')' but got '" + currentToken().TokenLiteral +
               "'");
      return nullptr;
    }
  }

  auto generic_call = std::make_unique<GenericCall>(std::move(ident), types);

  advance();

  if (currentToken().type != TokenType::AS) {
    logError("Expected 'as' but got '" + currentToken().TokenLiteral + "'");
    return nullptr;
  }
  Token as = currentToken();
  advance();

  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected 'identifier' but got '" + currentToken().TokenLiteral +
             "'");
  }
  Token alias = currentToken();
  advance();

  return std::make_unique<InstantiateStatement>(
      instantiate, std::move(generic_call), as, alias);
}

//__________________________ENUMS__________________________
// Parsing enum member node
std::unique_ptr<EnumMember> Parser::parseEnumMember() {
  // Check if the current token is an identifier
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Enum member must start with an identifier, got '" +
             currentToken().TokenLiteral + "'");
    return nullptr; // Return nullptr but don't advance to allow recovery in
                    // caller
  }
  Token memberToken = currentToken();

  std::string name = currentToken().TokenLiteral;
  advance(); // Consume the identifier
  std::unique_ptr<Expression> value = nullptr;

  // Handle optional value assignment
  if (currentToken().type == TokenType::ASSIGN) {
    advance(); // Consume the assignment token
    if (!isIntegerLiteralType(currentToken().type)) {
      logError("Enum member value must be an integer, got '" +
               currentToken().TokenLiteral + "'");
      return nullptr; // Return nullptr without advancing
    }
    // Parse the expression for the integer value
    value = parseExpression(Precedence::PREC_NONE);
    if (!value) {
      logError("Failed to parse enum member value");
      return nullptr;
    }
    // No need for manual advance here; parseExpression should handle token
    // consumption
  }

  return std::make_unique<EnumMember>(memberToken, name, std::move(value));
}

std::unique_ptr<Statement> Parser::parseEnumStatement() {
  bool isExportable = false;
  std::vector<std::unique_ptr<EnumMember>> enum_block;
  std::optional<Token> int_token;
  Token enum_token = currentToken();
  advance(); // Consume the enum keyword token

  // Parse the enum class's name
  auto enum_ident = parseIdentifier();
  if (!enum_ident) {
    logError("Expected identifier for enum class name, got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }

  // Handle optional underlying type
  if (currentToken().type == TokenType::COLON) {
    advance(); // Consume the colon token
    if (!isIntegerType(currentToken().type)) {
      logError("Expected integer type after colon in enum class, got '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }
    int_token = currentToken();
    advance(); // Consume the integer type token
  }

  // Check for opening brace
  if (currentToken().type != TokenType::LBRACE) {
    logError("Expected '{' for enum class body, got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // Consume the { token

  // Parse enum members
  while (currentToken().type != TokenType::RBRACE &&
         currentToken().type != TokenType::END) {
    auto enumClassMember = parseEnumMember();
    if (!enumClassMember) {
      // Skip to the next comma, closing brace, or end to recover
      while (currentToken().type != TokenType::COMMA &&
             currentToken().type != TokenType::RBRACE &&
             currentToken().type != TokenType::END) {
        advance();
      }
    } else {
      enum_block.push_back(std::move(enumClassMember));
    }

    // Handle comma or unexpected tokens
    if (currentToken().type == TokenType::COMMA) {
      advance(); // Consume the comma
    } else if (currentToken().type == TokenType::SEMICOLON) {
      logError("Semicolons are not allowed inside enum class; use commas to "
               "separate members");
      advance(); // Consume the semicolon to continue parsing
    } else if (currentToken().type != TokenType::RBRACE) {
      logError("Expected ',' or '}' after enum member, got '" +
               currentToken().TokenLiteral + "'");
      // Don't advance here; let the loop check the next token
    }
  }

  // Check for closing brace
  if (currentToken().type != TokenType::RBRACE) {
    logError("Expected '}' to close enum class, got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // Consume the } token

  // Handle optional semicolon
  if (currentToken().type == TokenType::SEMICOLON) {
    advance(); // Consume the semicolon
  }

  return std::make_unique<EnumStatement>(isExportable, enum_token,
                                         std::move(enum_ident), int_token,
                                         std::move(enum_block));
}

//____________________COMPONENTS___________________
// Parsing component statements
std::unique_ptr<Statement> Parser::parseComponentStatement() {
  bool isExportable = false;
  std::vector<std::unique_ptr<Statement>>
      privateData; // An empty vector where the private data if created shall
                   // all be stored
  std::vector<std::unique_ptr<Statement>> privateMethods;

  std::vector<std::unique_ptr<Statement>> usedDataBlocks;
  std::vector<std::unique_ptr<Statement>> usedBehaviorBlocks;

  std::optional<std::unique_ptr<Statement>> initConstructor;

  Token component_token = currentToken();
  advance(); // Consume the keyword component
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected component name but got '" + currentToken().TokenLiteral +
             "'");
  }
  auto component_name = parseIdentifier();
  if (currentToken().type != TokenType::LBRACE) {
    logError("Expected { but got '" + currentToken().TokenLiteral + "'");
  }
  advance(); // Consuming the LPAREN
  // Now here we are inside the block so we have to parse the stuff inside

  while (true) {
    if (currentToken().type == TokenType::RBRACE ||
        currentToken().type == TokenType::END) {
      break; // stop parsing component body
    }
    std::unique_ptr<Statement> stmt = nullptr;
    bool isValid = (isBasicType(currentToken().type) ||
                    currentToken().type == TokenType::IDENTIFIER ||
                    currentToken().type == TokenType::AUTO) ||
                   (currentToken().type == TokenType::PTR ||
                    currentToken().type == TokenType::REF) ||
                   (currentToken().type == TokenType::MUT ||
                    currentToken().type == TokenType::CONST);

    if (isValid) {
      stmt = parseStatement();
      privateData.push_back(std::move(stmt));
      continue;
    }

    switch (currentToken().type) {
    case TokenType::USE:
      if (nextToken().type == TokenType::RECORD) {
        stmt = parseUseStatement();
        usedDataBlocks.push_back(std::move(stmt));
      } else {
        logError("Expected 'use record',but got '" +
                 nextToken().TokenLiteral + "'");
        advance(); // skip the unexpected token
      }
      break;
    case TokenType::PTR:
      stmt = parsePointerStatement();
    case TokenType::EXPORT: {
      auto exportStmt = parseExportStatement();
      auto fnStmt = dynamic_cast<FunctionStatement *>(exportStmt.get());
      if (!fnStmt) {
        logError("Only function exports are allowed in component statement");
        break;
      }
      privateMethods.push_back(std::move(exportStmt));
      break;
    }

    case TokenType::FUNCTION:
      stmt = parseFunctionStatement();
      privateMethods.push_back(std::move(stmt));
      break;
    case TokenType::INIT:
      if (initConstructor.has_value()) {
        logError("Duplicate init constructor in component");
        advance();
        break;
      }
      stmt = parseInitConstructorStatement();
      initConstructor = std::move(stmt);
      break;
    case TokenType::SEMICOLON:
      advance();
      break;

    default:
      logError("Invalid statement encountered inside component");
      advance();
      continue;
    }
  }
  if (currentToken().type != TokenType::RBRACE) {
    logError("Expected } to close component block but got '" +
             currentToken().TokenLiteral + "'");
  }
  advance(); // Consume the RBRACE

  return std::make_unique<ComponentStatement>(
      isExportable, component_token, std::move(component_name),
      std::move(privateData), std::move(privateMethods),
      std::move(usedDataBlocks), std::move(usedBehaviorBlocks),
      std::move(initConstructor));
}

std::unique_ptr<Statement> Parser::parseInitConstructorStatement() {
  Token init_token = currentToken();
  advance(); // Consume 'init'

  std::vector<std::unique_ptr<Statement>> args;

  // Expect LPAREN
  if (currentToken().type != TokenType::LPAREN) {
    logError("Expected '(' after 'init', but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // consume '('

  // Parse arguments until RPAREN
  while (currentToken().type != TokenType::RPAREN &&
         currentToken().type != TokenType::END) {
    // Parse argument
    auto arg = parseStatement();
    if (arg) {
      args.push_back(std::move(arg));
    }

    if (currentToken().type == TokenType::COMMA) {
      advance(); // consume comma and continue
    } else if (currentToken().type != TokenType::RPAREN) {
      logError("Expected ',' or ')' in init argument list, got '" +
               currentToken().TokenLiteral + "'");
      return nullptr;
    }
  }

  // Expect closing RPAREN
  if (currentToken().type != TokenType::RPAREN) {
    logError("Expected ')' to close init argument list but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance(); // consume ')'

  // Parse the block statement
  if (currentToken().type != TokenType::LBRACE) {
    logError("[ERROR] Expected '{' to start init block but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }

  auto block = parseBlockStatement();
  return std::make_unique<InitStatement>(init_token, std::move(args),
                                         std::move(block));
}

// Parsing use statement
std::unique_ptr<Statement> Parser::parseUseStatement() {
  std::optional<std::unique_ptr<Expression>> functionCallOrData;
  Token use_token = currentToken();
  advance(); // Consume the use keyword token

  Token kind_token;
  if (currentToken().type == TokenType::RECORD) {
    kind_token = currentToken();
    advance(); // Consume 'record' or 'behavior'
  } else {
    logError("Expected either 'record' or 'behavior' keyword but got '" +
             currentToken().TokenLiteral + "'");
  }

  if (currentToken().type == TokenType::FULLSTOP ||
      currentToken().type == TokenType::SCOPE_OPERATOR) {
    logError("Expected record name but you are starting with a fullstop");
  }

  std::unique_ptr<Expression> expr = parseExpression(Precedence::PREC_NONE);

  // Special check to prevent function calls
  auto callExpr = dynamic_cast<CallExpression *>(expr.get());
  if (callExpr) {
    logError("Unexpected function call");
    return nullptr;
  }

  if (currentToken().type == TokenType::SEMICOLON) {
    advance();
  }

  return std::make_unique<UseStatement>(use_token, kind_token, std::move(expr));
}

//______________________RECORDS____________________
// Parsing record statement
std::unique_ptr<Statement> Parser::parseRecordStatement() {
  bool isExportable = false;
  Mutability mutability = Mutability::IMMUTABLE;
  std::vector<std::unique_ptr<Statement>> fields;
  if (currentToken().type == TokenType::MUT) {
    mutability = Mutability::MUTABLE;
    advance(); // Consuming the mut keyword token
  } else if (currentToken().type == TokenType::CONST) {
    mutability = Mutability::CONSTANT;
    advance(); // Comsume the const keyword
  }
  Token record_token = currentToken();
  advance(); // Consuming the data keyword token
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected record name but got '" + currentToken().TokenLiteral +
             "'");
  }
  auto dataBlockName = parseIdentifier();
  if (currentToken().type != TokenType::LBRACE) {
    logError("Expected { to start record block but got '" +
             currentToken().TokenLiteral + "'");
  }

  advance(); // Consuming the LPAREN

  while (currentToken().type != TokenType::RBRACE &&
         currentToken().type != TokenType::END) {
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
      continue;
    }
    auto recordStmt = parseStatement();
    if (isDeclaration(recordStmt.get())) {
      fields.push_back(std::move(recordStmt));
    } else {
      logError("Unsupported statement inside record");
    }
  }

  if (currentToken().type != TokenType::RBRACE) {
    logError("Expected } to close data block but got '" +
             currentToken().TokenLiteral + "'");
  }
  advance();

  return std::make_unique<RecordStatement>(
      isExportable, mutability, record_token, std::move(dataBlockName),
      std::move(fields));
}

// Parsing instance expression
std::unique_ptr<Expression>
Parser::parseInstanceExpression(std::unique_ptr<Expression> left) {
  std::vector<std::unique_ptr<Statement>> args;

  if (currentToken().type != TokenType::LBRACE) {
    logError("Expected '{' but got '" + currentToken().TokenLiteral + "'");
    advance();
    return nullptr;
  }

  advance(); // consume '{'

  // Check if immediately closed
  if (currentToken().type == TokenType::RBRACE) {
    advance(); // consume '}'
    return std::make_unique<InstanceExpression>(std::move(left),
                                                std::move(args));
  }

  // Parse the first argument
  std::unique_ptr<Statement> firstArg = parseAssignmentStatement();
  if (firstArg)
    args.push_back(std::move(firstArg));

  while (currentToken().type == TokenType::COMMA) {
    advance(); // consume ','
    std::unique_ptr<Statement> arg = parseAssignmentStatement();
    if (!arg) {
      return nullptr;
    }
    args.push_back(std::move(arg));
  }

  if (currentToken().type == TokenType::RBRACE) {
    advance(); // consume '}'
  } else {
    logError("Expected '}' after instance arguments");
    return nullptr;
  }

  return std::make_unique<InstanceExpression>(std::move(left), std::move(args));
}

// EXPORT STATEMENT
std::unique_ptr<Statement> Parser::parseExportStatement() {
  advance(); // Consume export token
  auto stmt = parseStatement();

  if (!stmt)
    return nullptr;

  if (auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get())) {
    if (auto fnExpr =
            dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get())) {
      fnExpr->isExportable = true;
    }

    if (auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(
            fnStmt->funcExpr.get())) {
      if (auto fnDeclr = dynamic_cast<FunctionDeclaration *>(
              fnDeclrExpr->funcDeclrStmt.get())) {
        fnDeclr->isExportable = true;
      }
    }
  } else if (auto recordStmt = dynamic_cast<RecordStatement *>(stmt.get())) {
    recordStmt->isExportable = true;
  } else if (auto compStmt = dynamic_cast<ComponentStatement *>(stmt.get())) {
    compStmt->isExportable = true;
  } else if (auto enumStmt = dynamic_cast<EnumStatement *>(stmt.get())) {
    enumStmt->isExportable = true;
  } else if (auto allocStmt = dynamic_cast<AllocatorStatement *>(stmt.get())) {
    allocStmt->isExportable = true;
  } else if (auto letStmt = dynamic_cast<LetStatement *>(stmt.get())) {
    letStmt->isExportable = true;
  } else if (auto sealStmt = dynamic_cast<SealStatement *>(stmt.get())) {
    sealStmt->isExportable = true;
  } else {
    logError("'export' can only be applied to functions, and custom types");
    return nullptr;
  }

  return stmt;
}

// Parsing block statements
std::unique_ptr<Statement> Parser::parseBlockStatement() {
  Token lbrace = currentToken();
  if (lbrace.type != TokenType::LBRACE) {
    logError("Expected '{' to start block but got '" + lbrace.TokenLiteral +
             "'");
    return nullptr;
  }
  advance();
  std::vector<std::unique_ptr<Statement>> statements;

  while (currentToken().type != TokenType::RBRACE &&
         currentToken().type != TokenType::END) {
    if (currentToken().type == TokenType::SEMICOLON) {
      advance(); // skip empty statement
      continue;
    }
    auto stmt = parseStatement();
    if (stmt != nullptr) {
      statements.push_back(std::move(stmt));
    } else {
      advance();
    }
  }

  if (currentToken().type != TokenType::RBRACE) {
    logError("Expected } to close block but got '" +
             currentToken().TokenLiteral + "'");
    return nullptr;
  }
  advance();

  return std::make_unique<BlockStatement>(lbrace, std::move(statements));
}
