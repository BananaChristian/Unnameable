#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"

std::unique_ptr<Statement> Parser::parseStructureModifier() {
  bool isPacked = false;
  bool isBitField = false;
  bool isUnion = false;

  Token alignToken;
  bool alignModded = false;
  std::unique_ptr<Expression> alignExpr;
  while (true) {
    if (currentToken().type == TokenType::ALIGN) {
      logInternal("align");
      alignToken = currentToken();
      alignModded = true;
      if (nextToken().type == TokenType::LPAREN) {
        advance(); // Consume align
        advance(); // Consume (
        alignExpr = parseExpression(get_precedence(currentToken().type));
        if (currentToken().type != TokenType::RPAREN) {
          logError(ErrorCode::UnexpectedToken, currentToken(),
                   {"')'", currentToken().TokenLiteral});
          return nullptr;
        }
        advance(); // Consume )
      } else {
        advance();
      }
    } else if (currentToken().type == TokenType::PACKED) {
      logInternal("packed");
      isPacked = true;
      advance();
    } else if (currentToken().type == TokenType::UNION) {
      logInternal("union");
      isUnion = true;
      advance();
    } else if (currentToken().type == TokenType::BITFIELD) {
      logInternal("bitfield");
      isBitField = true;
      advance();
    } else {
      break;
    }
  }

  // For now only record statements
  std::unique_ptr<Statement> stmt = parseRecordStatement();

  if (!stmt)
    return nullptr;

  auto recordStmt = dynamic_cast<RecordStatement *>(stmt.get());
  if (!recordStmt->modifiers)
    recordStmt->modifiers =
        std::make_unique<StructureModifier>(false, false, false, nullptr);

  if (recordStmt) {
    recordStmt->modifiers->isBitfield = isBitField;
    recordStmt->modifiers->isUnion = isUnion;
    recordStmt->modifiers->isPacked = isPacked;
  } else {
    logError(ErrorCode::InvalidModifier, currentToken());
    return nullptr;
  }

  if (alignModded) {
    recordStmt->modifiers->_align = std::move(alignExpr);
  }

  return stmt;
}

std::unique_ptr<ASMConstraint> Parser::parseASMConstraint() {
  Token colon_token = currentToken();
  advance(); // consume ':'

  // Parse direction "in" or "out"
  std::string direction = currentToken().TokenLiteral;
  if (direction != "in" && direction != "out") {
    logError(ErrorCode::InvalidConstraint, currentToken(),
             {currentToken().TokenLiteral});
    return nullptr;
  }
  advance(); // consume direction

  // Parse constraint "r", "m", "rm"
  std::string constraint = currentToken().TokenLiteral;
  advance(); // consume constraint

  auto variable = parseIdentifier();

  return std::make_unique<ASMConstraint>(colon_token, direction, constraint,
                                         std::move(variable));
}

// Parse Asm instructions
std::unique_ptr<Statement> Parser::parseASMInstruction() {
  std::string mnemonic = currentToken().TokenLiteral;
  advance();

  std::vector<std::string> operands;

  auto isOperandTerminator = [&]() -> bool {
    auto t = currentToken().type;
    return t == TokenType::COMMA || t == TokenType::COLON ||
           t == TokenType::RBRACE || t == TokenType::SEMICOLON ||
           t == TokenType::END;
  };

  auto parseOneOperand = [&]() -> std::string {
    std::string operand;

    if (currentToken().type == TokenType::LBRACKET) {
      operand += "[";
      advance();
      while (currentToken().type != TokenType::RBRACKET &&
             currentToken().type != TokenType::RBRACE &&
             currentToken().type != TokenType::END) {
        operand += currentToken().TokenLiteral;
        advance();
      }
      operand += "]";
      advance();
      return operand;
    }

    while (!isOperandTerminator()) {
      if (currentToken().type == TokenType::DOLLAR) {
        if (!operand.empty())
          operand += " ";
        operand += "$";
        advance();
        operand += currentToken().TokenLiteral;
        advance();
      } else {
        if (!operand.empty())
          operand += " ";
        operand += currentToken().TokenLiteral;
        advance();
      }
    }
    return operand;
  };

  // First operand, no comma
  if (!isOperandTerminator()) {
    operands.push_back(parseOneOperand());
  }

  // Subsequent operands, comma separated
  while (currentToken().type == TokenType::COMMA) {
    advance();
    operands.push_back(parseOneOperand());
  }

  // Constraints
  std::vector<std::unique_ptr<ASMConstraint>> constraints;
  while (currentToken().type == TokenType::COLON) {
    auto constraint = parseASMConstraint();
    if (constraint)
      constraints.push_back(std::move(constraint));
  }

  return std::make_unique<ASMInstruction>(mnemonic, std::move(operands),
                                          std::move(constraints));
}

std::unique_ptr<Statement> Parser::parseASMStatement() {
  Token asm_token = currentToken();
  advance(); // consume 'asm'

  // Parse optional dialect <intel> or <att>
  std::string dialect = "intel";
  if (currentToken().type == TokenType::LESS_THAN) {
    advance(); // consume '<'
    dialect = currentToken().TokenLiteral;
    advance(); // consume dialect name
    if (currentToken().type != TokenType::GREATER_THAN) {
      logError(ErrorCode::UnexpectedToken, currentToken(),
               {">", currentToken().TokenLiteral});
      return nullptr;
    }
    advance(); // consume '>'
  }

  if (currentToken().type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  advance(); // consume '{'

  std::vector<std::unique_ptr<Statement>> instructions;

  while (currentToken().type != TokenType::RBRACE &&
         currentToken().type != TokenType::END) {
    // Skip semicolons between instructions
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
      continue;
    }
    auto instruction = parseASMInstruction();
    if (instruction)
      instructions.push_back(std::move(instruction));
  }

  if (currentToken().type == TokenType::RBRACE)
    advance(); // consume '}'

  return std::make_unique<ASMStatement>(false, asm_token, dialect,
                                        std::move(instructions));
}

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
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
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
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"identifier", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  auto genericIdent = parseIdentifier();

  // type param list
  if (currentToken().type != TokenType::LPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'('", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
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
      return nullptr;
    }
  }

  if (currentToken().type != TokenType::RPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"')'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  advance(); // consume ')'

  if (currentToken().type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
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
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'identifier", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }

  auto ident = parseIdentifier();
  // Expect '('
  if (currentToken().type != TokenType::LPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'('", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
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
      return nullptr;
    }

    // If comma → eat it and continue
    if (currentToken().type == TokenType::COMMA) {
      advance();
      continue;
    }

    // If not comma, next MUST be ')'
    if (currentToken().type != TokenType::RPAREN) {
      logError(ErrorCode::UnexpectedToken, currentToken(),
               {"',' or ')'", currentToken().TokenLiteral});
      synchronize(SyncLevel::TOP);
      return nullptr;
    }
  }

  auto generic_call = std::make_unique<GenericCall>(std::move(ident), types);

  advance();

  if (currentToken().type != TokenType::AS) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'as'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  Token as = currentToken();
  advance();

  if (currentToken().type != TokenType::IDENTIFIER) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'identifier'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  Token alias = currentToken();
  advance();

  return std::make_unique<InstantiateStatement>(
      false, instantiate, std::move(generic_call), as, alias);
}

//__________________________ENUMS__________________________
// Parsing enum member node
std::unique_ptr<EnumMember> Parser::parseEnumMember() {
  // Check if the current token is an identifier
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'identifier'", currentToken().TokenLiteral});
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
      logError(ErrorCode::ExpectIntegerToken, currentToken(),
               {currentToken().TokenLiteral});
      return nullptr; // Return nullptr without advancing
    }
    // Parse the expression for the integer value
    value = parseExpression(Precedence::PREC_NONE);
    if (!value) {
      return nullptr;
    }
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
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'identifier'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }

  // Handle optional underlying type
  if (currentToken().type == TokenType::COLON) {
    advance(); // Consume the colon token
    if (!isIntegerType(currentToken().type)) {
      logError(ErrorCode::ExpectIntegerToken, currentToken(),
               {currentToken().TokenLiteral});
      synchronize(SyncLevel::TOP);
      return nullptr;
    }
    int_token = currentToken();
    advance(); // Consume the integer type token
  }

  // Check for opening brace
  if (currentToken().type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
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
      advance(); // Consume the semicolon to continue parsing
    } else if (currentToken().type != TokenType::RBRACE) {
      logError(ErrorCode::UnexpectedToken, currentToken(),
               {"',' or '}'", currentToken().TokenLiteral});
      // Don't advance here; let the loop check the next token
    }
  }

  // Check for closing brace
  if (currentToken().type != TokenType::RBRACE) {
    logError(ErrorCode::MissingClosingBracket, currentToken(),
             {currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
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
      fields; // An empty vector where the private data if created shall
              // all be stored
  std::vector<std::unique_ptr<Statement>> methods;

  std::vector<std::unique_ptr<Statement>> injectedfields;

  std::optional<std::unique_ptr<Statement>> initConstructor;

  Token component_token = currentToken();
  advance(); // Consume the keyword component
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'identifier'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  auto component_name = parseIdentifier();
  advance(); // Consuming the LPAREN
  // Now here we are inside the block so we have to parse the stuff inside

  while (true) {
    if (currentToken().type == TokenType::RBRACE ||
        currentToken().type == TokenType::END) {
      break; // stop parsing component body
    }
    std::unique_ptr<Statement> stmt = nullptr;
    bool isValid = (isBasicType(currentToken().type) ||
                    currentToken().type == TokenType::FN ||
                    currentToken().type == TokenType::IDENTIFIER ||
                    currentToken().type == TokenType::AUTO) ||
                   (currentToken().type == TokenType::PTR ||
                    currentToken().type == TokenType::REF) ||
                   (currentToken().type == TokenType::MUT ||
                    currentToken().type == TokenType::CONST);

    if (isValid) {
      stmt = parseStatement();
      fields.push_back(std::move(stmt));
      continue;
    }

    switch (currentToken().type) {
    case TokenType::INJECT:
      if (nextToken().type == TokenType::RECORD) {
        stmt = parseInjectStatement();
        injectedfields.push_back(std::move(stmt));
      } else {
        logError(ErrorCode::UnexpectedToken, currentToken(),
                 {"'record", nextToken().TokenLiteral});
        advance(); // skip the unexpected token
      }
      break;
    case TokenType::EXPORT: {
      auto exportStmt = parseVariableModifier();
      auto fnStmt = dynamic_cast<FunctionStatement *>(exportStmt.get());
      if (!fnStmt) {
        break;
      }
      methods.push_back(std::move(exportStmt));
      break;
    }

    case TokenType::FUNCTION:
      stmt = parseFunctionStatement();
      methods.push_back(std::move(stmt));
      break;
    case TokenType::INIT:
      if (initConstructor.has_value()) {
        logError(ErrorCode::DuplicateInit, currentToken());
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
      advance();
      continue;
    }
  }
  if (currentToken().type != TokenType::RBRACE) {
    logError(ErrorCode::MissingClosingBracket, currentToken(),
             {currentToken().TokenLiteral});
  }
  advance(); // Consume the RBRACE

  return std::make_unique<ComponentStatement>(
      isExportable, component_token, std::move(component_name),
      std::move(fields), std::move(methods), std::move(injectedfields),
      std::move(initConstructor));
}

std::unique_ptr<Statement> Parser::parseInitConstructorStatement() {
  Token init_token = currentToken();
  advance(); // Consume 'init'

  std::vector<std::unique_ptr<Statement>> args;

  // Expect LPAREN
  if (currentToken().type != TokenType::LPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'('", currentToken().TokenLiteral});
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
      logError(ErrorCode::UnexpectedToken, currentToken(),
               {"',' or ')'", currentToken().TokenLiteral});
      return nullptr;
    }
  }

  // Expect closing RPAREN
  if (currentToken().type != TokenType::RPAREN) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"')'", currentToken().TokenLiteral});
    return nullptr;
  }
  advance(); // consume ')'

  // Parse the block statement
  if (currentToken().type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
    return nullptr;
  }

  auto block = parseBlockStatement();
  return std::make_unique<InitStatement>(init_token, std::move(args),
                                         std::move(block));
}

// Parsing use statement
std::unique_ptr<Statement> Parser::parseInjectStatement() {
  std::optional<std::unique_ptr<Expression>> functionCallOrData;
  Token inject_token = currentToken();
  advance(); // Consume the inject keyword token

  Token kind_token;
  if (currentToken().type == TokenType::RECORD) {
    kind_token = currentToken();
    advance(); // Consume 'record'
  } else {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'record'", currentToken().TokenLiteral});
  }

  if (currentToken().type == TokenType::FULLSTOP ||
      currentToken().type == TokenType::SCOPE_OPERATOR) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'identifier'", currentToken().TokenLiteral});
  }

  std::unique_ptr<Expression> expr = parseExpression(Precedence::PREC_NONE);

  if (currentToken().type == TokenType::SEMICOLON) {
    advance();
  }

  return std::make_unique<InjectStatement>(inject_token, kind_token,
                                           std::move(expr));
}

//______________________RECORDS____________________
// Parsing record statement
std::unique_ptr<Statement> Parser::parseRecordStatement() {
  std::vector<std::unique_ptr<Statement>> fields;
  Token record_token = currentToken();
  advance(); // Consuming the data keyword token
  if (currentToken().type != TokenType::IDENTIFIER) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'identifier'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }
  auto recordName = parseIdentifier();
  if (currentToken().type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
    synchronize(SyncLevel::TOP);
    return nullptr;
  }

  advance(); // Consuming the LBRACE

  while (currentToken().type != TokenType::RBRACE &&
         currentToken().type != TokenType::END) {
    if (currentToken().type == TokenType::SEMICOLON) {
      advance();
      continue;
    }
    auto field = parseStatement();
    if (dynamic_cast<VariableDeclaration *>(field.get())) {
      fields.push_back(std::move(field));
    } else {
      logError(ErrorCode::UnsupportedStatement, currentToken());
    }
  }

  if (currentToken().type != TokenType::RBRACE) {
    logError(ErrorCode::MissingClosingBracket, currentToken(),
             {currentToken().TokenLiteral});
  }
  advance();

  return std::make_unique<RecordStatement>(
      record_token, false, false, Mutability::IMMUTABLE, nullptr,
      std::move(recordName), std::move(fields));
}

// Parsing instance expression
std::unique_ptr<Expression>
Parser::parseInstanceExpression(std::unique_ptr<Expression> left) {
  std::vector<std::unique_ptr<Statement>> args;

  if (currentToken().type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
    synchronize(SyncLevel::MID);
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
    logError(ErrorCode::MissingClosingBracket, currentToken(),
             {currentToken().TokenLiteral});
    return nullptr;
  }

  return std::make_unique<InstanceExpression>(std::move(left), std::move(args));
}

// Parsing block statements
std::unique_ptr<Statement> Parser::parseBlockStatement() {
  Token lbrace = currentToken();
  if (lbrace.type != TokenType::LBRACE) {
    logError(ErrorCode::UnexpectedToken, currentToken(),
             {"'{'", currentToken().TokenLiteral});
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
    logError(ErrorCode::MissingClosingBracket, currentToken(),
             {currentToken().TokenLiteral});
    return nullptr;
  }
  advance();

  return std::make_unique<BlockStatement>(lbrace, std::move(statements));
}
