#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"
#include <memory>

std::unique_ptr<Statement> Parser::parseVariableModifier() {
  // Collect all modifiers
  bool isPersist = false;
  bool isHeap = false;
  bool isMut = false;
  bool isConst = false;
  bool isVolatile = false;
  bool isRestrict = false;
  bool isExportable = false;

  Token heap_token;
  std::unique_ptr<Expression> allocType = nullptr;     // For heap<allocator>
  std::unique_ptr<Expression> type_modifier = nullptr; // For shit like ptr,ref
  std::unique_ptr<Expression> fnPtr_mod=nullptr; //For shit like func(i32):
  bool dynamicHeapDecl = false;
  bool isTypeModified = false;
  bool isFuncModded=false;

  while (true) {
    if (currentToken().type == TokenType::HEAP) {
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
    } else if (currentToken().type == TokenType::PERSIST) {
      isPersist = true;
      advance();
    } else if (currentToken().type == TokenType::VOLATILE) {
      isVolatile = true;
      advance();
    } else if (currentToken().type == TokenType::RESTRICT) {
      isRestrict = true;
      advance();
    } else if (currentToken().type == TokenType::EXPORT) {
      isExportable = true;
      advance();
    } else if(currentToken().type==TokenType::FN){
      isFuncModded=true;
      fnPtr_mod=parseFunctionPointerModifier();
    }else if (currentToken().type == TokenType::PTR ||
               currentToken().type == TokenType::REF ||
               currentToken().type == TokenType::ARRAY) {
      isTypeModified = true;
      type_modifier = parseTypeModifier();
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
  if (currentToken().type == TokenType::RECORD)
    stmt = parseRecordStatement();
  else if (currentToken().type == TokenType::COMPONENT)
    stmt = parseComponentStatement();
  else if (currentToken().type == TokenType::ASM)
    stmt = parseASMStatement();
  else if (currentToken().type == TokenType::FUNCTION)
    stmt = parseFunctionStatement();
  else if (currentToken().type == TokenType::ALLOCATOR)
    stmt = parseAllocatorStatement();
  else if (isBasicType(currentToken().type) ||
           currentToken().type == TokenType::IDENTIFIER ||
           currentToken().type == TokenType::AUTO)
    stmt = parseVariableDeclaration();
  else {
    logError("Expected variable declaration,components,records,allocators or "
             "function after modifiers",
             currentToken());
    return nullptr;
  }

  if (!stmt)
    return nullptr;

  auto varDecl = dynamic_cast<VariableDeclaration *>(stmt.get());
  // Apply collected modifiers to the statement
  if (varDecl) {
    varDecl->isPersist = isPersist;
    varDecl->isHeap = isHeap;
    varDecl->isVolatile = isVolatile;
    varDecl->isRestrict = isRestrict;
    varDecl->isExportable = isExportable;
    varDecl->mutability = mutability;
  } else if (auto recordStmt = dynamic_cast<RecordStatement *>(stmt.get())) {
    recordStmt->isVolatile = isVolatile;
    recordStmt->isExportable = isExportable;
    recordStmt->mutability = mutability;
  } else if (auto compStmt = dynamic_cast<ComponentStatement *>(stmt.get())) {
    compStmt->isExportable = isExportable;
  } else if (auto allocStmt = dynamic_cast<AllocatorStatement *>(stmt.get())) {
    allocStmt->isExportable = isExportable;
  } else if (auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get())) {
    auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
    auto fnDeclrExpr =
        dynamic_cast<FunctionDeclarationExpression *>(fnStmt->funcExpr.get());
    if (fnExpr)
      fnExpr->isExportable = isExportable;
    if (fnDeclrExpr) {
      if (auto fnDeclStmt = dynamic_cast<FunctionDeclaration *>(
              fnDeclrExpr->funcDeclrStmt.get())) {
        fnDeclStmt->isExportable = isExportable;
      }
    }
  } else if (auto asmStmt = dynamic_cast<ASMStatement *>(stmt.get())) {
    asmStmt->isVolatile = isVolatile;
  } else {
    logError("Cannot apply modifiers to this statement type", currentToken());
    return nullptr;
  }

  if (dynamicHeapDecl) {
    varDecl->allocator = std::move(allocType);
  }

  if (isTypeModified) {
    varDecl->modified_type = std::move(type_modifier);
  }

  if(isFuncModded){
    varDecl->fnPtrMod=std::move(fnPtr_mod);
  }

  return stmt;
}

std::unique_ptr<Statement> Parser::parseVariableDeclaration() {
  // Modifiers
  std::unique_ptr<Expression> allocator = nullptr;
  std::unique_ptr<Expression> fn_mod=nullptr;
  std::unique_ptr<Expression> modified_type = nullptr;

  // Cores
  std::unique_ptr<Expression> base_type = nullptr;
  std::unique_ptr<Expression> identifier = nullptr;

  if (isBasicType(currentToken().type) ||
      currentToken().type == TokenType::IDENTIFIER ||
      currentToken().type == TokenType::AUTO) {
    base_type = parseBasicType();
    if (!base_type) {
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

  if (currentToken().type != TokenType::IDENTIFIER) {
    logError("Expected variable name after data type but got '" +
                 currentToken().TokenLiteral + "'",
             currentToken());
    return nullptr;
  }

  identifier = parseIdentifier();
  if(!identifier){
    logError("Failed to parse identifier for variable name",previousToken());
    return nullptr;
  }

  std::optional<Token> assign_token;
  std::unique_ptr<Expression> value = nullptr;

  if (currentToken().type == TokenType::ASSIGN ||
      currentToken().type == TokenType::ARROW) {
    assign_token = currentToken();
    advance();
    value = parseExpression(Precedence::PREC_NONE);
  }

  return std::make_unique<VariableDeclaration>(
      std::move(allocator),std::move(fn_mod), std::move(modified_type), std::move(base_type),
      std::move(identifier), std::move(value), assign_token,
      Mutability::IMMUTABLE, false, false, false, false, false);
}
