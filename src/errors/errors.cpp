#include "errors.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>

#include "ast.hpp"

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

ErrorHandler::ErrorHandler(const std::string &file) : fileName(file) {
  loadSourceLines();
}

void ErrorHandler::report(CompilerError &error) {
  if (!hintBuffer.empty()) {
    for (auto &h : hintBuffer) {
      error.message.hints.push_back(std::move(h));
    }
    hintBuffer.clear();
  }

  std::string displayName = getLevelDisplayName(error.level);
  std::string baseName = getBaseName(fileName);

  // Header
  std::cerr << COLOR_BOLD << COLOR_RED << "[" << displayName << "] "
            << COLOR_RESET << baseName << ":" << error.line << ":"
            << error.column << " " << error.message.message << "\n";

  // Print 2 lines context
  int startLine = std::max(1, static_cast<int>(error.line) - 2);
  int endLine = std::min(static_cast<int>(sourceLines.size()),
                         static_cast<int>(error.line) + 2);

  for (int i = startLine; i <= endLine; i++) {
    // Line number and source
    std::cerr << " " << i << " | " << sourceLines[i - 1] << "\n";

    // Caret line for error line
    if (i == error.line) {
      std::cerr << "   | ";

      // Handle tabs for positioning
      [[maybe_unused]] int visualColumn = 0;
      std::string line = sourceLines[i - 1];

      // First, print spaces/tabs to reach the error column visually
      for (int j = 0;
           j < error.column - 1 && j < static_cast<int>(line.length()); j++) {
        if (line[j] == '\t') {
          std::cerr << "\t";
          visualColumn += 4; // Tabs count as 4 visually
        } else {
          std::cerr << " ";
          visualColumn++;
        }
      }

      // Draw the underline using tokenLength
      std::cerr << COLOR_RED;
      for (size_t k = 0; k < error.length; k++) {
        std::cerr << "^";
      }
      std::cerr << COLOR_RESET << "\n";
    }
  }

  // Hints
  if (!error.message.hints.empty()) {
    for (const auto &hint : error.message.hints) {
      std::cerr << COLOR_YELLOW << "suggestion: " << COLOR_RESET << hint
                << "\n";
    }
  }
}

std::string ErrorHandler::format_string(std::string &message,
                                        std::vector<std::string> &args) {
  for (size_t i = 0; i < args.size(); i++) {
    std::string slot = "{" + std::to_string(i) + "}";
    size_t pos = message.find(slot);
    if (pos != std::string::npos) {
      message.replace(pos, slot.length(), args[i]);
    }
  }
  return message;
}

ErrorMessage ErrorHandler::generateErrorMessage(ErrorCode code) {
  ErrorMessage message;
  switch (code) {
  case ErrorCode::UnexpectedChar: {
    message.code = ErrorCode::UnexpectedChar;
    message.message = "unexpected character '{0}'";
    return message;
  }
  case ErrorCode::UnterminatedString: {
    message.code = ErrorCode::UnterminatedString;
    message.message = "unterminated string '{0}'";
    return message;
  }
  case ErrorCode::UnterminatedComment: {
    message.code = ErrorCode::UnterminatedComment;
    message.message = "unterminated comment";
    return message;
  }
  case ErrorCode::UnexpectedToken: {
    message.code = ErrorCode::UnexpectedToken;
    message.message = "expected token '{0}' but got '{1}'";
    return message;
  }
  case ErrorCode::MissingClosingBracket: {
    message.code = ErrorCode::MissingClosingBracket;
    message.message = "missing closing bracket expected '{0}' but got '{1}'";
    return message;
  }
  case ErrorCode::InvalidReturnType: {
    message.code = ErrorCode::InvalidReturnType;
    message.message = "expected a return type but got '{0}'";
    return message;
  }
  case ErrorCode::InvalidType: {
    message.code = ErrorCode::InvalidType;
    message.message = "expected a type but got '{0}'";
    return message;
  }
  case ErrorCode::InvalidModifier: {
    message.code = ErrorCode::InvalidModifier;
    message.message = "cannot apply modifier to this statement";
    return message;
  }
  case ErrorCode::InvalidConstraint: {
    message.code = ErrorCode::InvalidConstraint;
    message.message="expected 'in' or 'out' after ':' but got '{0}'";
    return message;
  }case ErrorCode::ExpectIntegerToken:{
    message.code=ErrorCode::ExpectIntegerToken;
    message.message="expected an intenger token but got '{0}'";
    return message;
  }case ErrorCode::UnsupportedStatement:{
    message.code=ErrorCode::UnsupportedStatement;
    message.message="unexpected statement";
    return message;
  }case ErrorCode::DuplicateInit:{
    message.code=ErrorCode::DuplicateInit;
    message.message="duplicate init";
    return message;
  }
  case ErrorCode::UndefinedVariable: {
    message.code = ErrorCode::UndefinedVariable;
    message.message = "Undefined variable '{0}'";
    return message;
  }
  case ErrorCode::TypeMismatch: {
    message.code = ErrorCode::TypeMismatch;
    message.message = "type mismatch expected '{0}' but got '{1}'";
    message.hints.push_back("Try casting");
    return message;
  }
  }
}

std::string ErrorHandler::getLevelDisplayName(ErrorLevel level) {
  switch (level) {
  case ErrorLevel::LEXER:
    return "Lexer error";
  case ErrorLevel::PARSER:
    return "Parsing error";
  case ErrorLevel::SEMANTIC:
    return "Semantic error";
  case ErrorLevel::AUDITOR:
    return "Auditor error";
  case ErrorLevel::LAYOUT:
    return "Layout error";
  case ErrorLevel::INTERNAL:
    return "INTERNAL COMPILER ERROR";
  case ErrorLevel::IMPORT:
    return "Import error";
  }

  return "Unknown issue";
}

std::string ErrorHandler::getBaseName(const std::string &fullPath) {
  if (fullPath.empty()) {
    std::cerr << COLOR_RED << "[ERROR HANDLER WARNING]" << COLOR_RESET
              << " fullPath is empty!\n";
    return "unknown_file";
  }

  size_t pos = fullPath.find_last_of("/\\");
  if (pos == std::string::npos)
    return fullPath;

  return fullPath.substr(pos + 1);
}

void ErrorHandler::loadSourceLines() {
  std::ifstream in(fileName);
  if (!in.is_open()) {
    std::cerr << COLOR_RED << "[SOURCE LOAD ERROR]" << COLOR_RESET
              << "Cannot open file: " << fileName << "\n";
    return;
  }

  std::string line;
  while (std::getline(in, line)) {
    sourceLines.push_back(line);
  }

  if (sourceLines.empty())
    std::cerr << COLOR_YELLOW << "[SOURCE LOAD WARNING]" << COLOR_RESET
              << "File has no lines: " << fileName << "\n";
}

std::string ErrorHandler::getSourceLine(int line,
                                        std::vector<std::string> sourceLines) {
  if (line - 1 < 0 || line - 1 >= static_cast<int>(sourceLines.size())) {
    return " ";
  }

  return sourceLines[line - 1];
}

ErrorHandler &ErrorHandler::addHint(const std::string &hint) {
  hintBuffer.push_back(hint);
  return *this;
}

int ErrorHandler::getTokenLength(Node *contextNode) {
  if (auto exprStmt = dynamic_cast<ExpressionStatement *>(contextNode))
    return getTokenLength(exprStmt->expression.get());

  if (auto ident = dynamic_cast<Identifier *>(contextNode))
    return ident->identifier.TokenLiteral.length();

  if (auto i8Lit = dynamic_cast<I8Literal *>(contextNode))
    return i8Lit->i8_token.TokenLiteral.length();
  if (auto u8Lit = dynamic_cast<U8Literal *>(contextNode))
    return u8Lit->u8_token.TokenLiteral.length();
  if (auto i16Lit = dynamic_cast<I16Literal *>(contextNode))
    return i16Lit->i16_token.TokenLiteral.length();
  if (auto u16Lit = dynamic_cast<U16Literal *>(contextNode))
    return u16Lit->u16_token.TokenLiteral.length();
  if (auto i32Lit = dynamic_cast<I32Literal *>(contextNode))
    return i32Lit->i32_token.TokenLiteral.length();
  if (auto u32Lit = dynamic_cast<U32Literal *>(contextNode))
    return u32Lit->u32_token.TokenLiteral.length();
  if (auto i64Lit = dynamic_cast<I64Literal *>(contextNode))
    return i64Lit->i64_token.TokenLiteral.length();
  if (auto u64Lit = dynamic_cast<U64Literal *>(contextNode))
    return u64Lit->u64_token.TokenLiteral.length();
  if (auto i128Lit = dynamic_cast<I128Literal *>(contextNode))
    return i128Lit->i128_token.TokenLiteral.length();
  if (auto u128Lit = dynamic_cast<U128Literal *>(contextNode))
    return u128Lit->u128_token.TokenLiteral.length();
  if (auto iSize = dynamic_cast<ISIZELiteral *>(contextNode))
    return iSize->isize_token.TokenLiteral.length();
  if (auto uSizeLit = dynamic_cast<USIZELiteral *>(contextNode))
    return uSizeLit->usize_token.TokenLiteral.length();

  if (auto f32Lit = dynamic_cast<F32Literal *>(contextNode))
    return f32Lit->f32_token.TokenLiteral.length();
  if (auto f64Lit = dynamic_cast<F64Literal *>(contextNode))
    return f64Lit->f64_token.TokenLiteral.length();

  if (auto strLit = dynamic_cast<StringLiteral *>(contextNode))
    return strLit->string_token.TokenLiteral.length();

  if (auto char8Lit = dynamic_cast<Char8Literal *>(contextNode))
    return char8Lit->char8_token.TokenLiteral.length();
  if (auto char16Lit = dynamic_cast<Char16Literal *>(contextNode))
    return char16Lit->char16_token.TokenLiteral.length();
  if (auto char32Lit = dynamic_cast<Char32Literal *>(contextNode))
    return char32Lit->char32_token.TokenLiteral.length();

  if (auto boolLit = dynamic_cast<BooleanLiteral *>(contextNode))
    return boolLit->boolean_token.TokenLiteral.length();

  if (auto arrLit = dynamic_cast<ArrayLiteral *>(contextNode)) {
    auto dimensionCount = 2;
    if (arrLit->array.empty()) {
      for (const auto &expr : arrLit->array) {
        dimensionCount += getTokenLength(expr.get());
      }
    }

    return dimensionCount;
  }

  if (auto fnStmt = dynamic_cast<FunctionStatement *>(contextNode))
    return getTokenLength(fnStmt->funcExpr.get());

  if (auto fnExpr = dynamic_cast<FunctionExpression *>(contextNode))
    return fnExpr->func_key.TokenLiteral.length();

  if (auto fnDeclExpr =
          dynamic_cast<FunctionDeclarationExpression *>(contextNode))
    return getTokenLength(fnDeclExpr->funcDeclrStmt.get());

  if (auto fnDecl = dynamic_cast<FunctionDeclaration *>(contextNode))
    return getTokenLength(fnDecl->function_name.get());

  if (auto basicType = dynamic_cast<BasicType *>(contextNode))
    return basicType->data_token.TokenLiteral.length();

  if (auto fnCall = dynamic_cast<CallExpression *>(contextNode))
    return getTokenLength(fnCall->function_identifier.get());

  if (auto infix = dynamic_cast<InfixExpression *>(contextNode)) {
    auto rightLen = getTokenLength(infix->right_operand.get());
    auto leftLen = getTokenLength(infix->left_operand.get());
    auto operatorLen = infix->operat.TokenLiteral.length();
    return rightLen + operatorLen + leftLen;
  }

  if (auto sizeExpr = dynamic_cast<SizeOfExpression *>(contextNode)) {
    auto sizeKeyLen = sizeExpr->sizeOf.TokenLiteral.length();
    auto typeLen = getTokenLength(sizeExpr->type.get());
    return sizeKeyLen + typeLen + 2;
  }

  if (auto castExpr = dynamic_cast<CastExpression *>(contextNode)) {
    auto castKeyLen = castExpr->cast.TokenLiteral.length();
    auto typeLen = getTokenLength(castExpr->type.get());
    auto exprLen = getTokenLength(castExpr->expr.get());
    return castKeyLen + typeLen + exprLen + 4;
  }

  if (auto bitcastExpr = dynamic_cast<BitcastExpression *>(contextNode)) {
    auto bitcastKeyLen = bitcastExpr->bitcast.TokenLiteral.length();
    auto typeLen = getTokenLength(bitcastExpr->type.get());
    auto exprLen = getTokenLength(bitcastExpr->expr.get());
    return bitcastKeyLen + typeLen + exprLen + 4;
  }

  // Default to 1 if u dont know
  return 1;
}
