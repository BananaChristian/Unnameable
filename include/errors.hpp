#pragma once
#include "ast.hpp"
#include <cstddef>
#include <string>
#include <vector>

enum class ErrorLevel {
  LEXER,
  PARSER,
  IMPORT,
  SEMANTIC,
  AUDITOR,
  LAYOUT,
  INTERNAL
};

enum ErrorCode {
  GenericError=0000,

  // Lexer errors
  UnexpectedChar = 1001,
  UnterminatedString = 1002,
  InvalidEscape = 1003,
  UnterminatedChar = 1004,
  UnterminatedComment = 1005,
  InvalidToken = 1006,

  // Parser errors
  UnexpectedToken = 2001,
  MissingClosingBracket = 2002,
  InvalidReturnType = 2003,
  InvalidType = 2004,
  InvalidModifier = 2005,
  InvalidConstraint = 2006,
  ExpectIntegerToken = 2007,
  DuplicateInit = 2008,
  UnsupportedStatement = 2009,
  MultipleMutSpecifiers = 2010,

  // Semantics
  UndefinedVariable = 3001,
  TypeMismatch = 3002,
};

struct ErrorMessage {
  ErrorCode code;
  std::string message;
  std::vector<std::string> hints;
};

struct CompilerError {
  ErrorLevel level;
  ErrorCode code;
  size_t line;
  size_t column;
  ErrorMessage message;
  size_t length;
};

class ErrorHandler {
  std::string fileName;
  std::vector<std::string> hintBuffer;

public:
  ErrorHandler(const std::string &fileName);
  void report(CompilerError &compilerError);
  ErrorHandler &addHint(const std::string &hint);
  ErrorMessage generateErrorMessage(ErrorCode code);
  int getTokenLength(Node *contextNode);
  std::string format_string(std::string &message,
                            std::vector<std::string> &args);

private:
  std::vector<std::string> sourceLines;

  void loadSourceLines();
  std::string getLevelDisplayName(ErrorLevel level);
  std::string getBaseName(const std::string &fullPath);
  std::string getSourceLine(int line, std::vector<std::string> sourceLines);
};
