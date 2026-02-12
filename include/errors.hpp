#pragma once
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

struct CompilerError {
  ErrorLevel level;
  int line;
  int col;
  std::string message;
  std::vector<std::string> hints;
};

class ErrorHandler {
  std::string fileName;
  std::vector<std::string> hintBuffer;

public:
  ErrorHandler(const std::string &fileName);
  void report(CompilerError &compilerError);
  ErrorHandler &addHint(const std::string &hint);

private:
  std::vector<std::string> sourceLines;

  void loadSourceLines();
  std::string getLevelDisplayName(ErrorLevel level);
  std::string getBaseName(const std::string &fullPath);
  std::string getSourceLine(int line, std::vector<std::string> sourceLines);
};
