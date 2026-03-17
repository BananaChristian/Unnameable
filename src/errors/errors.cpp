#include "errors.hpp"
#include <fstream>
#include <iostream>

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
      error.hints.push_back(std::move(h));
    }
    hintBuffer.clear();
  }

  std::string displayName = getLevelDisplayName(error.level);
  std::string baseName = getBaseName(fileName);

  // Header
  std::cerr << COLOR_BOLD << COLOR_RED << "[" << displayName << "] "
            << COLOR_RESET << baseName << ":" << error.line << ":" << error.col
            << " " << error.message << "\n";

  // Print 2 lines context
  int startLine = std::max(1, error.line - 2);
  int endLine = std::min((int)sourceLines.size(), error.line + 2);

  for (int i = startLine; i <= endLine; i++) {
    // Line number and source
    std::cerr << " " << i << " | " << sourceLines[i - 1] << "\n";

    // Caret line for error line
    if (i == error.line) {
      std::cerr << "   | ";
      for (int j = 0; j < error.col - 1; j++) {
        std::cerr << (sourceLines[i - 1][j] == '\t' ? "\t" : " ");
      }
      std::cerr << COLOR_RED << "^" << COLOR_RESET << "\n";
    }
  }

  // Hints
  if (!error.hints.empty()) {
    for (const auto &hint : error.hints) {
      std::cerr << COLOR_YELLOW << "note: " << COLOR_RESET << hint << "\n";
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
