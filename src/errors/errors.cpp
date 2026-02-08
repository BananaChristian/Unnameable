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
  std::string sourceLine = getSourceLine(error.line, sourceLines);

  std::string baseName = getBaseName(fileName);

  std::cerr << COLOR_BOLD << COLOR_RED << "[" << displayName << "] "
            << COLOR_RESET << baseName << error.line << ":" << error.col << " "
            << error.message << "\n";

  // Print the source line + caret if the line exists
  if (!sourceLine.empty()) {
    std::cerr << sourceLine << "\n" << std::string(error.col, ' ') << "^\n";
  }

  // Optional hints
  if (!error.hints.empty()) {
    std::cerr << COLOR_BOLD << COLOR_YELLOW << "[HINT]" << COLOR_RESET << "\n";
  }
  for (const auto &hint : error.hints) {
    std::cerr << COLOR_YELLOW << hint << COLOR_RESET << "\n";
  }
}

std::string ErrorHandler::getLevelDisplayName(ErrorLevel level) {
  switch (level) {
  case ErrorLevel::LEXER:
    return "Lexer issue";
  case ErrorLevel::PARSER:
    return "Parsing issue";
  case ErrorLevel::SEMANTIC:
    return "Semantic issue";
  case ErrorLevel::LAYOUT:
    return "Layout issue";
  case ErrorLevel::INTERNAL:
    return "Internal Compiler error";
  case ErrorLevel::IMPORT:
    return "Import issue";
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
  else
    std::cerr << "[SOURCE LOAD INFO] Loaded " << sourceLines.size()
              << " lines from " << fileName << "\n";
}

std::string ErrorHandler::getSourceLine(int line,
                                        std::vector<std::string> sourceLines) {
  if (line - 1 < 0 || line - 1 >= sourceLines.size()) {
    return " ";
  }

  return sourceLines[line - 1];
}

ErrorHandler &ErrorHandler::addHint(const std::string &hint) {
  hintBuffer.push_back(hint);
  return *this;
}
