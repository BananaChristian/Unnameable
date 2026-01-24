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

ErrorHandler::ErrorHandler(const std::string &file)
    : fileName(file), hintGen() {
  loadSourceLines();
}

void ErrorHandler::report(CompilerError &error) {
  std::string displayName = getLevelDisplayName(error.level);
  std::string sourceLine = getSourceLine(error.line, sourceLines);

  std::string baseName = getBaseName(fileName);

  error.hints = hintGen.generateHints(error, sourceLine);

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
  case ErrorLevel::SENTINEL:
    return "Sentinel issue";
  case ErrorLevel::IRGEN:
    return "Codegen issue";
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

//---------------------HINT GENERATOR-------------------------

HintGenerator::HintGenerator() { registerHintFns(); }

void HintGenerator::registerHintFns() {
  hintGeneratorFnsMap[ErrorLevel::PARSER] = &HintGenerator::generateParserHints;
  hintGeneratorFnsMap[ErrorLevel::SEMANTIC] =
      &HintGenerator::generateSemanticHints;
}

std::vector<std::string>
HintGenerator::generateHints(const CompilerError &error,
                             std::string sourceLine) {

  std::vector<std::string> out;
  auto it = hintGeneratorFnsMap.find(error.level);
  if (it != hintGeneratorFnsMap.end()) {
    out = (this->*(it->second))(error, sourceLine);
  }

  return out;
}

std::vector<std::string>
HintGenerator::generateParserHints(const CompilerError &error,
                                   std::string sourceLine) {
  std::vector<std::string> hints;
  std::smatch match;

  static const std::vector<HintRule> rules = {

      {std::regex("unexpected token '([^']+)'"),
       [](const std::smatch &m, const std::string &src) {
         std::vector<std::string> h;
         h.push_back("Token '" + m[1].str() + "' cannot appear here.");
         if (!src.empty() && src.back() != ';')
           h.push_back("This line may be missing a semicolon.");
         return h;
       }}};

  for (const auto &rule : rules) {
    if (std::regex_search(error.message, match, rule.pattern)) {
      auto result = rule.handler(match, sourceLine);
      hints.insert(hints.end(), result.begin(), result.end());
    }
  }

  return hints;
}

std::vector<std::string>
HintGenerator::generateSemanticHints(const CompilerError &error,
                                     std::string sourceLine) {
  // Local output
  std::vector<std::string> hints;
  std::smatch match;

  // ---- RULES TABLE ----
  static const std::vector<HintRule> rules = {

      // Undeclared variable
      {std::regex("undeclared[^']*'([a-zA-Z_][a-zA-Z0-9_]*)'"),
       [](const std::smatch &m, const std::string &src) {
         return std::vector<std::string>{
             "You used '" + m[1].str() + "' but it was never declared",
             "Check for typos or missing declarations"};
       }},

      // Type mismatch ( expected X but got Y)
      {std::regex("expected '([^']+)' but got '([^']+)'"),
       [](const std::smatch &m, const std::string &src) {
         return std::vector<std::string>{"Convert '" + m[2].str() + "' to '" +
                                         m[1].str() +
                                         "' or adjust the variable type"};
       }},

      // Cannot convert implicitly(This will come in handy when I add casting)
      {std::regex("cannot convert from '([^']+)' to '([^']+)'"),
       [](const std::smatch &m, const std::string &) {
         return std::vector<std::string>{
             "Consider explicit casting or using the correct type: '" +
             m[1].str() + "' → '" + m[2].str() + "'"};
       }}};

  // Rule execution
  for (const auto &rule : rules) {
    if (std::regex_search(error.message, match, rule.pattern)) {
      auto result = rule.handler(match, sourceLine);
      hints.insert(hints.end(), result.begin(), result.end());
    }
  }

  return hints;
}
