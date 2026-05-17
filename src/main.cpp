#include <filesystem>
#include <fstream>
#include <iostream>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "audit.hpp"
#include "deserial.hpp"
#include "irgen.hpp"
#include "layout.hpp"
#include "lexer.hpp"
#include "linker.hpp"
#include "parser.hpp"
#include "semantics.hpp"
#include "stubgen.hpp"
#include "token.hpp"

namespace fs = std::filesystem;

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

bool logOutput = false;
OptLevel currentOptLevel = OptLevel::NONE;

std::string readFileToString(const std::string &filepath) {
  std::ifstream file(filepath);
  if (!file.is_open())
    throw std::runtime_error("Failed to open file: " + filepath);
  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

void ensureParentDirectoryExists(const fs::path &filePath,
                                 const std::string &logMessage) {
  fs::path parentDir = filePath.parent_path();
  if (!parentDir.empty() && !fs::exists(parentDir)) {
    fs::create_directories(parentDir);
    if (logOutput && !logMessage.empty()) {
      std::cout << COLOR_CYAN << "[INFO] " << logMessage << ": "
                << parentDir.string() << COLOR_RESET << "\n";
    }
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << COLOR_YELLOW << "Usage:" << COLOR_RESET
              << " unnc <source.unn> [options]\n";
    return 1;
  }

  // Help and version
  if (argc == 2) {
    std::string arg = argv[1];
    if (arg == "-help" || arg == "--help") {
      std::cout
          << COLOR_CYAN << "Unnameable Compiler (unnc)" << COLOR_RESET << "\n\n"
          << COLOR_BOLD << "Usage:\n"
          << COLOR_RESET
          << "  unnc <source.unn> [loads] [links] [mode] [options]\n\n"
          << COLOR_BOLD << "Modes:\n"
          << COLOR_RESET << "  -stub <file>          Generate stub file only\n"
          << "  -compile <file>       Compile to object file only\n"
          << "  -build <file>         Compile and link to executable\n"
          << "  -static               Generate a static library\n"
          << "  -link-only            Link only, no compilation\n"
          << "  -check                Run the frontend only\n\n"
          << COLOR_BOLD << "Loads & Links:\n"
          << COLOR_RESET
          << "  -load <stub>          Load a stub file for module resolution\n"
          << "  -link <lib>           Link an external library\n\n"
          << COLOR_BOLD << "Options:\n"
          << COLOR_RESET
          << "  -verbose              Enable verbose internal logs\n"
          << "  -help                 Show this help message\n"
          << "  --version             Show version\n\n"
          << COLOR_BOLD << "Optimization:\n"
          << COLOR_RESET << "  --debug               No optimizations\n"
          << "  --basic               Basic optimizations\n"
          << "  --release             Standard optimizations\n"
          << "  --aggressive          Aggressive optimizations\n"
          << "  --size                Optimize for binary size\n\n"
          << COLOR_YELLOW << "Examples:\n"
          << COLOR_RESET << "  unnc main.unn -build build/app\n"
          << "  unnc main.unn -load stubs/test.stub -build build/app\n"
          << "  unnc main.unn -load stubs/test.stub -link pthread -build "
             "build/app\n"
          << "  unnc -link-only -link obj/main.o -link obj/lib.o -build "
             "build/app\n"
          << "  unnc main.unn -stub stubs/main.stub\n"
          << "  unnc main.unn -compile obj/main.o\n\n";
      return 0;
    }
    if (arg == "--version") {
      std::cout << COLOR_GREEN << "Unnameable Compiler v0.0.0\n" << COLOR_RESET;
      return 0;
    }
  }

  std::string sourceFile;
  std::string objFile;
  std::string exeFile;
  std::string stubFile;
  std::vector<std::string> loads;
  std::vector<std::string> links;
  bool compileOnly = false;
  bool stubOnly = false;
  bool staticCompile = false;
  bool checkOnly = false;
  bool linkOnly = false;

  // Parse arguments
  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "-compile" && i + 1 < argc) {
      objFile = argv[++i];
      if (fs::path(objFile).extension() != ".o")
        objFile += ".o";
      compileOnly = true;
    } else if (arg == "-build" && i + 1 < argc) {
      exeFile = argv[++i];
    } else if (arg == "-stub" && i + 1 < argc) {
      stubFile = argv[++i];
      stubOnly = true;
    } else if (arg == "-load" && i + 1 < argc) {
      loads.push_back(argv[++i]);
    } else if (arg == "-link" && i + 1 < argc) {
      links.push_back(argv[++i]);
    } else if (arg == "-static") {
      staticCompile = true;
    } else if (arg == "-check") {
      checkOnly = true;
    } else if (arg == "-link-only") {
      linkOnly = true;
    } else if (arg == "-verbose") {
      logOutput = true;
    } else if (arg == "--debug") {
      currentOptLevel = OptLevel::NONE;
    } else if (arg == "--basic") {
      currentOptLevel = OptLevel::BASIC;
    } else if (arg == "--release") {
      currentOptLevel = OptLevel::STANDARD;
    } else if (arg == "--aggressive") {
      currentOptLevel = OptLevel::AGGRESSIVE;
    } else if (arg == "--size") {
      currentOptLevel = OptLevel::AGGRESSIVE;
    } else if (arg[0] != '-') {
      sourceFile = arg;
    } else {
      std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
                << " Unknown flag: " << arg << "\n";
      return 1;
    }
  }

  // Pure link mode — no source file needed
  if (linkOnly) {
    if (links.empty()) {
      std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
                << " -link-only requires at least one -link flag\n";
      return 1;
    }
    if (exeFile.empty()) {
      std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
                << " -link-only requires -build <output>\n";
      return 1;
    }
    try {
      fs::path exePath = fs::absolute(exeFile);
      ensureParentDirectoryExists(exePath, "Creating executable directory");
      std::cout << COLOR_YELLOW << "Linking: " << exePath.string()
                << COLOR_RESET << "\n";
      Linker linker(staticCompile);
      std::string mainObj = links[0];
      std::vector<std::string> restLinks(links.begin() + 1, links.end());
      linker.processLinks(mainObj, restLinks, exePath.string());
      std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET
                << " Executable: " << exePath.string() << "\n";
    } catch (const std::exception &e) {
      std::cerr << COLOR_RED << "[FATAL]" << COLOR_RESET << " " << e.what()
                << "\n";
      return 1;
    }
    return 0;
  }

  if (sourceFile.empty()) {
    std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
              << " No source file provided\n";
    return 1;
  }

  fs::path srcPath(sourceFile);

  // Default output paths
  if (objFile.empty())
    objFile = srcPath.stem().string() + ".o";
  if (exeFile.empty())
    exeFile = srcPath.stem().string();
  if (stubFile.empty())
    stubFile = srcPath.stem().string() + ".stub";

  if (logOutput) {
    std::cout << COLOR_BOLD << COLOR_BLUE << "Optimization level: ";
    switch (currentOptLevel) {
    case OptLevel::NONE:
      std::cout << "NONE (--debug)";
      break;
    case OptLevel::BASIC:
      std::cout << "BASIC (--basic)";
      break;
    case OptLevel::STANDARD:
      std::cout << "STANDARD (--release)";
      break;
    case OptLevel::AGGRESSIVE:
      std::cout << "AGGRESSIVE";
      break;
    }
    std::cout << COLOR_RESET << "\n";

    if (!loads.empty()) {
      std::cout << COLOR_BLUE << "Loads:\n" << COLOR_RESET;
      for (const auto &l : loads)
        std::cout << "  " << l << "\n";
    }
    if (!links.empty()) {
      std::cout << COLOR_BLUE << "Links:\n" << COLOR_RESET;
      for (const auto &l : links)
        std::cout << "  " << l << "\n";
    }
  }

  ErrorHandler errorHandler(sourceFile);

  try {
    std::string sourceCode = readFileToString(sourceFile);

    // Lexer
    Lexer lexer(sourceCode, errorHandler);
    lexer.updateTokenList();
    if (lexer.failed())
      return 1;

    if (logOutput) {
      std::cout << COLOR_BOLD << COLOR_BLUE << "Tokens\n" << COLOR_RESET;
      for (const auto &token : lexer.token_list)
        std::cout << COLOR_CYAN << TokenTypeToLiteral(token.type) << COLOR_RESET
                  << " -> \"" << token.TokenLiteral << "\"\n";
    }

    // Parser
    Parser parser(lexer.token_list, errorHandler);
    auto AST = parser.parseProgram();
    if (parser.failed())
      return 1;

    if (logOutput) {
      std::cout << COLOR_BOLD << COLOR_BLUE << "AST\n" << COLOR_RESET;
      for (auto &node : AST)
        std::cout << "Node -> " << node->toString() << "\n";
    }

    // Deserializer
    Deserializer deserial(errorHandler, logOutput);
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Deserializer\n" << COLOR_RESET;
    deserial.processLoads(loads);
    if (deserial.failed())
      return 1;

    // Semantics
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Semantics\n" << COLOR_RESET;
    Semantics semantics(deserial, errorHandler, logOutput);
    for (const auto &node : AST)
      semantics.walker(node.get());
    if (semantics.failed())
      return 1;

    // Auditor
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Auditor\n" << COLOR_RESET;
    Auditor auditor(semantics, errorHandler, logOutput);
    for (const auto &node : AST)
      auditor.runClassifier(node.get());
    for (const auto &node : AST)
      auditor.audit(node.get());
    if (auditor.failed())
      return 1;

    if (checkOnly)
      return 0;

    // Layout
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Layout\n" << COLOR_RESET;
    llvm::LLVMContext llvmContext;
    Layout layout(semantics, llvmContext, errorHandler, logOutput);
    for (const auto &node : AST)
      layout.calculatorDriver(node.get());
    if (layout.failed())
      return 1;

    // Stub generation
    if (stubOnly) {
      ensureParentDirectoryExists(stubFile, "Creating stub directory");
      if (logOutput)
        std::cout << COLOR_BOLD << COLOR_BLUE << "Stub\n" << COLOR_RESET;
      StubGen stubGen(semantics, stubFile, logOutput);
      for (const auto &node : AST)
        stubGen.stubGenerator(node.get());
      stubGen.finish();
      if (stubGen.failed())
        return 1;
      std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET
                << " Stub generated: " << stubFile << "\n";
      return 0;
    }

    // IR generation
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "IR\n" << COLOR_RESET;
    IRGenerator irgen(semantics, errorHandler, auditor, layout.totalHeapSize,
                      logOutput, currentOptLevel);
    irgen.generate(AST);
    if (logOutput)
      irgen.dumpIR();

    // Emit object
    fs::path objPath = fs::absolute(objFile);
    ensureParentDirectoryExists(objPath, "Creating object directory");
    std::cout << COLOR_YELLOW << "Generating object: " << objPath.string()
              << COLOR_RESET << "\n";
    if (!irgen.emitObjectFile(objPath.string())) {
      std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
                << " Failed to emit object: " << objPath.string() << "\n";
      return 1;
    }

    if (compileOnly) {
      std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET
                << " Object generated: " << objPath.string() << "\n";
      return 0;
    }

    // Link
    fs::path exePath = fs::absolute(exeFile);
    ensureParentDirectoryExists(exePath, "Creating executable directory");
    std::cout << COLOR_YELLOW << "Linking: " << exePath.string() << COLOR_RESET
              << "\n";
    Linker linker(staticCompile);
    linker.processLinks(objPath.string(), links, exePath.string());
    std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET
              << " Executable: " << exePath.string() << "\n";

  } catch (const std::exception &e) {
    std::cerr << COLOR_RED << "[FATAL]" << COLOR_RESET << " " << e.what()
              << "\n";
    return 1;
  }

  return 0;
}
