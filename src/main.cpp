#include <filesystem>
#include <fstream>
#include <iostream>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <sstream>
#include <string>

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

// COLORS
#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

bool logOutput = false;

std::string readFileToString(const std::string &filepath) {
  std::ifstream file(filepath);
  if (!file.is_open())
    throw std::runtime_error("Failed to open file: " + filepath);

  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

// Locate compiler root (parent of /bin directory)
fs::path getCompilerRoot() {
  try {
    fs::path exePath = fs::canonical("/proc/self/exe");
    return exePath.parent_path().parent_path(); // /bin/unnc -> root/
  } catch (...) {
    throw std::runtime_error(
        "[FATAL] Unable to determine compiler root directory");
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << COLOR_YELLOW << "Usage:" << COLOR_RESET
              << " unnc <source.unn> [-compile <object>] [-build <executable>] "
                 "[-verbose]\n";
    return 1;
  }

  // Handle help and version early
  if (argc == 2) {
    std::string arg = argv[1];
    if (arg == "-help" || arg == "--help") {
      std::cout << COLOR_CYAN << "Unnameable Compiler (unnc)" << COLOR_RESET
                << "\n\n"
                << COLOR_BOLD << "Usage:\n"
                << COLOR_RESET << "  unnc <source.unn> [options]\n\n"
                << COLOR_BOLD << "Options:\n"
                << COLOR_RESET
                << "  -compile <file>       Compile to object file only\n"
                << "  -build <file>       Compile and link to executable\n"
                << "  -stub <file>      Generate a stub file\n"
                << "  -verbose        Enable verbose internal logs\n"
                << "  -check <file>   Just run the front end\n"
                << "  -help           Show this help message\n"
                << "  -static          Generate a static library instead of an "
                   "executable\n"
                << "  --version       Show compiler version\n\n"
                << COLOR_YELLOW << "Example:\n"
                << COLOR_RESET << "  unnc main.unn -build app\n"
                << "  unnc main.unn -compile main.o\n\n";
      return 0;
    }
    if (arg == "--version") {
      std::cout << COLOR_GREEN << "Unnameable Compiler v0.0.0\n"
                << COLOR_RESET << "\n";
      return 0;
    }
  }

  // Get the absolute path of the file we are TRYING to compile right now
  fs::path currentArgPath = (argc > 1) ? fs::absolute(argv[1]) : "";

  // Check the "Bloodline"
  const char *env_stack = std::getenv("UNNC_IMPORT_STACK");
  std::string importStack = env_stack ? env_stack : "";

  if (!importStack.empty() && !currentArgPath.empty()) {
    std::stringstream ss(importStack);
    std::string modulePath;

    while (std::getline(ss, modulePath, ',')) {
      // Both are now absolute paths. Compare apples to apples.
      if (currentArgPath == fs::path(modulePath)) {
        std::cerr << COLOR_RED << COLOR_BOLD
                  << "\n[FATAL] Circular Import Detected!\n"
                  << COLOR_RESET << COLOR_YELLOW << "Trace: " << importStack
                  << " -> " << COLOR_RED << currentArgPath.string()
                  << COLOR_RESET << "\n";
        return 1;
      }
    }
  }

  std::string sourceFile;
  std::string objFile;
  std::string exeFile;
  std::string stubFile;
  bool compileOnly = false;
  bool stubOnly = false;
  bool staticCompile = false; // Boolean flag for static libgen
  bool checkOnly = false;

  // Parse arguments
  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "-compile" && i + 1 < argc) {
      objFile = argv[++i];
      // .o extension if the user forgot it
      if (fs::path(objFile).extension() != ".o") {
        objFile += ".o";
      }
      compileOnly = true;
    } else if (arg == "-static") {
      staticCompile = true;
    } else if (arg == "-check") {
      checkOnly = true;
    } else if (arg == "-build" && i + 1 < argc) {
      exeFile = argv[++i];
    } else if (arg == "-stub") {
      // Check if a filename was provided after -stub, or if the next arg is
      // another flag
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        stubFile = argv[++i];
      }
      stubOnly = true;
    } else if (arg == "-verbose") {
      logOutput = true;
    } else if (arg[0] != '-') {
      sourceFile = arg;
    } else {
      std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
                << " Unknown flag: " << arg << "\n";
      return 1;
    }
  }

  if (sourceFile.empty()) {
    std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
              << " No source file provided\n";
    return 1;
  }

  fs::path srcPath(sourceFile);
  if (objFile.empty())
    objFile = srcPath.stem().string() + ".o";
  if (exeFile.empty())
    exeFile = srcPath.stem().string();
  if (stubFile.empty())
    stubFile = srcPath.stem().string() + ".stub";

  ErrorHandler errorHandler(sourceFile);

  try {
    std::string sourceCode = readFileToString(sourceFile);

    // Lexer phase
    Lexer lexer(sourceCode, errorHandler);
    lexer.updateTokenList();

    if (lexer.failed())
      return 1;

    if (logOutput) {
      std::cout << COLOR_BOLD << COLOR_BLUE << "Tokeninizing tokens ..."
                << COLOR_RESET << "\n";
      for (const auto &token : lexer.token_list) {
        std::cout << COLOR_CYAN << "Token: " << COLOR_RESET
                  << TokenTypeToLiteral(token.type) << ", Literal: \""
                  << token.TokenLiteral << "\"\n";
      }
    }

    // Parser phase
    Parser parser(lexer.token_list, errorHandler);
    auto AST = parser.parseProgram();

    if (parser.failed())
      return 1;

    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Generating AST..."
                << COLOR_RESET << "\n";

    for (auto &node : AST) {
      if (logOutput) {
        std::cout << "Node->" << node->toString() << " \n";
      }
    }

    // Deserializer phase
    Deserializer deserial(errorHandler, logOutput);
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Deserializing stub.."
                << COLOR_RESET << "\n";
    deserial.processImports(AST, sourceFile);

    if (deserial.failed()) {
      return 1;
    }

    // SEMANTICS PASS
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Analyzing semantics..."
                << COLOR_RESET << "\n";
    Semantics semantics(deserial, errorHandler, logOutput);
    for (const auto &node : AST)
      semantics.walker(node.get());

    if (semantics.failed()) {
      return 1;
    }

    // BATON AUDITOR PASS
    if (logOutput)
      std::cout << COLOR_BOLD << COLOR_BLUE << "Auditing baton state..."
                << COLOR_RESET << "\n";
    Auditor auditor(semantics, errorHandler, logOutput);
    for (const auto &node : AST)
      auditor.audit(node.get());

    if (auditor.failed()) {
      return 1;
    }

    // LAYOUT PASS
    if (!checkOnly) {
      if (logOutput)
        std::cout << COLOR_BOLD << COLOR_BLUE << "Calculating layout..."
                  << COLOR_RESET << "\n";
      llvm::LLVMContext llvmContext;
      Layout layout(semantics, llvmContext, errorHandler, logOutput);
      for (const auto &node : AST)
        layout.calculatorDriver(node.get());

      if (layout.failed()) {
        return 1;
      }

      // STUB GENERATION
      StubGen stubGen(semantics, stubFile, logOutput);
      if (compileOnly || stubOnly) {
        if (logOutput)
          std::cout << COLOR_BLUE << "Generating stub ..." << COLOR_RESET
                    << "\n";
        for (const auto &node : AST)
          stubGen.stubGenerator(node.get());

        stubGen.finish();

        if (stubGen.failed()) {
          return 1;
        }

        // If the user ONLY wanted a stub, we stop here
        if (stubOnly && !compileOnly) {
          std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET
                    << " Interface stub generated: " << stubFile << "\n";
          return 0;
        }
      }

      // IR GENERATION
      if (logOutput)
        std::cout << COLOR_BOLD << COLOR_BLUE << "Generating IR..."
                  << COLOR_RESET << "\n";
      IRGenerator irgen(semantics, errorHandler, auditor, layout.totalHeapSize,
                        logOutput);
      irgen.generate(AST);
      if (logOutput)
        irgen.dumpIR();

      // Emit Object
      fs::path objPath = fs::absolute(objFile);
      std::cout << COLOR_YELLOW
                << "\nGenerating object file: " << objPath.string()
                << COLOR_RESET << "\n";
      if (!irgen.emitObjectFile(objPath.string())) {
        std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET
                  << " Failed to generate object file: " << objPath.string()
                  << "\n";
        return 1;
      }

      if (compileOnly) {
        std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET
                  << " Object file generated: " << objPath.string() << "\n";
        return 0;
      }

      // Link Executable
      fs::path exePath = fs::absolute(exeFile);
      std::cout << COLOR_YELLOW << "\nLinking executable: " << exePath.string()
                << COLOR_RESET << "\n";

      Linker linker(deserial, objPath.string(), staticCompile);
      linker.processLinks(AST, sourceFile, exePath.string());

      std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET
                << " Executable generated: " << exePath.string() << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << COLOR_RED << "[FATAL]" << COLOR_RESET << " " << e.what()
              << "\n";
    return 1;
  }

  return 0;
}
