#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <fstream>
#include <sstream>
#include <unordered_set>
#include <filesystem>
#include <algorithm>
#include <llvm/IR/LLVMContext.h>

#include "lexer/lexer.hpp"
#include "token/token.hpp"
#include "parser/parser.hpp"
#include "semantics/semantics.hpp"
#include "irgen/irgen.hpp"
#include "layout/layout.hpp"
#include "sentinel/sentinel.hpp"
#include "stubgen/stubgen.hpp"

namespace fs = std::filesystem;

// COLORS
#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

std::string fileName;
bool logOutput = false;

struct CompilationUnit
{
    std::vector<std::shared_ptr<FileUnit>> files;
    std::vector<std::unique_ptr<Node>> mergedNodes;
};

std::string readFileToString(const std::string &filepath)
{
    std::ifstream file(filepath);
    if (!file.is_open())
        throw std::runtime_error("Failed to open file: " + filepath);

    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

std::string resolveImportPath(const std::string &currentFile, const std::string &importString)
{
    fs::path currentDir = fs::path(currentFile).parent_path();
    fs::path importPath(importString);

    if (importPath.extension() != ".unn")
        importPath += ".unn";

    fs::path resolved = fs::weakly_canonical(currentDir / importPath);
    fs::path projectRoot = fs::current_path();

    if (resolved.string().rfind(projectRoot.string(), 0) != 0)
        throw std::runtime_error("[MERGE ERROR] File '" + resolved.string() +
                                 "' climbs out of project directory.");
    if (!fs::exists(resolved))
        throw std::runtime_error("[MERGE ERROR] Merged file not found: " + resolved.string());

    return resolved.string();
}

std::shared_ptr<FileUnit> loadFileRecursive(
    const std::string &path,
    CompilationUnit &cu,
    std::unordered_set<std::string> &visited,
    std::vector<std::string> &stack)
{
    fs::path canonical = fs::weakly_canonical(fs::path(path));
    std::string canon_str = canonical.string();

    fileName = canon_str;

    if (std::find(stack.begin(), stack.end(), canon_str) != stack.end())
        throw std::runtime_error("[MERGE ERROR] Circular import detected at " + canon_str);

    if (visited.count(canon_str))
        return nullptr;

    stack.push_back(canon_str);
    std::string code = readFileToString(canon_str);

    Lexer lexer(code, fileName);
    lexer.updateTokenList();

    if (logOutput)
    {
        std::cout << COLOR_BLUE << "[LEXICAL ANALYSIS]" << COLOR_RESET << "\n";
        for (const auto &token : lexer.token_list)
        {
            std::cout << COLOR_CYAN << "Token: " << COLOR_RESET
                      << TokenTypeToLiteral(token.type)
                      << ", Literal: \"" << token.TokenLiteral << "\"\n";
        }
    }

    Parser parser(lexer.token_list, fileName);
    auto fileUnit = parser.generateFileUnit();
    fileUnit->fileName = canon_str;

    for (const auto &imp : fileUnit->mergers)
    {
        std::string resolved = resolveImportPath(canon_str, imp);
        if (resolved == canon_str)
            throw std::runtime_error("[MERGE ERROR] File merges with itself: " + canon_str);
        loadFileRecursive(resolved, cu, visited, stack);
    }

    cu.files.push_back(fileUnit);
    visited.insert(canon_str);
    stack.pop_back();

    return fileUnit;
}

// Locate compiler root (parent of /bin directory)
fs::path getCompilerRoot()
{
    try
    {
        fs::path exePath = fs::canonical("/proc/self/exe");
        return exePath.parent_path().parent_path(); // /bin/unnc -> root/
    }
    catch (...)
    {
        throw std::runtime_error("[FATAL] Unable to determine compiler root directory");
    }
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << COLOR_YELLOW << "Usage:" << COLOR_RESET
                  << " unnc <source.unn> [-c <object>] [-o <executable>] [-verbose]\n";
        return 1;
    }

    // Handle help and version early
    if (argc == 2)
    {
        std::string arg = argv[1];
        if (arg == "-help" || arg == "--help")
        {
            std::cout << COLOR_CYAN << "Unnameable Compiler (unnc)" << COLOR_RESET << "\n\n"
                      << COLOR_BOLD << "Usage:\n"
                      << COLOR_RESET
                      << "  unnc <source.unn> [options]\n\n"
                      << COLOR_BOLD << "Options:\n"
                      << COLOR_RESET
                      << "  -c <file>       Compile to object file only\n"
                      << "  -o <file>       Compile and link to executable\n"
                      << "  -verbose        Enable verbose internal logs\n"
                      << "  -help           Show this help message\n"
                      << "  --version       Show compiler version\n\n"
                      << COLOR_YELLOW << "Example:\n"
                      << COLOR_RESET
                      << "  unnc main.unn -o app\n"
                      << "  unnc main.unn -c main.o\n\n";
            return 0;
        }
        if (arg == "--version")
        {
            std::cout << COLOR_GREEN << "Unnameable Compiler v0.0.0\n"
                      << COLOR_RESET << "\n";
            return 0;
        }
    }

    std::string sourceFile;
    std::string objFile;
    std::string exeFile;
    bool compileOnly = false;

    // Parse arguments
    for (int i = 1; i < argc; ++i)
    {
        std::string arg = argv[i];
        if (arg == "-c" && i + 1 < argc)
        {
            objFile = argv[++i];
            compileOnly = true;
        }
        else if (arg == "-o" && i + 1 < argc)
        {
            exeFile = argv[++i];
        }
        else if (arg == "-verbose")
        {
            logOutput = true;
        }
        else if (arg[0] != '-')
        {
            sourceFile = arg;
        }
        else
        {
            std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET << " Unknown flag: " << arg << "\n";
            return 1;
        }
    }

    if (sourceFile.empty())
    {
        std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET << " No source file provided\n";
        return 1;
    }

    fs::path srcPath(sourceFile);
    if (objFile.empty())
        objFile = srcPath.stem().string() + ".o";
    if (exeFile.empty())
        exeFile = srcPath.stem().string();

    CompilationUnit cu;
    std::unordered_set<std::string> visited;

    try
    {
        std::vector<std::string> stack;
        loadFileRecursive(sourceFile, cu, visited, stack);

        if (logOutput)
            std::cout << COLOR_BLUE << "[AST GENERATION]" << COLOR_RESET << "\n";

        for (auto &fu : cu.files)
            for (auto &node : fu->nodes)
            {
                if (logOutput)
                {
                    std::cout << "Node->" << node->toString() << " \n";
                }
                cu.mergedNodes.push_back(std::move(node));
            }

        if (logOutput)
            std::cout << COLOR_BLUE << "[SEMANTIC ANALYSIS]" << COLOR_RESET << "\n";
        Semantics semantics(fileName);
        for (const auto &node : cu.mergedNodes)
            semantics.walker(node.get());

        if (logOutput)
            std::cout << COLOR_BLUE << "[LAYOUT ANALYSIS]" << COLOR_RESET << "\n";
        llvm::LLVMContext llvmContext;
        Layout layout(semantics, llvmContext);
        for (const auto &node : cu.mergedNodes)
            layout.calculatorDriver(node.get());

        if (logOutput)
            std::cout << COLOR_BLUE << "[SENTINEL ANALYSIS]" << COLOR_RESET << "\n";
        Sentinel sentinel(semantics);
        for (const auto &node : cu.mergedNodes)
            sentinel.sentinelDriver(node.get());

        StubGen stubGen(semantics,fileName);
        if (compileOnly)
        {
            if (logOutput)
                std::cout << COLOR_BLUE << "[STUBGEN]" << COLOR_RESET << "\n";
            for (const auto &node : cu.mergedNodes)
                stubGen.stubGenerator(node.get());

            stubGen.finish();
        }

        if (logOutput)
            std::cout << COLOR_BLUE << "[IR GENERATION]" << COLOR_RESET << "\n";
        IRGenerator irgen(semantics, layout.totalHeapSize);
        irgen.generate(cu.mergedNodes);
        if (logOutput)
            irgen.dumpIR();

        // Emit Object
        fs::path objPath = fs::absolute(objFile);
        std::cout << COLOR_YELLOW << "\nGenerating object file: " << objPath.string() << COLOR_RESET << "\n";
        if (!irgen.emitObjectFile(objPath.string()))
        {
            std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET << " Failed to generate object file: " << objPath.string() << "\n";
            return 1;
        }

        if (compileOnly)
        {
            std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET << " Object file generated: " << objPath.string() << "\n";
            return 0;
        }

        // Link Executable
        fs::path exePath = fs::absolute(exeFile);
        fs::path root = getCompilerRoot();
        fs::path sagePath = root / "runtime" / "sage.o";
        fs::path helperPath = root / "runtime" / "helper.o";

        std::cout << COLOR_YELLOW << "\nLinking executable: " << exePath.string() << COLOR_RESET << "\n";

        std::string linkCmd = "g++ -o \"" + exePath.string() + "\" \"" + objPath.string() +
                              "\" \"" + sagePath.string() + "\" \"" + helperPath.string() + "\"";

        int ret = std::system(linkCmd.c_str());
        if (ret != 0)
        {
            std::cerr << COLOR_RED << "[ERROR]" << COLOR_RESET << " Linking failed!\n";
            return 1;
        }

        std::cout << COLOR_GREEN << "[SUCCESS]" << COLOR_RESET << " Executable generated: " << exePath.string() << "\n";
    }
    catch (const std::exception &e)
    {
        std::cerr << COLOR_RED << "[FATAL]" << COLOR_RESET << " " << e.what() << "\n";
        return 1;
    }

    return 0;
}
