#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <fstream>
#include <sstream>
#include "lexer/lexer.hpp"
#include "token/token.hpp"
#include "parser/parser.hpp"
#include "semantics/semantics.hpp"
#include "irgen/irgen.hpp"
#include "layout/layout.hpp"
#include "sentinel/sentinel.hpp"
#include <unordered_set>
#include <llvm/IR/LLVMContext.h>
#include <filesystem>
namespace fs = std::filesystem;

struct CompilationUnit
{
    std::vector<std::shared_ptr<FileUnit>> files;
    std::vector<std::unique_ptr<Node>> mergedNodes;
};

std::string readFileToString(const std::string &filepath)
{
    std::ifstream file(filepath);
    if (!file.is_open())
    {
        throw std::runtime_error("Failed to open file: " + filepath);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

#include <filesystem>
namespace fs = std::filesystem;

std::string resolveImportPath(const std::string &currentFile, const std::string &importString)
{
    fs::path currentDir = fs::path(currentFile).parent_path();

    fs::path importPath(importString);

    // Append .unn if user didn't include it
    if (importPath.extension() != ".unn")
        importPath += ".unn";

    fs::path resolved = fs::weakly_canonical(currentDir / importPath);

    fs::path projectRoot = fs::current_path();

    // Prevent climbing out of project tree
    if (resolved.string().rfind(projectRoot.string(), 0) != 0)
    {
        throw std::runtime_error(
            "[IMPORT ERROR] File '" + resolved.string() +
            "' climbs out of project directory.");
    }

    // Check file existence
    if (!fs::exists(resolved))
    {
        throw std::runtime_error("[IMPORT ERROR] Imported file not found: " + resolved.string());
    }

    return resolved.string();
}

std::shared_ptr<FileUnit> loadFileRecursive(
    const std::string &path,
    CompilationUnit &cu,
    std::unordered_set<std::string> &visited,
    std::vector<std::string> &stack)
{
    namespace fs = std::filesystem;

    // Canonicalize the path we were given
    fs::path canonical = fs::weakly_canonical(fs::path(path));
    std::string canon_str = canonical.string();

    // If this file is already on the stack, we found a cycle (including self-import)
    auto it_on_stack = std::find(stack.begin(), stack.end(), canon_str);
    if (it_on_stack != stack.end())
    {
        // Build a cycle message
        std::ostringstream oss;
        oss << "[IMPORT ERROR] circular import detected: ";
        for (auto it = it_on_stack; it != stack.end(); ++it)
            oss << *it << " -> ";
        oss << canon_str; // complete the cycle
        throw std::runtime_error(oss.str());
    }

    // If already fully visited (parsed earlier), we can skip re-parsing.
    if (visited.count(canon_str))
    {
        return nullptr;
    }

    // Mark as "visiting"
    stack.push_back(canon_str);

    // Read file and parse
    std::string code = readFileToString(canon_str);

    Lexer lexer(code);
    lexer.updateTokenList();

    // Debug: print tokens
    for (const auto &token : lexer.token_list)
    {
        std::cout << "Token: " << TokenTypeToLiteral(token.type)
                  << ", Literal: \"" << token.TokenLiteral << "\"\n";
    }

    Parser parser(lexer.token_list);
    auto fileUnit = parser.generateFileUnit();
    fileUnit->fileName = canon_str;

    // For each import string in this file, resolve path and recurse
    for (const auto &imp : fileUnit->imports)
    {
        // Resolve import relative to this file
        std::string resolved = resolveImportPath(canon_str, imp);

        // If resolved == canonical current file, that's a self-import -> error
        if (resolved == canon_str)
        {
            std::ostringstream oss;
            oss << "[IMPORT ERROR] file imports itself: " << canon_str;
            throw std::runtime_error(oss.str());
        }

        // If resolved is already on the stack, that will be detected at the top of the call,
        // but we can also pre-check here for a friendlier message:
        if (std::find(stack.begin(), stack.end(), resolved) != stack.end())
        {
            std::ostringstream oss;
            oss << "[IMPORT ERROR] circular import detected: ";
            for (const auto &p : stack)
                oss << p << " -> ";
            oss << resolved;
            throw std::runtime_error(oss.str());
        }

        // Recurse
        loadFileRecursive(resolved, cu, visited, stack);
    }
    cu.files.push_back(fileUnit);

    // Mark this file as fully visited (parsed)
    visited.insert(canon_str);

    // Done visiting this file; pop from stack
    stack.pop_back();

    return fileUnit;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: unnc <source-file.unn>\n";
        return 1;
    }

    std::string filepath = argv[1];

    if (filepath.substr(filepath.find_last_of('.') + 1) != "unn")
    {
        std::cerr << "[WARNING] File doesn't have .unn extension. Continuing anyway...\n";
    }

    CompilationUnit cu;
    std::unordered_set<std::string> visited;

    try
    {
        std::vector<std::string> stack;
        std::shared_ptr<FileUnit> rootFile = loadFileRecursive(filepath, cu, visited, stack);

        std::cout << "---AST generation---\n";
        for (auto &fu : cu.files)
        {
            for (auto &node : fu->nodes)
            {
                std::cout <<"Node ->"<< node->toString() << "\n";
                cu.mergedNodes.push_back(std::move(node));
            }
        }

        std::cout << "\n--- Semantic Analysis ---\n";
        Semantics semantics;
        for (const auto &node : cu.mergedNodes)
        {
            semantics.walker(node.get());
        }
        std::cout << "\n----Layout analysis------\n";
        llvm::LLVMContext llvmContext;
        Layout layout(semantics, llvmContext);
        for (const auto &node : cu.mergedNodes)
        {
            layout.calculatorDriver(node.get());
        }
        std::cout << "\n----Sentinel analysis------\n";
        Sentinel sentinel(semantics);
        for (const auto &node : cu.mergedNodes)
        {
            sentinel.sentinelDriver(node.get());
        }
        std::cout << "\n--- LLVM IR Generation ---\n";
        IRGenerator irgen(semantics, layout.totalHeapSize);
        irgen.generate(cu.mergedNodes); // <--- pass vector of nodes
        irgen.dumpIR();                 // Print the IR

        //----Object File Emission----//
        const std::string objFile = "test.o";
        std::cout << "\n--- Generating object file: " << objFile << " ---\n";
        if (irgen.emitObjectFile(objFile))
        {
            std::cout << "[SUCCESS] Object file generated: " << objFile << "\n";
        }
        else
        {
            std::cerr << "[ERROR] Failed to generate object file.\n";
            return 1;
        }

        std::string exeFile = "program";
        std::string linkCmd = "g++ -o " + exeFile + " " + objFile + " sage.o";
        int ret = std::system(linkCmd.c_str());
        if (ret != 0)
        {
            std::cerr << "[ERROR] Linking failed with ld!\n";
            return 1;
        }
        else
        {
            std::cout << "[SUCCESS] Executable generated: " << exeFile << "\n";
        }
    }
    catch (const std::exception &e)
    {
        std::cerr << "[FATAL] " << e.what() << "\n";
        return 1;
    }

    return 0;
}
