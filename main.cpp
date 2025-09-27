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
#include <llvm/IR/LLVMContext.h>

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

    try
    {
        std::string code = readFileToString(filepath);

        Lexer lexer(code);
        lexer.updateTokenList();
        std::vector<Token> tokens = lexer.token_list;

        std::cout << "\n------Tokens---------\n";
        for (const auto &token : tokens)
        {
            std::cout << "  Type: " << TokenTypeToLiteral(token.type)
                      << ", Literal: \"" << token.TokenLiteral << "\"\n";
        }

        Parser parser(tokens);

        std::vector<std::unique_ptr<Node>> nodes = parser.parseProgram();

        std::cout << "\n--- AST ---\n";
        for (const auto &node : nodes)
        {
            std::cout << " Node ->  " << node->toString() << "\n";
        }

        std::cout << "\n--- Semantic Analysis ---\n";
        Semantics semantics;
        for (const auto &node : nodes)
        {
            semantics.walker(node.get());
        }
        std::cout << "\n----Layout analysis------\n";
        llvm::LLVMContext llvmContext;
        Layout layout(semantics, llvmContext);
        for (const auto &node : nodes)
        {
            layout.calculatorDriver(node.get());
        }
        std::cout << "\n--- LLVM IR Generation ---\n";
        IRGenerator irgen(semantics, layout.totalHeapSize);
        irgen.generate(nodes); // <--- pass vector of nodes
        irgen.dumpIR();        // Print the IR

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
    }
    catch (const std::exception &e)
    {
        std::cerr << "[FATAL] " << e.what() << "\n";
        return 1;
    }

    return 0;
}
