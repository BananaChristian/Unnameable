#include <iostream>
#include <fstream>
#include <sstream>
#include "lexer.hpp"
#include "parser.hpp"
#include "token.hpp"

// COLORS
#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

std::string readFileToString(const std::string &filepath) {
    std::ifstream file(filepath);
    if (!file.is_open())
        throw std::runtime_error("Failed to open file: " + filepath);
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << "usage: unnc_test <file.unn>\n";
        return 1;
    }

    ErrorHandler errorHandler(argv[1]);

    try {
        std::string source = readFileToString(argv[1]);

        // Lexer
        Lexer lexer(source, errorHandler);
        lexer.updateTokenList();

        if (lexer.failed()) {
            std::cerr << COLOR_RED << "Lexer failed\n" << COLOR_RESET;
            return 1;
        }

        std::cout << COLOR_BOLD << COLOR_BLUE << "Tokens:\n" << COLOR_RESET;
        for (const auto &token : lexer.token_list) {
            std::cout << COLOR_CYAN << TokenTypeToLiteral(token.type) 
                      << COLOR_RESET << " -> \"" << token.TokenLiteral << "\"\n";
        }

        // Parser
        Parser parser(lexer.token_list, errorHandler);
        auto AST = parser.parseProgram();

        if (parser.failed()) {
            std::cerr << COLOR_RED << "Parser failed\n" << COLOR_RESET;
            return 1;
        }

        std::cout << COLOR_BOLD << COLOR_GREEN << "\nAST:\n" << COLOR_RESET;
        for (auto &node : AST) {
            std::cout << node->toString() << "\n";
        }

        std::cout << COLOR_GREEN << "\n[SUCCESS] Frontend passed\n" << COLOR_RESET;

    } catch (const std::exception &e) {
        std::cerr << COLOR_RED << "[FATAL] " << e.what() << "\n" << COLOR_RESET;
        return 1;
    }

    return 0;
}
