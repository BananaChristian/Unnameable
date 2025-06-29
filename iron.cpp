#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include "lexer/lexer.hpp"
#include "token/token.hpp"
#include "parser/parser.hpp"

int main()
{
    std::cout << "Iron is running (type 'done' to compile, 'exit' to quit)" << endl;

    std::vector<std::string> input_lines;

    while (true)
    {
        cout << ">> ";
        std::string input_line;

        if (!getline(cin, input_line))
        {
            std::cout << "\nExiting Iron REPL." << endl;
            break;
        }

        if (input_line == "exit")
        {
            std::cout << "Exiting Iron REPL." << endl;
            break;
        }

        if (input_line == "done")
        {
            std::string full_input;
            for (const auto &line : input_lines)
            {
                full_input += line + "\n";
            }
            input_lines.clear();
            Lexer lexer(full_input);
            lexer.updateTokenList();
            std::vector<Token> tokens = lexer.token_list;

            std::cout << "\n--- Tokens ---\n";
            for (const auto &token : tokens)
            {
                cout << "  Type: " << TokenTypeToLiteral(token.type)
                     << ", Literal: \"" << token.TokenLiteral << "\"\n";
            }

            Parser parser(tokens);
            std::vector<std::unique_ptr<Node>> nodes = parser.parseProgram();

            std::cout << "\n--- AST ---\n";
            for (const auto &node : nodes)
            {
                std::cout << " Node ->  " << node->toString() << endl;
            }

            std::cout << "\n";
            continue;
        }

        input_lines.push_back(input_line);
    }

    return 0;
}
