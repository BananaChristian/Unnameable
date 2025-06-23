#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include "lexer/lexer.hpp"
#include "token/token.hpp"
#include "parser/parser.hpp"

using namespace std;

int main() {
    string input_line;

    cout << "Iron is running (type 'exit' to quit)" << endl;

    while (true) {
        cout << ">> ";

        if (!getline(cin, input_line)) {
            cout << "\nExiting Iron REPL." << endl;
            break;
        }

        if (input_line == "exit") {
            cout << "Exiting Iron REPL." << endl;
            break;
        }

        Lexer lexer(input_line);
        lexer.updateTokenList(); 
        vector<Token> tokens = lexer.token_list;

        cout << "\n--- Tokens ---\n";
        for (const auto& token : tokens) {
            cout << "  Type: " << TokenTypeToLiteral(token.type)
                 << ", Literal: \"" << token.TokenLiteral << "\"\n";
        }

        Parser parser(tokens);
        vector<unique_ptr<Node>> nodes = parser.parseProgram();

        cout << "\n--- AST ---\n";
        for (const auto& node : nodes) {
            cout << " Node ->  " << node->toString() << endl;
        }

        cout << "\n";
    }

    return 0;
}
