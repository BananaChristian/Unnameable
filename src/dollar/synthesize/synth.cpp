#include "synthesizer.hpp"

// The synthesizer's driver
Node *Synthesizer::synthesizeNode(Code &code) {
  auto tokens = lexer.translateIR(code);
  auto AST = parser.parseProgram();

}
