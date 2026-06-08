#pragma once
#include "dollar_commons.hpp"
#include "parser.hpp"
#include "token.hpp"
#include <vector>

class DBIRLexer {
public:
  std::vector<Token> &translateIR(Code &code);

private:
  std::vector<Token> tokens;
};

class Synthesizer {
public:
  Node *synthesizeNode(Code &code);

private:
  DBIRLexer lexer;
  Parser parser;
};
