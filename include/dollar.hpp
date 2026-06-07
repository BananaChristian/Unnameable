#pragma once
#include "ast.hpp"
#include "defs.hpp"
#include "semantics.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

enum Opcode {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,

  EQ,
  NEQ,
  LT,
  GT,
  LTE,
  GTE,

  AND,
  OR,
  NOT,

  JMP,
  JMP_IF,
  RET,

  MOV,
  PHI,

  EMIT,
  CALL,
  CONST,
};

struct Register {
  int id = 0;
};

using Value = std::variant<int64_t, double, bool, std::string, Node *>;

struct Instruction {
  Opcode opcode;                 // Crap like add
  Register result;               // If no result -1
  std::vector<Register> sources; // Source registers
};

struct Block {
  std::string label;
  std::vector<Instruction> instructions;
};

class VirtualMachine {
  std::unordered_map<int, Register> register_map;

private:
  Register new_register();
  void set();
  Value get();
};

// Dollar bill's manager
class DollarBill {
public:
  DollarBill(
      std::vector<std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>>
          symbolTable);
  Node *emit(Node *macroNode); // This is what the walkers interface with feed
                               // it the macroNode and get the evaluatedNode

private:
  std::vector<std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>>
      DBsymbolTable; //A copy of the semantics that called's symbol table

  Semantics semantics; // This is the semantics that will be called internally
                       // by dollar bill
  VirtualMachine machine; // This is the guy who will create the Node we want

  // Helpers
};
