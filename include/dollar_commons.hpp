#pragma  once
#include <vector>
#include "ast.hpp"

enum OpcodeType {
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

  GARBAGE, // Special instruction for failed crap maybe
};

struct Opcode {
  OpcodeType type;
  std::string literal;

  // --- Arithmetic Factories ---
  static Opcode add() { return {ADD, "ADD"}; }
  static Opcode sub() { return {SUB, "SUB"}; }
  static Opcode mul() { return {MUL, "MUL"}; }
  static Opcode div() { return {DIV, "DIV"}; }
  static Opcode mod() { return {MOD, "MOD"}; }

  // --- Comparison Factories ---
  static Opcode eq() { return {EQ, "EQ"}; }
  static Opcode neq() { return {NEQ, "NEQ"}; }
  static Opcode lt() { return {LT, "LT"}; }
  static Opcode gt() { return {GT, "GT"}; }
  static Opcode lte() { return {LTE, "LTE"}; }
  static Opcode gte() { return {GTE, "GTE"}; }

  // --- Logical Factories ---
  static Opcode log_and() { return {AND, "AND"}; }
  static Opcode log_or() { return {OR, "OR"}; }
  static Opcode log_not() { return {NOT, "NOT"}; }

  // --- Control Flow Factories ---
  static Opcode jmp() { return {JMP, "JMP"}; }
  static Opcode jmp_if() { return {JMP_IF, "JMP_IF"}; }
  static Opcode ret() { return {RET, "RET"}; }

  // --- Data Movement / SSA Factories ---
  static Opcode mov() { return {MOV, "MOV"}; }
  static Opcode phi() { return {PHI, "PHI"}; }

  // --- Compiler Interface / Special Factories ---
  static Opcode emit() { return {EMIT, "EMIT"}; }
  static Opcode call() { return {CALL, "CALL"}; }
  static Opcode const_val() {
    return {CONST, "CONST"};
  } // Loads immediate compile-time constants

  // Special garbage instruction for failure
  static Opcode garbage() { return {GARBAGE, "GARBAGE"}; }
};

struct Register {
  int id = 0;

  bool is_failed() { return id == -1; }
  static Register failed() { return Register{-1}; }
};

enum Primitive {
  SCALAR,
  BOOLEAN,
  FLOAT,
  ADDRESS,
};

struct Value {
  Node *node;          // This is the actual node behind it
  Primitive primitive; // This is what the VM actually deals in
};

struct Instruction {
  Register result;               // Where we wanna place the result
  Opcode opcode;                 // Crap like ADD,JMP
  std::vector<Register> sources; // Source registers

  // For CONST
  Value intermediate;

  // For JMP
  std::string jump_label;

  // For JMP_IF
  std::string true_label;
  std::string false_label;

  // for PHI
  std::vector<std::pair<std::string, Register>> phi_sources;

  static Instruction garbage() {
    return {.result = Register::failed(), .opcode = Opcode::garbage()};
  }
};

struct Block {
  std::string label;                     // The blocks label
  std::vector<Instruction> instructions; // The instructions in said block
  std::vector<Block> nested_blocks;      // As the name describes
};

struct Code {
  std::string entry = "ledger: "; // This is parent entry
  Block master_block;             // The master block sits here
  Register final_return;          // The very end result sits here
};
