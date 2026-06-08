#pragma  once
#include "dollar_commons.hpp"
#include <unordered_map>

class VirtualMachine {
  std::unordered_map<int, Value> register_map;
  Value return_register; // special, always holds the result
  std::unordered_map<std::string, Block *> block_map;

  int PC = 0; // Current instruction index
  struct CallFrame {
    int PC;     // Instruction to return to
    int SP = 0; // The frames stack pointer
    std::string block_label;
  };

  std::vector<CallFrame> call_stack;

public:
  bool hasFailed = false;
  void execute(Code &code);
  Value get_result() { return return_register; }

  VirtualMachine();

private:
  using executorFns = void (VirtualMachine::*)(Instruction &instruction);
  std::unordered_map<OpcodeType, executorFns> executorFnsMap;
  void register_executors();
  CallFrame &current_frame();

  void execute_block(Block &block);
  void execute_instruction(Instruction &instruction);
  Block *find_block(const std::string &label);

  void execute_garbage(Instruction &instruction);
  void execute_add(Instruction &instruction);
};
