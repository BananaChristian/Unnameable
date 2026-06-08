#include "vm.hpp"

VirtualMachine::VirtualMachine() { register_executors(); }

void VirtualMachine::register_executors() {
  executorFnsMap[OpcodeType::GARBAGE] = &VirtualMachine::execute_garbage;
}
void VirtualMachine::execute(Code &code) {
  register_map.clear();
  block_map.clear();

  CallFrame entry_frame;
  entry_frame.PC = 0;
  entry_frame.SP = 0;
  entry_frame.block_label = "entry:";
  call_stack.push_back(entry_frame);

  execute_block(code.master_block);

  return_register = register_map[code.final_return.id];
}

void VirtualMachine::execute_instruction(Instruction &instruction) {
  auto it = executorFnsMap.find(instruction.opcode.type);
  if (it == executorFnsMap.end()) {
    return (this->*it->second)(instruction);
  }
}

void VirtualMachine::execute_garbage(Instruction &instruction) {
  hasFailed = true;
  call_stack.clear();
}

void VirtualMachine::execute_add(Instruction &instruction) {
  Value a = register_map[current_frame().SP + instruction.sources[0].id];
  Value b = register_map[current_frame().SP + instruction.sources[1].id];

  


}

void VirtualMachine::execute_block(Block &block) {
  CallFrame &current_frame = call_stack.back();

  while (current_frame.PC < block.instructions.size()) {
    Instruction &current_instruction = block.instructions[current_frame.PC];
    execute_instruction(current_instruction);
  }
}
