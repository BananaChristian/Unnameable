#include "emitter.hpp"

Instruction Emitter::emit_literal(Node *node) {
  if (!semantics.isConstLiteral(node))
    throw std::runtime_error("Dollar bill: Invalid literal");

  Instruction instruction;
  instruction.result = fresh_register();
  instruction.opcode = Opcode::const_val();
  instruction.intermediate = {node, mapLiteralToPrimitive(node)};
  updateBlock(instruction);
  return instruction;
}

Instruction Emitter::emit_binary(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  // Getting the source registers
  Register left = dispatch_emitters(infix->left_operand.get()).result;
  Register right = dispatch_emitters(infix->right_operand.get()).result;

  Instruction instruction;
  instruction.result = fresh_register();
  instruction.sources = {left, right};
  instruction.opcode = mapBinaryOperands(infix->operat.type);
  updateBlock(instruction);
  return instruction;
}
