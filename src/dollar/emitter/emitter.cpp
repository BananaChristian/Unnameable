#include "ast.hpp"
#include "dollar.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include "token.hpp"
#include <stdexcept>

Emitter::Emitter(Semantics &semantics, ErrorHandler &handler, Node *macro,
                 bool verbose)
    : verbose(verbose), macro(macro), handler(handler), semantics(semantics) {
  register_emitters();
}

void Emitter::register_emitters() {
  emitterFunctionsMap[typeid(INTLiteral)] = &Emitter::emit_literal;
  emitterFunctionsMap[typeid(BooleanLiteral)] = &Emitter::emit_literal;
  emitterFunctionsMap[typeid(FloatLiteral)] = &Emitter::emit_literal;
  emitterFunctionsMap[typeid(InfixExpression)] = &Emitter::emit_binary;
}

Instruction Emitter::dispatch_emitters(Node *node) {
  auto it = emitterFunctionsMap.find(typeid(*node));
  if (it != emitterFunctionsMap.end()) {
    return (this->*it->second)(node);
  }
  reportDevBug("Dollar bill: Unknown node type", node);

  return Instruction::garbage();
}

void Emitter::updateBlock(Instruction instruction) {
  currentBlock->instructions.push_back(instruction);
}

Opcode Emitter::mapBinaryOperands(TokenType type) {
  switch (type) {
  case TokenType::PLUS:
    return Opcode::add();
  case TokenType::MINUS:
    return Opcode::sub();
  case TokenType::ASTERISK:
    return Opcode::mul();
  case TokenType::DIVIDE:
    return Opcode::div();
  case TokenType::MODULUS:
    return Opcode::mod();
  case TokenType::EQUALS:
    return Opcode::eq();
  case TokenType::NOT_EQUALS:
    return Opcode::neq();
  case TokenType::LESS_THAN:
    return Opcode::lt();
  case TokenType::GREATER_THAN:
    return Opcode::gt();
  case TokenType::LT_OR_EQ:
    return Opcode::lte();
  case TokenType::GT_OR_EQ:
    return Opcode::gte();
  default:
    throw std::runtime_error("Unsupported binary operator");
  }
}

// This is a block creator
Block Emitter::new_block(const std::string &label,
                         std::vector<Instruction> instructions) {
  Block new_block;
  new_block.label = label;
  new_block.instructions = instructions;
  return new_block;
}

// This emits the final overall code
Code Emitter::emit_code(Node *macroNode) {
  macro = macroNode;
  hasFailed = false;
  register_count = 0;
  label_count = 0;

  code = Code();

  // Initialize the master execution block space
  master_block = new_block("entry:", {});
  currentBlock = &master_block;

  logInternal("[DB Emit] Commencing ledger writing loop");

  // Dispatch the recursive walk
  Instruction final_instruction = dispatch_emitters(macroNode);

  if (final_instruction.result.is_failed()) {
    logInternal(
        "[DB Emit] Compilation aborted. Yielding poisoned code blueprint.");
    return code;
  }

  // The VM will run the code sequence, look here, and know exactly
  // which register contains the ultimate result to pull out.
  code.final_return = final_instruction.result;

  logInternal("[DB Emit] Ledger sealed successfully.");
  return code;
}

Register Emitter::fresh_register() { return Register{register_count++}; }

Primitive Emitter::mapLiteralToPrimitive(Node *node) {
  if (semantics.isIntegerConstant(node)) {
    return Primitive::SCALAR;
  }
  if (semantics.isFloatConstant(node)) {
    return Primitive::FLOAT;
  }
  return Primitive::ADDRESS;
}

void Emitter::logError(ErrorCode code, Node *node,
                       std::vector<std::string> args) {
  auto tokenLine = 0;
  auto tokenColumn = 0;
  if (node) {
    tokenLine = node->token.line;
    tokenColumn = node->token.column;
  }

  hasFailed = true;

  CompilerError error;
  error.level = ErrorLevel::ERROR;
  error.line = tokenLine;
  error.column = tokenColumn;
  error.length = handler.getTokenLength(node);
  error.code = code;

  ErrorMessage msg = handler.generateErrorMessage(code);
  msg.message = handler.format_string(msg.message, args);
  error.message = msg;

  handler.report(error);
}

void Emitter::logInternal(const std::string &message) {
  if (verbose) {
    std::cout << message << "\n";
  }
}

void Emitter::reportDevBug(const std::string &message, Node *node) {
  int line = 0;
  int col = 0;

  if (node) {
    line = node->token.line;
    col = node->token.column;
  }

  CompilerError error;
  error.level = ErrorLevel::FATAL;
  error.line = line;
  error.column = col;
  error.length = handler.getTokenLength(node);
  error.message.message = "internal compiler error: " + message;
  error.message.hints.push_back("this is a compiler bug, not your fault");
  error.message.hints.push_back(
      "please report at https://github.com/BananaChristian/Unnameable/issues");
  handler.report(error);
  std::abort();
}
