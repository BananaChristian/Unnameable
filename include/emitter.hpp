#include "dollar_commons.hpp"
#include "semantics.hpp"

class Emitter {
public:
  Emitter(Semantics &semantics, ErrorHandler &handler, Node *macro,
          bool verbose);
  Code emit_code(Node *macro);

private:
  bool hasFailed = false;
  bool verbose = false;
  int register_count = 0;
  int label_count = 0;
  Block *currentBlock = nullptr;
  std::string fresh_label();
  Register fresh_register();

  Code code;
  Node *macro;
  ErrorHandler &handler;
  Block master_block;
  Semantics &semantics; // the internal mini semantics
  Block new_block(const std::string &label,
                  std::vector<Instruction> block_instructions);
  using emitterFunctions = Instruction (Emitter::*)(Node *node);
  std::unordered_map<std::type_index, emitterFunctions> emitterFunctionsMap;

  void register_emitters();

  void logInternal(const std::string &message);
  void logError(ErrorCode code, Node *node, std::vector<std::string> args = {});
  void reportDevBug(const std::string &message, Node *node);

  Opcode mapBinaryOperands(TokenType type);
  Primitive mapLiteralToPrimitive(Node *node);
  void
  updateBlock(Instruction instruction); // This is called after analyzing a node

  // Emitters
  Instruction dispatch_emitters(Node *node);
  Instruction emit_literal(Node *node); // For stuff like 1,2
  Instruction emit_binary(Node *node);
};
