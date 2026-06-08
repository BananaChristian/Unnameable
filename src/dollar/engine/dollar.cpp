#include "dollar.hpp"
#include "deserial.hpp"
#include "errors.hpp"
#include "semantics.hpp"

DollarBill::DollarBill(SemanticPayload payload, Node *macro,
                       ErrorHandler &handler, Deserializer &deserial,
                       bool verbose, bool freestanding)
    : verbose(verbose), freestanding(freestanding), DBSpayload(payload),
      handler(handler), deserial(deserial), macro(macro),
      DBsemantics(deserial, handler, verbose, freestanding, true),
      emmiter(DBsemantics, handler, macro, verbose), machine(VirtualMachine()) {
  DBsemantics.overrideSemantics(DBSpayload);
  DBsemantics.walker(macro);
}

Node *DollarBill::emit(Node *macroNode) {
  Code code = emmiter.emit_code(macroNode);
  machine.execute(code);
  return machine.get_result().node;
}

void DollarBill::logInternal(const std::string &message) {
  if (verbose) {
    std::cout << message << "\n";
  }
}

void DollarBill::logError(ErrorCode code, Node *node,
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

void DollarBill::reportDevBug(const std::string &message, Node *node) {
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
