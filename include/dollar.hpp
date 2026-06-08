#pragma once
#include "ast.hpp"
#include "deserial.hpp"
#include "errors.hpp"
#include "semantics.hpp"
#include "vm.hpp"
#include <string>
#include <vector>
#include "emitter.hpp"

// Dollar bill's engine
class DollarBill {
public:
  DollarBill(SemanticPayload payload, Node *macro, ErrorHandler &handler,
             Deserializer &deserial, bool verbose, bool freestanding);

  Node *emit(Node *macroNode); // This is what the walkers interface with feed
                               // it the macroNode and get the evaluatedNode

  void logInternal(const std::string &message);
  void logError(ErrorCode code, Node *node, std::vector<std::string> args = {});
  void reportDevBug(const std::string &message, Node *node);

private:
  bool hasFailed = false;
  bool verbose = false;
  bool freestanding = false;
  SemanticPayload DBSpayload;
  ErrorHandler &handler;
  Deserializer &deserial;
  Node *macro;
  Semantics DBsemantics;
  Emitter emmiter; // declared after, can now reference DBsemantics
  VirtualMachine machine;
  //These fields are where the dollar scope sits when the return node is created they get fed to the resulting node
  int macro_line;
  int macro_col;
};
