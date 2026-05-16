#pragma once
#include "ast.hpp"
#include <cstddef>
#include <string>
#include <vector>

enum class ErrorLevel {
  WARNING,
  ERROR,
  FATAL,
};

enum ErrorCode : int {
  GenericError = 0000,

  // Lexer errors
  UnexpectedChar = 1001,
  UnterminatedString = 1002,
  InvalidEscape = 1003,
  UnterminatedChar = 1004,
  UnterminatedComment = 1005,
  InvalidToken = 1006,

  // Parser errors
  UnexpectedToken = 2001,
  MissingClosingBracket = 2002,
  InvalidReturnType = 2003,
  InvalidType = 2004,
  InvalidModifier = 2005,
  InvalidConstraint = 2006,
  ExpectIntegerToken = 2007,
  DuplicateInit = 2008,
  UnsupportedStatement = 2009,
  MultipleMutSpecifiers = 2010,

  // Semantics
  UndefinedVariable = 3001,
  TypeMismatch = 3002,
  TypeMismatchArrayLit = 3003,
  NoneIndexableType = 3004,
  NoneDereferencableType = 3005,
  NotaFuncOrFnPtr = 3006,
  InvalidSelfAccess = 3007,
  LHSMustBeNull = 3008,
  InvalidUsageOfNull = 3009,
  NonExistantMember = 3010,
  InvalidBindOperator = 3011,
  InvalidOperationOnTypes = 3012,
  InvalidPrefixOrPostfixOps = 3013,
  ArgumentSizeMismatch = 3014,
  ArgumentTypeMismatch = 3015,
  NullPassFailure = 3016,
  FailedToInfer = 3017,
  GlobalHeapVar = 3018,
  InvalidImmutUse = 3019,
  InvalidAddrOperand = 3020,
  NoneBitcastableType = 3022,
  NoneCastableType = 3023,
  UnwrappableType = 3024,
  InvalidInfix = 3025,
  InvalidUninitUse = 3030,
  FloatingReturns = 3031,
  NonVoidReturn = 3032,
  DuplicateName = 3033,
  InvalidHeapParam = 3034,
  InvalidPersistParam = 3035,
  InvalidNullReferenceParam = 3036,
  InvalidAutoUse = 3037,
  NoParamDefaultVal = 3038,
  DanglingReferenceReturn = 3039,
  IllegalFunctionDeclaration = 3040,
  IllegalStmtInSeal = 3041,
  InvalidNullReturn = 3042,
  InvalidFinalExpression = 3043,
  IllegalFunctionDefinition = 3044,
  DefnDeclMismatch = 3045,
  AlreadyDefinedFunc = 3046,
  MatchExportsToTypes = 3047,
  InvalidParam = 3048,
  FloatingTrace = 3049,
  NotDefinedOrDeclared = 3050,
};

struct ErrorMessage {
  ErrorCode code;
  std::string message;
  std::vector<std::string> hints;
};

struct CompilerError {
  ErrorLevel level;
  ErrorCode code;
  size_t line;
  size_t column;
  ErrorMessage message;
  size_t length;
};

class ErrorHandler {
  std::string fileName;
  std::vector<std::string> hintBuffer;

public:
  ErrorHandler(const std::string &fileName);
  void report(CompilerError &compilerError);
  ErrorHandler &addHint(const std::string &hint);
  ErrorMessage generateErrorMessage(ErrorCode code);
  int getTokenLength(Node *contextNode);
  std::string format_string(std::string &message,
                            std::vector<std::string> &args);

private:
  std::vector<std::string> sourceLines;
  std::vector<std::string> suggestForError(ErrorCode code);
  int errorCodeToInt(ErrorCode code);
  std::string errorCodeName(ErrorCode code, ErrorLevel level);

  void loadSourceLines();
  std::string getBaseName(const std::string &fullPath);
  std::string getSourceLine(int line, std::vector<std::string> sourceLines);
};
