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
  AlreadyDeclaredFunc = 3055,
  MissingOrInvalidReturnType = 3056,
  MissingOrInvalidBody = 3057,
  NonVoidNoReturn = 3058,
  CaseCondLiteral = 3059,
  InvalidSwitchOrCaseTarget = 3060,
  FloatingControl = 3061,
  UnreachableCode = 3062,
  InvalidFuncsInAllocator = 3063,
  InvalidAllocatorContract = 3064,
  IllegalStmtInAllocator = 3065,
  InvalidCountInAllocator = 3066,
  InvalidAllocationFuncParam = 3067,
  InvalidAllocationReturn = 3068,
  InvalidDeallocatorParam = 3069,
  InvalidConstUse = 3070,
  CantReassignImmut = 3071,
  InvalidOperatorStyle = 3072,
  OpaqueNullAssignment = 3073,
  OpaqueTypeMismatch = 3074,
  NullAssignmentToNonNullable = 3075,
  SelfOnlyInComponent = 3076,
  InvalidFieldChain = 3079,
  ExpectedIdentifierInSelf = 3080,
  ArrayShapeMismatch = 3081,
  LhsHasError = 3082,
  NullAssignmentToField = 3083,
  InvalidStmtInGenerics = 3084,
  FloatingFString = 3085,
  NotaMemberOf = 3086,
  IllegalStmtInMethods = 3087,
  InstNotaRecord = 3088,
  InjectionCollision = 3089,
  GlobalInstantiation = 3090,
  InvalidInjectionOp = 3091,
  NoNeedForInit = 3092,
  InvalidEnumMemberVal = 3093,
  InvalidEnumLitType = 3094,
  NegativeMember = 3097,
  FloatingInit = 3098,
  ConstMustBeInitialized = 3100,
  ConstRequiresLiteral = 3101,
  ConstCannotBeNullable = 3102,
  ConstCannotBePersist = 3103,
  ConstCannotBeVolatile = 3104,
  ConstCannotBeHeap = 3105,
  HeapFnPtrInvalid = 3106,
  PersistRequiresHeap = 3107,
  ExportableRequiresGlobal = 3108,
  GlobalArraySizeConst = 3110,
  PointerRequiresArrow = 3111,
  RefRequiresArrow = 3112,
  RestrictOnRefInvalid = 3113,
  PointerMustBeInitialized = 3114,
  RestrictOnNonPointer = 3115,
  RefMustBeInitialized = 3116,
  RefCannotBeNullable = 3117,
  RefCannotBePersist = 3118,
  RefCannotBeVolatile = 3119,
  RefCannotBeHeap = 3120,
  ArrayMissingDimensions = 3121,
  VoidVariableInvalid = 3122,
  NullToNonNullable = 3123,
  NullToArrayNoDims = 3124,
  NullToReference = 3125,
  UnknownAllocator = 3126,
  ErroniousInitializer = 3127,
  OpaqueNonPointer = 3128,
  OpaqueInitNonPointer = 3129,
  RefToNullable = 3130,
  RefToLocal = 3131,
  MutableRefToImmutable = 3132,
  ArrayDimCountMismatch = 3133,
  ModMustBeGlobal = 3134,
  ImportMustBeGlobal = 3135,
  IllegalUseInFreeStanding = 3136,
  InterruptsMustBeVoid = 3137,
  CannotCallInterrupts = 3138,
  CannotPassCustomByVal = 3139,
  CannotAllowParamsInNaked = 3140,
  ExpectedOnlyASM = 3141,
  CannotBeInterruptAndNaked = 3142,
  AlreadySetGlobalAllocator = 3143,
  globalAllocatorMustBeGlobal = 3144,
  AlignMustBeIntegerConstant = 3145,
  PowerOfTwoAlign = 3146,
  InvalidHeapScope = 3147,
  IllegalStmtInRecord = 3148,
  AlreadySetModule = 3149,
  AlreadyImportedModule = 3150,
  ExportablevsLocal = 3151,

  ArraySizeMismatch = 4001,

  IllegalCycle = 5001,
  InvalidReturnEscape = 5002,
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
