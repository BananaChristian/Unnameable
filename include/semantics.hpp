#pragma once

#include "ast.hpp"
#include "deserial.hpp"
#include "errors.hpp"
#include "token.hpp"
#include <cstdint>
#include <llvm/IR/Value.h>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <typeindex>

// Type system tracker
enum class DataType {
  I8,    // 8 BIT signed integer
  U8,    // 8 bit unsigned integer
  I16,   // 16 BIT signed integer
  U16,   // 16 BUT unsigned integer
  I32,   // 32 BIT signed integer
  U32,   // 32 BIT unsigned integer
  I64,   // 64 BIT signed integer
  U64,   // 64 BIT unsigned integer
  I128,  // 128 BIT signed integer
  U128,  // 128 BIT unsigned integer
  ISIZE, // CPU native width signed integer
  USIZE, // CPU native width unsigned integer
  BOOLEAN,
  STRING,
  F32,
  F64,
  CHAR8,  // 8 BIT Char
  CHAR16, // 16 BIT Char
  CHAR32, // 32 BIT Char
  OPAQUE, // opaque ptr type;

  ENUM,
  RECORD,
  COMPONENT,

  ERROR,
  VOID,
  GENERIC,
  UNKNOWN
};

enum class StorageType {
  STACK = 0,
  HEAP = 1,
  SAGE = 2,
  GLOBAL = 3,
};

enum class AllocatorRole { ALLOCATE, FREE, NONE };

struct ResolvedType {
  DataType kind; // For the custom inbuilt types
  std::string resolvedName = "unknown";
  bool isPointer = false;
  bool isRef = false;
  bool isNull = false;
  bool isArray = false;
};

struct MemberInfo {
  std::string memberName;
  ResolvedType type; // Type of the member
  std::vector<std::pair<ResolvedType, std::string>> paramTypes;
  ResolvedType returnType;
  bool isNullable = false;
  bool isMutable = false;
  bool isConstant = false;
  bool isInitialised = false;
  // Function flags
  bool isFunction = false;
  bool isDeclared = false;
  bool isDefined = false;

  // Info for enum members
  std::int64_t constantValue = 0;
  ResolvedType parentType; // Parent type for enum members

  Node *node = nullptr;
  Node *typeNode = nullptr;
  llvm::Value *llvmValue = nullptr;
  llvm::Type *llvmType = nullptr;
  int memberIndex = -1;

  // Export flag
  bool isExportable = false;

  // Storage info
  StorageType storage;

  bool isHeap = false;    // TODO: REVISIT THIS
  bool isRef = false;     // Reference flag
  bool isPointer = false; // Pointer flag
  Node *lastUseNode = nullptr;
};

struct CustomTypeInfo {
  std::string typeName;
  ResolvedType type;
  // Special for enum class
  DataType underLyingType = DataType::I32; // Defaulting to 32 bit integer
  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  bool isExportable = false;
};

struct ScopeInfo {
  ResolvedType type;
  std::string typeName;
  bool hasInitConstructor = false;
  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  Node *node = nullptr;
};

struct GenericBluePrint {
  std::string name;                    // Block name
  std::vector<std::string> typeParams; // The type parameters
  std::unordered_map<std::string, int> typeParamIndex;
  std::unique_ptr<Node> blockAST; // original AST subtree for the generic block
};

struct GenericInstantiationInfo {
  std::string aliasName;
  std::string blueprintName;
  std::unordered_map<std::string, ResolvedType> paramToType; // T -> int
  std::unordered_map<std::string, Token> rawTypeMap;         // T -> int token
  std::unique_ptr<Node> instantiatedAST; // cloned + substituted AST

  GenericInstantiationInfo() = default;

  GenericInstantiationInfo(const GenericInstantiationInfo &) = delete;
  GenericInstantiationInfo &
  operator=(const GenericInstantiationInfo &) = delete;

  GenericInstantiationInfo(GenericInstantiationInfo &&) = default;
  GenericInstantiationInfo &operator=(GenericInstantiationInfo &&) = default;
};

struct LifeTime;

// Information about the symbol(variable or object, whatever)
struct SymbolInfo {
  ResolvedType type;
  std::string genericName;
  bool isNullable = false;
  bool isDefinitelyNull = false;
  bool isMutable = false;
  bool isConstant = false;
  bool isInitialized = false;
  int64_t constIntVal;
  std::vector<std::pair<ResolvedType, std::string>> paramTypes;
  ResolvedType returnType;
  std::vector<ResolvedType> initArgs;
  // Function flags
  bool isDeclaration = false;
  bool isDefined = false;

  std::vector<uint64_t> sizePerDimensions;

  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  llvm::Value *llvmValue = nullptr;
  llvm::Type *llvmType = nullptr;
  ResolvedType derefPtrType;
  int memberIndex = -1;

  // Storage types
  StorageType storage;

  bool isSage = false;   // Sage heap flag
  bool isHeap = false;   // Dynamic heap flag
  std::string allocType; // The name of the allocator the heap will use

  bool isRef = false;     // Reference flag
  bool isPointer = false; // Pointer flag

  bool needsImplicitAddress = false;

  bool isParam = false;

  bool isFunction = false;
  bool isBehavior = false;
  bool isComponent = false;
  bool isDataBlock = false;
  std::shared_ptr<SymbolInfo> targetSymbol;  // For the deref system
  std::shared_ptr<SymbolInfo> refereeSymbol; // Symbol being refered to

  std::shared_ptr<SymbolInfo>
      componentSymbol; // Symbol of the component being instantiated

  std::shared_ptr<SymbolInfo> baseSymbol; // The owner (e.g., p in p.health)
  std::shared_ptr<SymbolInfo>
      fieldSymbol; // The actual member accessed (health)

  size_t componentSize;
  llvm::Align alignment;
  std::string ID;
  int refCount = 0;
  int pointerCount = 0;
  bool isInvalid = false;
  bool needsPostLoopFree = false;
  bool bornInLoop = false;

  // Error flag
  bool hasError = false;

  // Generic flag
  bool isGeneric = false;
  bool isInstantiation = false;
  std::optional<GenericInstantiationInfo> instTable;

  // Export flag
  bool isExportable = false;

  SymbolInfo() = default;

  // No copying
  SymbolInfo(const SymbolInfo &) = delete;
  SymbolInfo &operator=(const SymbolInfo &) = delete;

  // Movable
  SymbolInfo(SymbolInfo &&) = default;
  SymbolInfo &operator=(SymbolInfo &&) = default;
};

struct LifeTime {
  std::string ID;
  bool isResponsible = false;
  std::map<std::string, std::shared_ptr<SymbolInfo>> dependents;
};

struct AllocatorHandle {
  std::string allocateName;
  std::shared_ptr<SymbolInfo> allocatorSymbol;
  std::string freeName;
  std::shared_ptr<SymbolInfo> freeSymbol;
};

class Semantics {
  ErrorHandler &errorHandler;

public:
  Deserializer &deserializer;
  Semantics(Deserializer &deserializer, ErrorHandler &handler, bool isVerbose);
  void walker(Node *node);
  bool failed();

  using walkerFunctions = void (Semantics::*)(Node *);
  std::unordered_map<std::type_index, walkerFunctions> walkerFunctionsMap;
  std::vector<std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>>
      symbolTable;
  std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>>
      customTypesTable;
  std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>>
      ImportedComponentTable;
  std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>>
      ImportedRecordTable;
  std::unordered_map<Node *, std::shared_ptr<SymbolInfo>> metaData;
  std::unordered_map<Node *, std::unique_ptr<LifeTime>> responsibilityTable;
  std::unordered_map<
      std::string, std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>>
      sealTable;
  std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> importedInits;
  std::optional<std::shared_ptr<SymbolInfo>> currentFunction;
  std::vector<bool> loopContext;
  std::vector<ScopeInfo> currentTypeStack;
  std::vector<Identifier *> currentBranchIdents;
  std::set<SymbolInfo *> loopDeathRow;
  std::set<SymbolInfo *> loopResidentDeathRow;

  std::unordered_map<std::string, GenericBluePrint> genericMap;
  std::unordered_map<std::string, AllocatorHandle> allocatorMap;
  std::unordered_map<std::string, std::vector<ResolvedType>> componentInitArgs;

  // Public helpers
  std::shared_ptr<SymbolInfo> resolveSymbolInfo(const std::string &name);
  std::shared_ptr<SymbolInfo> lookUpInCurrentScope(const std::string &name);
  ResolvedType resolvedDataType(Token token, Node *node);
  bool hasReturnPath(Node *node);
  bool switchReturns(SwitchStatement *sw);
  bool hasReturnPathList(const std::vector<std::unique_ptr<Statement>> &stmts);
  ResolvedType inferNodeDataType(Node *node);
  std::string stripPtrSuffix(const std::string &typeName);
  std::string stripRefSuffix(const std::string &typeName);
  std::string stripOptionalSuffix(const std::string &typeName);
  std::string extractIdentifierName(Node *node);
  std::string extractDeclarationName(Node *node);
  ResolvedType peelRef(ResolvedType t);

  bool isInteger(const ResolvedType &t);
  bool isFloat(const ResolvedType &t);
  bool isBoolean(const ResolvedType &t);
  bool isString(const ResolvedType &t);
  bool isChar(const ResolvedType &t);

  std::shared_ptr<SymbolInfo> getSymbolFromMeta(Node *node);
  Node *queryForLifeTimeBaton(const std::string &familyID);
  ResolvedType getArrayElementType(ResolvedType &arrayType);
  bool isIntegerConstant(Node *node);
  std::vector<uint64_t> getSizePerDimesion(Node *node);
  uint64_t getIntegerConstant(Node *node);

private:
  bool insideFunction = false;
  bool insideComponent = false;
  bool insideRecord = false;
  bool insideSeal = false;
  bool insideAllocator = false;

  bool hasFailed = false;
  bool verbose = false;

  uint64_t letDeclCount = 0;
  uint64_t ptrDeclCount = 0;

  // Walking the data type literals
  void walkI8Literal(Node *node);
  void walkU8Literal(Node *node);
  void walkI16Literal(Node *node);
  void walkU16Literal(Node *node);
  void walkI32Literal(Node *node);
  void walkU32Literal(Node *node);
  void walkI64Literal(Node *node);
  void walkU64Literal(Node *node);
  void walkI128Literal(Node *node);
  void walkU128Literal(Node *node);
  void walkISIZELiteral(Node *node);
  void walkUSIZELiteral(Node *node);

  void walkStringLiteral(Node *node);
  void walkBooleanLiteral(Node *node);

  void walkChar8Literal(Node *node);
  void walkChar16Literal(Node *node);
  void walkChar32Literal(Node *node);

  void walkF64Literal(Node *node);
  void walkF32Literal(Node *node);

  void walkSizeOfExpression(Node *node);

  void walkNullLiteral(Node *node);

  // Walking the component functions declaration
  void walkRecordStatement(Node *node);
  void walkComponentStatement(Node *node);
  void walkNewComponentExpression(Node *node);
  void walkInitConstructor(Node *node);
  void walkSelfExpression(Node *node);
  void walkEnumStatement(Node *node);
  void walkInstanceExpression(Node *node);
  void walkMethodCallExpression(Node *node);

  // Waling infix, prefix and postfix expressions
  void walkInfixExpression(Node *node);
  void walkPrefixExpression(Node *node);
  void walkPostfixExpression(Node *node);

  void walkIdentifierExpression(Node *node);
  void walkAddressExpression(Node *node);
  void walkDereferenceExpression(Node *node);
  void walkMoveExpression(Node *node);

  // Walking expression statement
  void walkExpressionStatement(Node *node);

  // Walking the let statements and assignment statements
  void walkLetStatement(Node *node);
  void walkAssignStatement(Node *node);
  void walkFieldAssignmentStatement(Node *node);

  // Walking reference and pointer statement
  void walkReferenceStatement(Node *node);
  void walkPointerStatement(Node *node);

  // Walking the loop disruption statements
  void walkBreakStatement(Node *node);
  void walkContinueStatement(Node *node);

  // Walking control flow nodes
  void walkWhileStatement(Node *node);
  void walkForStatement(Node *node);
  void walkIfStatement(Node *node);
  void walkElifStatement(Node *node);
  void walkSwitchStatement(Node *node);
  void walkCaseStatement(Node *node, const ResolvedType &targetType);

  // walking functional programming nodes
  void walkFunctionStatement(Node *node);
  void walkFunctionExpression(Node *node);
  void walkFunctionDeclarationStatement(Node *node);
  void walkFunctionDeclarationExpression(Node *node);
  void walkFunctionCallExpression(Node *node);
  void walkUnwrapExpression(Node *node);
  void walkReturnStatement(Node *node);

  // Walking type expressions
  void walkBasicType(Node *node);
  void walkArrayType(Node *node);

  // Walking blocks
  void walkBlockStatement(Node *node);
  void walkBlockExpression(Node *node);

  // Walking arrays
  void walkArrayLiteral(Node *node);
  void walkArrayStatement(Node *node);
  void walkArraySubscriptExpression(Node *node);

  // Walking allocator interface
  void walkAllocatorInterface(Node *node);
  void walkHeapStatement(Node *node);

  // Walking generics
  void walkGenericStatement(Node *node);
  void walkInstantiateStatement(Node *node);

  // Walking casts
  void walkCastExpression(Node *node);
  void walkBitcastExpression(Node *node);

  void walkSealStatement(Node *node);

  // Walking the shout statement
  void walkTraceStatement(Node *node);

  void walkFunctionParameters(Node *node);

  void walkSealCallExpression(Node *node, const std::string &sealName);

  // HELPER FUNCTIONS
  void registerWalkerFunctions();
  ResolvedType inferInfixExpressionType(Node *node);
  ResolvedType inferPrefixExpressionType(Node *node);
  ResolvedType inferPostfixExpressionType(Node *node);
  ResolvedType resultOfBinary(TokenType operatorType, ResolvedType leftType,
                              ResolvedType rightType, Node *expr);
  ResolvedType resultOfUnary(TokenType operatorType,
                             const ResolvedType &oprendType, Node *node);
  ResolvedType tokenTypeToResolvedType(Token token, bool isNullable);
  std::shared_ptr<SymbolInfo> resultOfScopeOrDot(TokenType operatorType,
                                                 const ResolvedType &parentType,
                                                 const std::string &childName,
                                                 InfixExpression *infix);
  ResolvedType isPointerType(ResolvedType t);
  ResolvedType isRefType(ResolvedType t);
  ResolvedType *resolveSelfChain(SelfExpression *selfExpr,
                                 const std::string &componentName);
  ResolvedType
  convertImportedTypetoResolvedType(const ImportedType &importType);
  DataType
  convertImportedDataTypetoResolvedDataType(const ImportedDataType &dataType);
  StorageType convertImportedStorageTypetoStorageType(
      const ImportedStorageType &storageType);
  std::vector<std::pair<ResolvedType, std::string>>
  convertImportedParamstoResolvedParams(
      const std::vector<std::pair<ImportedType, std::string>> &params);
  bool isTypeCompatible(const ResolvedType &expected,
                        const ResolvedType &actual);
  int inferLiteralDimensions(ArrayLiteral *arrLit);
  void inferSizePerDimension(ArrayLiteral *lit, std::vector<int64_t> &sizes);
  void substituteTypes(Node *node,
                       std::unordered_map<std::string, Token> &subMap);
  void mangleGenericName(Node *node, std::string aliasName);

  void importSeals();
  void importComponents();
  void importComponentInits();
  void importRecords();
  void importEnums();
  void importAllocators();
  void import();

  void registerInbuiltAllocatorTypes();
  bool isGlobalScope();
  AllocatorRole
  getFunctionRole(const std::vector<std::unique_ptr<Statement>> &params,
                  Expression *returnType, const std::string &funcName);
  bool areSignaturesCompatible(const SymbolInfo &declInfo,
                               FunctionExpression *funcExpr);
  bool signaturesMatchBehaviorDeclaration(
      const std::shared_ptr<MemberInfo> &declMember,
      FunctionExpression *funcExpr);
  bool isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr);
  bool isMethodCallCompatible(const MemberInfo &memFuncInfo,
                              CallExpression *callExpr);

  bool isLiteral(Node *node);
  bool isConstLiteral(Node *node);
  void popScope();
  std::string generateLifetimeID(Node *declarationNode);
  std::unique_ptr<LifeTime>
  createLifeTimeTracker(Node *declarationNode, LifeTime *targetBaton,
                        const std::shared_ptr<SymbolInfo> &declSym);
  void transferResponsibility(LifeTime *currentBaton, LifeTime *targetBaton,
                              const std::shared_ptr<SymbolInfo> &tagetSym);
  bool validateLatticeMove(const StorageType &holderType,
                           const StorageType &targetType);
  std::string storageStr(const StorageType &storage);
  StorageType determineTier(const std::shared_ptr<SymbolInfo> &sym);
  void logSemanticErrors(const std::string &message, int tokenLine,
                         int tokenColumn);
  void reportDevBug(const std::string &message);
  void logInternal(const std::string &message);
};
