#pragma once

#include "ast.hpp"
#include "deserial.hpp"
#include "errors.hpp"
#include "token.hpp"
#include <cstdint>
#include <llvm/IR/Value.h>
#include <map>
#include <memory>
#include <string>
#include <typeindex>

struct SymbolInfo;
struct MemberInfo;

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

enum class AllocatorRole { ALLOCATE, FREE, NONE };

enum class Modifier {
  NONE,      // base type, I32, F32, custom etc
  POINTER,   // ptr
  REFERENCE, // ref
  ARRAY      // arr
};

struct ResolvedType {
  // What modifier am I at this level
  Modifier modifier = Modifier::NONE;

  // Only meaningful if modifier == NONE
  DataType kind = DataType::UNKNOWN;
  std::string resolvedName = "unknown";

  // Only meaningful if modifier == ARRAY
  uint64_t arraySize = 0; // 0 means dynamic
  bool isConstantSize = false;

  // The thing I point to / contain / reference
  // nullptr means I am the base
  std::shared_ptr<ResolvedType> innerType = nullptr;

  // Nullable applies at any level
  bool isNull = false;

  bool isPointer() const { return modifier == Modifier::POINTER; }
  bool isRef() const { return modifier == Modifier::REFERENCE; }
  bool isArray() const { return modifier == Modifier::ARRAY; }
  bool isBase() const { return modifier == Modifier::NONE; }

  // Walk to the bottom of the chain
  const ResolvedType &base() const {
    if (innerType)
      return innerType->base();
    return *this;
  }

  // How deep is the nesting
  int depth() const {
    if (!innerType)
      return 0;
    return 1 + innerType->depth();
  }

  std::string toString() const {
    switch (modifier) {
    case Modifier::POINTER:
      return "ptr<" + (innerType ? innerType->toString() : "?") + ">";
    case Modifier::REFERENCE:
      return "ref<" + (innerType ? innerType->toString() : "?") + ">";
    case Modifier::ARRAY:
      return "arr[" + (arraySize ? std::to_string(arraySize) : "?") + "]<" +
             (innerType ? innerType->toString() : "?") + ">";
    case Modifier::NONE:
      return resolvedName + (isNull ? "?" : "");
    }
    return "unknown";
  }

  ResolvedType() = default;

  static ResolvedType makeBase(DataType k, const std::string &name,
                               bool null = false) {
    ResolvedType t;
    t.modifier = Modifier::NONE;
    t.kind = k;
    t.resolvedName = name;
    t.isNull = null;
    return t;
  }

  static ResolvedType error() { return makeBase(DataType::ERROR, "error"); }
  static ResolvedType unknown() {
    return makeBase(DataType::UNKNOWN, "unknown");
  }
  static ResolvedType null() { return makeBase(DataType::UNKNOWN, "null"); }
};

struct TypeInfo {
  ResolvedType type;         // The full resolved type
  ResolvedType derefPtrType; // Type after one pointer dereference

  bool isNullable = false;       // Can hold null
  bool isDefinitelyNull = false; // Statically proven to be null right now

  // Indirection flags — these mirror what ResolvedType already encodes at the
  // top level, but are kept here for fast access during semantic checks.
  bool isRef = false;
  bool isPointer = false;
  bool isArray = false;

  // Heap address flag — set when the variable needs an implicit address-of
  // during codegen (e.g. heap-allocated structs passed by pointer).
  bool isAddress = false;
  bool needsImplicitAddress = false;

  int memberIndex = -1;
  std::vector<uint64_t> sizePerDimensions; // per-dimension sizes for arrays
};

// StorageInfo,where and how is this symbol allocated?
struct StorageInfo {
  bool isHeap = false;   // Explicit dynamic heap allocation
  std::string allocType; // Name of the allocator to use

  bool isVolatile = false; // volatile qualifier
  bool isRestrict = false; // restrict qualifier (no aliasing)
  bool isPersist = false;  // Survives the current scope

  bool isMutable = false;  // Declared with 'var'
  bool isConstant = false; // Declared with 'const'
  bool isInitialized = false;
  int64_t constIntVal = 0; // Compile-time integer value (when isConstant)

  int pointerCount = 0;
  int refCount = 0;       // Number of live references to this symbol
  bool isInvalid = false; // Consumed by a move, no longer usable
};

// FunctionInfo, only populated when the symbol represents a callable
struct FunctionInfo {
  std::string funcName;
  std::vector<std::pair<ResolvedType, std::string>> paramTypes;
  ResolvedType returnType;
  std::vector<ResolvedType> initArgs;

  bool isDeclaration = false; // Forward-declared but not yet defined
  bool isDefined = false;
  bool isBehavior = false; // Component method / behaviour block
};

// GenericInfo, only populated for generic symbols and their instantiations
struct GenericBluePrint {
  std::string name;
  std::vector<std::string> typeParams;
  std::unordered_map<std::string, int> typeParamIndex;
  std::unique_ptr<Node> blockAST;
};

struct GenericInstantiationInfo {
  std::string aliasName;
  std::string blueprintName;
  std::unordered_map<std::string, ResolvedType> paramToType;
  std::unordered_map<std::string, Token> rawTypeMap;
  std::unique_ptr<Node> instantiatedAST;

  GenericInstantiationInfo() = default;
  GenericInstantiationInfo(const GenericInstantiationInfo &) = delete;
  GenericInstantiationInfo &
  operator=(const GenericInstantiationInfo &) = delete;
  GenericInstantiationInfo(GenericInstantiationInfo &&) = default;
  GenericInstantiationInfo &operator=(GenericInstantiationInfo &&) = default;
};

struct GenericInfo {
  std::string genericName;
  bool isGeneric = false;
  bool isInstantiation = false;
  std::optional<GenericInstantiationInfo> instTable;
};

// RelationshipInfo, links between related symbols
struct RelationshipInfo {
  // Pointer / reference targets
  std::shared_ptr<SymbolInfo> targetSymbol;  // What this pointer points to
  std::shared_ptr<SymbolInfo> refereeSymbol; // What this reference refers to

  // Field access: 'p.health' → baseSymbol = p, fieldSymbol = health
  std::shared_ptr<SymbolInfo> baseSymbol;
  std::shared_ptr<SymbolInfo> fieldSymbol;

  // Component instantiation
  std::shared_ptr<SymbolInfo> componentSymbol;
};

// CodegenInfo,populated during IR generation, invisible to semantic passes
struct CodegenInfo {
  llvm::Value *llvmValue = nullptr;
  llvm::Type *llvmType = nullptr;
  llvm::Align alignment;
  std::string ID; // Unique symbol ID used by ownership tracking
  size_t componentSize = 0;
};

// MemberInfo — describes a field or method inside a component / record / enum

struct MemberInfo {
  std::string memberName;
  ResolvedType type;
  ResolvedType parentType; // Parent type (meaningful for enum members)
  ResolvedType returnType; // Return type  (meaningful for function members)
  std::vector<std::pair<ResolvedType, std::string>> paramTypes;

  bool isNullable = false;
  bool isMutable = false;
  bool isConstant = false;
  bool isInitialised = false;
  bool isVolatile = false;
  bool isExportable = false;

  // Indirection
  bool isRef = false;
  bool isPointer = false;

  // Function / method flags
  bool isFunction = false;
  bool isDeclared = false;
  bool isDefined = false;

  // Enum member
  int64_t constantValue = 0;

  int memberIndex = -1;
  Node *node = nullptr;
  Node *typeNode = nullptr;
  Node *lastUseNode = nullptr;

  llvm::Value *llvmValue = nullptr;
  llvm::Type *llvmType = nullptr;
};

// CustomTypeInfo
struct CustomTypeInfo {
  std::string typeName;
  ResolvedType type;
  DataType underLyingType = DataType::I32; // enum backing type
  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  bool isExportable = false;
};

// ScopeInfo
struct ScopeInfo {
  ResolvedType type;
  std::string typeName;
  bool hasInitConstructor = false;
  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
  Node *node = nullptr;
};

// SymbolInfo,the central symbol descriptor
struct SymbolInfo {
  bool isFunction = false;
  bool isBehavior = false;
  bool isComponent = false;
  bool isRecord = false;

  // Members map,only populated for component/record symbols
  std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;

  bool isParam = false;
  bool isExportable = false;
  bool hasError = false;

  TypeInfo &type() {
    ensureTypeInfo();
    return *_typeInfo;
  }
  StorageInfo &storage() {
    ensureStorageInfo();
    return *_storageInfo;
  }
  FunctionInfo &func() {
    ensureFuncInfo();
    return *_functionInfo;
  }
  GenericInfo &generic() {
    ensureGenericInfo();
    return *_genericInfo;
  }
  RelationshipInfo &relations() {
    ensureRelationInfo();
    return *_relationInfo;
  }
  CodegenInfo &codegen() {
    ensureCodegenInfo();
    return *_codegenInfo;
  }

  const TypeInfo &type() const {
    ensureTypeInfo();
    return *_typeInfo;
  }
  const StorageInfo &storage() const {
    ensureStorageInfo();
    return *_storageInfo;
  }
  const FunctionInfo &func() const {
    ensureFuncInfo();
    return *_functionInfo;
  }
  const GenericInfo &generic() const {
    ensureGenericInfo();
    return *_genericInfo;
  }
  const RelationshipInfo &relations() const {
    ensureRelationInfo();
    return *_relationInfo;
  }
  const CodegenInfo &codegen() const {
    ensureCodegenInfo();
    return *_codegenInfo;
  }

  SymbolInfo() = default;

  SymbolInfo(const SymbolInfo &) = delete;
  SymbolInfo &operator=(const SymbolInfo &) = delete;

  SymbolInfo(SymbolInfo &&) = default;
  SymbolInfo &operator=(SymbolInfo &&) = default;

private:
  mutable std::shared_ptr<TypeInfo> _typeInfo;
  mutable std::shared_ptr<StorageInfo> _storageInfo;
  mutable std::shared_ptr<FunctionInfo> _functionInfo;
  mutable std::shared_ptr<GenericInfo> _genericInfo;
  mutable std::shared_ptr<RelationshipInfo> _relationInfo;
  mutable std::shared_ptr<CodegenInfo> _codegenInfo;

  void ensureTypeInfo() const {
    if (!_typeInfo)
      _typeInfo = std::make_shared<TypeInfo>();
  }
  void ensureStorageInfo() const {
    if (!_storageInfo)
      _storageInfo = std::make_shared<StorageInfo>();
  }
  void ensureFuncInfo() const {
    if (!_functionInfo)
      _functionInfo = std::make_shared<FunctionInfo>();
  }
  void ensureGenericInfo() const {
    if (!_genericInfo)
      _genericInfo = std::make_shared<GenericInfo>();
  }
  void ensureRelationInfo() const {
    if (!_relationInfo)
      _relationInfo = std::make_shared<RelationshipInfo>();
  }
  void ensureCodegenInfo() const {
    if (!_codegenInfo)
      _codegenInfo = std::make_shared<CodegenInfo>();
  }
};

struct LifeTime {
  std::string ID;      // Main lifetime family ID
  std::string ownedBy; // Who robbed this baton and now owns it
  bool persist = false;
  bool isResponsible =
      false; // Is this baton responsible for the actual memory it respresents
  bool isAlive = false; // Auditor liveness simulation flag
  std::map<std::string, std::shared_ptr<SymbolInfo>>
      dependents; // Dependents map for batons that have been robbed

  LifeTime() = default;
  LifeTime(const LifeTime &other) = default;
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

  using LifeTimeTable = std::unordered_map<Node *, std::unique_ptr<LifeTime>>;
  LifeTimeTable responsibilityTable;

  std::unordered_map<
      std::string, std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>>
      sealTable;
  std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> importedInits;
  std::optional<std::shared_ptr<SymbolInfo>> currentFunction;
  std::vector<bool> loopContext;
  std::vector<bool> caseContext;
  std::vector<ScopeInfo> currentTypeStack;

  std::unordered_map<std::string, GenericBluePrint> genericMap;
  std::unordered_map<std::string, AllocatorHandle> allocatorMap;
  std::unordered_map<std::string, std::vector<ResolvedType>> componentInitArgs;

  std::unordered_map<Node *, std::vector<std::string>> bornInBlock;

  // Public helpers
  std::shared_ptr<SymbolInfo> resolveSymbolInfo(const std::string &name);
  std::shared_ptr<SymbolInfo> lookUpInCurrentScope(const std::string &name);
  ResolvedType inferDeclarationBaseType(VariableDeclaration *declaration);
  bool hasReturnPath(Node *node);
  bool switchReturnsInAllPaths(SwitchStatement *sw);
  bool ifReturnsInAllPaths(ifStatement *ifStmt);
  bool hasReturnPathInBlock(
      const std::vector<std::unique_ptr<Statement>> &statements);
  bool hasReturnPathList(const std::vector<std::unique_ptr<Statement>> &stmts);
  ResolvedType inferNodeDataType(Node *node);
  std::string extractIdentifierName(Node *node);
  std::string extractDeclarationName(Node *node);
  ResolvedType peelRef(const ResolvedType &t);
  bool isBornInScope(Node *root, const std::string &ID);
  bool isTerminator(Node *node);

  bool isInteger(const ResolvedType &t);
  bool isFloat(const ResolvedType &t);
  bool isBoolean(const ResolvedType &t);
  bool isString(const ResolvedType &t);
  bool isChar(const ResolvedType &t);

  LifeTime *getBaton(const std::string &ID);
  std::vector<Identifier *> digIdentifiers(Node *node);
  std::shared_ptr<SymbolInfo> getSymbolFromMeta(Node *node);
  void transferResponsibility(LifeTime *currentBaton, LifeTime *targetBaton,
                              const std::shared_ptr<SymbolInfo> &tagetSym);
  Node *queryForLifeTimeBaton(const std::string &familyID);
  const std::unique_ptr<LifeTime> &readBatonInfo(const std::string &batonID);
  ResolvedType getArrayElementType(const ResolvedType &arrayType);
  bool isIntegerConstant(Node *node);
  std::vector<uint64_t> getSizePerDimesion(Node *node);
  std::string getBaseTypeName(const ResolvedType &type);
  uint64_t getIntegerConstant(Node *node);

private:
  bool insideFunction = false;
  bool insideComponent = false;
  bool insideRecord = false;
  bool insideSeal = false;
  bool insideAllocator = false;

  bool hasFailed = false;
  bool hasError = false;
  bool verbose = false;
  std::vector<Node *> activeBlocks;

  uint64_t normalDeclCount = 0;
  uint64_t ptrDeclCount = 0;
  uint64_t arrDeclCount = 0;

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
  void walkINTLiteral(Node *node);

  void walkStringLiteral(Node *node);
  void walkBooleanLiteral(Node *node);

  void walkChar8Literal(Node *node);
  void walkChar16Literal(Node *node);
  void walkChar32Literal(Node *node);

  void walkF64Literal(Node *node);
  void walkF32Literal(Node *node);
  void walkFloatLiteral(Node *node);

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
  void walkVariableDeclaration(Node *node);
  void walkSelfAssignment(AssignmentStatement *assignStmt);
  void walkAssignStatement(Node *node);
  void walkFieldAssignmentStatement(Node *node);

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
  // Walking blocks
  void walkBlockStatement(Node *node);
  void walkBlockExpression(Node *node);

  // Walking arrays
  void walkArrayLiteral(Node *node);
  void walkArraySubscriptExpression(Node *node);

  // Walking allocator interface
  void walkAllocatorInterface(Node *node);

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
  void enforceDeclarationRules(VariableDeclaration *declaration,
                               const std::shared_ptr<SymbolInfo> &declInfo);
  void handleNullInitializers(VariableDeclaration *declaration,
                              const std::shared_ptr<SymbolInfo> &declInfo,
                              const std::shared_ptr<SymbolInfo> &nullInfo);
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
  ResolvedType resolveTypeWithModifier(Node *modifier,
                                       const ResolvedType &base);
  ResolvedType makePointerType(const ResolvedType &inner, bool isNull);
  ResolvedType makeRefType(const ResolvedType &inner, bool isNull);
  ResolvedType makeArrayType(const ResolvedType &inner, uint64_t size,
                             bool isNull);
  ResolvedType *resolveSelfChain(SelfExpression *selfExpr,
                                 const std::string &componentName);
  ResolvedType
  convertImportedTypetoResolvedType(const ImportedType &importType);
  DataType
  convertImportedDataTypetoResolvedDataType(const ImportedDataType &dataType);
  Modifier
  convertImportedModifierToResolvedModifier(const ImportedModifier &mod);
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
  void checkOperatorStyle(TokenType op, bool isPointer, const std::string &name,
                          Node *site);
  void checkMutability(const SymbolInfo &sym, const std::string &name,
                       Node *site);
  bool checkTypeCompatible(ResolvedType lhs, ResolvedType rhs, bool rhsIsNull,
                           const std::string &name, Node *site);
  bool handleNullRhs(NullLiteral *nullLit,
                     const std::shared_ptr<SymbolInfo> &lhsSym,
                     const std::string &name, AssignmentStatement *assignStmt);
  void giveGenericLiteralContext(Node *literal,
                                 const std::shared_ptr<SymbolInfo> &contextSym,
                                 const std::shared_ptr<SymbolInfo> &litSym);

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
  bool checkParamListCompatibility(
      const std::vector<std::pair<ResolvedType, std::string>> &expectedParams,
      const std::vector<std::unique_ptr<Statement>> &actualParams);

  bool isCallCompatible(const SymbolInfo &funcInfo, CallExpression *callExpr);
  bool isMethodCallCompatible(const MemberInfo &memFuncInfo,
                              CallExpression *callExpr);
  bool isLiteral(Node *node);
  bool isConstLiteral(Node *node);
  void popScope();
  void registerLiteral(Node *literal, const ResolvedType &type);
  Node *getCurrentBlock();
  std::string getTerminatorString(Node *node);
  std::string generateLifetimeID(const std::shared_ptr<SymbolInfo> &sym);
  std::unique_ptr<LifeTime>
  createLifeTimeTracker(Node *declarationNode, LifeTime *targetBaton,
                        const std::shared_ptr<SymbolInfo> &declSym);
  void transferBaton(Node *receiver, const std::string &familyID);
  void logSemanticErrors(const std::string &message, Node *contextNode);
  void logSpecialErrors(const std::string &message, int line, int col);
  void reportDevBug(const std::string &message, Node *contextNode);
  void logInternal(const std::string &message);
};
