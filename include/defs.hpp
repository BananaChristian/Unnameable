#pragma once
#include <llvm/IR/Value.h>

#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "ast.hpp"

struct SymbolInfo;
struct MemberInfo;

// Type system tracker
enum class DataType {
    I8,     // 8 BIT signed integer
    U8,     // 8 bit unsigned integer
    I16,    // 16 BIT signed integer
    U16,    // 16 BUT unsigned integer
    I32,    // 32 BIT signed integer
    U32,    // 32 BIT unsigned integer
    I64,    // 64 BIT signed integer
    U64,    // 64 BIT unsigned integer
    I128,   // 128 BIT signed integer
    U128,   // 128 BIT unsigned integer
    ISIZE,  // CPU native width signed integer
    USIZE,  // CPU native width unsigned integer
    BOOLEAN,
    STRING,
    F32,
    F64,
    CHAR8,   // 8 BIT Char
    CHAR16,  // 16 BIT Char
    CHAR32,  // 32 BIT Char
    OPAQUE,  // opaque ptr type;

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
    NONE,       // base type, I32, F32, custom etc
    POINTER,    // ptr
    REFERENCE,  // ref
    ARRAY       // arr
};

struct ResolvedType {
    // What modifier am I at this level
    Modifier modifier = Modifier::NONE;

    // Only meaningful if modifier == NONE
    DataType kind = DataType::UNKNOWN;
    std::string resolvedName = "unknown";

    // Only meaningful if modifier == ARRAY
    uint64_t arraySize = 0;  // 0 means dynamic
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
        if (innerType) return innerType->base();
        return *this;
    }

    // How deep is the nesting
    int depth() const {
        if (!innerType) return 0;
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

    static ResolvedType makeBase(DataType k, const std::string &name, bool null = false) {
        ResolvedType t;
        t.modifier = Modifier::NONE;
        t.kind = k;
        t.resolvedName = name;
        t.isNull = null;
        return t;
    }

    static ResolvedType error() { return makeBase(DataType::ERROR, "error"); }
    static ResolvedType unknown() { return makeBase(DataType::UNKNOWN, "unknown"); }
    static ResolvedType null() { return makeBase(DataType::UNKNOWN, "null"); }
};

struct TypeInfo {
    ResolvedType type;          // The full resolved type
    ResolvedType derefPtrType;  // Type after one pointer dereference

    bool isNullable = false;        // Can hold null
    bool isDefinitelyNull = false;  // Statically proven to be null right now

    // Indirection flags — these mirror what ResolvedType already encodes at the
    // top level, but are kept here for fast access during semantic checks.
    bool isRef = false;
    bool isPointer = false;
    bool isArray = false;

    // Heap address flag, set when the variable needs an implicit address-of
    // during codegen (e.g. heap-allocated structs passed by pointer).
    bool isAddress = false;
    bool needsImplicitAddress = false;

    int memberIndex = -1;
    std::vector<uint64_t> sizePerDimensions;   // per-dimension sizes for arrays
    std::vector<Node *> dynSizePerDimensions;  // If the size isnt known at compile time we store
                                               // that node's symbol info here
};

// StorageInfo,where and how is this symbol allocated?
struct StorageInfo {
    bool isHeap = false;    // Explicit dynamic heap allocation
    bool isGlobal = false;  // Was this variable born in global scope
    std::string allocType;  // Name of the allocator to use

    bool isVolatile = false;  // volatile qualifier
    bool isRestrict = false;  // restrict qualifier (no aliasing)
    bool isPersist = false;   // Baton will only free at scope end

    bool isMutable = false;   // Declared with 'var'
    bool isConstant = false;  // Declared with 'const'
    bool isInitialized = false;
    int64_t constIntVal = 0;  // Compile-time integer value (when isConstant)

    int pointerCount = 0;
    int refCount = 0;        // Number of live references to this symbol
    bool isInvalid = false;  // Consumed by a move, no longer usable
};

// FunctionInfo, only populated when the symbol represents a callable
struct FunctionInfo {
    std::string funcName;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
    ResolvedType returnType;
    std::vector<ResolvedType> initArgs;

    bool isDeclaration = false;  // Forward-declared but not yet defined
    bool isDefined = false;
    bool isBehavior = false;  // Component method / behaviour block
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
    GenericInstantiationInfo &operator=(const GenericInstantiationInfo &) = delete;
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
    std::shared_ptr<SymbolInfo> targetSymbol;   // What this pointer points to
    std::shared_ptr<SymbolInfo> refereeSymbol;  // What this reference refers to

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
    std::string ID;  // Unique symbol ID used by ownership tracking
    size_t componentSize = 0;
};

// MemberInfo,describes a field or method inside a component / record / enum

struct MemberInfo {
    std::string memberName;
    ResolvedType type;
    ResolvedType parentType;  // Parent type (meaningful for enum members)
    ResolvedType returnType;  // Return type  (meaningful for function members)
    bool isReturnHeap;//If the value being returned is heap
    std::string allocType;
    std::string retFamilyID;
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
    DataType underLyingType = DataType::I32;  // enum backing type
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
        if (!_typeInfo) _typeInfo = std::make_shared<TypeInfo>();
    }
    void ensureStorageInfo() const {
        if (!_storageInfo) _storageInfo = std::make_shared<StorageInfo>();
    }
    void ensureFuncInfo() const {
        if (!_functionInfo) _functionInfo = std::make_shared<FunctionInfo>();
    }
    void ensureGenericInfo() const {
        if (!_genericInfo) _genericInfo = std::make_shared<GenericInfo>();
    }
    void ensureRelationInfo() const {
        if (!_relationInfo) _relationInfo = std::make_shared<RelationshipInfo>();
    }
    void ensureCodegenInfo() const {
        if (!_codegenInfo) _codegenInfo = std::make_shared<CodegenInfo>();
    }
};

struct LifeTime {
    std::string ID;       // Main lifetime family ID
    std::string ownedBy;  // Who robbed this baton and now owns it
    bool persist = false;
    bool isResponsible = false;  // Is this baton responsible for the actual memory it respresents
    bool isAlive = false;        // Auditor liveness simulation flag
    std::map<std::string, std::shared_ptr<SymbolInfo>>
        dependents;  // Dependents map for batons that have been robbed
    int ptrCount;
    int refCount;

    LifeTime() = default;
    LifeTime(const LifeTime &other) = default;
};

struct AllocatorHandle {
    std::string allocateName;
    std::shared_ptr<SymbolInfo> allocatorSymbol;
    std::string freeName;
    std::shared_ptr<SymbolInfo> freeSymbol;
};

// Stub generation structs
enum class StubSection : uint8_t {
    SEALS,
    COMPONENTS,
    RECORDS,
    ENUMS,
    ALLOCATORS,
    FUNCTIONS,
    VARIABLES,
    GENERICS
};

struct SealFunction {
    std::string funcName;
    ResolvedType returnType;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
};

struct SealTable {
    std::string sealName;
    std::vector<SealFunction> sealFns;
};

// Struct that will hold  component members such as int x or whatever
struct ComponentMember {
    std::string memberName;
    ResolvedType type;        // Member type like int or whatever
    int memberIndex = -1;     // Will be used by reader's IRGen so lemme keep it
    bool isNullable = false;  // Is the member nullable
    bool isMutable = false;
    bool isConstant = false;
    bool isRef = false;
    bool isPointer = false;
};

// Struct that will hold a component method
struct ComponentMethod {
    std::string methodName;
    ResolvedType returnType;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
    bool isFunction = false;
};

struct ComponentInit {
    std::vector<ResolvedType> initArgs;
    ResolvedType returnType;
    ResolvedType type;
};

struct ComponentTable {
    std::string componentName;
    std::vector<ComponentMember> members;
    std::vector<ComponentMethod> methods;
    bool hasInit = false;
    ComponentInit init;  // Every component only has one init so only one field
                         // here but its optional
};

struct RecordMember {
    std::string memberName;
    ResolvedType type;        // Member type like int or whatever
    int memberIndex = -1;     // Will be used by reader's IRGen so lemme keep it
    bool isNullable = false;  // Is the member nullable
    bool isMutable = false;
    bool isConstant = false;
    bool isRef = false;
    bool isPointer = false;
};

struct RecordTable {
    std::string recordName;
    std::vector<RecordMember> members;
};

struct EnumMembers {
    std::string memberName;
    ResolvedType type;
    ResolvedType enumType;
    int64_t constantValue;
};

struct EnumTable {
    std::string enumName;
    ResolvedType underlyingType;
    std::vector<EnumMembers> members;
};

struct AllocatorFunction {
    std::string functionName;
    ResolvedType returnType;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
};

struct Allocator {
    std::string allocatorName;
    AllocatorFunction allocator;
    AllocatorFunction free;
};

struct FunctionEntry {
    std::string funcName;
    ResolvedType returnType;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
    bool isDeclaration;
};

struct VariableEntry{
    std::string var_name;
    ResolvedType declaredType;
    bool isConstant;
    bool isMutable;
    bool isInitialized;
};

struct Generics {
    std::string aliasName;
    std::vector<RecordTable> records;
    std::vector<ComponentTable> components;
    std::vector<FunctionEntry> functions;
};

struct StubTable {
    std::vector<SealTable> seals;
    std::vector<ComponentTable> components;
    std::vector<RecordTable> records;
    std::vector<EnumTable> enums;
    std::vector<Allocator> allocators;
    std::vector<FunctionEntry> functions;
    std::vector<VariableEntry> variables;
    std::vector<Generics> generics;
};
