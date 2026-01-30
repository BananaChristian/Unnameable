#pragma once

#include <unordered_map>
#include <string>
#include <vector>
#include <memory>
#include "ast.hpp"
#include "errors.hpp"

enum class ImportedStubSection : uint8_t
{
    SEALS,
    COMPONENTS,
    RECORDS,
    ENUMS
};

enum class ImportedDataType
{
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
    OPAQUE,
    ENUM,
    RECORD,
    COMPONENT,
    ERROR,
    VOID,
    GENERIC,
    UNKNOWN
};

struct ImportedType
{
    ImportedDataType kind; // For the custom inbuilt types
    std::string resolvedName = "unknown";
    bool isPointer = false;
    bool isRef = false;
    bool isNull = false;
    bool isArray = false;
};

enum class ImportedStorageType
{
    GLOBAL,
    STACK,
    HEAP,
};

// Minimal symbol info holder shall late be used to populate the semantics symbolInfo
struct ImportedSymbolInfo
{
    ImportedType returnType;
    std::vector<std::pair<ImportedType, std::string>> paramTypes;
    std::vector<ImportedType> initArgs;
    ImportedType type;
    ImportedType enumType;
    int64_t constantValue;
    int memberIndex = -1;
    bool isNullable = false;
    bool isMutable = false;
    bool isConstant = false;
    bool isRef = false;
    bool isPointer = false;
    ImportedStorageType storage;
    bool isFunction = false;
};

struct RawSealFunction
{
    std::string funcName;
    ImportedType returnType;
    std::vector<std::pair<ImportedType, std::string>> paramTypes;
};

struct RawSealTable
{
    std::string sealName;
    std::vector<RawSealFunction> sealFns;
};

struct RawComponentMember
{
    std::string memberName;
    ImportedType type;
    int memberIndex = -1;
    bool isNullable = false; // Is the member nullable
    bool isMutable = false;
    bool isConstant = false;
    bool isRef = false;
    bool isPointer = false;
    ImportedStorageType storage;
};

struct RawComponentMethod
{
    std::string methodName;
    ImportedType returnType;
    std::vector<std::pair<ImportedType, std::string>> paramTypes;
    bool isFunction = false;
};

struct RawComponentInit
{
    std::vector<ImportedType> initArgs;
    ImportedType returnType;
    ImportedType type;
};

struct RawComponentTable
{
    std::string componentName;
    std::vector<RawComponentMember> members;
    std::vector<RawComponentMethod> methods;
    bool hasInit = false;
    RawComponentInit init;
};

struct RawRecordMember
{
    std::string memberName;
    ImportedType type;
    int memberIndex = -1;
    bool isNullable = false; // Is the member nullable
    bool isMutable = false;
    bool isConstant = false;
    bool isRef = false;
    bool isPointer = false;
    ImportedStorageType storage;
};

struct RawRecordTable
{
    std::string recordName;
    std::vector<RawRecordMember> members;
};

struct RawEnumMembers{
    std::string memberName;
    ImportedType type;
    ImportedType enumType;
    int64_t constantValue;
};

struct RawEnumTable{
    std::string enumName;
    ImportedType underLyingType;
    std::vector<RawEnumMembers>members;
};

struct RawStubTable
{
    std::vector<RawSealTable> seals;
    std::vector<RawComponentTable> components;
    std::vector<RawRecordTable> records;
    std::vector<RawEnumTable>enums;
};

class Deserializer
{
    ErrorHandler &errorHandler;
public:
    Deserializer(ErrorHandler &handler,bool isVerbose);
    void processImports(const std::vector<std::unique_ptr<Node>> &nodes, const std::string &currentFile);
    bool failed();

    std::unordered_map<std::string, std::string> loadedStubs;

    std::unordered_map<std::string, std::unordered_map<std::string, ImportedSymbolInfo>> importedSealTable;
    std::unordered_map<std::string, std::unordered_map<std::string, ImportedSymbolInfo>> importedComponentTable;
    std::unordered_map<std::string, std::unordered_map<std::string, ImportedSymbolInfo>> importedRecordsTable;
    std::unordered_map<std::string, std::unordered_map<std::string, ImportedSymbolInfo>> importedEnumsTable;

    // Maps ComponentName to Constructor Metadata
    std::unordered_map<std::string, ImportedSymbolInfo> importedInitTable;

    RawStubTable stub;

private:
    uint32_t STUB_MAGIC = 0x53545542;
    uint16_t STUB_VERSION = 1;
    bool isVerbose=false;
    bool hasFailed=false;

    std::string resolveImportPath(ImportStatement *import, const std::string &currentFile);

    void loadStub(const std::string &resolved);
    void readOrFail(std::istream &in, void *dst, size_t size, const std::string &context);
    uint8_t read_u8(std::istream &in, const std::string &context);
    uint16_t read_u16(std::istream &in, const std::string &context);
    uint32_t read_u32(std::istream &in, const std::string &context);
    int32_t read_s32(std::istream &in, const std::string &context);
    int64_t read_s64(std::istream &in,const std::string &context);
    std::string readString(std::istream &in, const std::string &context);
    ImportedType readImportedType(std::istream &in);
    std::vector<std::pair<ImportedType, std::string>> readParamTypes(std::istream &in);

    RawComponentMember readComponentMember(std::istream &in);
    RawComponentMethod readComponentMethod(std::istream &in);
    RawComponentInit readComponentInit(std::istream &in);

    RawRecordMember readRecordMember(std::istream &in);
    
    RawEnumMembers readEnumMember(std::istream &in);
    RawStubTable readStubTable(std::istream &in);
    
    void reportDevBug(const std::string &message);
    void logImportError(const std::string &message,int line,int col);
    void logInternal(const std::string &message);
};