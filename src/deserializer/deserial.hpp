#pragma once

#include <unordered_map>
#include <string>
#include <vector>
#include <memory>
#include <fstream>
#include "ast.hpp"

enum class ImportedStubSection : uint8_t
{
    SEALS,
    COMPONENTS,
    DATA
};

enum class ImportedDataType
{
    SHORT_INT,  // 16 BIT signed integer
    USHORT_INT, // 16 BUT unsigned integer
    INTEGER,    // 32 BIT signed integer
    UINTEGER,   // 32 BIT unsigned integer
    LONG_INT,   // 64 BIT signed integer
    ULONG_INT,  // 64 BIT unsigned integer
    EXTRA_INT,  // 128 BIT signed integer
    UEXTRA_INT, // 128 BIT unsigned integer
    BOOLEAN,
    STRING,
    FLOAT,
    DOUBLE,
    CHAR,   // 8 BIT Char
    CHAR16, // 16 BIT Char
    CHAR32, // 32 BIT Char
    ENUM,
    DATABLOCK,
    BEHAVIORBLOCK,
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
    std::shared_ptr<ImportedType> innerType;
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
    ImportedType type;
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
    bool isFunction=false;
};

struct RawComponentTable
{
    std::string componentName;
    std::vector<RawComponentMember> members;
    std::vector<RawComponentMethod> methods;
};

struct RawDataMember
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

struct RawDataTable
{
    std::string dataName;
    std::vector<RawDataMember> members;
};

struct RawStubTable
{
    std::vector<RawSealTable> seals;
    std::vector<RawComponentTable> components;
    std::vector<RawDataTable> data;
};

class Deserializer
{
public:
    Deserializer();
    void processImports(const std::vector<std::unique_ptr<Node>> &nodes, const std::string &currentFile);

    std::unordered_map<std::string, std::string> loadedStubs;

    std::unordered_map<std::string, std::unordered_map<std::string, ImportedSymbolInfo>> importedSealTable;
    std::unordered_map<std::string, std::unordered_map<std::string, ImportedSymbolInfo>> importedComponentTable;
    std::unordered_map<std::string, std::unordered_map<std::string, ImportedSymbolInfo>> importedDataTable;

    RawStubTable stub;

private:
    uint32_t STUB_MAGIC = 0x53545542;
    uint16_t STUB_VERSION = 1;

    std::string resolveImportPath(ImportStatement *import, const std::string &currentFile);

    void loadStub(const std::string &resolved);
    void readOrFail(std::istream &in, void *dst, size_t size, const std::string &context);
    uint8_t read_u8(std::istream &in, const std::string &context);
    uint16_t read_u16(std::istream &in, const std::string &context);
    uint32_t read_u32(std::istream &in, const std::string &context);
    int32_t read_s32(std::istream &in, const std::string &context);
    std::string readString(std::istream &in, const std::string &context);
    ImportedType readImportedType(std::istream &in);
    std::vector<std::pair<ImportedType, std::string>> readParamTypes(std::istream &in);

    RawComponentMember readComponentMember(std::istream &in);
    RawComponentMethod readComponentMethod(std::istream &in);

    RawDataMember readDataMember(std::istream &in);
    RawStubTable readStubTable(std::istream &in);
};