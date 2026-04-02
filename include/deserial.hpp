#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "ast.hpp"
#include "defs.hpp"
#include "errors.hpp"

enum class LinkOrigin { NATIVE_IMPORT, MANUAL_LINK };

struct RegistryEntry {
    std::string path;
    LinkOrigin origin;
};

class Deserializer {
   public:
    Deserializer(ErrorHandler &handler, bool isVerbose);

    // Entry point, processes all import statements in the AST
    void processImports(const std::vector<std::unique_ptr<Node>> &nodes,
                        const std::string &currentFile);

    bool failed();

    // Fully deserialized stub data,semantics converts this into live symbols
    StubTable stub;

    // Paths that need to be handed to the linker
    std::vector<RegistryEntry> linkerRegistry;

    // Tracks already,loaded stubs to avoid double-loading (path -> path)
    std::unordered_map<std::string, std::string> loadedStubs;

   private:
    ErrorHandler &errorHandler;
    bool isVerbose = false;
    bool hasFailed = false;

    static constexpr uint32_t STUB_MAGIC = 0x53545542;  // "STUB"
    static constexpr uint16_t STUB_VERSION = 1;

    // Import resolution
    std::string resolveImportPath(ImportStatement *import, const std::string &currentFile);
    void recordLink(const std::string &path);
    void loadStub(const std::string &resolved);

    // Binary read primitives
    void readOrFail(std::istream &in, void *dst, size_t size, const std::string &context);
    uint8_t read_u8(std::istream &in, const std::string &context);
    uint16_t read_u16(std::istream &in, const std::string &context);
    uint32_t read_u32(std::istream &in, const std::string &context);
    uint64_t read_u64(std::istream &in, const std::string &context);
    int32_t read_s32(std::istream &in, const std::string &context);
    int64_t read_s64(std::istream &in, const std::string &context);
    std::string readString(std::istream &in, const std::string &context);

    // Type deserialization
    ResolvedType readResolvedType(std::istream &in);
    std::vector<std::pair<ResolvedType, std::string>> readParamTypes(std::istream &in);

    // Section readers
    SealFunction readSealFunction(std::istream &in);
    SealTable readSealTable(std::istream &in);

    ComponentMember readComponentMember(std::istream &in);
    ComponentMethod readComponentMethod(std::istream &in);
    ComponentInit readComponentInit(std::istream &in);
    ComponentTable readComponentTable(std::istream &in);

    RecordMember readRecordMember(std::istream &in);
    RecordTable readRecordTable(std::istream &in);

    EnumMembers readEnumMember(std::istream &in);
    EnumTable readEnumTable(std::istream &in);

    AllocatorFunction readAllocatorFunction(std::istream &in);
    Allocator readAllocator(std::istream &in);

    FunctionEntry readFunctionEntry(std::istream &in);

    Generics readGenerics(std::istream &in);

    //  Top-level stub reader
    void readStubTable(std::istream &in);

    //  Logging
    void reportDevBug(const std::string &message);
    void logImportError(const std::string &message, int line, int col);
    void logInternal(const std::string &message);
};
