#pragma once
#include <ostream>

#include "ast.hpp"
#include "semantics.hpp"
#include "typeindex"

class StubGen {
   public:
    StubGen(Semantics &semantics, std::string &fileName, bool isVerbose);
    using stubGenFns = void (StubGen::*)(Node *node);

    void stubGenerator(Node *node);
    bool failed();
    void finish();

   private:
    // Key vars
    Semantics &semantics;
    std::string fileName;
    bool isVerbose = false;
    bool hasFailed = false;
    std::unordered_map<std::type_index, stubGenFns> stubGenFnsMap;

    StubTable stubTable;

    // Generator functions
    void generateSealStatement(Node *node);
    void generateComponentStatement(Node *node);
    void generateRecordStatement(Node *node);
    void generateEnumStatement(Node *node);
    void generateAllocatorStatement(Node *node);
    void generateInstantiateStatement(Node *node);
    void generateFunctionStatement(Node *node);

    void generateFunctionExpression(FunctionExpression *fnExpr);
    void generateFunctionDeclaration(FunctionDeclarationExpression  *fnEclrExpr);

    // Helper functions
    void registerStubGeneratorFns();
    std::string unmangle(const std::string &mangled);

    // Serializer functions
    inline void writeString(std::ostream &out, const std::string &str);
    inline void write_u8(std::ostream &out, uint8_t v);
    inline void write_s32(std::ostream &out, int32_t v);
    inline void write_s64(std::ostream &out, int64_t v);
    inline void write_u16(std::ostream &out, uint16_t v);
    inline void write_u32(std::ostream &out, uint32_t v);
    inline void write_u64(std::ostream &out, uint64_t v);
    void serializeResolvedType(std::ostream &out, const ResolvedType &t);
    void serializeParamTypes(std::ostream &out,
                             const std::vector<std::pair<ResolvedType, std::string>> &params);

    void serializeSealFunction(std::ostream &out, const SealFunction &fn);
    void serializeSealTable(std::ostream &out, const SealTable &seal);

    void serializeComponentMethod(std::ostream &out, const ComponentMethod &method);
    void serializeComponentMember(std::ostream &out, const ComponentMember &member);
    void serializeComponentTable(std::ostream &out, const ComponentTable &component);
    void serializeComponentInit(std::ostream &out, const ComponentInit &init);

    void serializeRecordMember(std::ostream &out, const RecordMember &member);
    void serializeRecordTable(std::ostream &out, const RecordTable &record);

    void serializeEnumMember(std::ostream &out, const EnumMembers &member);
    void serializeEnumTable(std::ostream &out, const EnumTable &enumTable);

    void serializeFunctionEntry(std::ostream &out, const FunctionEntry &entry);

    void serializeAllocatorFunction(std::ostream &out, const AllocatorFunction &function);
    void serializeAllocator(std::ostream &out, const Allocator &allocator);
    void serializeGenerics(std::ostream &out, const Generics &generic);

    void serializeFullStubTable(const StubTable &table, const std::string &filename);
    void logInternal(const std::string &message);
    void reportDevBug(const std::string &message);
};
