#include "ast.hpp"
#include "semantics/semantics.hpp"
#include "typeindex"
#include <unordered_map>
#include <vector>
#include <fstream>

enum class exportType
{
    ENUM,
    COMPONENT,
    DATA,
    SEAL,
    BEHAVIOR
};

struct SealFunction
{
    std::string funcName;
    ResolvedType returnType;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
};

struct SealTable
{
    std::string sealName;
    std::vector<SealFunction> sealFns;
};

struct StubTable
{
    std::vector<SealTable> seals;
};

class StubGen
{
public:
    StubGen(Semantics &semantics, std::string &fileName);
    using stubGenFns = void (StubGen::*)(Node *node);

    void stubGenerator(Node *node);

    void finish();

private:
    // Key vars
    Semantics &semantics;
    std::string fileName;
    std::unordered_map<std::type_index, stubGenFns> stubGenFnsMap;

    StubTable stubTable;

    // Generator functions
    void generateSealStatement(Node *node);
    void generateFunctionStatementStub(Node *node);
    void generateFunctionExpressionStub(Node *node);
    void generateFunctionDeclarationStub(Node *node);

    // Helper functions
    void registerStubGeneratorFns();
    std::string unmangle(const std::string &mangled);

    // Serializer functions
    inline void writeString(std::ostream &out, const std::string &str);
    inline void write_u8(std::ostream &out, uint8_t v);
    inline void write_u32(std::ostream &out, uint32_t v);
    void serializeResolvedType(std::ostream &out, const ResolvedType &t);
    void serializeParamTypes(std::ostream &out, const std::vector<std::pair<ResolvedType, std::string>> &params);
    void serializeSealFunction(std::ostream &out, const SealFunction &fn);
    void serializeSealTable(std::ostream &out, const SealTable &seal);
    void serializeFullStubTable(const StubTable &table, const std::string &filename);
};