#include "ast.hpp"
#include "semantics/semantics.hpp"
#include "typeindex"
#include <unordered_map>
#include <vector>
#include <fstream>

enum class exportType
{
    FUNCTION,
    DECLARATION,
    ENUM,
    COMPONENT,
    DATA,
    BEHAVIOR
};

struct Stub
{
    std::string name;                                             // Name of the object getting exported
    ResolvedType returnType;                                      // If the object is a declaration or function this is its return type
    std::vector<std::pair<ResolvedType, std::string>> paramTypes; // The param types of the object and the names of the params
    exportType type;                                              // Just a safety flag for later use in decoding                                              // The node of the object
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

    std::unordered_map<std::string, Stub> stubTable;

    // Generator functions
    void generateFunctionStatementStub(Node *node);
    void generateFunctionExpressionStub(Node *node);
    void generateFunctionDeclarationStub(Node *node);

    // Helper functions
    void registerStubGeneratorFns();

    // Serializer functions
    inline void writeString(std::ostream &out, const std::string &str);
    inline void write_u8(std::ostream &out, uint8_t v);
    inline void write_u32(std::ostream &out, uint32_t v);
    void serializeResolvedType(std::ostream &out, const ResolvedType &t);
    void serializeParamTypes(std::ostream &out, const std::vector<std::pair<ResolvedType, std::string>> &params);
    void serializeStub(std::ostream &out, const Stub &stub);
    void serializeStubTable(const std::unordered_map<std::string, Stub> &table, const std::string &filename);
};