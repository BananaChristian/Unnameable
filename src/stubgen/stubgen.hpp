#include "ast.hpp"
#include "semantics/semantics.hpp"
#include "typeindex"
#include <unordered_map>
#include <vector>
#include <fstream>

enum class StubSection : uint8_t
{
    SEALS,
    COMPONENTS
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

// Struct that will hold  component members such as int x or whatever
struct ComponentMember
{
    std::string memberName;
    ResolvedType type;       // Member type like int or whatever
    int memberIndex = -1;    // Will be used by reader's IRGen so lemme keep it
    bool isNullable = false; // Is the member nullable
    bool isMutable = false;
    bool isConstant = false;
    bool isRef = false;
    bool isPointer = false;
    StorageType storage;
};

// Struct that will hold a component method
struct ComponentMethod
{
    std::string methodName;
    ResolvedType returnType;
    std::vector<std::pair<ResolvedType, std::string>> paramTypes;
};

struct ComponentTable
{
    std::string componentName;
    std::vector<ComponentMember> members;
    std::vector<ComponentMethod> methods;
};

struct StubTable
{
    std::vector<SealTable> seals;
    std::vector<ComponentTable> components;
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
    void generateComponentStatement(Node *node);

    // Helper functions
    void registerStubGeneratorFns();
    std::string unmangle(const std::string &mangled);

    // Serializer functions
    inline void writeString(std::ostream &out, const std::string &str);
    inline void write_u8(std::ostream &out, uint8_t v);
    inline void write_s32(std::ostream &out, int32_t v);
    inline void write_u16(std::ostream &out, uint16_t v);
    inline void write_u32(std::ostream &out, uint32_t v);
    void serializeResolvedType(std::ostream &out, const ResolvedType &t);
    void serializeParamTypes(std::ostream &out, const std::vector<std::pair<ResolvedType, std::string>> &params);

    void serializeSealFunction(std::ostream &out, const SealFunction &fn);
    void serializeSealTable(std::ostream &out, const SealTable &seal);

    void serializeComponentMethod(std::ostream &out, const ComponentMethod &method);
    void serializeComponentMember(std::ostream &out, const ComponentMember &member);
    void serializeComponentTable(std::ostream &out, const ComponentTable &component);

    void serializeFullStubTable(const StubTable &table, const std::string &filename);
};