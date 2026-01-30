#include "ast.hpp"
#include "semantics.hpp"
#include "typeindex"
#include <unordered_map>
#include <vector>

enum class StubSection : uint8_t { SEALS, COMPONENTS, RECORDS, ENUMS };

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
  ComponentInit init; // Every component only has one init so only one field
                      // here but its optional
};

struct RecordMember {
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

struct StubTable {
  std::vector<SealTable> seals;
  std::vector<ComponentTable> components;
  std::vector<RecordTable> records;
  std::vector<EnumTable> enums;
};

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

  // Helper functions
  void registerStubGeneratorFns();
  std::string unmangle(const std::string &mangled);

  // Serializer functions
  inline void writeString(std::ostream &out, const std::string &str);
  inline void write_u8(std::ostream &out, uint8_t v);
  inline void write_s32(std::ostream &out, int32_t v);
  inline void write_s64(std::ostream &out,int64_t v);
  inline void write_u16(std::ostream &out, uint16_t v);
  inline void write_u32(std::ostream &out, uint32_t v);
  void serializeResolvedType(std::ostream &out, const ResolvedType &t);
  void serializeParamTypes(
      std::ostream &out,
      const std::vector<std::pair<ResolvedType, std::string>> &params);

  void serializeSealFunction(std::ostream &out, const SealFunction &fn);
  void serializeSealTable(std::ostream &out, const SealTable &seal);

  void serializeComponentMethod(std::ostream &out,
                                const ComponentMethod &method);
  void serializeComponentMember(std::ostream &out,
                                const ComponentMember &member);
  void serializeComponentTable(std::ostream &out,
                               const ComponentTable &component);
  void serializeComponentInit(std::ostream &out, const ComponentInit &init);

  void serializeRecordMember(std::ostream &out, const RecordMember &member);
  void serializeRecordTable(std::ostream &out, const RecordTable &record);

  void serializeEnumMember(std::ostream &out, const EnumMembers &member);
  void serializeEnumTable(std::ostream &out, const EnumTable &enumTable);

  void serializeFullStubTable(const StubTable &table,
                              const std::string &filename);
  void logInternal(const std::string &message);
  void reportDevBug(const std::string &message);
};
