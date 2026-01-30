#include "ast.hpp"
#include "stubgen.hpp"
#include <fstream>
#include <string>

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"

StubGen::StubGen(Semantics &semantics, std::string &file, bool isVerbose)
    : semantics(semantics), fileName(file), isVerbose(isVerbose) {
  logInternal("StubGen initialized with file: " + fileName);
  registerStubGeneratorFns(); // Register the generators
}

void StubGen::registerStubGeneratorFns() {
  stubGenFnsMap[typeid(ComponentStatement)] =
      &StubGen::generateComponentStatement;
  stubGenFnsMap[typeid(SealStatement)] = &StubGen::generateSealStatement;
  stubGenFnsMap[typeid(RecordStatement)] = &StubGen::generateRecordStatement;
  stubGenFnsMap[typeid(EnumStatement)] = &StubGen::generateEnumStatement;
}

void StubGen::finish() {
  if (fileName.empty()) {
    // Fallback if the filename was never properly set
    fileName = "output";
  }

  std::string stubFile = fileName;
  auto pos = stubFile.rfind('.');
  if (pos != std::string::npos)
    stubFile = stubFile.substr(0, pos);

  stubFile += ".stub";

  logInternal("Total seals in stubTable " +
              std::to_string(stubTable.seals.size()));
  for (auto &seal : stubTable.seals) {
    logInternal("Seal '" + seal.sealName + "' has " +
                std::to_string(seal.sealFns.size()) + " functions");
  }

  serializeFullStubTable(stubTable, stubFile);

  logInternal("Stub file generated at: " + stubFile);
}

void StubGen::stubGenerator(Node *node) {
  auto stubIt = stubGenFnsMap.find(typeid(*node));
  if (stubIt == stubGenFnsMap.end()) {
    return;
  }

  (this->*stubIt->second)(node);
}

//_____________SERIALIZER HELPERS_____________
void StubGen::writeString(std::ostream &out, const std::string &str) {
  uint32_t len = static_cast<uint32_t>(str.size());
  out.write(reinterpret_cast<const char *>(&len), sizeof(len));
  out.write(str.data(), len);
}

void StubGen::write_u8(std::ostream &out, uint8_t v) {
  out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_s32(std::ostream &out, int32_t v) {
  out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_s64(std::ostream &out, int64_t v) {
  out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_u16(std::ostream &out, uint16_t v) {
  out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_u32(std::ostream &out, uint32_t v) {
  out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::serializeResolvedType(std::ostream &out, const ResolvedType &t) {
  // DataType
  write_u32(out, static_cast<uint32_t>(t.kind));

  // resolvedName
  writeString(out, t.resolvedName);

  // Flags
  write_u8(out, t.isPointer ? 1 : 0);
  write_u8(out, t.isRef ? 1 : 0);
  write_u8(out, t.isNull ? 1 : 0);
  write_u8(out, t.isArray ? 1 : 0);
}

void StubGen::serializeParamTypes(
    std::ostream &out,
    const std::vector<std::pair<ResolvedType, std::string>> &params) {
  write_u32(out, static_cast<uint32_t>(params.size()));

  for (const auto &p : params) {
    serializeResolvedType(out, p.first); // the type
    writeString(out, p.second);          // param name
  }
}

//__________________SEAL SERIALIZATION___________
void StubGen::serializeSealFunction(std::ostream &out, const SealFunction &fn) {
  writeString(out, fn.funcName);
  serializeResolvedType(out, fn.returnType);
  serializeParamTypes(out, fn.paramTypes);
}

void StubGen::serializeSealTable(std::ostream &out, const SealTable &seal) {
  writeString(out, seal.sealName);
  write_u32(out, (uint32_t)seal.sealFns.size());
  for (const auto &fn : seal.sealFns) {
    serializeSealFunction(out, fn);
  }
}

//_____________COMPONENT SERIALIZER_______________
void StubGen::serializeComponentMethod(std::ostream &out,
                                       const ComponentMethod &method) {
  writeString(out, method.methodName);
  serializeResolvedType(out, method.returnType);
  serializeParamTypes(out, method.paramTypes);
  write_u8(out, method.isFunction);
}

void StubGen::serializeComponentMember(std::ostream &out,
                                       const ComponentMember &member) {
  writeString(out, member.memberName);
  serializeResolvedType(out, member.type);
  write_s32(out, member.memberIndex);
  write_u8(out, member.isNullable);
  write_u8(out, member.isMutable);
  write_u8(out, member.isConstant);
  write_u8(out, member.isRef);
  write_u8(out, member.isPointer);
  write_u32(out, static_cast<uint32_t>(member.storage));
}

void StubGen::serializeComponentInit(std::ostream &out,
                                     const ComponentInit &init) {
  write_u32(out, (uint32_t)init.initArgs.size());
  for (const auto &type : init.initArgs) {
    serializeResolvedType(out, type);
  }
  serializeResolvedType(out, init.returnType);
  serializeResolvedType(out, init.type);
}

void StubGen::serializeComponentTable(std::ostream &out,
                                      const ComponentTable &component) {
  writeString(out, component.componentName); // Component name

  // MEMBERS SECTION
  write_u32(out, static_cast<uint32_t>(component.members.size()));
  for (const auto &member : component.members) {
    serializeComponentMember(out, member);
  }

  // METHODS SECTION
  write_u32(out, static_cast<uint32_t>(component.methods.size()));
  for (const auto &method : component.methods) {
    serializeComponentMethod(out, method);
  }

  // INIT CONSTRUCTOR SECTION
  write_u8(out, component.hasInit);
  if (component.hasInit) {
    serializeComponentInit(out, component.init);
  }
}

//________________RECORDS SERIALIZATION___________
void StubGen::serializeRecordMember(std::ostream &out,
                                    const RecordMember &member) {
  writeString(out, member.memberName);
  serializeResolvedType(out, member.type);
  write_s32(out, member.memberIndex);
  write_u8(out, member.isNullable);
  write_u8(out, member.isMutable);
  write_u8(out, member.isConstant);
  write_u8(out, member.isRef);
  write_u8(out, member.isPointer);
  write_u32(out, static_cast<uint32_t>(member.storage));
}

void StubGen::serializeRecordTable(std::ostream &out,
                                   const RecordTable &record) {
  writeString(out, record.recordName);

  write_u32(out, static_cast<uint32_t>(record.members.size()));
  for (const auto &member : record.members) {
    serializeRecordMember(out, member);
  }
}

//_____________ENUM SERIALIZATION___________________
void StubGen::serializeEnumMember(std::ostream &out,
                                  const EnumMembers &member) {
  writeString(out, member.memberName);
  serializeResolvedType(out, member.type);
  serializeResolvedType(out, member.enumType);
  write_s64(out, member.constantValue);
}

void StubGen::serializeEnumTable(std::ostream &out, const EnumTable &table) {
  writeString(out, table.enumName);
  serializeResolvedType(out, table.underlyingType);
  write_u32(out, static_cast<uint32_t>(table.members.size()));
  for (const auto &member : table.members) {
    serializeEnumMember(out, member);
  }
}

//________________FINAL STUB SERIALIZATION____________
void StubGen::serializeFullStubTable(const StubTable &table,
                                     const std::string &filename) {
  logInternal("Serializing stub table");
  std::ofstream out(filename, std::ios::binary);
  if (!out)
    throw std::runtime_error("Failed to open stub file");

  // HEADER
  write_u32(out, 0x53545542); // STUB
  write_u32(out, 1);          // Version
  write_u16(out, 4);          // Section count (4 for now)

  // SEALS
  write_u8(out, static_cast<uint8_t>(StubSection::SEALS));
  write_u32(out, static_cast<uint32_t>(table.seals.size())); // Seal count
  for (const auto &seal : table.seals) {
    serializeSealTable(out, seal);
  }

  // COMPONENTS
  write_u8(out, static_cast<uint8_t>(StubSection::COMPONENTS));
  write_u32(out,
            static_cast<uint32_t>(table.components.size())); // Component count
  for (const auto &component : table.components) {
    serializeComponentTable(out, component);
  }

  // RECORDS
  write_u8(out, static_cast<uint8_t>(StubSection::RECORDS));
  write_u32(out, static_cast<uint32_t>(table.records.size()));
  for (const auto &record : table.records) {
    serializeRecordTable(out, record);
  }

  // ENUMS
  write_u8(out, static_cast<uint8_t>(StubSection::ENUMS));
  write_u32(out, static_cast<uint32_t>(table.enums.size()));
  for (const auto &enums : table.enums) {
    serializeEnumTable(out, enums);
  }
}

// HELPERS
std::string StubGen::unmangle(const std::string &mangled) {
  auto pos = mangled.find('_');
  if (pos == std::string::npos)
    return mangled; // no prefix

  return mangled.substr(pos + 1); // everything after the first '_'
}

void StubGen::logInternal(const std::string &message) {
  if (isVerbose) {
    std::cout << message << "\n";
  }
}

void StubGen::reportDevBug(const std::string &message) {
  hasFailed = true;
  std::cerr << COLOR_RED << "[INTERNAL COMPILER ERROR] " << COLOR_RESET
            << message << "\n";
}

bool StubGen::failed() { return hasFailed; }
