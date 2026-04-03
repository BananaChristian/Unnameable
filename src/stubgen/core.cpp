#include <fstream>
#include <ostream>
#include <string>

#include "ast.hpp"
#include "stubgen.hpp"

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"

StubGen::StubGen(Semantics& semantics, std::string& file, bool isVerbose)
    : semantics(semantics), fileName(file), isVerbose(isVerbose) {
    logInternal("StubGen initialized with file: " + fileName);
    registerStubGeneratorFns();  // Register the generators
}

void StubGen::registerStubGeneratorFns() {
    stubGenFnsMap[typeid(ComponentStatement)] = &StubGen::generateComponentStatement;
    stubGenFnsMap[typeid(SealStatement)] = &StubGen::generateSealStatement;
    stubGenFnsMap[typeid(RecordStatement)] = &StubGen::generateRecordStatement;
    stubGenFnsMap[typeid(EnumStatement)] = &StubGen::generateEnumStatement;
    stubGenFnsMap[typeid(AllocatorStatement)] = &StubGen::generateAllocatorStatement;
    stubGenFnsMap[typeid(InstantiateStatement)] = &StubGen::generateInstantiateStatement;
}

void StubGen::finish() {
    if (fileName.empty()) {
        // Fallback if the filename was never properly set
        fileName = "output";
    }

    std::string stubFile = fileName;
    auto pos = stubFile.rfind('.');
    if (pos != std::string::npos) stubFile = stubFile.substr(0, pos);

    stubFile += ".stub";

    logInternal("Total seals in stubTable " + std::to_string(stubTable.seals.size()));
    for (auto& seal : stubTable.seals) {
        logInternal("Seal '" + seal.sealName + "' has " + std::to_string(seal.sealFns.size()) +
                    " functions");
    }

    serializeFullStubTable(stubTable, stubFile);

    logInternal("Stub file generated at: " + stubFile);
}

void StubGen::stubGenerator(Node* node) {
    auto stubIt = stubGenFnsMap.find(typeid(*node));
    if (stubIt == stubGenFnsMap.end()) {
        return;
    }

    (this->*stubIt->second)(node);
}

//_____________SERIALIZER HELPERS_____________
void StubGen::writeString(std::ostream& out, const std::string& str) {
    uint32_t len = static_cast<uint32_t>(str.size());
    out.write(reinterpret_cast<const char*>(&len), sizeof(len));
    out.write(str.data(), len);
}

void StubGen::write_u8(std::ostream& out, uint8_t v) {
    out.write(reinterpret_cast<const char*>(&v), sizeof(v));
}

void StubGen::write_s32(std::ostream& out, int32_t v) {
    out.write(reinterpret_cast<const char*>(&v), sizeof(v));
}

void StubGen::write_s64(std::ostream& out, int64_t v) {
    out.write(reinterpret_cast<const char*>(&v), sizeof(v));
}

void StubGen::write_u16(std::ostream& out, uint16_t v) {
    out.write(reinterpret_cast<const char*>(&v), sizeof(v));
}

void StubGen::write_u32(std::ostream& out, uint32_t v) {
    out.write(reinterpret_cast<const char*>(&v), sizeof(v));
}

void StubGen::write_u64(std::ostream& out, uint64_t v) {
    out.write(reinterpret_cast<const char*>(&v), sizeof(v));
}

void StubGen::serializeResolvedType(std::ostream& out, const ResolvedType& t) {
    write_u8(out, static_cast<uint8_t>(t.modifier));  // u8 not u32
    write_u8(out, static_cast<uint8_t>(t.kind));      // u8 not u32
    writeString(out, t.resolvedName);
    write_u64(out, t.arraySize);
    write_u8(out, static_cast<uint8_t>(t.isConstantSize));
    write_u8(out, static_cast<uint8_t>(t.isNull));

    bool hasInner = (t.innerType != nullptr);
    write_u8(out, static_cast<uint8_t>(hasInner));  // last, not first
    if (hasInner) serializeResolvedType(out, *t.innerType);
}

void StubGen::serializeParamTypes(std::ostream& out,
                                  const std::vector<std::pair<ResolvedType, std::string>>& params) {
    write_u32(out, static_cast<uint32_t>(params.size()));

    for (const auto& p : params) {
        serializeResolvedType(out, p.first);  // the type
        writeString(out, p.second);           // param name
    }
}

//__________________SEAL SERIALIZATION___________
void StubGen::serializeSealFunction(std::ostream& out, const SealFunction& fn) {
    writeString(out, fn.funcName);
    serializeResolvedType(out, fn.returnType);
    serializeParamTypes(out, fn.paramTypes);
}

void StubGen::serializeSealTable(std::ostream& out, const SealTable& seal) {
    writeString(out, seal.sealName);
    write_u32(out, (uint32_t)seal.sealFns.size());
    for (const auto& fn : seal.sealFns) {
        serializeSealFunction(out, fn);
    }
}

//_____________COMPONENT SERIALIZER_______________
void StubGen::serializeComponentMethod(std::ostream& out, const ComponentMethod& method) {
    writeString(out, method.methodName);
    serializeResolvedType(out, method.returnType);
    serializeParamTypes(out, method.paramTypes);
    write_u8(out, method.isFunction);
}

void StubGen::serializeComponentMember(std::ostream& out, const ComponentMember& member) {
    writeString(out, member.memberName);
    serializeResolvedType(out, member.type);
    write_s32(out, member.memberIndex);
    write_u8(out, member.isNullable);
    write_u8(out, member.isMutable);
    write_u8(out, member.isConstant);
    write_u8(out, member.isRef);
    write_u8(out, member.isPointer);
}

void StubGen::serializeComponentInit(std::ostream& out, const ComponentInit& init) {
    serializeResolvedType(out, init.returnType);     // returnType first
    serializeResolvedType(out, init.type);           // type second
    write_u32(out, static_cast<uint32_t>(init.initArgs.size()));  // count third
    for (const auto& type : init.initArgs) serializeResolvedType(out, type);
}

void StubGen::serializeComponentTable(std::ostream& out, const ComponentTable& component) {
    writeString(out, component.componentName);  // Component name

    // MEMBERS SECTION
    write_u32(out, static_cast<uint32_t>(component.members.size()));
    for (const auto& member : component.members) {
        serializeComponentMember(out, member);
    }

    // METHODS SECTION
    write_u32(out, static_cast<uint32_t>(component.methods.size()));
    for (const auto& method : component.methods) {
        serializeComponentMethod(out, method);
    }

    // INIT CONSTRUCTOR SECTION
    write_u8(out, component.hasInit);
    if (component.hasInit) {
        serializeComponentInit(out, component.init);
    }
}

//________________RECORDS SERIALIZATION___________
void StubGen::serializeRecordMember(std::ostream& out, const RecordMember& member) {
    writeString(out, member.memberName);
    serializeResolvedType(out, member.type);
    write_s32(out, member.memberIndex);
    write_u8(out, member.isNullable);
    write_u8(out, member.isMutable);
    write_u8(out, member.isConstant);
    write_u8(out, member.isRef);
    write_u8(out, member.isPointer);
}

void StubGen::serializeRecordTable(std::ostream& out, const RecordTable& record) {
    writeString(out, record.recordName);

    write_u32(out, static_cast<uint32_t>(record.members.size()));
    for (const auto& member : record.members) {
        serializeRecordMember(out, member);
    }
}

//_____________ENUM SERIALIZATION___________________
void StubGen::serializeEnumMember(std::ostream& out, const EnumMembers& member) {
    writeString(out, member.memberName);
    serializeResolvedType(out, member.type);
    serializeResolvedType(out, member.enumType);
    write_s64(out, member.constantValue);
}

void StubGen::serializeEnumTable(std::ostream& out, const EnumTable& table) {
    writeString(out, table.enumName);
    serializeResolvedType(out, table.underlyingType);
    write_u32(out, static_cast<uint32_t>(table.members.size()));
    for (const auto& member : table.members) {
        serializeEnumMember(out, member);
    }
}

//___________________ALLOCATOR SERIALIZATION________________
void StubGen::serializeAllocatorFunction(std::ostream& out, const AllocatorFunction& function) {
    writeString(out, function.functionName);
    serializeResolvedType(out, function.returnType);
    serializeParamTypes(out, function.paramTypes);
}

void StubGen::serializeAllocator(std::ostream& out, const Allocator& allocator) {
    writeString(out, allocator.allocatorName);
    serializeAllocatorFunction(out, allocator.allocator);
    serializeAllocatorFunction(out, allocator.free);
}

//________________FUNCTION ENTRY SERIALIZATION_____________
void StubGen::serializeFunctionEntry(std::ostream& out, const FunctionEntry& entry) {
    writeString(out, entry.funcName);
    serializeResolvedType(out, entry.returnType);
    serializeParamTypes(out, entry.paramTypes);
}

//__________GENERICS SERIALIZATION______________________
void StubGen::serializeGenerics(std::ostream& out, const Generics& generic) {
    writeString(out, generic.aliasName);
    // components first to match reader
    write_u32(out, generic.components.size());
    for (const auto& component : generic.components) serializeComponentTable(out, component);
    // records second
    write_u32(out, generic.records.size());
    for (const auto& record : generic.records) serializeRecordTable(out, record);
    // functions third
    write_u32(out, generic.functions.size());
    for (const auto& func : generic.functions) serializeFunctionEntry(out, func);
}

//________________FINAL STUB SERIALIZATION____________
void StubGen::serializeFullStubTable(const StubTable& table, const std::string& filename) {
    logInternal("Serializing stub table");
    std::ofstream out(filename, std::ios::binary);
    if (!out) throw std::runtime_error("Failed to open stub file");

    // HEADER
    write_u32(out, 0x53545542);  // STUB
    write_u32(out, 1);           // Version
    write_u16(out, 7);           // Section count (7 for now)

    // SEALS
    write_u8(out, static_cast<uint8_t>(StubSection::SEALS));
    write_u32(out, static_cast<uint32_t>(table.seals.size()));  // Seal count
    for (const auto& seal : table.seals) {
        serializeSealTable(out, seal);
    }

    // COMPONENTS
    write_u8(out, static_cast<uint8_t>(StubSection::COMPONENTS));
    write_u32(out,
              static_cast<uint32_t>(table.components.size()));  // Component count
    for (const auto& component : table.components) {
        serializeComponentTable(out, component);
    }

    // RECORDS
    write_u8(out, static_cast<uint8_t>(StubSection::RECORDS));
    write_u32(out, static_cast<uint32_t>(table.records.size()));
    for (const auto& record : table.records) {
        serializeRecordTable(out, record);
    }

    // ENUMS
    write_u8(out, static_cast<uint8_t>(StubSection::ENUMS));
    write_u32(out, static_cast<uint32_t>(table.enums.size()));
    for (const auto& enums : table.enums) {
        serializeEnumTable(out, enums);
    }

    // ALLOCATORS
    write_u8(out, static_cast<uint8_t>(StubSection::ALLOCATORS));
    write_u32(out, static_cast<uint32_t>(table.allocators.size()));
    for (const auto& allocator : table.allocators) {
        serializeAllocator(out, allocator);
    }

    // FUNCTIONS
    write_u8(out, static_cast<uint8_t>(StubSection::FUNCTIONS));
    write_u32(out, static_cast<uint32_t>(table.functions.size()));
    for (const auto& func : table.functions) {
        serializeFunctionEntry(out, func);
    }

    // GENERICS
    write_u8(out, static_cast<uint8_t>(StubSection::GENERICS));
    write_u32(out, static_cast<uint32_t>(table.generics.size()));
    for (const auto& generic : table.generics) {
        serializeGenerics(out, generic);
    }
}

// HELPERS
std::string StubGen::unmangle(const std::string& mangled) {
    auto pos = mangled.find('_');
    if (pos == std::string::npos) return mangled;  // no prefix

    return mangled.substr(pos + 1);  // everything after the first '_'
}

void StubGen::logInternal(const std::string& message) {
    if (isVerbose) {
        std::cout << message << "\n";
    }
}

void StubGen::reportDevBug(const std::string& message) {
    hasFailed = true;
    std::cerr << COLOR_RED << "[INTERNAL COMPILER ERROR] " << COLOR_RESET << message << "\n";
    std::abort();
}

bool StubGen::failed() { return hasFailed; }
