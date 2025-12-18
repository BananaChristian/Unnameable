#include "stubgen.hpp"
#include "typeindex"

StubGen::StubGen(Semantics &semantics, std::string &file) : semantics(semantics), fileName(file)
{
    registerStubGeneratorFns(); // Register the generators
}

void StubGen::registerStubGeneratorFns()
{
    stubGenFnsMap[typeid(ComponentStatement)] = &StubGen::generateComponentStatement;
    stubGenFnsMap[typeid(SealStatement)] = &StubGen::generateSealStatement;
    stubGenFnsMap[typeid(DataStatement)] = &StubGen::generateDataStatement;
}

void StubGen::finish()
{
    std::string stubFile = fileName;
    auto pos = stubFile.rfind('.');
    if (pos != std::string::npos)
        stubFile = stubFile.substr(0, pos);

    stubFile += ".stub";

    std::cout << "[DEBUG] Total seals in stubTable: " << stubTable.seals.size() << "\n";
    for (auto &seal : stubTable.seals)
        std::cout << "[DEBUG] Seal " << seal.sealName
                  << " has " << seal.sealFns.size() << " functions.\n";

    serializeFullStubTable(stubTable, stubFile);

    std::cout << "Stub file generated at: " << stubFile << "\n";
}

void StubGen::stubGenerator(Node *node)
{
    auto stubIt = stubGenFnsMap.find(typeid(*node));
    if (stubIt == stubGenFnsMap.end())
    {
        return;
    }

    (this->*stubIt->second)(node);
}

//_____________SERIALIZER HELPERS_____________
void StubGen::writeString(std::ostream &out, const std::string &str)
{
    uint32_t len = static_cast<uint32_t>(str.size());
    out.write(reinterpret_cast<const char *>(&len), sizeof(len));
    out.write(str.data(), len);
}

void StubGen::write_u8(std::ostream &out, uint8_t v)
{
    out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_s32(std::ostream &out, int32_t v)
{
    out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_u16(std::ostream &out, uint16_t v)
{
    out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_u32(std::ostream &out, uint32_t v)
{
    out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::serializeResolvedType(std::ostream &out, const ResolvedType &t)
{
    // DataType
    write_u32(out, static_cast<uint32_t>(t.kind));

    // resolvedName
    writeString(out, t.resolvedName);

    // Flags
    write_u8(out, t.isPointer ? 1 : 0);
    write_u8(out, t.isRef ? 1 : 0);
    write_u8(out, t.isNull ? 1 : 0);
    write_u8(out, t.isArray ? 1 : 0);

    // Inner type prescence
    uint8_t hasInner = (t.innerType != nullptr) ? 1 : 0;
    write_u8(out, hasInner);

    if (hasInner)
    {
        serializeResolvedType(out, *t.innerType);
    }
}

void StubGen::serializeParamTypes(std::ostream &out, const std::vector<std::pair<ResolvedType, std::string>> &params)
{
    write_u32(out, static_cast<uint32_t>(params.size()));

    for (const auto &p : params)
    {
        serializeResolvedType(out, p.first); // the type
        writeString(out, p.second);          // param name
    }
}

//__________________SEAL SERIALIZATION___________
void StubGen::serializeSealFunction(std::ostream &out, const SealFunction &fn)
{
    writeString(out, fn.funcName);
    serializeResolvedType(out, fn.returnType);
    serializeParamTypes(out, fn.paramTypes);
}

void StubGen::serializeSealTable(std::ostream &out, const SealTable &seal)
{
    writeString(out, seal.sealName);
    write_u32(out, (uint32_t)seal.sealFns.size());
    for (const auto &fn : seal.sealFns)
    {
        serializeSealFunction(out, fn);
    }
}

//_____________COMPONENT SERIALIZER_______________
void StubGen::serializeComponentMethod(std::ostream &out, const ComponentMethod &method)
{
    writeString(out, method.methodName);
    serializeResolvedType(out, method.returnType);
    serializeParamTypes(out, method.paramTypes);
    write_u8(out, method.isFunction);
}

void StubGen::serializeComponentMember(std::ostream &out, const ComponentMember &member)
{
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

void StubGen::serializeComponentTable(std::ostream &out, const ComponentTable &component)
{
    writeString(out, component.componentName); // Component name

    // MEMBERS SECTION
    write_u32(out, (uint32_t)component.members.size());
    for (const auto &member : component.members)
    {
        serializeComponentMember(out, member);
    }

    // METHODS SECTION
    write_u32(out, (uint32_t)component.methods.size());
    for (const auto &method : component.methods)
    {
        serializeComponentMethod(out, method);
    }
}

//________________DATA BLOCKS SERIALIZATION___________
void StubGen::serializeDataMember(std::ostream &out, const DataMember &member)
{
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

void StubGen::serializeDataTable(std::ostream &out, const DataTable &data)
{
    writeString(out, data.dataName);

    write_u32(out, (uint32_t)data.members.size());
    for (const auto &member : data.members)
    {
        serializeDataMember(out, member);
    }
}

//________________FINAL STUB SERIALIZATION____________
void StubGen::serializeFullStubTable(const StubTable &table, const std::string &filename)
{
    std::cout << "Serializing stub table\n";
    std::ofstream out(filename, std::ios::binary);
    if (!out)
        throw std::runtime_error("Failed to open stub file");

    // HEADER
    write_u32(out, 0x53545542); // STUB
    write_u32(out, 1);          // Version
    write_u16(out, 3);          // Section count (3 for now)

    // SEALS SECTION
    write_u8(out, (uint8_t)StubSection::SEALS);
    write_u32(out, (uint32_t)table.seals.size()); // Seal count
    for (const auto &seal : table.seals)
    {
        serializeSealTable(out, seal);
    }

    // COMPONENTS
    write_u8(out, (uint8_t)StubSection::COMPONENTS);
    write_u32(out, (uint32_t)table.components.size()); // Component count
    for (const auto &component : table.components)
    {
        serializeComponentTable(out, component);
    }

    // DATA
    write_u8(out, (uint8_t)StubSection::DATA);
    write_u32(out, (uint32_t)table.data.size());
    for (const auto &data : table.data)
    {
        serializeDataTable(out, data);
    }
}

// HELPERS
std::string StubGen::unmangle(const std::string &mangled)
{
    auto pos = mangled.find('_');
    if (pos == std::string::npos)
        return mangled; // no prefix

    return mangled.substr(pos + 1); // everything after the first '_'
}
