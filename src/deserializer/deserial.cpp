
#include "deserial.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iomanip>

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

namespace fs = std::filesystem;

Deserializer::Deserializer() {}

void Deserializer::processImports(const std::vector<std::unique_ptr<Node>> &nodes, const std::string &currentFile)
{
    for (auto &node : nodes)
    {
        if (auto import = dynamic_cast<ImportStatement *>(node.get()))
        {
            std::string resolved = resolveImportPath(import, currentFile);
            loadStub(resolved);
        }
    }
}

std::string Deserializer::resolveImportPath(ImportStatement *import, const std::string &currentFile)
{
    if (!import)
    {
        std::cerr << COLOR_RED << "[IMPORT ERROR]" << COLOR_RESET
                  << " invalid import node in file: " << currentFile << "\n";
        throw std::runtime_error("Invalid import node");
    }

    if (!import->stringExpr)
    {
        std::cerr << COLOR_RED << "[IMPORT ERROR]" << COLOR_RESET
                  << " invalid import string in file: " << currentFile << "\n";
        throw std::runtime_error("Invalid string node");
    }

    auto strLit = dynamic_cast<StringLiteral *>(import->stringExpr.get());
    std::string raw = strLit->string_token.TokenLiteral;

    std::cout << "IMPORT STRING: " << raw << "\n";

    fs::path currentDir = fs::path(currentFile).parent_path();
    fs::path importPath(raw);

    // If .stub wasnt added add it
    if (importPath.extension() != ".stub")
        importPath += ".stub";

    fs::path candidate = currentDir / importPath;

    // Canonicalize (weakly_canonical so we tolerate non-existing symlink parents)
    fs::path resolved;
    try
    {
        resolved = fs::weakly_canonical(candidate);
    }
    catch (const std::exception &e)
    {
        std::cerr << COLOR_RED << "[IMPORT ERROR]" << COLOR_RESET
                  << " failed to canonicalize import path: " << candidate.string()
                  << " (" << e.what() << ")\n";
        throw std::runtime_error("Failed to canonicalize path");
    }

    // Block climbing out of root directory
    fs::path projectRoot = fs::current_path();
    std::string projRootStr = projectRoot.string();
    std::string resolvedStr = resolved.string();

    if (resolvedStr.rfind(projRootStr, 0) != 0)
    {
        std::cerr << COLOR_RED << "[IMPORT ERROR]" << COLOR_RESET
                  << " import '" << raw << "' resolves outside project root: " << resolvedStr << "\n";
        throw std::runtime_error("Import resolves outside project root");
    }

    // If the stub file doesnt exist
    if (!fs::exists(resolved))
    {
        std::cerr << COLOR_RED << "[IMPORT ERROR]" << COLOR_RESET
                  << " stub file not found: " << resolvedStr << "\n";

        throw std::runtime_error("Stub not found: " + resolved.string());
    }

    if (loadedStubs.find(resolved.string()) != loadedStubs.end())
    {
        // Already loaded, just return it
        return resolved.string();
    }

    std::cout << COLOR_GREEN << "[IMPORT]" << COLOR_RESET << " Loaded stub: " << resolved.string() << "\n";

    return resolved.string();
}

void Deserializer::loadStub(const std::string &resolved)
{
    auto [it, inserted] = loadedStubs.emplace(resolved, resolved);
    if (!inserted)
        return;

    std::ifstream in(resolved, std::ios::binary);
    if (!in)
        throw std::runtime_error("Failed to open stub");

    stub = readStubTable(in);

    // Seal reading
    for (const auto &seal : stub.seals)
    {
        std::cout << "DESERIALIZER IMPORT SEAL NAME: " << seal.sealName << "\n";
        auto &sealMap = importedSealTable[seal.sealName];
        for (const auto &fn : seal.sealFns)
        {
            ImportedSymbolInfo info;
            info.returnType = fn.returnType;
            info.paramTypes = fn.paramTypes;
            sealMap.emplace(fn.funcName, std::move(info));
        }
    }

    // Component reading
    for (const auto &component : stub.components)
    {
        std::cout << "DESERIALIZER COMPONENT NAME: " << component.componentName << "\n";
        auto &compMap = importedComponentTable[component.componentName];
        for (const auto &member : component.members)
        {
            ImportedSymbolInfo info;
            info.type = member.type;
            info.memberIndex = member.memberIndex;
            info.isNullable = member.isNullable;
            info.isMutable = member.isMutable;
            info.isConstant = member.isConstant;
            info.isRef = member.isRef;
            info.isPointer = member.isPointer;
            info.storage = member.storage;

            compMap.emplace(member.memberName, std::move(info));
        }

        for (const auto &method : component.methods)
        {
            ImportedSymbolInfo info;
            info.returnType = method.returnType;
            info.paramTypes = method.paramTypes;
            info.isFunction = method.isFunction;

            compMap.emplace(method.methodName, std::move(info));
        }

        // Dealing with the init
        if (component.hasInit)
        {
            ImportedSymbolInfo info;
            info.initArgs = component.init.initArgs;
            info.returnType = component.init.returnType;
            info.type = component.init.type;

            // Store in a special init table
            importedInitTable[component.componentName] = info;
        }
    }

    // Data reading
    for (const auto &record : stub.records)
    {
        std::cout << "RECORD NAME: " << record.recordName << "\n";
        auto &recordMap = importedRecordsTable[record.recordName];
        for (const auto &member : record.members)
        {
            ImportedSymbolInfo info;
            info.type = member.type;
            info.memberIndex = member.memberIndex;
            info.isNullable = member.isNullable;
            info.isMutable = member.isMutable;
            info.isConstant = member.isConstant;
            info.isRef = member.isRef;
            info.isPointer = member.isPointer;
            info.storage = member.storage;

            recordMap.emplace(member.memberName, std::move(info));
        }
    }
}

void Deserializer::readOrFail(std::istream &in, void *dst, size_t size, const std::string &context)
{
    // Capture position before the read attempt
    std::streampos start_pos = in.tellg();

    // Attempt the read
    if (!in.read(reinterpret_cast<char *>(dst), size))
    {
        // On failure, clear flags to get true EOF/Fail status, then restore
        auto state = in.rdstate();
        in.clear();

        std::cerr << COLOR_RED << COLOR_BOLD << "[!!! FAILURE !!!]" << COLOR_RESET << "\n"
                  << "  Context: " << context << "\n"
                  << "  Read Attempt Failed: " << size << " bytes.\n"
                  << "  File Start Pos: " << start_pos << "\n"
                  << "  Flags (EOF/Fail/Bad): " << in.eof() << "/" << in.fail() << "/" << in.bad() << "\n";

        // Restore flags
        in.setstate(state);

        throw std::runtime_error("Unexpected EOF while reading stub");
    }
}

uint8_t Deserializer::read_u8(std::istream &in, const std::string &context)
{
    uint8_t v;
    std::streampos pos = in.tellg();
    readOrFail(in, &v, sizeof(v), context);
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET
              << "Pos: " << std::hex << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read u8: " << (int)v << "\t\t| Context: " << context << "\n";
    return v;
}

uint16_t Deserializer::read_u16(std::istream &in, const std::string &context)
{
    uint16_t v;
    std::streampos pos = in.tellg();
    readOrFail(in, &v, sizeof(v), context);
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET
              << "Pos: " << std::hex << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read u16: " << v << "\t\t| Context: " << context << "\n";
    return v;
}

uint32_t Deserializer::read_u32(std::istream &in, const std::string &context)
{
    uint32_t v;
    std::streampos pos = in.tellg();
    readOrFail(in, &v, sizeof(v), context);
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET
              << "Pos: " << std::hex << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read u32: " << v << "\t| Context: " << context << "\n";
    return v;
}

int32_t Deserializer::read_s32(std::istream &in, const std::string &context)
{
    int32_t v;
    std::streampos pos = in.tellg();
    readOrFail(in, &v, sizeof(v), context);
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET
              << "Pos: " << std::hex << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read s32: " << v << "\t| Context: " << context << "\n";
    return v;
}

std::string Deserializer::readString(std::istream &in, const std::string &context)
{
    uint32_t len = read_u32(in, "String Length: " + context);

    std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET
              << "Reading string of calculated length: " << len << "\t| Context: " << context << "\n";

    std::string s(len, '\0');

    readOrFail(in, s.data(), len, "String Data (" + std::to_string(len) + " bytes): " + context);

    std::cout << COLOR_GREEN << "[SUCCESS] " << COLOR_RESET
              << "Read string: '" << s << "'\n";

    return s;
}

ImportedType Deserializer::readImportedType(std::istream &in)
{
    ImportedType t;
    std::cout << COLOR_BLUE << "\n[FLOW] " << COLOR_RESET << "Starting ImportedType read.\n";

    t.kind = static_cast<ImportedDataType>(read_u32(in, "Type Kind"));
    t.resolvedName = readString(in, "Type Resolved Name");

    t.isPointer = read_u8(in, "Type Flag: isPointer");
    t.isRef = read_u8(in, "Type Flag: isRef");
    t.isNull = read_u8(in, "Type Flag: isNull");
    t.isArray = read_u8(in, "Type Flag: isArray");
    uint8_t hasInner = read_u8(in, "Type Flag: hasInnerType");

    if (hasInner)
    {
        std::cout << COLOR_BLUE << "[FLOW] " << COLOR_RESET << "Recursing for Inner Type.\n";
        t.innerType = std::make_shared<ImportedType>(readImportedType(in));
    }
    std::cout << COLOR_BLUE << "[FLOW] " << COLOR_RESET << "Finished ImportedType read.\n";

    return t;
}

std::vector<std::pair<ImportedType, std::string>> Deserializer::readParamTypes(std::istream &in)
{
    std::cout << COLOR_BLUE << "\n[FLOW] " << COLOR_RESET << "Starting Parameter List read.\n";
    uint32_t paramCount = read_u32(in, "Parameter Count");

    std::vector<std::pair<ImportedType, std::string>> params;
    params.reserve(paramCount);
    std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Expected " << paramCount << " parameters.\n";

    for (uint32_t i = 0; i < paramCount; ++i)
    {
        std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Reading Parameter " << i + 1 << " / " << paramCount << "\n";
        ImportedType type = readImportedType(in);
        std::string name = readString(in, "Parameter Name");
        params.emplace_back(std::move(type), std::move(name));
    }
    std::cout << COLOR_BLUE << "[FLOW] " << COLOR_RESET << "Finished Parameter List read.\n";

    return params;
}

//____________________COMPONENT MEMBERS READING_____________________
RawComponentMember Deserializer::readComponentMember(std::istream &in)
{
    std::cout << COLOR_BLUE << "\n[FLOW] " << COLOR_RESET << "Starting Component Member read.\n";
    RawComponentMember member;
    member.memberName = readString(in, "Member Name");
    member.type = readImportedType(in);
    member.memberIndex = read_s32(in, "Member Index");
    member.isNullable = read_u8(in, "Member Flag: isNullable");
    member.isMutable = read_u8(in, "Member Flag: isMutable");
    member.isConstant = read_u8(in, "Member Flag: isConstant");
    member.isRef = read_u8(in, "Member Flag: isRef");
    member.isPointer = read_u8(in, "Member Flag: isPointer");
    member.storage = static_cast<ImportedStorageType>(read_u32(in, "Member Storage Type"));
    std::cout << COLOR_BLUE << "[FLOW] " << COLOR_RESET << "Finished Component Member read.\n";

    return member;
}

RawComponentMethod Deserializer::readComponentMethod(std::istream &in)
{
    std::cout << COLOR_BLUE << "\n[FLOW] " << COLOR_RESET << "Starting Component Method read.\n";
    RawComponentMethod method;
    method.methodName = readString(in, "Method Name");
    method.returnType = readImportedType(in);
    method.paramTypes = readParamTypes(in);
    method.isFunction = read_u8(in, "isFunction");
    std::cout << COLOR_BLUE << "[FLOW] " << COLOR_RESET << "Finished Component Method read.\n";

    return method;
}

RawComponentInit Deserializer::readComponentInit(std::istream &in)
{
    std::cout << COLOR_BLUE << "\n[FLOW] " << COLOR_RESET << "Starting Component Init read.\n";
    RawComponentInit init;

    uint32_t argCount = read_u32(in, "Init Argument Count");
    init.initArgs.reserve(argCount);
    for (uint32_t i = 0; i < argCount; ++i)
    {
        init.initArgs.push_back(readImportedType(in));
    }

    init.returnType = readImportedType(in);
    init.type = readImportedType(in);

    std::cout << COLOR_BLUE << "[FLOW] " << COLOR_RESET << "Finished Component Init read.\n";
    return init;
}

//__________________DATA MEMBER READING_______________
RawRecordMember Deserializer::readRecordMember(std::istream &in)
{
    std::cout << COLOR_BLUE << "\n[FLOW] " << COLOR_RESET << "Starting Record Member read.\n";
    RawRecordMember member;
    member.memberName = readString(in, "Record Name");
    member.type = readImportedType(in);
    member.memberIndex = read_s32(in, "Member Index");
    member.isNullable = read_u8(in, "Member Flag: isNullable");
    member.isMutable = read_u8(in, "Member Flag: isMutable");
    member.isConstant = read_u8(in, "Member Flag: isConstant");
    member.isRef = read_u8(in, "Member Flag: isRef");
    member.isPointer = read_u8(in, "Member Flag: isPointer");
    member.storage = static_cast<ImportedStorageType>(read_u32(in, "Member Storage Type"));
    std::cout << COLOR_BLUE << "[FLOW] " << COLOR_RESET << "Finished Record Member read.\n";

    return member;
}

//___________________STUB TABLE READ___________________

RawStubTable Deserializer::readStubTable(std::istream &in)
{
    RawStubTable table;
    std::cout << COLOR_BOLD << "\n--- STARTING STUB TABLE DESERIALIZATION ---\n"
              << COLOR_RESET;

    // HEADER
    uint32_t magic = read_u32(in, "Header: Magic Number (STUB)");
    if (magic != STUB_MAGIC)
    {
        std::cerr << COLOR_RED << "[FATAL] " << COLOR_RESET << "Invalid stub magic: Expected "
                  << std::hex << STUB_MAGIC << " but got " << magic << std::dec << "\n";
        throw std::runtime_error("Invalid stub magic");
    }

    uint32_t version = read_u32(in, "Header: Version (u32)"); // NOTE: Deserializer reads u32, serializer writes u32
    if (version != STUB_VERSION)
    {
        std::cerr << COLOR_RED << "[FATAL] " << COLOR_RESET << "Unsupported stub version: Expected "
                  << STUB_VERSION << " but got " << version << "\n";
        throw std::runtime_error("Unsupported stub version");
    }

    uint16_t sectionCount = read_u16(in, "Header: Section Count");
    std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Expecting " << sectionCount << " sections.\n";

    // SECTIONS
    for (uint16_t s = 0; s < sectionCount; ++s)
    {
        std::cout << COLOR_BOLD << "\n--- READING SECTION " << s + 1 << " / " << sectionCount << " ---\n"
                  << COLOR_RESET;
        ImportedStubSection section = static_cast<ImportedStubSection>(read_u8(in, "Section Type ID"));
        uint32_t entryCount = read_u32(in, "Section Entry Count");
        std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Section ID: " << (int)section << ", Expected Entries: " << entryCount << "\n";

        switch (section)
        {
        case ImportedStubSection::SEALS:
        {
            if (entryCount > 1000000)
            { // No stub should have 1 million entries
                throw std::runtime_error("Stub corruption detected: absurdly high entry count (" + std::to_string(entryCount) + ")");
            }
            table.seals.reserve(entryCount);

            for (uint32_t i = 0; i < entryCount; ++i)
            {
                std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET << " Reading Seal " << i + 1 << " of " << entryCount << "\n";
                RawSealTable seal;

                seal.sealName = readString(in, "Seal Table Name");

                uint32_t fnCount = read_u32(in, "Seal Function Count");
                std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Seal '" << seal.sealName << "' expects " << fnCount << " functions.\n";

                seal.sealFns.reserve(fnCount);

                for (uint32_t j = 0; j < fnCount; ++j)
                {
                    std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET << " Reading Function " << j + 1 << " of " << fnCount << " for Seal '" << seal.sealName << "'\n";
                    RawSealFunction fn;
                    fn.funcName = readString(in, "Seal Function Name");
                    fn.returnType = readImportedType(in);
                    fn.paramTypes = readParamTypes(in);

                    seal.sealFns.push_back(std::move(fn));
                }

                table.seals.push_back(std::move(seal));
            }
            break;
        }

        case ImportedStubSection::COMPONENTS:
        {
            if (entryCount > 1000000)
            {
                throw std::runtime_error("Stub corruption detected: absurdly high entry count (" + std::to_string(entryCount) + ")");
            }
            table.components.reserve(entryCount);

            for (uint32_t i = 0; i < entryCount; ++i)
            {
                std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET << " Reading Component " << i + 1 << " of " << entryCount << "\n";
                RawComponentTable comp;
                comp.componentName = readString(in, "Component Table Name");

                // members
                uint32_t memberCount = read_u32(in, "Component Member Count");
                comp.members.reserve(memberCount);
                std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Component '" << comp.componentName << "' expects " << memberCount << " members.\n";
                for (uint32_t m = 0; m < memberCount; ++m)
                {
                    comp.members.push_back(readComponentMember(in));
                }

                // methods
                uint32_t methodCount = read_u32(in, "Component Method Count");
                comp.methods.reserve(methodCount);
                std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Component '" << comp.componentName << "' expects " << methodCount << " methods.\n";
                for (uint32_t m = 0; m < methodCount; ++m)
                {
                    comp.methods.push_back(readComponentMethod(in));
                }

                // Init
                bool hasInit = read_u8(in, "Has Init");
                comp.hasInit = hasInit;
                if (hasInit)
                {
                    comp.init = readComponentInit(in);
                }

                std::cout << "DEBUG: Finishing Component " << i + 1 << " at Pos: " << in.tellg() << "\n";
                table.components.push_back(std::move(comp));
            }
            break;
        }
        case ImportedStubSection::DATA:
        {
            if (entryCount > 1000000)
            {
                throw std::runtime_error("Stub corruption detected: absurdly high entry count (" + std::to_string(entryCount) + ")");
            }
            table.records.reserve(entryCount);

            for (uint32_t i = 0; i < entryCount; i++)
            {
                std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET << " Reading Data " << i + 1 << " of " << entryCount << "\n";
                RawRecordTable record;
                record.recordName = readString(in, "Record Table Name");

                // Members
                uint32_t memberCount = read_u32(in, "Record Member Count");
                record.members.reserve(memberCount);
                std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Record '" << record.recordName << "' expects " << memberCount << " members.\n";
                for (uint32_t m = 0; m < memberCount; ++m)
                {
                    record.members.push_back(readRecordMember(in));
                }

                table.records.push_back(std::move(record));
            }
            break;
        }

        default:
            std::cerr << COLOR_RED << "[FATAL] " << COLOR_RESET << "Unknown stub section ID: " << (int)section << "\n";
            throw std::runtime_error("Unknown stub section");
        }
    }

    std::cout << COLOR_BOLD << "\n--- FINISHED STUB TABLE DESERIALIZATION ---\n"
              << COLOR_RESET;
    return table;
}