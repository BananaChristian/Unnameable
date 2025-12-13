#include "deserial.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <iostream>

// COLORS
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

    uint32_t magic = read_u32(in);
    if (magic != STUB_MAGIC)
        throw std::runtime_error("Invalid magic");

    uint32_t version = read_u32(in);
    if (version != STUB_VERSION)
        throw std::runtime_error("Unsupported stub version");

    stub = readStubTable(in);

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
}

void Deserializer::readOrFail(std::istream &in, void *dst, size_t size)
{
    if (!in.read(reinterpret_cast<char *>(dst), size))
    {
        throw std::runtime_error("Unexpected EOF while reading stub");
    }
}

uint8_t Deserializer::read_u8(std::istream &in)
{
    uint8_t v;
    readOrFail(in, &v, sizeof(v));
    return v;
}

uint16_t Deserializer::read_u16(std::istream &in)
{
    uint16_t v;
    readOrFail(in, &v, sizeof(v));
    return v;
}

uint32_t Deserializer::read_u32(std::istream &in)
{
    uint32_t v;
    readOrFail(in, &v, sizeof(v));
    return v;
}

std::string Deserializer::readString(std::istream &in)
{
    uint32_t len = read_u32(in);
    std::string s(len, '\0');
    readOrFail(in, s.data(), len);
    return s;
}

ImportedType Deserializer::readImportedType(std::istream &in)
{
    ImportedType t;
    t.kind = static_cast<ImportedDataType>(read_u32(in));
    t.resolvedName = readString(in);

    t.isPointer = read_u8(in);
    t.isRef = read_u8(in);
    t.isNull = read_u8(in);
    t.isArray = read_u8(in);
    uint8_t hasInner = read_u8(in);
    if (hasInner)
    {
        t.innerType = std::make_shared<ImportedType>(readImportedType(in));
    }

    return t;
}

RawStubTable Deserializer::readStubTable(std::istream &in)
{
    RawStubTable table;

    uint32_t sealCount = read_u32(in);
    table.seals.reserve(sealCount);

    for (uint32_t i = 0; i < sealCount; ++i)
    {
        RawSealTable seal;
        seal.sealName = readString(in);

        uint32_t fnCount = read_u32(in);
        seal.sealFns.reserve(fnCount);

        for (uint32_t j = 0; j < fnCount; ++j)
        {
            RawSealFunction fn;
            fn.funcName = readString(in);
            fn.returnType = readImportedType(in);

            uint32_t paramCount = read_u32(in);
            fn.paramTypes.reserve(paramCount);

            for (uint32_t k = 0; k < paramCount; ++k)
            {
                ImportedType paramType = readImportedType(in);
                std::string paramName = readString(in);
                fn.paramTypes.emplace_back(paramType, paramName);
            }

            seal.sealFns.push_back(std::move(fn));
        }

        table.seals.push_back(std::move(seal));
    }

    return table;
}