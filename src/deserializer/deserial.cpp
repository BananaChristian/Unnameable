#include "deserial.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <istream>
#include <stdexcept>
namespace fs = std::filesystem;

Deserializer::Deserializer(ErrorHandler& handler, bool verbose)
    : errorHandler(handler), isVerbose(verbose) {}

bool Deserializer::failed() { return hasFailed; }

void Deserializer::readOrFail(std::istream& in, void* dst, size_t size,
                              const std::string& context) {
    if (!in.read(reinterpret_cast<char*>(dst), size)) reportDevBug("Read failed at: " + context);
}

uint8_t Deserializer::read_u8(std::istream& in, const std::string& ctx) {
    uint8_t v;
    readOrFail(in, &v, 1, ctx);
    return v;
}

uint16_t Deserializer::read_u16(std::istream& in, const std::string& ctx) {
    uint16_t v;
    readOrFail(in, &v, 2, ctx);
    return v;
}

uint32_t Deserializer::read_u32(std::istream& in, const std::string& ctx) {
    uint32_t v;
    readOrFail(in, &v, 4, ctx);
    return v;
}

uint64_t Deserializer::read_u64(std::istream& in, const std::string& ctx) {
    uint64_t v;
    readOrFail(in, &v, 8, ctx);
    return v;
}

int32_t Deserializer::read_s32(std::istream& in, const std::string& ctx) {
    int32_t v;
    readOrFail(in, &v, 4, ctx);
    return v;
}

int64_t Deserializer::read_s64(std::istream& in, const std::string& ctx) {
    int64_t v;
    readOrFail(in, &v, 8, ctx);
    return v;
}

std::string Deserializer::readString(std::istream& in, const std::string& ctx) {
    uint32_t len = read_u32(in, ctx + "/len");
    std::string s(len, '\0');
    readOrFail(in, s.data(), len, ctx + "/data");
    return s;
}

ResolvedType Deserializer::readResolvedType(std::istream& in) {
    ResolvedType t;
    t.modifier = static_cast<Modifier>(read_u8(in, "type/modifier"));
    t.kind = static_cast<DataType>(read_u8(in, "type/kind"));
    t.resolvedName = readString(in, "type/resolvedName");
    t.arraySize = read_u64(in, "type/arraySize");
    t.isConstantSize = static_cast<bool>(read_u8(in, "type/isConstantSize"));
    t.isNull = static_cast<bool>(read_u8(in, "type/isNull"));

    bool hasInner = static_cast<bool>(read_u8(in, "type/hasInner"));
    if (hasInner) t.innerType = std::make_shared<ResolvedType>(readResolvedType(in));

    return t;
}

std::vector<std::pair<ResolvedType, std::string>> Deserializer::readParamTypes(std::istream& in) {
    uint32_t count = read_u32(in, "params/count");
    std::vector<std::pair<ResolvedType, std::string>> params;
    params.reserve(count);
    for (uint32_t i = 0; i < count; ++i) {
        ResolvedType type = readResolvedType(in);
        std::string name = readString(in, "params/name");
        params.emplace_back(std::move(type), std::move(name));
    }
    return params;
}

SealFunction Deserializer::readSealFunction(std::istream& in) {
    SealFunction fn;
    fn.funcName = readString(in, "sealFnname");
    fn.returnType = readResolvedType(in);
    fn.paramTypes = readParamTypes(in);
    return fn;
}

SealTable Deserializer::readSealTable(std::istream& in) {
    SealTable seal;
    seal.sealName = readString(in, "seal/name");
    uint32_t fnCount = read_u32(in, "seal/fnCount");
    seal.sealFns.reserve(fnCount);
    for (uint32_t i = 0; i < fnCount; ++i) seal.sealFns.push_back(readSealFunction(in));
    return seal;
}

ComponentMember Deserializer::readComponentMember(std::istream& in) {
    ComponentMember m;
    m.memberName = readString(in, "compMember/name");
    m.type = readResolvedType(in);
    m.memberIndex = read_s32(in, "compMember/index");
    m.isNullable = static_cast<bool>(read_u8(in, "compMember/nullable"));
    m.isMutable = static_cast<bool>(read_u8(in, "compMember/mutable"));
    m.isConstant = static_cast<bool>(read_u8(in, "compMember/constant"));
    m.isRef = static_cast<bool>(read_u8(in, "compMember/ref"));
    m.isPointer = static_cast<bool>(read_u8(in, "compMember/pointer"));
    return m;
}

ComponentMethod Deserializer::readComponentMethod(std::istream& in) {
    ComponentMethod m;
    m.methodName = readString(in, "compMethod/name");
    m.returnType = readResolvedType(in);
    m.paramTypes = readParamTypes(in);
    m.isFunction = static_cast<bool>(read_u8(in, "compMethod/isFunction"));
    return m;
}

ComponentInit Deserializer::readComponentInit(std::istream& in) {
    ComponentInit init;
    init.returnType = readResolvedType(in);
    init.type = readResolvedType(in);
    uint32_t argCount = read_u32(in, "compInit/argCount");
    init.initArgs.reserve(argCount);
    for (uint32_t i = 0; i < argCount; ++i) init.initArgs.push_back(readResolvedType(in));
    return init;
}

ComponentTable Deserializer::readComponentTable(std::istream& in) {
    ComponentTable comp;
    comp.componentName = readString(in, "comp/name");

    uint32_t memberCount = read_u32(in, "comp/memberCount");
    comp.members.reserve(memberCount);
    for (uint32_t i = 0; i < memberCount; ++i) comp.members.push_back(readComponentMember(in));

    uint32_t methodCount = read_u32(in, "comp/methodCount");
    comp.methods.reserve(methodCount);
    for (uint32_t i = 0; i < methodCount; ++i) comp.methods.push_back(readComponentMethod(in));

    comp.hasInit = static_cast<bool>(read_u8(in, "comp/hasInit"));
    if (comp.hasInit) comp.init = readComponentInit(in);

    return comp;
}

RecordMember Deserializer::readRecordMember(std::istream& in) {
    RecordMember m;
    m.memberName = readString(in, "recMember/name");
    m.type = readResolvedType(in);
    m.memberIndex = read_s32(in, "recMember/index");
    m.isNullable = static_cast<bool>(read_u8(in, "recMember/nullable"));
    m.isMutable = static_cast<bool>(read_u8(in, "recMember/mutable"));
    m.isConstant = static_cast<bool>(read_u8(in, "recMember/constant"));
    m.isRef = static_cast<bool>(read_u8(in, "recMember/ref"));
    m.isPointer = static_cast<bool>(read_u8(in, "recMember/pointer"));
    return m;
}

RecordTable Deserializer::readRecordTable(std::istream& in) {
    RecordTable record;
    record.recordName = readString(in, "record/name");
    uint32_t memberCount = read_u32(in, "record/memberCount");
    record.members.reserve(memberCount);
    for (uint32_t i = 0; i < memberCount; ++i) record.members.push_back(readRecordMember(in));
    return record;
}

EnumMembers Deserializer::readEnumMember(std::istream& in) {
    EnumMembers m;
    m.memberName = readString(in, "enumMember/name");
    m.type = readResolvedType(in);
    m.enumType = readResolvedType(in);
    m.constantValue = read_s64(in, "enumMember/value");
    return m;
}

EnumTable Deserializer::readEnumTable(std::istream& in) {
    EnumTable en;
    en.enumName = readString(in, "enum/name");
    en.underlyingType = readResolvedType(in);
    uint32_t memberCount = read_u32(in, "enum/memberCount");
    en.members.reserve(memberCount);
    for (uint32_t i = 0; i < memberCount; ++i) en.members.push_back(readEnumMember(in));
    return en;
}

AllocatorFunction Deserializer::readAllocatorFunction(std::istream& in) {
    AllocatorFunction fn;
    fn.functionName = readString(in, "allocFn/name");
    fn.returnType = readResolvedType(in);
    fn.paramTypes = readParamTypes(in);
    return fn;
}

Allocator Deserializer::readAllocator(std::istream& in) {
    Allocator alloc;
    alloc.allocatorName = readString(in, "alloc/name");
    alloc.allocator = readAllocatorFunction(in);
    alloc.free = readAllocatorFunction(in);
    return alloc;
}

FunctionEntry Deserializer::readFunctionEntry(std::istream& in) {
    FunctionEntry fn;
    fn.funcName = readString(in, "fn/name");
    fn.returnType = readResolvedType(in);
    fn.paramTypes = readParamTypes(in);
    return fn;
}

Generics Deserializer::readGenerics(std::istream& in) {
    Generics gen;
    gen.aliasName = readString(in, "generics/alias");

    // Components
    uint32_t compCount = read_u32(in, "generics/compCount");
    gen.components.reserve(compCount);
    for (uint32_t i = 0; i < compCount; ++i) gen.components.push_back(readComponentTable(in));

    // Records
    uint32_t recCount = read_u32(in, "generics/recCount");
    gen.records.reserve(recCount);
    for (uint32_t i = 0; i < recCount; ++i) gen.records.push_back(readRecordTable(in));

    // Functions
    uint32_t fnCount = read_u32(in, "generics/fnCount");
    gen.functions.reserve(fnCount);
    for (uint32_t i = 0; i < fnCount; ++i) gen.functions.push_back(readFunctionEntry(in));

    return gen;
}

void Deserializer::readStubTable(std::istream& in) {
    // Validate header
    uint32_t magic = read_u32(in, "header/magic");
    uint32_t version = read_u32(in, "header/version");
    uint16_t sections = read_u16(in, "header/sectionCount");

    if (magic != STUB_MAGIC) reportDevBug("Invalid stub file — bad magic number");
    if (version != STUB_VERSION) reportDevBug("Stub version mismatch");

    logInternal("Stub header valid — " + std::to_string(sections) + " section(s)");

    for (uint16_t s = 0; s < sections; ++s) {
        auto sectionTag = static_cast<StubSection>(read_u8(in, "section/tag"));

        switch (sectionTag) {
            case StubSection::SEALS: {
                uint32_t count = read_u32(in, "seals/count");
                stub.seals.reserve(count);
                for (uint32_t i = 0; i < count; ++i) stub.seals.push_back(readSealTable(in));
                logInternal("Read " + std::to_string(count) + " seal(s)");
                break;
            }

            case StubSection::COMPONENTS: {
                uint32_t count = read_u32(in, "components/count");
                stub.components.reserve(count);
                for (uint32_t i = 0; i < count; ++i)
                    stub.components.push_back(readComponentTable(in));
                logInternal("Read " + std::to_string(count) + " component(s)");
                break;
            }

            case StubSection::RECORDS: {
                uint32_t count = read_u32(in, "records/count");
                stub.records.reserve(count);
                for (uint32_t i = 0; i < count; ++i) stub.records.push_back(readRecordTable(in));
                logInternal("Read " + std::to_string(count) + " record(s)");
                break;
            }

            case StubSection::ENUMS: {
                uint32_t count = read_u32(in, "enums/count");
                stub.enums.reserve(count);
                for (uint32_t i = 0; i < count; ++i) stub.enums.push_back(readEnumTable(in));
                logInternal("Read " + std::to_string(count) + " enum(s)");
                break;
            }

            case StubSection::ALLOCATORS: {
                uint32_t count = read_u32(in, "allocators/count");
                stub.allocators.reserve(count);
                for (uint32_t i = 0; i < count; ++i) stub.allocators.push_back(readAllocator(in));
                logInternal("Read " + std::to_string(count) + " allocator(s)");
                break;
            }

            case StubSection::FUNCTIONS: {
                uint32_t count = read_u32(in, "functions/count");
                stub.functions.reserve(count);
                for (uint32_t i = 0; i < count; ++i)
                    stub.functions.push_back(readFunctionEntry(in));
                logInternal("Read " + std::to_string(count) + " function(s)");
                break;
            }

            case StubSection::GENERICS: {
                uint32_t count = read_u32(in, "generics/count");
                stub.generics.reserve(count);
                for (uint32_t i = 0; i < count; ++i) stub.generics.push_back(readGenerics(in));
                logInternal("Read " + std::to_string(count) + " generic instantiation(s)");
                break;
            }

            default:
                throw std::runtime_error("Unknown stub section tag: " +
                                         std::to_string(static_cast<uint8_t>(sectionTag)));
        }
    }
}

void Deserializer::processImports(const std::vector<std::unique_ptr<Node>>& nodes,
                                  const std::string& currentFile) {
    for (const auto& node : nodes) {
        auto* importStmt = dynamic_cast<ImportStatement*>(node.get());
        if (!importStmt) continue;

        std::string resolved = resolveImportPath(importStmt, currentFile);
        if (resolved.empty()) {
            logImportError("Could not resolve import path", importStmt->token.line,
                           importStmt->token.column);
            hasFailed = true;
            continue;
        }

        // Skip already loaded stubs
        if (loadedStubs.count(resolved)) {
            logInternal("Skipping already loaded stub: " + resolved);
            continue;
        }

        loadStub(resolved);
    }
}

std::string Deserializer::resolveImportPath(ImportStatement* import,
                                            const std::string& currentFile) {
    // Validation with detailed feedback
    if (!import || !import->stringExpr) {
        std::string err =
            "AST Corrupt: Import statement missing string expression in " + currentFile;
        reportDevBug(err);
        throw std::runtime_error(err);
    }

    auto strLit = dynamic_cast<StringLiteral*>(import->stringExpr.get());
    std::string raw = strLit->string_token.TokenLiteral;

    // Normalize paths to absolute to prevent "Relative vs Absolute" comparison
    // bugs
    fs::path absCurrentFile = fs::absolute(currentFile);
    fs::path currentDir = absCurrentFile.parent_path();

    fs::path stubPath = currentDir / (raw + ".stub");
    fs::path unnPath = currentDir / (raw + ".unn");
    fs::path binaryPath = currentDir / (raw + ".o");  // For now use .o
    recordLink(binaryPath);

    // Auto-Generation Logic
    if (!fs::exists(stubPath)) {
        if (fs::exists(unnPath)) {
            logInternal("[AUTO-STUB] Missing interface for '" + raw +
                        "'. Spawning sub-compiler...");

            // Recursion Guard: Pass the absolute path of the current file to the
            // child
            const char* env_stack = std::getenv("UNNC_IMPORT_STACK");
            std::string newStack =
                (env_stack ? std::string(env_stack) + "," : "") + absCurrentFile.string();

#ifdef _WIN32
            _putenv_s("UNNC_IMPORT_STACK", newStack.c_str());
#else
            setenv("UNNC_IMPORT_STACK", newStack.c_str(), 1);
#endif

            // Find the absolute path of THIS compiler executable
            std::string compilerExe;
            try {
                compilerExe = fs::canonical("/proc/self/exe").string();
            } catch (...) {
                // Fallback if procfs is restricted
                compilerExe = "./unnc";
            }

            // Explicitly tell the child where to put the stub
            std::string cmd = compilerExe + " " + unnPath.string() + " -stub " + stubPath.string();

            logInternal("[EXEC] " + cmd);
            int result = std::system(cmd.c_str());

            if (result != 0) {
                std::string errorMsg =
                    "Dependency Error: Failed to generate stub for '" + raw +
                    "'. The sub-compiler returned exit code: " + std::to_string(result);
                logImportError(errorMsg, strLit->expression.line, strLit->expression.column);
                throw std::runtime_error(errorMsg);
            }
        } else {
            std::string errorMsg = "Module Not Found: Checked for '" + stubPath.string() +
                                   "' and '" + unnPath.string() + "'";
            logImportError(errorMsg, strLit->expression.line, strLit->expression.column);
            throw std::runtime_error(errorMsg);
        }
    }

    // Path Validation & Security (Weakly canonical handles symlinks)
    fs::path resolved;
    try {
        resolved = fs::weakly_canonical(stubPath);
    } catch (const std::exception& e) {
        resolved = stubPath;
    }

    // Security: Prevent accessing files outside the project workspace
    fs::path projectRoot = fs::current_path();
    std::string resolvedStr = resolved.string();

    if (resolvedStr.rfind(projectRoot.string(), 0) != 0) {
        std::string errorMsg = "Security Violation: Import path '" + raw +
                               "' escapes project root (" + projectRoot.string() + ")";
        logImportError(errorMsg, strLit->expression.line, strLit->expression.column);
        throw std::runtime_error(errorMsg);
    }

    // Deduplication: Don't reload what we already have
    if (loadedStubs.find(resolvedStr) != loadedStubs.end()) {
        return resolvedStr;
    }

    logInternal("[SUCCESS] Resolved module '" + raw + "' to " + resolvedStr);
    return resolvedStr;
}

void Deserializer::loadStub(const std::string& path) {
    logInternal("Loading stub: " + path);
    std::ifstream in(path, std::ios::binary);
    if (!in) {
        logImportError("Failed to open stub file: " + path, 0, 0);
        hasFailed = true;
        return;
    }
    try {
        readStubTable(in);
        loadedStubs[path] = path;
        // No recordLink here — the .o was already registered in resolveImportPath
        logInternal("Stub loaded successfully: " + path);
    } catch (const std::exception& e) {
        logImportError(std::string("Failed to read stub '") + path + "': " + e.what(), 0, 0);
        hasFailed = true;
    }
}

void Deserializer::recordLink(const std::string& path) {
    linkerRegistry.push_back({path, LinkOrigin::NATIVE_IMPORT});
}

void Deserializer::logInternal(const std::string& message) {
    if (isVerbose) std::cout << "[DESERIALIZER] " << message << "\n";
}

void Deserializer::logImportError(const std::string& message, int line, int col) {
    std::cerr << "[IMPORT ERROR] " << message;
    if (line > 0) std::cerr << " (" << line << ":" << col << ")";
    std::cerr << "\n";
}

void Deserializer::reportDevBug(const std::string& message) {
    std::cerr << "[DEV BUG] " << message << "\n";
    hasFailed = true;
    std::abort();
}
