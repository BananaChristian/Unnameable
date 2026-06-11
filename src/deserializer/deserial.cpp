#include "deserial.hpp"

#include "defs.hpp"
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <istream>
#include <stdexcept>
#include <string>
namespace fs = std::filesystem;

Deserializer::Deserializer(ErrorHandler &handler, bool verbose)
    : errorHandler(handler), isVerbose(verbose) {}

bool Deserializer::failed() { return hasFailed; }

void Deserializer::readOrFail(std::istream &in, void *dst, size_t size,
                              const std::string &context) {
  if (!in.read(reinterpret_cast<char *>(dst), size))
    reportDevBug("Read failed at: " + context);
}

uint8_t Deserializer::read_u8(std::istream &in, const std::string &ctx) {
  uint8_t v;
  readOrFail(in, &v, 1, ctx);
  return v;
}

uint16_t Deserializer::read_u16(std::istream &in, const std::string &ctx) {
  uint16_t v;
  readOrFail(in, &v, 2, ctx);
  return v;
}

uint32_t Deserializer::read_u32(std::istream &in, const std::string &ctx) {
  uint32_t v;
  readOrFail(in, &v, 4, ctx);
  return v;
}

uint64_t Deserializer::read_u64(std::istream &in, const std::string &ctx) {
  uint64_t v;
  readOrFail(in, &v, 8, ctx);
  return v;
}

int32_t Deserializer::read_s32(std::istream &in, const std::string &ctx) {
  int32_t v;
  readOrFail(in, &v, 4, ctx);
  return v;
}

int64_t Deserializer::read_s64(std::istream &in, const std::string &ctx) {
  int64_t v;
  readOrFail(in, &v, 8, ctx);
  return v;
}

std::string Deserializer::readString(std::istream &in, const std::string &ctx) {
  uint32_t len = read_u32(in, ctx + "/len");
  std::string s(len, '\0');
  readOrFail(in, s.data(), len, ctx + "/data");
  return s;
}

ResolvedType Deserializer::readResolvedType(std::istream &in) {
  ResolvedType t;
  t.modifier = static_cast<Modifier>(read_u8(in, "type/modifier"));
  t.kind = static_cast<DataType>(read_u8(in, "type/kind"));
  t.resolvedName = readString(in, "type/resolvedName");
  t.arraySize = read_u64(in, "type/arraySize");
  t.isConstantSize = static_cast<bool>(read_u8(in, "type/isConstantSize"));
  t.isNull = static_cast<bool>(read_u8(in, "type/isNull"));

  bool hasInner = static_cast<bool>(read_u8(in, "type/hasInner"));
  if (hasInner)
    t.innerType = std::make_shared<ResolvedType>(readResolvedType(in));

  return t;
}

std::vector<std::pair<ResolvedType, std::string>>
Deserializer::readParamTypes(std::istream &in) {
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

SealFunction Deserializer::readSealFunction(std::istream &in) {
  SealFunction fn;
  fn.funcName = readString(in, "sealFnname");
  fn.returnType = readResolvedType(in);
  fn.paramTypes = readParamTypes(in);
  return fn;
}

SealTable Deserializer::readSealTable(std::istream &in) {
  SealTable seal;
  seal.sealName = readString(in, "seal/name");
  uint32_t fnCount = read_u32(in, "seal/fnCount");
  seal.sealFns.reserve(fnCount);
  for (uint32_t i = 0; i < fnCount; ++i)
    seal.sealFns.push_back(readSealFunction(in));
  return seal;
}

RecordMember Deserializer::readRecordMember(std::istream &in) {
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

RecordTable Deserializer::readRecordTable(std::istream &in) {
  RecordTable record;
  record.recordName = readString(in, "record/name");
  uint32_t memberCount = read_u32(in, "record/memberCount");
  record.members.reserve(memberCount);
  for (uint32_t i = 0; i < memberCount; ++i)
    record.members.push_back(readRecordMember(in));
  return record;
}

RecordMethodsTable Deserializer::readMethodsTable(std::istream &in) {
  RecordMethodsTable methods;
  methods.recordName = readString(in, "methods/name");
  uint32_t methodsCount = read_u32(in, "methods/methodCount");
  methods.methods.reserve(methodsCount);
  for (uint32_t i = 0; i < methodsCount; ++i)
    methods.methods.push_back(readFunctionEntry(in));
  return methods;
}

EnumMembers Deserializer::readEnumMember(std::istream &in) {
  EnumMembers m;
  m.memberName = readString(in, "enumMember/name");
  m.type = readResolvedType(in);
  m.enumType = readResolvedType(in);
  m.constantValue = read_s64(in, "enumMember/value");
  return m;
}

EnumTable Deserializer::readEnumTable(std::istream &in) {
  EnumTable en;
  en.enumName = readString(in, "enum/name");
  en.underlyingType = readResolvedType(in);
  uint32_t memberCount = read_u32(in, "enum/memberCount");
  en.members.reserve(memberCount);
  for (uint32_t i = 0; i < memberCount; ++i)
    en.members.push_back(readEnumMember(in));
  return en;
}

AllocatorFunction Deserializer::readAllocatorFunction(std::istream &in) {
  AllocatorFunction fn;
  fn.functionName = readString(in, "allocFn/name");
  fn.returnType = readResolvedType(in);
  fn.paramTypes = readParamTypes(in);
  return fn;
}

Allocator Deserializer::readAllocator(std::istream &in) {
  Allocator alloc;
  alloc.allocatorName = readString(in, "alloc/name");
  alloc.allocator = readAllocatorFunction(in);
  alloc.free = readAllocatorFunction(in);
  return alloc;
}

FunctionEntry Deserializer::readFunctionEntry(std::istream &in) {
  FunctionEntry fn;
  fn.funcName = readString(in, "fn/name");
  fn.returnType = readResolvedType(in);
  fn.paramTypes = readParamTypes(in);
  fn.isDeclaration = read_u8(in, "fn/IsDeclaration");
  return fn;
}

VariableEntry Deserializer::readVariableEntry(std::istream &in) {
  VariableEntry var;
  var.var_name = readString(in, "var/var_name");
  var.declaredType = readResolvedType(in);
  var.isConstant = read_u8(in, "var/isConstant");
  var.isMutable = read_u8(in, "var/isMutable");
  var.isInitialized = read_u8(in, "var/isInitialized");

  return var;
}

Generics Deserializer::readGenerics(std::istream &in) {
  Generics gen;
  gen.aliasName = readString(in, "generics/alias");
  // Records
  uint32_t recCount = read_u32(in, "generics/recCount");
  gen.records.reserve(recCount);
  for (uint32_t i = 0; i < recCount; ++i)
    gen.records.push_back(readRecordTable(in));

  // Functions
  uint32_t fnCount = read_u32(in, "generics/fnCount");
  gen.functions.reserve(fnCount);
  for (uint32_t i = 0; i < fnCount; ++i)
    gen.functions.push_back(readFunctionEntry(in));

  return gen;
}

void Deserializer::readStubTable(std::istream &in) {
  // Validate header
  uint32_t magic = read_u32(in, "header/magic");
  uint32_t version = read_u32(in, "header/version");
  uint16_t sections = read_u16(in, "header/sectionCount");

  if (magic != STUB_MAGIC)
    reportDevBug("Invalid stub file — bad magic number");
  if (version != STUB_VERSION)
    reportDevBug("Stub version mismatch");

  logInternal("Stub header valid — " + std::to_string(sections) +
              " section(s)");

  for (uint16_t s = 0; s < sections; ++s) {
    auto sectionTag = static_cast<StubSection>(read_u8(in, "section/tag"));

    switch (sectionTag) {
    case StubSection::SEALS: {
      uint32_t count = read_u32(in, "seals/count");
      module.exports.seals.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.seals.push_back(readSealTable(in));
      logInternal("Read " + std::to_string(count) + " seal(s)");
      break;
    }

    case StubSection::RECORDS: {
      uint32_t count = read_u32(in, "records/count");
      module.exports.records.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.records.push_back(readRecordTable(in));
      logInternal("Read " + std::to_string(count) + " record(s)");
      break;
    }
    case StubSection::METHODS: {
      uint32_t count = read_u32(in, "methods/count");
      module.exports.methods.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.methods.push_back(readMethodsTable(in));
      logInternal("Read " + std::to_string(count) + " method(s)");
      break;
    }

    case StubSection::ENUMS: {
      uint32_t count = read_u32(in, "enums/count");
      module.exports.enums.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.enums.push_back(readEnumTable(in));
      logInternal("Read " + std::to_string(count) + " enum(s)");
      break;
    }

    case StubSection::ALLOCATORS: {
      uint32_t count = read_u32(in, "allocators/count");
      module.exports.allocators.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.allocators.push_back(readAllocator(in));
      logInternal("Read " + std::to_string(count) + " allocator(s)");
      break;
    }

    case StubSection::FUNCTIONS: {
      uint32_t count = read_u32(in, "functions/count");
      module.exports.functions.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.functions.push_back(readFunctionEntry(in));
      logInternal("Read " + std::to_string(count) + " function(s)");
      break;
    }

    case StubSection::VARIABLES: {
      uint32_t count = read_u32(in, "variables/count");
      module.exports.variables.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.variables.push_back(readVariableEntry(in));
      logInternal("Read " + std::to_string(count) + " variable(s)");
      break;
    }

    case StubSection::GENERICS: {
      uint32_t count = read_u32(in, "generics/count");
      module.exports.generics.reserve(count);
      for (uint32_t i = 0; i < count; ++i)
        module.exports.generics.push_back(readGenerics(in));
      logInternal("Read " + std::to_string(count) +
                  " generic instantiation(s)");
      break;
    }

    default:
      throw std::runtime_error(
          "Unknown stub section tag: " +
          std::to_string(static_cast<uint8_t>(sectionTag)));
    }
  }
}

void Deserializer::readModule(std::istream &in) {
  module.name = readString(in, "module/name");
  readStubTable(in);
}

void Deserializer::processLoads(const std::vector<std::string> &stubPaths) {
  for (const auto &path : stubPaths) {
    if (loadedModules.count(path)) {
      logInternal("Skipping already loaded module: " + path);
      continue;
    }
    if (!fs::exists(path)) {
      logImportError("Module not found: " + path, 0, 0);
      continue;
    }
    loadModule(path);
  }
}

void Deserializer::loadModule(const std::string &path) {
  logInternal("Loading stub: " + path);
  std::ifstream in(path, std::ios::binary);
  if (!in) {
    logImportError("Failed to open stub file: " + path, 0, 0);
    return;
  }
  try {
    readModule(in);
    loadedModules[path] = path;
    logInternal("Module loaded successfully: " + path);
  } catch (const std::exception &e) {
    logImportError(
        std::string("Failed to read module '") + path + "': " + e.what(), 0, 0);
  }
}

void Deserializer::logInternal(const std::string &message) {
  if (isVerbose)
    std::cout << "[DESERIALIZER] " << message << "\n";
}

void Deserializer::logImportError(const std::string &message, int line,
                                  int col) {
  hasFailed = true;
  std::cerr << "[IMPORT ERROR] " << message;
  if (line > 0)
    std::cerr << " (" << line << ":" << col << ")";
  std::cerr << "\n";
}

void Deserializer::reportDevBug(const std::string &message) {
  std::cerr << "[DEV BUG] " << message << "\n";
  hasFailed = true;
  std::abort();
}
