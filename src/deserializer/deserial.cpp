
#include "deserial.hpp"
#include "errors.hpp"
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <istream>
#include <string>

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

namespace fs = std::filesystem;

Deserializer::Deserializer(ErrorHandler &handler, bool verbose)
    : errorHandler(handler), isVerbose(verbose) {}

void Deserializer::processImports(
    const std::vector<std::unique_ptr<Node>> &nodes,
    const std::string &currentFile) {
  for (auto &node : nodes) {
    if (auto import = dynamic_cast<ImportStatement *>(node.get())) {
      std::string resolved = resolveImportPath(import, currentFile);
      loadStub(resolved);
    }
  }
}

std::string Deserializer::resolveImportPath(ImportStatement *import,
                                            const std::string &currentFile) {
  // Validation with detailed feedback
  if (!import || !import->stringExpr) {
    std::string err =
        "AST Corrupt: Import statement missing string expression in " +
        currentFile;
    reportDevBug(err);
    throw std::runtime_error(err);
  }

  auto strLit = dynamic_cast<StringLiteral *>(import->stringExpr.get());
  std::string raw = strLit->string_token.TokenLiteral;

  // Normalize paths to absolute to prevent "Relative vs Absolute" comparison
  // bugs
  fs::path absCurrentFile = fs::absolute(currentFile);
  fs::path currentDir = absCurrentFile.parent_path();

  fs::path stubPath = currentDir / (raw + ".stub");
  fs::path unnPath = currentDir / (raw + ".unn");
  fs::path binaryPath = currentDir / (raw + ".o"); // For now use .o
  recordLink(binaryPath);

  // Auto-Generation Logic
  if (!fs::exists(stubPath)) {
    if (fs::exists(unnPath)) {
      logInternal("[AUTO-STUB] Missing interface for '" + raw +
                  "'. Spawning sub-compiler...");

      // Recursion Guard: Pass the absolute path of the current file to the
      // child
      const char *env_stack = std::getenv("UNNC_IMPORT_STACK");
      std::string newStack = (env_stack ? std::string(env_stack) + "," : "") +
                             absCurrentFile.string();

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
      std::string cmd =
          compilerExe + " " + unnPath.string() + " -stub " + stubPath.string();

      logInternal("[EXEC] " + cmd);
      int result = std::system(cmd.c_str());

      if (result != 0) {
        std::string errorMsg =
            "Dependency Error: Failed to generate stub for '" + raw +
            "'. The sub-compiler returned exit code: " + std::to_string(result);
        logImportError(errorMsg, strLit->expression.line,
                       strLit->expression.column);
        throw std::runtime_error(errorMsg);
      }
    } else {
      std::string errorMsg = "Module Not Found: Checked for '" +
                             stubPath.string() + "' and '" + unnPath.string() +
                             "'";
      logImportError(errorMsg, strLit->expression.line,
                     strLit->expression.column);
      throw std::runtime_error(errorMsg);
    }
  }

  // Path Validation & Security (Weakly canonical handles symlinks)
  fs::path resolved;
  try {
    resolved = fs::weakly_canonical(stubPath);
  } catch (const std::exception &e) {
    resolved = stubPath;
  }

  // Security: Prevent accessing files outside the project workspace
  fs::path projectRoot = fs::current_path();
  std::string resolvedStr = resolved.string();

  if (resolvedStr.rfind(projectRoot.string(), 0) != 0) {
    std::string errorMsg = "Security Violation: Import path '" + raw +
                           "' escapes project root (" + projectRoot.string() +
                           ")";
    logImportError(errorMsg, strLit->expression.line,
                   strLit->expression.column);
    throw std::runtime_error(errorMsg);
  }

  // Deduplication: Don't reload what we already have
  if (loadedStubs.find(resolvedStr) != loadedStubs.end()) {
    return resolvedStr;
  }

  logInternal("[SUCCESS] Resolved module '" + raw + "' to " + resolvedStr);
  return resolvedStr;
}

void Deserializer::recordLink(const std::string &path) {
  // Check to avoid deduplication
  for (const auto &entry : linkerRegistery) {
    if (entry.path == path)
      return;
  }

  RegistryEntry entry;
  entry.path = path;
  entry.origin = LinkOrigin::NATIVE_IMPORT;

  linkerRegistery.push_back(entry);
}

void Deserializer::loadStub(const std::string &resolved) {
  auto [it, inserted] = loadedStubs.emplace(resolved, resolved);
  if (!inserted)
    return;

  std::ifstream in(resolved, std::ios::binary);
  if (!in)
    throw std::runtime_error("Failed to open stub");

  stub = readStubTable(in);

  // Seal reading
  for (const auto &seal : stub.seals) {
    logInternal("DESERIALIZER IMPORT SEAL NAME: " + seal.sealName);
    auto &sealMap = importedSealTable[seal.sealName];
    for (const auto &fn : seal.sealFns) {
      ImportedSymbolInfo info;
      info.returnType = fn.returnType;
      info.paramTypes = fn.paramTypes;
      sealMap.emplace(fn.funcName, std::move(info));
    }
  }

  // Component reading
  for (const auto &component : stub.components) {
    logInternal("DESERIALIZER COMPONENT NAME: " + component.componentName);
    auto &compMap = importedComponentTable[component.componentName];
    for (const auto &member : component.members) {
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

    for (const auto &method : component.methods) {
      ImportedSymbolInfo info;
      info.returnType = method.returnType;
      info.paramTypes = method.paramTypes;
      info.isFunction = method.isFunction;

      compMap.emplace(method.methodName, std::move(info));
    }

    // Dealing with the init
    if (component.hasInit) {
      ImportedSymbolInfo info;
      info.initArgs = component.init.initArgs;
      info.returnType = component.init.returnType;
      info.type = component.init.type;

      // Store in a special init table
      importedInitTable[component.componentName] = info;
    }
  }

  // Record reading
  for (const auto &record : stub.records) {
    logInternal("RECORD NAME: " + record.recordName);
    auto &recordMap = importedRecordsTable[record.recordName];
    for (const auto &member : record.members) {
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

  // Enum reading
  for (const auto &enums : stub.enums) {
    logInternal("ENUM NAME: " + enums.enumName);
    auto &enumMap = importedEnumsTable[enums.enumName];
    for (const auto &member : enums.members) {
      ImportedSymbolInfo info;
      info.type = member.type;
      info.enumType = member.enumType;
      info.constantValue = member.constantValue;

      enumMap.emplace(member.memberName, std::move(info));
    }
  }

  // Allocator reading
  for (const auto &allocators : stub.allocators) {
    logInternal("ALLOCATOR NAME: " + allocators.allocatorName);
    auto &allocatorMap = importedAllocatorsTable[allocators.allocatorName];
    // Allocation and freeing function
    auto allocFunc = allocators.allocationFunc;
    allocatorMap.allocateName = allocFunc.funcName;
    auto allocInfo = std::make_shared<ImportedSymbolInfo>();
    allocInfo->returnType = allocFunc.returnType;
    allocInfo->paramTypes = allocFunc.paramTypes;
    allocatorMap.allocatorSymbol = allocInfo;

    auto freeFunc = allocators.freeFunc;
    allocatorMap.freeName = freeFunc.funcName;
    auto freeInfo = std::make_shared<ImportedSymbolInfo>();
    freeInfo->returnType = freeFunc.returnType;
    freeInfo->paramTypes = freeFunc.paramTypes;
    allocatorMap.freeSymbol = freeInfo;
  }
}

void Deserializer::readOrFail(std::istream &in, void *dst, size_t size,
                              const std::string &context) {
  // Capture position before the read attempt
  std::streampos start_pos = in.tellg();

  // Attempt the read
  if (!in.read(reinterpret_cast<char *>(dst), size)) {
    // On failure, clear flags to get true EOF/Fail status, then restore
    auto state = in.rdstate();
    in.clear();

    std::cerr << COLOR_RED << COLOR_BOLD << "[!!! FAILURE !!!]" << COLOR_RESET
              << "\n"
              << "  Context: " << context << "\n"
              << "  Read Attempt Failed: " << size << " bytes.\n"
              << "  File Start Pos: " << start_pos << "\n"
              << "  Flags (EOF/Fail/Bad): " << in.eof() << "/" << in.fail()
              << "/" << in.bad() << "\n";

    // Restore flags
    in.setstate(state);

    reportDevBug("Unexpected EOF while reading stub");
  }
}

uint8_t Deserializer::read_u8(std::istream &in, const std::string &context) {
  uint8_t v;
  std::streampos pos = in.tellg();
  readOrFail(in, &v, sizeof(v), context);
  if (isVerbose) {
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET << "Pos: " << std::hex
              << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read u8: " << static_cast<int>(v)
              << "\t\t| Context: " << context << "\n";
  }
  return v;
}

uint16_t Deserializer::read_u16(std::istream &in, const std::string &context) {
  uint16_t v;
  std::streampos pos = in.tellg();
  readOrFail(in, &v, sizeof(v), context);
  if (isVerbose) {
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET << "Pos: " << std::hex
              << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read u16: " << v << "\t\t| Context: " << context << "\n";
  }
  return v;
}

uint32_t Deserializer::read_u32(std::istream &in, const std::string &context) {
  uint32_t v;
  std::streampos pos = in.tellg();
  readOrFail(in, &v, sizeof(v), context);
  if (isVerbose) {
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET << "Pos: " << std::hex
              << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read u32: " << v << "\t| Context: " << context << "\n";
  }
  return v;
}

int32_t Deserializer::read_s32(std::istream &in, const std::string &context) {
  int32_t v;
  std::streampos pos = in.tellg();
  readOrFail(in, &v, sizeof(v), context);
  if (isVerbose) {
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET << "Pos: " << std::hex
              << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read s32: " << v << "\t| Context: " << context << "\n";
  }
  return v;
}

int64_t Deserializer::read_s64(std::istream &in, const std::string &context) {
  int64_t v;
  std::streampos pos = in.tellg();
  readOrFail(in, &v, sizeof(v), context);
  if (isVerbose) {
    std::cout << COLOR_CYAN << "[TRACE] " << COLOR_RESET << "Pos: " << std::hex
              << std::setw(4) << std::setfill('0') << pos << std::dec
              << " | Read s64: " << v << "\t| Context: " << context << "\n";
  }
  return v;
}

std::string Deserializer::readString(std::istream &in,
                                     const std::string &context) {
  uint32_t len = read_u32(in, "String Length: " + context);

  if (isVerbose) {
    std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET
              << "Reading string of calculated length: " << len
              << "\t| Context: " << context << "\n";
  }

  std::string s(len, '\0');

  readOrFail(in, s.data(), len,
             "String Data (" + std::to_string(len) + " bytes): " + context);

  if (isVerbose) {
    std::cout << COLOR_GREEN << "[SUCCESS] " << COLOR_RESET << "Read string: '"
              << s << "'\n";
  }

  return s;
}

ImportedType Deserializer::readImportedType(std::istream &in) {
  ImportedType t;
  logInternal("Starting ImportedType read");

  t.kind = static_cast<ImportedDataType>(read_u32(in, "Type Kind"));
  t.resolvedName = readString(in, "Type Resolved Name");

  t.isPointer = read_u8(in, "Type Flag: isPointer");
  t.isRef = read_u8(in, "Type Flag: isRef");
  t.isNull = read_u8(in, "Type Flag: isNull");
  t.isArray = read_u8(in, "Type Flag: isArray");

  logInternal("Finished ImportedType read");

  return t;
}

std::vector<std::pair<ImportedType, std::string>>
Deserializer::readParamTypes(std::istream &in) {
  logInternal("Starting Parameter List read");
  uint32_t paramCount = read_u32(in, "Parameter Count");

  std::vector<std::pair<ImportedType, std::string>> params;
  params.reserve(paramCount);
  logInternal("Expecting " + std::to_string(paramCount));

  for (uint32_t i = 0; i < paramCount; ++i) {
    logInternal("Reading Parameter " + std::to_string(i + 1) + " paramCount");
    ImportedType type = readImportedType(in);
    std::string name = readString(in, "Parameter Name");
    params.emplace_back(std::move(type), std::move(name));
  }
  logInternal("Finished Parameter List read");

  return params;
}

//____________________COMPONENT MEMBERS READING_____________________
RawComponentMember Deserializer::readComponentMember(std::istream &in) {
  logInternal("Component Member read");
  RawComponentMember member;
  member.memberName = readString(in, "Member Name");
  member.type = readImportedType(in);
  member.memberIndex = read_s32(in, "Member Index");
  member.isNullable = read_u8(in, "Member Flag: isNullable");
  member.isMutable = read_u8(in, "Member Flag: isMutable");
  member.isConstant = read_u8(in, "Member Flag: isConstant");
  member.isRef = read_u8(in, "Member Flag: isRef");
  member.isPointer = read_u8(in, "Member Flag: isPointer");
  member.storage =
      static_cast<ImportedStorageType>(read_u32(in, "Member Storage Type"));
  logInternal("Finished Component Member read");

  return member;
}

RawComponentMethod Deserializer::readComponentMethod(std::istream &in) {
  logInternal("Component Method read");
  RawComponentMethod method;
  method.methodName = readString(in, "Method Name");
  method.returnType = readImportedType(in);
  method.paramTypes = readParamTypes(in);
  method.isFunction = read_u8(in, "isFunction");
  logInternal("Finished Component Method read");

  return method;
}

RawComponentInit Deserializer::readComponentInit(std::istream &in) {
  logInternal("Component Init read");
  RawComponentInit init;

  uint32_t argCount = read_u32(in, "Init Argument Count");
  init.initArgs.reserve(argCount);
  for (uint32_t i = 0; i < argCount; ++i) {
    init.initArgs.push_back(readImportedType(in));
  }

  init.returnType = readImportedType(in);
  init.type = readImportedType(in);

  logInternal("Finished component Init read");
  return init;
}

//__________________RECORD MEMBER READING_______________
RawRecordMember Deserializer::readRecordMember(std::istream &in) {
  logInternal("Record Member read");
  RawRecordMember member;
  member.memberName = readString(in, "Field Name");
  member.type = readImportedType(in);
  member.memberIndex = read_s32(in, "Member Index");
  member.isNullable = read_u8(in, "Member Flag: isNullable");
  member.isMutable = read_u8(in, "Member Flag: isMutable");
  member.isConstant = read_u8(in, "Member Flag: isConstant");
  member.isRef = read_u8(in, "Member Flag: isRef");
  member.isPointer = read_u8(in, "Member Flag: isPointer");
  member.storage =
      static_cast<ImportedStorageType>(read_u32(in, "Member Storage Type"));

  logInternal("Finished Record Member read");

  return member;
}

//___________________ENUM MEMBER READING_________________
RawEnumMembers Deserializer::readEnumMember(std::istream &in) {
  logInternal("Enum member read");
  RawEnumMembers member;
  member.memberName = readString(in, "Member name");
  member.type = readImportedType(in);
  member.enumType = readImportedType(in);
  member.constantValue = read_s64(in, "Enum constant value");

  logInternal("Finised enum member read");

  return member;
}

//_________________ALLOCATOR FUNCTIONS READING_______________
RawAlloctorFunction Deserializer::readAllocatorFunction(std::istream &in) {
  logInternal("Allocator function read");
  RawAlloctorFunction function;
  function.funcName = readString(in, "Function name");
  function.returnType = readImportedType(in);
  function.paramTypes = readParamTypes(in);

  logInternal("Finished allocator function read");
  return function;
}

//___________________STUB TABLE READ___________________

RawStubTable Deserializer::readStubTable(std::istream &in) {
  RawStubTable table;
  logInternal("STARTING STUB TABLE DESERIALIZATION");

  // HEADER
  uint32_t magic = read_u32(in, "Header: Magic Number (STUB)");
  if (magic != STUB_MAGIC) {
    std::cerr << COLOR_RED << "[FATAL] " << COLOR_RESET
              << "Invalid stub magic: Expected " << std::hex << STUB_MAGIC
              << " but got " << magic << std::dec << "\n";
    throw std::runtime_error("Invalid stub magic");
  }

  uint32_t version =
      read_u32(in, "Header: Version (u32)"); // NOTE: Deserializer reads u32,
                                             // serializer writes u32
  if (version != STUB_VERSION) {
    std::cerr << COLOR_RED << "[FATAL] " << COLOR_RESET
              << "Unsupported stub version: Expected " << STUB_VERSION
              << " but got " << version << "\n";
    throw std::runtime_error("Unsupported stub version");
  }

  uint16_t sectionCount = read_u16(in, "Header: Section Count");
  logInternal("Expecting " + std::to_string(sectionCount) + " sections");

  // SECTIONS
  for (uint16_t s = 0; s < sectionCount; ++s) {
    logInternal("READING SECTION '" + std::to_string(s + 1) +
                "' Total sections: " + std::to_string(sectionCount));
    ImportedStubSection section =
        static_cast<ImportedStubSection>(read_u8(in, "Section Type ID"));
    uint32_t entryCount = read_u32(in, "Section Entry Count");

    if (isVerbose) {
      std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET
                << "Section ID: " << static_cast<int>(section)
                << ", Expected Entries: " << entryCount << "\n";
    }

    switch (section) {
    case ImportedStubSection::SEALS: {
      if (entryCount > 1000000) { // No stub should have 1 million entries
        throw std::runtime_error(
            "Stub corruption detected: absurdly high entry count (" +
            std::to_string(entryCount) + ")");
      }
      table.seals.reserve(entryCount);

      for (uint32_t i = 0; i < entryCount; ++i) {
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET
                    << " Reading Seal " << i + 1 << " of " << entryCount
                    << "\n";
        }
        RawSealTable seal;

        seal.sealName = readString(in, "Seal Table Name");

        uint32_t fnCount = read_u32(in, "Seal Function Count");
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Seal '"
                    << seal.sealName << "' expects " << fnCount
                    << " functions.\n";
        }

        seal.sealFns.reserve(fnCount);

        for (uint32_t j = 0; j < fnCount; ++j) {
          if (isVerbose) {
            std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET
                      << " Reading Function " << j + 1 << " of " << fnCount
                      << " for Seal '" << seal.sealName << "'\n";
          }
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

    case ImportedStubSection::COMPONENTS: {
      if (entryCount > 1000000) {
        throw std::runtime_error(
            "Stub corruption detected: absurdly high entry count (" +
            std::to_string(entryCount) + ")");
      }
      table.components.reserve(entryCount);

      for (uint32_t i = 0; i < entryCount; ++i) {
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET
                    << " Reading Component " << i + 1 << " of " << entryCount
                    << "\n";
        }
        RawComponentTable comp;
        comp.componentName = readString(in, "Component Table Name");

        // members
        uint32_t memberCount = read_u32(in, "Component Member Count");
        comp.members.reserve(memberCount);
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Component '"
                    << comp.componentName << "' expects " << memberCount
                    << " members.\n";
        }
        for (uint32_t m = 0; m < memberCount; ++m) {
          comp.members.push_back(readComponentMember(in));
        }

        // methods
        uint32_t methodCount = read_u32(in, "Component Method Count");
        comp.methods.reserve(methodCount);
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Component '"
                    << comp.componentName << "' expects " << methodCount
                    << " methods.\n";
        }
        for (uint32_t m = 0; m < methodCount; ++m) {
          comp.methods.push_back(readComponentMethod(in));
        }

        // Init
        bool hasInit = read_u8(in, "Has Init");
        comp.hasInit = hasInit;
        if (hasInit) {
          comp.init = readComponentInit(in);
        }

        if (isVerbose) {
          std::cout << "DEBUG: Finishing Component " << i + 1
                    << " at Pos: " << in.tellg() << "\n";
        }
        table.components.push_back(std::move(comp));
      }
      break;
    }
    case ImportedStubSection::RECORDS: {
      if (entryCount > 1000000) {
        throw std::runtime_error(
            "Stub corruption detected: absurdly high entry count (" +
            std::to_string(entryCount) + ")");
      }
      table.records.reserve(entryCount);

      for (uint32_t i = 0; i < entryCount; i++) {
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET
                    << " Reading Record " << i + 1 << " of " << entryCount
                    << "\n";
        }
        RawRecordTable record;
        record.recordName = readString(in, "Record Table Name");

        // Members
        uint32_t memberCount = read_u32(in, "Record Member Count");
        record.members.reserve(memberCount);
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Record '"
                    << record.recordName << "' expects " << memberCount
                    << " members.\n";
        }
        for (uint32_t m = 0; m < memberCount; ++m) {
          record.members.push_back(readRecordMember(in));
        }

        table.records.push_back(std::move(record));
      }
      break;
    }
    case ImportedStubSection::ENUMS: {
      if (entryCount > 1000000) {
        throw std::runtime_error(
            "Stub corruption detected: absurdly high entry count (" +
            std::to_string(entryCount) + ")");
      }
      table.enums.reserve(entryCount);
      for (uint32_t i = 0; i < entryCount; i++) {
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET
                    << " Reading Enum " << i + 1 << " of " << entryCount
                    << "\n";
        }
        RawEnumTable enumTable;
        enumTable.enumName = readString(in, "Enum Name");

        enumTable.underLyingType = readImportedType(in);

        // Members
        uint32_t memberCount = read_u32(in, "Enum Member count");
        enumTable.members.reserve(memberCount);
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[INFO] " << COLOR_RESET << "Record '"
                    << enumTable.enumName << "' expects " << memberCount
                    << " members.\n";
        }
        for (uint32_t m = 0; m < memberCount; ++m) {
          enumTable.members.push_back(readEnumMember(in));
        }

        table.enums.push_back(std::move(enumTable));
      }
      break;
    }
    case ImportedStubSection::ALLOCATORS: {
      if (entryCount > 1000000) {
        throw std::runtime_error(
            "Stub corruption detected: absurdly high entry count (" +
            std::to_string(entryCount) + ")");
      }
      table.allocators.reserve(entryCount);
      for (uint32_t i = 0; i < entryCount; i++) {
        if (isVerbose) {
          std::cout << COLOR_YELLOW << "[DEBUG]" << COLOR_RESET
                    << " Reading Allocator " << i + 1 << " of " << entryCount
                    << "\n";
        }
        RawAllocatorTable allocatorTable;
        allocatorTable.allocatorName = readString(in, "Allocator Name");

        // The allocation and free function
        allocatorTable.allocationFunc = readAllocatorFunction(in);
        allocatorTable.freeFunc = readAllocatorFunction(in);

        table.allocators.push_back(std::move(allocatorTable));
      }
      break;
    }

    default:
      reportDevBug("Unknown stub section ID:" +
                   std::to_string(static_cast<int>(section)));
    }
  }

  logInternal("Finished stub table deserialization");
  return table;
}

void Deserializer::logImportError(const std::string &message, int line,
                                  int col) {
  hasFailed = true;
  CompilerError error;

  error.level = ErrorLevel::IMPORT;
  error.line = line;
  error.col = col;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);
}

void Deserializer::reportDevBug(const std::string &message) {
  hasFailed = true;
  std::cerr << COLOR_RED << "[INTERNAL COMPILER ERROR]" << COLOR_RED << message
            << "\n";
}

void Deserializer::logInternal(const std::string &message) {
  if (isVerbose) {
    std::cout << message << "\n";
  }
}

bool Deserializer::failed() { return hasFailed; }
