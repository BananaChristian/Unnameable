#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "defs.hpp"
#include "errors.hpp"

class Deserializer {
public:
  Deserializer(ErrorHandler &handler, bool isVerbose);

  void processLoads(const std::vector<std::string> &modulePaths);
  void setBuildDirectory(const std::string &dir) { buildDirectory = dir; }
  std::string buildDirectory;

  bool failed();

  // Fully deserialized stub data,semantics converts this into live symbols
  Module module;

  // Tracks already,loaded modules to avoid double-loading (path -> path)
  std::unordered_map<std::string, std::string> loadedModules;

private:
  ErrorHandler &errorHandler;
  bool isVerbose = false;
  bool hasFailed = false;

  static constexpr uint32_t STUB_MAGIC = 0x53545542; // "STUB"
  static constexpr uint16_t STUB_VERSION = 1;

  void loadModule(const std::string &resolved);

  // Binary read primitives
  void readOrFail(std::istream &in, void *dst, size_t size,
                  const std::string &context);
  uint8_t read_u8(std::istream &in, const std::string &context);
  uint16_t read_u16(std::istream &in, const std::string &context);
  uint32_t read_u32(std::istream &in, const std::string &context);
  uint64_t read_u64(std::istream &in, const std::string &context);
  int32_t read_s32(std::istream &in, const std::string &context);
  int64_t read_s64(std::istream &in, const std::string &context);
  std::string readString(std::istream &in, const std::string &context);

  // Type deserialization
  ResolvedType readResolvedType(std::istream &in);
  std::vector<std::pair<ResolvedType, std::string>>
  readParamTypes(std::istream &in);

  // Section readers
  SealFunction readSealFunction(std::istream &in);
  SealTable readSealTable(std::istream &in);

  RecordMember readRecordMember(std::istream &in);
  RecordTable readRecordTable(std::istream &in);
  RecordMethodsTable readMethodsTable(std::istream &in);

  EnumMembers readEnumMember(std::istream &in);
  EnumTable readEnumTable(std::istream &in);

  AllocatorFunction readAllocatorFunction(std::istream &in);
  Allocator readAllocator(std::istream &in);

  FunctionEntry readFunctionEntry(std::istream &in);
  VariableEntry readVariableEntry(std::istream &in);

  Generics readGenerics(std::istream &in);

  void readStubTable(std::istream &in);
  void readModule(std::istream &in);

  //  Logging
  void reportDevBug(const std::string &message);
  void logImportError(const std::string &message, int line, int col);
  void logInternal(const std::string &message);
};
