#include "defs.hpp"
#include "errors.hpp"
#include "semantics.hpp"

void Semantics::walkModuleStatement(Node *node) {
  auto modStmt = dynamic_cast<ModuleStatement *>(node);
  if (!modStmt)
    return;

  if (moduleSet)
    logSemanticErrors(ErrorCode::AlreadySetModule, modStmt);

  if (!isGlobalScope()) {
    logSemanticErrors(ErrorCode::ModMustBeGlobal, modStmt);
    return;
  }

  moduleSet = true;
}

void Semantics::walkImportStatement(Node *node) {
  auto importStmt = dynamic_cast<ImportStatement *>(node);
  if (!importStmt)
    return;

  if (!isGlobalScope()) {
    logSemanticErrors(ErrorCode::ModMustBeGlobal, importStmt);
    return;
  }

  auto module_name = extractIdentifierName(importStmt->module_name.get());

  auto it = imports.find(module_name);
  if (it == imports.end())
    imports.insert(module_name);
  else {
    logSemanticErrors(ErrorCode::AlreadyImportedModule, importStmt);
    return;
  }

  auto aliasName = importStmt->alias
                       ? extractIdentifierName(importStmt->alias.get())
                       : module_name;

  Modulespace module;
  module.originalName = module_name;
  module.aliasName = aliasName;

  importModule(module);
}

/// Build a MemberInfo from a RecordMember.
static std::shared_ptr<MemberInfo>
memberInfoFromRecordMember(const RecordMember &m) {
  auto info = std::make_shared<MemberInfo>();
  info->memberName = m.memberName;
  info->symbolInfo->type().type = m.type;
  info->symbolInfo->type().memberIndex = m.memberIndex;
  info->symbolInfo->type().isNullable = m.isNullable;
  info->symbolInfo->storage().isMutable = m.isMutable;
  info->symbolInfo->storage().isConstant = m.isConstant;
  info->symbolInfo->type().isPointer = m.isPointer;
  info->symbolInfo->type().isRef = m.isRef;
  info->symbolInfo->isExportable = true;
  return info;
}

/// Build a SymbolInfo for a standalone or generic imported function.
static std::shared_ptr<SymbolInfo>
symInfoFromFunctionEntry(const FunctionEntry &fn) {
  auto sym = std::make_shared<SymbolInfo>();
  sym->isFunction = true;
  sym->isExportable = true;
  sym->func().isDeclaration = true;
  sym->func().isDefined = true;
  sym->func().funcName = fn.funcName;
  sym->func().returnType = fn.returnType;
  sym->func().paramTypes = fn.paramTypes;
  sym->type().type = fn.returnType;
  return sym;
}

static std::shared_ptr<SymbolInfo>
symInfoFromVariableEntry(const VariableEntry &var) {
  auto sym = std::make_shared<SymbolInfo>();
  sym->type().type = var.declaredType;
  sym->storage().isMutable = var.isMutable;
  sym->storage().isInitialized = var.isInitialized;

  return sym;
}

static void registerTypeSymbol(
    const std::string &name, DataType kind,
    const std::unordered_map<std::string, std::shared_ptr<MemberInfo>> &members,
    std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>>
        &customTypesTable) {
  auto typeInfo = std::make_shared<CustomTypeInfo>();
  typeInfo->typeName = name;
  typeInfo->type = ResolvedType::makeBase(kind, name);
  typeInfo->members = members;
  typeInfo->isExportable = true;

  customTypesTable[name] = typeInfo;

  auto sym = std::make_shared<SymbolInfo>();
  sym->isExportable = true;
  sym->members = members;
  sym->type().type = ResolvedType::makeBase(kind, name);
}

// Special to register imported functions from standalone and from generics
static void registerFunctionSymbol(
    const FunctionEntry &funcEntry,
    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> &importTable) {
  auto funcInfo = std::make_shared<SymbolInfo>();
  funcInfo = symInfoFromFunctionEntry(funcEntry);
  // Register into the imported functions table
  importTable[funcEntry.funcName] = funcInfo;
}

static void registerVariableSymbol(
    const VariableEntry &varEntry,
    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> &importTable) {
  auto varInfo = std::make_shared<SymbolInfo>();
  varInfo = symInfoFromVariableEntry(varEntry);
  // Register into imported vars table
  importTable[varEntry.var_name] = varInfo;
}

void Semantics::importSeals(Modulespace &space) {
  logInternal("Importing seals: " +
              std::to_string(deserializer.module.exports.seals.size()));

  for (const auto &seal : deserializer.module.exports.seals) {
    logInternal("Importing seal '" + seal.sealName + "'");

    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> sealMap;

    for (const auto &fn : seal.sealFns) {
      logInternal("  function '" + fn.funcName + "'");
      // Reuse FunctionEntry adapter — SealFunction has the same shape
      FunctionEntry fe;
      fe.funcName = fn.funcName;
      fe.returnType = fn.returnType;
      fe.paramTypes = fn.paramTypes;
      sealMap[fn.funcName] = symInfoFromFunctionEntry(fe);
    }

    space.seals[seal.sealName] = std::move(sealMap);

    auto sealSym = std::make_shared<SymbolInfo>();
    sealSym->isExportable = true;

    logInternal("Finished importing seal '" + seal.sealName + "'");
  }
}

void Semantics::importRecords(Modulespace &space) {
  logInternal("Importing records: " +
              std::to_string(deserializer.module.exports.records.size()));

  for (const auto &record : deserializer.module.exports.records) {
    logInternal("Importing record '" + record.recordName + "'");

    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;

    for (const auto &m : record.members) {
      logInternal("  member '" + m.memberName + "'");
      members[m.memberName] = memberInfoFromRecordMember(m);
    }

    // Register into customTypesTable, ImportedRecordTable and symbolTable[0]
    registerTypeSymbol(record.recordName, DataType::RECORD, members,
                       space.importedTypes);

    logInternal("Finished importing record '" + record.recordName + "'");
  }
}

void Semantics::importMethods(Modulespace &space) {
  logInternal("Importing methods: " +
              std::to_string(deserializer.module.exports.enums.size()));

  for (const auto &met : deserializer.module.exports.methods) {
    logInternal("Importing methods for record: '" + met.recordName);

    auto importedTypeIt = space.importedTypes.find(met.recordName);
    if (importedTypeIt == space.importedTypes.end()) {
      // TODO: Add an error here
      return;
    }

    auto tyInfo = importedTypeIt->second;
    if (tyInfo->type.kind != DataType::RECORD)
      continue;

    for (const auto &fn : met.methods) {
      auto fnSym = symInfoFromFunctionEntry(fn);
      auto fnMem = std::make_shared<MemberInfo>();
      fnMem->symbolInfo = fnSym;
      fnMem->memberName = fn.funcName;
      tyInfo->members[fn.funcName] = fnMem;
    }
  }
}

void Semantics::importEnums(Modulespace &space) {
  logInternal("Importing enums: " +
              std::to_string(deserializer.module.exports.enums.size()));

  for (const auto &en : deserializer.module.exports.enums) {
    logInternal("Importing enum '" + en.enumName + "'");

    auto typeInfo = std::make_shared<CustomTypeInfo>();
    typeInfo->typeName = en.enumName;
    typeInfo->type = ResolvedType::makeBase(DataType::ENUM, en.enumName);
    typeInfo->underLyingType = en.underlyingType.kind;
    typeInfo->isExportable = true;

    std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;

    for (const auto &m : en.members) {
      logInternal("  member '" + m.memberName + "'");

      auto memInfo = std::make_shared<MemberInfo>();
      memInfo->memberName = m.memberName;
      memInfo->symbolInfo->type().type = m.type;
      memInfo->constantValue = m.constantValue;
      memInfo->parentType = m.enumType;
      memInfo->symbolInfo->storage().isConstant = true;
      memInfo->symbolInfo->storage().isInitialized = true;
      memInfo->symbolInfo->isExportable = true;

      typeInfo->members[m.memberName] = memInfo;
      members[m.memberName] = memInfo;
    }

    space.importedTypes[en.enumName] = typeInfo;

    auto sym = std::make_shared<SymbolInfo>();
    sym->isExportable = true;
    sym->members = members;
    sym->type().type = ResolvedType::makeBase(DataType::ENUM, en.enumName);

    logInternal("Finished importing enum '" + en.enumName + "'");
  }
}

void Semantics::importAllocators(Modulespace &space) {
  logInternal("Importing allocators: " +
              std::to_string(deserializer.module.exports.allocators.size()));

  for (const auto &alloc : deserializer.module.exports.allocators) {
    logInternal("Importing allocator '" + alloc.allocatorName + "'");

    AllocatorHandle handle;

    handle.allocateName = alloc.allocator.functionName;
    auto allocSym = std::make_shared<SymbolInfo>();
    allocSym->func().returnType = alloc.allocator.returnType;
    allocSym->func().paramTypes = alloc.allocator.paramTypes;
    allocSym->type().type = alloc.allocator.returnType;
    handle.allocatorSymbol = allocSym;
    logInternal("  allocate '" + handle.allocateName + "'");

    handle.freeName = alloc.free.functionName;
    auto freeSym = std::make_shared<SymbolInfo>();
    freeSym->func().returnType = alloc.free.returnType;
    freeSym->func().paramTypes = alloc.free.paramTypes;
    freeSym->type().type = alloc.free.returnType;
    handle.freeSymbol = freeSym;
    logInternal("  free '" + handle.freeName + "'");

    payload.allocatorMap[alloc.allocatorName] = handle;

    auto interfaceSym = std::make_shared<SymbolInfo>();
    interfaceSym->isExportable = true;

    logInternal("Finished importing allocator '" + alloc.allocatorName + "'");
  }
}

void Semantics::importFunctions(Modulespace &space) {
  logInternal("Importing standalone functions: " +
              std::to_string(deserializer.module.exports.functions.size()));

  for (const auto &fn : deserializer.module.exports.functions) {
    logInternal("Importing function '" + fn.funcName + "'");
    registerFunctionSymbol(fn, space.importedSymbols);
    logInternal("Finished importing function '" + fn.funcName + "'");
  }
}

void Semantics::importVariables(Modulespace &space) {
  logInternal("Importing standalone variables: " +
              std::to_string(deserializer.module.exports.variables.size()));
  for (const auto &var : deserializer.module.exports.variables) {
    logInternal("Importing variable '" + var.var_name + "'");
    registerVariableSymbol(var, space.importedSymbols);
    logInternal("Finished importing variable '" + var.var_name + "'");
  }
}

void Semantics::importGenerics(Modulespace &space) {
  logInternal("Importing generic instantiations: " +
              std::to_string(deserializer.module.exports.generics.size()));

  for (const auto &gen : deserializer.module.exports.generics) {
    logInternal("Importing instantiation '" + gen.aliasName + "'");

    for (const auto &record : gen.records) {
      logInternal("  generic record '" + record.recordName + "'");

      std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
      for (const auto &m : record.members)
        members[m.memberName] = memberInfoFromRecordMember(m);

      registerTypeSymbol(record.recordName, DataType::RECORD, members,
                         space.importedTypes);
    }

    for (const auto &fn : gen.functions) {
      logInternal("  generic function '" + fn.funcName + "'");
      registerFunctionSymbol(fn, space.importedSymbols);
    }

    logInternal("Finished importing instantiation '" + gen.aliasName + "'");
  }
}

void Semantics::importModule(Modulespace &space) {
  importSeals(space);
  importRecords(space);
  importMethods(space);
  importEnums(space);
  importAllocators(space);
  importFunctions(space);
  importVariables(space);
  importGenerics(space);
}
