#include "irgen.hpp"
#include "map"
#include <llvm-18/llvm/IR/Function.h>
#include <string>

void IRGenerator::declareImportedSeals() {
  for (const auto &sealPair : semantics.sealTable) {
    const std::string &sealName = sealPair.first;
    for (const auto &fnPair : sealPair.second) {
      const std::string &callName = fnPair.first; // e.g. "add"
      auto fnSym = fnPair.second;

      // Create the MANGLED name
      std::string mangledName = sealName + "_" + callName; // e.g. "Test_add"

      llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
      if (fnSym->isExportable)
        linkage = llvm::Function::ExternalLinkage;

      // Convert ResolvedType and ParamTypes to LLVM Types
      llvm::Type *llvmReturnType = getLLVMType(fnSym->returnType);
      std::vector<llvm::Type *> llvmParamTypes;

      for (const auto &param : fnSym->paramTypes) {
        llvmParamTypes.push_back(getLLVMType(param.first));
      }

      // Create the function type
      llvm::FunctionType *fnType =
          llvm::FunctionType::get(llvmReturnType, llvmParamTypes,
                                  false // Not variadic
          );

      // This is a declaration, not a definition, so the body is implicitly
      // external
      llvm::Function::Create(fnType, linkage, mangledName, module.get());
    }
  }
}

void IRGenerator::finalizeTypeBody(
    const std::string &typeName,
    const std::shared_ptr<CustomTypeInfo> &typeInfo, std::string category) {

  logInternal("Finalizing " + category + ": " + typeName);

  // Locate the Opaque Stub
  auto typeIt = llvmCustomTypes.find(typeName);
  if (typeIt == llvmCustomTypes.end()) {
    errorHandler
        .addHint("Type '" + typeName + "' missing from llvmCustomTypes.")
        .addHint("Ensure Phase 1 (declareCustomTypes) ran for this module.");
    reportDevBug("Internal IRGen Error: Type stub not found", 0, 0);
    return;
  }

  auto *structTy = llvm::cast<llvm::StructType>(typeIt->second);
  if (!structTy->isOpaque()) {
    return; // Body already defined, skip to avoid LLVM double-set panic
  }

  // Separate Data (Layout) from Methods
  // Use a map for layout to force correct index ordering (0, 1, 2...)
  std::map<int, std::pair<std::string, llvm::Type *>> layoutMap;
  std::vector<std::pair<std::string, std::shared_ptr<MemberInfo>>> methods;

  for (const auto &[mName, mInfo] : typeInfo->members) {
    if (mInfo->isFunction) {
      methods.push_back({mName, mInfo});
      continue;
    }

    llvm::Type *ty = getLLVMType(mInfo->type);
    if (!ty) {
      errorHandler.addHint("Failed to resolve type for member: " + mName)
          .addHint("Check if custom type '" + mInfo->type.resolvedName +
                   "' is fully declared.");
      reportDevBug("Type Resolution Failure", 0, 0);
    }

    layoutMap[mInfo->memberIndex] = std::make_pair(mName, ty);
  }

  // Flatten the Map into a Dense Field Vector
  std::vector<llvm::Type *> fieldTypes;
  for (auto const &[index, pair] : layoutMap) {
    fieldTypes.push_back(pair.second);
  }

  // Commit Body to LLVM
  structTy->setBody(fieldTypes, false);
  logInternal("Committed body for " + typeName);

  // Generate Method Declarations
  for (const auto &methodPair : methods) {
    declareImportedComponentMethods(methodPair.first, typeName,
                                    methodPair.second);
  }

  // Handle Component-specific logic
  if (category == "COMPONENT") {
    componentTypes[typeName] = structTy;
    declareImportedInit(typeName);
  }
}

void IRGenerator::declareImportedTypes() {
  // Loop through Components
  for (const auto &[name, typeInfo] : semantics.ImportedComponentTable) {
    finalizeTypeBody(name, typeInfo, "COMPONENT");
  }

  // Loop through Data Blocks
  for (const auto &[name, typeInfo] : semantics.ImportedRecordTable) {
    finalizeTypeBody(name, typeInfo, "RECORD");
  }
}

void IRGenerator::declareCustomTypes() {
  for (const auto &[name, info] : semantics.customTypesTable) {
    if (llvmCustomTypes.find(name) == llvmCustomTypes.end()) {
      logInternal("[+] Creating opaque '" + name + "'");
      llvmCustomTypes[name] = llvm::StructType::create(context, name);
    } else {
      logInternal("[.] Already exists '" + name + "'");
    }
  }
}

void IRGenerator::declareImportedComponentMethods(
    const std::string &funcName, const std::string &typeName,
    const std::shared_ptr<MemberInfo> &memberInfo) {

  logInternal("Importing method '" + funcName + "'");

  // Name Mangling
  std::string methodName = typeName + "_" + funcName;
  logInternal("Mangled name '" + methodName + "'");

  // The 'self' Pointer (This pointer)
  auto typeIt = llvmCustomTypes.find(typeName);
  if (typeIt == llvmCustomTypes.end()) {
    reportDevBug("Could not find base type '" + typeName +
                     "' for method declaration",
                 0, 0);
  }

  llvm::Type *thisPtrType = typeIt->second->getPointerTo();
  std::vector<llvm::Type *> paramTypes = {thisPtrType};
  logInternal("Added 'self' paramter as pointer to '" + typeName + "'");

  // User Parameters
  auto params = memberInfo->paramTypes;
  for (size_t i = 0; i < params.size(); ++i) {
    llvm::Type *pTy = getLLVMType(params[i].first);
    if (!pTy) {
      logInternal("Failed to resolve type for param " + std::to_string(i) +
                  " (" + params[i].second + ")");
      continue;
    }
    paramTypes.push_back(pTy);
  }

  // Return Type Investigation
  ResolvedType retType = memberInfo->returnType;

  llvm::Type *llvmRetTy = getLLVMType(retType);

  // Function Creation
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(llvmRetTy, paramTypes, false);

  // Check if function already exists in module to avoid symbol collisions
  if (module->getFunction(methodName)) {
    logInternal("Function '" + methodName +
                "' already declared in module skipping creation");
    return;
  }

  llvm::Function *declaredfn = llvm::Function::Create(
      fnType, llvm::Function::ExternalLinkage, methodName, module.get());

  // Name the 'self' parameter in the IR
  if (declaredfn->arg_size() > 0) {
    auto argIt = declaredfn->arg_begin();
    argIt->setName(typeName + ".self");
    logInternal("Name arg(0) as: " + typeName + ".self");
  }
}

void IRGenerator::declareImportedInit(const std::string &typeName) {
  auto it = semantics.importedInits.find(typeName);
  if (it == semantics.importedInits.end()) {
    return; // There is no init so dont bother
  }

  auto initSym = it->second;
  std::string initName = typeName + "_init";

  if (module->getFunction(initName))
    return; // It was already declared so dont bother

  auto structTy = llvmCustomTypes[typeName];
  std::vector<llvm::Type *> paramTypes = {structTy->getPointerTo()};
  for (const auto &argType : initSym->initArgs) {
    paramTypes.push_back(getLLVMType(argType));
  }

  llvm::FunctionType *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context), paramTypes, false);

  // Create the External Declaration
  llvm::Function *initFn = llvm::Function::Create(
      fnType, llvm::Function::ExternalLinkage, initName, module.get());

  // Label that first param
  if (initFn->arg_size() > 0) {
    initFn->arg_begin()->setName(typeName + ".self");
  }

  logInternal("Declared imported init '" + initName + "'");
}
