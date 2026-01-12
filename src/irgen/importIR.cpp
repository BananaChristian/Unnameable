#include "irgen.hpp"

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
  std::cout << "\n  >>> Finalizing [" << category << "]: " << typeName
            << " <<<\n";

  auto typeIt = llvmCustomTypes.find(typeName);
  if (typeIt == llvmCustomTypes.end()) {
    std::cerr << "  [!!!] ERROR: " << typeName
              << " not found in CustomTypes map!\n";
    return;
  }

  auto *structTy = llvm::cast<llvm::StructType>(typeIt->second);
  if (!structTy->isOpaque()) {
    std::cout << "  [i] Skipping: Body already defined.\n";
    return;
  }

  // Collect and Sort Members
  // Usage of the map will ensure indices are sorted 0, 1, 2... and prevents
  // gaps.
  std::map<int, std::pair<std::string, llvm::Type *>> layoutMap;
  std::vector<std::pair<std::string, std::shared_ptr<MemberInfo>>> methods;

  std::cout << "  [Step A] Resolving Members:\n";
  for (const auto &[mName, mInfo] : typeInfo->members) {
    if (mInfo->isFunction) {
      std::cout << "    - (Func) " << mName
                << " [Routed to method declaration storage]\n";
      methods.push_back({mName, mInfo});
      continue;
    }

    llvm::Type *ty = getLLVMType(mInfo->type);
    if (!ty) {
      std::cout << "    - (DATA) " << mName << " [!!! RESOLUTION FAILED !!!]\n";
      // Safety fallback to prevent LLVM Segfault
      ty = llvm::Type::getInt32Ty(context);
    } else {
      std::string typeNameStr;
      llvm::raw_string_ostream rso(typeNameStr);
      ty->print(rso);
      std::cout << "    - (DATA) " << mName
                << " | Index: " << mInfo->memberIndex
                << " | Type: " << typeNameStr << "\n";
    }
    layoutMap[mInfo->memberIndex] = {mName, ty};
  }

  // Build Dense Vector ("Anti-Segfault" Shield)
  std::vector<llvm::Type *> fieldTypes;
  std::cout << "  [Step B] Packing Struct Layout:\n";

  for (auto const &[index, pair] : layoutMap) {
    const auto &[name, ty] = pair;
    fieldTypes.push_back(ty);
    std::cout << "    Slot " << fieldTypes.size() - 1 << " <- " << name
              << " (orig index " << index << ")\n";
  }

  // Commit to LLVM
  structTy->setBody(fieldTypes, false);
  std::cout << "  [Step C] " << typeName
            << " body committed to LLVM context.\n";

  // Generate the component methods
  for (const auto &methodPair : methods) {
    declareImportedComponentMethods(methodPair.first, typeName,
                                    methodPair.second);
  }

  componentTypes[typeName] = structTy;

  std::cout << "  [Step D] " << typeName
            << " registered in componentTypes map.\n";

  if (category == "COMPONENT") {
    declareImportedInit(typeName);
  }
}

void IRGenerator::declareImportedTypes() {
  std::cout << "\n" << std::string(50, '=') << "\n";
  std::cout << "[PHASE 2] IMPORT BODY FINALIZATION\n";
  std::cout << std::string(50, '=') << "\n";

  // Loop through Components
  for (const auto &[name, typeInfo] : semantics.ImportedComponentTable) {
    finalizeTypeBody(name, typeInfo, "COMPONENT");
  }

  // Loop through Data Blocks
  for (const auto &[name, typeInfo] : semantics.ImportedRecordTable) {
    finalizeTypeBody(name, typeInfo, "RECORD");
  }

  std::cout << "\n" << std::string(50, '=') << "\n";
  std::cout << "[IR GENERATION STARTING...]\n";
  std::cout << std::string(50, '=') << "\n";
}

void IRGenerator::declareCustomTypes() {
  std::cout << "\n" << std::string(50, '=') << "\n";
  std::cout << "[PHASE 1] DECLARING OPAQUE STUBS\n";
  std::cout << std::string(50, '=') << "\n";

  for (const auto &[name, info] : semantics.customTypesTable) {
    if (llvmCustomTypes.find(name) == llvmCustomTypes.end()) {
      std::cout << "  [+] Creating Opaque: " << name << "\n";
      llvmCustomTypes[name] = llvm::StructType::create(context, name);
    } else {
      std::cout << "  [.] Already Exists: " << name << "\n";
    }
  }
}

void IRGenerator::declareImportedComponentMethods(
    const std::string &funcName, const std::string &typeName,
    const std::shared_ptr<MemberInfo> &memberInfo) {
  std::cout << "\n[METHOD IMPORT] >>> Starting Declaration: " << typeName
            << "::" << funcName << " <<<\n";

  // Name Mangling
  std::string methodName = typeName + "_" + funcName;
  std::cout << "  [DEBUG] Mangled Name: " << methodName << "\n";

  // The 'self' Pointer (This pointer)
  auto typeIt = llvmCustomTypes.find(typeName);
  if (typeIt == llvmCustomTypes.end()) {
    std::cerr << "  [CRITICAL] Could not find base type '" << typeName
              << "' for method declaration!\n";
    return;
  }

  llvm::Type *thisPtrType = typeIt->second->getPointerTo();
  std::vector<llvm::Type *> paramTypes = {thisPtrType};
  std::cout << "  [DEBUG] Added 'self' parameter as pointer to: " << typeName
            << "\n";

  // User Parameters
  auto params = memberInfo->paramTypes;
  for (size_t i = 0; i < params.size(); ++i) {
    llvm::Type *pTy = getLLVMType(params[i].first);
    if (!pTy) {
      std::cerr << "  [ERROR] Failed to resolve type for param " << i << " ("
                << params[i].second << ")\n";
      continue;
    }
    paramTypes.push_back(pTy);

    std::string tyStr;
    llvm::raw_string_ostream rso(tyStr);
    pTy->print(rso);
    std::cout << "  [DEBUG] Param " << i << " [" << params[i].second
              << "] resolved to: " << tyStr << "\n";
  }

  // Return Type Investigation
  ResolvedType retType = memberInfo->returnType;
  std::cout << "  [DEBUG] Metadata claims Return Type: " << retType.resolvedName
            << " (Kind: " << (int)retType.kind << ")\n";

  llvm::Type *llvmRetTy = getLLVMType(retType);

  // Print the actual LLVM type to catch return type discrepancy
  std::string retTyStr;
  llvm::raw_string_ostream rsoRet(retTyStr);
  llvmRetTy->print(rsoRet);
  std::cout << "  [DEBUG] lowerFunctionType result: " << retTyStr << "\n";

  // Function Creation
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(llvmRetTy, paramTypes, false);

  // Check if function already exists in module to avoid symbol collisions
  if (module->getFunction(methodName)) {
    std::cout << "  [WARN] Function " << methodName
              << " already declared in module. Skipping creation.\n";
    return;
  }

  llvm::Function *declaredfn = llvm::Function::Create(
      fnType, llvm::Function::ExternalLinkage, methodName, module.get());

  // Name the 'self' parameter in the IR
  if (declaredfn->arg_size() > 0) {
    auto argIt = declaredfn->arg_begin();
    argIt->setName(typeName + ".self");
    std::cout << "  [DEBUG] Named arg(0) as: " << typeName << ".self\n";
  }

  std::cout << "[METHOD IMPORT] <<< Declaration Complete >>>\n\n";
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

  std::cout << "  [Step E] Declared Imported Init: " << initName << "\n";
}
