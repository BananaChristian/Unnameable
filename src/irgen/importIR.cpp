#include <llvm-18/llvm/IR/Function.h>

#include <string>

#include "defs.hpp"
#include "irgen.hpp"
#include "map"

void IRGenerator::declareImportedSeals() {
  for (const auto &modPair : semantics.payload.modules) {
    auto mod = modPair.second;
    for (const auto &sealPair : mod.seals) {
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
        llvm::Type *llvmReturnType = getLLVMType(fnSym->func().returnType);
        std::vector<llvm::Type *> llvmParamTypes;

        for (const auto &param : fnSym->func().paramTypes) {
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
    reportDevBug("Internal IRGen Error: Type stub not found", nullptr);
    return;
  }

  auto *structTy = llvm::cast<llvm::StructType>(typeIt->second);
  if (!structTy->isOpaque()) {
    return; // Body already defined, skip to avoid LLVM double-set panic
  }

  std::map<int, std::pair<std::string, llvm::Type *>> layoutMap;
  std::vector<std::pair<std::string, std::shared_ptr<MemberInfo>>> methods;

  for (const auto &[mName, mInfo] : typeInfo->members) {
    if (mInfo->isFunction) {
      methods.push_back({mName, mInfo});
      continue;
    }
    auto mSym = mInfo->symbolInfo;

    llvm::Type *ty = getLLVMType(mSym->type().type);
    if (!ty) {
      errorHandler.addHint("Failed to resolve type for member: " + mName)
          .addHint("Check if custom type '" + mSym->type().type.resolvedName +
                   "' is fully declared.");
      reportDevBug("Type Resolution Failure", nullptr);
    }

    layoutMap[mSym->type().memberIndex] = std::make_pair(mName, ty);
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
  // for (const auto &methodPair : methods) {
  // declareImportedComponentMethods(methodPair.first, typeName,
  // methodPair.second);
  //}
}

void IRGenerator::declareImportedTypes() {
  // Loop through Data Blocks
  for (const auto &[name, modInfo] : semantics.payload.modules) {
    for (const auto &[name, typeInfo] : modInfo.importedTypes) {
      if (typeInfo->type.kind == DataType::ENUM)
        continue;
      finalizeTypeBody(name, typeInfo, "RECORD");
    }
  }
}

void IRGenerator::declareCustomTypes() {
  for (const auto &[name, info] : semantics.payload.customTypesTable) {
    if (llvmCustomTypes.find(name) == llvmCustomTypes.end()) {
      logInternal("[+] Creating opaque '" + name + "'");
      llvmCustomTypes[name] = llvm::StructType::create(context, name);
    } else {
      logInternal("[.] Already exists '" + name + "'");
    }
  }
}

void IRGenerator::declareImportedFunctions() {
  for (const auto &modPair : semantics.payload.modules) {
    auto mod = modPair.second;
    for (const auto &[name, symInfo] : mod.importedSymbols) {
      if (!symInfo->isFunction)
        continue;

      const auto &funcName = name;

      FunctionCoercion coercion;

      // Coerce parameters
      for (const auto &param : symInfo->func().paramTypes) {
        llvm::Type *originalTy = getLLVMType(param.first);
        coercion.originalParamTypes.push_back(originalTy);

        llvm::Type *coercedTy = originalTy;
        CoercionInfo paramInfo;
        paramInfo.isMemory = false;
        paramInfo.coercedType = originalTy;

        if (auto *structTy = llvm::dyn_cast<llvm::StructType>(originalTy)) {
          paramInfo = classifyStruct(structTy);
          if (paramInfo.isMemory) {
            // Pass large structs by pointer with 'byval'
            coercedTy = structTy->getPointerTo();
            paramInfo.coercedType = coercedTy;
          } else if (paramInfo.coercedType) {
            coercedTy = paramInfo.coercedType;
          }
        }

        coercion.coercedParamTypes.push_back(coercedTy);
        coercion.paramCoercion.push_back(paramInfo);
      }

      // Coerce return type
      llvm::Type *retType = getLLVMType(symInfo->func().returnType);
      coercion.returnCoercion.isMemory = false;
      coercion.returnCoercion.coercedType = retType;

      if (auto *structTy = llvm::dyn_cast<llvm::StructType>(retType)) {
        coercion.returnCoercion = classifyStruct(structTy);
        if (coercion.returnCoercion.isMemory) {
          // Return large structs by reference add hidden sret parameter
          // This requires modifying the function type to add a pointer
          // parameter For now, just use integer coercion
          retType = coercion.returnCoercion.coercedType;
          if (!retType)
            retType = llvm::Type::getVoidTy(context);
        } else if (coercion.returnCoercion.coercedType) {
          retType = coercion.returnCoercion.coercedType;
        }
      }

      // Create function type with coerced parameters
      llvm::FunctionType *fnType =
          llvm::FunctionType::get(retType, coercion.coercedParamTypes, false);
      llvm::Function *fn = llvm::Function::Create(
          fnType, llvm::Function::ExternalLinkage, funcName, module.get());

      // Add 'byval' attribute for large struct parameters
      for (size_t i = 0; i < coercion.paramCoercion.size(); i++) {
        if (coercion.paramCoercion[i].isMemory) {
          fn->addParamAttr(i, llvm::Attribute::ByVal);
        }
      }

      functionCoercionMap[fn] = coercion;
    }
  }
}

void IRGenerator::declareImportedVariables() {
  for (const auto &modPair : semantics.payload.modules) {
    auto mod = modPair.second;
    for (const auto &[varName, varSym] : mod.importedSymbols) {
      llvm::Type *varType = getLLVMType(varSym->type().type);
      if (!varType)
        reportDevBug("Failed to get LLVM type for variable '" + varName + "'",
                     nullptr);

      if (module->getGlobalVariable(varName)) {
        logInternal("Variable '" + varName +
                    "' was already declared, skipping...");
        continue;
      }

      // Create external global declaration
      llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
          *module, varType, varSym->storage().isConstant,
          llvm::GlobalValue::ExternalLinkage, nullptr, varName);

      varSym->codegen().llvmValue = globalVar;
      varSym->codegen().llvmType = varType;
      logInternal("Declared imported variable: " + varName);
    }
  }
}
