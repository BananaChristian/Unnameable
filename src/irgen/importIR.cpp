#include "irgen.hpp"

void IRGenerator::declareImportedSeals()
{
    for (const auto &sealPair : semantics.sealTable)
    {
        const std::string &sealName = sealPair.first;
        for (const auto &fnPair : sealPair.second)
        {
            const std::string &callName = fnPair.first; // e.g. "add"
            auto fnSym = fnPair.second;

            // Create the MANGLED name
            std::string mangledName = sealName + "_" + callName; // e.g. "Test_add"

            // Convert ResolvedType and ParamTypes to LLVM Types
            llvm::Type *llvmReturnType = getLLVMType(fnSym->returnType);
            std::vector<llvm::Type *> llvmParamTypes;

            for (const auto &param : fnSym->paramTypes)
            {
                llvmParamTypes.push_back(getLLVMType(param.first));
            }

            // Create the function type
            llvm::FunctionType *fnType = llvm::FunctionType::get(
                llvmReturnType,
                llvmParamTypes,
                false // Not variadic
            );

            // This is a declaration, not a definition, so the body is implicitly external
            llvm::Function::Create(
                fnType,
                llvm::Function::ExternalLinkage,
                mangledName,
                module.get());
        }
    }
}

void IRGenerator::finalizeTypeBody(const std::string &typeName, const std::shared_ptr<CustomTypeInfo> &typeInfo, std::string category)
{
    std::cout << "\n  >>> Finalizing [" << category << "]: " << typeName << " <<<\n";

    auto typeIt = llvmCustomTypes.find(typeName);
    if (typeIt == llvmCustomTypes.end())
    {
        std::cerr << "  [!!!] ERROR: " << typeName << " not found in CustomTypes map!\n";
        return;
    }

    auto *structTy = llvm::cast<llvm::StructType>(typeIt->second);
    if (!structTy->isOpaque())
    {
        std::cout << "  [i] Skipping: Body already defined.\n";
        return;
    }

    // Collect and Sort Members
    // Usage of the map will ensure indices are sorted 0, 1, 2... and prevents gaps.
    std::map<int, std::pair<std::string, llvm::Type *>> layoutMap;

    std::cout << "  [Step A] Resolving Members:\n";
    for (const auto &[mName, mInfo] : typeInfo->members)
    {
        if (mInfo->isFunction)
        {
            std::cout << "    - (Func) " << mName << " [Skipped]\n";
            continue;
        }

        llvm::Type *ty = getLLVMType(mInfo->type);
        if (!ty)
        {
            std::cout << "    - (DATA) " << mName << " [!!! RESOLUTION FAILED !!!]\n";
            // Safety fallback to prevent LLVM Segfault
            ty = llvm::Type::getInt32Ty(context);
        }
        else
        {
            std::string typeNameStr;
            llvm::raw_string_ostream rso(typeNameStr);
            ty->print(rso);
            std::cout << "    - (DATA) " << mName << " | Index: " << mInfo->memberIndex << " | Type: " << typeNameStr << "\n";
        }
        layoutMap[mInfo->memberIndex] = {mName, ty};
    }

    // Build Dense Vector ("Anti-Segfault" Shield)
    std::vector<llvm::Type *> fieldTypes;
    std::cout << "  [Step B] Packing Struct Layout:\n";

    for (auto const &[index, pair] : layoutMap)
    {
        const auto &[name, ty] = pair;
        fieldTypes.push_back(ty);
        std::cout << "    Slot " << fieldTypes.size() - 1 << " <- " << name << " (orig index " << index << ")\n";
    }

    // Commit to LLVM
    structTy->setBody(fieldTypes, false);
    std::cout << "  [Step C] " << typeName << " body committed to LLVM context.\n";
}

void IRGenerator::declareImportedTypes()
{
    std::cout << "\n"
              << std::string(50, '=') << "\n";
    std::cout << "[PHASE 2] IMPORT BODY FINALIZATION\n";
    std::cout << std::string(50, '=') << "\n";

    // Loop through Components
    for (const auto &[name, typeInfo] : semantics.ImportedComponentTable)
    {
        finalizeTypeBody(name, typeInfo, "COMPONENT");
    }

    // Loop through Data Blocks
    for (const auto &[name, typeInfo] : semantics.ImportedDataBlocksTable)
    {
        finalizeTypeBody(name, typeInfo, "DATABLOCK");
    }

    std::cout << "\n"
              << std::string(50, '=') << "\n";
    std::cout << "[IR GENERATION STARTING...]\n";
    std::cout << std::string(50, '=') << "\n";
}

void IRGenerator::declareCustomTypes()
{
    std::cout << "\n"
              << std::string(50, '=') << "\n";
    std::cout << "[PHASE 1] DECLARING OPAQUE STUBS\n";
    std::cout << std::string(50, '=') << "\n";

    for (const auto &[name, info] : semantics.customTypesTable)
    {
        if (llvmCustomTypes.find(name) == llvmCustomTypes.end())
        {
            std::cout << "  [+] Creating Opaque: " << name << "\n";
            llvmCustomTypes[name] = llvm::StructType::create(context, name);
        }
        else
        {
            std::cout << "  [.] Already Exists: " << name << "\n";
        }
    }
}