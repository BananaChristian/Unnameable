#include "irgen.hpp"

void IRGenerator::declareExternalSeals()
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

void IRGenerator::declareImportedComponents()
{
    std::cout << "Registering imported component type" << "\n";

    for (const auto &compTypesPair : semantics.ImportedComponentTable)
    {
        const auto &[compName, typeInfo] = compTypesPair;
        const auto &members = typeInfo->members;
        std::vector<llvm::Type *> fieldTypes;

        for (const auto &memberPair : members)
        {
            const auto &[memberName, memInfo] = memberPair;
            llvm::Type *fieldType = getLLVMType(memInfo->type);
            fieldTypes.push_back(fieldType);
        }

        auto *structTy = llvm::cast<llvm::StructType>(llvmCustomTypes[compName]);
        if (!structTy->isOpaque())
            return;

        structTy->setBody(fieldTypes, false);
    }
}