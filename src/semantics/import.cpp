#include "semantics.hpp"

ResolvedType Semantics::convertImportedTypetoResolvedType(const ImportedType &importType)
{
    ResolvedType type;
    type.kind = convertImportedDataTypetoResolvedDataType(importType.kind);
    type.resolvedName = importType.resolvedName;
    type.isPointer = importType.isPointer;
    type.isRef = importType.isRef;
    type.isNull = importType.isNull;
    type.isArray = importType.isArray;

    if (importType.innerType)
    {
        const ImportedType &innerImportedType = *importType.innerType;
        ResolvedType innerResolvedType = convertImportedTypetoResolvedType(innerImportedType);
        type.innerType = std::make_shared<ResolvedType>(innerResolvedType);
    }
    else
    {
        type.innerType = nullptr;
    }

    return type;
}

StorageType Semantics::convertImportedStorageTypetoStorageType(const ImportedStorageType &storageType)
{
    switch (storageType)
    {
    case ImportedStorageType::GLOBAL:
        return StorageType::GLOBAL;
    case ImportedStorageType::HEAP:
        return StorageType::HEAP;
    case ImportedStorageType::STACK:
        return StorageType::STACK;
    default:
        return StorageType::GLOBAL; // A failure to convert make it global although I really dont expect this to happen
    }
}

std::vector<std::pair<ResolvedType, std::string>> Semantics::convertImportedParamstoResolvedParams(const std::vector<std::pair<ImportedType, std::string>> &params)
{
    std::vector<std::pair<ResolvedType, std::string>> resolvedParams;
    resolvedParams.reserve(params.size());

    for (const auto &p : params)
    {
        const ImportedType &importedType = p.first;
        const std::string &paramName = p.second;

        ResolvedType resolvedType = convertImportedTypetoResolvedType(importedType);
        resolvedParams.emplace_back(resolvedType, paramName);
    }

    return resolvedParams;
}

DataType Semantics::convertImportedDataTypetoResolvedDataType(const ImportedDataType &dataType)
{
    switch (dataType)
    {
    case ImportedDataType::I8:
        return DataType::I8;
    case ImportedDataType::I16:
        return DataType::I16;
    case ImportedDataType::U16:
        return DataType::U16;
    case ImportedDataType::I32:
        return DataType::I32;
    case ImportedDataType::U32:
        return DataType::U32;
    case ImportedDataType::I64:
        return DataType::I64;
    case ImportedDataType::U64:
        return DataType::U64;
    case ImportedDataType::I128:
        return DataType::I128;
    case ImportedDataType::U128:
        return DataType::U128;
    case ImportedDataType::ISIZE:
        return DataType::ISIZE;
    case ImportedDataType::USIZE:
        return DataType::USIZE;
    case ImportedDataType::BOOLEAN:
        return DataType::BOOLEAN;
    case ImportedDataType::STRING:
        return DataType::STRING;
    case ImportedDataType::F32:
        return DataType::F32;
    case ImportedDataType::F64:
        return DataType::F64;
    case ImportedDataType::CHAR8:
        return DataType::CHAR8;
    case ImportedDataType::CHAR16:
        return DataType::CHAR16;
    case ImportedDataType::CHAR32:
        return DataType::CHAR32;
    case ImportedDataType::ENUM:
        return DataType::ENUM;
    case ImportedDataType::RECORD:
        return DataType::RECORD;
    case ImportedDataType::COMPONENT:
        return DataType::COMPONENT;
    case ImportedDataType::ERROR:
        return DataType::ERROR;
    case ImportedDataType::VOID:
        return DataType::VOID;
    case ImportedDataType::GENERIC:
        return DataType::GENERIC;
    case ImportedDataType::UNKNOWN:
        return DataType::UNKNOWN;
    default:
        return DataType::UNKNOWN;
    }
}

void Semantics::importSeals()
{
    std::cout << "Imported seals count: " << deserializer.importedSealTable.size() << "\n";

    // Iterate over all seals
    for (auto &sealPair : deserializer.importedSealTable)
    {
        const std::string &sealName = sealPair.first;
        std::cout << "IMPORTED SEAL NAME: " << sealName << "\n";

        std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> sealMap;

        auto &funcMap = sealPair.second;

        // Iterate over all functions in the seal
        for (auto &funcPair : funcMap)
        {
            const std::string &funcName = funcPair.first;
            std::cout << "IMPORTED FUNCTION NAME: " << funcName << "\n";
            ImportedSymbolInfo &info = funcPair.second;

            // Create symbolInfo for the function from the imported info
            auto sym = std::make_shared<SymbolInfo>();
            sym->isFunction = true;
            sym->isDeclaration = true; // Assume it exists in some form in the external module
            sym->type = convertImportedTypetoResolvedType(info.returnType);
            sym->returnType = convertImportedTypetoResolvedType(info.returnType);
            sym->paramTypes = convertImportedParamstoResolvedParams(info.paramTypes);

            sealMap[funcName] = sym;
        }

        sealTable[sealName] = std::move(sealMap);
        auto sealSym = std::make_shared<SymbolInfo>();
        sealSym->isExportable = true;

        symbolTable[0][sealName] = sealSym; // Inject the seal symbol into the semantics symbols
    }
}

void Semantics::importComponents()
{
    std::cout << "Imported components count: " << deserializer.importedComponentTable.size() << "\n";

    for (auto &compPair : deserializer.importedComponentTable)
    {
        const std::string &compName = compPair.first;
        std::cout << "IMPORTED COMPONENT NAME: " << compName << "\n";

        auto typeInfo = std::make_shared<CustomTypeInfo>();

        typeInfo->typeName = compName;
        typeInfo->type = ResolvedType{DataType::COMPONENT, compName};

        std::unordered_map<std::string, std::shared_ptr<MemberInfo>> symMembers;
        auto &memberMap = compPair.second;

        // Iterate over all members in component
        for (auto &memPair : memberMap)
        {
            const std::string &memberName = memPair.first;
            std::cout << "IMPORTED COMPONENT MEMBER NAME: " << memberName << "\n";

            ImportedSymbolInfo &info = memPair.second;

            // Create member info for the member
            auto memInfo = std::make_shared<MemberInfo>();
            memInfo->type = convertImportedTypetoResolvedType(info.type);
            memInfo->returnType = convertImportedTypetoResolvedType(info.returnType);
            memInfo->paramTypes = convertImportedParamstoResolvedParams(info.paramTypes);
            memInfo->memberName = memberName;
            memInfo->isNullable = info.isNullable;
            memInfo->isExportable = true;
            memInfo->memberIndex = info.memberIndex;
            memInfo->storage = convertImportedStorageTypetoStorageType(info.storage);
            memInfo->isDeclared = true;
            memInfo->isConstant = info.isConstant;
            memInfo->isPointer = info.isPointer;
            memInfo->isRef = info.isRef;
            memInfo->isMutable = info.isMutable;
            memInfo->isFunction = info.isFunction;
            if (info.isFunction)
            {
                memInfo->isDeclared = true; // By default if it is a function and it has made it this far then it was declared
            }

            typeInfo->members[memberName] = memInfo;
            symMembers[memberName] = memInfo;
        }

        customTypesTable[compName] = typeInfo;
        ImportedComponentTable[compName] = typeInfo;
        auto compSym = std::make_shared<SymbolInfo>();
        compSym->isExportable = true;
        compSym->members = symMembers;
        compSym->type = ResolvedType{DataType::COMPONENT, compName};

        symbolTable[0][compName] = compSym;
    }
}

void Semantics::importRecords()
{
    std::cout << "Imported records count: " << deserializer.importedRecordsTable.size();

    for (const auto &recordPair : deserializer.importedRecordsTable)
    {
        const std::string &recordName = recordPair.first;
        std::cout << "IMPORTED RECORD NAME: " << recordName << "\n";

        auto typeInfo = std::make_shared<CustomTypeInfo>();

        typeInfo->typeName = recordName;
        typeInfo->type = ResolvedType{DataType::RECORD, recordName};

        std::unordered_map<std::string, std::shared_ptr<MemberInfo>> symMembers;
        auto &memberMap = recordPair.second;

        for (const auto &memPair : memberMap)
        {
            const std::string &memberName = memPair.first;
            std::cout << "IMPORTED RECORD MEMBER NAME: " << memberName << "\n";

            const ImportedSymbolInfo &info = memPair.second;

            // Create the member inside the member info
            auto memInfo = std::make_shared<MemberInfo>();
            memInfo->type = convertImportedTypetoResolvedType(info.type);
            memInfo->returnType = convertImportedTypetoResolvedType(info.returnType);
            memInfo->paramTypes = convertImportedParamstoResolvedParams(info.paramTypes);
            memInfo->memberName = memberName;
            memInfo->isNullable = info.isNullable;
            memInfo->isExportable = true;
            memInfo->memberIndex = info.memberIndex;
            memInfo->storage = convertImportedStorageTypetoStorageType(info.storage);
            memInfo->isDeclared = true;
            memInfo->isConstant = info.isConstant;
            memInfo->isPointer = info.isPointer;
            memInfo->isRef = info.isRef;
            memInfo->isMutable = info.isMutable;

            typeInfo->members[memberName] = memInfo;
            symMembers[memberName] = memInfo;
        }

        customTypesTable[recordName] = typeInfo;
        ImportedRecordTable[recordName] = typeInfo;
        auto recordSym = std::make_shared<SymbolInfo>();
        recordSym->isExportable = true;
        recordSym->members = symMembers;
        recordSym->type = ResolvedType{DataType::RECORD, recordName};

        symbolTable[0][recordName] = recordSym;
    }
}

void Semantics::importComponentInits()
{
    for (const auto &initPair : deserializer.importedInitTable)
    {
        const std::string componentName = initPair.first;

        auto initSym = std::make_shared<SymbolInfo>();
        auto initInfo = initPair.second;
        // Begin the conversions
        for (const auto &type : initInfo.initArgs)
        {
            auto resolvedType = convertImportedTypetoResolvedType(type);
            initSym->initArgs.push_back(resolvedType);
        }
        initSym->returnType = convertImportedTypetoResolvedType(initInfo.returnType);
        initSym->isDeclaration = true;

        importedInits[componentName] = initSym;
    }
}
