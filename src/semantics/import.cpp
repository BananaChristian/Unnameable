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
    case ImportedDataType::SHORT_INT:
        return DataType::SHORT_INT;
    case ImportedDataType::USHORT_INT:
        return DataType::USHORT_INT;
    case ImportedDataType::INTEGER:
        return DataType::INTEGER;
    case ImportedDataType::UINTEGER:
        return DataType::UINTEGER;
    case ImportedDataType::LONG_INT:
        return DataType::LONG_INT;
    case ImportedDataType::ULONG_INT:
        return DataType::ULONG_INT;
    case ImportedDataType::EXTRA_INT:
        return DataType::EXTRA_INT;
    case ImportedDataType::UEXTRA_INT:
        return DataType::UEXTRA_INT;
    case ImportedDataType::BOOLEAN:
        return DataType::BOOLEAN;
    case ImportedDataType::STRING:
        return DataType::STRING;
    case ImportedDataType::FLOAT:
        return DataType::FLOAT;
    case ImportedDataType::DOUBLE:
        return DataType::DOUBLE;
    case ImportedDataType::CHAR:
        return DataType::CHAR;
    case ImportedDataType::CHAR16:
        return DataType::CHAR16;
    case ImportedDataType::CHAR32:
        return DataType::CHAR32;
    case ImportedDataType::ENUM:
        return DataType::ENUM;
    case ImportedDataType::DATABLOCK:
        return DataType::DATABLOCK;
    case ImportedDataType::BEHAVIORBLOCK:
        return DataType::BEHAVIORBLOCK;
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

void Semantics::walkImportStatement(Node *node)
{
    auto import = dynamic_cast<ImportStatement *>(node);
    if (!import)
        return;

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

        symbolTable[0][sealName] = sealSym;
    }
}
