#include "stubgen.hpp"

//____________SERIALIZER GENERATOR____________________
void StubGen::generateSealStatement(Node *node)
{
    auto sealStmt = dynamic_cast<SealStatement *>(node);
    if (!sealStmt)
    {
        std::cout << "[DEBUG] Node is not a SealStatement.\n";
        return;
    }

    auto sealName = sealStmt->sealName->expression.TokenLiteral;
    std::cout << "[DEBUG] Processing seal: " << sealName << "\n";

    auto sealIt = semantics.sealTable.find(sealName);
    if (sealIt == semantics.sealTable.end())
    {
        std::cout << "[DEBUG] Seal not found in semantics.sealTable: " << sealName << "\n";
        return;
    }

    auto sealFnMap = sealIt->second;

    // Print all keys in the seal function map
    std::cout << "[DEBUG] Functions in sealFnMap: ";
    for (auto &kv : sealFnMap)
        std::cout << kv.first << " ";
    std::cout << "\n";

    SealTable sealTable;
    sealTable.sealName = sealName;

    auto sealBlock = dynamic_cast<BlockStatement *>(sealStmt->block.get());
    if (!sealBlock)
    {
        std::cout << "[DEBUG] Seal block is null.\n";
        return;
    }

    for (const auto &stmt : sealBlock->statements)
    {
        auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
        if (!fnStmt)
        {
            std::cout << "[DEBUG] Statement is not a FunctionStatement.\n";
            continue;
        }

        auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
        if (!fnExpr)
        {
            std::cout << "[DEBUG] FunctionStatement does not contain a FunctionExpression.\n";
            continue;
        }

        if (!fnExpr->isExportable)
        {
            std::cout << "[DEBUG] Function " << fnExpr->func_key.TokenLiteral << " is not exportable.\n";
            continue;
        }

        auto fnName = fnExpr->func_key.TokenLiteral;
        std::string unmangled = unmangle(fnName);
        std::cout << "[DEBUG] Looking for function in sealFnMap: " << unmangled << "\n";

        auto sealFnIt = sealFnMap.find(unmangled);
        if (sealFnIt == sealFnMap.end())
        {
            std::cout << "[DEBUG] Function not found in sealFnMap: " << unmangled << "\n";
            continue;
        }

        auto fnSym = sealFnIt->second;
        if (!fnSym)
        {
            std::cout << "[DEBUG] SymbolInfo pointer is null for function: " << unmangled << "\n";
            continue;
        }

        // create a new sealFn for each function
        SealFunction sealFn;
        sealFn.funcName = unmangled;
        sealFn.returnType = fnSym->returnType;
        sealFn.paramTypes = fnSym->paramTypes;

        // add to the current seal table
        sealTable.sealFns.push_back(sealFn);
        std::cout << "[DEBUG] Added function: " << fnName << " to sealTable.\n";
    }

    stubTable.seals.push_back(sealTable);
    std::cout << "[DEBUG] Finished seal: " << sealName << " with "
              << sealTable.sealFns.size() << " functions.\n";
}

void StubGen::generateComponentStatement(Node *node)
{
    auto compStmt = dynamic_cast<ComponentStatement *>(node);
    if (!compStmt)
        return;

    // If the component is not export do not bother
    if (!compStmt->isExportable)
        return;

    std::string componentName = compStmt->component_name->expression.TokenLiteral;
    std::cout << "[DEBUG] Processing component: " << componentName << "\n";

    auto compIt = semantics.metaData.find(compStmt);
    if (compIt == semantics.metaData.end())
        throw std::runtime_error("Failed to find component metadata");

    auto compSym = compIt->second;
    if (!compSym)
        throw std::runtime_error("No component symbolInfo");

    if (compSym->hasError)
        throw std::runtime_error("Semantic error");

    ComponentTable componentTable;
    componentTable.componentName = componentName;

    auto memMap = compSym->members;
    for (const auto &[memName, memInfo] : memMap)
    {
        if (memInfo->isFunction)
        {
            ComponentMethod method;
            method.methodName = memName;
            method.paramTypes = memInfo->paramTypes;
            method.returnType = memInfo->returnType;
            method.isFunction = memInfo->isFunction;

            componentTable.methods.push_back(method);
        }
        else // It is some sort of var declaration
        {
            ComponentMember member;
            member.memberName = memInfo->memberName;
            member.type = memInfo->type;
            member.memberIndex = memInfo->memberIndex;
            member.isNullable = memInfo->isNullable;
            member.isMutable = memInfo->isMutable;
            member.isConstant = memInfo->isConstant;
            member.isPointer = memInfo->isPointer;
            member.isRef = memInfo->isRef;
            member.storage = memInfo->storage;

            componentTable.members.push_back(member);
        }
    }

    // Add the init if it exists
    if (compStmt->initConstructor.has_value())
    {
        ComponentInit init;
        componentTable.hasInit = true;
        // Since the init constructor isnt actually in the member info I will use a metaData look up
        auto initStmt = dynamic_cast<InitStatement *>(compStmt->initConstructor.value().get());
        auto initIt = semantics.metaData.find(initStmt);
        if (initIt == semantics.metaData.end())
        {
            std::cout << "Failed to find init constructor metaData\n";
            return;
        }

        auto initSym = initIt->second;
        if (!initSym)
        {
            std::cout << "Failed to retreive init constructor symbol\n";
            return;
        }

        init.initArgs = initSym->initArgs;
        init.returnType = initSym->returnType;
        init.type = initSym->type;

        componentTable.init = init;
    }

    // Add to the stub table
    stubTable.components.push_back(componentTable);
    std::cout << "[DEBUG] Finished serializing component '" + componentName << "\n";
}

void StubGen::generateRecordStatement(Node *node)
{
    auto recordStmt = dynamic_cast<RecordStatement *>(node);
    if (!recordStmt)
        return;

    if (!recordStmt->isExportable)
        return;

    std::string recordName = recordStmt->recordName->expression.TokenLiteral;

    std::cout << "Processing record: " << recordName << "\n";

    auto dataIt = semantics.metaData.find(recordStmt);
    if (dataIt == semantics.metaData.end())
        throw std::runtime_error("Failed to find record metaData");

    auto recordSym = dataIt->second;
    if (!recordSym)
        throw std::runtime_error("No record symbolInfo");

    if (recordSym->hasError)
        throw std::runtime_error("Semantic error");

    RecordTable recordTable;
    recordTable.recordName = recordName;

    auto recordMembers = recordSym->members;
    for (const auto &[memName, memInfo] : recordMembers)
    {
        RecordMember member;
        member.memberName = memInfo->memberName;
        member.type = memInfo->type;
        member.memberIndex = memInfo->memberIndex;
        member.isNullable = memInfo->isNullable;
        member.isMutable = memInfo->isMutable;
        member.isConstant = memInfo->isConstant;
        member.isPointer = memInfo->isPointer;
        member.isRef = memInfo->isRef;
        member.storage = memInfo->storage;

        recordTable.members.push_back(member);
    }

    stubTable.records.push_back(recordTable);
    std::cout << "[DEBUG] Finished serializing record '" + recordName << "\n";
}