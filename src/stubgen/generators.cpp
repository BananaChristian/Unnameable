#include "ast.hpp"
#include "stubgen.hpp"
#include <string>

//____________SERIALIZER GENERATOR____________________
void StubGen::generateSealStatement(Node *node)
{
    auto sealStmt = dynamic_cast<SealStatement *>(node);
    if (!sealStmt)
    {
        std::cout << "[DEBUG] Node is not a SealStatement.\n";
        reportDevBug("Node is not a seal statement");
        return;
    }

    auto sealName = sealStmt->sealName->expression.TokenLiteral;
    logInternal("Proccesing seal: "+sealName);

    auto sealIt = semantics.sealTable.find(sealName);
    if (sealIt == semantics.sealTable.end())
    {
        reportDevBug("Seal not found in the semantics sealTable");
        return;
    }

    auto sealFnMap = sealIt->second;

    // Print all keys in the seal function map
    logInternal("Functions in sealFnMap: ");
    for (auto &kv : sealFnMap)
        logInternal(kv.first);
    std::cout << "\n";

    SealTable sealTable;
    sealTable.sealName = sealName;

    auto sealBlock = dynamic_cast<BlockStatement *>(sealStmt->block.get());
    if (!sealBlock)
    {
        reportDevBug("Seal block is null");
        return;
    }

    for (const auto &stmt : sealBlock->statements)
    {
        auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
        if (!fnStmt)
        {
            reportDevBug("Statement is not a function statement");
            continue;
        }

        auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
        if (!fnExpr)
        {
            reportDevBug("Function statement does not contain a function expression");
            continue;
        }

        if (!fnExpr->isExportable)
        {
            std::cout << "[DEBUG] Function " << fnExpr->func_key.TokenLiteral << " is not exportable.\n";
            continue;
        }

        auto fnName = fnExpr->func_key.TokenLiteral;
        std::string unmangled = unmangle(fnName);
        logInternal("Looking for function in sealFnMap: "+unmangled);

        auto sealFnIt = sealFnMap.find(unmangled);
        if (sealFnIt == sealFnMap.end())
        {
            logInternal("Function not found in sealFnMap: "+unmangled);
            continue;
        }

        auto fnSym = sealFnIt->second;
        if (!fnSym)
        {
            reportDevBug("SymbolInfo pointer is null for function: "+unmangled);
            continue;
        }

        // create a new sealFn for each function
        SealFunction sealFn;
        sealFn.funcName = unmangled;
        sealFn.returnType = fnSym->returnType;
        sealFn.paramTypes = fnSym->paramTypes;

        // add to the current seal table
        sealTable.sealFns.push_back(sealFn);
        logInternal("Added function: "+fnName+" to sealTable");
    }

    stubTable.seals.push_back(sealTable);
    logInternal("Finished seal:"+sealName+" with "+std::to_string(sealTable.sealFns.size())+" functions");
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
    logInternal("Processing component: "+componentName);

    auto compIt = semantics.metaData.find(compStmt);
    if (compIt == semantics.metaData.end())
    {
        reportDevBug("Failed to find component metaData");
        return;
    }

    auto compSym = compIt->second;
    if (!compSym)
    {
        reportDevBug("No component symbolInfo found");
        return;
    }

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
            reportDevBug("Failed to find init constructor metaData");
            return;
        }

        auto initSym = initIt->second;
        if (!initSym)
        {
            reportDevBug("Failed to retreive init constructor symbol");
            return;
        }

        init.initArgs = initSym->initArgs;
        init.returnType = initSym->returnType;
        init.type = initSym->type;

        componentTable.init = init;
    }

    // Add to the stub table
    stubTable.components.push_back(componentTable);
    logInternal("Finished serializing component '"+componentName+"'");
}

void StubGen::generateRecordStatement(Node *node)
{
    auto recordStmt = dynamic_cast<RecordStatement *>(node);
    if (!recordStmt)
    {
        reportDevBug("Invalid record node");
        return;
    }

    if (!recordStmt->isExportable)
        return;

    std::string recordName = recordStmt->recordName->expression.TokenLiteral;

    logInternal("Processing record '"+recordName+"'");
    auto recordIt = semantics.metaData.find(recordStmt);
    if (recordIt == semantics.metaData.end())
    {
        reportDevBug("Failed to find record metaData");
        return;
    }

    auto recordSym = recordIt->second;
    if (!recordSym)
    {
        reportDevBug("No record symbolInfo");
        return;
    }


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
    logInternal("Finished serializing record '"+recordName+"'");
}

void StubGen::generateEnumStatement(Node *node){
    auto enumStmt=dynamic_cast<EnumStatement*>(node);
    if(!enumStmt){
        reportDevBug("Invalid enum node");
        return;
    }
    
    if(!enumStmt->isExportable)
        return;
    
    auto enumName=enumStmt->enum_identifier->expression.TokenLiteral;
    
    logInternal("Processing enum '"+enumName+"'");
    
    //I will use the customTypesTable as it has more information about the members
    auto enumIt=semantics.customTypesTable.find(enumName);
    if(enumIt==semantics.customTypesTable.end()){
        reportDevBug("Could not find type '"+enumName+"' in semantics custom types table");
        return;
    }
    
    auto enumInfo=enumIt->second;
    if(!enumInfo){
        reportDevBug("Could not find type info for '"+enumName+"'");
        return;
    }
    
    EnumTable enumTable;
    enumTable.enumName=enumName;
    
    auto enumMembers=enumInfo->members;
    for(const auto &[memName,memInfo]:enumMembers){
        EnumMembers member;
        member.memberName=memInfo->memberName;
        member.type=memInfo->type;
        member.constantValue=memInfo->constantValue;
        member.enumType=memInfo->parentType;
        
        enumTable.members.push_back(member);
    }
    
    stubTable.enums.push_back(enumTable);
    logInternal("Finished serializing enum '"+enumName+"'");
}