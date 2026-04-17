#include <string>

#include "ast.hpp"
#include "stubgen.hpp"

//____________SERIALIZER GENERATOR____________________
void StubGen::generateSealStatement(Node *node) {
    auto sealStmt = dynamic_cast<SealStatement *>(node);
    if (!sealStmt) {
        reportDevBug("Node is not a seal statement");
    }

    auto sealName = sealStmt->sealName->expression.TokenLiteral;
    logInternal("Proccesing seal: " + sealName);
    auto sealIt = semantics.sealTable.find(sealName);

    if (sealIt == semantics.sealTable.end()) {
        reportDevBug("Seal not found in the semantics sealTable");
    }

    auto sealFnMap = sealIt->second;  // Print all keys in the seal function map
                                      // logInternal("Functions in sealFnMap: ");
    for (auto &kv : sealFnMap) logInternal(kv.first);
    std::cout << "\n";

    SealTable sealTable;
    sealTable.sealName = sealName;

    auto sealBlock = dynamic_cast<BlockStatement *>(sealStmt->block.get());
    if (!sealBlock) {
        reportDevBug("Seal block is null");
        return;
    }

    for (const auto &stmt : sealBlock->statements) {
        auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get());
        if (!fnStmt) {
            reportDevBug("Statement is not a function statement");
        }

        auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
        if (!fnExpr) {
            reportDevBug("Function statement does not contain a function expression");
        }

        if (!fnExpr->isExportable) {
            std::cout << "[DEBUG] Function " << fnExpr->func_key.TokenLiteral
                      << " is not exportable.\n";
            continue;
        }

        auto fnName = fnExpr->func_key.TokenLiteral;
        std::string unmangled = unmangle(fnName);
        logInternal("Looking for function in sealFnMap: " + unmangled);

        auto sealFnIt = sealFnMap.find(unmangled);
        if (sealFnIt == sealFnMap.end()) {
            logInternal("Function not found in sealFnMap: " + unmangled);
            continue;
        }

        auto fnSym = sealFnIt->second;
        if (!fnSym) {
            reportDevBug("SymbolInfo pointer is null for function: " + unmangled);
        }

        // create a new sealFn for each function
        SealFunction sealFn;
        sealFn.funcName = unmangled;
        sealFn.returnType = fnSym->func().returnType;
        sealFn.paramTypes = fnSym->func().paramTypes;

        // add to the current seal table
        sealTable.sealFns.push_back(sealFn);
        logInternal("Added function: " + fnName + " to sealTable");
    }

    stubTable.seals.push_back(sealTable);
    logInternal("Finished seal:" + sealName + " with " + std::to_string(sealTable.sealFns.size()) +
                " functions");
}

void StubGen::generateComponentStatement(Node *node) {
    auto compStmt = dynamic_cast<ComponentStatement *>(node);
    if (!compStmt) return;

    // If the component is not export do not bother
    if (!compStmt->isExportable) return;

    std::string componentName = compStmt->component_name->expression.TokenLiteral;
    logInternal("Processing component: " + componentName);

    auto compSym = semantics.getSymbolFromMeta(compStmt);
    if (!compSym) {
        reportDevBug("No component symbolInfo found");
    }

    ComponentTable componentTable;
    componentTable.componentName = componentName;

    auto memMap = compSym->members;
    for (const auto &[memName, memInfo] : memMap) {
        if (memInfo->isFunction) {
            ComponentMethod method;
            method.methodName = memName;
            method.paramTypes = memInfo->paramTypes;
            method.returnType = memInfo->returnType;
            method.isFunction = memInfo->isFunction;

            componentTable.methods.push_back(method);
        } else  // It is some sort of var declaration
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

            componentTable.members.push_back(member);
        }
    }

    // Add the init if it exists
    if (compStmt->initConstructor.has_value()) {
        ComponentInit init;
        componentTable.hasInit = true;
        // Since the init constructor isnt actually in the member info I will use a
        // metaData look up
        auto initStmt = dynamic_cast<InitStatement *>(compStmt->initConstructor.value().get());

        auto initSym = semantics.getSymbolFromMeta(initStmt);
        if (!initSym) {
            reportDevBug("Failed to retreive init constructor symbol");
        }

        init.initArgs = initSym->func().initArgs;
        init.returnType = initSym->func().returnType;
        init.type = initSym->type().type;

        componentTable.init = init;
    }

    // Add to the stub table
    stubTable.components.push_back(componentTable);
    logInternal("Finished serializing component '" + componentName + "'");
}

void StubGen::generateRecordStatement(Node *node) {
    auto recordStmt = dynamic_cast<RecordStatement *>(node);
    if (!recordStmt) {
        reportDevBug("Invalid record node");
    }

    if (!recordStmt->isExportable) return;

    std::string recordName = recordStmt->recordName->expression.TokenLiteral;

    logInternal("Processing record '" + recordName + "'");

    auto recordSym = semantics.getSymbolFromMeta(recordStmt);
    if (!recordSym) {
        reportDevBug("No record symbolInfo");
    }

    RecordTable recordTable;
    recordTable.recordName = recordName;

    auto recordMembers = recordSym->members;
    for (const auto &[memName, memInfo] : recordMembers) {
        RecordMember member;
        member.memberName = memInfo->memberName;
        member.type = memInfo->type;
        member.memberIndex = memInfo->memberIndex;
        member.isNullable = memInfo->isNullable;
        member.isMutable = memInfo->isMutable;
        member.isConstant = memInfo->isConstant;
        member.isPointer = memInfo->isPointer;
        member.isRef = memInfo->isRef;

        recordTable.members.push_back(member);
    }

    stubTable.records.push_back(recordTable);
    logInternal("Finished serializing record '" + recordName + "'");
}

void StubGen::generateEnumStatement(Node *node) {
    auto enumStmt = dynamic_cast<EnumStatement *>(node);
    if (!enumStmt) {
        reportDevBug("Invalid enum node");
    }

    if (!enumStmt->isExportable) return;

    auto enumName = enumStmt->enum_identifier->expression.TokenLiteral;

    logInternal("Processing enum '" + enumName + "'");

    // I will use the customTypesTable as it has more information about the
    // members
    auto enumIt = semantics.customTypesTable.find(enumName);
    if (enumIt == semantics.customTypesTable.end()) {
        reportDevBug("Could not find type '" + enumName + "' in semantics custom types table");
    }

    auto enumInfo = enumIt->second;
    if (!enumInfo) {
        reportDevBug("Could not find type info for '" + enumName + "'");
    }

    EnumTable enumTable;
    enumTable.enumName = enumName;

    auto enumMembers = enumInfo->members;
    for (const auto &[memName, memInfo] : enumMembers) {
        EnumMembers member;
        member.memberName = memInfo->memberName;
        member.type = memInfo->type;
        member.constantValue = memInfo->constantValue;
        member.enumType = memInfo->parentType;

        enumTable.members.push_back(member);
    }

    stubTable.enums.push_back(enumTable);
    logInternal("Finished serializing enum '" + enumName + "'");
}

void StubGen::generateAllocatorStatement(Node *node) {
    auto allocatorStmt = dynamic_cast<AllocatorStatement *>(node);
    if (!allocatorStmt) {
        reportDevBug("Invalid allocator interface");
    }

    if (!allocatorStmt->isExportable) return;

    std::string allocatorName = allocatorStmt->allocator_name->expression.TokenLiteral;

    logInternal("Processing allocator interface '" + allocatorName + "'");
    auto allocatorIt = semantics.allocatorMap.find(allocatorName);
    if (allocatorIt == semantics.allocatorMap.end()) {
        reportDevBug("Failed to find allocator '" + allocatorName + "' in allocator map");
    }

    Allocator allocator;
    allocator.allocatorName = allocatorName;

    // Get the corresponding allocator handle
    auto allocatorHandle = allocatorIt->second;
    auto allocSym = allocatorHandle.allocatorSymbol;

    // Dealing with the allocation function
    AllocatorFunction alloc;
    alloc.functionName = allocatorHandle.allocateName;
    logInternal("Stored allocator function name: " + alloc.functionName);
    alloc.paramTypes = allocSym->func().paramTypes;
    for (const auto &paramInfo : alloc.paramTypes) {
        const std::string &paramTypeName = paramInfo.first.resolvedName;

        logInternal("Storedallocator '" + alloc.functionName + "' with param of type '" +
                    paramTypeName + "'");
    }

    alloc.returnType = allocSym->func().returnType;
    logInternal("Stored allocator function return type: " + alloc.returnType.resolvedName);

    auto freeSym = allocatorHandle.freeSymbol;
    // Dealing with the free function
    AllocatorFunction free;
    free.functionName = allocatorHandle.freeName;
    logInternal("Stored deallocation function name: " + free.functionName);
    free.paramTypes = freeSym->func().paramTypes;
    for (const auto &paramInfo : free.paramTypes) {
        const std::string &paramTypeName = paramInfo.first.resolvedName;
        logInternal("Stored deallocation function '" + free.functionName +
                    "' with parameter of type '" + paramTypeName + "'");
    }
    free.returnType = freeSym->func().returnType;
    logInternal("Stored deallocator function return type: " + alloc.returnType.resolvedName);

    allocator.allocator = alloc;
    allocator.free = free;

    stubTable.allocators.push_back(allocator);
    logInternal("Finished serializing allocator '" + allocatorName + "'");
}

void StubGen::generateInstantiateStatement(Node *node) {
    auto instStmt = dynamic_cast<InstantiateStatement *>(node);
    if (!instStmt) {
        reportDevBug("Invalid instantiation statement");
    }

    if (!instStmt->isExportable) return;

    auto sym = semantics.getSymbolFromMeta(instStmt);
    if (!sym) reportDevBug("Failed to get the instantiation symbol info");

    if(!sym->isExportable)
        return;

    // The instTable must exist — if it doesn't the semantic pass failed
    if (!sym->generic().instTable.has_value()) {
        reportDevBug("Instantiation symbol has no instTable");
    }

    auto &instTable = sym->generic().instTable.value();
    std::string aliasName = instTable.aliasName;

    logInternal("Processing instantiation '" + aliasName + "'");

    Generics generics;
    generics.aliasName = aliasName;

    // Walk the instantiated AST block and sort into records, components, funcs
    auto blockStmt = dynamic_cast<BlockStatement *>(instTable.instantiatedAST.get());
    if (!blockStmt) {
        reportDevBug("Instantiated AST is not a block statement");
    }

    for (const auto &stmt : blockStmt->statements) {
        // ── Record ──────────────────────────────────────────────────────────────
        if (auto recordStmt = dynamic_cast<RecordStatement *>(stmt.get())) {
            std::string mangledName = recordStmt->recordName->expression.TokenLiteral;
            logInternal("Collecting generic record '" + mangledName + "'");

            auto recordSym = semantics.getSymbolFromMeta(recordStmt);
            if (!recordSym) {
                reportDevBug("No symbolInfo for generic record '" + mangledName + "'");
            }

            RecordTable record;
            record.recordName = mangledName;

            for (const auto &[memName, memInfo] : recordSym->members) {
                RecordMember member;
                member.memberName = memInfo->memberName;
                member.type = memInfo->type;
                member.memberIndex = memInfo->memberIndex;
                member.isNullable = memInfo->isNullable;
                member.isMutable = memInfo->isMutable;
                member.isConstant = memInfo->isConstant;
                member.isPointer = memInfo->isPointer;
                member.isRef = memInfo->isRef;
                record.members.push_back(member);
            }

            generics.records.push_back(record);
            continue;
        }

        // ── Component ────────────────────────────────────────────────────────────
        if (auto compStmt = dynamic_cast<ComponentStatement *>(stmt.get())) {
            std::string mangledName = compStmt->component_name->expression.TokenLiteral;
            logInternal("Collecting generic component '" + mangledName + "'");

            auto compSym = semantics.getSymbolFromMeta(compStmt);
            if (!compSym) {
                reportDevBug("No symbolInfo for generic component '" + mangledName + "'");
                continue;
            }

            ComponentTable component;
            component.componentName = mangledName;

            for (const auto &[memName, memInfo] : compSym->members) {
                if (memInfo->isFunction) {
                    ComponentMethod method;
                    method.methodName = memInfo->memberName;
                    method.returnType = memInfo->returnType;
                    method.paramTypes = memInfo->paramTypes;
                    method.isFunction = true;
                    component.methods.push_back(method);
                } else {
                    ComponentMember member;
                    member.memberName = memInfo->memberName;
                    member.type = memInfo->type;
                    member.memberIndex = memInfo->memberIndex;
                    member.isNullable = memInfo->isNullable;
                    member.isMutable = memInfo->isMutable;
                    member.isConstant = memInfo->isConstant;
                    member.isPointer = memInfo->isPointer;
                    member.isRef = memInfo->isRef;
                    component.members.push_back(member);
                }
            }

            // Init constructor
            if (compStmt->initConstructor.has_value()) {
                auto initStmt =
                    dynamic_cast<InitStatement *>(compStmt->initConstructor.value().get());
                auto initSym = semantics.getSymbolFromMeta(initStmt);
                if (!initSym) {
                    reportDevBug("Failed to retrieve init constructor symbol for '" + mangledName +
                                 "'");
                } else {
                    ComponentInit init;
                    init.initArgs = initSym->func().initArgs;
                    init.returnType = initSym->func().returnType;
                    init.type = initSym->type().type;
                    component.hasInit = true;
                    component.init = init;
                }
            }

            generics.components.push_back(component);
            continue;
        }

        // ── Function ─────────────────────────────────────────────────────────────
        if (auto fnStmt = dynamic_cast<FunctionStatement *>(stmt.get())) {
            auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
            if (!fnExpr) continue;

            std::string mangledName = fnExpr->func_key.TokenLiteral;
            logInternal("Collecting generic function '" + mangledName + "'");

            auto fnSym = semantics.getSymbolFromMeta(fnExpr);
            if (!fnSym) {
                reportDevBug("No symbolInfo for generic function '" + mangledName + "'");
            }

            FunctionEntry fn;
            fn.funcName = mangledName;
            fn.returnType = fnSym->func().returnType;
            fn.paramTypes = fnSym->func().paramTypes;
            fn.isDeclaration = false;

            generics.functions.push_back(fn);
            continue;
        }

        // Anything else inside an instantiated block is a bug
        reportDevBug("Unexpected statement type inside instantiated AST");
    }

    stubTable.generics.push_back(generics);
    logInternal("Finished serializing instantiation '" + aliasName + "' — " +
                std::to_string(generics.records.size()) + " record(s), " +
                std::to_string(generics.components.size()) + " component(s), " +
                std::to_string(generics.functions.size()) + " function(s)");
}

void StubGen::generateFunctionStatement(Node *node) {
    auto fnStmt = dynamic_cast<FunctionStatement *>(node);
    if (!fnStmt) reportDevBug("Invalid function statement");

    auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get());
    auto fnDeclrExpr = dynamic_cast<FunctionDeclarationExpression *>(fnStmt->funcExpr.get());
    if (fnExpr) {
        generateFunctionExpression(fnExpr);
    } else if (fnDeclrExpr) {
        generateFunctionDeclaration(fnDeclrExpr);
    }
}

void StubGen::generateFunctionExpression(FunctionExpression *fnExpr) {
    if(!fnExpr->isExportable)
        return;

    auto fnSym = semantics.getSymbolFromMeta(fnExpr);
    if (!fnSym) reportDevBug("Failed to get symbol info for function");

    FunctionEntry fn;
    fn.funcName = fnExpr->func_key.TokenLiteral;
    fn.returnType = fnSym->func().returnType;
    fn.paramTypes = fnSym->func().paramTypes;
    fn.isDeclaration = false;

    stubTable.functions.push_back(fn);
}

void StubGen::generateFunctionDeclaration(FunctionDeclarationExpression *fnDeclrExpr) {
    auto fnDeclr = dynamic_cast<FunctionDeclaration *>(fnDeclrExpr->funcDeclrStmt.get());
    if (!fnDeclr) reportDevBug("Fialed to get function declaration");

    if(!fnDeclr->isExportable)
        return;

    auto fnDeclSym = semantics.getSymbolFromMeta(fnDeclr);
    if (!fnDeclSym) reportDevBug("Failed to get function declaration symbol info");

    FunctionEntry fn;
    fn.funcName = semantics.extractIdentifierName(fnDeclr->function_name.get());
    fn.returnType = fnDeclSym->func().returnType;
    fn.paramTypes = fnDeclSym->func().paramTypes;
    fn.isDeclaration = true;

    stubTable.functions.push_back(fn);
}

void StubGen::generateVariableDeclaration(Node *node){
    auto declaration=dynamic_cast<VariableDeclaration*>(node);
    if(!declaration)
        return;
    
    if(!declaration->isExportable)
        return;

    auto sym=semantics.getSymbolFromMeta(declaration);
    if(!sym)
        reportDevBug("Failed to get variable symbol info");

    auto declName=semantics.extractDeclarationName(declaration);
    VariableEntry entry;
    entry.var_name=declName;
    entry.declaredType=sym->type().type;
    entry.isMutable=sym->storage().isMutable;
    entry.isConstant=sym->storage().isConstant;
    entry.isInitialized=sym->storage().isInitialized;

    stubTable.variables.push_back(entry);
}
