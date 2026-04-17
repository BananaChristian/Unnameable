#include "semantics.hpp"

/// Build a MemberInfo from a ComponentMember.
static std::shared_ptr<MemberInfo> memberInfoFromComponentMember(const ComponentMember &m) {
    auto info = std::make_shared<MemberInfo>();
    info->memberName = m.memberName;
    info->type = m.type;
    info->memberIndex = m.memberIndex;
    info->isNullable = m.isNullable;
    info->isMutable = m.isMutable;
    info->isConstant = m.isConstant;
    info->isPointer = m.isPointer;
    info->isRef = m.isRef;
    info->isExportable = true;
    info->isDeclared = true;
    return info;
}

/// Build a MemberInfo from a RecordMember.
static std::shared_ptr<MemberInfo> memberInfoFromRecordMember(const RecordMember &m) {
    auto info = std::make_shared<MemberInfo>();
    info->memberName = m.memberName;
    info->type = m.type;
    info->memberIndex = m.memberIndex;
    info->isNullable = m.isNullable;
    info->isMutable = m.isMutable;
    info->isConstant = m.isConstant;
    info->isPointer = m.isPointer;
    info->isRef = m.isRef;
    info->isExportable = true;
    info->isDeclared = true;
    return info;
}

/// Build a MemberInfo from a ComponentMethod.
static std::shared_ptr<MemberInfo> memberInfoFromComponentMethod(const ComponentMethod &m) {
    auto info = std::make_shared<MemberInfo>();
    info->memberName = m.methodName;
    info->returnType = m.returnType;
    info->paramTypes = m.paramTypes;
    info->isFunction = true;
    info->isDeclared = true;
    info->isExportable = true;
    return info;
}

/// Build a SymbolInfo for a standalone or generic imported function.
static std::shared_ptr<SymbolInfo> symInfoFromFunctionEntry(const FunctionEntry &fn) {
    auto sym = std::make_shared<SymbolInfo>();
    sym->isFunction = true;
    sym->isExportable = true;
    sym->func().isDeclaration = true;
    sym->func().isDefined = true;
    sym->func().funcName = fn.funcName;
    sym->func().returnType = fn.returnType;
    sym->func().paramTypes = fn.paramTypes;
    sym->type().type = fn.returnType;
    return sym;
}

static std::shared_ptr<SymbolInfo> symInfoFromVariableEntry(const VariableEntry &var){
    auto sym=std::make_shared<SymbolInfo>();
    sym->type().type=var.declaredType;
    sym->storage().isMutable=var.isMutable;
    sym->storage().isInitialized=var.isInitialized;

    return sym;
}

/// Build a SymbolInfo for an imported init constructor.
static std::shared_ptr<SymbolInfo> symInfoFromComponentInit(const ComponentInit &init) {
    auto sym = std::make_shared<SymbolInfo>();
    sym->func().isDeclaration = true;
    sym->func().isDefined = true;
    sym->func().returnType = init.returnType;
    sym->type().type = init.type;
    for (const auto &arg : init.initArgs) sym->func().initArgs.push_back(arg);
    return sym;
}


static void registerTypeSymbol(
    const std::string &name, DataType kind,
    const std::unordered_map<std::string, std::shared_ptr<MemberInfo>> &members,
    std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>> &customTypesTable,
    std::unordered_map<std::string, std::shared_ptr<CustomTypeInfo>> &phantomTable,
    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> &globalScope) {
    auto typeInfo = std::make_shared<CustomTypeInfo>();
    typeInfo->typeName = name;
    typeInfo->type = ResolvedType::makeBase(kind, name);
    typeInfo->members = members;
    typeInfo->isExportable = true;

    customTypesTable[name] = typeInfo;
    phantomTable[name] = typeInfo;

    auto sym = std::make_shared<SymbolInfo>();
    sym->isExportable = true;
    sym->members = members;
    sym->type().type = ResolvedType::makeBase(kind, name);
    globalScope[name] = sym;

}

//Special to register imported functions from standalone and from generics
static void registerFunctionSymbol(const FunctionEntry &funcEntry,std::unordered_map<std::string,std::shared_ptr<SymbolInfo>> &importTable,std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> &globalScope){
   auto funcInfo=std::make_shared<SymbolInfo>();
   funcInfo=symInfoFromFunctionEntry(funcEntry);
   //Register into the imported functions table
   importTable[funcEntry.funcName]=funcInfo;
   //Register into global scope
   globalScope[funcEntry.funcName]=funcInfo;
}

static void registerVariableSymbol(const VariableEntry &varEntry, std::unordered_map<std::string,std::shared_ptr<SymbolInfo>> &importTable,std::unordered_map<std::string,std::shared_ptr<SymbolInfo>>&globalScope){
    auto varInfo=std::make_shared<SymbolInfo>();
    varInfo=symInfoFromVariableEntry(varEntry);
    //Register into imported vars table
    importTable[varEntry.var_name]=varInfo;
    //Register into global scope
    globalScope[varEntry.var_name]=varInfo;
}

void Semantics::importSeals() {
    logInternal("Importing seals: " + std::to_string(deserializer.stub.seals.size()));

    for (const auto &seal : deserializer.stub.seals) {
        logInternal("Importing seal '" + seal.sealName + "'");

        std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> sealMap;

        for (const auto &fn : seal.sealFns) {
            logInternal("  function '" + fn.funcName + "'");
            // Reuse FunctionEntry adapter — SealFunction has the same shape
            FunctionEntry fe;
            fe.funcName = fn.funcName;
            fe.returnType = fn.returnType;
            fe.paramTypes = fn.paramTypes;
            sealMap[fn.funcName] = symInfoFromFunctionEntry(fe);
        }

        sealTable[seal.sealName] = std::move(sealMap);

        auto sealSym = std::make_shared<SymbolInfo>();
        sealSym->isExportable = true;
        symbolTable[0][seal.sealName] = sealSym;

        logInternal("Finished importing seal '" + seal.sealName + "'");
    }
}

void Semantics::importComponents() {
    logInternal("Importing components: " + std::to_string(deserializer.stub.components.size()));

    for (const auto &comp : deserializer.stub.components) {
        logInternal("Importing component '" + comp.componentName + "'");

        std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;

        for (const auto &m : comp.members) {
            logInternal("  member '" + m.memberName + "'");
            members[m.memberName] = memberInfoFromComponentMember(m);
        }
        for (const auto &m : comp.methods) {
            logInternal("  method '" + m.methodName + "'");
            members[m.methodName] = memberInfoFromComponentMethod(m);
        }

        // Register into customTypesTable, ImportedComponentTable and symbolTable[0]
        registerTypeSymbol(comp.componentName, DataType::COMPONENT, members, customTypesTable,
                           ImportedComponentTable, symbolTable[0]);

        if (comp.hasInit) {
            importedInits[comp.componentName] = symInfoFromComponentInit(comp.init);
            logInternal("  init registered for '" + comp.componentName + "'");
        }

        logInternal("Finished importing component '" + comp.componentName + "'");
    }
}

void Semantics::importRecords() {
    logInternal("Importing records: " + std::to_string(deserializer.stub.records.size()));

    for (const auto &record : deserializer.stub.records) {
        logInternal("Importing record '" + record.recordName + "'");

        std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;

        for (const auto &m : record.members) {
            logInternal("  member '" + m.memberName + "'");
            members[m.memberName] = memberInfoFromRecordMember(m);
        }

        // Register into customTypesTable, ImportedRecordTable and symbolTable[0]
        registerTypeSymbol(record.recordName, DataType::RECORD, members, customTypesTable,
                           ImportedRecordTable, symbolTable[0]);

        logInternal("Finished importing record '" + record.recordName + "'");
    }
}

void Semantics::importEnums() {
    logInternal("Importing enums: " + std::to_string(deserializer.stub.enums.size()));

    for (const auto &en : deserializer.stub.enums) {
        logInternal("Importing enum '" + en.enumName + "'");

        auto typeInfo = std::make_shared<CustomTypeInfo>();
        typeInfo->typeName = en.enumName;
        typeInfo->type = ResolvedType::makeBase(DataType::ENUM, en.enumName);
        typeInfo->underLyingType = en.underlyingType.kind;
        typeInfo->isExportable = true;

        std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;

        for (const auto &m : en.members) {
            logInternal("  member '" + m.memberName + "'");

            auto memInfo = std::make_shared<MemberInfo>();
            memInfo->memberName = m.memberName;
            memInfo->type = m.type;
            memInfo->constantValue = m.constantValue;
            memInfo->parentType = m.enumType;
            memInfo->isConstant = true;
            memInfo->isInitialised = true;
            memInfo->isExportable = true;

            typeInfo->members[m.memberName] = memInfo;
            members[m.memberName] = memInfo;
        }

        customTypesTable[en.enumName] = typeInfo;

        auto sym = std::make_shared<SymbolInfo>();
        sym->isExportable = true;
        sym->members = members;
        sym->type().type = ResolvedType::makeBase(DataType::ENUM, en.enumName);
        symbolTable[0][en.enumName] = sym;

        logInternal("Finished importing enum '" + en.enumName + "'");
    }
}

void Semantics::importAllocators() {
    logInternal("Importing allocators: " + std::to_string(deserializer.stub.allocators.size()));

    for (const auto &alloc : deserializer.stub.allocators) {
        logInternal("Importing allocator '" + alloc.allocatorName + "'");

        AllocatorHandle handle;

        handle.allocateName = alloc.allocator.functionName;
        auto allocSym = std::make_shared<SymbolInfo>();
        allocSym->func().returnType = alloc.allocator.returnType;
        allocSym->func().paramTypes = alloc.allocator.paramTypes;
        allocSym->type().type = alloc.allocator.returnType;
        handle.allocatorSymbol = allocSym;
        logInternal("  allocate '" + handle.allocateName + "'");

        handle.freeName = alloc.free.functionName;
        auto freeSym = std::make_shared<SymbolInfo>();
        freeSym->func().returnType = alloc.free.returnType;
        freeSym->func().paramTypes = alloc.free.paramTypes;
        freeSym->type().type = alloc.free.returnType;
        handle.freeSymbol = freeSym;
        logInternal("  free '" + handle.freeName + "'");

        allocatorMap[alloc.allocatorName] = handle;

        auto interfaceSym = std::make_shared<SymbolInfo>();
        interfaceSym->isExportable = true;
        symbolTable[0][alloc.allocatorName] = interfaceSym;

        logInternal("Finished importing allocator '" + alloc.allocatorName + "'");
    }
}

void Semantics::importFunctions() {
    logInternal("Importing standalone functions: " +
                std::to_string(deserializer.stub.functions.size()));

    for (const auto &fn : deserializer.stub.functions) {
        logInternal("Importing function '" + fn.funcName + "'");
        registerFunctionSymbol(fn,ImportedFunctionsTable,symbolTable[0]);
        logInternal("Finished importing function '" + fn.funcName + "'");
    }
}

void Semantics::importVariables(){
    logInternal("Importing standalone variables: "+std::to_string(deserializer.stub.variables.size()));
    for(const auto &var:deserializer.stub.variables){
        logInternal("Importing variable '"+var.var_name+"'");
        registerVariableSymbol(var,ImportedVariablesTable,symbolTable[0]);
        logInternal("Finished importing variable '"+var.var_name+"'");
    }
}


void Semantics::importGenerics() {
    logInternal("Importing generic instantiations: " +
                std::to_string(deserializer.stub.generics.size()));

    for (const auto &gen : deserializer.stub.generics) {
        logInternal("Importing instantiation '" + gen.aliasName + "'");

        for (const auto &comp : gen.components) {
            logInternal("  generic component '" + comp.componentName + "'");

            std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
            for (const auto &m : comp.members)
                members[m.memberName] = memberInfoFromComponentMember(m);
            for (const auto &m : comp.methods)
                members[m.methodName] = memberInfoFromComponentMethod(m);

            registerTypeSymbol(comp.componentName, DataType::COMPONENT, members, customTypesTable,
                               ImportedComponentTable, symbolTable[0]);

            if (comp.hasInit) {
                importedInits[comp.componentName] = symInfoFromComponentInit(comp.init);
                logInternal("  init registered for '" + comp.componentName + "'");
            }
        }

        for (const auto &record : gen.records) {
            logInternal("  generic record '" + record.recordName + "'");

            std::unordered_map<std::string, std::shared_ptr<MemberInfo>> members;
            for (const auto &m : record.members)
                members[m.memberName] = memberInfoFromRecordMember(m);

            registerTypeSymbol(record.recordName, DataType::RECORD, members, customTypesTable,
                               ImportedRecordTable, symbolTable[0]);
        }

        for (const auto &fn : gen.functions) {
            logInternal("  generic function '" + fn.funcName + "'");
            registerFunctionSymbol(fn,ImportedFunctionsTable,symbolTable[0]);
        }

        logInternal("Finished importing instantiation '" + gen.aliasName + "'");
    }
}


void Semantics::import() {
    importSeals();
    importComponents();
    importRecords();
    importEnums();
    importAllocators();
    importFunctions();
    importVariables();
    importGenerics();
}
