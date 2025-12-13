#include "stubgen.hpp"
#include "typeindex"

StubGen::StubGen(Semantics &semantics, std::string &file) : semantics(semantics), fileName(file)
{
    registerStubGeneratorFns(); // Register the generators
}

void StubGen::registerStubGeneratorFns()
{
    stubGenFnsMap[typeid(FunctionStatement)] = &StubGen::generateFunctionStatementStub;
    stubGenFnsMap[typeid(FunctionExpression)] = &StubGen::generateFunctionExpressionStub;
    stubGenFnsMap[typeid(FunctionDeclaration)] = &StubGen::generateFunctionDeclarationStub;
    stubGenFnsMap[typeid(SealStatement)] = &StubGen::generateSealStatement;
}

void StubGen::finish()
{
    std::string stubFile = fileName;
    auto pos = stubFile.rfind('.');
    if (pos != std::string::npos)
        stubFile = stubFile.substr(0, pos);

    stubFile += ".stub";

    std::cout << "[DEBUG] Total seals in stubTable: " << stubTable.seals.size() << "\n";
    for (auto &seal : stubTable.seals)
        std::cout << "[DEBUG] Seal " << seal.sealName
                  << " has " << seal.sealFns.size() << " functions.\n";

    serializeFullStubTable(stubTable, stubFile);

    std::cout << "Stub file generated at: " << stubFile << "\n";
}

void StubGen::stubGenerator(Node *node)
{
    auto stubIt = stubGenFnsMap.find(typeid(*node));
    if (stubIt == stubGenFnsMap.end())
    {
        return;
    }

    (this->*stubIt->second)(node);
}

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

        // create a NEW sealFn for each function
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

// Function stub gen
void StubGen::generateFunctionStatementStub(Node *node)
{
    auto fnStmt = dynamic_cast<FunctionStatement *>(node);
    if (!fnStmt)
        return;

    if (auto fnExpr = dynamic_cast<FunctionExpression *>(fnStmt->funcExpr.get()))
    {
        stubGenerator(fnExpr);
    }
    else if (auto fnDeclExpr = dynamic_cast<FunctionDeclarationExpression *>(fnStmt->funcExpr.get()))
    {
        if (auto fnDeclrStmt = dynamic_cast<FunctionDeclaration *>(fnDeclExpr->funcDeclrStmt.get()))
        {
            stubGenerator(fnDeclrStmt);
        }
    }
}

void StubGen::generateFunctionExpressionStub(Node *node)
{
    auto fnExpr = dynamic_cast<FunctionExpression *>(node);
    if (!fnExpr)
        return;

    if (!fnExpr->isExportable)
        return;

    const std::string &funcName = fnExpr->func_key.TokenLiteral;

    // Retrieve semantic info
    auto it = semantics.metaData.find(fnExpr);
    if (it == semantics.metaData.end())
        return;

    auto sym = it->second;
    if (!sym)
        return;

    if (sym->hasError)
        return;

    SealFunction sealFn;
    sealFn.funcName = funcName;
    sealFn.returnType = sym->returnType;
    sealFn.paramTypes = sym->paramTypes;

    // stubTable[funcName] = stub;
}

void StubGen::generateFunctionDeclarationStub(Node *node)
{
    auto fnDeclr = dynamic_cast<FunctionDeclaration *>(node);
    if (!fnDeclr)
        return;

    if (!fnDeclr->isExportable)
        return;

    const std::string &funcName = fnDeclr->function_name->expression.TokenLiteral;

    auto it = semantics.metaData.find(fnDeclr);
    if (it == semantics.metaData.end())
        return;

    auto sym = it->second;
    if (!sym)
        return;

    if (sym->hasError)
        return;

    /*
    Stub stub;
    stub.name = funcName;
    stub.returnType = sym->returnType;
    stub.paramTypes = sym->paramTypes;
    stub.type = exportType::DECLARATION;

    stubTable[funcName] = stub;
    */
}

void StubGen::writeString(std::ostream &out, const std::string &str)
{
    int32_t len = str.size();
    out.write(reinterpret_cast<const char *>(&len), sizeof(len));
    out.write(str.data(), len);
}

void StubGen::write_u8(std::ostream &out, uint8_t v)
{
    out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::write_u32(std::ostream &out, uint32_t v)
{
    out.write(reinterpret_cast<const char *>(&v), sizeof(v));
}

void StubGen::serializeResolvedType(std::ostream &out, const ResolvedType &t)
{
    // DataType
    write_u32(out, static_cast<uint32_t>(t.kind));

    // resolvedName
    writeString(out, t.resolvedName);

    // Flags
    write_u8(out, t.isPointer ? 1 : 0);
    write_u8(out, t.isRef ? 1 : 0);
    write_u8(out, t.isNull ? 1 : 0);
    write_u8(out, t.isArray ? 1 : 0);

    // Inner type prescence
    uint8_t hasInner = (t.innerType != nullptr) ? 1 : 0;
    write_u8(out, hasInner);

    if (hasInner)
    {
        serializeResolvedType(out, *t.innerType);
    }
}

void StubGen::serializeParamTypes(std::ostream &out, const std::vector<std::pair<ResolvedType, std::string>> &params)
{
    write_u32(out, static_cast<uint32_t>(params.size()));

    for (const auto &p : params)
    {
        serializeResolvedType(out, p.first); // the type
        writeString(out, p.second);          // param name
    }
}

void StubGen::serializeSealFunction(std::ostream &out, const SealFunction &fn)
{
    writeString(out, fn.funcName);
    serializeResolvedType(out, fn.returnType);
    serializeParamTypes(out, fn.paramTypes);
}

void StubGen::serializeSealTable(std::ostream &out, const SealTable &seal)
{
    writeString(out, seal.sealName);
    write_u32(out, (uint32_t)seal.sealFns.size());
    for (const auto &fn : seal.sealFns)
    {
        serializeSealFunction(out, fn);
    }
}

void StubGen::serializeFullStubTable(const StubTable &table, const std::string &filename)
{
    std::cout << "Serializing stub table\n";
    std::ofstream out(filename, std::ios::binary);
    if (!out)
        throw std::runtime_error("Failed to open stub file");

    // STUB Magic
    write_u32(out, 0x53545542);
    // Version
    write_u32(out, 1);

    // SEALS
    write_u32(out, (uint32_t)table.seals.size());
    for (const auto &seal : table.seals)
    {
        serializeSealTable(out, seal);
    }
}

// HELPERS
std::string StubGen::unmangle(const std::string &mangled)
{
    auto pos = mangled.find('_');
    if (pos == std::string::npos)
        return mangled; // no prefix

    return mangled.substr(pos + 1); // everything after the first '_'
}
