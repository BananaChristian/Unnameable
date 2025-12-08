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
}

void StubGen::finish()
{
    // Manually create stub file path
    std::string stubFile = fileName;
    auto pos = stubFile.rfind('.');
    if (pos != std::string::npos)
        stubFile = stubFile.substr(0, pos); // strip original extension

    stubFile += ".stub"; // add .stub extension

    serializeStubTable(stubTable, stubFile);
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

    Stub stub;
    stub.name = funcName;
    stub.returnType = sym->returnType;
    stub.paramTypes = sym->paramTypes;
    stub.type = exportType::FUNCTION;

    stubTable[funcName] = stub;
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

    Stub stub;
    stub.name = funcName;
    stub.returnType = sym->returnType;
    stub.paramTypes = sym->paramTypes;
    stub.type = exportType::DECLARATION;

    stubTable[funcName] = stub;
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

void StubGen::serializeStub(std::ostream &out, const Stub &stub)
{
    writeString(out, stub.name);
    serializeResolvedType(out, stub.returnType);
    serializeParamTypes(out, stub.paramTypes);
    write_u32(out, static_cast<uint32_t>(stub.type));
}

void StubGen::serializeStubTable(const std::unordered_map<std::string, Stub> &table, const std::string &filename)
{
    std::cout << "Serializing stub table\n";
    std::ofstream out(filename, std::ios::binary);
    if (!out)
        throw std::runtime_error("Failed to open stub file");

    // STUB Magic
    write_u32(out, 0x53545542);
    // Version
    write_u32(out, 1);

    write_u32(out, table.size());

    for (const auto &kv : table)
    {
        serializeStub(out, kv.second);
    }
}
