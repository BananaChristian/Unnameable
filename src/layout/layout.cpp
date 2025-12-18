#include "layout.hpp"
#include <string>

Layout::Layout(Semantics &sem, llvm::LLVMContext &ctx)
    : semantics(sem), context(ctx)
{
    tempModule = std::make_unique<llvm::Module>("prestatic_temp", context);
    registerComponentCalculatorFns();

    // Use the target’s data layout string (can be copied from final module settings)
    tempModule->setDataLayout("e-m:e-i64:64-f80:128-n8:16:32:64-S128");

    declareAllCustomTypes();

    registerImportedTypes();
}

// Calculator driver
void Layout::calculatorDriver(Node *node)
{
    if (!node)
    {
        std::cerr << "Invalid node\n";
    }

    auto it = calculatorFnsMap.find(typeid(*node));
    if (it == calculatorFnsMap.end())
    {
        std::cerr << "No calculator found for node: " << node->toString() << " Ignoring...\n";
        return;
    }

    (this->*it->second)(node);
}

void Layout::registerComponentCalculatorFns()
{
    calculatorFnsMap[typeid(LetStatement)] = &Layout::calculateLetStatementSize;
    calculatorFnsMap[typeid(WhileStatement)] = &Layout::calculateWhileStatementSize;
    calculatorFnsMap[typeid(ForStatement)] = &Layout::calculateForStatementSize;
    calculatorFnsMap[typeid(ifStatement)] = &Layout::calculateIfStatementSize;
    calculatorFnsMap[typeid(elifStatement)] = &Layout::calculateElifStatementSize;
    calculatorFnsMap[typeid(BlockStatement)] = &Layout::calculateBlockStatementMembersSize;
    calculatorFnsMap[typeid(FunctionStatement)] = &Layout::calculateFunctionStatement;
    calculatorFnsMap[typeid(FunctionExpression)] = &Layout::calculateFunctionExpression;
    calculatorFnsMap[typeid(BlockExpression)] = &Layout::calculateBlockExpression;
    calculatorFnsMap[typeid(DataStatement)] = &Layout::calculateDataStatement;
    calculatorFnsMap[typeid(ComponentStatement)] = &Layout::calculateComponentStatement;
    calculatorFnsMap[typeid(InstantiateStatement)] = &Layout::calculateInstantiateStatement;
    calculatorFnsMap[typeid(SealStatement)] = &Layout::calculateSealStatement;
}

// Independent calculators
void Layout::calculateLetStatementSize(Node *node)
{
    auto letStmt = dynamic_cast<LetStatement *>(node);
    auto type = dynamic_cast<BasicType *>(letStmt->type.get());

    if (!letStmt)
        return;
    std::cout << "[PRESTATIC LOG]: Calculating for let statement " << letStmt->toString() << "\n";

    int line = type->data_token.line;
    int col = type->data_token.column;

    const std::string &name = letStmt->ident_token.TokenLiteral;

    uint64_t compSize = 0;
    llvm::Align compAlignment;

    // Getting the let statement metaData
    auto letMeta = semantics.metaData.find(letStmt);
    if (letMeta == semantics.metaData.end())
    {
        logPrestaticError("No meta data found for let statement '" + name + "'", line, col);
        return;
    }

    // Getting if it is heap allocated
    if (!letStmt->isHeap)
    {
        std::cout << "Not calculating since its not heap allocated \n";
        // Dont bother calculating just keep it zero
        compSize = 0;
        return;
    }

    // Getting the symbolInfo
    auto letSym = letMeta->second;

    // Get the type stub
    llvm::Type *letType = getLLVMType(letSym->type);

    llvm::DataLayout DL(tempModule.get());

    compSize = DL.getTypeAllocSize(letType);
    compAlignment = DL.getABITypeAlign(letType);

    letSym->alignment = compAlignment;
    letSym->componentSize = compSize;

    totalHeapSize += compSize;
}

void Layout::calculateBlockStatementMembersSize(Node *node)
{
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
        return;

    for (const auto &stmt : blockStmt->statements)
    {
        calculatorDriver(stmt.get());
    }
}

void Layout::calculateForStatementSize(Node *node)
{
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        return;

    calculatorDriver(forStmt->body.get());
}

void Layout::calculateWhileStatementSize(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
        return;

    auto content = whileStmt->loop.get();

    calculatorDriver(content);
}

void Layout::calculateIfStatementSize(Node *node)
{
    auto ifStmt = dynamic_cast<ifStatement *>(node);
    if (!ifStmt)
        return;

    calculatorDriver(ifStmt->if_result.get());
    if (!ifStmt->elifClauses.empty())
    {
        for (const auto &elif : ifStmt->elifClauses)
        {
            calculatorDriver(elif.get());
        }
    }
    if (ifStmt->else_result.has_value())
    {
        calculatorDriver(ifStmt->else_result.value().get());
    }
}

void Layout::calculateElifStatementSize(Node *node)
{
    auto elifStmt = dynamic_cast<elifStatement *>(node);
    if (!elifStmt)
        return;

    calculatorDriver(elifStmt->elif_result.get());
}

void Layout::calculateFunctionStatement(Node *node)
{
    auto funcStmt = dynamic_cast<FunctionStatement *>(node);
    if (!funcStmt)
        return;

    calculatorDriver(funcStmt->funcExpr.get());
}

void Layout::calculateFunctionExpression(Node *node)
{
    auto funcExpr = dynamic_cast<FunctionExpression *>(node);
    if (!funcExpr)
        return;

    calculatorDriver(funcExpr->block.get());
}

void Layout::calculateBlockExpression(Node *node)
{
    auto blockExpr = dynamic_cast<BlockExpression *>(node);
    if (!blockExpr)
        return;

    if (!blockExpr->statements.empty())
    {
        for (const auto &stmt : blockExpr->statements)
        {
            calculatorDriver(stmt.get());
        }
    }

    if (blockExpr->finalexpr.has_value())
    {
        calculatorDriver(blockExpr->finalexpr.value().get());
    }
}

void Layout::calculateInstantiateStatement(Node *node)
{
    auto instStmt = dynamic_cast<InstantiateStatement *>(node);

    if (!instStmt)
        return;

    auto line = instStmt->instantiate_token.line;
    auto col = instStmt->instantiate_token.column;

    // Extract the semantic metaData(It has the instantiation info)
    auto it = semantics.metaData.find(instStmt);
    if (it == semantics.metaData.end())
    {
        logPrestaticError("Failed to find instantiation statement metaData", line, col);
        return;
    }

    auto sym = it->second;

    // Get the instantiation info
    const auto &instTable = sym->instTable;

    if (instTable.has_value())
    {
        // Call the calculator driver on the cloned AST
        calculatorDriver(instTable->instantiatedAST.get());
    }
}

void Layout::calculateDataStatement(Node *node)
{
    auto dataStmt = dynamic_cast<DataStatement *>(node);
    if (!dataStmt)
        return;

    auto dataName = dataStmt->dataBlockName->expression.TokenLiteral;
    auto line = dataStmt->dataBlockName->expression.line;
    auto col = dataStmt->dataBlockName->expression.column;

    auto dataMeta = semantics.metaData.find(dataStmt);
    if (dataMeta == semantics.metaData.end())
    {
        logPrestaticError("Could not find data block '" + dataName + "' metaData", line, col);
        return;
    }

    auto dataSym = dataMeta->second;
    if (!dataSym)
    {
        logPrestaticError("Unidentified type '" + dataName + "'", line, col);
        return;
    }

    std::vector<llvm::Type *> fieldTypes;

    for (const auto &[key, value] : dataSym->members)
    {
        llvm::Type *fieldType = getLLVMType(value->type);
        fieldTypes.push_back(fieldType);
    }

    // Independent analysis of the members in the data block
    for (const auto &stmt : dataStmt->fields)
    {
        calculatorDriver(stmt.get());
    }
    llvm::StructType *structTy = llvm::StructType::create(context, dataName);
    structTy->setBody(fieldTypes, /*isPacked*/ false);

    typeMap[dataName] = structTy;
}

void Layout::calculateComponentStatement(Node *node)
{
    auto compStmt = dynamic_cast<ComponentStatement *>(node);
    if (!compStmt)
        return;

    auto compName = compStmt->component_name->expression.TokenLiteral;
    auto line = compStmt->component_name->expression.line;
    auto col = compStmt->component_name->expression.column;

    auto compMeta = semantics.metaData.find(compStmt);
    if (compMeta == semantics.metaData.end())
    {
        logPrestaticError("Could not find component '" + compName + "' metaData", line, col);
        return;
    }

    auto compSym = compMeta->second;
    if (!compSym)
    {
        logPrestaticError("Unidentified type '" + compName + "'", line, col);
        return;
    }

    if (compSym->hasError)
        logPrestaticError("Semantic error detected", line, col);

    // Creating a component sketch
    std::vector<llvm::Type *> fieldTypes;

    for (const auto &[key, value] : compSym->members)
    {
        // Each member has a ResolvedType, get its LLVM type
        llvm::Type *fieldType = getLLVMType(value->type);
        fieldTypes.push_back(fieldType);
    }

    // Creating an empty struct type for data field types
    auto *structTy = llvm::cast<llvm::StructType>(typeMap[compName]);
    if (!structTy->isOpaque())
        return; // already defined

    structTy->setBody(fieldTypes, false);

    // Calculating the imported fields
    for (const auto &[key, value] : compSym->members)
    {
        calculatorDriver(value->node);
    }

    // Calculating for the private fields
    for (const auto &data : compStmt->privateData)
    {
        std::cout << "Inside private data calculation for: " << data->toString() << "\n";
        calculatorDriver(data.get());
    }

    // Calculating for the private methods
    for (const auto &method : compStmt->privateMethods)
    {
        std::cout << "Inside private method calculation\n";
        calculatorDriver(method.get());
    }
}

void Layout::calculateSealStatement(Node *node)
{
    auto sealStmt = dynamic_cast<SealStatement *>(node);
    if (!sealStmt)
        return;

    calculatorDriver(sealStmt->block.get());
}

void Layout::logPrestaticError(const std::string &message, int line, int col)
{
    std::cerr << "[LAYOUT ERROR] " << message << " on line :" << line << "and column: " << col << "\n";
}

llvm::Type *Layout::getLLVMType(ResolvedType type)
{
    llvm::Type *baseType = nullptr;

    switch (type.kind)
    {
    case DataType::SHORT_INT:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::USHORT_INT:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::INTEGER:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::UINTEGER:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::LONG_INT:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }

    case DataType::ULONG_INT:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }

    case DataType::EXTRA_INT:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }

    case DataType::UEXTRA_INT:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }

    case DataType::BOOLEAN:
    {
        baseType = llvm::Type::getInt1Ty(context);
        break;
    }

    case DataType::CHAR:
    {
        baseType = llvm::Type::getInt8Ty(context);
        break;
    }

    case DataType::CHAR16:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }

    case DataType::CHAR32:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }

    case DataType::FLOAT:
    {
        baseType = llvm::Type::getFloatTy(context);
        break;
    }

    case DataType::DOUBLE:
    {
        baseType = llvm::Type::getDoubleTy(context);
        break;
    }

    case DataType::STRING:
    {
        baseType = llvm::PointerType::get(context, 0);
        break;
    }

    case DataType::VOID:
    {
        baseType = llvm::Type::getVoidTy(context);
        break;
    }

    case DataType::DATABLOCK:
    case DataType::COMPONENT:
    {
        if (type.resolvedName.empty())
            throw std::runtime_error("Custom type requested but resolvedName is empty");

        auto it = typeMap.find(type.resolvedName);
        if (it != typeMap.end())
            baseType = it->second;
        else
            throw std::runtime_error("Layout requested for unknown custom type '" + type.resolvedName + "'");
        break;
    }

    case DataType::ENUM:
    {
        auto enumInfo = semantics.customTypesTable[type.resolvedName];
        baseType = getLLVMType({enumInfo->underLyingType, ""});
        break;
    }

    case DataType::ERROR:
    case DataType::GENERIC:
    case DataType::UNKNOWN:
        throw std::runtime_error("Unsupported or unknown data type encountered in getLLVMType");
    }

    // Wrap in a pointer if isPointer is true
    if (type.isPointer)
        return llvm::PointerType::get(baseType, 0);

    if (type.isRef)
        return llvm::PointerType::get(baseType, 0);

    return baseType;
}

void Layout::declareAllCustomTypes()
{
    std::cout << "Declaring all custom types" << "\n";
    for (const auto &[name, info] : semantics.customTypesTable)
    {
        auto typeIt = typeMap.find(name);
        if (typeIt == typeMap.end())
            typeMap[name] = llvm::StructType::create(context, name);
    }
}

void Layout::registerImportedTypes()
{
    std::cout << "Registering component type" << "\n";
    for (const auto &compTypesPair : semantics.ImportedComponentTable)
    {
        const auto &[compName, typeInfo] = compTypesPair;
        const auto &members = typeInfo->members;
        std::vector<llvm::Type *> fieldTypes;

        for (const auto &memberPair : members)
        {
            const auto &[memberName, memInfo] = memberPair;
            if (memInfo->isFunction)
                continue;

            llvm::Type *fieldType = getLLVMType(memInfo->type);
            fieldTypes.push_back(fieldType);
        }

        auto *structTy = llvm::cast<llvm::StructType>(typeMap[compName]);
        if (!structTy->isOpaque())
            return; // already defined

        structTy->setBody(fieldTypes, false);
    }

    for (const auto &dataTypesPair : semantics.ImportedDataBlocksTable)
    {
        const auto &[dataName, typeInfo] = dataTypesPair;
        const auto &members = typeInfo->members;

        std::vector<llvm::Type *> fieldTypes;

        for (const auto &memberPair : members)
        {
            const auto &[memberName, memInfo] = memberPair;
            llvm::Type *fieldType = getLLVMType(memInfo->type);
            fieldTypes.push_back(fieldType);
        }

        auto *structTy = llvm::cast<llvm::StructType>(typeMap[dataName]);
        if (!structTy->isOpaque())
            return; // already defined

        structTy->setBody(fieldTypes, false);
    }
}