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
    calculatorFnsMap[typeid(ArrayStatement)] = &Layout::calculateArrayStatementSize;
    calculatorFnsMap[typeid(DheapStatement)] = &Layout::calculateDheapStatementSize;
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
    calculatorFnsMap[typeid(AllocatorStatement)] = &Layout::calculateAllocatorInterfaceSize;
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
    if (!letStmt->isHeap && !letStmt->isDheap)
    {
        std::cout << "Not calculating since its not heap or dheap allocated \n";
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

void Layout::calculateArrayStatementSize(Node *node)
{
    auto arrStmt = dynamic_cast<ArrayStatement *>(node);
    if (!arrStmt)
        return;

    const std::string &name = arrStmt->identifier->expression.TokenLiteral;
    auto arrMeta = semantics.metaData.find(arrStmt);
    if (arrMeta == semantics.metaData.end())
    {
        logPrestaticError("No meta data found for array '" + name + "'", arrStmt->identifier->expression.line, arrStmt->identifier->expression.column);
        return;
    }

    auto arrSym = arrMeta->second;

    if (!arrSym->isHeap && !arrSym->isDheap)
        return;

    // --- PROBE 1: Dimension Check ---
    auto &info = arrSym->arrayTyInfo;
    std::cout << "[DEBUG-LAYOUT] Processing: " << name << " | Dimensions found: " << info.sizePerDimension.size() << "\n";

    uint64_t totalElements = 1;
    if (info.sizePerDimension.empty())
    {
        std::cout << "[WARNING-LAYOUT] Array '" << name << "' has NO dimensions in metadata!\n";
        totalElements = 0; // This might be our culprit
    }

    for (size_t i = 0; i < info.sizePerDimension.size(); ++i)
    {
        int64_t dimSize = info.sizePerDimension[i];
        std::cout << "[DEBUG-LAYOUT]  -> Dim[" << i << "]: " << dimSize << "\n";
        if (dimSize > 0)
        {
            totalElements *= static_cast<uint64_t>(dimSize);
        }
        else
        {
            std::cout << "[WARNING-LAYOUT] Dimension " << i << " is <= 0. Possible dynamic box in SAGE?\n";
        }
    }

    // --- PROBE 2: Type Size Check ---
    llvm::Type *baseLLVMTy = getLLVMType(info.underLyingType);
    if (!baseLLVMTy)
    {
        std::cout << "[ERROR-LAYOUT] Could not resolve LLVM Type for base type: " << info.underLyingType.resolvedName << "\n";
        return;
    }

    llvm::DataLayout DL(tempModule.get());
    uint64_t elementSize = DL.getTypeAllocSize(baseLLVMTy);
    llvm::Align elementAlign = DL.getABITypeAlign(baseLLVMTy);

    std::cout << "[DEBUG-LAYOUT] Base Element: " << info.underLyingType.resolvedName
              << " | Size: " << elementSize << " bytes\n";

    uint64_t totalByteSize = totalElements * elementSize;

    // Store results for the IR Generator
    arrSym->componentSize = totalByteSize;
    arrSym->alignment = elementAlign;

    // --- PROBE 3: Pool Addition ---
    if (arrSym->isHeap)
    {
        totalHeapSize += totalByteSize;
        std::cout << "[LAYOUT-FINAL] SAGE Array '" << name << "' -> totalElements(" << totalElements
                  << ") * elementSize(" << elementSize << ") = " << totalByteSize << " bytes.\n";
        std::cout << "[LAYOUT-FINAL] Current Global SAGE Pool (totalHeapSize): " << totalHeapSize << " bytes.\n";
    }
    else
    {
        std::cout << "[LAYOUT-FINAL] DHEAP Array '" << name << "' -> Calculated " << totalByteSize << " bytes (skipped pool).\n";
    }
}

void Layout::calculateDheapStatementSize(Node *node)
{
    auto dheapStmt = dynamic_cast<DheapStatement *>(node);
    if (!dheapStmt)
        return;

    // Just gonna call the driver on whatever is wrapped
    calculatorDriver(dheapStmt->stmt.get());
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
    case DataType::I8:
    {
        baseType = llvm::Type::getInt8Ty(context);
        break;
    }
    case DataType::U8:
    {
        baseType = llvm::Type::getInt8Ty(context);
        break;
    }
    case DataType::I16:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }
    case DataType::U16:
    {
        baseType = llvm::Type::getInt16Ty(context);
        break;
    }
    case DataType::I32:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }
    case DataType::U32:
    {
        baseType = llvm::Type::getInt32Ty(context);
        break;
    }
    case DataType::I64:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }
    case DataType::U64:
    {
        baseType = llvm::Type::getInt64Ty(context);
        break;
    }
    case DataType::I128:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }
    case DataType::U128:
    {
        baseType = llvm::Type::getInt128Ty(context);
        break;
    }
    case DataType::ISIZE:
    case DataType::USIZE:
    {
        // We ask the module's DataLayout for the integer type that matches a pointer.
        // 'module' must be accessible here.
        baseType = tempModule->getDataLayout().getIntPtrType(context);
        break;
    }
    case DataType::BOOLEAN:
    {
        baseType = llvm::Type::getInt1Ty(context);
        break;
    }
    case DataType::CHAR8:
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

void Layout::calculateAllocatorInterfaceSize(Node *node)
{
    auto allocStmt = dynamic_cast<AllocatorStatement *>(node);
    if (!allocStmt)
        return;

    // Just call the driver on whatever is inside
    calculatorDriver(allocStmt->block.get());
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