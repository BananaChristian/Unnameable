#include "semantics.hpp"

void Semantics::walkArrayStatement(Node *node)
{
    auto arrStmt = dynamic_cast<ArrayStatement *>(node);
    if (!arrStmt)
        return;

    // --- Mutability flags ---
    bool isMutable = arrStmt->mutability == Mutability::MUTABLE;
    bool isConstant = arrStmt->mutability == Mutability::CONSTANT;

    const auto &arrayName = arrStmt->identifier->expression.TokenLiteral;
    int arrNameLine = arrStmt->identifier->expression.line;
    int arrNameCol = arrStmt->identifier->expression.column;

    if (resolveSymbolInfo(arrayName))
    {
        logSemanticErrors("Array name '" + arrayName + "' already exists", arrNameLine, arrNameCol);
        return;
    }

    bool hasError = false;
    bool isInitialized = false;
    bool isHeap = arrStmt->isHeap;
    bool isDheap = arrStmt->isDheap;
    ArrayTypeInfo arrTypeInfo;

    // --- Base type inference ---
    ResolvedType baseType = inferNodeDataType(arrStmt->arrayType.get());
    ResolvedType arrayType;
    // Get the array declaration dimensions
    int arrStmtDimensions = 0;
    // If the array dimesions are empty infer from the array literal but only if the array literal exists
    if (arrStmt->lengths.empty())
    {
        if (!arrStmt->array_content)
        {
            logSemanticErrors("Cannot infer dimension count if you do not initialize array declaration '" + arrayName + "'", arrNameLine, arrNameCol);
            return;
        }
        else
        {
            auto literalType = inferNodeDataType(arrStmt->array_content.get());
            arrStmtDimensions = getArrayTypeInfo(arrStmt->array_content.get()).dimensions;
            arrayType = literalType;
        }
    }
    else
    {
        arrStmtDimensions = arrStmt->lengths.size();
        arrayType = makeArrayType(baseType, arrStmtDimensions);
    }

    std::cout
        << "BASE TYPE: " << baseType.resolvedName << "\n";
    std::cout << "ARRAY TYPE: " << arrayType.resolvedName << "\n";

    arrTypeInfo.underLyingType = baseType;

    // --- Walk length expressions ---
    for (const auto &len : arrStmt->lengths)
    {
        walker(len.get());

        int64_t evaluatedLen = evaluateArrayLengthConstant(len.get());
        if (!isDheap && evaluatedLen <= 0)
        {
            logSemanticErrors("Array dimension length must be greater than 0", len->expression.line, len->expression.column);
            hasError = true;
        }

        arrTypeInfo.sizePerDimension.push_back(evaluatedLen);
    }

    // --- Handle initializer ---
    if (arrStmt->array_content)
    {
        walker(arrStmt->array_content.get());
        isInitialized = true;

        ArrayLiteral *arrLit = dynamic_cast<ArrayLiteral *>(arrStmt->array_content.get());
        if (!arrLit)
        {
            std::cerr << "Only use array literals for array statements\n";
            hasError = true;
            return;
        }

        ResolvedType literalType = inferNodeDataType(arrLit);

        // Retrieve the array literal dimensions
        int arrLitDimensions = getArrayTypeInfo(arrLit).dimensions;

        // Dimension check
        if (arrStmtDimensions != arrLitDimensions)
        {
            logSemanticErrors("Dimensions mismatch in array declaration '" + arrayName + "' expected '" + std::to_string(arrStmtDimensions) + "' but got '" + std::to_string(arrLitDimensions) + "'", arrNameLine, arrNameCol);
            hasError = true;
        }

        if (!isTypeCompatible(arrayType, literalType))
        {
            logSemanticErrors("Declared type '" + arrayType.resolvedName +
                                  "' does not match the initialized type '" + literalType.resolvedName + "'",
                              arrLit->expression.line, arrLit->expression.column);
            hasError = true;
        }

        // Use a recursive lambda to check every nested dimension
        if (!isDheap)
        {
            std::function<void(ArrayLiteral *, int)> verifyRecursive;
            verifyRecursive = [&](ArrayLiteral *lit, int dimIdx)
            {
                if (dimIdx >= arrTypeInfo.sizePerDimension.size())
                    return;

                // Use sizePerDimension vector to check against the current literal elements
                size_t expected = static_cast<size_t>(arrTypeInfo.sizePerDimension[dimIdx]);
                size_t actual = lit->array.size();

                if (expected != actual)
                {
                    logSemanticErrors("Dimension length mismatch, expected " + std::to_string(expected) + " elements but got " + std::to_string(actual), lit->expression.line, lit->expression.column);
                    hasError = true;
                    return;
                }

                // If there is a dimension below this one, we must keep checking
                if (dimIdx + 1 < arrTypeInfo.sizePerDimension.size())
                {
                    for (auto &element : lit->array)
                    {
                        auto subLit = dynamic_cast<ArrayLiteral *>(element.get());
                        if (subLit)
                        {
                            verifyRecursive(subLit, dimIdx + 1);
                        }
                        else
                        {
                            logSemanticErrors("Expected nested array literal for dimension " + std::to_string(dimIdx + 1),
                                              element->expression.line, element->expression.column);
                            hasError = true;
                            return;
                        }
                    }
                }
            };

            // Start the check at the top level (index 0)
            verifyRecursive(arrLit, 0);
        }
    }
    else if (isConstant)
    {
        logSemanticErrors("Constant array '" + arrayName + "' must be initialized",
                          arrStmt->statement.line, arrStmt->statement.column);
        hasError = true;
    }
    else if (arrStmt->lengths.empty())
    {
        logSemanticErrors("Uninitialized array '" + arrayName + "' is missing length declarations",
                          arrStmt->statement.line, arrStmt->statement.column);
        hasError = true;
    }

    arrTypeInfo.dimensions = arrStmtDimensions;

    if (isHeap)
    {
        for (int64_t len : arrTypeInfo.sizePerDimension)
        {
            if (len <= 0)
            {
                logSemanticErrors("heap arrays must have constant literal lengths > 0.",
                                  arrNameLine, arrNameCol);
                hasError = true;
                break;
            }
        }
    }

    // --- Store symbol info ---
    auto arrInfo = std::make_shared<SymbolInfo>();
    arrInfo->isMutable = isMutable;
    arrInfo->isConstant = isConstant;
    arrInfo->arrayTyInfo = arrTypeInfo;
    arrInfo->hasError = hasError;
    arrInfo->isInitialized = isInitialized;
    arrInfo->isHeap = isHeap;
    arrInfo->isDheap = isDheap;
    arrInfo->lastUseNode = arrStmt;
    arrInfo->type = arrayType; // store fully wrapped type
    arrInfo->arrayTyInfo = arrTypeInfo;
    if (!loopContext.empty() && loopContext.back())
    {
        arrInfo->needsPostLoopFree = true;
        arrInfo->bornInLoop = true;
    }

    metaData[arrStmt] = arrInfo;
    symbolTable.back()[arrayName] = arrInfo;

    std::cout << "FINAL ARRAY STATEMENT TYPE '" << arrayType.resolvedName << "'\n";
}