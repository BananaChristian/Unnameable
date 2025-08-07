#include "semantics.hpp"

void Semantics::walkDataStatement(Node *node)
{
    auto dataBlockStmt = dynamic_cast<DataStatement *>(node);

    if (!dataBlockStmt)
        return;

    std::cout << "[SEMANTIC LOG] Analysing data statement " + dataBlockStmt->toString() + "\n";
    // Checking is the data blocks name is already take by something else
    const std::string &dataBlockName = dataBlockStmt->dataBlockName->expression.TokenLiteral;

    // Check if the data block name already exists
    auto dataBlockSymbolInfo = resolveSymbolInfo(dataBlockName);
    if (dataBlockSymbolInfo)
    {
        logSemanticErrors("Cannot reuse name '" + dataBlockName + "' for data block", node->token.line, node->token.column);
        return;
    }

    // Dealing with the information inside the data block first we push a local scope
    symbolTable.emplace_back(); // Pushing a local scope
    std::vector<SymbolInfo> fieldSymbols;
    bool Constant = false;
    bool Mutable = false;
    auto &dataBlockFields = dataBlockStmt->fields;
    for (const auto &field : dataBlockFields)
    {
        auto letStmt = dynamic_cast<LetStatement *>(field.get());
        walker(field.get());
        if (letStmt)
        {
            if (letStmt->mutability == Mutability::MUTABLE)
            {
                Mutable = true;
            }
            else if (letStmt->mutability == Mutability::CONSTANT)
            {
                Constant = false;
            }

            fieldSymbols.push_back(
                {.symbolDataType = inferNodeDataType(letStmt),
                 .isNullable = letStmt->isNullable,
                 .isMutable = Mutable,
                 .isConstant = Constant,
                 .isInitialized = false});
        }
    }
    sharedDataBlocks[dataBlockName] = std::move(fieldSymbols);
    symbolTable.pop_back();
}
