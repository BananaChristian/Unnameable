#include "semantics.hpp"

void Semantics::walkBlockStatement(Node *node)
{
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing block statement: " << blockStmt->toString() << "\n";
    auto &stmts = blockStmt->statements;
    for (const auto &stmt : stmts)
    {
        walker(stmt.get());
    }
}

void Semantics::walkWhileStatement(Node *node)
{
    auto whileStmt = dynamic_cast<WhileStatement *>(node);
    if (!whileStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing while statement: " << whileStmt->toString() << "\n";
    auto whileCondition = whileStmt->condition.get();
    DataType whileCondType = inferNodeDataType(whileCondition);
    if (whileCondType != DataType::BOOLEAN)
    {
        std::cerr << "[SEMANTIC ERROR] Expected boolean type but got: " + dataTypetoString(whileCondType) +"\n";
    }
    walker(whileCondition);
    auto whileLoop = whileStmt->loop.get();
    walker(whileLoop);
}