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
        std::cerr << "[SEMANTIC ERROR] Expected boolean type but got: " + dataTypetoString(whileCondType) + "\n";
    }
    walker(whileCondition);
    auto whileLoop = whileStmt->loop.get();
    walker(whileLoop);
}

void Semantics::walkIfStatement(Node *node)
{
    auto ifStmt = dynamic_cast<ifStatement *>(node);
    if (!ifStmt)
        return;
    std::cout << "[SEMANTIC LOG]: Analysing if statement: " << ifStmt->toString() << "\n";
    auto ifStmtCondition = ifStmt->condition.get();
    DataType ifStmtType = inferNodeDataType(ifStmtCondition);
    if (ifStmtType != DataType::BOOLEAN)
    {
        std::cerr << "[SEMANTIC ERROR] Expected boolean type but got: " + dataTypetoString(ifStmtType) + "\n";
    }
    walker(ifStmtCondition);

    // Dealing with the if result
    auto ifResult = ifStmt->if_result.get();
    walker(ifResult);

    // Dealing with the else if conditon
    auto elseifStmt = ifStmt->elseif_condition.value().get();
    DataType elseIfCondType = inferNodeDataType(elseifStmt);
    if (elseIfCondType != DataType::BOOLEAN)
    {
        std::cerr << "[SEMANTIC ERROR] Expected boolean type but got: " + dataTypetoString(ifStmtType) + "\n";
    }
    walker(elseifStmt);
    // Dealing with else if result
    auto elseifResult = ifStmt->elseif_result.value().get();
    walker(elseifResult);

    // Dealing with else statement result
    auto elseStmt = ifStmt->elseif_result.value().get();
    walker(elseStmt);
}