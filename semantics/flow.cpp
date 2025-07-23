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
    if (ifStmt->elseif_condition.has_value())
    {
        auto elseifStmt = ifStmt->elseif_condition.value().get();
        DataType elseIfCondType = inferNodeDataType(elseifStmt);
        if (elseIfCondType != DataType::BOOLEAN)
        {
            std::cerr << "[SEMANTIC ERROR] Expected boolean type but got: " + dataTypetoString(ifStmtType) + "\n";
        }
        walker(elseifStmt);
    }

    // Dealing with else if result
    if (ifStmt->elseif_result.has_value())
    {
        auto elseifResult = ifStmt->elseif_result.value().get();
        walker(elseifResult);
    }

    // Dealing with else statement result
    if (ifStmt->else_result.has_value())
    {
        auto elseStmt = ifStmt->else_result.value().get();
        walker(elseStmt);
    }
}

void Semantics::walkForStatement(Node *node)
{
    auto forStmt = dynamic_cast<ForStatement *>(node);
    if (!forStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing for statement " << forStmt->toString() << "\n";

    // Handling the initializer
    auto initializer = forStmt->initializer.get();
    walker(initializer);
    // Handling the condition
    auto condition = forStmt->condition.get();
    walker(condition);
    // Handling the steps
    auto step = forStmt->step.get();
    walker(step);
    // Handling the block
    auto block = forStmt->step.get();
    walker(block);
}

void Semantics::walkEachStatement(Node *node)
{
    auto eachStmt = dynamic_cast<EachStatement *>(node);
    if (!eachStmt)
        return;
    std::cout << "[SEMANTIC LOG] Analyzing each statement " << eachStmt->toString() << "\n";
    // Handling iterator variable
    auto iterVar = eachStmt->iteratorVar.get();
    walker(iterVar);

    // Handling iterable
    auto iter = eachStmt->iterable.get();
    walker(iter);

    // Handling each stmt block
    auto block = eachStmt->body.get();
    walker(block);
}