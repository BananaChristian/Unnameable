#pragma once
#include <vector>
#include "ast.hpp"
#include "typeindex"
#include "semantics/semantics.hpp"
#include <unordered_map>
#include <string>

class Sentinel
{
public:
    Sentinel(Semantics &semantics);
    using sentinelFn = void (Sentinel::*)(Node *node);
    std::unordered_map<std::type_index, sentinelFn> sentinelFnsMap;
    int nextAllocId = 0;

    std::vector<std::shared_ptr<SymbolInfo>> sentinelStack;

    void sentinelDriver(Node *node);

private:
    Semantics &semantics;
    void checkLetStatement(Node *node);
    void checkAssignmentStatement(Node *node);
    void checkIdentifier(Node *node);
    void checkBlockStatement(Node *node);
    void checkIfStatement(Node *node);
    void checkElifStatement(Node *node);
    void checkWhileStatement(Node *node);
    void checkForStatement(Node *node);
    void registerSentinelFns();
    void logError(const std::string &message, int line, int col);
};