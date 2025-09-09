#pragma once
#include <cstddef>
#include "ast.hpp"
#include "semantics/semantics.hpp"
#include "irgen/irgen.hpp"
#include <typeindex>

class Static
{
public:
    Semantics &semantics;
    IRGenerator &irgen;
    Static(Semantics &semantics, IRGenerator &irgen);
    using analyzerFuncs = void (Static::*)(Node *node);
    std::unordered_map<std::type_index, analyzerFuncs> analyzerFuncsMap;
    uint64_t total_size = 0;
    std::vector<Node> allocationStack();
    // Main analyzer function
    void analyze(Node *node);

    void dumpTotal();

private:
    // Analyzer functions
    void analyzeLetStatement(Node *node);
    void analyzeAssignmentStatement(Node *node);
    void analyzeIdentifier(Node *node);
    void analyzeFunctionStatement(Node *node);
    void analyzeFunctionExpression(Node *node);
    void analyzeFunctionDeclarationExpression(Node *node);
    void analyzeDataStatement(Node *node);
    void analyzeBehaviorStatement(Node *node);
    void analyzeComponentStatement(Node *node);
    // Helpers
    void registerAnalyzerFunctions();
};