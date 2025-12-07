#include <string>
#include <vector>
#include <regex>
#include <unordered_map>
#pragma once

enum class ErrorLevel
{
    LEXER,
    PARSER,
    SEMANTIC,
    LAYOUT,
    SENTINEL,
    IRGEN
};

struct CompilerError
{
    ErrorLevel level;
    int line;
    int col;
    std::string message;
    std::vector<std::string> hints;
};

struct HintRule {
    std::regex pattern;
    std::function<std::vector<std::string>(const std::smatch&, const std::string&)> handler;
};


class HintGenerator
{
public:
    HintGenerator();
    std::vector<std::string> generateHints(const CompilerError &error, std::string sourceLine);

private:
    using hintGeneratorFns = std::vector<std::string> (HintGenerator::*)(const CompilerError &error, std::string sourceLine);
    std::unordered_map<ErrorLevel, hintGeneratorFns> hintGeneratorFnsMap;

    std::unordered_map<ErrorLevel, std::vector<HintRule>> rules;


    void registerHintFns();
    std::vector<std::string> generateParserHints(const CompilerError &error, std::string sourceLine);
    std::vector<std::string> generateSemanticHints(const CompilerError &error, std::string sourceLine);
};

class ErrorHandler
{
    std::string fileName;

public:
    ErrorHandler(const std::string &fileName);
    void report(CompilerError &compilerError);

private:
    HintGenerator hintGen;
    std::vector<std::string> sourceLines;

    void loadSourceLines();
    std::string getLevelDisplayName(ErrorLevel level);
    std::string getBaseName(const std::string &fullPath);
    std::string getSourceLine(int line, std::vector<std::string> sourceLines);
};