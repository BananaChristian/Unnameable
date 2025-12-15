#pragma once
#include "ast.hpp"
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>

class Linker
{
public:
    // Constructor takes the current compilation unit object file
    Linker(const std::string &currentObjectFile);

    void processLinks(const std::vector<std::unique_ptr<Node>> &nodes, const std::string &currentFile, const std::string &outputExecutable);

private:
    std::string currentObjectFile;

    bool checkLLD();

    // Resolves internal lib mapping or returns user-provided string
    std::string resolveLinkPath(LinkStatement *link, const std::string &currentFile);

    // Runtime objects always linked silently
    std::vector<std::string> compulsoryObjects;

    // Internal core libs map  (for my future use)
    std::unordered_map<std::string, std::string> coreLibsMap;

    std::string findLibrary(const std::string &name);

    void appendSystemLibs(std::string &cmd);

    std::string getExecutableDir();
};
