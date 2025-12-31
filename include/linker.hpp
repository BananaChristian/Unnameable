#pragma once
#include "ast.hpp"
#include <string>
#include <vector>

class Linker {
public:
  // Constructor takes the current compilation unit object file
  Linker(const std::string &currentObjectFile, bool isStatic);

  void processLinks(const std::vector<std::unique_ptr<Node>> &nodes,
                    const std::string &currentFile,
                    const std::string &outputExecutable);

private:
  bool isStatic = false;

  std::string currentObjectFile;

  bool checkLLD();

  // Resolves internal lib mapping or returns user-provided string
  std::string resolveLinkPath(LinkStatement *link,
                              const std::string &currentFile);

  std::string getExecutableDir();
};
