#pragma once
#include "ast.hpp"
#include "deserial.hpp"
#include <string>
#include <vector>

class Linker {
public:
  // Constructor takes the current compilation unit object file
  Linker(Deserializer &deserializer, const std::string &currentObjectFile,
         bool isStatic);

  void processLinks(const std::vector<std::unique_ptr<Node>> &nodes,
                    const std::string &currentFile,
                    const std::string &outputExecutable);

private:
  Deserializer &deserializer;
  bool isStatic = false;

  std::string currentObjectFile;

  bool checkLLD();

  // Resolves internal lib mapping or returns user-provided string
  std::string resolveLinkPath(LinkStatement *link,
                              const std::string &currentFile);

  void compileNativeModule(const std::string &source,
                           const std::string &output);

  std::string getExecutableDir();
};
