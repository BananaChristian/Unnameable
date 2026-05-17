#pragma once
#include <string>
#include <vector>

class Linker {
public:
  // Constructor takes the current compilation unit object file
  Linker(bool isStatic);

  void processLinks(const std::string &currentObject,
                    const std::vector<std::string> &userLinks,
                    const std::string &outputExecutable);

private:
  bool isStatic = false;
  bool checkLLD();
  std::string getExecutableDir();
};
