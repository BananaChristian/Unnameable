#pragma once
#include <string>
#include <vector>

class Linker {
public:
  // Constructor takes the current compilation unit object file
  Linker(bool isFreeStanding, std::string customScriptPath);

  void processLinks(const std::string &currentObject,
                    const std::vector<std::string> &userLinks,
                    const std::string &outputExecutable);

private:
  bool freeStanding = false;
  std::string customScriptPath;
  bool checkLLD();
  std::string getExecutableDir();
};
