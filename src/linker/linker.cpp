#include "linker.hpp"
#include <filesystem>
#include <iostream>
#include <stdexcept>
#include <unistd.h>

namespace fs = std::filesystem;

Linker::Linker(bool freeStanding, std::string customScriptPath)
    : freeStanding(freeStanding), customScriptPath(customScriptPath) {}

std::string Linker::getExecutableDir() {
  char buf[4096];
  ssize_t len = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
  if (len <= 0)
    throw std::runtime_error("Failed to resolve compiler path");
  buf[len] = '\0';
  return fs::path(buf).parent_path().string();
}

bool Linker::checkLLD() {
  return std::system("ld.lld --version > /dev/null 2>&1") == 0;
}

void Linker::processLinks(const std::string &currentObject,
                          const std::vector<std::string> &userLinks,
                          const std::string &outputExecutable) {

  if (!checkLLD())
    throw std::runtime_error(
        "ld.lld not found, please install lld to use unnc");

  // Determine if we need dynamic linking
  bool needsDynamic = false;
  for (const auto &link : userLinks) {
    if (link.find(".so") != std::string::npos)
      needsDynamic = true;
  }

  std::string exeDir = getExecutableDir();
  fs::path buildDir = fs::path(exeDir).parent_path() / "build";
  fs::path coreDir = buildDir / "core";
  fs::path palDir = buildDir / "pal" / "x86_64" / "linux";
  fs::path scriptDir = fs::path(exeDir).parent_path() / "scripts";

  // PAL files
  fs::path entryPath = palDir / "entry.o";
  fs::path interpPath = palDir / "interp.o";
  fs::path palLib = palDir / "pal.a";

  // Core file
  fs::path urcLib = coreDir / "urc.a";

  if (!freeStanding) {
    if (!fs::exists(entryPath))
      throw std::runtime_error("Missing pal/x86_64/linux/entry.o");
    if (!fs::exists(palLib))
      throw std::runtime_error("Missing pal/x86_64/linux/pal.a");
    if (!fs::exists(urcLib))
      throw std::runtime_error("Missing core/urc.a");
    if (needsDynamic && !fs::exists(interpPath))
      throw std::runtime_error(
          "Missing pal/x86_64/linux/interp.o for dynamic build");
  }

  // Pick linker script
  std::string scriptName =
      needsDynamic ? "linker_dynamic.ld" : "linker_static.ld";
  fs::path scriptPath = scriptDir / scriptName;

  // If the user provided a custom path override and use it
  if (!customScriptPath.empty())
    scriptPath = customScriptPath;

  if (!fs::exists(scriptPath))
    throw std::runtime_error("Could not find linker script: " +
                             customScriptPath);

  // Build command
  std::string cmd = "ld.lld -T " + scriptPath.string() +
                    " --gc-sections -o \"" + outputExecutable + "\"";

  // PAL objects, order matters
  if (!freeStanding) {
    if (needsDynamic)
      cmd += " \"" + interpPath.string() + "\"";
    cmd += " \"" + entryPath.string() + "\"";
  }

  // User's object
  cmd += " \"" + currentObject + "\"";

  // User links
  for (const auto &link : userLinks) {
    bool isPath = link.find('/') != std::string::npos ||
                  fs::path(link).extension() == ".o" ||
                  fs::path(link).extension() == ".a" ||
                  fs::path(link).extension() == ".so";
    if (isPath)
      cmd += " \"" + link + "\"";
    else
      cmd += " -l" + link;
  }

  // Core last always
  cmd += " \"" + urcLib.string() + "\"";

  // PAL.a after core if not freestanding
  if (!freeStanding)
    cmd += " \"" + palLib.string() + "\"";

  std::cout << "[LINKER] " << cmd << "\n";

  if (system(cmd.c_str()) != 0)
    throw std::runtime_error("ld.lld failed");
}
