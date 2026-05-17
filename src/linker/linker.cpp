#include "linker.hpp"
#include <filesystem>
#include <iostream>
#include <stdexcept>
#include <unistd.h>

namespace fs = std::filesystem;

Linker::Linker(bool isStatic) : isStatic(isStatic) {}

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
    throw std::runtime_error("ld.lld not found — install lld to use unnc");

  // Static library — just archive
  if (isStatic) {
    std::string libPath = outputExecutable;
    if (fs::path(libPath).extension() != ".a")
      libPath += ".a";
    std::string cmd = "ar rcs \"" + libPath + "\" \"" + currentObject + "\"";
    std::cout << "[ARCHIVER] " << cmd << "\n";
    if (system(cmd.c_str()) != 0)
      throw std::runtime_error("Archiver failed");
    return;
  }

  // Determine if we need dynamic linking
  bool needsDynamic = false;
  for (const auto &link : userLinks) {
    if (link.find(".so") != std::string::npos)
      needsDynamic = true;
  }

  std::string exeDir = getExecutableDir();
  fs::path coreDir = fs::path(exeDir).parent_path() / "core";
  fs::path scriptDir = fs::path(exeDir).parent_path() / "scripts";

  // Validate core files
  fs::path entryPath = coreDir / "entry.o";
  fs::path urcLib = coreDir / "urc.a";
  fs::path interpPath = coreDir / "interp.o";

  if (!fs::exists(entryPath))
    throw std::runtime_error("Missing core/entry.o");
  if (!fs::exists(urcLib))
    throw std::runtime_error("Missing core/urc.a");
  if (needsDynamic && !fs::exists(interpPath))
    throw std::runtime_error("Missing core/interp.o for dynamic build");

  // Pick linker script
  std::string scriptName =
      needsDynamic ? "linker_dynamic.ld" : "linker_static.ld";
  fs::path scriptPath = scriptDir / scriptName;
  if (!fs::exists(scriptPath))
    throw std::runtime_error("Missing linker script: " + scriptName);

  // Build command
  std::string cmd = "ld.lld -T " + scriptPath.string() +
                    " --gc-sections -o \"" + outputExecutable + "\"";

  // Core objects, order matters
  if (needsDynamic)
    cmd += " \"" + interpPath.string() + "\"";
  cmd += " \"" + entryPath.string() + "\"";

  // User's object
  cmd += " \"" + currentObject + "\"";

  // User links from Belt
  for (const auto &link : userLinks) {
    bool isPath = link.find('/') != std::string::npos ||
                  fs::path(link).extension() == ".o" ||
                  fs::path(link).extension() == ".a" ||
                  fs::path(link).extension() == ".so";

    if (isPath) {
      cmd += " \"" + link + "\"";
    } else {
      cmd += " -l" + link;
    }
  }

  // Core runtime last
  cmd += " \"" + urcLib.string() + "\"";

  std::cout << "[LINKER] Mode: " << (needsDynamic ? "DYNAMIC" : "STATIC")
            << "\n";
  std::cout << "[LINKER] " << cmd << "\n";

  if (system(cmd.c_str()) != 0)
    throw std::runtime_error("ld.lld failed");
}
