#include "linker.hpp"
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <unistd.h>

namespace fs = std::filesystem;

Linker::Linker(const std::string &currentObject)
    : currentObjectFile(currentObject) {
  // Compulsory runtime objects
  compulsoryObjects.push_back("syscalls.o");
  compulsoryObjects.push_back("allocator.o");
  compulsoryObjects.push_back("unnitoa.o");
}

std::string Linker::resolveLinkPath(LinkStatement *link,
                                    const std::string &currentFile) {
  if (!link || !link->stringExpr)
    throw std::runtime_error("Linker Error: Null link statement");

  auto stringExpr = dynamic_cast<StringLiteral *>(link->stringExpr.get());
  std::string target = stringExpr->string_token.TokenLiteral;

  // If there's no slash and no extension, it's a name-tag (-l flag)
  bool isPath = (target.find('/') != std::string::npos);
  bool hasExt = fs::path(target).has_extension();

  if (!isPath && !hasExt) {
    return "-l" + target;
  }

  // Resolve as a File Path
  fs::path targetPath(target);
  fs::path resolved;

  if (targetPath.is_absolute()) {
    resolved = targetPath;
  } else {
    // Resolve relative to the source file
    resolved = fs::path(currentFile).parent_path() / targetPath;
  }

  // Ensure extension (Default to .o if the user was lazy)
  if (!resolved.has_extension()) {
    resolved += ".o";
  }

  if (!fs::exists(resolved)) {
    throw std::runtime_error("Linker Error: File not found: " +
                             resolved.string());
  }

  return resolved.string();
}

void Linker::processLinks(const std::vector<std::unique_ptr<Node>> &nodes,
                          const std::string &currentFile,
                          const std::string &outputExecutable) {
  std::vector<std::string> filesToLink;
  std::string exeDir = getExecutableDir();
  fs::path coreDir = fs::path(exeDir).parent_path() / "core";

  fs::path entryPath = coreDir / "entry.o";
  if (!fs::exists(entryPath))
    throw std::runtime_error("Link Driver Error: core/entry.o missing!");
  filesToLink.push_back(entryPath.string());

  // The User's Code
  filesToLink.push_back(currentObjectFile);

  // User-requested Links (link "whatever")
  for (const auto &node : nodes) {
    auto link = dynamic_cast<LinkStatement *>(node.get());
    if (link)
      filesToLink.push_back(resolveLinkPath(link, currentFile));
  }

  // The Core Support
  for (auto &obj : compulsoryObjects) {
    fs::path full = coreDir / obj;
    if (!fs::exists(full))
      throw std::runtime_error("Link Driver Error: Missing " + obj);
    filesToLink.push_back(full.string());
  }

  // Construct the LLD command
  // -T points to the map. --gc-sections throws away what isn't being using.
  std::string cmd = "ld.lld -T " + (coreDir / "linker.ld").string() +
                    " --gc-sections -o " + outputExecutable;

  for (auto &f : filesToLink) {
    if (f[0] == '-') { // Don't quote flags like -lSDL2
      cmd += " " + f;
    } else {
      cmd += " \"" + f + "\"";
    }
  }

  std::cout << "[LINKER] Link: " << cmd << "\n";

  if (system(cmd.c_str()) != 0)
    throw std::runtime_error("Link Driver Error: ld.lld failed.");
}

bool Linker::checkLLD() {
  return std::system("ld.lld --version > /dev/null 2>&1") == 0;
}

std::string Linker::getExecutableDir() {
  char buf[4096];
  ssize_t len = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
  if (len <= 0)
    throw std::runtime_error("Failed to resolve compiler path");

  buf[len] = '\0';
  return fs::path(buf).parent_path().string();
}
