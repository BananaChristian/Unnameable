#include "linker.hpp"
#include "deserial.hpp"
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <stdexcept>
#include <sys/types.h>
#include <unistd.h>

namespace fs = std::filesystem;

Linker::Linker(Deserializer &deserializer, const std::string &currentObject,
               bool isStatic)
    : deserializer(deserializer), currentObjectFile(currentObject),
      isStatic(isStatic) {}

std::string Linker::resolveLinkPath(LinkStatement *link,
                                    const std::string &currentFile) {
  if (!link || !link->stringExpr)
    throw std::runtime_error("Linker Error: Null link statement");

  auto stringExpr = dynamic_cast<StringLiteral *>(link->stringExpr.get());
  std::string target = stringExpr->string_token.TokenLiteral;

  // ---  Directory Search Path (-L flag) ---
  // If it ends in a slash, treat it as a library search directory.
  // We check this first to prevent appending ".o" to directory names.
  if (!target.empty() && (target.back() == '/' || target.back() == '\\')) {
    fs::path searchPath(target);
    if (searchPath.is_relative()) {
      searchPath = fs::path(currentFile).parent_path() / searchPath;
    }
    return "-L" + fs::absolute(searchPath).string();
  }

  // --- System Library (-l flag) ---
  // No slash (not a path) and no extension (not a file) = name-tag.
  bool isPath = (target.find('/') != std::string::npos ||
                 target.find('\\') != std::string::npos);
  bool hasExt = fs::path(target).has_extension();

  if (!isPath && !hasExt) {
    return "-l" + target;
  }

  // --- Specific File Path (.o, .a, .so) ---
  fs::path targetPath(target);
  fs::path resolved;

  if (targetPath.is_absolute()) {
    resolved = targetPath;
  } else {
    // Resolve relative to the source file where 'link' was written
    resolved = fs::path(currentFile).parent_path() / targetPath;
  }

  // If the user  left off the extension for a file path
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

  if (isStatic) {
    std::string libPath = outputExecutable;
    // Force the .a extension if the user didn't provide it
    if (fs::path(libPath).extension() != ".a") {
      libPath += ".a";
    }

    std::string cmd =
        "ar rcs \"" + libPath + "\" \"" + currentObjectFile + "\"";

    std::cout << "[ARCHIVER] Packaging: " << cmd << "\n";
    if (system(cmd.c_str()) != 0)
      throw std::runtime_error("Link Driver Error: archiver(ar) failed.");

    return;
  }

  // --- ANALYZE FOR DYNAMIC DEPENDENCIES ---
  std::vector<std::string> userLinks;
  bool needsDynamic = false;

  for (const auto &node : nodes) {
    if (auto link = dynamic_cast<LinkStatement *>(node.get())) {
      std::string resolved = resolveLinkPath(link, currentFile);
      userLinks.push_back(resolved);

      // Trigger dynamic mode if we see a .so or a system library flag (-l)
      if (resolved.find(".so") != std::string::npos ||
          resolved.substr(0, 2) == "-l") {
        needsDynamic = true;
      }
    }
  }

  std::vector<std::string> filesToLink;
  std::string exeDir = getExecutableDir();
  fs::path coreDir = fs::path(exeDir).parent_path() / "core";

  if (needsDynamic) {
    fs::path interpPath = coreDir / "interp.o";
    if (!fs::exists(interpPath))
      throw std::runtime_error(
          "Linker Error: interp.o missing for dynamic build!");
    filesToLink.push_back(interpPath.string());
  }

  fs::path entryPath = coreDir / "entry.o";
  if (!fs::exists(entryPath))
    throw std::runtime_error("Link Driver Error: core/entry.o missing!");
  filesToLink.push_back(entryPath.string());

  // The User's Code
  filesToLink.push_back(currentObjectFile);

  // Link what the deserializer gave us
  for (const auto &entry : deserializer.linkerRegistery) {
    if (entry.origin == LinkOrigin::NATIVE_IMPORT) {
      fs::path objPath(entry.path);

      // If .o is missing, attempt to auto-compile the .unn
      if (!fs::exists(objPath)) {
        fs::path sourcePath = objPath;
        sourcePath.replace_extension(".unn");

        if (fs::exists(sourcePath)) {
          std::cout << "[AUTO-COMPILE] " << sourcePath.string() << "\n";
          compileNativeModule(sourcePath.string(), objPath.string());
        } else {
          throw std::runtime_error(
              "Linker Error: Cannot find object or source for: " + entry.path);
        }
      }
      filesToLink.push_back(objPath.string());
    }
  }

  // User-requested Links (link "whatever")
  for (const auto &node : nodes) {
    auto link = dynamic_cast<LinkStatement *>(node.get());
    if (link)
      filesToLink.push_back(resolveLinkPath(link, currentFile));
  }

  // The Core Support
  fs::path urcLib = coreDir / "urc.a";
  if (!fs::exists(urcLib))
    throw std::runtime_error("Link Driver Error: Missing 'urc.a' ");

  filesToLink.push_back(urcLib.string());

  // ---CHOOSE THE SCRIPT AND EXECUTE ---
  std::string scriptName =
      needsDynamic ? "linker_dynamic.ld" : "linker_static.ld";
  fs::path scriptPath = coreDir / scriptName;

  if (!fs::exists(scriptPath))
    throw std::runtime_error("Linker Error: Missing script " + scriptName);

  // Construct the LLD command
  // -T points to the map. --gc-sections throws away what isn't being using.
  std::string cmd = "ld.lld -T " + scriptPath.string() + " --gc-sections -o " +
                    outputExecutable;

  for (auto &f : filesToLink) {
    if (f[0] == '-') { // Don't quote flags like -lSDL2
      cmd += " " + f;
    } else {
      cmd += " \"" + f + "\"";
    }
  }

  std::cout << "[LINKER] Mode: " << (needsDynamic ? "DYNAMIC" : "STATIC")
            << "\n";
  std::cout << "[LINKER] Link: " << cmd << "\n";

  if (system(cmd.c_str()) != 0)
    throw std::runtime_error("Link Driver Error: ld.lld failed.");
}

void Linker::compileNativeModule(const std::string &source,
                                 const std::string &output) {
  // Get the absolute path to this compiler binary
  char buf[4096];
  ssize_t len = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
  if (len <= 0)
    throw std::runtime_error(
        "Linker Error: Failed to resolve self-path for auto-compile");

  buf[len] = '\0';
  std::string compilerPath(buf);

  // Build the command: unnc <source> -compile <output>
  std::string cmd =
      "\"" + compilerPath + "\" \"" + source + "\" -compile \"" + output + "\"";

  std::cout << "[AUTO-COMPILE] " << source << " -> " << output << "\n";

  // Fire the sub-compiler
  int result = std::system(cmd.c_str());

  if (result != 0) {
    throw std::runtime_error(
        "Linker Error: Failed to compile module: " + source +
        " (Exit code: " + std::to_string(result) + ")");
  }
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
