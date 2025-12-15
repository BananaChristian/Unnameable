#include "linker.hpp"
#include <iostream>
#include <cstdlib>
#include <filesystem>
#include <unistd.h>

namespace fs = std::filesystem;

Linker::Linker(const std::string &currentObject)
    : currentObjectFile(currentObject)
{
    // Compulsory runtime objects
    compulsoryObjects.push_back("allocator.o");
    compulsoryObjects.push_back("helper.o");
}

std::string Linker::resolveLinkPath(LinkStatement *link, const std::string &currentFile)
{
    if (!link)
        throw std::runtime_error("Invalid link node");

    if (!link->stringExpr)
        throw std::runtime_error("Invalid link string node");

    auto stringExpr = dynamic_cast<StringLiteral *>(link->stringExpr.get());

    std::string target = stringExpr->string_token.TokenLiteral;
    fs::path targetPath(target);

    // If the user already gave a full path, just return it
    if (targetPath.is_absolute())
        return targetPath.string();

    // If it ends with a recognized object/library extension, treat as file
    if (targetPath.has_extension())
    {
        fs::path resolved = fs::path(currentFile).parent_path() / targetPath;
        return resolved.string();
    }

#if defined(_WIN32)
    // Windows: .lib or .dll
    fs::path winLib = target + ".lib";
    fs::path resolvedWin = fs::path(currentFile).parent_path() / winLib;
    if (fs::exists(resolvedWin))
        return resolvedWin.string();
#else
    // Linux/Unix: lib{name}.a or lib{name}.so
    fs::path libStatic = fs::path("lib" + target + ".a");
    fs::path libShared = fs::path("lib" + target + ".so");

    fs::path resolvedDir = fs::path(currentFile).parent_path();
    fs::path resolvedStatic = resolvedDir / libStatic;
    fs::path resolvedShared = resolvedDir / libShared;

    if (fs::exists(resolvedStatic))
        return resolvedStatic.string();
    if (fs::exists(resolvedShared))
        return resolvedShared.string();
#endif

    // Fallback: assume object file in the current directory
    fs::path fallback = fs::path(currentFile).parent_path() / (target + ".o");
    return fallback.string();
}

void Linker::processLinks(const std::vector<std::unique_ptr<Node>> &nodes,
                          const std::string &currentFile,
                          const std::string &outputExecutable)
{
    std::vector<std::string> filesToLink;
    std::unordered_set<std::string> seen;

    // Always include the current compilation unit first
    filesToLink.push_back(currentObjectFile);
    seen.insert(currentObjectFile);

    // Include user links
    for (const auto &node : nodes)
    {
        auto link = dynamic_cast<LinkStatement *>(node.get());
        if (!link)
            continue;

        std::string resolved = resolveLinkPath(link, currentFile);
        if (seen.insert(resolved).second)
            filesToLink.push_back(resolved);
    }

    // Include runtime objects
    std::string exeDir = getExecutableDir();
    fs::path runtimeDir = fs::path(exeDir).parent_path() / "runtime";
    for (auto &obj : compulsoryObjects)
    {
        fs::path full = runtimeDir / obj;
        std::string fullStr = full.string();
        if (!fs::exists(full))
            throw std::runtime_error("Missing runtime object: " + fullStr);

        if (seen.insert(fullStr).second)
            filesToLink.push_back(fullStr);
    }

    // Use g++ as linker instead of LLD
    std::string cmd = "g++ -std=c++17 -g -o \"" + outputExecutable + "\"";
    for (auto &f : filesToLink)
        cmd += " \"" + f + "\"";


    std::cout << "[LINKER] Running: " << cmd << "\n";

    int result = system(cmd.c_str());
    if (result != 0)
        throw std::runtime_error("[LINKER ERROR] g++ failed with exit code " + std::to_string(result));

    std::cout << "[LINKER] Linking successful\n";
}


bool Linker::checkLLD()
{
#if defined(_WIN32)
    return std::system("lld-link --version > nul 2>&1") == 0;
#else
    return std::system("ld.lld --version > /dev/null 2>&1") == 0;
#endif
}

std::string Linker::findLibrary(const std::string &name)
{
    std::vector<std::string> searchPaths = {
        "/usr/lib/x86_64-linux-gnu",
        "/lib/x86_64-linux-gnu",
        "/usr/lib",
        "/lib"};

    for (auto &dir : searchPaths)
    {
        for (auto &entry : fs::directory_iterator(dir))
        {
            if (entry.path().filename().string().find(name) != std::string::npos)
            {
                return entry.path().string();
            }
        }
    }
    return ""; // Not found
}

void Linker::appendSystemLibs(std::string &cmd)
{
    std::string libc = findLibrary("libc.so");
    std::string libgcc = findLibrary("libgcc_s.so");

    if (!libc.empty())
        cmd += " " + libc;
    if (!libgcc.empty())
        cmd += " " + libgcc;

    if (libc.empty() || libgcc.empty())
        std::cerr << "[LINKER WARNING] Could not find libc or libgcc!\n";
}

std::string Linker::getExecutableDir()
{
    char buf[4096];
    ssize_t len = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
    if (len <= 0)
        throw std::runtime_error("Failed to resolve compiler path");

    buf[len] = '\0';
    return fs::path(buf).parent_path().string();
}
