use std::env::consts::{ARCH, OS};

pub struct TargetSpec {
    architecture: String,
    os: String,
    pub pointer_width: usize, //Size for the pointer width
    pub int_width: usize,     //Default size for usize and isize
}

impl TargetSpec {
    pub fn new(
        arch: Option<String>,
        os: Option<String>,
        pointer_width: Option<usize>,
        int_width: Option<usize>,
    ) -> Self {
        let host_arch = ARCH.to_string();
        let host_os = OS.to_string();
        let host_word_size = std::mem::size_of::<usize>();

        TargetSpec {
            architecture: arch.unwrap_or(host_arch),
            os: os.unwrap_or(host_os),
            pointer_width: pointer_width.unwrap_or(host_word_size),
            int_width: int_width.unwrap_or(host_word_size),
        }
    }

    /// Returns an LLVM target triple string.
    pub fn llvm_triple(&self) -> String {
        let arch = match self.architecture.as_str() {
            "x86_64" | "x86-64" | "amd64" => "x86_64",
            "aarch64" | "arm64" => "aarch64",
            "i686" | "i586" | "i386" | "x86" => "i686",
            "arm" => "arm",
            "riscv64" => "riscv64",
            "wasm32" => "wasm32",
            "wasm64" => "wasm64",
            other => other, // pass through unknowns and hope LLVM knows them
        };

        let (vendor, sys, abi) = match self.os.as_str() {
            "linux" => ("unknown", "linux", "gnu"),
            "windows" => ("pc", "windows", "msvc"),
            "macos" | "darwin" => ("apple", "darwin", ""),
            "freebsd" => ("unknown", "freebsd", ""),
            "dragonfly" => ("unknown", "dragonfly", ""),
            "openbsd" => ("unknown", "openbsd", ""),
            "netbsd" => ("unknown", "netbsd", ""),
            "solaris" | "illumos" => ("unknown", "solaris", ""),
            "android" => ("unknown", "linux", "android"),
            "ios" => ("apple", "ios", ""),
            "emscripten" => ("unknown", "emscripten", ""),
            "wasi" => ("wasi", "wasm32", ""), // special case
            other => ("unknown", other, ""),
        };

        if abi.is_empty() {
            format!("{}-{}-{}", arch, vendor, sys)
        } else {
            format!("{}-{}-{}-{}", arch, vendor, sys, abi)
        }
    }
}
