# ___Compiler & Tools_____
CXX = g++
CC  = gcc
NASM = nasm

# ____Pathing______
BIN_DIR       = bin
OBJ_DIR       = runtime
CORE_DIR      = core
LIB_DIR       = lib
URC_X86_LINUX = $(LIB_DIR)/urc/architecture/x86_64/linux

# _____Compiler Flags______
CXXFLAGS = -std=c++17 -g \
    -Isrc -Isrc/lexer -Isrc/parser -Isrc/token -Isrc/semantics \
    -Isrc/irgen -Isrc/layout -Isrc/linker -Isrc/sentinel -Isrc/errors \
    -Isrc/stubgen -Isrc/deserializer -Iinclude 

# ______Runtime Flags_________
RUNTIME_CXXFLAGS   = -std=c++17 -g -ffreestanding -nostdlib -fno-stack-protector -fno-exceptions
RUNTIME_CFLAGS     = -std=c11 -g -ffreestanding -nostdlib -fno-stack-protector
ASFLAGS            = -f elf64

# ________LLVM Integration___________
LLVM_CXXFLAGS = $(shell llvm-config --cxxflags | sed 's/-std=c++[0-9]*//g;s/-fno-exceptions//g')
LLVM_LDFLAGS  = $(shell llvm-config --ldflags --system-libs --libs core irreader support analysis transformutils bitwriter)

# _____Sanity Checks___________
SAN_FLAGS = -fsanitize=address,undefined

# ______Executable Extension Logic__________
ifeq ($(OS),Windows_NT)
    EXE_EXT := .exe
else
    EXE_EXT :=
endif

OUT ?= $(BIN_DIR)/unnc$(EXE_EXT)
SRC = $(wildcard src/**/*.cpp) $(wildcard src/*.cpp)

# TARGETS

all: $(OUT) core

# _____Build the Compiler Executable_______
$(OUT): $(SRC)
	@mkdir -p $(BIN_DIR)
ifdef SANITIZE
	@echo "Building with ASAN/UBSAN..."
	$(CXX) $(CXXFLAGS) $(SAN_FLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)
else
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)
	@echo "Compiler built at $(OUT)"
endif

# _____Build the Core Objects (Dumps into ./core)______
core: $(CORE_DIR)/entry.o $(CORE_DIR)/syscalls.o $(CORE_DIR)/allocator.o $(CORE_DIR)/unnitoa.o
	@echo "All sovereign core objects dumped into $(CORE_DIR)/"

$(CORE_DIR)/entry.o: $(URC_X86_LINUX)/entry.asm
	@mkdir -p $(CORE_DIR)
	$(NASM) $(ASFLAGS) $< -o $@

$(CORE_DIR)/syscalls.o: $(URC_X86_LINUX)/syscalls.asm
	@mkdir -p $(CORE_DIR)
	$(NASM) $(ASFLAGS) $< -o $@

# (SAGE Allocator)
$(CORE_DIR)/allocator.o: $(LIB_DIR)/allocator/allocator.c
	@mkdir -p $(CORE_DIR)
	$(CC) $(RUNTIME_CFLAGS) -c $< -o $@

# (Unnitoa)
$(CORE_DIR)/unnitoa.o: $(LIB_DIR)/tools/unnitoa.cpp
	@mkdir -p $(CORE_DIR)
	$(CXX) $(RUNTIME_CXXFLAGS) -c $< -o $@

# _____Utility Targets______

run: $(OUT) core
ifeq ($(OS),Windows_NT)
	$(OUT) compiler_test/test.unn -verbose
else
	./$(OUT) compiler_test/test.unn -verbose
endif

clean:
	rm -rf $(BIN_DIR) $(CORE_DIR)/*.o $(OBJ_DIR)/*.o
	@echo "Cleaned build artifacts."

.PHONY: all core clean run