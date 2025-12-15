CXX = g++
CXXFLAGS = -std=c++17 -g \
    -Isrc -Isrc/lexer -Isrc/parser -Isrc/token -Isrc/semantics \
    -Isrc/irgen -Isrc/layout -Isrc/linker -Isrc/sentinel -Isrc/errors -Isrc/stubgen -Isrc/deserializer -Iinclude 

CC=gcc
RUNTIME_CXXFLAGS = -std=c++17 -g
RUNTIME_CFLAGS   = -std=c11 -g


SAN_FLAGS = -fsanitize=address,undefined

LLVM_CXXFLAGS = $(shell llvm-config --cxxflags | sed 's/-std=c++[0-9]*//g;s/-fno-exceptions//g')
LLVM_LDFLAGS   = $(shell llvm-config --ldflags --system-libs --libs core irreader support analysis transformutils bitwriter)

# Detect OS for executable extension
ifeq ($(OS),Windows_NT)
    EXE_EXT := .exe
else
    EXE_EXT :=
endif

# Default directories
BIN_DIR = bin
OBJ_DIR = runtime

# Compiler binary
OUT ?= $(BIN_DIR)/unnc$(EXE_EXT)

# All source files for compiler
SRC = $(wildcard src/**/*.cpp) $(wildcard src/*.cpp)

# Runtime source files
RUNTIME_SRC = allocator/allocator.c tools/helper.cpp
RUNTIME_OBJ = $(OBJ_DIR)/allocator.o $(OBJ_DIR)/helper.o

# Default target: build compiler
all: $(OUT)

# Compiler build (does NOT include runtime objects)
$(OUT): $(SRC)
	@mkdir -p $(BIN_DIR)
ifdef SANITIZE
	@echo "Building with ASAN/UBSAN..."
	$(CXX) $(CXXFLAGS) $(SAN_FLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)
else
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)
	@echo "Compiler built at $(OUT)"
endif

# Build runtime objects (allocator.o + helper.o)
runtime: $(OBJ_DIR)/allocator.o $(OBJ_DIR)/helper.o
	@echo "Runtime objects built in $(OBJ_DIR)/"

$(OBJ_DIR)/allocator.o: allocator/allocator.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(RUNTIME_CFLAGS) -c $< -o $@

$(OBJ_DIR)/helper.o: tools/helper.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(RUNTIME_CXXFLAGS) -c $< -o $@


# Run compiler on test file
run: $(OUT) runtime
ifeq ($(OS),Windows_NT)
	$(OUT) compiler_test/test.unn -verbose
else
	./$(OUT) compiler_test/test.unn -verbose
endif

# Compile a user's .unn file
# Usage: make compile FILE=main.unn FLAGS="-c -o main.o"
compile: runtime
ifeq ($(FILE),)
	$(error Please specify FILE, e.g., make compile FILE=main.unn FLAGS="-o main")
endif
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	$(OUT) $(FILE) $(FLAGS)

# Clean build
clean:
	rm -rf $(OUT) $(OBJ_DIR)/*.o
	@echo "Cleaned compiler"
