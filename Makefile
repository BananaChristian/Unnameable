CXX = g++
CXXFLAGS = -std=c++17 -g -Isrc -Isrc/lexer -Isrc/parser -Isrc/token -Isrc/semantics -Isrc/irgen -Isrc/allocator -Isrc/layout -Isrc/sentinel -Iinclude

# Detect OS
ifeq ($(OS),Windows_NT)
    EXE_EXT := .exe
    # Set LLVM manually for Windows installer
    LLVM_INCLUDE_DIR ?= D:/LLVM/include
    LLVM_LIB_DIR ?= D:/LLVM/lib
    LLVM_LDFLAGS = -L$(LLVM_LIB_DIR) -lLLVM-18
    LLVM_CXXFLAGS = -I$(LLVM_INCLUDE_DIR)
else
    EXE_EXT :=
    # Linux: use llvm-config
    LLVM_CXXFLAGS = $(shell llvm-config --cxxflags | sed 's/-std=c++[0-9]*//g;s/-fno-exceptions//g')
    LLVM_LDFLAGS = $(shell llvm-config --ldflags --system-libs --libs core irreader support analysis transformutils bitwriter)
endif

# Default output locations
BIN_DIR = bin
OBJ_DIR = runtime
OUT ?= $(BIN_DIR)/unnc$(EXE_EXT)

# Source files
SRC = $(wildcard src/**/*.cpp) $(wildcard src/*.cpp)

# Runtime files (sage.o + helper.o)
RUNTIME_SRC = src/allocator/allocator.cpp src/tools/helper.cpp
RUNTIME_OBJ = $(OBJ_DIR)/sage.o $(OBJ_DIR)/helper.o

# Default target
all: runtime $(OUT)

# Build runtime objects
runtime:
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c src/allocator/allocator.cpp -o $(OBJ_DIR)/sage.o
	$(CXX) $(CXXFLAGS) -c src/tools/helper.cpp -o $(OBJ_DIR)/helper.o
	@echo "Runtime objects built in $(OBJ_DIR)/"

# Build compiler
$(OUT): $(SRC) $(RUNTIME_OBJ)
	@mkdir -p $(BIN_DIR)
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)
	@echo "Compiler built at $(OUT)"

# Run compiler on test file
run: $(OUT)
ifeq ($(OS),Windows_NT)
	$(OUT) tests/test.unn -verbose
else
	./$(OUT) tests/test.unn -verbose
endif

# Compile user's .unn file
# Usage: make compile FILE=main.unn FLAGS="-c -o test.o"
compile:
ifeq ($(FILE),)
	$(error Please specify source file: FILE=main.unn)
endif
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	$(OUT) $(FILE) $(FLAGS)

clean:
	rm -rf $(OUT) $(OBJ_DIR)/*.o
