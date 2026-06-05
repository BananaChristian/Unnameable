# ___Compiler & Tools_____
CXX   = g++
CC    = gcc
NASM  = nasm
AR    = ar

# ____Target Configuration (For PAL isolation)____
ARCH           ?= x86_64
OS_TARGET      ?= linux

# ____Pathing______
BIN_DIR        = bin
BUILD_DIR      = build

# --- Core is platform-independent
CORE_DIR       = $(BUILD_DIR)/core

# --- PAL matches source hierarchy build/pal/ARCH/OS/
PAL_BUILD_DIR  = $(BUILD_DIR)/pal/$(ARCH)/$(OS_TARGET)

#--- Library source paths
LIB_SRC              = lib
URC_SRC              = $(LIB_SRC)/core
PAL_SRC              = $(LIB_SRC)/pal
PAL_PLATFORM_SRC     = $(PAL_SRC)/$(ARCH)/$(OS_TARGET)

# ____Unnameable Compiler____
UNNC = ./$(OUT)

# _____Compiler Flags______
CXXFLAGS = -std=c++17 -Wall -Wno-trigraphs -g -Iinclude
HEADERS  = $(wildcard include/**/*.hpp) $(wildcard include/*.hpp) $(wildcard include/**/*.h) $(wildcard include/*.h)

# ______Runtime Flags_________
RUNTIME_CXXFLAGS   = -std=c++17 -g -ffreestanding -nostdlib -fno-stack-protector -fno-exceptions
RUNTIME_CFLAGS     = -std=c11 -g -ffreestanding -nostdlib -fno-stack-protector
ASFLAGS            = -f elf64

# ________LLVM Integration___________
LLVM_CXXFLAGS = $(shell llvm-config --cxxflags | sed 's/-std=c++[0-9]*//g;s/-fno-exceptions//g')
LLVM_LDFLAGS  = $(shell llvm-config --ldflags --system-libs --libs core irreader support analysis transformutils bitwriter)

# _____Sanity Checks___________
SAN_FLAGS = -fsanitize=address,undefined

# ______Output Strategy______
OUT  = $(BIN_DIR)/unnc
SRC  = $(wildcard src/**/*.cpp) $(wildcard src/*.cpp)

# ______URC Outputs______
URC_LIB   = $(CORE_DIR)/urc.a
CORE_OBJS = $(CORE_DIR)/divmod.o \
            $(CORE_DIR)/unnitoa.o \
            $(CORE_DIR)/unnftoa.o \
            $(CORE_DIR)/unniptoa.o \
            $(CORE_DIR)/unn_strcat.o

# ______PAL Outputs______
PAL_LIB   = $(PAL_BUILD_DIR)/pal.a
PAL_OBJS  = $(PAL_BUILD_DIR)/syscalls.o \
            $(PAL_BUILD_DIR)/interp.o

# TARGETS
all: $(OUT) core pal

# _____Build the Compiler Executable_______
$(OUT): $(SRC) $(HEADERS)
	@mkdir -p $(BIN_DIR)
ifdef SANITIZE
	@echo "Building with ASAN/UBSAN..."
	$(CXX) $(CXXFLAGS) $(SAN_FLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)
else
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)
	@echo "Compiler built at $(OUT)"
endif

# _____Parser and Lexer Test Benches______
TEST_SRC = parser_test/parser_test.cpp \
           src/token/token.cpp \
           src/lexer/lexer.cpp \
           src/parser/*.cpp \
           src/errors/errors.cpp

TEST_OUT = $(BIN_DIR)/unnc_test

test: $(TEST_OUT)

$(TEST_OUT): $(TEST_SRC) $(HEADERS)
	@mkdir -p $(BIN_DIR)
	$(CXX) $(CXXFLAGS) $(TEST_SRC) -o $(TEST_OUT)
	@echo "Test binary built at $(TEST_OUT)"

parser_test: test
	$(TEST_OUT) tests/test.unn

# _____Build the Core archive (Platform Agnostic)______
core: $(CORE_OBJS)
	@mkdir -p $(CORE_DIR)
	@echo "Bundling core objects into $(CORE_DIR)..."
	$(AR) rcs $(URC_LIB) $(CORE_OBJS)
	@echo "Core Library built: $(URC_LIB)"

#____________Building PAL Architecture (Platform Specific Nested Path)_______________
pal: $(PAL_BUILD_DIR)/entry.o $(PAL_OBJS)
	@mkdir -p $(PAL_BUILD_DIR)
	@echo "Bundling platform abstraction layer into $(PAL_LIB)..."
	$(AR) rcs $(PAL_LIB) $(PAL_OBJS)
	@echo "PAL Library built: $(PAL_LIB) (entry.o kept standalone)"

$(PAL_BUILD_DIR)/entry.o: $(PAL_PLATFORM_SRC)/entry.asm
	@mkdir -p $(PAL_BUILD_DIR)
	$(NASM) $(ASFLAGS) $< -o $@

$(PAL_BUILD_DIR)/syscalls.o: $(PAL_PLATFORM_SRC)/syscalls.asm
	@mkdir -p $(PAL_BUILD_DIR)
	$(NASM) $(ASFLAGS) $< -o $@

$(PAL_BUILD_DIR)/interp.o: $(PAL_PLATFORM_SRC)/interp.asm
	@mkdir -p $(PAL_BUILD_DIR)
	$(NASM) $(ASFLAGS) $< -o $@


#_________________Building the core (urc) objects____________
$(CORE_DIR)/divmod.o: $(URC_SRC)/divmod.c
	@mkdir -p $(CORE_DIR)
	$(CC) $(RUNTIME_CFLAGS) -c $< -o $@

$(CORE_DIR)/unnitoa.o: $(URC_SRC)/unnitoa.unn $(OUT)
	@mkdir -p $(CORE_DIR)
	$(UNNC) $< -compile $@ -freestanding

$(CORE_DIR)/unnftoa.o: $(URC_SRC)/unnftoa.unn $(OUT)
	@mkdir -p $(CORE_DIR)
	$(UNNC) $< -compile $@ -freestanding

$(CORE_DIR)/unniptoa.o: $(URC_SRC)/unniptoa.unn $(OUT)
	@mkdir -p $(CORE_DIR)
	$(UNNC) $< -compile $@ -freestanding

$(CORE_DIR)/unn_strcat.o: $(URC_SRC)/unn_strcat.unn $(OUT)
	@mkdir -p $(CORE_DIR)
	$(UNNC) $< -compile $@ -freestanding


# _____Utility Targets______
run: $(OUT) core pal
	$(UNNC) tests/test.unn -verbose

clean:
	rm -rf $(BIN_DIR) $(BUILD_DIR)
	@echo "Cleaned build artifacts."

.PHONY: all core pal clean run test parser_test
