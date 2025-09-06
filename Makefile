CXX = g++
CXXFLAGS = -std=c++17 -g -I. -Ilexer -Iparser -Itoken -Isemantic_analyzer
LLVM_CXXFLAGS = $(shell llvm-config --cxxflags | sed 's/-std=c++[0-9]*//g;s/-fno-exceptions//g')
LLVM_LDFLAGS = $(shell llvm-config --ldflags --system-libs --libs core irreader support analysis transformutils bitwriter)

OUT = unnc

SRC = main.cpp \
      lexer/lexer.cpp \
      parser/parser.cpp \
      parser/extended_parser.cpp \
      token/token.cpp \
      semantics/core.cpp \
      semantics/components.cpp \
      semantics/flow.cpp \
      semantics/expr.cpp \
      semantics/vars.cpp \
      semantics/fp.cpp\
      static/static.cpp\
      irgen/irgen.cpp\
      irgen/componentIR.cpp

$(OUT): $(SRC)
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(SRC) -o $(OUT) $(LLVM_LDFLAGS)

run: $(OUT)
	./$(OUT) test.unn

clean:
	rm -f $(OUT)