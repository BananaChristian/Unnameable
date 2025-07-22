CXX=g++
CXXFLAGS= -std=c++17 -g -I. -Ilexer -Iparser -Itoken -I semantic_analyzer

OUT=void

SRC=main.cpp\
	lexer/lexer.cpp\
	parser/parser.cpp\
	parser/extended_parser.cpp\
	token/token.cpp\
	semantics/core.cpp\
	semantics/flow.cpp\
	semantics/expr.cpp\
	semantics/vars.cpp

$(OUT):$(SRC)
	$(CXX) $(CXXFLAGS) $(SRC) -o $(OUT)

run: $(OUT)
	./$(OUT) test.unn

clean:
	rm -f $(OUT)
