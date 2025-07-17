CXX=g++
CXXFLAGS= -std=c++17 -g -I. -Ilexer -Iparser -Itoken -I semantic_analyzer

OUT=void

SRC=main.cpp\
	lexer/lexer.cpp\
	parser/parser.cpp\
	parser/extended_parser.cpp\
	token/token.cpp\
	semantic_analyzer/semantics.cpp\
	semantic_analyzer/component_semantics.cpp

$(OUT):$(SRC)
	$(CXX) $(CXXFLAGS) $(SRC) -o $(OUT)

run: $(OUT)
	./$(OUT) test.unn

clean:
	rm -f $(OUT)
