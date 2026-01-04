#pragma once
#include "errors.hpp"
#include "token.hpp"
#include <string>
#include <unordered_map>
#include <vector>

class Lexer {
  size_t currentPosition;
  size_t nextPosition;
  std::string input;
  int line = 1;
  int column = 0;
  std::string fileName;
  ErrorHandler errorHandler;

  std::unordered_map<std::string, TokenType> keywords = {
      {"auto", TokenType::AUTO},
      {"func", TokenType::FUNCTION},
      {"return", TokenType::RETURN},

      {"enum", TokenType::ENUM},
      {"class", TokenType::CLASS},

      {"component", TokenType::COMPONENT},
      {"self", TokenType::SELF},
      {"new", TokenType::NEW},
      {"record", TokenType::RECORD},
      {"behavior", TokenType::BEHAVIOR},
      {"use", TokenType::USE},
      {"init", TokenType::INIT},

      {"generic", TokenType::GENERIC},
      {"instantiate", TokenType::INSTANTIATE},
      {"as", TokenType::AS},

      {"if", TokenType::IF},
      {"else", TokenType::ELSE},
      {"elif", TokenType::ELSE_IF},
      {"while", TokenType::WHILE},
      {"for", TokenType::FOR},
      {"break", TokenType::BREAK},
      {"continue", TokenType::CONTINUE},

      {"switch", TokenType::SWITCH},
      {"case", TokenType::CASE},
      {"default", TokenType::DEFAULT},

      {"null", TokenType::NULLABLE},

      {"i8", TokenType::I8_KEYWORD},
      {"u8", TokenType::U8_KEYWORD},

      {"i16", TokenType::I16_KEYWORD},
      {"u16", TokenType::U16_KEYWORD},

      {"i32", TokenType::I32_KEYWORD},
      {"u32", TokenType::U32_KEYWORD},

      {"i64", TokenType::I64_KEYWORD},
      {"u64", TokenType::U64_KEYWORD},

      {"i128", TokenType::I128_KEYWORD},
      {"u128", TokenType::U128_KEYWORD},

      {"isize", TokenType::ISIZE_KEYWORD},
      {"usize", TokenType::USIZE_KEYWORD},

      {"char8", TokenType::CHAR8_KEYWORD},
      {"char16", TokenType::CHAR16_KEYWORD},
      {"char32", TokenType::CHAR32_KEYWORD},

      {"string", TokenType::STRING_KEYWORD},
      {"float", TokenType::FLOAT_KEYWORD},
      {"double", TokenType::DOUBLE_KEYWORD},
      {"void", TokenType::VOID},

      {"sizeof", TokenType::SIZEOF},

      {"true", TokenType::TRUE},
      {"false", TokenType::FALSE},
      {"bool", TokenType::BOOL_KEYWORD},
      {"arr", TokenType::ARRAY},

      {"const", TokenType::CONST},
      {"mut", TokenType::MUT},
      {"ref", TokenType::REF},
      {"ptr", TokenType::PTR},
      {"addr", TokenType::ADDR},
      {"deref", TokenType::DEREF},

      {"heap", TokenType::HEAP},
      {"dheap", TokenType::DHEAP},
      {"allocator", TokenType::ALLOCATOR},

      {"seal", TokenType::SEAL},
      {"shout", TokenType::SHOUT},
      {"unwrap", TokenType::UNWRAP},
      {"export", TokenType::EXPORT},

      {"import", TokenType::IMPORT},
      {"merge", TokenType::MERGE},
      {"link", TokenType::LINK},
      {"qualify", TokenType::QUALIFY}};

public:
  Lexer(const std::string &sourceCode, const std::string &fileName);
  Token tokenize();
  std::vector<Token> outputTokens;

  std::vector<Token> token_list;
  void updateTokenList();

private:
  std::vector<std::string> sourceLines;
  size_t getUTF8CharLength(size_t pos);
  char32_t decodeUTF8(size_t pos);
  void advance();
  void skipWhiteSpace();
  char32_t peekChar();
  char32_t currentChar();
  bool isDigit(char32_t ch);
  bool isAlpha(char32_t ch);
  void readComments();
  Token readNumbers();
  Token readBinary();
  Token readHex();
  Token parseSuffix(const std::string& value, int line, int col);
  bool isHexDigit(char32_t ch);
  bool isBinaryDigit(char32_t ch);
  char convertUnicodeDigit(char32_t ch);
  Token readIdentifiers();
  bool isIdentifierStart(char32_t ch);
  bool isIdentifierContinue(char32_t ch);
  Token readString();
  void appendUTF8(std::string &str, char32_t ch);
  Token readChar();
  void logError(const std::string &message, int line, int column);
};
