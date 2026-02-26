#include "lexer.hpp"
#include "errors.hpp"
#include "token.hpp"
#include <iostream>
#include <string>
#include <unordered_map>

#define CAPTURE_POS                                                            \
  int tokenLine = line;                                                        \
  int tokenColumn = column;

Lexer::Lexer(const std::string &sourceCode, ErrorHandler &handler)
    : input(sourceCode), errorHandler(handler), currentPosition(0),
      nextPosition(1), line(1), column(0){};

size_t Lexer::getUTF8CharLength(size_t pos) {
  if (pos >= input.length())
    return 0;
  unsigned char firstByte = input[pos];
  if (firstByte < 0x80)
    return 1;
  else if ((firstByte & 0xE0) == 0xC0)
    return 2;
  else if ((firstByte & 0xF0) == 0xE0)
    return 3;
  else if ((firstByte & 0xF8) == 0xF0)
    return 4;
  else
    return 1;
}

char32_t Lexer::decodeUTF8(size_t pos) {
  if (pos >= input.length())
    return U'\0';

  unsigned char firstByte = input[pos];

  // ASCII fast path
  if (firstByte < 0x80)
    return firstByte;

  char32_t codePoint = 0;
  size_t charLength = getUTF8CharLength(pos);

  // Safety: do we have enough bytes to decode this character?
  if (pos + charLength > input.length()) {
    std::cout << "[decodeUTF8] Incomplete UTF-8 character at end of input\n";
    return U'\0';
  }

  // Start decoding
  if (charLength == 2)
    codePoint = (firstByte & 0x1F) << 6;
  else if (charLength == 3)
    codePoint = (firstByte & 0x0F) << 12;
  else if (charLength == 4)
    codePoint = (firstByte & 0x07) << 18;
  else {
    std::cout << "[decodeUTF8] Invalid UTF-8 charLength = " << charLength
              << "\n";
    return U'\0';
  }

  for (size_t i = 1; i < charLength; ++i) {
    unsigned char nextByte = input[pos + i];
    codePoint |= (nextByte & 0x3F) << (6 * (charLength - 1 - i));
  }

  return codePoint;
}

void Lexer::advance() {
  if (nextPosition >= input.length()) {
    currentPosition = input.length();
    nextPosition = input.length();
    return;
  }

  // Decode the character at the currentPosition before we advance
  char32_t ch = decodeUTF8(currentPosition);

  // If the character we are leaving is a newline, move to next line
  if (ch == U'\n') {
    line++;
    column = 1; // Start first char of next line at column 1
  } else {
    column++;
  }

  currentPosition = nextPosition;

  // Read current character's UTF-8 byte length
  size_t charLength = getUTF8CharLength(currentPosition);

  // Handle invalid UTF-8
  if (charLength == 0 || currentPosition + charLength > input.length()) {
    if (currentPosition < input.length()) {
      logError("Invalid UTF-8 character", line, column);
    }
    charLength = 1;
  }

  // Update next position
  nextPosition = currentPosition + charLength;
}

char32_t Lexer::peekChar() {
  if (nextPosition >= input.length())
    return U'\0';
  return decodeUTF8(nextPosition);
}

char32_t Lexer::currentChar() {
  if (currentPosition >= input.length()) {
    return U'\0';
  }
  return decodeUTF8(currentPosition);
}

void Lexer::skipWhiteSpace() {
  while (true) {
    char32_t ch = currentChar();
    while (ch == U' ' || ch == U'\n' || ch == U'\t' || ch == U'\r' ||
           ch == U'\u00A0') {
      advance();
      ch = currentChar();
    }

    if (currentChar() == U'#') {
      readComments();
      continue;
    }
    break;
  }
}

bool Lexer::isHexDigit(char32_t ch) {
  return (ch >= U'0' && ch <= U'9') || (ch >= U'a' && ch <= U'f') ||
         (ch >= U'A' && ch <= U'F');
}

bool Lexer::isBinaryDigit(char32_t ch) { return (ch == U'0' || ch == U'1'); }

bool Lexer::isAlpha(char32_t ch) {
  return (ch >= U'a' && ch <= U'z') || (ch >= U'A' && ch <= U'Z') ||
         (ch == U'_');
}

Token Lexer::readBinary() {
  CAPTURE_POS;
  std::string number = "0b";
  advance(); // skip '0'
  advance(); // skip 'b'

  while (isBinaryDigit(currentChar()) || currentChar() == U'_') {
    if (currentChar() != U'_') {
      number += static_cast<char>(currentChar());
    }
    advance();
  }

  return parseSuffix(number, tokenLine, tokenColumn);
}

Token Lexer::readHex() {
  CAPTURE_POS;
  std::string number = "0x";
  advance(); // skip '0'
  advance(); // skip 'x'

  while (isHexDigit(currentChar()) || currentChar() == U'_') {
    if (currentChar() != U'_') {
      number += static_cast<char>(currentChar());
    }
    advance();
  }

  return parseSuffix(number, tokenLine, tokenColumn);
}

Token Lexer::parseFloatSuffix(const std::string &value, int tokenLine,
                              int tokenColumn) {
  std::string suffix;
  while (isAlpha(currentChar()) || isDigit(currentChar())) {
    suffix += static_cast<char>(currentChar());
    advance();
  }

  if (suffix == "f64")
    return Token{value, TokenType::F64, tokenLine, tokenColumn};

  if (suffix == "f32")
    return Token{value, TokenType::F32, tokenLine, tokenColumn};

  // If no suffix was given default to f32
  return Token{value, TokenType::F32, tokenLine, tokenColumn};
}

Token Lexer::parseSuffix(const std::string &value, int tokenLine,
                         int tokenColumn) {
  std::string suffix;

  // Check if a suffix follows (e.g., u8, i32)
  while (isAlpha(currentChar()) || isDigit(currentChar())) {
    suffix += static_cast<char>(currentChar());
    advance();
  }

  if (suffix == "i64")
    return Token{value, TokenType::INT64, tokenLine, tokenColumn};
  if (suffix == "u64")
    return Token{value, TokenType::UINT64, tokenLine, tokenColumn};
  if (suffix == "i16")
    return Token{value, TokenType::INT16, tokenLine, tokenColumn};
  if (suffix == "u16")
    return Token{value, TokenType::UINT16, tokenLine, tokenColumn};
  if (suffix == "i128")
    return Token{value, TokenType::INT128, tokenLine, tokenColumn};
  if (suffix == "u128")
    return Token{value, TokenType::UINT128, tokenLine, tokenColumn};
  if (suffix == "u32")
    return Token{value, TokenType::UINT32, tokenLine, tokenColumn};
  if (suffix == "i8")
    return Token{value, TokenType::INT8, tokenLine, tokenColumn};
  if (suffix == "u8")
    return Token{value, TokenType::UINT8, tokenLine, tokenColumn};
  if (suffix == "iz")
    return Token{value, TokenType::INTSIZE, tokenLine, tokenColumn};
  if (suffix == "uz")
    return Token{value, TokenType::UINTSIZE, tokenLine, tokenColumn};

  // Fallback if no suffix is provided
  return Token{value, TokenType::INT32, tokenLine, tokenColumn};
}

Token Lexer::readNumbers() {
  std::string number;
  CAPTURE_POS;

  if (currentChar() == U'0') {
    char32_t next = peekChar();
    if (next == U'x' || next == U'X') {
      return readHex();
    } else if (next == U'b' || next == U'B') {
      return readBinary();
    }
  }

  // Consume all the digits first
  while (isDigit(currentChar())) {
    number += convertUnicodeDigit(currentChar());
    advance();
  }

  // Handle decimals
  if (currentChar() == U'.') {
    number += currentChar();
    advance();
    while (isDigit(currentChar())) {
      number += currentChar();
      advance();
    }
    return parseFloatSuffix(number, tokenLine, tokenColumn);
  }

  return parseSuffix(number, tokenLine, tokenColumn);
}

bool Lexer::isDigit(char32_t ch) {
  return (ch >= U'0' && ch <= U'9') || (ch >= U'٠' && ch <= U'۹') ||
         (ch >= U'０' && ch <= U'９');
}

char Lexer::convertUnicodeDigit(char32_t ch) {
  if (ch >= U'0' && ch <= U'9')
    return static_cast<char>(ch); // ASCII
  if (ch >= U'٠' && ch <= U'۹')
    return '0' + (ch - U'٠'); // Arabic-Indic
  if (ch >= U'０' && ch <= U'９')
    return '0' + (ch - U'０'); // Fullwidth
  return '0'; // Fallback (shouldn't happen due to isDigit check)
}

Token Lexer::readIdentifiers() {
  std::string identifier;
  CAPTURE_POS;
  while (isIdentifierStart(currentChar())) {
    appendUTF8(identifier, currentChar());
    advance();
  }

  while (isIdentifierContinue(currentChar())) {
    appendUTF8(identifier, currentChar());
    advance();
  }

  TokenType type =
      keywords.count(identifier) ? keywords[identifier] : TokenType::IDENTIFIER;
  return Token{identifier, type, tokenLine, tokenColumn};
};

bool Lexer::isIdentifierStart(char32_t ch) {
  return
      // Basic Latin
      isAlpha(ch) ||

      // Latin-based scripts
      (ch >= 0x00C0 && ch <= 0x00FF) || // Latin-1 Supplement
      (ch >= 0x0100 && ch <= 0x017F) || // Latin Extended-A
      (ch >= 0x0180 && ch <= 0x024F) || // Latin Extended-B
      (ch >= 0x0250 && ch <= 0x02AF) || // IPA Extensions
      (ch >= 0x1E00 && ch <= 0x1EFF) || // Latin Extended Additional

      // Greek, Cyrillic, Hebrew, Arabic, etc.
      (ch >= 0x0370 && ch <= 0x03FF) || // Greek and Coptic
      (ch >= 0x0400 && ch <= 0x04FF) || // Cyrillic
      (ch >= 0x0530 && ch <= 0x058F) || // Armenian
      (ch >= 0x0590 && ch <= 0x05FF) || // Hebrew
      (ch >= 0x0600 && ch <= 0x06FF) || // Arabic
      (ch >= 0x0700 && ch <= 0x074F) || // Syriac
      (ch >= 0x0780 && ch <= 0x07BF) || // Thaana

      // Indian scripts
      (ch >= 0x0900 && ch <= 0x0DFF) || // Devanagari to Malayalam
      (ch >= 0x0E00 && ch <= 0x0E7F) || // Thai
      (ch >= 0x0E80 && ch <= 0x0EFF) || // Lao
      (ch >= 0x0F00 && ch <= 0x0FFF) || // Tibetan

      // Georgian and Hangul
      (ch >= 0x10A0 && ch <= 0x10FF) || // Georgian
      (ch >= 0x1100 && ch <= 0x11FF) || // Hangul Jamo

      // Chinese, Japanese, Korean
      (ch >= 0x3040 && ch <= 0x309F) || // Hiragana
      (ch >= 0x30A0 && ch <= 0x30FF) || // Katakana
      (ch >= 0x31F0 && ch <= 0x31FF) || // Katakana Phonetic Extensions
      (ch >= 0x4E00 && ch <= 0x9FFF) || // CJK Unified Ideographs (Chinese)
      (ch >= 0xAC00 && ch <= 0xD7AF) || // Hangul Syllables

      // Emoji and Symbols
      (ch >= 0x1F300 &&
       ch <= 0x1F5FF) || // Miscellaneous Symbols and Pictographs
      (ch >= 0x1F600 && ch <= 0x1F64F) || // Emoticons (Emoji)
      (ch >= 0x1F680 && ch <= 0x1F6FF) || // Transport and Map Symbols
      (ch >= 0x1F700 && ch <= 0x1F77F) || // Alchemical Symbols

      // Fullwidth compatibility
      (ch >= 0xFF00 && ch <= 0xFFEF) || // Fullwidth ASCII & Halfwidth Katakana

      (ch >= 0x3400 && ch <= 0x4DBF) ||   // CJK Unified Ideographs Extension A
      (ch >= 0x4E00 && ch <= 0x9FFF) ||   // CJK Unified Ideographs
      (ch >= 0x20000 && ch <= 0x2A6DF) || // CJK Unified Ideographs Extension B
      (ch >= 0x2A700 && ch <= 0x2B73F) || // CJK Unified Ideographs Extension C
      (ch >= 0x2B740 && ch <= 0x2B81F) || // CJK Unified Ideographs Extension D
      (ch >= 0x2B820 && ch <= 0x2CEAF) || // CJK Unified Ideographs Extension E
      (ch >= 0x2CEB0 && ch <= 0x2EBEF) || // CJK Unified Ideographs Extension F
      (ch >= 0x30000 && ch <= 0x3134F) || // CJK Unified Ideographs Extension G

      (ch >= 0xF900 && ch <= 0xFAFF) || // CJK Compatibility Ideographs
      (ch >= 0x2F800 &&
       ch <= 0x2FA1F); // CJK Compatibility Ideographs Supplement
}

bool Lexer::isIdentifierContinue(char32_t ch) {
  return isIdentifierStart(ch) || (ch >= U'0' && ch <= U'9') || (ch == U'\'') ||
         (ch == U'_');
}

void Lexer::readComments() {
  if (currentChar() == U'#') {
    advance();
    if (currentChar() == U'#') // Multi-line block
    {
      advance();
      while (currentChar() != U'\0') {
        if (currentChar() == U'#' && peekChar() == U'#') {
          advance();
          advance();
          return; // EXIT HERE
        }
        advance();
      }
    }
    // Single line comment
    while (currentChar() != U'\0' && currentChar() != U'\n') {
      advance();
    }
  }
}

Token Lexer::readString() {
  std::string value;
  CAPTURE_POS;
  advance(); // Skip opening quote

  while (currentChar() != U'\0' && currentChar() != U'\n') {
    if (currentChar() == U'"') {
      advance();
      return Token{value, TokenType::STRING, tokenLine, tokenColumn};
    }

    if (currentChar() == U'\\') {
      advance(); // Skip backslash
      char32_t escapedChar = currentChar();
      switch (escapedChar) {
      case U'n':
        value += '\n';
        break;
      case U't':
        value += '\t';
        break;
      case U'r':
        value += '\r';
        break;
      case U'\\':
        value += '\\';
        break;
      case U'\"':
        value += '\"';
        break;
      case U'\'':
        value += '\'';
        break;
      case U'0':
        value += '\0';
        break;
      default:
        logError("Invalid escape sequence", tokenLine, tokenColumn);
        return Token{"Invalid escape sequence", TokenType::ILLEGAL, tokenLine,
                     tokenColumn};
      }

      advance();
    } else {
      appendUTF8(value, currentChar());
    }
    advance(); // Single advance point for all paths
  }

  logError("Unterminated string", tokenLine, tokenColumn);
  return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
}

void Lexer::appendUTF8(std::string &str, char32_t ch) {
  if (ch <= 0x7F) { // ASCII (1 byte)
    str += static_cast<char>(ch);
  } else if (ch <= 0x7FF) { // 2 bytes
    str += static_cast<char>(0xC0 | ((ch >> 6) & 0x1F));
    str += static_cast<char>(0x80 | (ch & 0x3F));
  } else if (ch <= 0xFFFF) { // 3 bytes
    str += static_cast<char>(0xE0 | ((ch >> 12) & 0x0F));
    str += static_cast<char>(0x80 | ((ch >> 6) & 0x3F));
    str += static_cast<char>(0x80 | (ch & 0x3F));
  } else { // 4 bytes (e.g., emojis)
    str += static_cast<char>(0xF0 | ((ch >> 18) & 0x07));
    str += static_cast<char>(0x80 | ((ch >> 12) & 0x3F));
    str += static_cast<char>(0x80 | ((ch >> 6) & 0x3F));
    str += static_cast<char>(0x80 | (ch & 0x3F));
  }
}

char32_t Lexer::readHexEscape(int digits) {
  std::string hexStr;
  for (int i = 0; i < digits; ++i) {
    hexStr += static_cast<char>(currentChar());
    advance();
  }
  try {
    return static_cast<char32_t>(std::stoul(hexStr, nullptr, 16));
  } catch (...) {
    return 0;
  }
}

Token Lexer::readChar() {
  CAPTURE_POS;

  // Step 1: Check for prefix
  char32_t prefix = 0;
  if (currentChar() == U'u' || currentChar() == U'U') {
    prefix = currentChar();
    advance();
  }

  // Step 2: Expect opening single quote
  if (currentChar() != U'\'') {
    logError("Expected opening single quote for char literal", tokenLine,
             tokenColumn);
    return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
  }
  advance(); // Skip opening quote

  std::string charValue; // UTF-8 encoded char

  // Step 3: Read char or escape sequence
  if (currentChar() == U'\\') {
    advance(); // Skip backslash
    char32_t escapedChar = currentChar();
    char32_t unescapedChar;

    switch (escapedChar) {
    case U'n':
      unescapedChar = U'\n';
      break;
    case U't':
      unescapedChar = U'\t';
      break;
    case U'r':
      unescapedChar = U'\r';
      break;
    case U'0':
      unescapedChar = U'\0';
      break;
    case U'\'':
      unescapedChar = U'\'';
      break;
    case U'\"':
      unescapedChar = U'\"';
      break;
    case U'\\':
      unescapedChar = U'\\';
      break;
    case U'u':
      advance(); // move past 'u'
      unescapedChar = readHexEscape(4);
      break;
    case U'U':
      advance(); // move past 'U'
      unescapedChar = readHexEscape(8);
      break;
    default:
      logError("Invalid escape sequence", tokenLine, tokenColumn);
      return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
    }

    appendUTF8(charValue, unescapedChar);
    advance();
  } else {
    if (currentChar() == U'\'' || currentChar() == U'\n' ||
        currentChar() == U'\0') {
      logError("Empty or invalid character", tokenLine, tokenColumn);
      return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
    }

    appendUTF8(charValue, currentChar());
    advance();
  }

  // Step 4: Expect closing quote
  if (currentChar() != U'\'') {
    logError("Missing closing quote", tokenLine, tokenColumn);
    return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
  }
  advance(); // Skip closing quote

  // Step 5: Determine token type based on prefix
  TokenType type = TokenType::CHAR8; // default 8-bit char
  switch (prefix) {
  case U'u':
    type = TokenType::CHAR16;
    break;
  case U'U':
    type = TokenType::CHAR32;
    break;
  default:
    type = TokenType::CHAR8;
    break;
  }

  return Token{charValue, type, tokenLine, tokenColumn};
}

Token Lexer::tokenize() {
  skipWhiteSpace();
  char32_t character = currentChar();

  if (isDigit(character)) {
    return readNumbers();
  } else if (isIdentifierStart(character)) {
    if ((character == U'u' || character == U'U') && peekChar() == U'\'') {
      return readChar();
    }
    return readIdentifiers();
  }
  switch (character) {
  case U'=': {
    CAPTURE_POS;
    if (peekChar() == U'=') {
      advance();
      advance();
      return Token{"==", TokenType::EQUALS, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"=", TokenType::ASSIGN, tokenLine, tokenColumn};
    }
  }
  case U'!': {
    CAPTURE_POS;
    if (peekChar() == U'=') {
      advance();
      advance();
      return Token{"!=", TokenType::NOT_EQUALS, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"!", TokenType::BANG, tokenLine, tokenColumn};
    }
  }
  case U'@': {
    CAPTURE_POS;
    advance();
    return Token{"@", TokenType::AT, tokenLine, tokenColumn};
  }
  case U'?': {
    CAPTURE_POS;
    if (peekChar() == U'?') {
      advance();
      advance();
      return Token{"??", TokenType::COALESCE, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"?", TokenType::QUESTION_MARK, tokenLine, tokenColumn};
    }
  }
  case U'.': {
    CAPTURE_POS;
    advance();
    return Token{".", TokenType::FULLSTOP, tokenLine, tokenColumn};
  }
  case U'+': {
    CAPTURE_POS;
    if (peekChar() == U'+') {
      advance();
      advance();
      return Token{"++", TokenType::PLUS_PLUS, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"+", TokenType::PLUS, tokenLine, tokenColumn};
    }
  }
  case U'-': {
    CAPTURE_POS;
    if (peekChar() == U'-') {
      advance();
      advance();
      return Token{"--", TokenType::MINUS_MINUS, tokenLine, tokenColumn};
    } else if (peekChar() == U'>') {
      advance();
      advance();
      return Token{"->", TokenType::ARROW, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"-", TokenType::MINUS, tokenLine, tokenColumn};
    }
  }
  case U'*': {
    CAPTURE_POS;
    advance();
    return Token{"*", TokenType::ASTERISK, tokenLine, tokenColumn};
  }
  case U'/': {
    CAPTURE_POS;
    advance();
    return Token{"/", TokenType::DIVIDE, tokenLine, tokenColumn};
  }
  case U'%': {
    CAPTURE_POS;
    advance();
    return Token{"%", TokenType::MODULUS, tokenLine, tokenColumn};
  }
  case U'&': {
    CAPTURE_POS;
    if (peekChar() == U'&') {
      advance();
      advance();
      return Token{"&&", TokenType::AND, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"&", TokenType::BITWISE_AND, tokenLine, tokenColumn};
    }
  }
  case U'|': {
    CAPTURE_POS;
    if (peekChar() == U'|') {
      advance();
      advance();
      return Token{"||", TokenType::OR, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"|", TokenType::BITWISE_OR, tokenLine, tokenColumn};
    }
  }
  case U'^': {
    CAPTURE_POS;
    advance();
    return Token{"^", TokenType::BITWISE_XOR, tokenLine, tokenColumn};
  }
  case U'~': {
    CAPTURE_POS;
    advance();
    return Token{"~", TokenType::BITWISE_NOT, tokenLine, tokenColumn};
  }
  case '>': {
    CAPTURE_POS;
    if (peekChar() == U'>') {
      advance();
      advance();
      return Token{">>", TokenType::SHIFT_RIGHT, tokenLine, tokenColumn};
    } else if (peekChar() == U'=') {
      advance();
      advance();
      return Token{">=", TokenType::GT_OR_EQ, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{">", TokenType::GREATER_THAN, tokenLine, tokenColumn};
    }
  }
  case '<': {
    CAPTURE_POS;
    if (peekChar() == U'<') {
      advance();
      advance();
      return Token{"<<", TokenType::SHIFT_LEFT, tokenLine, tokenColumn};
    } else if (peekChar() == U'=') {
      advance();
      advance();
      return Token{"<=", TokenType::LT_OR_EQ, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{"<", TokenType::LESS_THAN, tokenLine, tokenColumn};
    }
  }
  case U'{': {
    CAPTURE_POS;
    advance();
    return Token{"{", TokenType::LBRACE, tokenLine, tokenColumn};
  }
  case U'}': {
    CAPTURE_POS;
    advance();
    return Token{"}", TokenType::RBRACE, tokenLine, tokenColumn};
  }
  case U'[': {
    CAPTURE_POS;
    advance();
    return Token{"[", TokenType::LBRACKET, tokenLine, tokenColumn};
  }
  case U']': {
    CAPTURE_POS;
    advance();
    return Token{"]", TokenType::RBRACKET, tokenLine, tokenColumn};
  }
  case U'(': {
    CAPTURE_POS;
    advance();
    return Token{"(", TokenType::LPAREN, tokenLine, tokenColumn};
  }
  case U')': {
    CAPTURE_POS;
    advance();
    return Token{")", TokenType::RPAREN, tokenLine, tokenColumn};
  }
  case U';': {
    CAPTURE_POS;
    advance();
    return Token{";", TokenType::SEMICOLON, tokenLine, tokenColumn};
  }
  case U',': {
    CAPTURE_POS;
    advance();
    return Token{",", TokenType::COMMA, tokenLine, tokenColumn};
  }
  case U':': {
    CAPTURE_POS;
    if (peekChar() == U':') {
      advance();
      advance();
      return Token{"::", TokenType::SCOPE_OPERATOR, tokenLine, tokenColumn};
    } else {
      advance();
      return Token{":", TokenType::COLON, tokenLine, tokenColumn};
    }
  }
  case U'"':
    return readString();
  case U'\'': {
    return readChar();
  }
  case U'\0': {
    CAPTURE_POS;
    return Token{"", TokenType::END, tokenLine, tokenColumn};
  }
  default: {
    CAPTURE_POS;
    std::string charStr;
    appendUTF8(charStr, character);
    errorHandler.addHint("This character is not supported");
    logError("Unexpected character: ", tokenLine, tokenColumn);
    advance();
    return Token{charStr, TokenType::ILLEGAL, tokenLine, tokenColumn};
  }
  }
};

void Lexer::updateTokenList() {
  token_list.clear();
  size_t safety_counter = 0;
  const size_t max_tokens = input.length() * 2 + 10; // Reasonable upper bound

  while (safety_counter++ < max_tokens) {
    Token tok = tokenize();
    token_list.push_back(tok);

    if (tok.type == TokenType::END) {
      return; // Exit when we get END token
    }
  }

  throw std::runtime_error("Lexer safety limit exceeded");
}

void Lexer::logError(const std::string &message, int line, int column) {
  hasFailed = true;
  CompilerError error;
  error.level = ErrorLevel::LEXER;
  error.line = line;
  error.col = column;
  error.message = message;
  error.hints = {};

  errorHandler.report(error);
}

bool Lexer::failed() { return hasFailed; }
