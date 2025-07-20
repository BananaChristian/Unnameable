#include "token/token.hpp"
#include "lexer.hpp"
#include <iostream>
#include <unordered_map>
#include <string>

#define CAPTURE_POS       \
    int tokenLine = line; \
    int tokenColumn = column;

Lexer::Lexer(const std::string &sourceCode) : input(sourceCode), currentPosition(0), nextPosition(1), line(1), column(0) {};

size_t Lexer::getUTF8CharLength(size_t pos)
{
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

char32_t Lexer::decodeUTF8(size_t pos)
{
    if (pos >= input.length())
        return U'\0';

    unsigned char firstByte = input[pos];

    // ASCII fast path
    if (firstByte < 0x80)
        return firstByte;

    char32_t codePoint = 0;
    size_t charLength = getUTF8CharLength(pos);

    // Safety: do we have enough bytes to decode this character?
    if (pos + charLength > input.length())
    {
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
    else
    {
        std::cout << "[decodeUTF8] Invalid UTF-8 charLength = " << charLength << "\n";
        return U'\0';
    }

    for (size_t i = 1; i < charLength; ++i)
    {
        unsigned char nextByte = input[pos + i];
        codePoint |= (nextByte & 0x3F) << (6 * (charLength - 1 - i));
    }

    return codePoint;
}

void Lexer::advance()
{
    if (nextPosition >= input.length())
    {
        currentPosition = input.length();
        nextPosition = input.length();
        return;
    }

    currentPosition = nextPosition;

    // Read current character's UTF-8 byte length
    size_t charLength = getUTF8CharLength(currentPosition);

    // Handle invalid UTF-8
    if (charLength == 0 || currentPosition + charLength > input.length())
    {
        logError("Invalid UTF-8 character", line, column);
        charLength = 1; // Skip 1 byte to avoid infinite loops
    }

    // Update next position
    nextPosition = currentPosition + charLength;

    // Decode character and update column/line
    char32_t ch = decodeUTF8(currentPosition);
    if (ch == U'\n')
    {
        line++;
        column = 0;
    }
    else
    {
        column++;
    }
}

char32_t Lexer::peekChar()
{
    if (nextPosition >= input.length())
        return U'\0';
    return decodeUTF8(nextPosition);
}

char32_t Lexer::currentChar()
{
    if (currentPosition >= input.length())
    {
        return U'\0';
    }
    return decodeUTF8(currentPosition);
}

void Lexer::skipWhiteSpace()
{
    while (true)
    {
        char32_t ch = currentChar();
        while (ch == U' ' || ch == U'\n' || ch == U'\t' || ch == U'\r' || ch == U'\u00A0')
        {
            advance();
            ch = currentChar();
        }

        if (currentChar() == U'#')
        {
            readComments();
            continue;
        }
        break;
    }
}

Token Lexer::readNumbers()
{
    std::string number;
    CAPTURE_POS;

    while (isDigit(currentChar()))
    {
        number += convertUnicodeDigit(currentChar());
        advance();
        if (currentChar() == U'.')
        {
            number += currentChar();
            advance();
            while (isDigit(currentChar()))
            {
                number += currentChar();
                advance();
            }
            return Token{number, TokenType::FLOAT, tokenLine, tokenColumn};
        }
    }
    return Token{number, TokenType::INTEGER, tokenLine, tokenColumn};
}

bool Lexer::isDigit(char32_t ch)
{
    return (ch >= U'0' && ch <= U'9') || (ch >= U'٠' && ch <= U'۹') || (ch >= U'０' && ch <= U'９');
}

char Lexer::convertUnicodeDigit(char32_t ch)
{
    if (ch >= U'0' && ch <= U'9')
        return static_cast<char>(ch); // ASCII
    if (ch >= U'٠' && ch <= U'۹')
        return '0' + (ch - U'٠'); // Arabic-Indic
    if (ch >= U'０' && ch <= U'９')
        return '0' + (ch - U'０'); // Fullwidth
    return '0';                    // Fallback (shouldn't happen due to isDigit check)
}

Token Lexer::readIdentifiers()
{
    std::string identifier;
    CAPTURE_POS;
    while (isIdentifierStart(currentChar()))
    {
        appendUTF8(identifier, currentChar());
        advance();
    }

    while (isIdentifierContinue(currentChar()))
    {
        appendUTF8(identifier, currentChar());
        advance();
    }

    TokenType type = keywords.count(identifier) ? keywords[identifier] : TokenType::IDENTIFIER;
    return Token{identifier, type, tokenLine, tokenColumn};
};

bool Lexer::isIdentifierStart(char32_t ch)
{
    return
        // Basic Latin
        (ch >= U'A' && ch <= U'Z') ||
        (ch >= U'a' && ch <= U'z') ||
        (ch == U'_') ||

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
        (ch >= 0x1F300 && ch <= 0x1F5FF) || // Miscellaneous Symbols and Pictographs
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
        (ch >= 0x2F800 && ch <= 0x2FA1F); // CJK Compatibility Ideographs Supplement
}

bool Lexer::isIdentifierContinue(char32_t ch)
{
    return isIdentifierStart(ch) || (ch >= U'0' && ch <= U'9') || (ch == U'\'');
}

void Lexer::readComments()
{
    if (currentChar() == U'#')
    {
        advance();
        while (currentChar() != U'\0' && currentChar() != U'\n')
        {
            advance();
        }
    }
}

Token Lexer::readString()
{
    std::string value;
    CAPTURE_POS;
    advance(); // Skip opening quote

    while (currentChar() != U'\0' && currentChar() != U'\n')
    {
        if (currentChar() == U'"')
        {
            advance();
            return Token{value, TokenType::STRING, tokenLine, tokenColumn};
        }

        if (currentChar() == U'\\')
        {
            advance(); // Skip backslash
            char32_t escapedChar = currentChar();
            switch (escapedChar)
            {
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
                return Token{"Invalid escape sequence", TokenType::ILLEGAL, tokenLine, tokenColumn};
            }

            advance();
        }
        else
        {
            appendUTF8(value, currentChar());
        }
        advance(); // Single advance point for all paths
    }

    logError("Unterminated string", tokenLine, tokenColumn);
    return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
}

void Lexer::appendUTF8(std::string &str, char32_t ch)
{
    if (ch <= 0x7F)
    { // ASCII (1 byte)
        str += static_cast<char>(ch);
    }
    else if (ch <= 0x7FF)
    { // 2 bytes
        str += static_cast<char>(0xC0 | ((ch >> 6) & 0x1F));
        str += static_cast<char>(0x80 | (ch & 0x3F));
    }
    else if (ch <= 0xFFFF)
    { // 3 bytes
        str += static_cast<char>(0xE0 | ((ch >> 12) & 0x0F));
        str += static_cast<char>(0x80 | ((ch >> 6) & 0x3F));
        str += static_cast<char>(0x80 | (ch & 0x3F));
    }
    else
    { // 4 bytes (e.g., emojis)
        str += static_cast<char>(0xF0 | ((ch >> 18) & 0x07));
        str += static_cast<char>(0x80 | ((ch >> 12) & 0x3F));
        str += static_cast<char>(0x80 | ((ch >> 6) & 0x3F));
        str += static_cast<char>(0x80 | (ch & 0x3F));
    }
}

Token Lexer::readChar()
{
    CAPTURE_POS;
    advance(); // Skip opening single quote

    std::string charValue; // Stores UTF-8 encoded char

    if (currentChar() == U'\\')
    {              // Escape sequence
        advance(); // Skip backslash
        char32_t escapedChar = currentChar();
        char32_t unescapedChar;

        switch (escapedChar)
        {
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
        default:
            logError("Invalid escape sequence", tokenLine, tokenColumn);
            return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
        }

        // Convert to UTF-8
        appendUTF8(charValue, unescapedChar);
        advance();
    }
    else
    {
        // Regular Unicode character
        if (currentChar() == U'\'' || currentChar() == U'\n' || currentChar() == U'\0')
        {
            logError("Empty or invalid character", tokenLine, tokenColumn);
            return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
        }

        appendUTF8(charValue, currentChar());
        advance();
    }

    // Expect closing quote
    if (currentChar() != U'\'')
    {
        logError("Missing closing quote", tokenLine, tokenColumn);
        return Token{"", TokenType::ILLEGAL, tokenLine, tokenColumn};
    }

    advance(); // Skip closing quote
    return Token{charValue, TokenType::CHAR, tokenLine, tokenColumn};
}

Token Lexer::tokenize()
{
    skipWhiteSpace();
    char32_t character = currentChar();

    if (isDigit(character))
    {
        return readNumbers();
    }
    else if (isIdentifierStart(character))
    {
        return readIdentifiers();
    }
    switch (character)
    {
    case U'=':
    {
        CAPTURE_POS;
        if (peekChar() == U'=')
        {
            advance();
            advance();
            return Token{"==", TokenType::EQUALS, tokenLine, tokenColumn};
        }
        else if (peekChar() == U'>')
        {
            advance();
            advance();
            return Token{"=>", TokenType::ARROW, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"=", TokenType::ASSIGN, tokenLine, tokenColumn};
        }
    }
    case U'!':
    {
        CAPTURE_POS;
        if (peekChar() == U'=')
        {
            advance();
            advance();
            return Token{"!=", TokenType::NOT_EQUALS, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"!", TokenType::BANG, tokenLine, tokenColumn};
        }
    }
    case U'.':
    {
        CAPTURE_POS;
        advance();
        return Token{".", TokenType::FULLSTOP, tokenLine, tokenColumn};
    }
    case U'+':
    {
        CAPTURE_POS;
        if (peekChar() == U'+')
        {
            advance();
            advance();
            return Token{"++", TokenType::PLUS_PLUS, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"+", TokenType::PLUS, tokenLine, tokenColumn};
        }
    }
    case U'-':
    {
        CAPTURE_POS;
        if (peekChar() == U'-')
        {
            advance();
            advance();
            return Token{"--", TokenType::MINUS_MINUS, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"-", TokenType::MINUS, tokenLine, tokenColumn};
        }
    }
    case U'*':
    {
        CAPTURE_POS;
        advance();
        return Token{"*", TokenType::ASTERISK, tokenLine, tokenColumn};
    }
    case U'/':
    {
        CAPTURE_POS;
        advance();
        return Token{"/", TokenType::DIVIDE, tokenLine, tokenColumn};
    }
    case U'&':
    {
        CAPTURE_POS;
        if (peekChar() == U'&')
        {
            advance();
            advance();
            return Token{"&&", TokenType::AND, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"&", TokenType::BITWISE_AND, tokenLine, tokenColumn};
        }
    }
    case U'|':
    {
        CAPTURE_POS;
        if (peekChar() == U'|')
        {
            advance();
            advance();
            return Token{"||", TokenType::OR, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"|", TokenType::BITWISE_OR, tokenLine, tokenColumn};
        }
    }
    case '>':
    {
        CAPTURE_POS;
        if (peekChar() == U'>')
        {
            advance();
            advance();
            return Token{">>", TokenType::SHIFT_RIGHT, tokenLine, tokenColumn};
        }
        else if (peekChar() == U'=')
        {
            advance();
            advance();
            return Token{">=", TokenType::GT_OR_EQ, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{">", TokenType::GREATER_THAN, tokenLine, tokenColumn};
        }
    }
    case '<':
    {
        CAPTURE_POS;
        if (peekChar() == U'<')
        {
            advance();
            advance();
            return Token{"<<", TokenType::SHIFT_LEFT, tokenLine, tokenColumn};
        }
        else if (peekChar() == U'=')
        {
            advance();
            advance();
            return Token{"<=", TokenType::LT_OR_EQ, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{"<", TokenType::LESS_THAN, tokenLine, tokenColumn};
        }
    }
    case U'{':
    {
        CAPTURE_POS;
        advance();
        return Token{"{", TokenType::LBRACE, tokenLine, tokenColumn};
    }
    case U'}':
    {
        CAPTURE_POS;
        advance();
        return Token{"}", TokenType::RBRACE, tokenLine, tokenColumn};
    }
    case U'[':
    {
        CAPTURE_POS;
        advance();
        return Token{"[", TokenType::LBRACKET, tokenLine, tokenColumn};
    }
    case U']':
    {
        CAPTURE_POS;
        advance();
        return Token{"]", TokenType::RBRACKET, tokenLine, tokenColumn};
    }
    case U'(':
    {
        CAPTURE_POS;
        advance();
        return Token{"(", TokenType::LPAREN, tokenLine, tokenColumn};
    }
    case U')':
    {
        CAPTURE_POS;
        advance();
        return Token{")", TokenType::RPAREN, tokenLine, tokenColumn};
    }
    case U';':
    {
        CAPTURE_POS;
        advance();
        return Token{";", TokenType::SEMICOLON, tokenLine, tokenColumn};
    }
    case U',':
    {
        CAPTURE_POS;
        advance();
        return Token{",", TokenType::COMMA, tokenLine, tokenColumn};
    }
    case U':':
    {
        CAPTURE_POS;
        if (peekChar() == U':')
        {
            advance();
            advance();
            return Token{"::", TokenType::SCOPE_OPERATOR, tokenLine, tokenColumn};
        }
        else
        {
            advance();
            return Token{":", TokenType::COLON, tokenLine, tokenColumn};
        }
    }
    case U'"':
        return readString();
    case U'\'':
    {
        return readChar();
    }
    case U'\0':
    {
        return Token{"", TokenType::END};
    }
    default:
    {
        CAPTURE_POS;
        std::string charStr;
        appendUTF8(charStr, character);
        logError("Unexpected character: ", tokenLine, tokenColumn);
        return Token{charStr, TokenType::ILLEGAL, tokenLine, tokenColumn};
    }
    }
};

void Lexer::updateTokenList()
{
    token_list.clear();
    size_t safety_counter = 0;
    const size_t max_tokens = input.length() * 2 + 10; // Reasonable upper bound

    while (safety_counter++ < max_tokens)
    {
        Token tok = tokenize();
        token_list.push_back(tok);

        if (tok.type == TokenType::END)
        {
            return; // Exit when we get END token
        }
    }

    throw std::runtime_error("Lexer safety limit exceeded");
}

void Lexer::logError(const std::string &message, int line, int column)
{
    std::cerr << "[TOKEN ERROR]: At line " << line << " column " << column << " : " << message << "\n";
}
