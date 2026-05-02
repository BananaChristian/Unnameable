#pragma once
#include "token.hpp"
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

template <typename T>
std::unique_ptr<T> clonePtr(const std::unique_ptr<T> &ptr) {
  if (!ptr)
    return nullptr;
  return std::unique_ptr<T>(static_cast<T *>(ptr->shallowClone()));
}

template <typename T>
std::vector<std::unique_ptr<T>>
clonePtrVector(const std::vector<std::unique_ptr<T>> &vec) {
  std::vector<std::unique_ptr<T>> out;
  out.reserve(vec.size());
  for (auto &item : vec) {
    out.push_back(std::unique_ptr<T>(static_cast<T *>(item->shallowClone())));
  }
  return out;
}

enum class Mutability { IMMUTABLE, MUTABLE, CONSTANT };

// GENERAL AST NODE
struct Node {
  Token token;

  virtual ~Node() = default;

  virtual std::string toString() {
    std::ostringstream oss;
    oss << "Node: " << token.TokenLiteral;
    return oss.str();
  };

  virtual Node *shallowClone() const = 0;

  Node(Token t) : token(t) {}
};

// GENERAL EXPRESSION NODE
struct Expression : Node {
  Token expression;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Expression " << expression.TokenLiteral;
    return oss.str();
  }

  Expression *shallowClone() const override {
    return new Expression(expression);
  }

  Expression(Token expr) : Node(expr), expression(expr) {};
};

// GENERAL STATEMENT NODE
struct Statement : Node {
  Token statement;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Statement: " << statement.TokenLiteral;
    return oss.str();
  }

  Statement *shallowClone() const override { return new Statement(statement); }
  Statement(Token stmt) : Node(stmt), statement(stmt) {};
};

// Identifier statement node
struct Identifier : Expression {
  Token identifier;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Identifier: " << identifier.TokenLiteral;
    return oss.str();
  }

  Identifier *shallowClone() const override {
    return new Identifier(identifier);
  }

  Identifier(Token ident) : Expression(ident), identifier(ident) {};
};

// Address expression node
struct AddressExpression : Expression {
  Token addr_token;
  std::unique_ptr<Expression> identifier;

  std::string toString() override {
    std::ostringstream oss;
    oss << "AddressExpression: (" << addr_token.TokenLiteral << " "
        << (identifier ? identifier->toString() : "expr");
    return oss.str();
  }

  AddressExpression *shallowClone() const override {
    return new AddressExpression(addr_token, clonePtr(identifier));
  }

  AddressExpression(Token addr_t, std::unique_ptr<Expression> ident)
      : Expression(addr_t), addr_token(addr_t), identifier(std::move(ident)) {};
};

// Dereference expression node
struct DereferenceExpression : Expression {
  Token deref_token;
  std::unique_ptr<Expression> identifier;

  std::string toString() override {
    std::ostringstream oss;
    oss << "DereferenceExpression: (" << deref_token.TokenLiteral << " "
        << (identifier ? identifier->toString() : "expr");
    return oss.str();
  }

  DereferenceExpression *shallowClone() const override {
    return new DereferenceExpression(deref_token, clonePtr(identifier));
  }

  DereferenceExpression(Token deref, std::unique_ptr<Expression> ident)
      : Expression(deref), deref_token(deref), identifier(std::move(ident)) {}
};

// Self expression
struct SelfExpression : Expression {
  Token self_token; // e.g. self
  std::vector<std::unique_ptr<Expression>> fields;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Self Expression: " << self_token.TokenLiteral;
    for (const auto &field : fields) {
      oss << "." << (field ? field->toString() : "null");
    }
    return oss.str();
  }

  SelfExpression(Token self, std::vector<std::unique_ptr<Expression>> fieldExpr)
      : Expression(self), self_token(self), fields(std::move(fieldExpr)) {};
};

struct NewComponentExpression : Expression {
  Token new_token;      // token for 'new'
  Token component_name; // e.g. 'Player'
  std::vector<std::unique_ptr<Expression>> arguments;

  std::string toString() override {
    std::ostringstream oss;
    oss << "NewComponentExpression: new " << component_name.TokenLiteral << "(";
    for (auto &arg : arguments) {
      oss << (arg ? arg->toString() : "null") << ",";
    }
    oss << ")";
    return oss.str();
  }

  NewComponentExpression *shallowClone() const override {
    return new NewComponentExpression(new_token, component_name,
                                      clonePtrVector(arguments));
  }

  NewComponentExpression(Token newTok, Token compName,
                         std::vector<std::unique_ptr<Expression>> args)
      : Expression(newTok), new_token(newTok), component_name(compName),
        arguments(std::move(args)) {}
};

// Null literal
struct NullLiteral : Expression {
  Token null_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Null Literal: " << null_token.TokenLiteral;
    return oss.str();
  }

  NullLiteral *shallowClone() const override {
    return new NullLiteral(null_token);
  }

  NullLiteral(Token null_tok) : Expression(null_tok), null_token(null_tok) {};
};

// Signed 8 bit Integer literal
struct I8Literal : Expression {
  Token i8_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "i8 literal: " << i8_token.TokenLiteral;
    return oss.str();
  }

  I8Literal *shallowClone() const override { return new I8Literal(i8_token); }
  I8Literal(Token i8_t) : Expression(i8_t), i8_token(i8_t) {}
};

// Unsigned 8 bit Integer literal
struct U8Literal : Expression {
  Token u8_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "u8 literal: " << u8_token.TokenLiteral;
    return oss.str();
  }

  U8Literal *shallowClone() const override { return new U8Literal(u8_token); }
  U8Literal(Token u8_t) : Expression(u8_t), u8_token(u8_t) {}
};

// Signed 16 bit Integer literal
struct I16Literal : Expression {
  Token i16_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "i16 literal" << i16_token.TokenLiteral;
    return oss.str();
  }

  I16Literal *shallowClone() const override {
    return new I16Literal(i16_token);
  }
  I16Literal(Token i16_t) : Expression(i16_t), i16_token(i16_t) {};
};

// Unsigned 16 bit Ineteger literal
struct U16Literal : Expression {
  Token u16_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "u16 literal: " << u16_token.TokenLiteral;
    return oss.str();
  }
  U16Literal *shallowClone() const override {
    return new U16Literal(u16_token);
  }

  U16Literal(Token u16_t) : Expression(u16_t), u16_token(u16_t) {};
};

// Signed 32 bit Integer literal
struct I32Literal : Expression {
  Token i32_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "i32 literal: " + i32_token.TokenLiteral;
    return oss.str();
  }

  I32Literal *shallowClone() const override {
    return new I32Literal(i32_token);
  }
  I32Literal(Token i32_t) : Expression(i32_t), i32_token(i32_t) {};
};

// Unsigned 32 bit integer literal
struct U32Literal : Expression {
  Token u32_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "u32 literal: " << u32_token.TokenLiteral;
    return oss.str();
  }

  U32Literal *shallowClone() const override {
    return new U32Literal(u32_token);
  }
  U32Literal(Token u32_t) : Expression(u32_t), u32_token(u32_t) {};
};

// Signed 64 bit integer literal
struct I64Literal : Expression {
  Token i64_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "i64 literal: " << i64_token.TokenLiteral;
    return oss.str();
  }

  I64Literal *shallowClone() const override {
    return new I64Literal(i64_token);
  }
  I64Literal(Token i64_t) : Expression(i64_t), i64_token(i64_t) {};
};

// Unsigned 64 bit integer literal
struct U64Literal : Expression {
  Token u64_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "u64 literal: " << u64_token.TokenLiteral;
    return oss.str();
  }

  U64Literal *shallowClone() const override {
    return new U64Literal(u64_token);
  }

  U64Literal(Token u64_t) : Expression(u64_t), u64_token(u64_t) {};
};

// Signed 128 bit integer literal
struct I128Literal : Expression {
  Token i128_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "i128 literal: " << i128_token.TokenLiteral;
    return oss.str();
  }

  I128Literal *shallowClone() const override {
    return new I128Literal(i128_token);
  }
  I128Literal(Token i128_t) : Expression(i128_t), i128_token(i128_t) {};
};

// Unsigned 128 bit integer literal
struct U128Literal : Expression {
  Token u128_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "u128 literal: " << u128_token.TokenLiteral;
    return oss.str();
  }

  U128Literal *shallowClone() const override {
    return new U128Literal(u128_token);
  }
  U128Literal(Token u128_t) : Expression(u128_t), u128_token(u128_t) {};
};

struct ISIZELiteral : Expression {
  Token isize_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "isize literal: " << isize_token.TokenLiteral;
    return oss.str();
  }

  ISIZELiteral *shallowClone() const override {
    return new ISIZELiteral(isize_token);
  }

  ISIZELiteral(Token isize_t) : Expression(isize_t), isize_token(isize_t) {};
};

struct USIZELiteral : Expression {
  Token usize_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "usize literal: " << usize_token.TokenLiteral;
    return oss.str();
  }

  USIZELiteral *shallowClone() const override {
    return new USIZELiteral(usize_token);
  }

  USIZELiteral(Token usize_t) : Expression(usize_t), usize_token(usize_t) {};
};

struct INTLiteral : Expression {
  Token int_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "int literal: " << int_token.TokenLiteral;
    return oss.str();
  }

  INTLiteral *shallowClone() const override {
    return new INTLiteral(int_token);
  }

  INTLiteral(Token int_t) : Expression(int_t), int_token(int_t) {};
};

// Boolean literal
struct BooleanLiteral : Expression {
  Token boolean_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "bool literal: " << boolean_token.TokenLiteral;
    return oss.str();
  }

  BooleanLiteral *shallowClone() const override {
    return new BooleanLiteral(boolean_token);
  }
  BooleanLiteral(Token bool_t) : Expression(bool_t), boolean_token(bool_t) {};
};

// F32 literal
struct F32Literal : Expression {
  Token f32_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "f32 literal: " << f32_token.TokenLiteral;
    return oss.str();
  }

  F32Literal *shallowClone() const override {
    return new F32Literal(f32_token);
  }
  F32Literal(Token f32_t) : Expression(f32_t), f32_token(f32_t) {};
};

// F64 literal
struct F64Literal : Expression {
  Token f64_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "f64 literal: " << f64_token.TokenLiteral;
    return oss.str();
  }

  F64Literal *shallowClone() const override {
    return new F64Literal(f64_token);
  }
  F64Literal(Token f64_t) : Expression(f64_t), f64_token(f64_t) {};
};

// Generic Float literal
struct FloatLiteral : Expression {
  Token float_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "float literal: " << float_token.TokenLiteral;
    return oss.str();
  }

  FloatLiteral *shallowClone() const override {
    return new FloatLiteral(float_token);
  }

  FloatLiteral(Token flot_tok) : Expression(flot_tok), float_token(flot_tok) {};
};

// 8 bit Char literal
struct Char8Literal : Expression {
  Token char8_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "char8 literal: " << char8_token.TokenLiteral;
    return oss.str();
  }

  Char8Literal *shallowClone() const override {
    return new Char8Literal(char8_token);
  }
  Char8Literal(Token char8_tok)
      : Expression(char8_tok), char8_token(char8_tok) {};
};

// 16 bit Char literal
struct Char16Literal : Expression {
  Token char16_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "char16 literal: " << char16_token.TokenLiteral;
    return oss.str();
  }

  Char16Literal *shallowClone() const override {
    return new Char16Literal(char16_token);
  }
  Char16Literal(Token char16t) : Expression(char16t), char16_token(char16t) {};
};

// 32 bit Char literal
struct Char32Literal : Expression {
  Token char32_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "char32 literal: " << char32_token.TokenLiteral;
    return oss.str();
  }

  Char32Literal *shallowClone() const override {
    return new Char32Literal(char32_token);
  }
  Char32Literal(Token char32t) : Expression(char32t), char32_token(char32t) {};
};

// String literal
struct StringLiteral : Expression {
  Token string_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "string literal: " << string_token.TokenLiteral;
    return oss.str();
  }

  StringLiteral *shallowClone() const override {
    return new StringLiteral(string_token);
  }
  StringLiteral(Token string_t)
      : Expression(string_t), string_token(string_t) {};
};

struct FStringSegment {
  std::unique_ptr<Expression> string_part;
  std::vector<std::unique_ptr<Expression>> values;
};

struct FStringLiteral : Expression {
  Token f_token;
  std::vector<FStringSegment> segments;

  std::string toString() override {
    std::ostringstream oss;
    oss << "F-String Literal: \"";

    for (const auto &seg : segments) {
      // Handle the String part (The text context)
      if (seg.string_part) {
        oss << seg.string_part->toString();
      }

      // Handle the Expression Payload (The { } stuff)
      if (!seg.values.empty()) {
        oss << "{";
        for (size_t i = 0; i < seg.values.size(); ++i) {
          if (seg.values[i]) {
            oss << seg.values[i]->toString();
          } else {
            oss << "null";
          }
          if (i < seg.values.size() - 1) {
            oss << ", ";
          }
        }
        oss << "}";
      }
    }

    oss << "\"";
    return oss.str();
  }

  FStringLiteral *shallowClone() const override {
    std::vector<FStringSegment> clonedSegs;
    for (const auto &seg : segments) {
      FStringSegment newSeg;
      if (seg.string_part)
        newSeg.string_part =
            std::unique_ptr<Expression>(seg.string_part->shallowClone());

      for (const auto &val : seg.values) {
        newSeg.values.push_back(
            std::unique_ptr<Expression>(val->shallowClone()));
      }
      clonedSegs.push_back(std::move(newSeg));
    }
    return new FStringLiteral(token, std::move(clonedSegs));
  }

  FStringLiteral(Token t, std::vector<FStringSegment> segs)
      : Expression(t), segments(std::move(segs)) {}
};

// Size of expression
struct SizeOfExpression : Expression {
  Token sizeOf;
  std::unique_ptr<Expression> type;

  std::string toString() override { return ""; }

  SizeOfExpression *shallowClone() const override {
    return new SizeOfExpression(sizeOf, clonePtr(type));
  }

  SizeOfExpression(Token token, std::unique_ptr<Expression> ty)
      : Expression(token), sizeOf(token), type(std::move(ty)) {};
};

// Cast expression
struct CastExpression : Expression {
  Token cast;
  std::unique_ptr<Expression> type;
  std::unique_ptr<Expression> expr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Cast Expression: " << cast.TokenLiteral << "<"
        << (type ? type->toString() : "no type") << ">("
        << (expr ? expr->toString() : "No expr") << ")";
    return oss.str();
  }

  CastExpression *shallowClone() const override {
    return new CastExpression(cast, clonePtr(type), clonePtr(expr));
  }

  CastExpression(Token c, std::unique_ptr<Expression> t,
                 std::unique_ptr<Expression> e)
      : Expression(c), cast(c), type(std::move(t)), expr(std::move(e)) {}
};

// Bitcast expression
struct BitcastExpression : Expression {
  Token bitcast;
  std::unique_ptr<Expression> type;
  std::unique_ptr<Expression> expr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Bitcast Expression: " << bitcast.TokenLiteral << "<"
        << (type ? type->toString() : "no type") << ">("
        << (expr ? expr->toString() : "No expr") << ")";
    return oss.str();
  }

  BitcastExpression *shallowClone() const override {
    return new BitcastExpression(bitcast, clonePtr(type), clonePtr(expr));
  }

  BitcastExpression(Token bit, std::unique_ptr<Expression> t,
                    std::unique_ptr<Expression> e)
      : Expression(bit), bitcast(bit), type(std::move(t)), expr(std::move(e)) {}
};

// Call expression
struct CallExpression : Expression {
  std::unique_ptr<Expression> function_identifier;
  std::vector<std::unique_ptr<Expression>> parameters;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Call Expression: "
        << (function_identifier ? function_identifier->toString() : "null")
        << "(";

    for (size_t i = 0; i < parameters.size(); ++i) {
      oss << (parameters[i] ? parameters[i]->toString() : "null");
      if (i < parameters.size() - 1) {
        oss << ", ";
      }
    }

    oss << ")";
    return oss.str();
  }

  CallExpression *shallowClone() const override {
    return new CallExpression(function_identifier->expression,
                              clonePtr(function_identifier),
                              clonePtrVector(parameters));
  }

  CallExpression(Token tok, std::unique_ptr<Expression> fn_ident,
                 std::vector<std::unique_ptr<Expression>> params)
      : Expression(tok), function_identifier(std::move(fn_ident)),
        parameters(std::move(params)) {};
};

// Unwrap call expression
struct UnwrapExpression : Expression {
  Token unwrap_token;
  std::unique_ptr<Expression> expr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "UnwrapExpression: (" << unwrap_token.TokenLiteral << " "
        << (expr ? expr->toString() : "empty");
    return oss.str();
  }

  UnwrapExpression *shallowClone() const override {
    return new UnwrapExpression(unwrap_token, clonePtr(expr));
  }

  UnwrapExpression(Token unwrap, std::unique_ptr<Expression> e)
      : Expression(unwrap), unwrap_token(unwrap), expr(std::move(e)) {};
};

// Instance expression
struct InstanceExpression : Expression {
  std::unique_ptr<Expression> blockIdent;
  std::vector<std::unique_ptr<Statement>> fields;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Instance expression: ";

    // Instance name
    oss << (blockIdent ? blockIdent->toString() : "null");

    oss << " {";

    // Fields with commas
    for (size_t i = 0; i < fields.size(); ++i) {
      if (i > 0)
        oss << ", ";
      oss << (fields[i] ? fields[i]->toString() : "null");
    }

    oss << "}";
    return oss.str();
  }

  InstanceExpression *shallowClone() const override {
    return new InstanceExpression(clonePtr(blockIdent), clonePtrVector(fields));
  }

  InstanceExpression(std::unique_ptr<Expression> ident,
                     std::vector<std::unique_ptr<Statement>> param)
      : Expression(ident->expression), blockIdent(std::move(ident)),
        fields(std::move(param)) {};
};

// Function expression struct node
struct FunctionExpression : Expression {
  bool isExportable;
  Token func_key;
  std::vector<std::unique_ptr<Statement>> call;
  std::unique_ptr<Expression> return_type;
  std::unique_ptr<Expression> block;

  std::string toString() override {
    std::ostringstream oss;

    // Export keyword
    if (isExportable) {
      oss << "export ";
    }

    oss << "FunctionExpression: " << func_key.TokenLiteral << " ";
    oss << "Function parameters: (";

    // Parameters
    for (size_t i = 0; i < call.size(); ++i) {
      if (i > 0)
        oss << ", ";
      oss << (call[i] ? call[i]->toString() : "<null>");
    }

    oss << ")";
    oss << " Return type: "
        << (return_type ? return_type->toString() : "<no type>");
    oss << " Function block: " << (block ? block->toString() : "<no block>");

    return oss.str();
  }

  FunctionExpression *shallowClone() const override {
    return new FunctionExpression(isExportable, func_key, clonePtrVector(call),
                                  clonePtr(return_type), clonePtr(block));
  }

  FunctionExpression(bool exportable, Token fn,
                     std::vector<std::unique_ptr<Statement>> c,
                     std::unique_ptr<Expression> return_t,
                     std::unique_ptr<Expression> bl)
      : Expression(fn), isExportable(exportable), func_key(fn),
        call(std::move(c)), return_type(std::move(return_t)),
        block(std::move(bl)) {};
};

struct BasicType : Expression {
  Token data_token;        // Basic token like i32
  bool isNullable = false; // If we see ? we toggle

  std::string toString() override {
    std::ostringstream oss;
    oss << "Basic Type: " << data_token.TokenLiteral << (isNullable ? "?" : "");
    return oss.str();
  }

  BasicType *shallowClone() const override {
    return new BasicType(data_token, isNullable);
  }

  BasicType(Token data, bool isNull)
      : Expression(data), data_token(data), isNullable(isNull) {};
};

struct ReturnType : Expression {
  std::unique_ptr<Expression> fnptr_mod;     // Function pointer modifier
  std::unique_ptr<Expression> modified_type; // TypeModifier or nullptr
  std::unique_ptr<Expression> base_type;     // BasicType always
  bool isVoid = false;

  std::string toString() override {
    if (isVoid) {
      return "Return Type: void";
    }

    std::ostringstream oss;
    oss << "Return Type: ";

    if (fnptr_mod) {
      oss << fnptr_mod->toString();
    }
    if (modified_type) {
      oss << modified_type->toString();
    }
    if (base_type) {
      oss << base_type->toString();
    } else {
      oss << "<null>";
    }

    return oss.str();
  }

  ReturnType *shallowClone() const override {
    return new ReturnType(clonePtr(fnptr_mod), clonePtr(modified_type),
                          clonePtr(base_type), isVoid);
  }

  // Void constructor
  ReturnType() : Expression(Token{}), isVoid(true) {}

  // Normal constructor
  ReturnType(std::unique_ptr<Expression> fn_mod,
             std::unique_ptr<Expression> mod, std::unique_ptr<Expression> base,
             bool void_ = false)
      : Expression(base ? base->expression : Token{}),
        fnptr_mod(std::move(fn_mod)), modified_type(std::move(mod)),
        base_type(std::move(base)), isVoid(void_) {}
};

// Prefix expression node for syntax like !true;
struct PrefixExpression : Expression {
  Token operat;
  std::unique_ptr<Expression> operand;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Prefix Expression: (" << operat.TokenLiteral
        << (operand ? operand->toString() : "No op") << ")";

    return oss.str();
  }

  PrefixExpression *shallowClone() const override {
    return new PrefixExpression(operat, clonePtr(operand));
  }

  PrefixExpression(Token opr, std::unique_ptr<Expression> oprand)
      : Expression(opr), operat(opr), operand(std::move(oprand)) {}
};

// Postfix expression node for syntax like i++
struct PostfixExpression : Expression {
  std::unique_ptr<Expression> operand;
  Token operator_token;

  std::string toString() override {
    std::ostringstream oss;
    oss << "PostfixExpression: (" << (operand ? operand->toString() : "No op")
        << operator_token.TokenLiteral << ")";

    return oss.str();
  }

  PostfixExpression *shallowClone() const override {
    return new PostfixExpression(clonePtr(operand), operator_token);
  }

  PostfixExpression(std::unique_ptr<Expression> op, Token operat)
      : Expression(operat), operand(std::move(op)), operator_token(operat) {};
};

// Infix Expression node for syntax like x+y;
struct InfixExpression : Expression {
  std::unique_ptr<Expression> left_operand;
  Token operat;
  std::unique_ptr<Expression> right_operand;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Infix Expression: ("
        << (left_operand ? left_operand->toString() : "left")
        << operat.TokenLiteral
        << (right_operand ? right_operand->toString() : "right") << ")";

    return oss.str();
  }

  InfixExpression *shallowClone() const override {
    return new InfixExpression(clonePtr(left_operand), operat,
                               clonePtr(right_operand));
  }
  InfixExpression(std::unique_ptr<Expression> left, Token op,
                  std::unique_ptr<Expression> right)
      : Expression(op), left_operand(std::move(left)), operat(op),
        right_operand(std::move(right)) {};
};

//-----STATEMENTS----

struct ExpressionStatement : Statement {
  Token expr;
  std::unique_ptr<Expression> expression;

  std::string toString() override {
    std::ostringstream oss;
    if (expression) {
      oss << "Expression statement: " << expression->toString() << "\n";
      return oss.str();
    }
    return "";
  }

  ExpressionStatement *shallowClone() const override {
    return new ExpressionStatement(expr, clonePtr(expression));
  }

  ExpressionStatement(Token exp, std::unique_ptr<Expression> expr)
      : Statement(exp), expr(exp), expression(std::move(expr)) {};
};

// Break statement node
struct BreakStatement : Statement {
  Token break_tok;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Break Statement: " << break_tok.TokenLiteral << "\n";
    return oss.str();
  }

  BreakStatement *shallowClone() const override {
    return new BreakStatement(break_tok);
  }

  BreakStatement(Token break_t) : Statement(break_t), break_tok(break_t) {};
};

// Continue statement struct
struct ContinueStatement : Statement {
  Token cont_tok;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Continue Statement: " << cont_tok.TokenLiteral << "\n";
    return oss.str();
  }

  ContinueStatement *shallowClone() const override {
    return new ContinueStatement(cont_tok);
  }

  ContinueStatement(Token cont_t) : Statement(cont_t), cont_tok(cont_t) {};
};

// Allocator interface statement
struct AllocatorStatement : Statement {
  bool isExportable;
  Token allocator_token;
  std::unique_ptr<Expression> allocator_name;
  std::unique_ptr<Statement> block;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Allocator Interface: ";

    if (isExportable) {
      oss << "export ";
    }

    if (allocator_name) {
      oss << allocator_name->toString() << " ";
    }

    oss << (block ? block->toString() : "<null block>");

    return oss.str();
  }

  AllocatorStatement(bool exportable, Token alloc,
                     std::unique_ptr<Expression> name,
                     std::unique_ptr<Statement> blk)
      : Statement(alloc), allocator_token(alloc),
        allocator_name(std::move(name)), block(std::move(blk)) {};
};

// Seal block statement
struct SealStatement : Statement {
  bool isExportable;
  Token seal_token;
  std::unique_ptr<Expression> sealName;
  std::unique_ptr<Statement> block;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Seal Statement: ";

    if (isExportable)
      oss << "export ";
    oss << seal_token.TokenLiteral << " ";
    oss << (sealName ? sealName->toString() : "No name");

    if (block)
      oss << " " << block->toString();

    return oss.str();
  }

  SealStatement(bool isExp, Token seal, std::unique_ptr<Expression> name,
                std::unique_ptr<Statement> blk)
      : Statement(seal), isExportable(isExp), seal_token(seal),
        sealName(std::move(name)), block(std::move(blk)) {}
};

// Inject statement struct
struct InjectStatement : Statement {
  Token inject_token;
  Token kind_token; // "record"
  std::unique_ptr<Expression> expr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Inject statement: " << kind_token.TokenLiteral
        << (expr ? expr->toString() : "expr");
    return oss.str();
  }

  InjectStatement(Token injectTok, Token kindTok,
                  std::unique_ptr<Expression> name)
      : Statement(injectTok), inject_token(injectTok), kind_token(kindTok),
        expr(std::move(name)) {}
};

// Structure modifier
struct StructureModifier : Expression {
  bool isPacked = false;
  bool isBitfield = false;
  bool isUnion = false;
  std::unique_ptr<Expression> _align;

  StructureModifier *shallowClone() const override {
    return new StructureModifier(isPacked, isBitfield, isUnion,
                                 clonePtr(_align));
  }

  StructureModifier() : Expression(Token{}) {}

  StructureModifier(bool packed, bool bitfield, bool union_,
                    std::unique_ptr<Expression> align = nullptr)
      : Expression(Token{}), isPacked(packed), isBitfield(bitfield),
        isUnion(union_), _align(std::move(align)) {}

  std::string toString() override {
    std::ostringstream oss;
    bool first = true;

    auto addModifier = [&](const std::string &mod) {
      if (!first)
        oss << " ";
      oss << mod;
      first = false;
    };

    if (isPacked)
      addModifier("packed");
    if (isBitfield)
      addModifier("bitfield");
    if (isUnion)
      addModifier("union");
    if (_align) {
      if (!first)
        oss << " ";
      oss << "align(" << _align->toString() << ")";
    }

    return oss.str();
  }
};

// Record statement struct
struct RecordStatement : Statement {
  bool isVolatile = false; // Every field in the structure is volatile
  bool isExportable = false;
  Mutability mutability = Mutability::IMMUTABLE;

  std::unique_ptr<StructureModifier> modifiers;
  Token record_token;
  std::unique_ptr<Expression> recordName;
  std::vector<std::unique_ptr<Statement>> fields;

  RecordStatement *shallowClone() const override {
    return new RecordStatement(record_token, isVolatile, isExportable,
                               mutability, clonePtr(modifiers),
                               clonePtr(recordName), clonePtrVector(fields));
  };

  RecordStatement(Token record, bool _volatile, bool _exportable,
                  Mutability mut, std::unique_ptr<StructureModifier> mods,
                  std::unique_ptr<Expression> name,
                  std::vector<std::unique_ptr<Statement>> record_fields)
      : Statement(record), isVolatile(_volatile), isExportable(_exportable),
        mutability(mut), modifiers(std::move(mods)), record_token(record),
        recordName(std::move(name)), fields(std::move(record_fields)) {}

  std::string toString() override {
    std::ostringstream oss;

    if (isVolatile)
      oss << "volatile ";
    if (isExportable)
      oss << "export ";

    if (mutability == Mutability::MUTABLE)
      oss << "mut ";
    if (mutability == Mutability::CONSTANT)
      oss << "const ";

    if (modifiers)
      oss << modifiers->toString();

    oss << "record " << (recordName ? recordName->toString() : "<null>");
    oss << " {\n";

    for (const auto &field : fields) {
      oss << "  " << (field ? field->toString() : "<null field>") << "\n";
    }

    oss << "}";
    return oss.str();
  }
};

// Init statement
struct InitStatement : Statement {
  Token init_token;
  std::vector<std::unique_ptr<Statement>> constructor_args;
  std::unique_ptr<Statement> block;

  std::string toString() override {
    std::ostringstream oss;
    oss << "InitStatement: " << init_token.TokenLiteral << "(";
    for (auto &arguments : constructor_args) {

      oss << (arguments ? arguments->toString() : "argument");
    }
    oss << ")" << (block ? block->toString() : "block");
    return oss.str();
  }

  InitStatement *shallowClone() const override {
    return new InitStatement(init_token, clonePtrVector(constructor_args),
                             clonePtr(block));
  }

  InitStatement(Token init, std::vector<std::unique_ptr<Statement>> args,
                std::unique_ptr<Statement> block_content)
      : Statement(init), constructor_args(std::move(args)),
        block(std::move(block_content)) {};
};

// Component statement struct
struct ComponentStatement : Statement {
  bool isExportable;
  Token component_token;
  std::unique_ptr<Expression> component_name;

  std::vector<std::unique_ptr<Statement>> fields;
  std::vector<std::unique_ptr<Statement>> methods;
  std::vector<std::unique_ptr<Statement>> injectedFields;
  std::optional<std::unique_ptr<Statement>> initConstructor;

  ComponentStatement *shallowClone() const override {
    return new ComponentStatement(
        isExportable, component_token, clonePtr(component_name),
        clonePtrVector(fields), clonePtrVector(methods),
        clonePtrVector(injectedFields), clonePtr(initConstructor.value()));
  };

  ComponentStatement(bool exportable, Token component,
                     std::unique_ptr<Expression> name,
                     std::vector<std::unique_ptr<Statement>> private_fields,
                     std::vector<std::unique_ptr<Statement>> private_methods,
                     std::vector<std::unique_ptr<Statement>> injected_fields,
                     std::optional<std::unique_ptr<Statement>> init)
      : Statement(component), isExportable(exportable),
        component_token(component), component_name(std::move(name)),
        fields(std::move(private_fields)), methods(std::move(private_methods)),
        injectedFields(std::move(injected_fields)),
        initConstructor(std::move(init)) {}

  std::string toString() override {
    std::ostringstream oss;

    if (isExportable)
      oss << "export ";

    oss << "component "
        << (component_name ? component_name->toString() : "<null>");
    oss << " {\n";

    for (const auto &field : fields) {
      if (!field) {
        oss << "  <null field>\n";
      } else {
        oss << "  " << field->toString() << "\n";
      }
    }

    for (const auto &method : methods) {
      if (!method) {
        oss << "  <null method>\n";
      } else {
        oss << "  " << method->toString() << "\n";
      }
    }

    for (const auto &injected : injectedFields) {
      oss << "  " << (injected ? injected->toString() : "<null>") << "\n";
    }

    if (initConstructor.has_value() && *initConstructor) {
      oss << "  " << (*initConstructor)->toString() << "\n";
    }

    oss << "}";
    return oss.str();
  }
};

// Function pointer modifier
struct FunctionPointerModifier : Expression {
  Token fn_token;
  std::vector<std::unique_ptr<Expression>> type_args;

  std::string toString() override {
    std::ostringstream oss;
    oss << " Func Pointer Mod: fn(";

    for (size_t i = 0; i < type_args.size(); ++i) {
      if (i > 0)
        oss << ", ";
      oss << (type_args[i] ? type_args[i]->toString() : "<null>");
    }

    oss << "): ";
    return oss.str();
  }

  FunctionPointerModifier *shallowClone() const override {
    return new FunctionPointerModifier(fn_token, clonePtrVector(type_args));
  }

  FunctionPointerModifier(Token fn,
                          std::vector<std::unique_ptr<Expression>> args)
      : Expression(fn), fn_token(fn), type_args(std::move(args)) {};
};

struct TypeModifier : Expression {
  bool isArray = false;
  bool isPointer = false;
  bool isReference = false;

  std::vector<std::unique_ptr<Expression>> dimensions;
  std::unique_ptr<Expression>
      inner_modifier; // For nested modifiers like ptr inside arr

  std::string toString() override {
    std::ostringstream oss;

    if (isPointer) {
      oss << "ptr";
      if (inner_modifier) {
        oss << "<" << inner_modifier->toString() << ">";
      }
      oss << " ";
    } else if (isReference) {
      oss << "ref";
      if (inner_modifier) {
        oss << "<" << inner_modifier->toString() << ">";
      }
      oss << " ";
    } else if (isArray) {
      oss << "arr";
      for (const auto &dim : dimensions) {
        oss << "[" << (dim ? dim->toString() : "?") << "]";
      }
      if (inner_modifier) {
        oss << "<" << inner_modifier->toString() << ">";
      }
      oss << " ";
    }

    return oss.str();
  }

  TypeModifier *shallowClone() const override {
    auto *clone = new TypeModifier();
    clone->isArray = isArray;
    clone->isPointer = isPointer;
    clone->isReference = isReference;
    clone->dimensions = clonePtrVector(dimensions);
    clone->inner_modifier = clonePtr(inner_modifier);
    return clone;
  }

  TypeModifier() : Expression(Token{}) {}

  TypeModifier(bool isArr, bool isPtr, bool isRef,
               std::vector<std::unique_ptr<Expression>> dims = {},
               std::unique_ptr<Expression> inner = nullptr)
      : Expression(Token{}), isArray(isArr), isPointer(isPtr),
        isReference(isRef), dimensions(std::move(dims)),
        inner_modifier(std::move(inner)) {}
};

struct VariableDeclaration : Statement {
  // Pointers first (8-byte aligned)
  std::unique_ptr<Expression> allocator;
  std::unique_ptr<Expression> modified_type;
  std::unique_ptr<Expression> fnPtrMod;
  std::unique_ptr<Expression> base_type;
  std::unique_ptr<Expression> var_name;
  std::unique_ptr<Expression> initializer;

  // Large objects next
  std::optional<Token> assign_token;

  // Small primitives last
  Mutability mutability = Mutability::IMMUTABLE;
  bool isPersist = false;
  bool isHeap = false;
  bool isVolatile = false;
  bool isRestrict = false;
  bool isExportable = false;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Variable Declaration: (";

    // Storage with optional allocator
    if (isHeap) {
      oss << "heap";
      if (allocator) {
        oss << "<" << allocator->toString() << ">";
      }
      oss << " ";
    }

    // Type qualifiers
    if (isPersist)
      oss << "persist ";
    if (isVolatile)
      oss << "volatile ";
    if (isRestrict)
      oss << "restrict ";
    if (isExportable)
      oss << "export ";
    if (mutability == Mutability::CONSTANT)
      oss << "const ";
    if (mutability == Mutability::MUTABLE)
      oss << "mut ";

    // Type modifiers (ptr, arr, ref)
    if (modified_type) {
      oss << modified_type->toString();
    }

    // Func modifier
    if (fnPtrMod) {
      oss << fnPtrMod->toString();
    }

    // Base type
    oss << (base_type ? base_type->toString() : "<unknown>");
    oss << " ";

    // name
    oss << (var_name ? var_name->toString() : "<unnamed>");

    // Initializer
    if (initializer) {
      oss << " = " << initializer->toString();
    } else {
      oss << " <uninitialized>";
    }

    oss << ")";
    return oss.str();
  }

  VariableDeclaration *shallowClone() const override {
    return new VariableDeclaration(clonePtr(allocator), clonePtr(fnPtrMod),
                                   clonePtr(modified_type), clonePtr(base_type),
                                   clonePtr(var_name), clonePtr(initializer),
                                   assign_token, mutability, isPersist, isHeap,
                                   isVolatile, isRestrict, isExportable);
  }

  VariableDeclaration(std::unique_ptr<Expression> _allocator,
                      std::unique_ptr<Expression> fn_mod,
                      std::unique_ptr<Expression> _modifier,
                      std::unique_ptr<Expression> base,
                      std::unique_ptr<Expression> name,
                      std::unique_ptr<Expression> init,
                      std::optional<Token> _assign, Mutability mut,
                      bool _persist, bool _heap, bool _volatile, bool _restrict,
                      bool _export)
      : Statement(name ? name->token : Token{}),
        allocator(std::move(_allocator)), modified_type(std::move(_modifier)),
        fnPtrMod(std::move(fn_mod)), base_type(std::move(base)),
        var_name(std::move(name)), initializer(std::move(init)),
        assign_token(_assign), mutability(mut), isPersist(_persist),
        isHeap(_heap), isVolatile(_volatile), isRestrict(_restrict),
        isExportable(_export) {}
};

struct AssignmentStatement : Statement {
  std::unique_ptr<Expression> identifier;
  Token op;
  std::unique_ptr<Expression> value;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Assignment statement: (Variable: " << identifier->toString()
        << op.TokenLiteral
        << " Value: " << (value ? value->toString() : "value") << ")";
    return oss.str();
  };

  AssignmentStatement *shallowClone() const override {
    return new AssignmentStatement(clonePtr(identifier), op, clonePtr(value));
  }
  AssignmentStatement(std::unique_ptr<Expression> ident, Token operat,
                      std::unique_ptr<Expression> val)
      : Statement(ident->token), identifier(std::move(ident)), op(operat),
        value(std::move(val)) {};
};

struct FieldAssignment : Statement {
  std::unique_ptr<Expression> lhs_chain;
  Token op;
  std::unique_ptr<Expression> value;

  FieldAssignment *shallowClone() const override {
    return new FieldAssignment(clonePtr(lhs_chain), op, clonePtr(value));
  };

  FieldAssignment(std::unique_ptr<Expression> lhs, Token operat,
                  std::unique_ptr<Expression> val)
      : Statement(lhs->expression), lhs_chain(std::move(lhs)), op(operat),
        value(std::move(val)) {};

  std::string toString() override {
    std::ostringstream oss;
    oss << "Field Assignment: (Path: " << lhs_chain->toString()
        << op.TokenLiteral
        << " Value: " << (value ? value->toString() : "value") << ")";
    return oss.str();
  }
};

// Return statement node
struct ReturnStatement : Statement {
  Token return_stmt;
  std::unique_ptr<Expression> return_value;
  std::string toString() override {
    std::ostringstream oss;
    oss << "Return Statement: " << return_stmt.TokenLiteral << " "
        << (return_value ? return_value->toString() : "");
    return oss.str();
  }

  ReturnStatement *shallowClone() const override {
    return new ReturnStatement(return_stmt, clonePtr(return_value));
  }
  ReturnStatement(Token ret, std::unique_ptr<Expression> ret_val)
      : Statement(ret), return_stmt(ret), return_value(std::move(ret_val)) {};
};

// Elif statement node
struct elifStatement : Statement {
  Token elif_token;
  std::unique_ptr<Expression> elif_condition;
  std::unique_ptr<Statement> elif_result;

  std::string toString() override {
    std::ostringstream oss;
    oss << "elifStatement: " << elif_token.TokenLiteral
        << "(" +
               (elif_condition ? elif_condition->toString() : "elif_condition")
        << ") {" << (elif_result ? elif_result->toString() : "elif_result")
        << "}";
    return oss.str();
  }

  elifStatement *shallowClone() const override {
    return new elifStatement(elif_token, clonePtr(elif_condition),
                             clonePtr(elif_result));
  }

  elifStatement(Token token, std::unique_ptr<Expression> condition,
                std::unique_ptr<Statement> result)
      : Statement(token), elif_token(token),
        elif_condition(std::move(condition)), elif_result(std::move(result)) {};
};

// If statement node
struct ifStatement : Statement {
  Token if_stmt;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> if_result;

  std::vector<std::unique_ptr<Statement>> elifClauses;

  std::optional<Token> else_stmt;
  std::optional<std::unique_ptr<Statement>> else_result;

  std::string toString() override {
    std::ostringstream oss;
    oss << "IfStatement:\n";

    // Condition
    if (condition) {
      oss << "  if (" << condition->toString() << ") {\n";
    } else {
      oss << "  if (<null condition>) {\n";
    }

    // If body
    if (if_result) {
      oss << "    " << if_result->toString() << "\n";
    } else {
      oss << "    <null if_result>\n";
    }

    oss << "  }\n";

    // Elif clauses
    for (const auto &elifs : elifClauses) {
      if (elifs) {
        oss << elifs->toString();
      } else {
        oss << "    <null elif>\n";
      }
    }
    oss << "\n";

    // ELSE block
    if (else_stmt.has_value()) {
      oss << "  else {\n";

      if (else_result.has_value() && *else_result) {
        oss << "    " << (*else_result)->toString() << "\n";
      } else {
        oss << "    <null else_result>\n";
      }

      oss << "  }\n";
    }

    return oss.str();
  }

  ifStatement *shallowClone() const override {
    return new ifStatement(if_stmt, clonePtr(condition), clonePtr(if_result),
                           clonePtrVector(elifClauses), else_stmt,
                           clonePtr(else_result.value()));
  }

  ifStatement(Token if_st, std::unique_ptr<Expression> condition_e,
              std::unique_ptr<Statement> if_r,
              std::vector<std::unique_ptr<Statement>> elifStmts,
              std::optional<Token> else_st,
              std::optional<std::unique_ptr<Statement>> else_r)
      : Statement(if_st), if_stmt(if_st), condition(std::move(condition_e)),
        if_result(std::move(if_r)), elifClauses(std::move(elifStmts)),
        else_stmt(else_st), else_result(std::move(else_r)) {};
};

// Case clause struct used in switch-case statement
struct CaseClause : Statement {
  Token case_token;
  std::unique_ptr<Expression> condition;
  std::vector<std::unique_ptr<Statement>> body;

  std::string toString() override {
    std::ostringstream oss;
    oss << "case " << (condition ? condition->toString() : "condition");
    for (const auto &content : body) {
      oss << (content ? content->toString() : "body");
    }
    return oss.str();
  }

  CaseClause *shallowClone() const override {
    return new CaseClause(case_token, clonePtr(condition),
                          clonePtrVector(body));
  }

  CaseClause(Token token, std::unique_ptr<Expression> cond,
             std::vector<std::unique_ptr<Statement>> stmt)
      : Statement(token), case_token(token), condition(std::move(cond)),
        body(std::move(stmt)) {}
};

struct SwitchStatement : Statement {
  Token switch_token;
  std::unique_ptr<Expression> switch_init;
  // Inside the braces
  // Cases
  std::vector<std::unique_ptr<Statement>> case_clauses;
  // Default case
  Token default_token;
  std::vector<std::unique_ptr<Statement>> default_statements;

  std::string toString() override {
    std::ostringstream oss;
    oss << "switch (" << (switch_init ? switch_init->toString() : "<null>")
        << ") {\n";

    // Print all case clauses
    for (const auto &clause : case_clauses) {
      oss << (clause ? clause->toString() : "<null clause>") << "\n";
    }

    // Print default statements if they exist
    if (!default_statements.empty()) {
      oss << "\n  default:\n";
      for (const auto &stmt : default_statements) {
        oss << "    " << (stmt ? stmt->toString() : "<null stmt>") << "\n";
      }
    }

    oss << "}";
    return oss.str();
  }

  SwitchStatement *shallowClone() const override {
    return new SwitchStatement(switch_token, clonePtr(switch_init),
                               clonePtrVector(case_clauses), default_token,
                               clonePtrVector(default_statements));
  }

  SwitchStatement(Token token, std::unique_ptr<Expression> expr,
                  std::vector<std::unique_ptr<Statement>> cases,
                  Token default_tok,
                  std::vector<std::unique_ptr<Statement>> default_stmts)
      : Statement(token), switch_token(token), switch_init(std::move(expr)),
        case_clauses(std::move(cases)), default_token(default_tok),
        default_statements(std::move(default_stmts)) {}
};

struct EnumMember : Node {
  Token t;
  std::string enumMember;
  std::unique_ptr<Expression> value;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Enum Member: " << enumMember
        << (value ? value->toString() : "value");
    return oss.str();
  }

  EnumMember *shallowClone() const override {
    return new EnumMember(t, enumMember, clonePtr(value));
  }

  EnumMember(Token token, std::string member, std::unique_ptr<Expression> val)
      : Node(token), enumMember(member), value(std::move(val)) {};
};

// Enum class struct
struct EnumStatement : Statement {
  bool isExportable;
  Token enum_token;
  std::unique_ptr<Expression> enum_identifier;
  std::optional<Token> int_type;
  std::vector<std::unique_ptr<EnumMember>> enum_content;

  std::string toString() override {
    std::ostringstream oss;

    if (isExportable)
      oss << "export ";
    oss << "Enum statement: "
        << (enum_identifier ? enum_identifier->toString() : "<null>");

    if (int_type.has_value()) {
      oss << " : " << int_type.value().TokenLiteral;
    }

    oss << " { ";

    for (size_t i = 0; i < enum_content.size(); ++i) {
      if (i > 0)
        oss << ", ";
      oss << (enum_content[i] ? enum_content[i]->toString() : "<null>");
    }

    oss << " }";
    return oss.str();
  }

  EnumStatement *shallowClone() const override {
    return new EnumStatement(isExportable, enum_token,
                             clonePtr(enum_identifier), int_type,
                             clonePtrVector(enum_content));
  }

  EnumStatement(bool exportable, Token enum_tok,
                std::unique_ptr<Expression> enum_ident,
                std::optional<Token> intType,
                std::vector<std::unique_ptr<EnumMember>> enum_block)
      : Statement(enum_tok), isExportable(exportable), enum_token(enum_tok),
        enum_identifier(std::move(enum_ident)), int_type(intType),
        enum_content(std::move(enum_block)) {};
};

struct ForStatement : Statement {
  Token for_key;
  std::unique_ptr<Statement> initializer; // mut i32 i;
  std::unique_ptr<Expression> condition;  // i < 10
  std::unique_ptr<Statement> step;        // i = i + 1
  std::unique_ptr<Statement> body;        // the loop body

  ForStatement *shallowClone() const override {
    return new ForStatement(for_key, clonePtr(initializer), clonePtr(condition),
                            clonePtr(step), clonePtr(body));
  }

  ForStatement(Token for_k, std::unique_ptr<Statement> init,
               std::unique_ptr<Expression> cond,
               std::unique_ptr<Statement> step, std::unique_ptr<Statement> body)
      : Statement(for_k), for_key(for_k), initializer(std::move(init)),
        condition(std::move(cond)), step(std::move(step)),
        body(std::move(body)) {};

  std::string toString() override {
    std::ostringstream oss;
    oss << "ForStatement(\n";
    oss << "  Init: " << (initializer ? initializer->toString() : "null")
        << "\n";
    oss << "  Cond: " << (condition ? condition->toString() : "null") << "\n";
    oss << "  Step: " << (step ? step->toString() : "null") << "\n";
    oss << "  Body: " << (body ? body->toString() : "null") << "\n";
    oss << ")";
    return oss.str();
  }
};

struct WhileStatement : Statement {
  Token while_key;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> loop;

  std::string toString() override {
    std::ostringstream oss;
    oss << "While : " << (condition ? condition->toString() : "condition")
        << (loop ? loop->toString() : "loop");
    return oss.str();
  }

  WhileStatement *shallowClone() const override {
    return new WhileStatement(while_key, clonePtr(condition), clonePtr(loop));
  }

  WhileStatement(Token while_k, std::unique_ptr<Expression> condition,
                 std::unique_ptr<Statement> l)
      : Statement(while_k), while_key(while_k), condition(std::move(condition)),
        loop(std::move(l)) {};
};

// Function Statement
struct FunctionStatement : Statement {
  Token token;
  std::unique_ptr<Expression> funcExpr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Function Statement: "
        << (funcExpr ? funcExpr->toString() : "func_expr");
    return oss.str();
  }

  FunctionStatement *shallowClone() const override {
    return new FunctionStatement(token, clonePtr(funcExpr));
  }

  FunctionStatement(Token funcStmtTok, std::unique_ptr<Expression> expr)
      : Statement(funcStmtTok), funcExpr(std::move(expr)) {};
};

// Function declaration statement
struct FunctionDeclaration : Statement {
  bool isExportable;
  Token func_keyword_token;
  std::unique_ptr<Expression> function_name;
  std::vector<std::unique_ptr<Statement>> parameters;
  std::unique_ptr<Expression> return_type;
  bool isNullable = false;

  std::string toString() override {
    std::ostringstream oss;

    if (isExportable)
      oss << "export ";
    oss << "Function Declaration Statement: ";
    oss << func_keyword_token.TokenLiteral << " ";
    oss << (function_name ? function_name->toString() : "[null_name]") << " ";

    for (size_t i = 0; i < parameters.size(); ++i) {
      if (parameters[i]) {
        oss << parameters[i]->toString();
      } else {
        oss << "<null>";
      }
    }

    oss << " ";
    oss << (return_type ? return_type->toString() : "[no_return]");

    return oss.str();
  }

  FunctionDeclaration *shallowClone() const override {
    return new FunctionDeclaration(
        isExportable, func_keyword_token, clonePtr(function_name),
        clonePtrVector(parameters), clonePtr(return_type));
  }

  FunctionDeclaration(bool exportable, Token func,
                      std::unique_ptr<Expression> identifier,
                      std::vector<std::unique_ptr<Statement>> params,
                      std::unique_ptr<Expression> ret_type)
      : Statement(func), isExportable(exportable), func_keyword_token(func),
        function_name(std::move(identifier)), parameters(std::move(params)),
        return_type(std::move(ret_type)) {};
};

// Function Declaration expression
struct FunctionDeclarationExpression : Expression {
  Token func_token;
  std::unique_ptr<Statement> funcDeclrStmt;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Function Declaration Expression: "
        << (funcDeclrStmt ? funcDeclrStmt->toString() : "fn_decl");
    return oss.str();
  }

  FunctionDeclarationExpression *shallowClone() const override {
    return new FunctionDeclarationExpression(func_token,
                                             clonePtr(funcDeclrStmt));
  }

  FunctionDeclarationExpression(Token func,
                                std::unique_ptr<Statement> declaration)
      : Expression(func), funcDeclrStmt(std::move(declaration)) {};
};

// Block statement
struct BlockStatement : Statement {
  Token brace;
  std::vector<std::unique_ptr<Statement>> statements;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Block Statement: { ";
    for (const auto &s : statements) {
      if (s) {
        oss << s->toString();
      }
    }
    oss << " }";
    return oss.str();
  }

  BlockStatement *shallowClone() const override {
    return new BlockStatement(brace, clonePtrVector(statements));
  }

  BlockStatement(Token brac, std::vector<std::unique_ptr<Statement>> cont)
      : Statement(brac), brace(brac), statements(std::move(cont)) {};
};

// GENERICS
// Generic statement node
struct GenericStatement : Statement {
  Token generic_token;
  std::unique_ptr<Expression> block_name;
  std::vector<Token> type_parameters;
  std::unique_ptr<Statement> block;

  std::string toString() override {
    std::ostringstream oss;

    oss << "Generic Statement: ";
    oss << (block_name ? block_name->toString() : "<null>");
    oss << "(";

    for (size_t i = 0; i < type_parameters.size(); ++i) {
      if (i > 0)
        oss << ",";
      oss << type_parameters[i].TokenLiteral;
    }

    oss << ")";

    if (block) {
      oss << block->toString();
    } else {
      oss << " { <blueprint stored> }";
    }

    return oss.str();
  }

  GenericStatement(Token generic, std::unique_ptr<Expression> name,
                   std::vector<Token> types, std::unique_ptr<Statement> content)
      : Statement(generic), generic_token(generic), block_name(std::move(name)),
        type_parameters(types), block(std::move(content)) {};
};

// Instatiate statement
struct InstantiateStatement : Statement {
  bool isExportable;
  Token instantiate_token;
  std::unique_ptr<Expression> generic_call;
  Token as_token;
  Token alias;

  std::string toString() override {
    std::ostringstream oss;
    if (isExportable)
      oss << "export ";

    oss << "Instantiate statement: "
        << (generic_call ? generic_call->toString() : "no generic_call()")
        << " as " << alias.TokenLiteral;
    return oss.str();
  }

  InstantiateStatement(bool _export, Token inst,
                       std::unique_ptr<Expression> call, Token as, Token name)
      : Statement(inst), isExportable(_export), instantiate_token(inst),
        generic_call(std::move(call)), as_token(as), alias(name) {};
};

// Generic call expression
struct GenericCall : Expression {
  std::unique_ptr<Expression> ident;
  std::vector<Token> args;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Generic Call:" << (ident ? ident->toString() : "<null>") << "(";

    for (size_t i = 0; i < args.size(); ++i) {
      if (i > 0)
        oss << ",";
      oss << args[i].TokenLiteral;
    }

    oss << ")";
    return oss.str();
  }

  GenericCall(std::unique_ptr<Expression> name, std::vector<Token> types)
      : Expression(name->token), ident(std::move(name)), args(types) {};
};

// Array Literal
struct ArrayLiteral : Expression {
  Token arr_token;
  std::vector<std::unique_ptr<Expression>> array;

  std::string toString() override {
    std::ostringstream oss;
    oss << "ArrayLiteral: [";
    for (const auto &item : array) {
      oss << (item ? item->toString() : "item") << ",";
    }
    oss << "]";
    return oss.str();
  }

  ArrayLiteral *shallowClone() const override {
    return new ArrayLiteral(arr_token, clonePtrVector(array));
  }

  ArrayLiteral(Token arr_tok, std::vector<std::unique_ptr<Expression>> arr)
      : Expression(arr_tok), arr_token(arr_tok), array(std::move(arr)) {};
};

// Array Subscript expression
struct ArraySubscript : Expression {
  std::unique_ptr<Expression> identifier;
  std::vector<std::unique_ptr<Expression>> index_exprs;

  std::string toString() override {
    std::ostringstream oss;
    oss << "ArraySubscript expression: "
        << (identifier ? identifier->toString() : "name");
    for (const auto &index : index_exprs) {
      oss << "[" << (index ? index->toString() : "index") << "]";
    }
    return oss.str();
  }

  ArraySubscript *shallowClone() const override {
    return new ArraySubscript(clonePtr(identifier),
                              clonePtrVector(index_exprs));
  }

  ArraySubscript(std::unique_ptr<Expression> ident,
                 std::vector<std::unique_ptr<Expression>> ids)
      : Expression(ident->expression), identifier(std::move(ident)),
        index_exprs(std::move(ids)) {};
};

// Import statement
struct ImportStatement : Statement {
  Token import_key;
  std::unique_ptr<Expression> stringExpr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Import Statement: " << import_key.TokenLiteral << " "
        << (stringExpr ? stringExpr->toString() : "<example>");
    return oss.str();
  }

  ImportStatement(Token import, std::unique_ptr<Expression> string)
      : Statement(import), import_key(import), stringExpr(std::move(string)) {};
};

// Link statement
struct LinkStatement : Statement {
  Token link_key;
  std::unique_ptr<Expression> stringExpr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Link Statement: " << link_key.TokenLiteral << " "
        << (stringExpr ? stringExpr->toString() : "<example>");
    return oss.str();
  }

  LinkStatement(Token link, std::unique_ptr<Expression> string)
      : Statement(link), link_key(link), stringExpr(std::move(string)) {};
};

// Trace statement
struct TraceStatement : Statement {
  Token trace_keyword;
  std::vector<std::unique_ptr<Expression>> arguments;

  std::string toString() override {
    std::ostringstream oss;
    oss << "Trace Statement: " << trace_keyword.TokenLiteral << "(";
    for (size_t i = 0; i < arguments.size(); ++i) {
      oss << (arguments[i] ? arguments[i]->toString() : "argument");
      if (i < arguments.size() - 1)
        oss << ", ";
    }
    oss << ")";
    return oss.str();
  }

  TraceStatement *shallowClone() const override {
    return new TraceStatement(trace_keyword, clonePtrVector(arguments));
  }

  TraceStatement(Token trace, std::vector<std::unique_ptr<Expression>> args)
      : Statement(trace), trace_keyword(trace), arguments(std::move(args)) {}
};

struct ASMConstraint : Expression {
  Token colon_token;
  std::string direction;
  std::string constraint;
  std::unique_ptr<Expression>
      variable; // I cant use a string semantics wants a node to tag symbol info

  ASMConstraint(Token colon_t, std::string _dir, std::string _constraint,
                std::unique_ptr<Expression> var)
      : Expression(colon_t), colon_token(colon_t), direction(_dir),
        constraint(_constraint), variable(std::move(var)) {};
};

struct ASMInstruction : Statement {
  std::string mnemonic;
  std::vector<std::string> operands;
  std::vector<std::unique_ptr<ASMConstraint>> constraints;

  std::string toString() override {
    std::ostringstream oss;
    oss << mnemonic;

    if (!operands.empty()) {
      oss << " ";
      for (size_t i = 0; i < operands.size(); ++i) {
        if (i > 0)
          oss << ", ";
        oss << operands[i];
      }
    }

    return oss.str();
  }

  ASMInstruction(std::string _mnemonic, std::vector<std::string> _operands,
                 std::vector<std::unique_ptr<ASMConstraint>> _constraints)
      : Statement(Token{}), mnemonic(std::move(_mnemonic)),
        operands(std::move(_operands)), constraints(std::move(_constraints)) {}
};

struct ASMStatement : Statement {
  bool isVolatile;
  Token asm_token;
  std::string dialect; // "intel", "att", default "intel"
  std::vector<std::unique_ptr<Statement>> instructions;

  std::string toASMString() {
    std::string result;
    for (size_t i = 0; i < instructions.size(); i++) {
      result += instructions[i]->toString();
      if (i < instructions.size() - 1)
        result += "\n";
    }
    return result;
  }

  std::string toString() override {
    std::ostringstream oss;

    if (isVolatile)
      oss << "volatile ";
    oss << "asm<" << dialect << "> {\n";

    for (const auto &instr : instructions) {
      if (instr) {
        oss << "    " << instr->toString() << "\n";
      } else {
        oss << "    <null instruction>\n";
      }
    }

    oss << "}";
    return oss.str();
  }

  ASMStatement(bool _volatile, Token asm_t, std::string _dialect,
               std::vector<std::unique_ptr<Statement>> _instructions)
      : Statement(asm_t), isVolatile(_volatile), asm_token(asm_t),
        dialect(std::move(_dialect)), instructions(std::move(_instructions)) {}
};

// BLOCKS
//  Block expression
struct BlockExpression : Expression {
  Token brace;
  std::vector<std::unique_ptr<Statement>> statements;
  std::optional<std::unique_ptr<Expression>> finalexpr;

  std::string toString() override {
    std::ostringstream oss;
    oss << "BlockExpression: { \n";
    for (auto &stmt : statements) {
      oss << (stmt ? stmt->toString() : "stmt") << "\n";
    }
    if (finalexpr.has_value() && finalexpr.value()) {
      oss << "Final Expression: "
          << (finalexpr.value() ? finalexpr.value()->toString() : "");
    }
    oss << "}";
    return oss.str();
  };

  BlockExpression *shallowClone() const override {
    auto b = new BlockExpression(brace);
    b->statements = clonePtrVector(statements);
    if (finalexpr.has_value()) {
      b->finalexpr = clonePtr(finalexpr.value());
    }
    return b;
  }

  BlockExpression(Token lbrace) : Expression(lbrace) {};
};

enum class Precedence {
  PREC_NONE = 0,
  PREC_ASSIGNMENT,  // =
  PREC_COALESCE,    //"??"
  PREC_OR,          // ||
  PREC_AND,         // &&
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_BITWISE_OR,  //|
  PREC_BITWISE_XOR, //^
  PREC_BITWISE_AND, //&
  PREC_SHIFT,       //<< >>
  PREC_TERM,        // + -
  PREC_FACTOR,      // "* /"
  PREC_UNARY,       // "! - ~"
  PREC_POSTFIX,
  PREC_CALL, // . ()
  PREC_PRIMARY
};
