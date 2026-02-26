#include "ast.hpp"
#include "semantics.hpp"

// Walking the data type literals
void Semantics::walkBooleanLiteral(Node *node) {
  auto boolLiteral = dynamic_cast<BooleanLiteral *>(node);
  if (!boolLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::BOOLEAN, "bool"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[boolLiteral] = info;
}

void Semantics::walkStringLiteral(Node *node) {
  auto strLiteral = dynamic_cast<StringLiteral *>(node);
  if (!strLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::STRING, "string"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[strLiteral] = info;
}

void Semantics::walkChar8Literal(Node *node) {
  auto char8Literal = dynamic_cast<Char8Literal *>(node);
  if (!char8Literal)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::CHAR8, "char8"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[char8Literal] = info;
}

void Semantics::walkChar16Literal(Node *node) {
  auto lit = dynamic_cast<Char16Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::CHAR16, "char16"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkChar32Literal(Node *node) {
  auto lit = dynamic_cast<Char32Literal *>(node);
  if (!lit)
    return;
  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::CHAR32, "char32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI8Literal(Node *node) {
  auto lit = dynamic_cast<I8Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I8, "i8"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU8Literal(Node *node) {
  auto lit = dynamic_cast<U8Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U8, "u8"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI16Literal(Node *node) {
  auto lit = dynamic_cast<I16Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I16, "i16"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU16Literal(Node *node) {
  auto lit = dynamic_cast<U16Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U16, "u16"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI32Literal(Node *node) {
  auto intLiteral = dynamic_cast<I32Literal *>(node);
  if (!intLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I32, "i32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[intLiteral] = info;
}

void Semantics::walkU32Literal(Node *node) {
  auto intLiteral = dynamic_cast<U32Literal *>(node);
  if (!intLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U32, "u32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[intLiteral] = info;
}

void Semantics::walkI64Literal(Node *node) {
  auto lit = dynamic_cast<I64Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I64, "i64"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU64Literal(Node *node) {
  auto lit = dynamic_cast<U64Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U64, "u64"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkI128Literal(Node *node) {
  auto lit = dynamic_cast<I128Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::I128, "i128"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkU128Literal(Node *node) {
  auto lit = dynamic_cast<U128Literal *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::U128, "u128"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkISIZELiteral(Node *node) {
  auto lit = dynamic_cast<ISIZELiteral *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::ISIZE, "isize"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkUSIZELiteral(Node *node) {
  auto lit = dynamic_cast<USIZELiteral *>(node);
  if (!lit)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::USIZE, "usize"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[lit] = info;
}

void Semantics::walkF32Literal(Node *node) {
  auto f32Literal = dynamic_cast<F32Literal *>(node);
  if (!f32Literal)
    return;

  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::F32, "f32"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[f32Literal] = info;
}

void Semantics::walkF64Literal(Node *node) {
  auto f64Literal = dynamic_cast<F64Literal *>(node);
  if (!f64Literal)
    return;
  auto info = std::make_shared<SymbolInfo>();

  info->type = ResolvedType{DataType::F64, "f64"};
  info->isNullable = false;
  info->isMutable = false;
  info->isConstant = false;
  metaData[f64Literal] = info;
}

// Walking the null literal
void Semantics::walkNullLiteral(Node *node) {
  auto nullLit = dynamic_cast<NullLiteral *>(node);
  if (!nullLit)
    return;

  auto symbol = std::make_shared<SymbolInfo>();
  symbol->type =
      ResolvedType{DataType::UNKNOWN, "null"}; // Unknown data type for now
  metaData[nullLit] = symbol;
}

// Walking the array literal
void Semantics::walkArrayLiteral(Node *node) {
  auto arrLit = dynamic_cast<ArrayLiteral *>(node);
  if (!arrLit)
    return;

  bool hasError = false;

  ResolvedType arrType = inferNodeDataType(arrLit);
  for (const auto &item : arrLit->array) {
    walker(item.get()); // Calling their walkers
  }

  auto sizePerDims = getSizePerDimesion(arrLit);

  // Storing metaData about the array
  auto arrInfo = std::make_shared<SymbolInfo>();
  arrInfo->type = arrType;
  arrInfo->isNullable = false;
  arrInfo->isMutable = false;
  arrInfo->isConstant = false;
  arrInfo->sizePerDimensions = sizePerDims;

  metaData[arrLit] = arrInfo;
}
