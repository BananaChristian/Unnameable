#include "ast.hpp"
#include "semantics.hpp"

void Semantics::registerLiteral(Node *literal, const ResolvedType &type) {
  auto info = std::make_shared<SymbolInfo>();

  info->type().type = type;
  info->type().isNullable = false;
  info->storage().isMutable = false;
  info->storage().isConstant = false;
  metaData[literal] = info;
}

// Walking the data type literals
void Semantics::walkBooleanLiteral(Node *node) {
  auto boolLiteral = dynamic_cast<BooleanLiteral *>(node);
  if (!boolLiteral)
    return;

  auto info = std::make_shared<SymbolInfo>();

  registerLiteral(boolLiteral,
                  ResolvedType::makeBase(DataType::BOOLEAN, "bool"));
}

void Semantics::walkStringLiteral(Node *node) {
  auto strLiteral = dynamic_cast<StringLiteral *>(node);
  if (!strLiteral)
    return;

  registerLiteral(strLiteral,
                  ResolvedType::makeBase(DataType::STRING, "string"));
}

void Semantics::walkFStringLiteral(Node *node){
    auto fStr= dynamic_cast<FStringLiteral*>(node);
    if(!fStr)
        return;
    
    //Call the walker on the crap inside bro
    auto segments=fStr->segments;
    for(const auto &seg:segments){
        if(seg.string_part)
            walker(seg.string_part.get());

        if(!seg.values.empty()){
            for(const auto &val:seg.values){
                walker(val.get());
            }
        }
    }

    registerLiteral(fStr,ResolvedType::makeBase(DataType::STRING,"string"));
}

void Semantics::walkChar8Literal(Node *node) {
  auto char8Literal = dynamic_cast<Char8Literal *>(node);
  if (!char8Literal)
    return;

  registerLiteral(char8Literal,
                  ResolvedType::makeBase(DataType::CHAR8, "char8"));
}

void Semantics::walkChar16Literal(Node *node) {
  auto lit = dynamic_cast<Char16Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::CHAR16, "char16"));
}

void Semantics::walkChar32Literal(Node *node) {
  auto lit = dynamic_cast<Char32Literal *>(node);
  if (!lit)
    return;
  auto info = std::make_shared<SymbolInfo>();

  registerLiteral(lit, ResolvedType::makeBase(DataType::CHAR32, "char32"));
}

void Semantics::walkI8Literal(Node *node) {
  auto lit = dynamic_cast<I8Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::I8, "i8"));
}

void Semantics::walkU8Literal(Node *node) {
  auto lit = dynamic_cast<U8Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::U8, "u8"));
}

void Semantics::walkI16Literal(Node *node) {
  auto lit = dynamic_cast<I16Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::I16, "i16"));
}

void Semantics::walkU16Literal(Node *node) {
  auto lit = dynamic_cast<U16Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::U16, "u16"));
}

void Semantics::walkI32Literal(Node *node) {
  auto intLiteral = dynamic_cast<I32Literal *>(node);
  if (!intLiteral)
    return;

  registerLiteral(intLiteral, ResolvedType::makeBase(DataType::I32, "i32"));
}

void Semantics::walkU32Literal(Node *node) {
  auto intLiteral = dynamic_cast<U32Literal *>(node);
  if (!intLiteral)
    return;

  registerLiteral(intLiteral, ResolvedType::makeBase(DataType::U32, "u32"));
}

void Semantics::walkI64Literal(Node *node) {
  auto lit = dynamic_cast<I64Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::I64, "i64"));
}

void Semantics::walkU64Literal(Node *node) {
  auto lit = dynamic_cast<U64Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::U64, "u64"));
}

void Semantics::walkI128Literal(Node *node) {
  auto lit = dynamic_cast<I128Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::I128, "i128"));
}

void Semantics::walkU128Literal(Node *node) {
  auto lit = dynamic_cast<U128Literal *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::U128, "u128"));
}

void Semantics::walkISIZELiteral(Node *node) {
  auto lit = dynamic_cast<ISIZELiteral *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::ISIZE, "isize"));
}

void Semantics::walkUSIZELiteral(Node *node) {
  auto lit = dynamic_cast<USIZELiteral *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::USIZE, "usize"));
}

void Semantics::walkINTLiteral(Node *node) {
  auto lit = dynamic_cast<INTLiteral *>(node);
  if (!lit)
    return;

  registerLiteral(lit, ResolvedType::makeBase(DataType::I32, "i32"));
}

void Semantics::walkF32Literal(Node *node) {
  auto f32Literal = dynamic_cast<F32Literal *>(node);
  if (!f32Literal)
    return;

  registerLiteral(f32Literal, ResolvedType::makeBase(DataType::F32, "f32"));
}

void Semantics::walkF64Literal(Node *node) {
  auto f64Literal = dynamic_cast<F64Literal *>(node);
  if (!f64Literal)
    return;
  registerLiteral(f64Literal, ResolvedType::makeBase(DataType::F64, "f64"));
}

void Semantics::walkFloatLiteral(Node *node) {
  auto fltLit = dynamic_cast<FloatLiteral *>(node);
  if (!fltLit)
    return;
  registerLiteral(fltLit, ResolvedType::makeBase(DataType::F32, "f32"));
}

// Walking the null literal
void Semantics::walkNullLiteral(Node *node) {
  auto nullLit = dynamic_cast<NullLiteral *>(node);
  if (!nullLit)
    return;

  auto symbol = std::make_shared<SymbolInfo>();
  symbol->type().type =
      ResolvedType::makeBase(DataType::UNKNOWN,
                             "null"); // Unknown data type for now
  metaData[nullLit] = symbol;
}

// Walking the array literal
void Semantics::walkArrayLiteral(Node *node) {
  auto arrLit = dynamic_cast<ArrayLiteral *>(node);
  if (!arrLit)
    return;

  ResolvedType arrType = inferNodeDataType(arrLit);
  for (const auto &item : arrLit->array) {
    walker(item.get()); // Calling their walkers
  }

  auto sizePerDims = getSizePerDimesion(arrLit);

  // Storing metaData about the array
  auto arrInfo = std::make_shared<SymbolInfo>();
  arrInfo->type().type = arrType;
  arrInfo->type().isNullable = false;
  arrInfo->storage().isMutable = false;
  arrInfo->storage().isConstant = false;
  arrInfo->type().sizePerDimensions = sizePerDims;
  metaData[arrLit] = arrInfo;
}
