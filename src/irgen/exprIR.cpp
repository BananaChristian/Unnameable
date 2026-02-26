#include "ast.hpp"
#include "irgen.hpp"
#include <cstdint>
#include <llvm-18/llvm/IR/Constants.h>
#include <stdexcept>

//___________________Literals generator_____________________
llvm::Value *IRGenerator::generateStringLiteral(Node *node) {
  auto strLit = dynamic_cast<StringLiteral *>(node);
  if (!strLit) {
    reportDevBug("Invalid string literal", node->token.line,
                 node->token.column);
  }
  auto it = semantics.metaData.find(strLit);
  if (it == semantics.metaData.end()) {
    reportDevBug("String literal not found in metaData",
                 strLit->expression.line, strLit->expression.column);
  }
  DataType dt = it->second->type.kind;

  if (dt != DataType::STRING) {
    reportDevBug("Invalid type expected 'string'", strLit->expression.line,
                 strLit->expression.column);
  }
  std::string raw = strLit->string_token.TokenLiteral;
  llvm::Constant *strConst =
      llvm::ConstantDataArray::getString(context, raw, true);

  auto *globalStr = new llvm::GlobalVariable(*module, strConst->getType(), true,
                                             llvm::GlobalValue::PrivateLinkage,
                                             strConst, ".str");

  // Pointer to first element
  llvm::Constant *zero =
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
  llvm::Constant *indices[] = {zero, zero};
  llvm::Constant *strPtr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      strConst->getType(), globalStr, indices);

  return strPtr;
}

llvm::Value *IRGenerator::generateChar8Literal(Node *node) {
  auto charLit = dynamic_cast<Char8Literal *>(node);
  if (!charLit) {
    throw std::runtime_error("Invalid char literal");
  }
  auto it = semantics.metaData.find(charLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Char8 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::CHAR8) {
    throw std::runtime_error("Type error: Expected CHAR for CharLiteral");
  }
  std::string tokenLiteral = charLit->char8_token.TokenLiteral;

  char c = decodeCharLiteral(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                static_cast<uint8_t>(c), false);
}

llvm::Value *IRGenerator::generateChar16Literal(Node *node) {
  auto char16Lit = dynamic_cast<Char16Literal *>(node);
  if (!char16Lit) {
    throw std::runtime_error("Invalid char 16 literal");
  }
  auto it = semantics.metaData.find(char16Lit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Char16 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::CHAR16) {
    throw std::runtime_error("Type error: Expected CHAR16 for Char16Literal");
  }
  std::string tokenLiteral = char16Lit->char16_token.TokenLiteral;
  uint16_t c = decodeCharLiteral(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), c, false);
}

llvm::Value *IRGenerator::generateChar32Literal(Node *node) {
  auto char32Lit = dynamic_cast<Char32Literal *>(node);
  if (!char32Lit) {
    throw std::runtime_error("Invalid char32 literal");
  }
  auto it = semantics.metaData.find(char32Lit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Char32 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::CHAR32) {
    throw std::runtime_error("Type error: Expected CHAR32 for Char32Literal");
  }
  std::string tokenLiteral = char32Lit->char32_token.TokenLiteral;
  uint32_t c = decodeChar32Literal(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), c, false);
}

llvm::Value *IRGenerator::generateBooleanLiteral(Node *node) {
  auto boolLit = dynamic_cast<BooleanLiteral *>(node);
  if (!boolLit) {
    throw std::runtime_error("Invalid boolean type");
  }
  auto it = semantics.metaData.find(boolLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Boolean literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::BOOLEAN) {
    throw std::runtime_error("Type error: Expected BOOLEAN for BooleanLiteral");
  }

  bool value = (boolLit->boolean_token.TokenLiteral == "true");

  return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), value);
}

llvm::Value *IRGenerator::generateI8Literal(Node *node) {
  auto i8Lit = dynamic_cast<I8Literal *>(node);
  if (!i8Lit)
    throw std::runtime_error("Invalid i8 literal");

  return generateIntegerLiteral(i8Lit->i8_token.TokenLiteral, 8, true);
}

llvm::Value *IRGenerator::generateU8Literal(Node *node) {
  auto u8Lit = dynamic_cast<U8Literal *>(node);
  if (!u8Lit)
    throw std::runtime_error("Invalid u8 literal");

  return generateIntegerLiteral(u8Lit->u8_token.TokenLiteral, 8, false);
}

llvm::Value *IRGenerator::generateI16Literal(Node *node) {
  auto i16Lit = dynamic_cast<I16Literal *>(node);
  if (!i16Lit)
    throw std::runtime_error("Invalid i16 literal");

  return generateIntegerLiteral(i16Lit->i16_token.TokenLiteral, 16, true);
}

llvm::Value *IRGenerator::generateU16Literal(Node *node) {
  auto u16Lit = dynamic_cast<U16Literal *>(node);
  if (!u16Lit)
    throw std::runtime_error("Invalid u16 literal");

  return generateIntegerLiteral(u16Lit->u16_token.TokenLiteral, 16, false);
}

llvm::Value *IRGenerator::generateI32Literal(Node *node) {
  auto i32Lit = dynamic_cast<I32Literal *>(node);
  if (!i32Lit) {
    throw std::runtime_error("Invalid i32 literal");
  }

  return generateIntegerLiteral(i32Lit->i32_token.TokenLiteral, 32, true);
}

llvm::Value *IRGenerator::generateU32Literal(Node *node) {
  auto u32Lit = dynamic_cast<U32Literal *>(node);
  if (!u32Lit)
    throw std::runtime_error("Invalid u32 literal");

  return generateIntegerLiteral(u32Lit->u32_token.TokenLiteral, 32, false);
}

llvm::Value *IRGenerator::generateI64Literal(Node *node) {
  auto i64Lit = dynamic_cast<I64Literal *>(node);
  if (!i64Lit)
    throw std::runtime_error("Invalid i64 literal");

  return generateIntegerLiteral(i64Lit->i64_token.TokenLiteral, 64, true);
}

llvm::Value *IRGenerator::generateU64Literal(Node *node) {
  auto u64Lit = dynamic_cast<U64Literal *>(node);
  if (!u64Lit)
    throw std::runtime_error("Invalid u64 literal");

  return generateIntegerLiteral(u64Lit->u64_token.TokenLiteral, 64, false);
}

llvm::Value *IRGenerator::generateI128Literal(Node *node) {
  auto i128Lit = dynamic_cast<I128Literal *>(node);
  if (!i128Lit)
    throw std::runtime_error("Invalid i128 literal");

  return generateIntegerLiteral(i128Lit->i128_token.TokenLiteral, 128, true);
}

llvm::Value *IRGenerator::generateU128Literal(Node *node) {
  auto u128Lit = dynamic_cast<U128Literal *>(node);
  if (!u128Lit)
    throw std::runtime_error("Invalid u128 literal");

  return generateIntegerLiteral(u128Lit->u128_token.TokenLiteral, 128, false);
}

llvm::Value *IRGenerator::generateISIZELiteral(Node *node) {
  auto lit = dynamic_cast<ISIZELiteral *>(node);
  unsigned int ptrWidth = layout->getPointerSizeInBits();

  return generateIntegerLiteral(lit->isize_token.TokenLiteral, ptrWidth, true);
}

llvm::Value *IRGenerator::generateUSIZELiteral(Node *node) {
  auto lit = dynamic_cast<USIZELiteral *>(node);
  unsigned int ptrWidth = layout->getPointerSizeInBits();

  return generateIntegerLiteral(lit->usize_token.TokenLiteral, ptrWidth, false);
}

llvm::Value *IRGenerator::generateF32Literal(Node *node) {
  auto f32Lit = dynamic_cast<F32Literal *>(node);
  if (!f32Lit) {
    throw std::runtime_error("Invalid f32 literal");
  }
  auto it = semantics.metaData.find(f32Lit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("F32 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::F32) {
    throw std::runtime_error("Type error: Expected F32 for F32Literal ");
  }
  float value = std::stof(f32Lit->f32_token.TokenLiteral);
  return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
}

llvm::Value *IRGenerator::generateF64Literal(Node *node) {
  auto f64Lit = dynamic_cast<F64Literal *>(node);
  if (!f64Lit) {
    throw std::runtime_error("Invalid f64 literal");
  }
  auto it =
      semantics.metaData.find(f64Lit); // Creating an iterator to find specific
                                       // meta data about the double literal
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("f64 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::F64) {
    throw std::runtime_error("Type error: Expected f64 for f64Literal");
  }
  // Checking if we have metaData about the double literal and if so we check to
  // see if the data type is double
  double value = std::stod(
      f64Lit->f64_token.TokenLiteral); // Converting the double literal from a
                                       // string to a double
  return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context),
                               value); // Returning double value
}

// Generator function for identifier expression
llvm::Value *IRGenerator::generateIdentifierExpression(Node *node) {
  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr) {
    reportDevBug("Invalid identifier node", node->token.line,
                 node->token.column);
  }

  const std::string &identName = identExpr->identifier.TokenLiteral;

  // Lookup symbol
  auto metaIt = semantics.metaData.find(identExpr);
  if (metaIt == semantics.metaData.end()) {
    reportDevBug("Unidentified identifier '" + identName + "'",
                 identExpr->identifier.line, identExpr->identifier.column);
  }

  auto sym = metaIt->second;

  // Get address and possible pending free
  llvm::Value *address = generateIdentifierAddress(identExpr);
  if (!address) {
    reportDevBug("No address for '" + identName + "'",
                 identExpr->identifier.line, identExpr->identifier.column);
  }

  // Component instance -> return pointer to the struct instance (address is
  // already correct)
  auto compIt = componentTypes.find(sym->type.resolvedName);
  if (compIt != componentTypes.end()) {
    return address;
  }

  // Heap scalar: variableAddr is a T* (runtime pointer). Load T from it.
  if (sym->isSage) {
    llvm::Type *elemTy = sym->llvmType;
    if (!elemTy) {
      reportDevBug("No type for sage scalar '" + identName + "'",
                   identExpr->identifier.line, identExpr->identifier.column);
    }

    // variableAddr should be T* (address of object). If not, bitcast it.
    llvm::PointerType *expectedPtrTy = elemTy->getPointerTo();
    if (address->getType() != expectedPtrTy)
      address = funcBuilder.CreateBitCast(address, expectedPtrTy,
                                          identName + "_ptr_typed");

    // Load the value
    llvm::Value *loadedVal =
        funcBuilder.CreateLoad(elemTy, address, identName + "_val");

    return loadedVal;
  }
  if (sym->isHeap) {
    llvm::Type *elemTy = sym->llvmType;
    llvm::Value *actualHeapAddr = address;

    if (!elemTy) {
      reportDevBug("No type for heap scalar '" + identName + "'",
                   identExpr->identifier.line, identExpr->identifier.column);
    }

    // Grab the value from the heap address
    llvm::Value *loadedVal =
        funcBuilder.CreateLoad(elemTy, actualHeapAddr, identName + "_val");

    // If the inhibitor is on that means there is a parent who wants this dude
    // to relax but if he is off proceed to shoot
    if (!inhibitCleanUp)
      emitCleanup(identExpr, sym);

    return loadedVal;
  }

  // Non-heap scalar: variableAddr is a pointer to T, just load
  llvm::Type *loadedType;
  if (sym->isRef) {
    loadedType = getLLVMType(semantics.peelRef(sym->type));
  } else {
    loadedType = getLLVMType(sym->type);
  }

  if (!loadedType) {
    reportDevBug("Type mapper failed for scalar '" + identName + "'",
                 identExpr->identifier.line, identExpr->identifier.column);
  }

  // variableAddr should already be a pointer, load from it
  llvm::Value *val =
      funcBuilder.CreateLoad(loadedType, address, identName + "_val");

  return val;
}

void IRGenerator::flattenArrayLiteral(ArrayLiteral *arrLit,
                                      std::vector<llvm::Constant *> &flatElems,
                                      llvm::Type *&baseType) {
  for (auto &element : arrLit->array) {
    if (auto nested = dynamic_cast<ArrayLiteral *>(element.get())) {
      flattenArrayLiteral(nested, flatElems, baseType);
    } else {
      llvm::Value *val = generateExpression(element.get());
      auto *constVal = llvm::dyn_cast<llvm::Constant>(val);
      if (!constVal)
        throw std::runtime_error("Array literal element must be constant");

      if (!baseType)
        baseType = constVal->getType();
      flatElems.push_back(constVal);
    }
  }
}

llvm::Value *IRGenerator::generateArrayLiteral(Node *node) {
  auto arrLit = dynamic_cast<ArrayLiteral *>(node);
  if (!arrLit)
    throw std::runtime_error("Invalid array literal");

  // Steamroll the literal into a flat vector of scalars
  std::vector<llvm::Constant *> flatElems;
  llvm::Type *baseType = nullptr;
  flattenArrayLiteral(arrLit, flatElems, baseType);

  if (flatElems.empty() || !baseType) {
    throw std::runtime_error("Empty or invalid array literal");
  }

  // Create a flat LLVM Array Type [TotalCount x BaseType]
  llvm::ArrayType *flatArrayTy =
      llvm::ArrayType::get(baseType, flatElems.size());
  llvm::Constant *constantData =
      llvm::ConstantArray::get(flatArrayTy, flatElems);

  //  Shove it into a Global Variable (The Data Segment)
  // This is like 'static const int hidden_data[] = {1, 2, 3, 4};' in C
  auto *globalData = new llvm::GlobalVariable(
      *module, flatArrayTy,
      true, // Constant? Yes.
      llvm::GlobalValue::PrivateLinkage, constantData, "flat_array_literal");
  globalData->setAlignment(layout->getABITypeAlign(baseType));

  // Return the Pointer to the start of the data
  // In LLVM IR: bitcast [4 x i32]* @flat_array_literal to i32*
  return funcBuilder.CreateBitCast(globalData, funcBuilder.getPtrTy());
}

llvm::Value *IRGenerator::generateNullLiteral(Node *node) {
  auto nullLit = dynamic_cast<NullLiteral *>(node);
  if (!nullLit)
    throw std::runtime_error("Invalid null literal");

  auto nullMeta = semantics.metaData[nullLit];
  if (!nullMeta)
    throw std::runtime_error("Could not find null metaData");

  ResolvedType stampedType = semantics.metaData[nullLit]->type;
  llvm::Type *llvmType = getLLVMType(stampedType);

  return llvm::ConstantAggregateZero::get(llvmType);
}

//_____________________Infix expression_________________________
llvm::Value *IRGenerator::generateInfixExpression(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  if (!infix)
    throw std::runtime_error("Invalid infix expression");

  llvm::Value *result = nullptr;
  if (infix->operat.type == TokenType::SCOPE_OPERATOR)
    return handleEnumAccess(infix);

  auto infixSym = semantics.getSymbolFromMeta(infix);

  auto leftSym = semantics.getSymbolFromMeta(infix->left_operand.get());
  auto rightSym = semantics.getSymbolFromMeta(infix->right_operand.get());

  // Helper lambda for integer type promotion
  auto promoteInt = [&](llvm::Value *val, DataType fromType,
                        DataType toType) -> llvm::Value * {
    unsigned fromBits = getIntegerBitWidth(fromType);
    unsigned toBits = getIntegerBitWidth(toType);
    if (fromBits == 0 || toBits == 0)
      return val; // Not integer type
    if (fromBits == toBits)
      return val; // Same bit width, no promotion needed

    bool fromSigned = isSignedInteger(fromType);
    if (toBits > fromBits) {
      if (fromSigned)
        return funcBuilder.CreateSExt(
            val, llvm::IntegerType::get(context, toBits), "sexttmp");
      else
        return funcBuilder.CreateZExt(
            val, llvm::IntegerType::get(context, toBits), "zexttmp");
    } else {
      // Truncation if needed (rare, probably invalid here)
      return funcBuilder.CreateTrunc(
          val, llvm::IntegerType::get(context, toBits), "trunctmp");
    }
  };

  inhibitCleanUp = true;

  llvm::Value *leftVal = generateExpression(infix->left_operand.get());
  llvm::Value *rightVal = generateExpression(infix->right_operand.get());

  if (infix->operat.type == TokenType::FULLSTOP)
    result = handleMemberAccess(infix, leftVal, rightVal, leftSym, rightSym);

  auto resultType = infixSym->type;
  // Promote operands to widest integer type among left, right, and result
  if (isIntegerType(resultType.kind)) {
    unsigned targetBits = getIntegerBitWidth(resultType.kind);
    leftVal = promoteInt(leftVal, leftSym->type.kind, resultType.kind);
    rightVal = promoteInt(rightVal, rightSym->type.kind, resultType.kind);
  }

  // Handle Null Coalesce (??)
  if (infix->operat.type == TokenType::COALESCE)
    result = handleCoalesce(infix, leftVal);

  // Handle boolean logical operators (AND, OR)
  if (infix->operat.type == TokenType::AND ||
      infix->operat.type == TokenType::OR)
    result = handleLogical(infix, leftVal, rightVal, leftSym, rightSym);

  // Handle floating point conversions
  if (resultType.kind == DataType::F32) {
    if (isIntegerType(leftSym->type.kind))
      leftVal = funcBuilder.CreateSIToFP(
          leftVal, llvm::Type::getFloatTy(context), "inttof32");
    if (isIntegerType(rightSym->type.kind))
      rightVal = funcBuilder.CreateSIToFP(
          rightVal, llvm::Type::getFloatTy(context), "inttof32");
  } else if (resultType.kind == DataType::F64) {
    if (isIntegerType(leftSym->type.kind))
      leftVal = funcBuilder.CreateSIToFP(
          leftVal, llvm::Type::getDoubleTy(context), "inttof64");
    if (isIntegerType(rightSym->type.kind))
      rightVal = funcBuilder.CreateSIToFP(
          rightVal, llvm::Type::getDoubleTy(context), "inttof64");
  }

  // Comparison operators - different for signed/unsigned integers
  if (ComparisonOperator(infix->operat.type))
    result = handleComparison(infix, leftVal, rightVal, leftSym, rightSym);

  // Arithmetic operators
  if (ArithmeticOrBitwiseOperator(infix->operat.type))
    result =
        handleArithmeticAndBitwise(infix, leftVal, rightVal, leftSym, rightSym);

  inhibitCleanUp = false;
  emitInfixClean(infix, leftSym, rightSym);
  return result;
}

//__________________Prefix expression______________________
llvm::Value *IRGenerator::generatePrefixExpression(Node *node) {
  auto prefix = dynamic_cast<PrefixExpression *>(node);
  if (!prefix)
    throw std::runtime_error("Invalid prefix expression");

  if (isGlobalScope)
    throw std::runtime_error(
        "Executable statements arent allowed in the global scope");

  llvm::Value *operand = generateExpression(prefix->operand.get());

  if (!operand)
    throw std::runtime_error("Failed to generate IR for prefix operand");

  const std::string &name = prefix->operand->expression.TokenLiteral;
  auto prefixIt = semantics.metaData.find(prefix);
  if (prefixIt == semantics.metaData.end()) {
    throw std::runtime_error("Unknown variable '" + name + "' in prefix");
  }

  ResolvedType resultType = prefixIt->second->type;

  switch (prefix->operat.type) {
  case TokenType::MINUS: {
    if (resultType.kind == DataType::F32 || resultType.kind == DataType::F64)
      return funcBuilder.CreateFNeg(operand, llvm::Twine("fnegtmp"));
    else if (isIntegerType(resultType.kind))
      return funcBuilder.CreateNeg(operand, llvm::Twine("negtmp"));
    else
      throw std::runtime_error("Unsupported type for unary minus");
  }
  case TokenType::BITWISE_NOT:
    return funcBuilder.CreateNot(operand, llvm::Twine("bitnottmp"));
  case TokenType::BANG: { // If it's a pointer, we don't 'NOT' it, we compare it
                          // to NULL
    if (operand->getType()->isPointerTy()) {
      // Logic: !p  is equivalent to  p == null
      llvm::Value *nullPtr = llvm::ConstantPointerNull::get(
          llvm::cast<llvm::PointerType>(operand->getType()));
      return funcBuilder.CreateICmpEQ(operand, nullPtr, "ptr_is_null");
    }
    return funcBuilder.CreateNot(operand, llvm::Twine("boolnottmp"));
  }

  case TokenType::PLUS_PLUS:
  case TokenType::MINUS_MINUS: {
    auto ident = dynamic_cast<Identifier *>(prefix->operand.get());
    if (!ident)
      throw std::runtime_error("Prefix ++/-- must be used on a variable");

    const std::string &name = ident->expression.TokenLiteral;
    auto identIt = semantics.metaData.find(ident);
    if (identIt == semantics.metaData.end()) {
      throw std::runtime_error("Udefined variable '" + name + "'");
    }
    llvm::Value *address = generateIdentifierAddress(ident);
    // Inject the free checker
    emitCleanup(ident, identIt->second);

    if (!address)
      throw std::runtime_error("Null variable pointer for: " +
                               ident->identifier.TokenLiteral);

    // Get the type from resultType instead of getPointerElementType
    llvm::Type *varType = getLLVMType(resultType);
    if (!varType)
      throw std::runtime_error("Invalid type for variable: " +
                               ident->identifier.TokenLiteral);

    llvm::Value *loaded =
        funcBuilder.CreateLoad(varType, address, llvm::Twine("loadtmp"));

    llvm::Value *delta = nullptr;
    if (resultType.kind == DataType::F32 || resultType.kind == DataType::F64) {
      delta = llvm::ConstantFP::get(varType, 1.0);
    } else if (isIntegerType(resultType.kind)) {
      unsigned bits = getIntegerBitWidth(resultType.kind);
      delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
    } else {
      throw std::runtime_error("Unsupported type for ++/--");
    }

    llvm::Value *updated = nullptr;
    if (prefix->operat.type == TokenType::PLUS_PLUS)
      updated =
          (resultType.kind == DataType::F32 || resultType.kind == DataType::F64)
              ? funcBuilder.CreateFAdd(loaded, delta,
                                       llvm::Twine("preincfptmp"))
              : funcBuilder.CreateAdd(loaded, delta, llvm::Twine("preinctmp"));
    else
      updated =
          (resultType.kind == DataType::F32 || resultType.kind == DataType::F64)
              ? funcBuilder.CreateFSub(loaded, delta,
                                       llvm::Twine("predecfptmp"))
              : funcBuilder.CreateSub(loaded, delta, llvm::Twine("predectmp"));

    funcBuilder.CreateStore(updated, address);

    /*{for (const auto pendingFree : addrAndFree.pendingFrees)
    funcBuilder.Insert(pendingFree);}*/

    return updated;
  }

  default:
    throw std::runtime_error(
        "Unsupported prefix operator: " + prefix->operat.TokenLiteral +
        " at line " + std::to_string(prefix->operat.line));
  }
}

//__________________Postfix expression______________________
llvm::Value *IRGenerator::generatePostfixExpression(Node *node) {
  auto postfix = dynamic_cast<PostfixExpression *>(node);
  if (!postfix)
    throw std::runtime_error("Invalid postfix expression");

  auto identifier = dynamic_cast<Identifier *>(postfix->operand.get());
  if (!identifier)
    throw std::runtime_error("Postfix operand must be a variable");
  auto identName = identifier->identifier.TokenLiteral;
  auto identIt = semantics.metaData.find(identifier);

  llvm::Value *address = generateIdentifierAddress(identifier);

  emitCleanup(identifier, identIt->second);

  if (!address)
    throw std::runtime_error("Null variable pointer for: " +
                             identifier->identifier.TokenLiteral);

  auto postfixIt = semantics.metaData.find(postfix);
  if (postfixIt == semantics.metaData.end()) {
    throw std::runtime_error("Variable '" + identName + "' does not exist");
  }

  ResolvedType resultType = postfixIt->second->type;

  // Get the type from resultType for CreateLoad
  llvm::Type *varType = getLLVMType(resultType);
  if (!varType)
    throw std::runtime_error("Invalid type for variable: " +
                             identifier->identifier.TokenLiteral);

  llvm::Value *originalValue =
      funcBuilder.CreateLoad(varType, address, llvm::Twine("loadtmp"));

  llvm::Value *delta = nullptr;
  if (resultType.kind == DataType::F32 || resultType.kind == DataType::F64) {
    delta = llvm::ConstantFP::get(varType, 1.0);
  } else if (isIntegerType(resultType.kind)) {
    unsigned bits = getIntegerBitWidth(resultType.kind);
    delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
  } else {
    throw std::runtime_error("Unsupported type for ++/--");
  }

  llvm::Value *updatedValue = nullptr;

  if (postfix->operator_token.type == TokenType::PLUS_PLUS)
    updatedValue =
        (resultType.kind == DataType::F32 || resultType.kind == DataType::F64)
            ? funcBuilder.CreateFAdd(originalValue, delta, llvm::Twine("finc"))
            : funcBuilder.CreateAdd(originalValue, delta, llvm::Twine("inc"));
  else if (postfix->operator_token.type == TokenType::MINUS_MINUS)
    updatedValue =
        (resultType.kind == DataType::F32 || resultType.kind == DataType::F64)
            ? funcBuilder.CreateFSub(originalValue, delta, llvm::Twine("fdec"))
            : funcBuilder.CreateSub(originalValue, delta, llvm::Twine("dec"));
  else
    throw std::runtime_error("Unsupported postfix operator: " +
                             postfix->operator_token.TokenLiteral +
                             " at line " +
                             std::to_string(postfix->operator_token.line));

  funcBuilder.CreateStore(updatedValue, address);

  /*{for (const auto &pendingFree : addrAndFree.pendingFrees)
  funcBuilder.Insert(pendingFree);}*/

  // Return original value since postfix
  return originalValue;
}

llvm::Value *IRGenerator::generateUnwrapExpression(Node *node) {
  auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node);
  if (!unwrapExpr)
    throw std::runtime_error("Invalid unwrap expression call");

  auto unIt = semantics.metaData.find(unwrapExpr);
  if (unIt == semantics.metaData.end())
    throw std::runtime_error("Missing unwrap metaData");

  if (unIt->second->hasError)
    throw std::runtime_error("Error detected");

  if (isGlobalScope)
    throw std::runtime_error("Cannot unwrap in global scope");

  llvm::Value *box = generateExpression(unwrapExpr->expr.get());

  if (!box->getType()->isStructTy()) {
    std::string typeStr;
    llvm::raw_string_ostream rso(typeStr);
    box->getType()->print(rso);
    std::cout << "[CRITICAL] Unwrap expected a struct, but got: " << rso.str()
              << "\n";
    // This will prove that test() is returning the wrong physical thing.
  }

  llvm::Function *currentFunc = funcBuilder.GetInsertBlock()->getParent();

  llvm::BasicBlock *successBB =
      llvm::BasicBlock::Create(context, "unwrap.success", currentFunc);
  llvm::BasicBlock *panicBB =
      llvm::BasicBlock::Create(context, "unwrap.panic", currentFunc);

  llvm::Value *isPresent = funcBuilder.CreateExtractValue(box, 0, "is_present");
  funcBuilder.CreateCondBr(isPresent, successBB, panicBB);

  funcBuilder.SetInsertPoint(panicBB);

  llvm::Function *trap =
      llvm::Intrinsic::getDeclaration(module.get(), llvm::Intrinsic::trap);
  funcBuilder.CreateCall(trap);
  funcBuilder.CreateUnreachable();

  funcBuilder.SetInsertPoint(successBB);

  return funcBuilder.CreateExtractValue(box, 1, "unwrapped_val");
}

llvm::Value *IRGenerator::generateCastExpression(Node *node) {
  auto castExpr = dynamic_cast<CastExpression *>(node);
  if (!castExpr)
    throw std::runtime_error("Invalid cast expression");

  auto it = semantics.metaData.find(castExpr);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Failed to find cast metaData");

  auto castSym = it->second;
  if (castSym->hasError)
    throw std::runtime_error("Error detected in cast");

  llvm::Value *sourceVal = generateExpression(castExpr->expr.get());
  if (!sourceVal)
    throw std::runtime_error("Failed to generate source value");

  llvm::Type *srcType = sourceVal->getType();
  llvm::Type *dstType = getLLVMType(castSym->type);

  if (srcType == dstType)
    return sourceVal;

  if (srcType->isIntegerTy() && dstType->isIntegerTy()) {
    unsigned srcBits = srcType->getIntegerBitWidth();
    unsigned dstBits = dstType->getIntegerBitWidth();

    if (dstBits < srcBits) {
      return funcBuilder.CreateTrunc(sourceVal, dstType, "cast_trunc");
    } else {
      if (isUnsigned(semantics.metaData[castExpr->expr.get()]->type)) {
        return funcBuilder.CreateZExt(sourceVal, dstType, "cast_zext");
      } else {
        return funcBuilder.CreateSExt(sourceVal, dstType, "cast_sext");
      }
    }
  }

  // Floating Point to Integer
  if (srcType->isFloatingPointTy() && dstType->isIntegerTy()) {
    return funcBuilder.CreateFPToSI(sourceVal, dstType, "cast_fptosi");
  }

  // Integer to Floating Point
  if (srcType->isIntegerTy() && dstType->isFloatingPointTy()) {
    return funcBuilder.CreateSIToFP(sourceVal, dstType, "cast_sitofp");
  }

  // Float to Double (e.g., float to double)
  if (srcType->isFloatingPointTy() && dstType->isFloatingPointTy()) {
    if (dstType->getFPMantissaWidth() > srcType->getFPMantissaWidth())
      return funcBuilder.CreateFPExt(sourceVal, dstType, "cast_fpext");
    else
      return funcBuilder.CreateFPTrunc(sourceVal, dstType, "cast_fptrunc");
  }

  throw std::runtime_error("Unhandled cast from " + castSym->type.resolvedName);
}

llvm::Value *IRGenerator::generateBitcastExpression(Node *node) {
  auto bitcastExpr = dynamic_cast<BitcastExpression *>(node);
  if (!bitcastExpr)
    throw std::runtime_error("Invalid bitcast expression");

  // 1. Retrieve the Semantic Metadata we fixed earlier
  auto it = semantics.metaData.find(bitcastExpr);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Failed to find bitcast metaData");

  auto bitcastSym = it->second;
  if (bitcastSym->hasError)
    throw std::runtime_error("Error detected in bitcast: skipping generation");

  // Generate the source value (the bits we are reinterpreting)
  llvm::Value *sourceVal = generateExpression(bitcastExpr->expr.get());
  if (!sourceVal)
    throw std::runtime_error("Failed to generate source value");

  llvm::Type *srcType = sourceVal->getType();
  llvm::Type *dstType = getLLVMType(bitcastSym->type);

  // If types are already identical, just return the value
  if (srcType == dstType)
    return sourceVal;

  // Handle Pointer <-> Integer Reinterpretation
  // LLVM does not allow 'BitCast' between different address spaces or
  // between the Integer and Pointer register classes.

  if (srcType->isPointerTy() && dstType->isIntegerTy()) {
    return funcBuilder.CreatePtrToInt(sourceVal, dstType, "bitcast_ptr_to_int");
  }

  if (srcType->isIntegerTy() && dstType->isPointerTy()) {
    // If the integer is smaller than the pointer (e.g., i32 to i64 ptr),
    // we need to sign-extend it first to ensure bits like '-1' fill the
    // address.
    auto ptrWidth = module->getDataLayout().getPointerSizeInBits();
    if (srcType->getIntegerBitWidth() < ptrWidth) {
      sourceVal = funcBuilder.CreateSExt(
          sourceVal, funcBuilder.getIntNTy(ptrWidth), "bitcast_sext");
    }
    return funcBuilder.CreateIntToPtr(sourceVal, dstType, "bitcast_int_to_ptr");
  }

  // Handle Pointer <-> Pointer or Same-Size Type Reinterpretation
  // This covers things like ptr u8 to ptr STRUCT, or u64 to i64.
  // We use CreateBitCast here because the bit-width must be identical.
  return funcBuilder.CreateBitCast(sourceVal, dstType, "bitcast_raw");
}

llvm::Value *IRGenerator::generateArraySubscriptExpression(Node *node) {
  auto arrExpr = dynamic_cast<ArraySubscript *>(node);
  if (!arrExpr)
    throw std::runtime_error("Invalid array access expression");

  auto arrMetaIt = semantics.metaData.find(arrExpr);
  if (arrMetaIt == semantics.metaData.end()) {
    throw std::runtime_error("Could not find array subscript metaData");
  }
  auto arrSym = arrMetaIt->second;

  if (arrSym->hasError) {
    throw std::runtime_error("Semantic error was detected in array subscript");
  }

  llvm::Value *ptr = generateArraySubscriptAddress(node);

  llvm::Type *elemTy = getLLVMType(arrSym->type);
  auto loadedVal = funcBuilder.CreateLoad(elemTy, ptr, "arr_elem_load");

  emitCleanup(arrExpr, arrSym);

  return loadedVal;
}

llvm::Value *IRGenerator::generateSizeOfExpression(Node *node) {
  auto sizeOf = dynamic_cast<SizeOfExpression *>(node);
  if (!sizeOf)
    throw std::runtime_error("Invalid sizeof expression");

  Expression *typeExpr = sizeOf->type.get();
  ResolvedType type = semantics.inferNodeDataType(typeExpr);

  llvm::Type *llvmTY = getLLVMType(type);

  uint64_t size = layout->getTypeAllocSize(llvmTY);
  llvm::IntegerType *usizeTy = layout->getIntPtrType(context);
  return llvm::ConstantInt::get(usizeTy, size);
}

llvm::Value *IRGenerator::generateSelfExpression(Node *node) {
  auto selfExpr = dynamic_cast<SelfExpression *>(node);
  if (!selfExpr)
    throw std::runtime_error("Invalid self expression");

  std::cout << "[IR] Generating IR for self expression: "
            << selfExpr->toString() << "\n";

  const std::string &compName =
      currentComponent->component_name->expression.TokenLiteral;

  // Lookup LLVM struct for top-level component
  llvm::StructType *currentStructTy = nullptr;
  auto it = componentTypes.find(compName);
  if (it == componentTypes.end())
    throw std::runtime_error("Component '" + compName +
                             "' not found in componentTypes");

  currentStructTy = it->second;

  // --- Load 'self' pointer ---
  llvm::AllocaInst *selfAlloca = currentFunctionSelfMap[currentFunction];
  if (!selfAlloca)
    throw std::runtime_error("'self' access outside component method");

  llvm::Value *currentPtr = funcBuilder.CreateLoad(
      currentStructTy->getPointerTo(), selfAlloca, "self_load");

  // --- Semantic chain walk ---
  auto ctIt = semantics.customTypesTable.find(compName);
  if (ctIt == semantics.customTypesTable.end())
    throw std::runtime_error("Unknown component '" + compName + "'");

  auto currentTypeInfo = ctIt->second;
  std::shared_ptr<MemberInfo> lastMemberInfo = nullptr;

  for (size_t i = 0; i < selfExpr->fields.size(); ++i) {
    auto ident = dynamic_cast<Identifier *>(selfExpr->fields[i].get());
    if (!ident)
      throw std::runtime_error("Expected identifier in self chain");

    std::string currentTypeName = currentTypeInfo->type.resolvedName;
    if (currentTypeInfo->type.isPointer)
      currentTypeName =
          semantics.stripPtrSuffix(currentTypeInfo->type.resolvedName);
    else if (currentTypeInfo->type.isRef)
      currentTypeName =
          semantics.stripRefSuffix(currentTypeInfo->type.resolvedName);

    std::string fieldName = ident->identifier.TokenLiteral;

    auto memIt = currentTypeInfo->members.find(fieldName);
    if (memIt == currentTypeInfo->members.end())
      throw std::runtime_error("Field not found in '" + currentTypeName + "'");

    lastMemberInfo = memIt->second;

    // --- GEP for this field ---
    auto llvmIt = llvmCustomTypes.find(currentTypeName);
    if (llvmIt == llvmCustomTypes.end())
      throw std::runtime_error("LLVM struct missing for type '" +
                               currentTypeName + "'");

    llvm::StructType *structTy = llvmIt->second;

    currentPtr = funcBuilder.CreateStructGEP(
        structTy, currentPtr, lastMemberInfo->memberIndex, fieldName + "_ptr");

    // --- Drill into nested type if needed ---
    if (lastMemberInfo->type.kind == DataType::COMPONENT ||
        lastMemberInfo->type.kind == DataType::RECORD) {
      std::string lookUpName = lastMemberInfo->type.resolvedName;
      if (lastMemberInfo->type.isPointer) {
        lookUpName =
            semantics.stripPtrSuffix(lastMemberInfo->type.resolvedName);
      } else if (lastMemberInfo->type.isRef)
        lookUpName =
            semantics.stripRefSuffix(lastMemberInfo->type.resolvedName);
      auto nestedIt = semantics.customTypesTable.find(lookUpName);
      if (nestedIt == semantics.customTypesTable.end())
        throw std::runtime_error("Nested type '" + lookUpName + "'not found");

      currentTypeInfo = nestedIt->second;
    } else {
      // primitive reached, stop drilling
      currentTypeInfo = nullptr;
    }
  }

  // --- Final load ---
  llvm::Type *finalTy = getLLVMType(lastMemberInfo->type);
  return funcBuilder.CreateLoad(finalTy, currentPtr,
                                selfExpr->fields.back()->toString() + "_val");
}

llvm::Value *IRGenerator::generateDereferenceExpression(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
  if (!derefExpr)
    return nullptr;

  auto derefSym = semantics.getSymbolFromMeta(derefExpr);

  Node *current = node;
  int derefCount = 0;
  while (auto nested = dynamic_cast<DereferenceExpression *>(current)) {
    derefCount++;
    current = nested->identifier.get();
  }
  auto identNode = dynamic_cast<Identifier *>(current);

  // Get the address of the variable (e.g., the alloca on the stack)
  llvm::Value *addr = generateIdentifierAddress(identNode);
  derefSym->llvmValue = addr;
  auto meta = semantics.getSymbolFromMeta(node);

  // Load the actual address stored in the variable.
  // This MUST be a pointer type if we intend to dereference it further.
  llvm::Type *ptrTy = llvm::PointerType::getUnqual(context);
  addr = funcBuilder.CreateLoad(ptrTy, addr, "base_address");

  // Perform the dereference hops
  for (int i = 0; i < derefCount; i++) {
    // If we have more hops to go, we are still loading addresses (pointers)
    // If this is the LAST hop, we load the actual data (e.g., i32)
    bool isLastHop = (i == derefCount - 1);

    llvm::Type *loadTy = isLastHop ? getLLVMType(meta->type) : ptrTy;

    // Create the load.
    // If it's the last hop, this returns the value (i32).
    // Otherwise, it returns the next address in the chain.
    addr = funcBuilder.CreateLoad(loadTy, addr,
                                  isLastHop ? "deref_val" : "ptr_hop");
  }

  emitCleanup(derefExpr, meta);

  return addr;
}

llvm::Value *IRGenerator::generateMoveExpression(Node *node) {
  auto moveExpr = dynamic_cast<MoveExpression *>(node);
  if (!moveExpr)
    return nullptr;

  llvm::Value *sourceAddr = generateAddress(moveExpr->expr.get());

  llvm::Value *baton =
      funcBuilder.CreateLoad(funcBuilder.getPtrTy(), sourceAddr, "move_tmp");

  auto nullPtr = llvm::ConstantPointerNull::get(funcBuilder.getPtrTy());
  if (sourceAddr) {
    funcBuilder.CreateStore(nullPtr, sourceAddr);
  }

  return baton;
}

llvm::Value *IRGenerator::generateAddressExpression(Node *node) {
  auto addrExpr = dynamic_cast<AddressExpression *>(node);
  if (!addrExpr) {
    errorHandler.addHint("Sent the wrong node to the address generator");
    reportDevBug("Invalid address expression", node->token.line,
                 node->token.column);
  }

  logInternal("Handling address expression generation");

  auto metaIt = semantics.metaData.find(addrExpr);
  if (metaIt == semantics.metaData.end()) {
    errorHandler.addHint("Semantics failed to register the address metaData");
    reportDevBug("Could not find address metadata", addrExpr->expression.line,
                 addrExpr->expression.column);
  }

  auto sym = metaIt->second;

  auto targetSym = sym->targetSymbol;
  llvm::Value *variablePtr = targetSym->llvmValue;
  if (!variablePtr) {
    reportDevBug("No value was assigned to the target",
                 addrExpr->expression.line, addrExpr->expression.column);
  }
  sym->llvmValue = variablePtr;

  emitCleanup(addrExpr, sym);

  return variablePtr;
}
