#include "ast.hpp"
#include "irgen.hpp"
#include "semantics.hpp"
#include <cstdint>
#include <llvm-18/llvm/IR/Constants.h>
#include <llvm-18/llvm/Support/Alignment.h>
#include <stdexcept>

//___________________Literals generator_____________________
llvm::Value *IRGenerator::generateStringLiteral(Node *node) {
  auto strLit = dynamic_cast<StringLiteral *>(node);
  if (!strLit) 
    reportDevBug("Invalid string literal", node);
  

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

llvm::Value *IRGenerator::generateFStringLiteral(Node *node){
    auto fStr=dynamic_cast<FStringLiteral*>(node);
    if(!fStr)
        reportDevBug("Invalid f-string literal",node);
    
    auto i8Ty = llvm::Type::getInt8Ty(context);
    auto i64Ty=llvm::Type::getInt64Ty(context);
    
    llvm::Value *requiredSize=calculateFStringSize(fStr);

    llvm::Value* buffer = funcBuilder.CreateAlloca(i8Ty, 0, requiredSize, "fstr_tmp");
    
    llvm::Value* cursor = funcBuilder.getInt64(0);
    
    inhibitCleanUp=true;
    
    for(const auto &seg:fStr->segments){
        //The literal part
        if(seg.string_part){
            auto str=dynamic_cast<StringLiteral*>(seg.string_part.get());
            uint64_t len =str->string_token.TokenLiteral.length();
            llvm::Value *litPtr=generateExpression(str);
            llvm::Value *writePos=funcBuilder.CreateGEP(i8Ty,buffer,cursor);
            
            funcBuilder.CreateMemCpy(writePos,llvm::MaybeAlign(1),litPtr,llvm::MaybeAlign(1),len);
            cursor=funcBuilder.CreateAdd(cursor,llvm::ConstantInt::get(i64Ty,len));
        }
        
        //Add the hole placeholder
        for(auto &valExpr:seg.values){
            llvm::Value *val=generateExpression(valExpr.get());
            if(!val)
                reportDevBug("Failed to generate value ",valExpr.get());
            
            auto sym=semantics.getSymbolFromMeta(valExpr.get());
            if(!sym)
                reportDevBug("Failed to get value symbol info",valExpr.get());
            
            llvm::Value *strVal=stringizeValue(val,sym->type().type);
            llvm::Value *strLen=funcBuilder.CreateCall(getOrDeclareStrlen(),{strVal});
            
            llvm::Value *writePos=funcBuilder.CreateGEP(i8Ty,buffer,cursor);
            funcBuilder.CreateMemCpy(writePos,llvm::MaybeAlign(1),strVal,llvm::MaybeAlign(1),strLen);
            
            cursor=funcBuilder.CreateAdd(cursor,strLen);
        }
    }
    
    //Final null terminator
    llvm::Value *finalPos=funcBuilder.CreateGEP(i8Ty,buffer,cursor);
    funcBuilder.CreateStore(llvm::ConstantInt::get(i8Ty,0),finalPos);
    inhibitCleanUp=false;
    
    for(const auto &seg:fStr->segments){
        for(const auto &valExpr:seg.values){
            auto sym=semantics.getSymbolFromMeta(valExpr.get());            
            emitCleanup(valExpr.get());
        }
    }
    
    return buffer;

}

llvm::Value *IRGenerator::generateChar8Literal(Node *node) {
  auto charLit = dynamic_cast<Char8Literal *>(node);
  if (!charLit) 
    reportDevBug("Invalid char literal",node);
  
 
  std::string tokenLiteral = charLit->char8_token.TokenLiteral;

  char c = decodeCharLiteral(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                static_cast<uint8_t>(c), false);
}

llvm::Value *IRGenerator::generateChar16Literal(Node *node) {
  auto char16Lit = dynamic_cast<Char16Literal *>(node);
  if (!char16Lit) 
    reportDevBug("Invalid char 16 literal",node);
  
  
  std::string tokenLiteral = char16Lit->char16_token.TokenLiteral;
  uint16_t c = decodeCharLiteral(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), c, false);
}

llvm::Value *IRGenerator::generateChar32Literal(Node *node) {
  auto char32Lit = dynamic_cast<Char32Literal *>(node);
  if (!char32Lit) 
    reportDevBug("Invalid char32 literal",node);
  
  
  std::string tokenLiteral = char32Lit->char32_token.TokenLiteral;
  uint32_t c = decodeChar32Literal(tokenLiteral);
  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), c, false);
}

llvm::Value *IRGenerator::generateBooleanLiteral(Node *node) {
  auto boolLit = dynamic_cast<BooleanLiteral *>(node);
  if (!boolLit) 
    reportDevBug("Invalid boolean type",node);
  

  bool value = (boolLit->boolean_token.TokenLiteral == "true");

  return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), value);
}

llvm::Value *IRGenerator::generateI8Literal(Node *node) {
  auto i8Lit = dynamic_cast<I8Literal *>(node);
  if (!i8Lit)
    reportDevBug("Invalid i8 literal",node);

  return generateIntegerLiteral(i8Lit->i8_token.TokenLiteral, 8, true);
}

llvm::Value *IRGenerator::generateU8Literal(Node *node) {
  auto u8Lit = dynamic_cast<U8Literal *>(node);
  if (!u8Lit)
    reportDevBug("Invalid u8 literal",node);

  return generateIntegerLiteral(u8Lit->u8_token.TokenLiteral, 8, false);
}

llvm::Value *IRGenerator::generateI16Literal(Node *node) {
  auto i16Lit = dynamic_cast<I16Literal *>(node);
  if (!i16Lit)
    reportDevBug("Invalid i16 literal",node);

  return generateIntegerLiteral(i16Lit->i16_token.TokenLiteral, 16, true);
}

llvm::Value *IRGenerator::generateU16Literal(Node *node) {
  auto u16Lit = dynamic_cast<U16Literal *>(node);
  if (!u16Lit)
    reportDevBug("Invalid u16 literal",node);

  return generateIntegerLiteral(u16Lit->u16_token.TokenLiteral, 16, false);
}

llvm::Value *IRGenerator::generateI32Literal(Node *node) {
  auto i32Lit = dynamic_cast<I32Literal *>(node);
  if (!i32Lit) 
    reportDevBug("Invalid i32 literal",node);
  

  return generateIntegerLiteral(i32Lit->i32_token.TokenLiteral, 32, true);
}

llvm::Value *IRGenerator::generateU32Literal(Node *node) {
  auto u32Lit = dynamic_cast<U32Literal *>(node);
  if (!u32Lit)
    reportDevBug("Invalid u32 literal",node);

  return generateIntegerLiteral(u32Lit->u32_token.TokenLiteral, 32, false);
}

llvm::Value *IRGenerator::generateI64Literal(Node *node) {
  auto i64Lit = dynamic_cast<I64Literal *>(node);
  if (!i64Lit)
    reportDevBug("Invalid i64 literal",node);

  return generateIntegerLiteral(i64Lit->i64_token.TokenLiteral, 64, true);
}

llvm::Value *IRGenerator::generateU64Literal(Node *node) {
  auto u64Lit = dynamic_cast<U64Literal *>(node);
  if (!u64Lit)
    reportDevBug("Invalid u64 literal",node);

  return generateIntegerLiteral(u64Lit->u64_token.TokenLiteral, 64, false);
}

llvm::Value *IRGenerator::generateI128Literal(Node *node) {
  auto i128Lit = dynamic_cast<I128Literal *>(node);
  if (!i128Lit)
    reportDevBug("Invalid i128 literal",node);

  return generateIntegerLiteral(i128Lit->i128_token.TokenLiteral, 128, true);
}

llvm::Value *IRGenerator::generateU128Literal(Node *node) {
  auto u128Lit = dynamic_cast<U128Literal *>(node);
  if (!u128Lit)
    throw std::runtime_error("Invalid u128 literal");

  return generateIntegerLiteral(u128Lit->u128_token.TokenLiteral, 128, false);
}

llvm::Value *IRGenerator::generateINTLiteral(Node *node) {
  auto intLit = dynamic_cast<INTLiteral *>(node);
  if (!intLit)
    reportDevBug("Invalid int literal", node);

  auto litSym = semantics.getSymbolFromMeta(intLit);
  if (!litSym)
    reportDevBug("Failed to get literal symbol info", intLit);

  bool isSigned = isSignedInteger(litSym->type().type.kind);
  uint32_t bitWidth = convertIntTypeToWidth(litSym->type().type.kind);

  return generateIntegerLiteral(intLit->int_token.TokenLiteral, bitWidth,
                                isSigned);
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
  if (!f32Lit)
    reportDevBug("Invalid f32 literal", node);

  float value = std::stof(f32Lit->f32_token.TokenLiteral);
  return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
}

llvm::Value *IRGenerator::generateF64Literal(Node *node) {
  auto f64Lit = dynamic_cast<F64Literal *>(node);
  if (!f64Lit)
    reportDevBug("Invalid f64 literal", node);

  double value = std::stod(
      f64Lit->f64_token.TokenLiteral); // Converting the double literal from a
                                       // string to a double
  return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context),
                               value); // Returning double value
}

llvm::Value *IRGenerator::generateFloatLiteral(Node *node) {
  auto fltLit = dynamic_cast<FloatLiteral *>(node);
  if (!fltLit)
    reportDevBug("Invalid generic float literal", node);

  auto sym = semantics.getSymbolFromMeta(fltLit);
  if (!sym)
    reportDevBug("Failed to get literal symbol info", fltLit);

  if (sym->type().type.kind == DataType::F64) {
    double value = std::stod(fltLit->float_token.TokenLiteral);
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), value);
  }

  float value = std::stof(fltLit->float_token.TokenLiteral);
  return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
}

// Generator function for identifier expression
llvm::Value *IRGenerator::generateIdentifierExpression(Node *node) {
  auto identExpr = dynamic_cast<Identifier *>(node);
  if (!identExpr) {
    reportDevBug("Invalid identifier node", node);
  }

  const std::string &identName = identExpr->identifier.TokenLiteral;

  auto sym = semantics.getSymbolFromMeta(identExpr);
  if(!sym)
      reportDevBug("Failed to get identifier symbol info",identExpr);

  // Get address and possible pending free
  llvm::Value *address = generateIdentifierAddress(identExpr);
  if (!address) {
    reportDevBug("No address for '" + identName + "'", identExpr);
  }

  // Component instance -> return pointer to the struct instance (address is
  // already correct)
  auto compIt = componentTypes.find(sym->type().type.resolvedName);
  if (compIt != componentTypes.end()) {
    return address;
  }


  if (sym->storage().isHeap) {
    llvm::Type *elemTy = sym->codegen().llvmType;
    llvm::Value *actualHeapAddr = address;

    if (!elemTy) {
      reportDevBug("No type for heap scalar '" + identName + "'", identExpr);
    }

    // Grab the value from the heap address
    llvm::LoadInst *loadedVal =
        funcBuilder.CreateLoad(elemTy, actualHeapAddr, identName + "_val");
    if (sym->storage().isVolatile) {
      loadedVal->setVolatile(true);
    }

    emitCleanup(identExpr);

    return loadedVal;
  }

  // Non-heap scalar: variableAddr is a pointer to T, just load
  llvm::Type *loadedType;
  if (sym->type().isRef) {
    loadedType = getLLVMType(semantics.peelRef(sym->type().type));
  } else {
    loadedType = getLLVMType(sym->type().type);
  }

  if (!loadedType) {
    reportDevBug("Type mapper failed for scalar '" + identName + "'",
                 identExpr);
  }

  // variableAddr should already be a pointer, load from it
  llvm::LoadInst *val =
      funcBuilder.CreateLoad(loadedType, address, identName + "_val");
  if (sym->storage().isVolatile) {
    val->setVolatile(true);
  }

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
    reportDevBug("Invalid null literal",node);

  auto nullSym=semantics.getSymbolFromMeta(nullLit);
  if(!nullSym)
    reportDevBug("Failed to get null symbol info",nullLit);

  llvm::Type *llvmType = getLLVMType(nullSym->type().type);

  return llvm::ConstantAggregateZero::get(llvmType);
}

//_____________________Infix expression_________________________
llvm::Value *IRGenerator::generateInfixExpression(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  if (!infix)
    reportDevBug("Invalid infix expression",node);

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
  if (infix->operat.type == TokenType::FULLSTOP)
    return handleMemberAccess(infix, leftVal, leftSym, rightSym);

  // Handle boolean logical operators (AND, OR)
  if (infix->operat.type == TokenType::AND ||
      infix->operat.type == TokenType::OR)
    return  handleLogical(infix, leftVal, leftSym, rightSym);

  // Handle Null Coalesce (??)
  if (infix->operat.type == TokenType::COALESCE)
    result = handleCoalesce(infix, leftVal);

  llvm::Value *rightVal = generateExpression(infix->right_operand.get());

  auto resultType = infixSym->type().type;
  // Promote operands to widest integer type among left, right, and result
  if (isIntegerType(resultType.kind)) {
    leftVal = promoteInt(leftVal, leftSym->type().type.kind, resultType.kind);
    rightVal = promoteInt(rightVal, rightSym->type().type.kind, resultType.kind);
  }


  // Handle floating point conversions
  if (resultType.kind == DataType::F32) {
    if (isIntegerType(leftSym->type().type.kind))
      leftVal = funcBuilder.CreateSIToFP(
          leftVal, llvm::Type::getFloatTy(context), "inttof32");
    if (isIntegerType(rightSym->type().type.kind))
      rightVal = funcBuilder.CreateSIToFP(
          rightVal, llvm::Type::getFloatTy(context), "inttof32");
  } else if (resultType.kind == DataType::F64) {
    if (isIntegerType(leftSym->type().type.kind))
      leftVal = funcBuilder.CreateSIToFP(
          leftVal, llvm::Type::getDoubleTy(context), "inttof64");
    if (isIntegerType(rightSym->type().type.kind))
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
  emitCleanup(infix);
  return result;
}

//__________________Prefix expression______________________
llvm::Value *IRGenerator::generatePrefixExpression(Node *node) {
  auto prefix = dynamic_cast<PrefixExpression *>(node);
  if (!prefix)
    reportDevBug("Invalid prefix expression",node);

  if (isGlobalScope)
    reportDevBug(
        "Executable statements arent allowed in the global scope",prefix);

  llvm::Value *operand = generateExpression(prefix->operand.get());

  if (!operand)
    throw std::runtime_error("Failed to generate IR for prefix operand");

  const std::string &name = prefix->operand->expression.TokenLiteral;
  auto prefixSym = semantics.getSymbolFromMeta(prefix);
  if (!prefixSym) 
    reportDevBug("Unknown variable '" + name + "' in prefix",prefix);
  

  ResolvedType resultType = prefixSym->type().type;

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
    if (!address)
      throw std::runtime_error("Null variable pointer for: " +
                               ident->identifier.TokenLiteral);

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
        delta = llvm::ConstantInt::get(varType, 1);
    } else {
      reportDevBug("Unsupported type for ++/--",prefix);
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
    reportDevBug("Invalid postfix expression",node);

  auto identifier = dynamic_cast<Identifier *>(postfix->operand.get());
  if (!identifier)
    reportDevBug("Postfix operand must be a variable",identifier);
  auto identName = identifier->identifier.TokenLiteral;
  llvm::Value *address = generateIdentifierAddress(identifier);

  if (!address)
    throw std::runtime_error("Null variable pointer for: " +
                             identifier->identifier.TokenLiteral);

  auto postfixSym = semantics.getSymbolFromMeta(postfix);
  if (!postfixSym) {
    reportDevBug("Variable '" + identName + "' does not exist",postfix);
  }

  ResolvedType resultType = postfixSym->type().type;

  // Get the type from resultType for CreateLoad
  llvm::Type *varType = getLLVMType(resultType);
  if (!varType)
    reportDevBug("Invalid type for variable: " +
                             identifier->identifier.TokenLiteral,postfix);

  llvm::Value *originalValue =
      funcBuilder.CreateLoad(varType, address, llvm::Twine("loadtmp"));

  llvm::Value *delta = nullptr;
  if (resultType.kind == DataType::F32 || resultType.kind == DataType::F64) {
    delta = llvm::ConstantFP::get(varType, 1.0);
  } else if (isIntegerType(resultType.kind)) {
      delta = llvm::ConstantInt::get(varType, 1);
  } else {
    reportDevBug("Unsupported type for ++/--",postfix);
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

  // Return original value since postfix
  return originalValue;
}

llvm::Value *IRGenerator::generateUnwrapExpression(Node *node) {
  auto unwrapExpr = dynamic_cast<UnwrapExpression *>(node);
  if (!unwrapExpr)
    reportDevBug("Invalid unwrap expression",node);


  if (isGlobalScope)
    reportDevBug("Cannot unwrap in global scope",unwrapExpr);

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
    reportDevBug("Invalid cast expression",node);


  auto castSym = semantics.getSymbolFromMeta(castExpr);
  if (!castSym)
    reportDevBug("Failed to get cast symbol info",castExpr);

  llvm::Value *sourceVal = generateExpression(castExpr->expr.get());
  if (!sourceVal)
    throw std::runtime_error("Failed to generate source value");

  llvm::Type *srcType = sourceVal->getType();
  llvm::Type *dstType = getLLVMType(castSym->type().type);

  if (srcType == dstType)
    return sourceVal;

  if (srcType->isIntegerTy() && dstType->isIntegerTy()) {
    unsigned srcBits = srcType->getIntegerBitWidth();
    unsigned dstBits = dstType->getIntegerBitWidth();

    if (dstBits < srcBits) {
      return funcBuilder.CreateTrunc(sourceVal, dstType, "cast_trunc");
    } else {
      if (isUnsigned(semantics.metaData[castExpr->expr.get()]->type().type)) {
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

  throw std::runtime_error("Unhandled cast from " + castSym->type().type.resolvedName);
}

llvm::Value *IRGenerator::generateBitcastExpression(Node *node) {
  auto bitcastExpr = dynamic_cast<BitcastExpression *>(node);
  if (!bitcastExpr)
    reportDevBug("Invalid bitcast expression",node);

  auto bitcastSym = semantics.getSymbolFromMeta(bitcastExpr);
  if(!bitcastSym)
    reportDevBug("Failed to get bitcast symbol info",bitcastExpr);

  // Generate the source value (the bits we are reinterpreting)
  llvm::Value *sourceVal = generateExpression(bitcastExpr->expr.get());
  if (!sourceVal)
    reportDevBug("Failed to generate source value",bitcastExpr->expr.get());

  llvm::Type *srcType = sourceVal->getType();
  llvm::Type *dstType = getLLVMType(bitcastSym->type().type);

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
  if (!arrExpr) reportDevBug("Invalid array access expression", node);

  auto arrSym = semantics.getSymbolFromMeta(arrExpr);
  if(!arrSym) reportDevBug("Failed to get array subscript symbol info", arrExpr);

  inhibitCleanUp = true;
  // Get the address of the element/sub-array
  llvm::Value *ptr = generateArraySubscriptAddress(node);
  inhibitCleanUp = false;

  // KEEN CHECK: Are we looking at a final scalar/pointer, or a sub-array?
  if (arrSym->type().type.isArray()) {
      // If the result is still an array (partial indexing), 
      // return the pointer to the start of this sub-dimension.
      return ptr; 
  }

  // If it's a base type (i32, f64, ptr, etc.), load the value from memory.
  llvm::Type *elemTy = getLLVMType(arrSym->type().type);
  auto loadedVal = funcBuilder.CreateLoad(elemTy, ptr, "arr_elem_load");
  if(arrSym->storage().isVolatile)
      loadedVal->setVolatile(true);

  emitCleanup(arrExpr);
  return loadedVal;
}

llvm::Value *IRGenerator::generateSizeOfExpression(Node *node) {
  auto sizeOf = dynamic_cast<SizeOfExpression *>(node);
  if (!sizeOf)
    reportDevBug("Invalid sizeof expression",node);

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
    reportDevBug("Invalid self expression",node);


  const std::string &compName =
      currentComponent->component_name->expression.TokenLiteral;

  // Lookup LLVM struct for top-level component
  llvm::StructType *currentStructTy = nullptr;
  auto it = componentTypes.find(compName);
  if (it == componentTypes.end())
    throw std::runtime_error("Component '" + compName +
                             "' not found in componentTypes");

  currentStructTy = it->second;

  //  Load 'self' pointer
  llvm::AllocaInst *selfAlloca = currentFunctionSelfMap[currentFunction];
  if (!selfAlloca)
    throw std::runtime_error("'self' access outside component method");

  llvm::Value *currentPtr = funcBuilder.CreateLoad(
      currentStructTy->getPointerTo(), selfAlloca, "self_load");
  // self pointer itself is never volatile (it's just a pointer to the
  // component)

  // Semantic chain walk 
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
    if (currentTypeInfo->type.isPointer() || currentTypeInfo->type.isRef())
      currentTypeName = semantics.getBaseTypeName(currentTypeInfo->type);

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
      if (lastMemberInfo->type.isPointer() || lastMemberInfo->type.isRef())
        lookUpName = semantics.getBaseTypeName(lastMemberInfo->type);

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
  auto *finalLoad = funcBuilder.CreateLoad(
      finalTy, currentPtr, selfExpr->fields.back()->toString() + "_val");

  // Check if the field itself is volatile
  if (lastMemberInfo->isVolatile) {
    finalLoad->setVolatile(true);
  }

  return finalLoad;
}

llvm::Value *IRGenerator::generateDereferenceExpression(Node *node) {
  auto derefExpr = dynamic_cast<DereferenceExpression *>(node);
  if (!derefExpr)
    return nullptr;

  auto derefSym = semantics.getSymbolFromMeta(derefExpr);
  inhibitCleanUp = true;
  Node *current = node;
  int derefCount = 0;
  while (auto nested = dynamic_cast<DereferenceExpression *>(current)) {
    derefCount++;
    current = nested->identifier.get();
  }
  auto identNode = dynamic_cast<Identifier *>(current);

  // Get the address of the variable (e.g., the alloca on the stack)
  llvm::Value *addr = generateIdentifierAddress(identNode);
  derefSym->codegen().llvmValue = addr;
  auto meta = semantics.getSymbolFromMeta(node);

  // Load the actual address stored in the variable.
  // This MUST be a pointer type if we intend to dereference it further.
  llvm::Type *ptrTy = llvm::PointerType::getUnqual(context);
  auto *baseLoad = funcBuilder.CreateLoad(ptrTy, addr, "base_address");
  if (meta && meta->storage().isVolatile) {
    baseLoad->setVolatile(true);
  }
  addr = baseLoad;

  // Perform the dereference hops
  for (int i = 0; i < derefCount; i++) {
    // If we have more hops to go, we are still loading addresses (pointers)
    // If this is the LAST hop, we load the actual data (e.g., i32)
    bool isLastHop = (i == derefCount - 1);

    llvm::Type *loadTy = isLastHop ? getLLVMType(meta->type().type) : ptrTy;

    // Create the load.
    // If it's the last hop, this returns the value (i32).
    // Otherwise, it returns the next address in the chain.
    auto *hopLoad = funcBuilder.CreateLoad(loadTy, addr,
                                           isLastHop ? "deref_val" : "ptr_hop");
    if (meta && meta->storage().isVolatile) {
      hopLoad->setVolatile(true);
    }
    addr = hopLoad;
  }
  inhibitCleanUp = false;
  emitCleanup(derefExpr);

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
    reportDevBug("Invalid address expression", node);
  }

  logInternal("Handling address expression generation");
  inhibitCleanUp = true;
  
  auto sym = semantics.getSymbolFromMeta(addrExpr);
  if(!sym)
    reportDevBug("Failed to find address symbol info",addrExpr);

  auto targetSym = sym->relations().targetSymbol;
  llvm::Value *variablePtr = targetSym->codegen().llvmValue;
  if (!variablePtr) {
    reportDevBug("No value was assigned to the target", addrExpr);
  }
  sym->codegen().llvmValue = variablePtr;
  inhibitCleanUp = false;
  emitCleanup(addrExpr);

  return variablePtr;
}
