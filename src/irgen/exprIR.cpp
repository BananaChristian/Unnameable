#include "irgen.hpp"
#include <cstdint>

//___________________Literals generator_____________________
llvm::Value *IRGenerator::generateStringLiteral(Node *node) {
  std::cout << "INSIDE GENERATE IR FOR STRING\n";
  auto strLit = dynamic_cast<StringLiteral *>(node);
  if (!strLit) {
    throw std::runtime_error("Invalid string literal");
  }
  auto it = semantics.metaData.find(strLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("String literal not found in metadata");
  }
  DataType dt = it->second->type.kind;

  if (dt != DataType::STRING) {
    throw std::runtime_error("Type error: Expected STRING");
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
    throw std::runtime_error("Char literal not found in metadata");
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
    throw std::runtime_error("Invalid char 32 literal");
  }
  auto it = semantics.metaData.find(char32Lit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Char16 literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::CHAR16) {
    throw std::runtime_error("Type error: Expected CHAR16 for Char16Literal");
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
  unsigned int ptrWidth = module->getDataLayout().getPointerSizeInBits();

  return generateIntegerLiteral(lit->isize_token.TokenLiteral, ptrWidth, true);
}

llvm::Value *IRGenerator::generateUSIZELiteral(Node *node) {
  auto lit = dynamic_cast<USIZELiteral *>(node);
  unsigned int ptrWidth = module->getDataLayout().getPointerSizeInBits();

  return generateIntegerLiteral(lit->usize_token.TokenLiteral, ptrWidth, false);
}

llvm::Value *IRGenerator::generateFloatLiteral(Node *node) {
  auto fltLit = dynamic_cast<FloatLiteral *>(node);
  if (!fltLit) {
    throw std::runtime_error("Invalid float literal");
  }
  auto it = semantics.metaData.find(fltLit);
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Float literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::FLOAT) {
    throw std::runtime_error("Type error: Expected Float for FloatLiteral ");
  }
  float value = std::stof(fltLit->float_token.TokenLiteral);
  return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), value);
}

llvm::Value *IRGenerator::generateDoubleLiteral(Node *node) {
  auto dbLit = dynamic_cast<DoubleLiteral *>(node);
  if (!dbLit) {
    throw std::runtime_error("Invalid double literal");
  }
  auto it =
      semantics.metaData.find(dbLit); // Creating an iterator to find specific
                                      // meta data about the double literal
  if (it == semantics.metaData.end()) {
    throw std::runtime_error("Double literal not found in metadata");
  }
  DataType dt = it->second->type.kind;
  if (dt != DataType::DOUBLE) {
    throw std::runtime_error("Type error: Expected DOUBLE for DoubleLiteral");
  }
  // Checking if we have metaData about the double literal and if so we check to
  // see if the data type is double
  double value = std::stod(
      dbLit->double_token.TokenLiteral); // Converting the double literal from a
                                         // string to a double
  return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context),
                               value); // Returning double value
}

llvm::Value *IRGenerator::generateArrayLiteral(Node *node) {
  auto arrLit = dynamic_cast<ArrayLiteral *>(node);
  if (!arrLit)
    throw std::runtime_error("Invalid array literal");

  auto it = semantics.metaData.find(arrLit);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Array literal not found in metaData");

  auto sym = it->second;

  // Base element type
  llvm::Type *llvmElemTy = nullptr;

  size_t arraySize = arrLit->array.size();
  // llvm::ArrayType *arrayTy = llvm::ArrayType::get(llvmElemTy, arraySize);

  // Create the vector of element constants
  std::vector<llvm::Constant *> elems;
  elems.reserve(arraySize);

  for (size_t i = 0; i < arraySize; ++i) {
    Node *child = arrLit->array[i].get();
    llvm::Value *currentVal = nullptr;

    if (auto nested = dynamic_cast<ArrayLiteral *>(child)) {
      currentVal = generateArrayLiteral(nested);
      // Recursive constant array
      llvm::Constant *nestedConst = llvm::cast<llvm::Constant>(currentVal);
      elems.push_back(nestedConst);

      if (i == 0) {
        llvmElemTy = nestedConst->getType();
      }
    } else {
      currentVal = generateExpression(child);

      auto *constVal = llvm::dyn_cast<llvm::Constant>(currentVal);
      if (!constVal)
        throw std::runtime_error("Array literal element is not constant");

      elems.push_back(constVal);

      if (i == 0) {
        llvmElemTy = constVal->getType();
      }
    }
  }

  if (elems.empty() || !llvmElemTy) {
    throw std::runtime_error("Cannot infer element type for array literal.");
  }

  llvm::ArrayType *arrayTy = llvm::ArrayType::get(llvmElemTy, arraySize);

  // Return a constant LLVM array
  return llvm::ConstantArray::get(arrayTy, elems);
}

llvm::Value *IRGenerator::generateNullLiteral(NullLiteral *nullLit,
                                              DataType type) {
  switch (type) {
  case DataType::STRING:
    return llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0));

  case DataType::I8:
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                  llvm::APInt(8, INT8_MIN, true));

  case DataType::U8:
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                  llvm::APInt(8, 0));

  case DataType::I16:
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),
                                  llvm::APInt(16, INT16_MIN, true));

  case DataType::U16:
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),
                                  llvm::APInt(16, 0));

  case DataType::I32:
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                  llvm::APInt(32, INT32_MIN, true));

  case DataType::U32:
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                  llvm::APInt(32, 0));

  case DataType::I64:
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                  llvm::APInt(64, INT64_MIN, true));

  case DataType::U64:
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                  llvm::APInt(64, 0));

  case DataType::I128:
  case DataType::U128:
    return llvm::ConstantInt::get(llvm::Type::getInt128Ty(context),
                                  llvm::APInt(128, 0));

  case DataType::FLOAT:
    return llvm::ConstantFP::getNaN(llvm::Type::getFloatTy(context));

  case DataType::DOUBLE:
    return llvm::ConstantFP::getNaN(llvm::Type::getDoubleTy(context));

  case DataType::BOOLEAN:
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),
                                  llvm::APInt(1, 0));

  case DataType::CHAR8:
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                  llvm::APInt(8, 0));

  case DataType::CHAR16:
    return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),
                                  llvm::APInt(16, 0));

  case DataType::CHAR32:
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                  llvm::APInt(32, 0));

  default:
    throw std::runtime_error(
        "Unsupported nullable data type in generateNullLiteral");
  }
}

//_____________________Infix expression_________________________
llvm::Value *IRGenerator::generateInfixExpression(Node *node) {
  auto infix = dynamic_cast<InfixExpression *>(node);
  if (!infix)
    throw std::runtime_error("Invalid infix expression");

  auto infixIt = semantics.metaData.find(infix);
  if (infixIt == semantics.metaData.end())
    throw std::runtime_error("Cannot find infix metaData");

  auto infixSym = infixIt->second;
  if (infixSym->hasError)
    throw std::runtime_error("Semantic error detected");

  auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
  auto lhsIdent = dynamic_cast<Identifier *>(infix->left_operand.get());

  if (isGlobalScope && (lhsIdent || rhsIdent)) {
    throw std::runtime_error(
        "Executable statements are not allowed at global scope: '" +
        infix->operat.TokenLiteral + "' at line " +
        std::to_string(infix->operat.line));
  }

  if (infix->operat.type == TokenType::FULLSTOP) {
    // sanity check: member access only inside a function
    if (isGlobalScope)
      throw std::runtime_error("Member access not allowed at global scope");

    llvm::Value *lhsVal = generateExpression(infix->left_operand.get());
    auto rhsIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
    if (!rhsIdent)
      throw std::runtime_error("Right side of '.' must be an identifier");

    std::string memberName = rhsIdent->expression.TokenLiteral;

    // get metadata for the left operand
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];
    if (!lhsMeta)
      throw std::runtime_error("Missing metadata for struct expression");

    std::string parentTypeName = lhsMeta->type.resolvedName;
    std::string lookUpName = semantics.stripPtrSuffix(parentTypeName);

    // get struct definition
    auto parentTypeIt = semantics.customTypesTable.find(lookUpName);
    if (parentTypeIt == semantics.customTypesTable.end())
      throw std::runtime_error("Unknown struct type '" + lookUpName + "'");

    auto &parentInfo = parentTypeIt->second;
    auto memberIt = parentInfo->members.find(memberName);
    if (memberIt == parentInfo->members.end())
      throw std::runtime_error("No member '" + memberName + "' in type '" +
                               lookUpName + "'");

    // get member index + type
    unsigned memberIndex = memberIt->second->memberIndex;
    llvm::StructType *structTy = llvmCustomTypes[lookUpName];
    llvm::Type *memberType = getLLVMType(memberIt->second->type);

    // if lhs is struct by value, we need a pointer to use GEP
    llvm::Value *lhsPtr = lhsVal;
    if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(lhsVal)) {
      lhsPtr = funcBuilder.CreateLoad(structTy->getPointerTo(), gv,
                                      gv->getName() + ".loaded");
    } else if (!lhsVal->getType()->isPointerTy()) {
      llvm::Value *allocaTmp = funcBuilder.CreateAlloca(
          lhsVal->getType(), nullptr, lookUpName + "_tmp");
      funcBuilder.CreateStore(lhsVal, allocaTmp);
      lhsPtr = allocaTmp;
    }

    llvm::Value *memberPtr =
        funcBuilder.CreateStructGEP(structTy, lhsPtr, memberIndex, memberName);

    // now return by-value load, not a pointer
    llvm::Value *memberVal =
        funcBuilder.CreateLoad(memberType, memberPtr, memberName + "_val");

    // If this is the last use and the component instance was heap raised
    if (lhsMeta->isHeap && lhsMeta->lastUseNode == infix &&
        lhsMeta->refCount == 0) {
      llvm::FunctionCallee sageFreeFn = module->getOrInsertFunction(
          "sage_free", llvm::Type::getVoidTy(context),
          llvm::Type::getInt64Ty(context));
      funcBuilder.CreateCall(
          sageFreeFn,
          {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                  lhsMeta->componentSize)},
          infix->left_operand->expression.TokenLiteral + "_sage_free");
    }

    return memberVal;
  }

  if (infix->operat.type == TokenType::SCOPE_OPERATOR) {
    std::cout << "TRIGGERED SCOPE OPERATOR GUY\n";

    // "::" is only for enum access
    auto enumName = lhsIdent->expression.TokenLiteral;
    auto leftTypeIt = semantics.customTypesTable.find(enumName);
    if (leftTypeIt == semantics.customTypesTable.end())
      throw std::runtime_error("Unknown enum class '" + enumName + "'");

    auto leftType = leftTypeIt->second->type;

    // If the type isn't an enum reject it
    if (leftType.kind != DataType::ENUM) {
      throw std::runtime_error(
          "The '::' operator is only valid for enum access");
    }

    auto &enumInfo = leftTypeIt->second;

    // RHS must be an identifier (enum member)
    auto memberIdent = dynamic_cast<Identifier *>(infix->right_operand.get());
    if (!memberIdent)
      throw std::runtime_error(
          "Right-hand of '::' must be an enum member identifier");

    auto memberName = memberIdent->expression.TokenLiteral;

    // Find enum member
    auto memIt = enumInfo->members.find(memberName);
    if (memIt == enumInfo->members.end())
      throw std::runtime_error("Enum '" + enumName + "' has no member named '" +
                               memberName + "'");

    uint64_t memberValue = memIt->second->constantValue;

    // Return constant integer
    llvm::Type *llvmEnumTy = getLLVMType(leftType);
    return llvm::ConstantInt::get(llvmEnumTy, memberValue);
  }

  llvm::Value *left = generateExpression(infix->left_operand.get());
  llvm::Value *right = generateExpression(infix->right_operand.get());

  if (!left || !right)
    throw std::runtime_error("Failed to generate IR for infix expression");

  auto it = semantics.metaData.find(node);
  if (it == semantics.metaData.end())
    throw std::runtime_error("Meta data missing for infix node");

  ResolvedType resultType = it->second->type;
  DataType leftType = semantics.metaData[infix->left_operand.get()]->type.kind;
  DataType rightType =
      semantics.metaData[infix->right_operand.get()]->type.kind;

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

  // Promote operands to widest integer type among left, right, and result
  if (isIntegerType(resultType.kind)) {
    unsigned targetBits = getIntegerBitWidth(resultType.kind);
    left = promoteInt(left, leftType, resultType.kind);
    right = promoteInt(right, rightType, resultType.kind);
  }

  // Handle BOOLEAN logical operators (AND, OR)
  if (infix->operat.type == TokenType::AND ||
      infix->operat.type == TokenType::OR) {
    if (left->getType() != funcBuilder.getInt1Ty())
      left = funcBuilder.CreateICmpNE(
          left, llvm::ConstantInt::get(left->getType(), 0), "boolcastl");
    if (right->getType() != funcBuilder.getInt1Ty())
      right = funcBuilder.CreateICmpNE(
          right, llvm::ConstantInt::get(right->getType(), 0), "boolcastr");

    if (infix->operat.type == TokenType::AND)
      return funcBuilder.CreateAnd(left, right, "andtmp");
    else
      return funcBuilder.CreateOr(left, right, "ortmp");
  }

  // Handle floating point conversions
  if (resultType.kind == DataType::FLOAT) {
    if (isIntegerType(leftType))
      left = funcBuilder.CreateSIToFP(left, llvm::Type::getFloatTy(context),
                                      "inttofloat");
    if (isIntegerType(rightType))
      right = funcBuilder.CreateSIToFP(right, llvm::Type::getFloatTy(context),
                                       "inttofloat");
  } else if (resultType.kind == DataType::DOUBLE) {
    if (isIntegerType(leftType))
      left = funcBuilder.CreateSIToFP(left, llvm::Type::getDoubleTy(context),
                                      "inttodouble");
    if (isIntegerType(rightType))
      right = funcBuilder.CreateSIToFP(right, llvm::Type::getDoubleTy(context),
                                       "inttodouble");
  }

  // Now generate code based on operator and result type
  // Comparison operators - different for signed/unsigned integers
  if (infix->operat.type == TokenType::EQUALS ||
      infix->operat.type == TokenType::NOT_EQUALS ||
      infix->operat.type == TokenType::LESS_THAN ||
      infix->operat.type == TokenType::LT_OR_EQ ||
      infix->operat.type == TokenType::GREATER_THAN ||
      infix->operat.type == TokenType::GT_OR_EQ) {
    if (isIntegerType(leftType) || leftType == DataType::ENUM) {
      if (isIntegerType(rightType) || rightType == DataType::ENUM) {
        bool signedInt = isSignedInteger(leftType);

        switch (infix->operat.type) {
        case TokenType::EQUALS:
          return funcBuilder.CreateICmpEQ(left, right, "cmptmp");
        case TokenType::NOT_EQUALS:
          return funcBuilder.CreateICmpNE(left, right, "cmptmp");
        case TokenType::LESS_THAN:
          return signedInt ? funcBuilder.CreateICmpSLT(left, right, "cmptmp")
                           : funcBuilder.CreateICmpULT(left, right, "cmptmp");
        case TokenType::LT_OR_EQ:
          return signedInt ? funcBuilder.CreateICmpSLE(left, right, "cmptmp")
                           : funcBuilder.CreateICmpULE(left, right, "cmptmp");
        case TokenType::GREATER_THAN:
          return signedInt ? funcBuilder.CreateICmpSGT(left, right, "cmptmp")
                           : funcBuilder.CreateICmpUGT(left, right, "cmptmp");
        case TokenType::GT_OR_EQ:
          return signedInt ? funcBuilder.CreateICmpSGE(left, right, "cmptmp")
                           : funcBuilder.CreateICmpUGE(left, right, "cmptmp");
        default:
          throw std::runtime_error("Unsupported int comparison operator");
        }
      }
    } else if (leftType == DataType::FLOAT || leftType == DataType::DOUBLE) {
      switch (infix->operat.type) {
      case TokenType::EQUALS:
        return funcBuilder.CreateFCmpOEQ(left, right, "fcmptmp");
      case TokenType::NOT_EQUALS:
        return funcBuilder.CreateFCmpONE(left, right, "fcmptmp");
      case TokenType::LESS_THAN:
        return funcBuilder.CreateFCmpOLT(left, right, "fcmptmp");
      case TokenType::LT_OR_EQ:
        return funcBuilder.CreateFCmpOLE(left, right, "fcmptmp");
      case TokenType::GREATER_THAN:
        return funcBuilder.CreateFCmpOGT(left, right, "fcmptmp");
      case TokenType::GT_OR_EQ:
        return funcBuilder.CreateFCmpOGE(left, right, "fcmptmp");
      default:
        throw std::runtime_error("Unsupported float comparison operator");
      }
    } else {
      throw std::runtime_error(
          "Comparison not supported for type '" +
          semantics.metaData[infix->left_operand.get()]->type.resolvedName +
          "'");
    }
  }
  // Arithmetic operators
  switch (infix->operat.type) {
  case TokenType::PLUS: {
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];

    if (lhsMeta->type.isPointer) {
      ResolvedType baseTypeInfo = lhsMeta->type;

      // Strip the copy to avoid mutating the original
      baseTypeInfo.isPointer = false;
      baseTypeInfo.resolvedName =
          semantics.stripPtrSuffix(lhsMeta->type.resolvedName);

      llvm::Type *baseTy = getLLVMType(baseTypeInfo);
      return funcBuilder.CreateGEP(baseTy, left, right, "ptr_addtmp");
    }

    if (isIntegerType(resultType.kind))
      return funcBuilder.CreateAdd(left, right, "addtmp");
    else
      return funcBuilder.CreateFAdd(left, right, "faddtmp");
  }

  case TokenType::MINUS: {
    auto lhsMeta = semantics.metaData[infix->left_operand.get()];

    if (lhsMeta->type.isPointer) {
      ResolvedType baseTypeInfo = lhsMeta->type;

      // Strip the copy to avoid mutating the original
      baseTypeInfo.isPointer = false;
      baseTypeInfo.resolvedName =
          semantics.stripPtrSuffix(lhsMeta->type.resolvedName);

      llvm::Type *baseTy = getLLVMType(baseTypeInfo);

      llvm::Value *negRight = funcBuilder.CreateNeg(right, "neg_offset");
      return funcBuilder.CreateGEP(baseTy, left, negRight, "ptr_subtmp");
    }

    if (isIntegerType(resultType.kind))
      return funcBuilder.CreateSub(left, right, "subtmp");
    else
      return funcBuilder.CreateFSub(left, right, "fsubtmp");
  }

  case TokenType::ASTERISK: {
    if (isIntegerType(resultType.kind))
      return funcBuilder.CreateMul(left, right, "multmp");
    else
      return funcBuilder.CreateFMul(left, right, "fmultmp");
  }

  case TokenType::DIVIDE: {
    if (isIntegerType(resultType.kind))
      return isSignedInteger(resultType.kind)
                 ? funcBuilder.CreateSDiv(left, right, "divtmp")
                 : funcBuilder.CreateUDiv(left, right, "divtmp");
    else
      return funcBuilder.CreateFDiv(left, right, "fdivtmp");
  }

  case TokenType::MODULUS: {

    if (isIntegerType(resultType.kind))
      return isSignedInteger(resultType.kind)
                 ? funcBuilder.CreateSRem(left, right, "modtmp")
                 : funcBuilder.CreateURem(left, right, "modtmp");
    else
      throw std::runtime_error(
          "Modulus not supported for FLOAT or DOUBLE at line " +
          std::to_string(infix->operat.line));
  }

  case TokenType::BITWISE_AND:
    return funcBuilder.CreateAnd(left, right, "andtmp");
  case TokenType::BITWISE_OR:
    return funcBuilder.CreateOr(left, right, "ortmp");
  case TokenType::BITWISE_XOR:
    return funcBuilder.CreateXor(left, right, "xortmp");
  case TokenType::SHIFT_LEFT:
    return funcBuilder.CreateShl(left, right, "shltmp");
  case TokenType::SHIFT_RIGHT: {
    return isSignedInteger(resultType.kind)
               ? funcBuilder.CreateAShr(left, right, "ashrtmp")
               : funcBuilder.CreateLShr(left, right, "lshrtmp");
  }
  default:
    throw std::runtime_error(
        "Unsupported infix operator: " + infix->operat.TokenLiteral +
        " at line " + std::to_string(infix->operat.line));
  }
}

//__________________Prefix expression______________________
llvm::Value *IRGenerator::generatePrefixExpression(Node *node) {
  std::cout << "Inside prefix generator\n";
  auto prefix = dynamic_cast<PrefixExpression *>(node);
  if (!prefix)
    throw std::runtime_error("Invalid prefix expression");

  if (isGlobalScope)
    throw std::runtime_error(
        "Executable statements arent allowed in the global scope");

  llvm::Value *operand=generateExpression(prefix->operand.get());

  if (!operand)
    throw std::runtime_error("Failed to generate IR for prefix operand");

  const std::string &name = prefix->operand->expression.TokenLiteral;
  auto prefixIt = semantics.metaData.find(prefix);
  if (prefixIt == semantics.metaData.end()) {
    throw std::runtime_error("Unknown variable '" + name + "' in prefix");
  }

  ResolvedType resultType = prefixIt->second->type;

  // Helper to check if integer type
  auto isIntType = [&](DataType dt) { return isIntegerType(dt); };

  // Helper to get LLVM type from DataType
  auto getLLVMType = [&](DataType dt) -> llvm::Type * {
    if (dt == DataType::FLOAT)
      return llvm::Type::getFloatTy(context);
    if (dt == DataType::DOUBLE)
      return llvm::Type::getDoubleTy(context);
    if (isIntType(dt)) {
      unsigned bits = getIntegerBitWidth(dt);
      return llvm::Type::getIntNTy(context, bits);
    }
    return nullptr;
  };

  switch (prefix->operat.type) {
  case TokenType::MINUS: {
    if (resultType.kind == DataType::FLOAT ||
        resultType.kind == DataType::DOUBLE)
      return funcBuilder.CreateFNeg(operand, llvm::Twine("fnegtmp"));
    else if (isIntType(resultType.kind))
      return funcBuilder.CreateNeg(operand, llvm::Twine("negtmp"));
    else
      throw std::runtime_error("Unsupported type for unary minus");
  }
  case TokenType::BITWISE_NOT:
    return funcBuilder.CreateNot(operand, llvm::Twine("bitnottmp"));
  case TokenType::BANG:
    return funcBuilder.CreateNot(operand, llvm::Twine("boolnottmp"));

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
    AddressAndPendingFree addrAndFree = generateIdentifierAddress(ident);
    llvm::Value *varPtr = addrAndFree.address;
    for (auto *freeCall : addrAndFree.pendingFrees) {
      funcBuilder.Insert(freeCall);
    }

    if (!varPtr)
      throw std::runtime_error("Null variable pointer for: " +
                               ident->identifier.TokenLiteral);

    // Get the type from resultType instead of getPointerElementType
    llvm::Type *varType = getLLVMType(resultType.kind);
    if (!varType)
      throw std::runtime_error("Invalid type for variable: " +
                               ident->identifier.TokenLiteral);

    llvm::Value *loaded =
        funcBuilder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

    llvm::Value *delta = nullptr;
    if (resultType.kind == DataType::FLOAT ||
        resultType.kind == DataType::DOUBLE) {
      delta = llvm::ConstantFP::get(varType, 1.0);
    } else if (isIntType(resultType.kind)) {
      unsigned bits = getIntegerBitWidth(resultType.kind);
      delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
    } else {
      throw std::runtime_error("Unsupported type for ++/--");
    }

    llvm::Value *updated = nullptr;
    if (prefix->operat.type == TokenType::PLUS_PLUS)
      updated =
          (resultType.kind == DataType::FLOAT ||
           resultType.kind == DataType::DOUBLE)
              ? funcBuilder.CreateFAdd(loaded, delta,
                                       llvm::Twine("preincfptmp"))
              : funcBuilder.CreateAdd(loaded, delta, llvm::Twine("preinctmp"));
    else
      updated =
          (resultType.kind == DataType::FLOAT ||
           resultType.kind == DataType::DOUBLE)
              ? funcBuilder.CreateFSub(loaded, delta,
                                       llvm::Twine("predecfptmp"))
              : funcBuilder.CreateSub(loaded, delta, llvm::Twine("predectmp"));

    funcBuilder.CreateStore(updated, varPtr);

    for (auto *freeCall : addrAndFree.pendingFrees) {
      funcBuilder.Insert(freeCall);
    }

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

  AddressAndPendingFree addrAndFree = generateIdentifierAddress(identifier);
  llvm::Value *varPtr = addrAndFree.address;
  for (auto *freeCall : addrAndFree.pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  if (!varPtr)
    throw std::runtime_error("Null variable pointer for: " +
                             identifier->identifier.TokenLiteral);

  auto postfixIt = semantics.metaData.find(postfix);
  if (postfixIt == semantics.metaData.end()) {
    throw std::runtime_error("Variable '" + identName + "' does not exist");
  }

  std::cerr << "[IR-DEBUG] ident lastUse = "
            << (identIt->second->lastUseNode
                    ? typeid(*identIt->second->lastUseNode).name()
                    : "NULL")
            << ", postfix lastUse = "
            << (postfixIt != semantics.metaData.end() &&
                        postfixIt->second->lastUseNode
                    ? typeid(*postfixIt->second->lastUseNode).name()
                    : "NULL")
            << "\n";

  if (postfixIt->second->hasError)
    return nullptr;

  ResolvedType resultType = postfixIt->second->type;

  // Helper to check if integer type
  auto isIntType = [&](DataType dt) { return isIntegerType(dt); };

  // Helper to get LLVM type from DataType
  auto getLLVMType = [&](DataType dt) -> llvm::Type * {
    if (dt == DataType::FLOAT)
      return llvm::Type::getFloatTy(context);
    if (dt == DataType::DOUBLE)
      return llvm::Type::getDoubleTy(context);
    if (isIntType(dt)) {
      unsigned bits = getIntegerBitWidth(dt);
      return llvm::Type::getIntNTy(context, bits);
    }
    return nullptr;
  };

  // Get the type from resultType for CreateLoad
  llvm::Type *varType = getLLVMType(resultType.kind);
  if (!varType)
    throw std::runtime_error("Invalid type for variable: " +
                             identifier->identifier.TokenLiteral);

  llvm::Value *originalValue =
      funcBuilder.CreateLoad(varType, varPtr, llvm::Twine("loadtmp"));

  llvm::Value *delta = nullptr;
  if (resultType.kind == DataType::FLOAT ||
      resultType.kind == DataType::DOUBLE) {
    delta = llvm::ConstantFP::get(varType, 1.0);
  } else if (isIntType(resultType.kind)) {
    unsigned bits = getIntegerBitWidth(resultType.kind);
    delta = llvm::ConstantInt::get(llvm::Type::getIntNTy(context, bits), 1);
  } else {
    throw std::runtime_error("Unsupported type for ++/--");
  }

  llvm::Value *updatedValue = nullptr;

  if (postfix->operator_token.type == TokenType::PLUS_PLUS)
    updatedValue =
        (resultType.kind == DataType::FLOAT ||
         resultType.kind == DataType::DOUBLE)
            ? funcBuilder.CreateFAdd(originalValue, delta, llvm::Twine("finc"))
            : funcBuilder.CreateAdd(originalValue, delta, llvm::Twine("inc"));
  else if (postfix->operator_token.type == TokenType::MINUS_MINUS)
    updatedValue =
        (resultType.kind == DataType::FLOAT ||
         resultType.kind == DataType::DOUBLE)
            ? funcBuilder.CreateFSub(originalValue, delta, llvm::Twine("fdec"))
            : funcBuilder.CreateSub(originalValue, delta, llvm::Twine("dec"));
  else
    throw std::runtime_error("Unsupported postfix operator: " +
                             postfix->operator_token.TokenLiteral +
                             " at line " +
                             std::to_string(postfix->operator_token.line));

  funcBuilder.CreateStore(updatedValue, varPtr);

  for (auto *freeCall : addrAndFree.pendingFrees) {
    funcBuilder.Insert(freeCall);
  }

  // Return original value since postfix
  return originalValue;
}
