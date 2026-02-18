#include "ast.hpp"
#include "irgen.hpp"
#include "semantics.hpp"
#include "token.hpp"
#include <llvm-18/llvm/IR/Value.h>

bool IRGenerator::ArithmeticOrBitwiseOperator(TokenType op) {
  switch (op) {
  case TokenType::PLUS:
  case TokenType::MINUS:
  case TokenType::ASTERISK:
  case TokenType::DIVIDE:
  case TokenType::MODULUS:
  case TokenType::BITWISE_AND:
  case TokenType::BITWISE_XOR:
  case TokenType::BITWISE_OR:
  case TokenType::SHIFT_LEFT:
  case TokenType::SHIFT_RIGHT:
    return true;
  default:
    return false;
  }
}

bool IRGenerator::ComparisonOperator(TokenType op) {
  switch (op) {
  case TokenType::EQUALS:
  case TokenType::NOT_EQUALS:
  case TokenType::LESS_THAN:
  case TokenType::LT_OR_EQ:
  case TokenType::GREATER_THAN:
  case TokenType::GT_OR_EQ:
    return true;
  default:
    return false;
  }
}

void IRGenerator::emitInfixClean(InfixExpression *infix,
                                 const std::shared_ptr<SymbolInfo> &leftSym,
                                 const std::shared_ptr<SymbolInfo> &rightSym) {
  emitCleanup(infix->left_operand.get(), leftSym);
  emitCleanup(infix->right_operand.get(), rightSym);
}

llvm::Value *IRGenerator::handleArithmeticAndBitwise(
    InfixExpression *infix, llvm::Value *left, llvm::Value *right,
    const std::shared_ptr<SymbolInfo> &leftSym,
    const std::shared_ptr<SymbolInfo> &rightSym) {
  auto infixSym = semantics.getSymbolFromMeta(infix);
  auto resultType = infixSym->type;
  llvm::Value *result = nullptr;

  switch (infix->operat.type) {
  case TokenType::PLUS: {
    if (leftSym->type.isPointer) {
      ResolvedType baseTypeInfo = leftSym->type;

      // Strip the copy to avoid mutating the original
      baseTypeInfo.isPointer = false;
      baseTypeInfo.resolvedName =
          semantics.stripPtrSuffix(leftSym->type.resolvedName);

      llvm::Type *baseTy = getLLVMType(baseTypeInfo);
      result = funcBuilder.CreateGEP(baseTy, left, right, "ptr_addtmp");
      return result;
    }

    if (isIntegerType(resultType.kind)) {
      result = funcBuilder.CreateAdd(left, right, "addtmp");
      return result;
    } else {
      result = funcBuilder.CreateFAdd(left, right, "faddtmp");
      return result;
    }
  }

  case TokenType::MINUS: {
    if (leftSym->type.isPointer) {
      ResolvedType baseTypeInfo = leftSym->type;
      // Strip the copy to avoid mutating the original
      baseTypeInfo.isPointer = false;
      baseTypeInfo.resolvedName =
          semantics.stripPtrSuffix(leftSym->type.resolvedName);

      llvm::Type *baseTy = getLLVMType(baseTypeInfo);

      llvm::Value *negRight = funcBuilder.CreateNeg(right, "neg_offset");
      result = funcBuilder.CreateGEP(baseTy, left, negRight, "ptr_subtmp");
      return result;
    }

    if (isIntegerType(resultType.kind)) {
      result = funcBuilder.CreateSub(left, right, "subtmp");
      return result;
    } else {
      result = funcBuilder.CreateFSub(left, right, "fsubtmp");
      return result;
    }
  }

  case TokenType::ASTERISK: {
    if (isIntegerType(resultType.kind)) {
      result = funcBuilder.CreateMul(left, right, "multmp");
      return result;
    } else {
      result = funcBuilder.CreateFMul(left, right, "fmultmp");
      return result;
    }
  }

  case TokenType::DIVIDE: {
    if (isIntegerType(resultType.kind)) {
      result = isSignedInteger(resultType.kind)
                   ? funcBuilder.CreateSDiv(left, right, "divtmp")
                   : funcBuilder.CreateUDiv(left, right, "divtmp");
      return result;
    } else {
      result = funcBuilder.CreateFDiv(left, right, "fdivtmp");
      return result;
    }
  }

  case TokenType::MODULUS: {

    if (isIntegerType(resultType.kind)) {
      result = isSignedInteger(resultType.kind)
                   ? funcBuilder.CreateSRem(left, right, "modtmp")
                   : funcBuilder.CreateURem(left, right, "modtmp");
      emitInfixClean(infix, leftSym, rightSym);
      return result;
    } else
      throw std::runtime_error(
          "Modulus not supported for FLOAT or DOUBLE at line " +
          std::to_string(infix->operat.line));
  }

  case TokenType::BITWISE_AND: {
    result = funcBuilder.CreateAnd(left, right, "andtmp");
    return funcBuilder.CreateAnd(left, right, "andtmp");
  }
  case TokenType::BITWISE_OR: {
    result = funcBuilder.CreateOr(left, right, "ortmp");
    return result;
  }
  case TokenType::BITWISE_XOR: {
    result = funcBuilder.CreateXor(left, right, "xortmp");
    return result;
  }
  case TokenType::SHIFT_LEFT: {
    result = funcBuilder.CreateShl(left, right, "shltmp");
    return result;
  }
  case TokenType::SHIFT_RIGHT: {
    result = isSignedInteger(resultType.kind)
                 ? funcBuilder.CreateAShr(left, right, "ashrtmp")
                 : funcBuilder.CreateLShr(left, right, "lshrtmp");
    return result;
  }
  default:
    throw std::runtime_error(
        "Unsupported infix operator: " + infix->operat.TokenLiteral +
        " at line " + std::to_string(infix->operat.line));
  }
}

llvm::Value *
IRGenerator::handleComparison(InfixExpression *infix, llvm::Value *left,
                              llvm::Value *right,
                              const std::shared_ptr<SymbolInfo> &leftSym,
                              const std::shared_ptr<SymbolInfo> &rightSym) {

  DataType leftType = leftSym->type.kind;
  DataType rightType = rightSym->type.kind;
  llvm::Value *cmpRes = nullptr;

  if (isIntegerType(leftType) || leftType == DataType::ENUM) {
    if (isIntegerType(rightType) || rightType == DataType::ENUM) {
      bool signedInt = isSignedInteger(leftType);
      switch (infix->operat.type) {
      case TokenType::EQUALS:
        cmpRes = funcBuilder.CreateICmpEQ(left, right, "cmptmp");
        break;
      case TokenType::NOT_EQUALS:
        cmpRes = funcBuilder.CreateICmpNE(left, right, "cmptmp");
      case TokenType::LESS_THAN:
        cmpRes = signedInt ? funcBuilder.CreateICmpSLT(left, right, "cmptmp")
                           : funcBuilder.CreateICmpULT(left, right, "cmptmp");
        break;
      case TokenType::LT_OR_EQ:
        cmpRes = signedInt ? funcBuilder.CreateICmpSLE(left, right, "cmptmp")
                           : funcBuilder.CreateICmpULE(left, right, "cmptmp");
        break;
      case TokenType::GREATER_THAN:
        cmpRes = signedInt ? funcBuilder.CreateICmpSGT(left, right, "cmptmp")
                           : funcBuilder.CreateICmpUGT(left, right, "cmptmp");
        break;
      case TokenType::GT_OR_EQ:
        cmpRes = signedInt ? funcBuilder.CreateICmpSGE(left, right, "cmptmp")
                           : funcBuilder.CreateICmpUGE(left, right, "cmptmp");
        break;
      default:
        throw std::runtime_error("Unsupported int comparison operator");
      }

      return cmpRes;
    }
  } else if (leftType == DataType::F32 || leftType == DataType::F64) {
    llvm::Value *cmpRes = nullptr;
    switch (infix->operat.type) {
    case TokenType::EQUALS:
      cmpRes = funcBuilder.CreateFCmpOEQ(left, right, "fcmptmp");
      break;
    case TokenType::NOT_EQUALS:
      cmpRes = funcBuilder.CreateFCmpONE(left, right, "fcmptmp");
      break;
    case TokenType::LESS_THAN:
      cmpRes = funcBuilder.CreateFCmpOLT(left, right, "fcmptmp");
      break;
    case TokenType::LT_OR_EQ:
      cmpRes = funcBuilder.CreateFCmpOLE(left, right, "fcmptmp");
      break;
    case TokenType::GREATER_THAN:
      cmpRes = funcBuilder.CreateFCmpOGT(left, right, "fcmptmp");
      break;
    case TokenType::GT_OR_EQ:
      cmpRes = funcBuilder.CreateFCmpOGE(left, right, "fcmptmp");
      break;
    default:
      throw std::runtime_error("Unsupported float comparison operator");
    }

    return cmpRes;
  } else if (leftSym->type.isPointer || rightSym->type.isPointer) {

    if (left->getType() != right->getType()) {
      right = funcBuilder.CreateBitCast(right, left->getType(), "ptr_cmp_norm");
    }
    llvm::Value *cmpRes = nullptr;
    // Pointers are compared as Unsigned Integers
    switch (infix->operat.type) {
    case TokenType::EQUALS:
      cmpRes = funcBuilder.CreateICmpEQ(left, right, "ptr_eq");
      break;
    case TokenType::NOT_EQUALS:
      cmpRes = funcBuilder.CreateICmpNE(left, right, "ptr_ne");
      break;
    case TokenType::LESS_THAN:
      cmpRes = funcBuilder.CreateICmpULT(left, right, "ptr_lt");
      break;
    case TokenType::LT_OR_EQ:
      cmpRes = funcBuilder.CreateICmpULE(left, right, "ptr_le");
      break;
    case TokenType::GREATER_THAN:
      cmpRes = funcBuilder.CreateICmpUGT(left, right, "ptr_gt");
      break;
    case TokenType::GT_OR_EQ:
      cmpRes = funcBuilder.CreateICmpUGE(left, right, "ptr_ge");
      break;
    default:
      throw std::runtime_error("Unsupported pointer comparison operator");
    }

    return cmpRes;
  } else {
    throw std::runtime_error("Comparison not supported for types '" +
                             leftSym->type.resolvedName + "' and '" +
                             rightSym->type.resolvedName + "'");
  }
  return cmpRes;
}

llvm::Value *IRGenerator::handleCoalesce(InfixExpression *infix,
                                         llvm::Value *leftStruct) {
  // Extract the is_present flag from the null box
  llvm::Value *isPresent =
      funcBuilder.CreateExtractValue(leftStruct, 0, "is_present");

  // Set up basic blocks
  llvm::Function *theFunction = funcBuilder.GetInsertBlock()->getParent();

  // Attach 'hasValueBB' immediately so it follows the current block
  llvm::BasicBlock *hasValueBB =
      llvm::BasicBlock::Create(context, "has_value", theFunction);
  llvm::BasicBlock *isNullBB = llvm::BasicBlock::Create(context, "is_null");
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(context, "coalesce_merge");

  // Branching If true, go to value; if false, go to null handler
  funcBuilder.CreateCondBr(isPresent, hasValueBB, isNullBB);

  // If has Value
  funcBuilder.SetInsertPoint(hasValueBB);
  // Reach into the struct and grab the actual data (index 1)
  llvm::Value *extractedVal =
      funcBuilder.CreateExtractValue(leftStruct, 1, "extracted_val");
  funcBuilder.CreateBr(mergeBB);
  // Update pointer in case builder moved
  hasValueBB = funcBuilder.GetInsertBlock();

  // If it is actually null short circuit it
  isNullBB->insertInto(theFunction);
  funcBuilder.SetInsertPoint(isNullBB);
  // ONLY generate RHS here. If LHS was valid, this code never runs.
  llvm::Value *rightVal = generateExpression(infix->right_operand.get());
  funcBuilder.CreateBr(mergeBB);
  isNullBB = funcBuilder.GetInsertBlock();

  // Merge
  mergeBB->insertInto(theFunction);
  funcBuilder.SetInsertPoint(mergeBB);

  // The PHI node picks the value based on which block we arrived from
  llvm::PHINode *phi =
      funcBuilder.CreatePHI(rightVal->getType(), 2, "coalesce_res");
  phi->addIncoming(extractedVal, hasValueBB);
  phi->addIncoming(rightVal, isNullBB);

  return phi;
}

llvm::Value *
IRGenerator::handleMemberAccess(InfixExpression *infix, llvm::Value *left,
                                llvm::Value *right,
                                const std::shared_ptr<SymbolInfo> &leftSym,
                                const std::shared_ptr<SymbolInfo> &rightSym) {
  std::string parentTypeName = leftSym->type.resolvedName;
  std::string lookUpName = parentTypeName;
  std::string memberName =
      semantics.extractIdentifierName(infix->right_operand.get());

  if (leftSym->type.isPointer)
    lookUpName = semantics.stripPtrSuffix(parentTypeName);
  else if (leftSym->type.isRef)
    lookUpName = semantics.stripRefSuffix(parentTypeName);

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
  llvm::Value *lhsPtr = left;
  if (auto *gv = llvm::dyn_cast<llvm::GlobalVariable>(left)) {
    if (gv->getValueType() != structTy) {
      lhsPtr = funcBuilder.CreateLoad(structTy->getPointerTo(), gv,
                                      gv->getName() + "_loaded");
    }
  } else if (!left->getType()->isPointerTy()) {
    llvm::Value *allocaTmp =
        funcBuilder.CreateAlloca(left->getType(), nullptr, lookUpName + "_tmp");
    funcBuilder.CreateStore(left, allocaTmp);
    lhsPtr = allocaTmp;
  }

  llvm::Value *memberPtr =
      funcBuilder.CreateStructGEP(structTy, lhsPtr, memberIndex, memberName);

  // now return by-value load, not a pointer
  llvm::Value *memberVal =
      funcBuilder.CreateLoad(memberType, memberPtr, memberName + "_val");

  return memberVal;
}

llvm::Value *IRGenerator::handleEnumAccess(InfixExpression *infix) {

  auto enumName = semantics.extractIdentifierName(infix->left_operand.get());
  auto leftTypeIt = semantics.customTypesTable.find(enumName);
  if (leftTypeIt == semantics.customTypesTable.end())
    throw std::runtime_error("Unknown enum '" + enumName + "'");

  auto leftType = leftTypeIt->second->type;

  auto &enumInfo = leftTypeIt->second;

  auto memberName = semantics.extractIdentifierName(infix->right_operand.get());

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

llvm::Value *
IRGenerator::handleLogical(InfixExpression *infix, llvm::Value *left,
                           llvm::Value *right,
                           const std::shared_ptr<SymbolInfo> &leftSym,
                           const std::shared_ptr<SymbolInfo> &rightSym) {
  llvm::Value *result = nullptr;
  if (left->getType() != funcBuilder.getInt1Ty())
    left = funcBuilder.CreateICmpNE(
        left, llvm::ConstantInt::get(left->getType(), 0), "boolcastl");
  if (right->getType() != funcBuilder.getInt1Ty())
    right = funcBuilder.CreateICmpNE(
        right, llvm::ConstantInt::get(right->getType(), 0), "boolcastr");

  if (infix->operat.type == TokenType::AND) {
    result = funcBuilder.CreateAnd(left, right, "andtmp");
    return result;
  } else {
    result = funcBuilder.CreateOr(left, right, "ortmp");
    return result;
  }
}
