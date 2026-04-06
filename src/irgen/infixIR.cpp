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

llvm::Value *IRGenerator::handleArithmeticAndBitwise(
    InfixExpression *infix, llvm::Value *left, llvm::Value *right,
    const std::shared_ptr<SymbolInfo> &leftSym,
    const std::shared_ptr<SymbolInfo> &rightSym) {

  auto infixSym = semantics.getSymbolFromMeta(infix);
  auto resultType = infixSym->type().type;

  switch (infix->operat.type) {
  case TokenType::PLUS: {
    // Pointer arithmetic, GEP into what the pointer points to
    if (leftSym->type().type.isPointer()) {
      if (!leftSym->type().type.innerType)
        reportDevBug("Pointer has no inner type for arithmetic", infix);
      llvm::Type *baseTy = getLLVMType(*leftSym->type().type.innerType);
      return funcBuilder.CreateGEP(baseTy, left, right, "ptr_addtmp");
    }
    if (isIntegerType(resultType.base().kind))
      return funcBuilder.CreateAdd(left, right, "addtmp");
    return funcBuilder.CreateFAdd(left, right, "faddtmp");
  }

  case TokenType::MINUS: {
    // Pointer arithmetic,negative GEP
    if (leftSym->type().type.isPointer()) {
      if (!leftSym->type().type.innerType)
        reportDevBug("Pointer has no inner type for arithmetic", infix);
      llvm::Type *baseTy = getLLVMType(*leftSym->type().type.innerType);
      llvm::Value *negRight = funcBuilder.CreateNeg(right, "neg_offset");
      return funcBuilder.CreateGEP(baseTy, left, negRight, "ptr_subtmp");
    }
    if (isIntegerType(resultType.base().kind))
      return funcBuilder.CreateSub(left, right, "subtmp");
    return funcBuilder.CreateFSub(left, right, "fsubtmp");
  }

  case TokenType::ASTERISK: {
    if (isIntegerType(resultType.base().kind))
      return funcBuilder.CreateMul(left, right, "multmp");
    return funcBuilder.CreateFMul(left, right, "fmultmp");
  }

  case TokenType::DIVIDE: {
    if (isIntegerType(resultType.base().kind)) {
      return isSignedInteger(resultType.base().kind)
                 ? funcBuilder.CreateSDiv(left, right, "divtmp")
                 : funcBuilder.CreateUDiv(left, right, "divtmp");
    }
    return funcBuilder.CreateFDiv(left, right, "fdivtmp");
  }

  case TokenType::MODULUS: {
    if (!isIntegerType(resultType.base().kind))
      throw std::runtime_error(
          "Modulus not supported for float types at line " +
          std::to_string(infix->operat.line));
    return isSignedInteger(resultType.base().kind)
               ? funcBuilder.CreateSRem(left, right, "modtmp")
               : funcBuilder.CreateURem(left, right, "modtmp");
  }

  case TokenType::BITWISE_AND:
    return funcBuilder.CreateAnd(left, right, "andtmp");

  case TokenType::BITWISE_OR:
    return funcBuilder.CreateOr(left, right, "ortmp");

  case TokenType::BITWISE_XOR:
    return funcBuilder.CreateXor(left, right, "xortmp");

  case TokenType::SHIFT_LEFT:
    return funcBuilder.CreateShl(left, right, "shltmp");

  case TokenType::SHIFT_RIGHT:
    return isSignedInteger(resultType.base().kind)
               ? funcBuilder.CreateAShr(left, right, "ashrtmp")
               : funcBuilder.CreateLShr(left, right, "lshrtmp");

  default:
    reportDevBug("Unsupported infix operator: " + infix->operat.TokenLiteral +
                     " at line " + std::to_string(infix->operat.line),
                 infix);
    return nullptr;
  }
}

llvm::Value *
IRGenerator::handleComparison(InfixExpression *infix, llvm::Value *left,
                              llvm::Value *right,
                              const std::shared_ptr<SymbolInfo> &leftSym,
                              const std::shared_ptr<SymbolInfo> &rightSym) {

  DataType leftType = leftSym->type().type.kind;
  DataType rightType = rightSym->type().type.kind;
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
        break;
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
      reportDevBug("Unsupported float comparison operator", infix);
      break;
    }

    return cmpRes;
  } else if (leftSym->type().type.isPointer() || rightSym->type().type.isPointer()) {

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
      reportDevBug("Unsupported pointer comparison operator", infix);
      break;
    }

    return cmpRes;
  } else {
    reportDevBug("Comparison not supported for types '" +
                     leftSym->type().type.resolvedName + "' and '" +
                     rightSym->type().type.resolvedName + "'",
                 infix);
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
                                const std::shared_ptr<SymbolInfo> &leftSym,
                                const std::shared_ptr<SymbolInfo> &rightSym) {
  std::string parentTypeName = leftSym->type().type.resolvedName;
  std::string lookUpName = parentTypeName;
  std::string memberName =
      semantics.extractIdentifierName(infix->right_operand.get());

  if (leftSym->type().type.isPointer() || leftSym->type().type.isRef())
    lookUpName = semantics.getBaseTypeName(leftSym->type().type);

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
  llvm::LoadInst *memberVal =
      funcBuilder.CreateLoad(memberType, memberPtr, memberName + "_val");

  // Check if the member itself is volatile
  if (memberIt->second->isVolatile) {
    memberVal->setVolatile(true);
  }
  // Also check if the parent is volatile (volatile propagates to members)
  else if (leftSym->storage().isVolatile) {
    memberVal->setVolatile(true);
  }

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

llvm::Value *IRGenerator::handleLogical(InfixExpression *infix,
                                         llvm::Value *left,
                                         const std::shared_ptr<SymbolInfo> &leftSym,
                                         const std::shared_ptr<SymbolInfo> &rightSym) {
    // Ensure left is i1
    if (left->getType() != funcBuilder.getInt1Ty())
        left = funcBuilder.CreateICmpNE(
            left, llvm::Constant::getNullValue(left->getType()), "boolcastl");

    llvm::Function *fn = funcBuilder.GetInsertBlock()->getParent();

    if (infix->operat.type == TokenType::AND) {
        // Short circuit AND:
        // if left is false, skip right and return false
        llvm::BasicBlock *evalRight = llvm::BasicBlock::Create(context, "and.rhs", fn);
        llvm::BasicBlock *merge    = llvm::BasicBlock::Create(context, "and.merge", fn);

        llvm::BasicBlock *leftBlock = funcBuilder.GetInsertBlock();
        funcBuilder.CreateCondBr(left, evalRight, merge);

        // Evaluate right only if left was true
        funcBuilder.SetInsertPoint(evalRight);
        llvm::Value *right = generateExpression(infix->right_operand.get());
        if (right->getType() != funcBuilder.getInt1Ty())
            right = funcBuilder.CreateICmpNE(
                right, llvm::Constant::getNullValue(right->getType()), "boolcastr");
        llvm::BasicBlock *rightBlock = funcBuilder.GetInsertBlock();
        funcBuilder.CreateBr(merge);

        funcBuilder.SetInsertPoint(merge);
        llvm::PHINode *phi = funcBuilder.CreatePHI(funcBuilder.getInt1Ty(), 2, "andtmp");
        phi->addIncoming(funcBuilder.getFalse(), leftBlock);
        phi->addIncoming(right, rightBlock);
        return phi;

    } else {
        // Short circuit OR:
        // if left is true, skip right and return true
        llvm::BasicBlock *evalRight = llvm::BasicBlock::Create(context, "or.rhs", fn);
        llvm::BasicBlock *merge    = llvm::BasicBlock::Create(context, "or.merge", fn);

        llvm::BasicBlock *leftBlock = funcBuilder.GetInsertBlock();
        funcBuilder.CreateCondBr(left, merge, evalRight);

        // Evaluate right only if left was false
        funcBuilder.SetInsertPoint(evalRight);
        llvm::Value *right = generateExpression(infix->right_operand.get());
        if (right->getType() != funcBuilder.getInt1Ty())
            right = funcBuilder.CreateICmpNE(
                right, llvm::Constant::getNullValue(right->getType()), "boolcastr");
        llvm::BasicBlock *rightBlock = funcBuilder.GetInsertBlock();
        funcBuilder.CreateBr(merge);

        funcBuilder.SetInsertPoint(merge);
        llvm::PHINode *phi = funcBuilder.CreatePHI(funcBuilder.getInt1Ty(), 2, "ortmp");
        phi->addIncoming(funcBuilder.getTrue(), leftBlock);
        phi->addIncoming(right, rightBlock);
        return phi;
    }
}
