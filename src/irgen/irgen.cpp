#include "irgen.hpp"

#include <inttypes.h>
#include <llvm-18/llvm/IR/Constants.h>
#include <llvm-18/llvm/IR/DataLayout.h>
#include <llvm-18/llvm/IR/DerivedTypes.h>
#include <llvm-18/llvm/IR/Instruction.h>
#include <llvm-18/llvm/IR/Type.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/CodeGen/TargetPassConfig.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

#include "ast.hpp"
#include "audit.hpp"
#include "errors.hpp"
#include "llvm/TargetParser/Host.h"

IRGenerator::IRGenerator(Semantics &semantics, ErrorHandler &handler, Auditor &auditor,
                         size_t totalHeap, bool isVerbose, OptLevel optLevel)
    : optLevel(optLevel),
      context(),
      funcBuilder(context),
      module(std::make_unique<llvm::Module>("unnameable", context)),
      semantics(semantics),
      errorHandler(handler),
      auditor(auditor),
      totalHeapSize(totalHeap),
      isVerbose(isVerbose) {
    setupTargetLayout();

    registerGeneratorFunctions();
    registerExpressionGeneratorFunctions();
    registerAddressGeneratorFunctions();

    declareCustomTypes();
    declareImportedTypes();
    declareImportedSeals();
    declareImportedFunctions();
    registerAllocators();
}

// MAIN GENERATOR FUNCTION
void IRGenerator::generate(const std::vector<std::unique_ptr<Node>> &program) {
    // Generate program body
    for (const auto &node : program) {
        generateStatement(node.get());
    }
}

// MAIN GENERATOR FUNCTION FOR EXPRESSION
//  Main Expression generator helper function
llvm::Value *IRGenerator::generateExpression(Node *node) {
    if (!node) {
        throw std::runtime_error("Null node sent to the expression generator dispatcher");
    }
    auto exprIt = expressionGeneratorsMap.find(typeid(*node));
    if (exprIt == expressionGeneratorsMap.end()) {
        reportDevBug("Could not find expression type IR generator for: " + node->toString(), node);
    }

    return (this->*exprIt->second)(node);
}

// Main L-value generator helper functions
llvm::Value *IRGenerator::generateAddress(Node *node) {
    if (!node) {
        reportDevBug("Null node sent to the address generator dispatcher", node);
    }

    auto addrIt = addressGeneratorsMap.find(typeid(*node));
    if (addrIt == addressGeneratorsMap.end()) {
        errorHandler.addHint(
            "The address generator(L-value) for this node does not exist "
            "in the address generator functions map");
        reportDevBug("Could not find address generator for " + node->toString(), node);
    }

    return (this->*addrIt->second)(node);
}

// GENERATOR FUNCTIONS
void IRGenerator::generateStatement(Node *node) {
    auto generatorIt = generatorFunctionsMap.find(typeid(*node));
    if (generatorIt == generatorFunctionsMap.end()) {
        return;
    }
    (this->*generatorIt->second)(node);
}

// Expression statement IR generator function
void IRGenerator::generateExpressionStatement(Node *node) {
    auto exprStmt = dynamic_cast<ExpressionStatement *>(node);
    if (!exprStmt) {
        throw std::runtime_error("Invalid expression statement node");
    }
    generateExpression(exprStmt->expression.get());
}

void IRGenerator::generatePointerAssignmentStatement(AssignmentStatement *assignStmt) {
    auto assignSym = semantics.getSymbolFromMeta(assignStmt);
    if (!assignSym) return;

    logInternal("Inside the pointer reassignment");
    inhibitCleanUp = true;
    llvm::Value *targetPtr = generateAddress(assignStmt->identifier.get());
    llvm::Value *newAddr = generateExpression(assignStmt->value.get());

    auto *storeInst = funcBuilder.CreateStore(newAddr, targetPtr);
    if (assignSym->storage().isVolatile) {
        storeInst->setVolatile(true);
    }
    inhibitCleanUp = false;

    emitCleanup(assignStmt);
}

// Assignment statement IR generator function
void IRGenerator::generateAssignmentStatement(Node *node) {
    auto *assignStmt = dynamic_cast<AssignmentStatement *>(node);
    if (!assignStmt) return;

    auto assignSym = semantics.getSymbolFromMeta(assignStmt);
    if (!assignSym) return;

    // Pointer reassignment has its own path
    if (assignSym->type().type.isPointer()) {
        generatePointerAssignmentStatement(assignStmt);
        return;
    }

    inhibitCleanUp = true;

    llvm::Value *targetPtr = generateAddress(assignStmt->identifier.get());
    if (!targetPtr) reportDevBug("Failed to get L-Value address for LHS", assignStmt);

    llvm::Value *initValue = generateExpression(assignStmt->value.get());
    if (!initValue) reportDevBug("Failed to generate R-Value for assignment", assignStmt);

    auto valSym = semantics.getSymbolFromMeta(assignStmt->value.get());
    auto assignMeta = semantics.metaData[assignStmt];
    if (!assignMeta) reportDevBug("Failed to get assignment metaData", assignStmt);

    // Deep copy, arrays and large structs
    bool isHuge =
        assignMeta->type().type.isBase() && (assignMeta->type().type.kind == DataType::COMPONENT ||
                                             assignMeta->type().type.kind == DataType::RECORD);

    if (assignMeta->type().type.isArray() || isHuge) {
        uint64_t byteSize = assignMeta->codegen().componentSize;
        llvm::Type *elemLLVMTy =
            getLLVMType(assignMeta->type().type.isArray()
                            ? semantics.getArrayElementType(assignMeta->type().type)
                            : assignMeta->type().type);
        llvm::Align align = layout->getABITypeAlign(elemLLVMTy);
        funcBuilder.CreateMemCpy(targetPtr, align, initValue, align,
                                 funcBuilder.getInt64(byteSize));
        inhibitCleanUp = false;
        emitCleanup(assignStmt);
        return;
    }

    // Scalar assignment
    llvm::StoreInst *storeInst = nullptr;
    if (assignMeta->storage().isHeap) {
        llvm::Value *valueToStore = initValue;
        if (valSym && valSym->type().isAddress) {
            valueToStore = funcBuilder.CreateLoad(getLLVMType(assignMeta->type().type), initValue,
                                                  "loaded_val");
            if (valSym->storage().isVolatile) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(valueToStore))
                    load->setVolatile(true);
            }
        }
        storeInst = funcBuilder.CreateStore(valueToStore, targetPtr);
    } else {
        storeInst = funcBuilder.CreateStore(initValue, targetPtr);
    }

    if (assignSym->storage().isVolatile && storeInst) storeInst->setVolatile(true);

    inhibitCleanUp = false;
    emitCleanup(assignStmt);
    logInternal("Ended assignment generation");
}

void IRGenerator::generateFieldAssignmentStatement(Node *node) {
    auto *fieldStmt = dynamic_cast<FieldAssignment *>(node);
    if (!fieldStmt) return;

    auto fieldSym = semantics.getSymbolFromMeta(fieldStmt);
    auto lhsSym = semantics.getSymbolFromMeta(fieldStmt->lhs_chain.get());
    auto valSym = semantics.getSymbolFromMeta(fieldStmt->value.get());

    if (!fieldSym || !lhsSym || !valSym) {
        reportDevBug("Missing symbols in field assignment", node);
        return;
    }

    logInternal("Generating field assignment");
    inhibitCleanUp = true;

    llvm::Value *fieldAddress = generateInfixAddress(fieldStmt->lhs_chain.get());
    if (!fieldAddress) reportDevBug("Failed to get field address", fieldStmt);

    // Pointer field reassignment
    if (fieldSym->type().type.isPointer()) {
        llvm::Value *newAddr = generateExpression(fieldStmt->value.get());
        auto *storeInst = funcBuilder.CreateStore(newAddr, fieldAddress);
        if (fieldSym->storage().isVolatile) storeInst->setVolatile(true);
        inhibitCleanUp = false;
        emitCleanup(fieldStmt);
        return;
    }

    llvm::Value *rhsValue = generateExpression(fieldStmt->value.get());
    // Deep copy, arrays and large structs not behind a pointer
    bool isHuge =
        fieldSym->type().type.isBase() && (fieldSym->type().type.kind == DataType::COMPONENT ||
                                           fieldSym->type().type.kind == DataType::RECORD);

    if (fieldSym->type().type.isArray() || isHuge) {
        logInternal("Handling deep copy to field");
        uint64_t byteSize = fieldSym->codegen().componentSize;
        llvm::Type *elemLLVMTy = getLLVMType(
            fieldSym->type().type.isArray() ? semantics.getArrayElementType(fieldSym->type().type)
                                            : fieldSym->type().type);
        llvm::Align align = layout->getABITypeAlign(elemLLVMTy);

        if (fieldSym->type().type.isArray()) {
            llvm::Value *fieldBasePtr =
                funcBuilder.CreateLoad(funcBuilder.getPtrTy(), fieldAddress, "field_array_base");
            if (fieldSym->storage().isVolatile) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(fieldBasePtr))
                    load->setVolatile(true);
            }
            funcBuilder.CreateMemCpy(fieldBasePtr, align, rhsValue, align,
                                     funcBuilder.getInt64(byteSize));
        } else {
            funcBuilder.CreateMemCpy(fieldAddress, align, rhsValue, align,
                                     funcBuilder.getInt64(byteSize));
        }

        inhibitCleanUp = false;
        emitCleanup(fieldStmt);
        return;
    }

    // Scalar assignment
    llvm::StoreInst *storeInst = nullptr;
    if (fieldSym->storage().isHeap) {
        llvm::Value *valueToStore = rhsValue;
        if (valSym->type().isAddress) {
            valueToStore = funcBuilder.CreateLoad(getLLVMType(fieldSym->type().type), rhsValue,
                                                  "loaded_field_val");
            if (valSym->storage().isVolatile) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(valueToStore))
                    load->setVolatile(true);
            }
        }
        storeInst = funcBuilder.CreateStore(valueToStore, fieldAddress);
    } else {
        storeInst = funcBuilder.CreateStore(rhsValue, fieldAddress);
    }

    if (fieldSym->storage().isVolatile && storeInst) storeInst->setVolatile(true);

    inhibitCleanUp = false;
    emitCleanup(fieldStmt);
    logInternal("Ended field assignment generation");
}

void IRGenerator::generateBlockStatement(Node *node) {
    auto blockStmt = dynamic_cast<BlockStatement *>(node);
    if (!blockStmt) {
        reportDevBug("Invalid block statement", node);
    }

    logInternal("Generating block statement with '" + std::to_string(blockStmt->statements.size()) +
                "' statements");
    for (const auto &stmt : blockStmt->statements) {
        auto currentBlock = funcBuilder.GetInsertBlock();
        if (currentBlock && currentBlock->getTerminator()) {
            break;
        }

        generateStatement(stmt.get());

        currentBlock = funcBuilder.GetInsertBlock();
        if (currentBlock && currentBlock->getTerminator()) {
            break;
        }
    }
}

void IRGenerator::generateTraceStatement(Node *node) {
    auto traceStmt = dynamic_cast<TraceStatement *>(node);

    if (!traceStmt) {
        reportDevBug("Invalid trace statement node", node);
    }

    inhibitCleanUp = true;
    for (const auto &expr : traceStmt->arguments) {
        auto val = generateExpression(expr.get());
        if (!val) reportDevBug("Failed to get value for the trace expression", traceStmt);

        auto sym = semantics.getSymbolFromMeta(expr.get());
        if (!sym) reportDevBug("Failed to get argument info", expr.get());

        traceRuntime(val, sym->type().type);
    }
    inhibitCleanUp = false;

    // Clean up loop
    for (const auto &expr : traceStmt->arguments) emitCleanup(expr.get());
}

llvm::Value *IRGenerator::generateBlockExpression(Node *node) {
    auto blockExpr = dynamic_cast<BlockExpression *>(node);
    if (!blockExpr) {
        errorHandler.addHint(
            "Sent a wrong node to the block expression generator "
            "could be a malformed AST");
        reportDevBug("Invalid block expression", blockExpr);
    }

    for (const auto &stmts : blockExpr->statements) {
        // Check if the current block is already terminated by a return or branch
        if (funcBuilder.GetInsertBlock()->getTerminator()) {
            logInternal("Skipping statement- block terminated");
            break;
        }
        generateStatement(stmts.get());
    }

    // After generating all statements, check if we have a terminator
    llvm::BasicBlock *currentBlock = funcBuilder.GetInsertBlock();
    llvm::Instruction *terminator = currentBlock ? currentBlock->getTerminator() : nullptr;

    if (terminator) {
        // Save the terminator (return) position
        llvm::ReturnInst *ret = llvm::dyn_cast<llvm::ReturnInst>(terminator);
        if (ret) {
            // Remove the terminator temporarily
            terminator->removeFromParent();

            // Insert cleanup at the current insertion point (which is before where
            // the return was)
            emitBlockCleanUp(blockExpr);

            // Re-insert the return
            funcBuilder.Insert(ret);
        } else {
            // Other terminator (branch, etc.) - just add cleanup before it
            emitBlockCleanUp(blockExpr);
        }
    } else {
        // No terminator, just add cleanup
        emitBlockCleanUp(blockExpr);
    }

    // A block expression should not return an llvm::Value directly.
    return nullptr;
}

// HELPER FUNCTIONS
llvm::Type *IRGenerator::getLLVMType(const ResolvedType &type) {
    // ── Modifier cases — recurse first ───────────────────
    if (type.isPointer() || type.isRef()) {
        llvm::Type *ptrTy = llvm::PointerType::get(context, 0);
        if (type.isNull) {
            return llvm::StructType::get(context, {llvm::Type::getInt1Ty(context), ptrTy});
        }
        return ptrTy;
    }

    if (type.isArray()) {
        if (!type.innerType) reportDevBug("Array type has no inner element type", nullptr);
        llvm::Type *elemTy = getLLVMType(*type.innerType);
        llvm::Type *arrTy = llvm::PointerType::get(elemTy, 0);
        if (type.isNull) {
            return llvm::StructType::get(context, {llvm::Type::getInt1Ty(context), arrTy});
        }
        return arrTy;
    }

    // ── Base case ─────────────────────────────────────────
    llvm::Type *baseType = nullptr;
    switch (type.kind) {
        case DataType::I8:
        case DataType::U8:
        case DataType::CHAR8:
            baseType = llvm::Type::getInt8Ty(context);
            break;

        case DataType::I16:
        case DataType::U16:
        case DataType::CHAR16:
            baseType = llvm::Type::getInt16Ty(context);
            break;

        case DataType::I32:
        case DataType::U32:
        case DataType::CHAR32:
            baseType = llvm::Type::getInt32Ty(context);
            break;

        case DataType::I64:
        case DataType::U64:
            baseType = llvm::Type::getInt64Ty(context);
            break;

        case DataType::I128:
        case DataType::U128:
            baseType = llvm::Type::getInt128Ty(context);
            break;

        case DataType::ISIZE:
        case DataType::USIZE:
            baseType = module->getDataLayout().getIntPtrType(context);
            break;

        case DataType::BOOLEAN:
            baseType = llvm::Type::getInt1Ty(context);
            break;

        case DataType::F32:
            baseType = llvm::Type::getFloatTy(context);
            break;

        case DataType::F64:
            baseType = llvm::Type::getDoubleTy(context);
            break;

        case DataType::STRING:
        case DataType::OPAQUE:
            baseType = llvm::PointerType::get(context, 0);
            break;

        case DataType::VOID:
            baseType = llvm::Type::getVoidTy(context);
            break;

        case DataType::RECORD:
        case DataType::COMPONENT: {
            if (type.resolvedName.empty()) {
                errorHandler.addHint("Semantics probably stored a type with empty name");
                reportDevBug("Custom type requested but resolvedName is empty", nullptr);
            }
            auto it = llvmCustomTypes.find(type.resolvedName);
            if (it == llvmCustomTypes.end()) {
                errorHandler.addHint("Type '" + type.resolvedName +
                                     "' was not stored in the IRGenerator custom type map");
                reportDevBug("IRGenerator requested unknown type '" + type.resolvedName + "'",
                             nullptr);
            }
            baseType = it->second;
            break;
        }

        case DataType::ENUM: {
            auto enumIt = semantics.customTypesTable.find(type.resolvedName);
            if (enumIt == semantics.customTypesTable.end()) {
                reportDevBug("IRGenerator requested unknown enum '" + type.resolvedName + "'",
                             nullptr);
            }
            baseType = getLLVMType(
                ResolvedType::makeBase(enumIt->second->underLyingType, type.resolvedName));
            break;
        }

        case DataType::ERROR:
        case DataType::GENERIC:
        case DataType::UNKNOWN:
            errorHandler.addHint("Semantics failed to guard against unknown type leaks");
            reportDevBug("Unknown type '" + type.resolvedName + "' reached IRGenerator", nullptr);
    }

    if (type.isNull) {
        return llvm::StructType::get(context, {llvm::Type::getInt1Ty(context), baseType});
    }

    return baseType;
}

// Registering generator functions for statements
void IRGenerator::registerGeneratorFunctions() {
    generatorFunctionsMap[typeid(VariableDeclaration)] = &IRGenerator::generateVariableDeclaration;
    generatorFunctionsMap[typeid(ExpressionStatement)] = &IRGenerator::generateExpressionStatement;
    generatorFunctionsMap[typeid(AssignmentStatement)] = &IRGenerator::generateAssignmentStatement;
    generatorFunctionsMap[typeid(FieldAssignment)] = &IRGenerator::generateFieldAssignmentStatement;
    generatorFunctionsMap[typeid(WhileStatement)] = &IRGenerator::generateWhileStatement;
    generatorFunctionsMap[typeid(ForStatement)] = &IRGenerator::generateForStatement;
    generatorFunctionsMap[typeid(ifStatement)] = &IRGenerator::generateIfStatement;
    generatorFunctionsMap[typeid(SwitchStatement)] = &IRGenerator::generateSwitchStatement;
    generatorFunctionsMap[typeid(BreakStatement)] = &IRGenerator::generateBreakStatement;
    generatorFunctionsMap[typeid(ContinueStatement)] = &IRGenerator::generateContinueStatement;
    generatorFunctionsMap[typeid(BlockStatement)] = &IRGenerator::generateBlockStatement;
    generatorFunctionsMap[typeid(FunctionStatement)] = &IRGenerator::generateFunctionStatement;
    generatorFunctionsMap[typeid(ReturnStatement)] = &IRGenerator::generateReturnStatement;
    generatorFunctionsMap[typeid(FunctionDeclaration)] = &IRGenerator::generateFunctionDeclaration;
    // Special case
    generatorFunctionsMap[typeid(FunctionDeclarationExpression)] =
        &IRGenerator::generateFunctionDeclarationExpression;
    // Component system
    generatorFunctionsMap[typeid(RecordStatement)] = &IRGenerator::generateRecordStatement;
    generatorFunctionsMap[typeid(ComponentStatement)] = &IRGenerator::generateComponentStatement;
    generatorFunctionsMap[typeid(EnumStatement)] = &IRGenerator::generateEnumStatement;
    generatorFunctionsMap[typeid(TraceStatement)] = &IRGenerator::generateTraceStatement;
    generatorFunctionsMap[typeid(InstantiateStatement)] =
        &IRGenerator::generateInstantiateStatement;
    generatorFunctionsMap[typeid(SealStatement)] = &IRGenerator::generateSealStatement;
    generatorFunctionsMap[typeid(AllocatorStatement)] = &IRGenerator::generateAllocatorInterface;
}

void IRGenerator::registerAddressGeneratorFunctions() {
    addressGeneratorsMap[typeid(SelfExpression)] = &IRGenerator::generateSelfAddress;
    addressGeneratorsMap[typeid(CallExpression)] = &IRGenerator::generateCallAddress;
    addressGeneratorsMap[typeid(ArraySubscript)] = &IRGenerator::generateArraySubscriptAddress;
    addressGeneratorsMap[typeid(DereferenceExpression)] = &IRGenerator::generateDereferenceAddress;
    addressGeneratorsMap[typeid(Identifier)] = &IRGenerator::generateIdentifierAddress;
    addressGeneratorsMap[typeid(InfixExpression)] = &IRGenerator::generateInfixAddress;
}

void IRGenerator::registerExpressionGeneratorFunctions() {
    expressionGeneratorsMap[typeid(InfixExpression)] = &IRGenerator::generateInfixExpression;
    expressionGeneratorsMap[typeid(PrefixExpression)] = &IRGenerator::generatePrefixExpression;
    expressionGeneratorsMap[typeid(PostfixExpression)] = &IRGenerator::generatePostfixExpression;
    expressionGeneratorsMap[typeid(StringLiteral)] = &IRGenerator::generateStringLiteral;
    expressionGeneratorsMap[typeid(FStringLiteral)] = &IRGenerator::generateFStringLiteral;
    expressionGeneratorsMap[typeid(Char8Literal)] = &IRGenerator::generateChar8Literal;
    expressionGeneratorsMap[typeid(Char16Literal)] = &IRGenerator::generateChar16Literal;
    expressionGeneratorsMap[typeid(Char32Literal)] = &IRGenerator::generateChar32Literal;
    expressionGeneratorsMap[typeid(BooleanLiteral)] = &IRGenerator::generateBooleanLiteral;
    expressionGeneratorsMap[typeid(I8Literal)] = &IRGenerator::generateI8Literal;
    expressionGeneratorsMap[typeid(U8Literal)] = &IRGenerator::generateU8Literal;
    expressionGeneratorsMap[typeid(I16Literal)] = &IRGenerator::generateI16Literal;
    expressionGeneratorsMap[typeid(U16Literal)] = &IRGenerator::generateU16Literal;
    expressionGeneratorsMap[typeid(I32Literal)] = &IRGenerator::generateI32Literal;
    expressionGeneratorsMap[typeid(U32Literal)] = &IRGenerator::generateU32Literal;
    expressionGeneratorsMap[typeid(I64Literal)] = &IRGenerator::generateI64Literal;
    expressionGeneratorsMap[typeid(U64Literal)] = &IRGenerator::generateU64Literal;
    expressionGeneratorsMap[typeid(I128Literal)] = &IRGenerator::generateI128Literal;
    expressionGeneratorsMap[typeid(U128Literal)] = &IRGenerator::generateU128Literal;
    expressionGeneratorsMap[typeid(ISIZELiteral)] = &IRGenerator::generateISIZELiteral;
    expressionGeneratorsMap[typeid(USIZELiteral)] = &IRGenerator::generateUSIZELiteral;
    expressionGeneratorsMap[typeid(INTLiteral)] = &IRGenerator::generateINTLiteral;
    expressionGeneratorsMap[typeid(F32Literal)] = &IRGenerator::generateF32Literal;
    expressionGeneratorsMap[typeid(F64Literal)] = &IRGenerator::generateF64Literal;
    expressionGeneratorsMap[typeid(FloatLiteral)] = &IRGenerator::generateFloatLiteral;
    expressionGeneratorsMap[typeid(ArrayLiteral)] = &IRGenerator::generateArrayLiteral;
    expressionGeneratorsMap[typeid(NullLiteral)] = &IRGenerator::generateNullLiteral;
    expressionGeneratorsMap[typeid(Identifier)] = &IRGenerator::generateIdentifierExpression;
    expressionGeneratorsMap[typeid(SizeOfExpression)] = &IRGenerator::generateSizeOfExpression;
    expressionGeneratorsMap[typeid(CastExpression)] = &IRGenerator::generateCastExpression;
    expressionGeneratorsMap[typeid(BitcastExpression)] = &IRGenerator::generateBitcastExpression;
    expressionGeneratorsMap[typeid(AddressExpression)] = &IRGenerator::generateAddressExpression;
    expressionGeneratorsMap[typeid(DereferenceExpression)] =
        &IRGenerator::generateDereferenceExpression;
    expressionGeneratorsMap[typeid(BlockExpression)] = &IRGenerator::generateBlockExpression;
    expressionGeneratorsMap[typeid(CallExpression)] = &IRGenerator::generateCallExpression;
    expressionGeneratorsMap[typeid(UnwrapExpression)] = &IRGenerator::generateUnwrapExpression;
    expressionGeneratorsMap[typeid(MethodCallExpression)] =
        &IRGenerator::generateMethodCallExpression;
    expressionGeneratorsMap[typeid(SelfExpression)] = &IRGenerator::generateSelfExpression;
    expressionGeneratorsMap[typeid(InstanceExpression)] = &IRGenerator::generateInstanceExpression;
    expressionGeneratorsMap[typeid(ArraySubscript)] =
        &IRGenerator::generateArraySubscriptExpression;
}

char IRGenerator::decodeCharLiteral(const std::string &literal) {
    if (literal.empty()) return '\0';
    // Just return the first byte of the already-decoded UTF-8 string
    return literal[0];
}

uint32_t IRGenerator::decodeUTF8ToCodePoint(const std::string &literal) {
    if (literal.empty()) return 0;

    unsigned char first = static_cast<unsigned char>(literal[0]);
    if (first < 0x80) return first;  // Simple ASCII

    // Reassemble the bits
    uint32_t cp = 0;
    size_t len = literal.length();

    if ((first & 0xE0) == 0xC0 && len >= 2) {
        cp = ((first & 0x1F) << 6) | (static_cast<unsigned char>(literal[1]) & 0x3F);
    } else if ((first & 0xF0) == 0xE0 && len >= 3) {
        cp = ((first & 0x0F) << 12) | ((static_cast<unsigned char>(literal[1]) & 0x3F) << 6) |
             (static_cast<unsigned char>(literal[2]) & 0x3F);
    } else if ((first & 0xF8) == 0xF0 && len >= 4) {
        cp = ((first & 0x07) << 18) | ((static_cast<unsigned char>(literal[1]) & 0x3F) << 12) |
             ((static_cast<unsigned char>(literal[2]) & 0x3F) << 6) |
             (static_cast<unsigned char>(literal[3]) & 0x3F);
    }
    return cp;
}

uint16_t IRGenerator::decodeChar16Literal(const std::string &literal) {
    return static_cast<uint16_t>(decodeUTF8ToCodePoint(literal));
}

uint32_t IRGenerator::decodeChar32Literal(const std::string &literal) {
    return decodeUTF8ToCodePoint(literal);
}

llvm::Value *IRGenerator::coerceToBoolean(llvm::Value *val, Node *exprNode) {
    if (!val) return nullptr;

    auto it = semantics.metaData.find(exprNode);
    if (it == semantics.metaData.end()) {
    }

    auto sym = semantics.getSymbolFromMeta(exprNode);
    if (!sym) {
        reportDevBug("Could not find condition expression symbol info", exprNode);
        return nullptr;
    }

    // Handle Nullable Boxes {i1, T}
    if (sym->type().isNullable) {
        // If we have a pointer to the box (from an alloca or global), load the
        // struct first
        if (val->getType()->isPointerTy()) {
            // Check if it's a pointer to a struct, not just any pointer
            llvm::Type *boxType = getLLVMType(sym->type().type);
            val = funcBuilder.CreateLoad(boxType, val, "nullable.load");
        }
        // The boolean 'is_present' is always at index 0
        return funcBuilder.CreateExtractValue(val, {0}, "is_present");
    }

    // Handle Booleans (i1)
    if (val->getType()->isIntegerTy(1)) {
        return val;
    }

    // Handle Raw Pointers
    if (val->getType()->isPointerTy()) {
        // Pointers are compared against the null pointer constant
        llvm::Value *nullPtr =
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(val->getType()));
        return funcBuilder.CreateICmpNE(val, nullPtr, "ptr.not_null");
    }

    // Handle Scalars (i8, i32, i64, etc.)
    if (val->getType()->isIntegerTy()) {
        return funcBuilder.CreateICmpNE(val, llvm::ConstantInt::get(val->getType(), 0),
                                        "coerce.bool");
    }

    reportDevBug("Attempted to coerce a non-nullable aggregate to boolean", exprNode);
    return funcBuilder.getInt1(false);
}

bool IRGenerator::isDynamicArrayLiteral(ArrayLiteral *literal) {
    for (const auto &element : literal->array) {
        if (auto nested = dynamic_cast<ArrayLiteral *>(element.get())) {
            if (isDynamicArrayLiteral(nested)) return true;

            continue;
        }

        if (!semantics.isConstLiteral(element.get())) {
            return true;
        }
    }

    return false;
}

llvm::Type *IRGenerator::getArrayBaseType(const ResolvedType &type) {
    // If it's an array, keep digging.
    if (type.isArray() && type.innerType) {
        return getArrayBaseType(*type.innerType);
    }

    logInternal("ARRAY LITERAL ELEMENT TYPE: " + type.resolvedName);

    return getLLVMType(type);
}

void IRGenerator::emitDynamicInitialization(ArrayLiteral *arrLit, llvm::Value *ptr,
                                            llvm::Type *baseTy, int &index) {
    for (auto &element : arrLit->array) {
        if (auto nested = dynamic_cast<ArrayLiteral *>(element.get())) {
            emitDynamicInitialization(nested, ptr, baseTy, index);
        } else {
            // This call generates the Load/Instruction for 'x'
            llvm::Value *val = generateExpression(element.get());

            // Calculate my_arr[index]
            auto *slotPtr = funcBuilder.CreateGEP(baseTy, ptr, funcBuilder.getInt64(index++));

            // The Baton sees this Store and says: "Inhibit dealloc for the source ID!"
            funcBuilder.CreateStore(val, slotPtr);
        }
    }
}

size_t IRGenerator::getFlatCount(ArrayLiteral *arrLit) {
    size_t count = 0;
    for (auto &el : arrLit->array) {
        if (auto nested = dynamic_cast<ArrayLiteral *>(el.get())) {
            count += getFlatCount(nested);
        } else {
            count++;
        }
    }
    return count;
}

uint32_t IRGenerator::convertIntTypeToWidth(const DataType &dt) {
    switch (dt) {
        case DataType::I8:
        case DataType::U8:
            return 8;
        case DataType::I16:
        case DataType::U16:
            return 16;
        case DataType::I32:
        case DataType::U32:
            return 32;
        case DataType::I64:
        case DataType::U64:
            return 64;
        case DataType::I128:
        case DataType::U128:
            return 128;
        case DataType::ISIZE:
        case DataType::USIZE:
            return layout->getPointerSizeInBits();
        default:
            return 32;
    }
}

bool IRGenerator::isIntegerType(const DataType &dt) {
    switch (dt) {
        case DataType::I8:
        case DataType::U8:
        case DataType::I16:
        case DataType::U16:
        case DataType::I32:
        case DataType::U32:
        case DataType::I64:
        case DataType::U64:
        case DataType::I128:
        case DataType::U128:
        case DataType::ISIZE:
        case DataType::USIZE:
            return true;
        default:
            return false;
    }
}

bool IRGenerator::isSignedInteger(const DataType &dt) {
    switch (dt) {
        case DataType::I8:
        case DataType::I16:
        case DataType::I32:
        case DataType::I64:
        case DataType::I128:
        case DataType::ISIZE:
            return true;
        default:
            return false;
    }
}

bool IRGenerator::isUnsigned(const ResolvedType &type) {
    auto dt = type.kind;
    switch (dt) {
        case DataType::U8:
        case DataType::U16:
        case DataType::U32:
        case DataType::U64:
        case DataType::U128:
        case DataType::USIZE:
            return true;
        default:
            return false;
    }
}

unsigned IRGenerator::getIntegerBitWidth(DataType dt) {
    switch (dt) {
        case DataType::I8:
        case DataType::U8:
            return 8;
        case DataType::I16:
        case DataType::U16:
            return 16;
        case DataType::I32:
        case DataType::U32:
            return 32;
        case DataType::I64:
        case DataType::U64:
            return 64;
        case DataType::I128:
        case DataType::U128:
            return 128;
        default:
            return 0;  // Not an integer type
    }
}

char *IRGenerator::const_unnitoa(__int128 val, char *buf) {
    char temp[64];  // Enough for 128-bit decimal digits (max ~39 digits).
    int i = 0;
    int neg = 0;

    if (val == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return buf;
    }

    if (val < 0) {
        neg = 1;
        val = -val;
    }

    // Extract digits into temp (reversed)
    while (val > 0) {
        __int128 digit = val % 10;
        val /= 10;
        temp[i++] = '0' + (int)digit;
    }

    if (neg) temp[i++] = '-';

    // Reverse into buf
    int j = 0;
    while (i > 0) buf[j++] = temp[--i];

    buf[j] = '\0';
    return buf;
}

void IRGenerator::traceRuntime(llvm::Value *val, ResolvedType type) {
    if (!val) reportDevBug("trace called with a null value", nullptr);

    auto i32Ty = llvm::Type::getInt32Ty(context);
    auto i64Ty = llvm::Type::getInt64Ty(context);
    // auto ptrTy = llvm::PointerType::getUnqual(context); // Generic Opaque Pointer

    // This turns I8, U128, BOOLEAN, or F-STRING into a char*
    llvm::Value *strPtr = stringizeValue(val, type);

    llvm::Function *writeFn = getOrDeclareWrite();
    llvm::Function *strlenFn = getOrDeclareStrlen();

    // Print main value
    auto fd = llvm::ConstantInt::get(i32Ty, 1);  // 1 = STDOUT

    auto len = funcBuilder.CreateCall(strlenFn, {strPtr}, "msg_len");

    // write(fd, buf, len)
    funcBuilder.CreateCall(writeFn, {fd, strPtr, len});

    // Automatic newline
    // CreateGlobalStringPtr returns a 'ptr' to a null-terminated string
    llvm::Value *newline = funcBuilder.CreateGlobalStringPtr("\n");

    // Hardcode 1 for the newline length to save a strlen call
    auto nlLen = llvm::ConstantInt::get(i64Ty, 1);
    funcBuilder.CreateCall(writeFn, {fd, newline, nlLen});
}

void IRGenerator::freeDynamicHeapStorage(const std::string &allocatorType, llvm::Value *toFree) {
    auto it = semantics.allocatorMap.find(allocatorType);
    if (it == semantics.allocatorMap.end()) return;

    auto deallocatorName = it->second.freeName;
    llvm::Function *deallocFunc = module->getFunction(deallocatorName);

    llvm::Value *castPtr =
        funcBuilder.CreatePointerCast(toFree, deallocFunc->getFunctionType()->getParamType(0));

    funcBuilder.CreateCall(deallocFunc, {castPtr});
}

void IRGenerator::freeForeigners(Node *block) {
    auto &map = auditor.foreignersToFree;

    logInternal(
        "\n[FOREINER-CLEANUP] >>> Entering Foreigner Cleanup for "
        "block at line: " +
        block->toString());
    logInternal("[LOOP KEY] " + std::to_string((uintptr_t)block));

    auto it = map.find(block);
    if (it == map.end()) {
        logInternal("  [SKIP] No foreigners registered for this block. Basket is empty.");
        return;
    }

    logInternal("  [BASKET-FOUND] Found " + std::to_string(it->second.size()) +
                " batons to process.");

    std::set<std::string> freedBatons;

    for (auto &[baton, contextSym] : it->second) {
        if (!baton) continue;

        if (freedBatons.count(baton->ID)) {
            logInternal("  [SKIP] Already freed: " + baton->ID);
            continue;
        }

        logInternal("  [INSPECTING] Baton ID: " + baton->ID);

        if (!contextSym) {
            logInternal("    [ERROR] Metadata missing for holder of " + baton->ID);
            continue;
        }

        logInternal("    [TERMINAL-REACHED] Commencing deallocation sequence for: " +
                    contextSym->codegen().ID);

        // Clear out dependents (skip if already freed)
        if (!baton->dependents.empty()) {
            logInternal("    [DEPS] Freeing " + std::to_string(baton->dependents.size()) +
                        " dependents...");
            for (const auto &[depID, depSym] : baton->dependents) {
                if (freedBatons.count(depID)) {
                    logInternal("      -> Skipping (already freed): " + depID);
                    continue;
                }
                logInternal("      -> Killing Dependent: " + depID);
                executePhysicalFree(depSym);
                freedBatons.insert(depID);
            }
        }

        // Kill the leader
        if (contextSym->storage().isHeap) {
            logInternal("    [LEADER] Emitting physical free for: " + contextSym->codegen().ID);
            executePhysicalFree(contextSym);
        }

        // Mark as dead
        baton->isResponsible = false;
        baton->isAlive = false;
        freedBatons.insert(baton->ID);
        logInternal("    [SUCCESS] Baton " + baton->ID + " is now officially history.");
    }

    map.erase(block);
    logInternal("[FOREINER-CLEANUP] <<< Finished processing foreigners.\n");
}

void IRGenerator::freeNatives(Node *block) {
    auto &map = auditor.nativesToFree;

    logInternal(
        "\n[NATIVE-CLEANUP] >>> Entering Native Cleanup for block "
        "at line: " +
        block->toString());
    logInternal("[LOOP KEY] " + std::to_string((uintptr_t)block));

    auto it = map.find(block);
    if (it == map.end()) {
        logInternal("  [SKIP] No natives registered for this loop. Basket is empty.");
        return;
    }

    logInternal("  [BASKET-FOUND] Found " + std::to_string(it->second.size()) +
                " batons to process.");

    std::set<std::string> freedBatons;

    for (auto &[baton, contextSym] : it->second) {
        if (!baton) continue;

        if (freedBatons.count(baton->ID)) {
            logInternal("  [SKIP] Already freed: " + baton->ID);
            continue;
        }

        logInternal("  [INSPECTING] Baton ID: " + baton->ID);

        if (!contextSym) {
            logInternal("    [ERROR] Metadata missing for holder of " + baton->ID);
            continue;
        }

        logInternal("    [TERMINAL-REACHED] Commencing deallocation sequence for: " +
                    contextSym->codegen().ID);

        // Clear out dependents (skip if they're already freed)
        if (!baton->dependents.empty()) {
            logInternal("    [DEPS] Freeing " + std::to_string(baton->dependents.size()) +
                        " dependents...");
            for (const auto &[depID, depSym] : baton->dependents) {
                if (freedBatons.count(depID)) {
                    logInternal("      -> Skipping (already freed): " + depID);
                    continue;
                }
                logInternal("      -> Killing Dependent: " + depID);
                executePhysicalFree(depSym);
                freedBatons.insert(depID);
            }
        }

        // Kill the leader
        if (contextSym->storage().isHeap) {
            logInternal("    [LEADER] Emitting physical free for: " + contextSym->codegen().ID);
            executePhysicalFree(contextSym);
        }

        // Mark as dead
        baton->isResponsible = false;
        baton->isAlive = false;
        freedBatons.insert(baton->ID);
        logInternal("    [SUCCESS] Baton " + baton->ID + " is now officially history.");
    }

    map.erase(block);
    logInternal("[NATIVE-CLEANUP] <<< Finished processing natives.\n");
}

void IRGenerator::emitBlockCleanUp(Node *block) {
    if (!block) return;

    logInternal("[IR-BUNKER-CLEANUP] Processing cleanup for block at: " +
                std::to_string((uintptr_t)block));
    freeNatives(block);
    freeForeigners(block);
}

void IRGenerator::executePhysicalFree(const std::shared_ptr<SymbolInfo> &sym) {
    if (!sym) {
        logInternal("  [EXEC-FREE-FAIL] Symbol is null.");
        return;
    }

    logInternal("  [EXEC-FREE] ID: " + sym->codegen().ID +
                " | HasLLVM: " + (sym->codegen().llvmValue ? "True" : "False") +
                " | IsHeap: " + (sym->storage().isHeap ? "True" : "False"));

    if (!sym->codegen().llvmValue || !sym->storage().isHeap) return;

    logInternal("  [Deallocating] Generating  deallocation for: " + sym->codegen().ID);

    auto it = semantics.allocatorMap.find(sym->storage().allocType);
    if (it == semantics.allocatorMap.end()) {
        logInternal("  [EXEC-FREE-FAIL] No allocator mapping for type allocator type '" +
                    sym->storage().allocType + "'");
        return;
    }

    auto deallocatorName = it->second.freeName;
    llvm::Function *deallocFunc = module->getFunction(deallocatorName);

    if (!deallocFunc) {
        logInternal("  [EXEC-FREE-FAIL] Could not find LLVM function: " + deallocatorName);
        return;
    }

    logInternal("  [EMITTING-CALL] @ " + deallocatorName + " for " + sym->codegen().ID);

    llvm::Value *castPtr = funcBuilder.CreatePointerCast(
        sym->codegen().llvmValue, deallocFunc->getFunctionType()->getParamType(0));

    funcBuilder.CreateCall(deallocFunc, {castPtr});
}

void IRGenerator::emitDeclarationClean(Node *contextNode) {
    logInternal("[DECL-CLEAN] Node: " + contextNode->toString());

    auto it = semantics.responsibilityTable.find(contextNode);
    if (it == semantics.responsibilityTable.end()) {
        logInternal("  [DECL-CLEAN] No baton associated with this node: " +
                    contextNode->toString());
        return;
    }

    auto &baton = it->second;
    if (!baton) {
        logInternal("  [DECL-CLEAN] Baton pointer is null.");
        return;
    }

    auto sym = semantics.getSymbolFromMeta(contextNode);
    if (!sym)
        reportDevBug("Failed to get identifier symbol info for node: " + contextNode->toString(),
                     contextNode);

    logInternal("  [DECL-CLEAN] ID: " + sym->codegen().ID +
                " | Responsible: " + (baton->isResponsible ? "T" : "F") +
                " | PtrCount: " + std::to_string(baton->ptrCount) +
                " | RefCount: " + std::to_string(baton->refCount));

    if (!baton->isResponsible) {
        logInternal("  [DECL-CLEAN] Baton is not responsible for memory.");
        return;
    }

    // Trigger Logic (Counts)
    if (baton->ptrCount == 0 && baton->refCount == 0) {
        logInternal("  [DECL-CLEAN] Last use detected. Commencing physical free.");
        // Free the depenedents first
        logInternal("  [Dependents] Size: " + std::to_string(baton->dependents.size()));
        for (const auto &[id, depSym] : baton->dependents) {
            logInternal("    [Dep-Free] ID: " + id);
            executePhysicalFree(depSym);
        }

        // Free the leader
        if (sym->storage().isHeap) {
            logInternal("  [Leader] ID: " + sym->codegen().ID);
            executePhysicalFree(sym);
        }

        // Defuse to avoid issues
        baton->isResponsible = false;
        logInternal("  [DECL-CLEAN] Responsibility revoked for: " + sym->codegen().ID);
    } else {
        logInternal("  [DECL-CLEAN] Baton still has active references/pointers.");
    }
}

void IRGenerator::emitCleanup(Node *contextNode) {
    if (inhibitCleanUp) {
        logInternal("[NORMAL-CLEAN] Cleanup inhibited");
        return;
    }
    logInternal("[NORMAL-CLEAN] Node: " + contextNode->toString());
    if (dynamic_cast<Identifier *>(contextNode) != contextNode)
        logInternal("[NORMAL-CLEAN] Composite node detected");

    auto identifiers = semantics.digIdentifiers(contextNode);
    for (const auto &identifier : identifiers) {
        auto it = semantics.responsibilityTable.find(identifier);
        if (it == semantics.responsibilityTable.end()) {
            logInternal("  [NORMAL-CLEAN] No baton associated with this node: " +
                        identifier->toString());
            continue;
        }

        auto &baton = it->second;
        if (!baton) {
            logInternal("  [NORMAL-CLEAN] Baton pointer is null.");
            return;
        }

        auto identSym = semantics.getSymbolFromMeta(identifier);
        if (!identSym)
            reportDevBug("Failed to get identifier symbol info for '" +
                             semantics.extractIdentifierName(identifier) +
                             "' in node: " + contextNode->toString(),
                         identifier);

        logInternal("  [NORMAL-CLEAN] ID: " + identSym->codegen().ID +
                    " | Responsible: " + (baton->isResponsible ? "T" : "F") +
                    " | PtrCount: " + std::to_string(baton->ptrCount) +
                    " | RefCount: " + std::to_string(baton->refCount));

        if (!baton->isResponsible) {
            logInternal("  [NORMAL-CLEAN] Baton is not responsible for memory.");
            return;
        }

        // Trigger Logic (Counts)
        if (baton->ptrCount == 0 && baton->refCount == 0) {
            logInternal("  [NORMAL-CLEAN] Last use detected. Commencing physical free.");
            // Free the depenedents first
            logInternal("  [Dependents] Size: " + std::to_string(baton->dependents.size()));
            for (const auto &[id, depSym] : baton->dependents) {
                logInternal("    [Dep-Free] ID: " + id);
                executePhysicalFree(depSym);
            }

            // Free the leader
            if (identSym->storage().isHeap) {
                logInternal("  [Leader] ID: " + identSym->codegen().ID);
                executePhysicalFree(identSym);
            }

            // Defuse to avoid issues
            baton->isResponsible = false;
            logInternal("  [NORMAL-CLEAN] Responsibility revoked for: " + identSym->codegen().ID);
        } else {
            logInternal("  [NORMAL-CLEAN] Baton still has active references/pointers.");
        }
    }
}

llvm::Value *IRGenerator::generateIntegerLiteral(const std::string &literalStr, uint32_t bitWidth,
                                                 bool isSigned) {
    int base = 10;
    std::string cleanStr = literalStr;
    if (literalStr.size() > 2) {
        if (literalStr[1] == 'x' || literalStr[1] == 'X') {
            base = 16;
            cleanStr = literalStr.substr(2);
        } else if (literalStr[1] == 'b' || literalStr[1] == 'B') {
            base = 2;
            cleanStr = literalStr.substr(2);
        }
    }

    // Create a WIDE APInt to hold the value safely during the check.
    // Using a 129 bits so a 128-bit number can't "accidentally" overflow it.
    llvm::APInt wideVal(129, cleanStr, base);

    // Perform the Range Check
    if (isSigned) {
        // Create the min/max bounds for the target bitWidth
        llvm::APInt minBound = llvm::APInt::getSignedMinValue(bitWidth).sext(129);
        llvm::APInt maxBound = llvm::APInt::getSignedMaxValue(bitWidth).sext(129);

        // slt = Signed Less Than, sgt = Signed Greater Than
        if (wideVal.slt(minBound) || wideVal.sgt(maxBound)) {
            throw std::runtime_error("Overflow: Value out of range for signed i" +
                                     std::to_string(bitWidth));
        }
    } else {
        llvm::APInt maxBound = llvm::APInt::getMaxValue(bitWidth).zext(129);

        // ult = Unsigned Less Than, ugt = Unsigned Greater Than
        if (wideVal.ugt(maxBound)) {
            throw std::runtime_error("Overflow: Value out of range for unsigned u" +
                                     std::to_string(bitWidth));
        }
    }

    // Shrink the wide value down to the actual target size
    return llvm::ConstantInt::get(context, wideVal.trunc(bitWidth));
}

llvm::Value *IRGenerator::stringizeValue(llvm::Value *val, const ResolvedType &type) {
    auto i8Ty = llvm::Type::getInt8Ty(context);
    auto i128Ty = llvm::Type::getInt128Ty(context);
    auto f64Ty = llvm::Type::getDoubleTy(context);

    if (type.isRef()) {
        ResolvedType targetType = semantics.peelRef(type);
        return stringizeValue(val,targetType);
    }

    // POINTERS & OPAQUE (The "Address" handler)
    if (type.isPointer() || type.kind == DataType::OPAQUE) {
        llvm::Value *addrInt = funcBuilder.CreatePtrToInt(val, llvm::Type::getInt64Ty(context));
        llvm::Value *buf = funcBuilder.CreateAlloca(i8Ty, 0, funcBuilder.getInt64(32), "ptr_buf");
        funcBuilder.CreateCall(getOrDeclareUnniptoa(), {addrInt, buf});
        return buf;
    }

    // BASE TYPES (Modifier::NONE)
    if (type.isBase()) {
        switch (type.kind) {
            case DataType::STRING:
                return val;  // Already a ptr to chars

            case DataType::BOOLEAN: {
                // Return static global strings "true" or "false"
                auto *trueStr = funcBuilder.CreateGlobalStringPtr("true");
                auto *falseStr = funcBuilder.CreateGlobalStringPtr("false");
                return funcBuilder.CreateSelect(val, trueStr, falseStr);
            }

            // All Integers (I8 through I128, ISIZE, USIZE)
            case DataType::I8:
            case DataType::U8:
            case DataType::I16:
            case DataType::U16:
            case DataType::I32:
            case DataType::U32:
            case DataType::I64:
            case DataType::U64:
            case DataType::I128:
            case DataType::U128:
            case DataType::ISIZE:
            case DataType::USIZE: {
                // Cast everything to i128 for the universal printer
                // isSigned depends on whether the DataType starts with 'I' or 'U'
                bool isSigned = (type.kind == DataType::I8 || type.kind == DataType::I16 ||
                                 type.kind == DataType::I32 || type.kind == DataType::I64 ||
                                 type.kind == DataType::I128 || type.kind == DataType::ISIZE);

                llvm::Value *extended = funcBuilder.CreateIntCast(val, i128Ty, isSigned);
                llvm::Value *buf =
                    funcBuilder.CreateAlloca(i8Ty, 0, funcBuilder.getInt64(64), "int_buf");
                funcBuilder.CreateCall(getOrDeclareUnnitoa(), {extended, buf});
                return buf;
            }

            case DataType::F32:
            case DataType::F64: {
                llvm::Value *dval = funcBuilder.CreateFPCast(val, f64Ty);
                llvm::Value *buf =
                    funcBuilder.CreateAlloca(i8Ty, 0, funcBuilder.getInt64(64), "float_buf");
                funcBuilder.CreateCall(getOrDeclareUnnftoa(), {dval, buf});
                return buf;
            }
            case DataType::CHAR8:
            case DataType::CHAR16:
            case DataType::CHAR32: {
                llvm::Value *buf =
                    funcBuilder.CreateAlloca(i8Ty, 0, funcBuilder.getInt64(2), "char_buf");

                llvm::Value *charVal;
                if (val->getType()->isFloatingPointTy()) {
                    // Convert float to int (e.g., 65.0 -> 65/'A')
                    charVal = funcBuilder.CreateFPToUI(val, i8Ty);
                } else {
                    // Standard integer truncation (e.g., i32 -> i8)
                    charVal = funcBuilder.CreateIntCast(val, i8Ty, false);
                }

                funcBuilder.CreateStore(charVal, buf);

                // Null terminate the second byte
                auto *nullPos = funcBuilder.CreateGEP(i8Ty, buf, funcBuilder.getInt64(1));
                funcBuilder.CreateStore(llvm::ConstantInt::get(i8Ty, 0), nullPos);
                return buf;
            }

            default:
                return funcBuilder.CreateGlobalStringPtr("<unsupported>");
        }
    }

    if (type.isArray()) return funcBuilder.CreateGlobalStringPtr("<array>");

    return funcBuilder.CreateGlobalStringPtr("<unknown>");
}

llvm::Function *IRGenerator::getOrDeclareStrlen() {
    if (auto *f = module->getFunction("strlen")) return f;

    auto i64Ty = llvm::Type::getInt64Ty(context);
    auto ptrTy = llvm::PointerType::getUnqual(context);

    auto *fty = llvm::FunctionType::get(i64Ty, {ptrTy}, false);
    return llvm::Function::Create(fty, llvm::Function::ExternalLinkage, "strlen", *module);
}

llvm::Function *IRGenerator::getOrDeclareUnnitoa() {
    if (auto *f = module->getFunction("unnitoa")) return f;

    auto ptrTy = llvm::PointerType::getUnqual(context);
    auto i128Ty = llvm::Type::getInt128Ty(context);

    // Signature is ptr unnitoa(i128, ptr)
    auto *fty = llvm::FunctionType::get(ptrTy, {i128Ty, ptrTy}, false);
    return llvm::Function::Create(fty, llvm::Function::ExternalLinkage, "unnitoa", *module);
}

llvm::Function *IRGenerator::getOrDeclareUnnftoa() {
    if (auto *f = module->getFunction("unnftoa")) return f;
    auto f64Ty = llvm::Type::getDoubleTy(context);
    auto ptrTy = llvm::PointerType::getUnqual(context);

    // Signature is ptr unnftoa(f64,ptr)
    auto *funcTy = llvm::FunctionType::get(ptrTy, {f64Ty, ptrTy}, false);
    return llvm::Function::Create(funcTy, llvm::Function::ExternalLinkage, "unnftoa", *module);
}

llvm::Function *IRGenerator::getOrDeclareUnniptoa() {
    if (auto *func = module->getFunction("unniptoa")) return func;
    auto i64Ty = llvm::Type::getInt64Ty(context);
    auto ptrTy = llvm::PointerType::getUnqual(context);

    // Signature is ptr unniptoa(u64,ptr)
    auto *funcTy = llvm::FunctionType::get(ptrTy, {i64Ty, ptrTy}, false);
    return llvm::Function::Create(funcTy, llvm::Function::ExternalLinkage, "unniptoa", *module);
}

llvm::Function *IRGenerator::getOrDeclareStrcat() {
    if (auto *f = module->getFunction("unn_strcat")) return f;
    auto ptrTy = llvm::PointerType::getUnqual(context);

    // Signature, ptr unn_strcat(ptr dest, ptr src)
    auto *fty = llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy}, false);

    return llvm::Function::Create(fty, llvm::Function::ExternalLinkage, "unn_strcat", *module);
}

llvm::Function *IRGenerator::getOrDeclareWrite() {
    if (auto *func = module->getFunction("write")) {
        return func;
    }

    auto i64Ty = llvm::Type::getInt64Ty(context);
    auto i32Ty = llvm::Type::getInt32Ty(context);
    auto ptrTy = llvm::PointerType::getUnqual(context);

    std::vector<llvm::Type *> args = {i32Ty, ptrTy, i64Ty};
    auto *funcTy = llvm::FunctionType::get(i64Ty, args, false);

    llvm::Function *writeFunc =
        llvm::Function::Create(funcTy, llvm::Function::ExternalLinkage, "write", *module);

    writeFunc->addParamAttr(1, llvm::Attribute::NoCapture);

    return writeFunc;
}

llvm::Value *IRGenerator::calculateFStringSize(FStringLiteral *fStr) {
    auto i64Ty = llvm::Type::getInt64Ty(context);
    llvm::Value *totalSize = llvm::ConstantInt::get(i64Ty, 1);
    for (const auto &seg : fStr->segments) {
        if (seg.string_part) {
            auto str = dynamic_cast<StringLiteral *>(seg.string_part.get());
            uint64_t len = str->string_token.TokenLiteral.length();
            totalSize = funcBuilder.CreateAdd(totalSize, llvm::ConstantInt::get(i64Ty, len));
        }

        for (const auto &valExpr : seg.values) {
            auto sym = semantics.getSymbolFromMeta(valExpr.get());
            if (!sym) reportDevBug("Missing value info", valExpr.get());

            llvm::Value *val = generateExpression(valExpr.get());

            llvm::Value *strVal = stringizeValue(val, sym->type().type);
            llvm::Value *strLen = funcBuilder.CreateCall(getOrDeclareStrlen(), {strVal});

            totalSize = funcBuilder.CreateAdd(totalSize, strLen);
        }
    }
    return totalSize;
}

llvm::GlobalVariable *IRGenerator::createGlobalArrayConstant(llvm::Constant *constantArray) {
    llvm::ArrayType *arrayTy = llvm::cast<llvm::ArrayType>(constantArray->getType());

    if (!arrayTy) {
        // If the constant is NOT an ArrayType, something is fundamentally wrong
        // with the output of generateArrayLiteral.
        throw std::runtime_error(
            "Attempted to create a global array constant from a non-array type.");
    }

    // Create the Global Variable
    llvm::GlobalVariable *globalArray =
        new llvm::GlobalVariable(*module,  // The owning module
                                 arrayTy,  // The type of the global variable
                                 true,     // IsConstant (Read-only)
                                 llvm::GlobalValue::PrivateLinkage,  // Linkage
                                 constantArray,  // The initializer constant value
                                 "array.init"    // Name
        );
    // Set alignment for safety
    globalArray->setAlignment(llvm::MaybeAlign(arrayTy->getPrimitiveSizeInBits() / 8));
    return globalArray;
}

void IRGenerator::dumpIR() { module->print(llvm::outs(), nullptr); }

bool IRGenerator::currentBlockIsTerminated() {
    llvm::BasicBlock *bb = funcBuilder.GetInsertBlock();
    return bb && bb->getTerminator();
}

llvm::Module &IRGenerator::getLLVMModule() { return *module; }

void IRGenerator::setupTargetLayout() {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();

    std::string targetTripleStr = llvm::sys::getDefaultTargetTriple();
    module->setTargetTriple(targetTripleStr);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);
    if (!target) throw std::runtime_error("Target not found");

    // Convert OptLevel for layout (though layout mostly uses it for ABI
    // decisions)
    llvm::CodeGenOptLevel llvmOptLevel;
    switch (optLevel) {
        case OptLevel::NONE:
            llvmOptLevel = llvm::CodeGenOptLevel::None;
            break;
        case OptLevel::BASIC:
            llvmOptLevel = llvm::CodeGenOptLevel::Less;
            break;
        case OptLevel::STANDARD:
            llvmOptLevel = llvm::CodeGenOptLevel::Default;
            break;
        case OptLevel::AGGRESSIVE:
            llvmOptLevel = llvm::CodeGenOptLevel::Aggressive;
            break;
        default:
            llvmOptLevel = llvm::CodeGenOptLevel::None;
            break;
    }

    llvm::TargetOptions opt;

    // Create target machine WITH optimization level
    std::optional<llvm::Reloc::Model> relocModel = llvm::Reloc::PIC_;
    std::optional<llvm::CodeModel::Model> codeModel = std::nullopt;

    auto targetMachine =
        target->createTargetMachine(targetTripleStr, "generic", "", opt, relocModel, codeModel,
                                    llvmOptLevel,  // Optimization level passed here
                                    false);        //  Optional unique suffix

    if (!targetMachine) throw std::runtime_error("Failed to create TargetMachine");

    module->setDataLayout(targetMachine->createDataLayout());
    this->layout = &module->getDataLayout();
}

bool IRGenerator::emitObjectFile(const std::string &filename) {
    // Initialization
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    // Target Setup
    std::string targetTripleStr = llvm::sys::getDefaultTargetTriple();
    module->setTargetTriple(targetTripleStr);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);
    if (!target) {
        llvm::errs() << "Failed to find target: " << error << "\n";
        return false;
    }

    llvm::TargetOptions opt;

    // Convert our OptLevel to LLVM optimization level
    llvm::CodeGenOptLevel llvmOptLevel;
    switch (optLevel) {
        case OptLevel::NONE:
            llvmOptLevel = llvm::CodeGenOptLevel::None;
            break;
        case OptLevel::BASIC:
            llvmOptLevel = llvm::CodeGenOptLevel::Less;
            break;
        case OptLevel::STANDARD:
            llvmOptLevel = llvm::CodeGenOptLevel::Default;
            break;
        case OptLevel::AGGRESSIVE:
            llvmOptLevel = llvm::CodeGenOptLevel::Aggressive;
            break;
        default:
            llvmOptLevel = llvm::CodeGenOptLevel::None;
            break;
    }

    // CPU and features using generic for now
    std::string cpu = "generic";
    std::string features = "";

    // Create target machine with optimization level
    std::optional<llvm::Reloc::Model> relocModel = llvm::Reloc::PIC_;
    std::optional<llvm::CodeModel::Model> codeModel = std::nullopt;

    auto targetMachine =
        target->createTargetMachine(targetTripleStr, cpu, features, opt, relocModel, codeModel,
                                    llvmOptLevel,  // Optimization level
                                    false);        // Optional unique suffix

    if (!targetMachine) {
        llvm::errs() << "Failed to create TargetMachine\n";
        return false;
    }

    // Set data layout from target machine
    module->setDataLayout(targetMachine->createDataLayout());

    // Open output file
    std::error_code EC;
    llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);
    if (EC) {
        llvm::errs() << "Could not open file: " << EC.message() << "\n";
        return false;
    }

    // Create pass manager
    llvm::legacy::PassManager pass;

    // Set file type to object file
    llvm::CodeGenFileType fileType = llvm::CodeGenFileType::ObjectFile;

    // Add passes to emit object file
    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
        llvm::errs() << "TargetMachine can't emit object file\n";
        return false;
    }

    // Run the passes
    pass.run(*module);

    // Flush output
    dest.flush();

    return true;
}

void IRGenerator::reportDevBug(const std::string &message, Node *contextNode) {
    int line = 0;
    int col = 0;
    if (contextNode) {
        line = contextNode->token.line;
        col = contextNode->token.column;
    }

    CompilerError error;
    error.level = ErrorLevel::INTERNAL;
    error.line = line;
    error.col = col;
    error.message = message;
    error.tokenLength = errorHandler.getTokenLength(contextNode);
    error.hints = {};

    errorHandler.report(error);

    std::abort();
}

void IRGenerator::logInternal(const std::string &message) {
    if (isVerbose) {
        std::cout << message << "\n";
    }
}
