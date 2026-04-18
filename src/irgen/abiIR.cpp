#include "irgen.hpp"
#include <llvm/IR/DataLayout.h>
#include <vector>

// Merge two classifications according to SysV ABI rules
Classification IRGenerator::mergeClassifications(Classification c1, Classification c2) {
    if (c1 == Classification::NO_CLASS || c2 == Classification::NO_CLASS) {
        return c1 == Classification::NO_CLASS ? c2 : c1;
    }
    if (c1 == Classification::MEMORY || c2 == Classification::MEMORY) {
        return Classification::MEMORY;
    }
    if (c1 == Classification::INTEGER || c2 == Classification::INTEGER) {
        return Classification::INTEGER;
    }
    if (c1 == Classification::X87 || c2 == Classification::X87 ||
        c1 == Classification::COMPLEX_X87 || c2 == Classification::COMPLEX_X87) {
        return Classification::MEMORY;
    }
    return Classification::SSE;
}

// Classify a single 8-byte word of a struct
void IRGenerator::classifyWord(uint64_t offset, uint64_t size, llvm::Type* type,
                                std::vector<Classification>& classes) {
    uint64_t wordIndex = offset / 8;
    uint64_t wordOffset = offset % 8;

    // Calculate word size safely
    uint64_t remainingInWord = 8 - wordOffset;
    [[maybe_unused]] uint64_t wordSize = (size < remainingInWord) ? size : remainingInWord;

    Classification cls = Classification::NO_CLASS;

    // Determine classification based on type
    if (type->isIntegerTy() || type->isPointerTy()) {
        cls = Classification::INTEGER;
    } else if (type->isFloatTy()) {
        cls = Classification::SSE;
    } else if (type->isDoubleTy()) {
        cls = Classification::SSE;
    } else if (type->isStructTy()) {
        auto* structTy = llvm::cast<llvm::StructType>(type);
        const llvm::DataLayout& DL = module->getDataLayout();
        auto* structLayout = DL.getStructLayout(structTy);

        for (unsigned i = 0; i < structTy->getNumElements(); i++) {
            llvm::Type* elemTy = structTy->getElementType(i);
            uint64_t elemOffset = structLayout->getElementOffset(i);
            uint64_t elemSize = DL.getTypeAllocSize(elemTy);
            classifyWord(offset + elemOffset, elemSize, elemTy, classes);
        }
        return;
    } else if (type->isArrayTy()) {
        auto* arrayTy = llvm::cast<llvm::ArrayType>(type);
        llvm::Type* elemTy = arrayTy->getElementType();
        uint64_t elemSize = module->getDataLayout().getTypeAllocSize(elemTy);
        uint64_t numElems = arrayTy->getNumElements();

        for (uint64_t i = 0; i < numElems; i++) {
            classifyWord(offset + i * elemSize, elemSize, elemTy, classes);
        }
        return;
    } else if (type->isVectorTy()) {
        cls = Classification::SSE;
    } else {
        cls = Classification::MEMORY;
    }

    // Ensure classes vector has enough elements
    while (classes.size() <= wordIndex) {
        classes.push_back(Classification::NO_CLASS);
    }

    classes[wordIndex] = mergeClassifications(classes[wordIndex], cls);
}

// Create coerced type from classification
llvm::Type* IRGenerator::createCoercedType(const std::vector<Classification>& classes,
                                            uint64_t size) {
    if (size == 0) {
        return llvm::Type::getVoidTy(context);
    }

    // If classified as MEMORY, signal byval
    for (auto cls : classes) {
        if (cls == Classification::MEMORY) {
            return nullptr;
        }
    }

    std::vector<llvm::Type*> coercedTypes;

    for (size_t i = 0; i < classes.size(); i++) {
        uint64_t remaining = (i * 8 < size) ? (size - i * 8) : 0;
        if (remaining == 0) break;

        uint64_t wordSize = (remaining < 8) ? remaining : 8;

        switch (classes[i]) {
            case Classification::INTEGER:
                coercedTypes.push_back(llvm::IntegerType::get(context, wordSize * 8));
                break;
            case Classification::SSE:
                if (wordSize == 4) {
                    coercedTypes.push_back(llvm::Type::getFloatTy(context));
                } else if (wordSize == 8) {
                    coercedTypes.push_back(llvm::Type::getDoubleTy(context));
                } else {
                    coercedTypes.push_back(llvm::IntegerType::get(context, wordSize * 8));
                }
                break;
            case Classification::SSEUP:
                coercedTypes.push_back(llvm::IntegerType::get(context, wordSize * 8));
                break;
            default:
                break;
        }
    }

    if (coercedTypes.empty()) {
        return llvm::Type::getVoidTy(context);
    }

    if (coercedTypes.size() == 1) {
        return coercedTypes[0];
    }

    return llvm::StructType::get(context, coercedTypes);
}

// Main classification function
CoercionInfo IRGenerator::classifyStruct(llvm::StructType* structTy) {
    CoercionInfo info;
    info.isMemory = false;
    info.coercedType = nullptr;

    const llvm::DataLayout& DL = module->getDataLayout();
    uint64_t size = DL.getTypeAllocSize(structTy);

    // Rule: > 16 bytes -> MEMORY
    if (size > 16) {
        info.isMemory = true;
        return info;
    }

    // Classify each 8-byte word
    std::vector<Classification> classes;

    for (unsigned i = 0; i < structTy->getNumElements(); i++) {
        llvm::Type* elemTy = structTy->getElementType(i);
        uint64_t offset = DL.getStructLayout(structTy)->getElementOffset(i);
        uint64_t elemSize = DL.getTypeAllocSize(elemTy);
        classifyWord(offset, elemSize, elemTy, classes);
    }

    // Post-processing rules
    if (classes.size() == 2) {
        // If one class is SSE and the other is INTEGER, they merge to INTEGER
        if ((classes[0] == Classification::SSE && classes[1] == Classification::INTEGER) ||
            (classes[0] == Classification::INTEGER && classes[1] == Classification::SSE)) {
            classes[0] = Classification::INTEGER;
            classes[1] = Classification::INTEGER;
        }

        // If the second word is SSEUP, treat as SSE
        if (classes[1] == Classification::SSEUP) {
            classes[1] = Classification::SSE;
        }
    }

    // Check if any word is MEMORY
    for (auto cls : classes) {
        if (cls == Classification::MEMORY) {
            info.isMemory = true;
            return info;
        }
    }

    info.classes = classes;
    info.coercedType = createCoercedType(classes, size);

    if (!info.coercedType && !info.isMemory) {
        info.isMemory = true;
    }

    return info;
}

// Coerce an argument value for function call
llvm::Value* IRGenerator::coerceArgument(llvm::Value* arg, const CoercionInfo& info) {
    if (info.isMemory) {
        llvm::AllocaInst* tempAlloca = funcBuilder.CreateAlloca(arg->getType(), nullptr, "byval.tmp");
        funcBuilder.CreateStore(arg, tempAlloca);
        return tempAlloca;
    }

    if (!info.coercedType || arg->getType() == info.coercedType) {
        return arg;
    }

    // Bitcast to coerced type
    return funcBuilder.CreateBitCast(arg, info.coercedType);
}

// Coerce return value back to original struct
llvm::Value* IRGenerator::coerceReturn(llvm::Value* retVal, const CoercionInfo& info) {
    if (info.isMemory || !info.coercedType) {
        return retVal;
    }

    if (retVal->getType() == info.coercedType) {
        return retVal;
    }

    return funcBuilder.CreateBitCast(retVal, info.coercedType);
}
