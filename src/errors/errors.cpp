#include "errors.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "ast.hpp"

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_GREEN "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE "\033[34m"
#define COLOR_CYAN "\033[36m"
#define COLOR_BOLD "\033[1m"

ErrorHandler::ErrorHandler(const std::string &file) : fileName(file) {
  loadSourceLines();
}

void ErrorHandler::report(CompilerError &error) {
  if (!hintBuffer.empty()) {
    for (auto &h : hintBuffer) {
      error.message.hints.push_back(std::move(h));
    }
    hintBuffer.clear();
  }

  std::string displayName = errorCodeName(error.code, error.level);
  std::string baseName = getBaseName(fileName);

  // Header
  std::cerr << COLOR_BOLD << COLOR_RED << "[" << displayName << "] "
            << COLOR_RESET << baseName << ":" << error.line << ":"
            << error.column << " " << error.message.message << "\n";

  // Print 2 lines context
  int startLine = std::max(1, static_cast<int>(error.line) - 2);
  int endLine = std::min(static_cast<int>(sourceLines.size()),
                         static_cast<int>(error.line) + 2);

  for (int i = startLine; i <= endLine; i++) {
    // Line number and source
    std::cerr << " " << i << " | " << sourceLines[i - 1] << "\n";

    // Caret line for error line
    if (static_cast<size_t>(i) == error.line) {
      std::cerr << "   | ";

      // Handle tabs for positioning
      [[maybe_unused]] int visualColumn = 0;
      std::string line = sourceLines[i - 1];

      // First, print spaces/tabs to reach the error column visually
      for (size_t j = 0; j < error.column - 1 && j < line.length(); j++) {
        if (line[j] == '\t') {
          std::cerr << "\t";
          visualColumn += 4; // Tabs count as 4 visually
        } else {
          std::cerr << " ";
          visualColumn++;
        }
      }

      // Draw the underline using tokenLength
      std::cerr << COLOR_RED;
      for (size_t k = 0; k < error.length; k++) {
        std::cerr << "^";
      }
      std::cerr << COLOR_RESET << "\n";
    }
  }

  // Hints
  if (!error.message.hints.empty()) {
    for (const auto &hint : error.message.hints) {
      std::cerr << COLOR_YELLOW << "suggestion: " << COLOR_RESET << hint
                << "\n";
    }
  }
}

int ErrorHandler::errorCodeToInt(ErrorCode code) {
  return static_cast<int>(code);
}

std::string ErrorHandler::format_string(std::string &message,
                                        std::vector<std::string> &args) {
  for (size_t i = 0; i < args.size(); i++) {
    std::string slot = "{" + std::to_string(i) + "}";
    size_t pos = message.find(slot);
    if (pos != std::string::npos) {
      message.replace(pos, slot.length(), args[i]);
    }
  }
  return message;
}

std::vector<std::string> ErrorHandler::suggestForError(ErrorCode code) {
  std::vector<std::string> suggestions;
  switch (code) {
  case ErrorCode::UnexpectedChar: {
    suggestions.push_back(
        "Valid characters include letters, numbers, and standard operators");
    suggestions.push_back("Check if you meant to use a different symbol");
    return suggestions;
  }
  case ErrorCode::UnterminatedString: {
    suggestions.push_back(
        "Add a closing double quote (\") at the end of the string");
    suggestions.push_back("For multi-line strings, use \"\"\" instead");
    return suggestions;
  }
  case ErrorCode::InvalidEscape: {
    suggestions.push_back("Valid escape sequences: \\n (newline), \\t (tab), "
                          "\\r (carriage return)");
    suggestions.push_back("Also valid: \\\" (double quote), \\\\ (backslash)");
    suggestions.push_back("To include a literal backslash, use \\\\");
    return suggestions;
  }
  case ErrorCode::UnterminatedChar: {
    suggestions.push_back(
        "Add a closing single quote (') at the end of the character");
    suggestions.push_back(
        "Character literals must contain exactly one character");
    return suggestions;
  }
  case ErrorCode::InvalidToken: {
    suggestions.push_back(
        "Check for special characters that might not be allowed");
    suggestions.push_back("Tokens must start with a letter or underscore");
    return suggestions;
  }
  case ErrorCode::UnterminatedComment: {
    suggestions.push_back("Close the comment with '##'");
    suggestions.push_back("Comments cannot be nested");
    return suggestions;
  }
  case ErrorCode::UnexpectedToken: {
    suggestions.push_back("Check the expected syntax and fix the token");
    suggestions.push_back("You might have forgotten a semicolon or operator");
    return suggestions;
  }
  case ErrorCode::MissingClosingBracket: {
    suggestions.push_back("Add a closing '}' to match the opening '{'");
    suggestions.push_back("Check for unmatched braces in your code");
    return suggestions;
  }
  case ErrorCode::InvalidReturnType: {
    suggestions.push_back(
        "Valid return types: void, i32, i64, f32, f64, bool, ptr, ref");
    suggestions.push_back("Or use a custom type defined with 'record'");
    return suggestions;
  }
  case ErrorCode::InvalidType: {
    suggestions.push_back(
        "Built-in types: i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool");
    suggestions.push_back("Pointer: ptr<type> or type*");
    suggestions.push_back("Reference: ref<type> or type&");
    suggestions.push_back("Nullable: add '?' after the type");
    return suggestions;
  }
  case ErrorCode::InvalidModifier: {
    suggestions.push_back("Modifiers like 'mut', 'const', 'heap' only apply to "
                          "variable declarations");
    suggestions.push_back(
        "Cannot apply modifiers to expressions or statements");
    return suggestions;
  }
  case ErrorCode::MultipleMutSpecifiers: {
    suggestions.push_back(
        "'mut' and 'const' cannot be used together as they are contradictory");
    suggestions.push_back(
        "Choose one: 'mut' for mutable, 'const' for immutable");
    return suggestions;
  }
  case ErrorCode::InvalidConstraint: {
    suggestions.push_back(
        "Use 'in' for input parameters that won't be modified");
    suggestions.push_back(
        "Use 'out' for output parameters that will be written to");
    suggestions.push_back("Example: func process(in x: i32, out y: i32)");
    return suggestions;
  }
  case ErrorCode::ExpectIntegerToken: {
    suggestions.push_back("Expected an integer literal like 0, 42, 100, or -5");
    suggestions.push_back("Hex: 0xFF, Binary: 0b1010, Octal: 0o755");
    return suggestions;
  }
  case ErrorCode::UnsupportedStatement: {
    suggestions.push_back(
        "This statement is not supported in the current context");
    suggestions.push_back("Check if you meant to use a different construct");
    return suggestions;
  }
  case ErrorCode::DuplicateInit: {
    suggestions.push_back("Remove one of the duplicate initializers");
    suggestions.push_back("You can only initialize a variable once");
    return suggestions;
  }
  case ErrorCode::UndefinedVariable: {
    suggestions.push_back(
        "Declare the variable before using it with 'let' or 'var'");
    suggestions.push_back("Check for typos in the variable name");
    suggestions.push_back("The variable might be out of scope");
    return suggestions;
  }
  case ErrorCode::TypeMismatch: {
    suggestions.push_back("Use 'as' to cast between compatible types");
    suggestions.push_back("Example: let x: i32 = y as i32");
    suggestions.push_back(
        "Check if both sides of the operation have the same type");
    return suggestions;
  }
  case ErrorCode::TypeMismatchArrayLit: {
    suggestions.push_back(
        "All elements in an array literal must be the same type");
    suggestions.push_back("Check element {0} for type mismatch");
    suggestions.push_back("Consider using a tuple or struct if types differ");
    return suggestions;
  }
  case ErrorCode::NoneIndexableType: {
    suggestions.push_back(
        "Only array and pointer types can be subscripted with []");
    suggestions.push_back(
        "Make sure the type is an array like [T; N] or ptr<T>");
    suggestions.push_back(
        "If using a custom type, implement an indexer method");
    return suggestions;
  }
  case ErrorCode::NoneDereferencableType: {
    suggestions.push_back(
        "Only pointer types can be dereferenced with the * operator");
    suggestions.push_back("Use 'ptr<T>' or 'T*' for pointer types");
    suggestions.push_back("References (&T) are automatically dereferenced");
    return suggestions;
  }
  case ErrorCode::InvalidSelfAccess: {
    suggestions.push_back("'self' is only available inside method definitions");
    suggestions.push_back("Use 'self' to refer to the current instance");
    suggestions.push_back("Static methods don't have access to 'self'");
    return suggestions;
  }
  case ErrorCode::NotaFuncOrFnPtr: {
    suggestions.push_back("'{0}' is not a function or a function pointer");
    suggestions.push_back("Check if you meant to call a method instead");
    suggestions.push_back("Function pointers: fn(type) : return_type");
    return suggestions;
  }
  case ErrorCode::LHSMustBeNull: {
    suggestions.push_back(
        "Left hand side of the coalesce operator '??' must be nullable");
    suggestions.push_back("Add '?' to the type to make it nullable");
    suggestions.push_back("Example: let x: i32? = value ?? 0");
    return suggestions;
  }
  case ErrorCode::InvalidUsageOfNull: {
    suggestions.push_back(
        "Nullable variables cannot be directly used in operations");
    suggestions.push_back("Use '!' to unwrap: nullable! (panics if null)");
    suggestions.push_back("Use '??' to coalesce: nullable ?? default_value");
    suggestions.push_back("Check with 'if nullable != null' before using");
    return suggestions;
  }
  case ErrorCode::NonExistantMember: {
    suggestions.push_back("'{0}' is not a member of type '{1}'");
    suggestions.push_back("Check the spelling of the member name");
    suggestions.push_back(
        "Review the type definition to see available members");
    suggestions.push_back("Maybe you meant a different type?");
    return suggestions;
  }
  case ErrorCode::InvalidOperationOnTypes: {
    suggestions.push_back(
        "Cannot carry out '{0}' operation on types '{1}' and '{2}'");
    suggestions.push_back("Both operands need to support this operation");
    suggestions.push_back("Consider casting one of the operands first");
    return suggestions;
  }
  case ErrorCode::InvalidPrefixOrPostfixOps: {
    suggestions.push_back("Cannot carry out '{0}' on type '{1}'");
    suggestions.push_back(
        "Prefix/postfix operators work on numeric types (i32, f64, etc.)");
    suggestions.push_back(
        "For custom types, implement the operator in a method");
    return suggestions;
  }
  case ErrorCode::ArgumentSizeMismatch: {
    suggestions.push_back(
        "Function '{0}' expected '{1}' arguments but got '{2}'");
    suggestions.push_back(
        "Check the function definition for correct parameter count");
    suggestions.push_back("You might be missing some arguments or have extras");
    return suggestions;
  }
  case ErrorCode::ArgumentTypeMismatch: {
    suggestions.push_back(
        "Type mismatch in argument '{0}': expected '{1}' but got '{2}'");
    suggestions.push_back(
        "Cast the argument to the correct type: arg as target_type");
    suggestions.push_back("Check if you passed parameters in the wrong order");
    return suggestions;
  }
  case ErrorCode::NullPassFailure: {
    suggestions.push_back(
        "Cannot pass null to argument '{0}' which expects type '{1}'");
    suggestions.push_back("Use '!' to unwrap: nullable! (panics if null)");
    suggestions.push_back(
        "Make the parameter nullable by adding '?' to its type");
    suggestions.push_back("Check with 'if nullable != null' before passing");
    return suggestions;
  }
  case ErrorCode::FailedToInfer: {
    suggestions.push_back(
        "Could be an unknown variable or lack of a value for auto");
    suggestions.push_back(
        "Provide an explicit type annotation: let x: i32 = ...");
    suggestions.push_back("Make sure the initializer has a clear type");
    return suggestions;
  }
  case ErrorCode::GlobalHeapVar: {
    suggestions.push_back("Heap variables cannot be declared in global scope");
    suggestions.push_back("Move the heap allocation inside a function body");
    suggestions.push_back("Use normal variables for global state");
    suggestions.push_back(
        "The compiler cannot determine the lifetime of global heap vars");
    return suggestions;
  }
  case ErrorCode::InvalidBindOperator: {
    suggestions.push_back("Use '->' for pointer or reference assignment");
    suggestions.push_back("Example: ptr i32 x -> addr y");
    suggestions.push_back("Use '::' for enum variant access");
    suggestions.push_back("Use '.' for struct/record field access");
    return suggestions;
  }
  case ErrorCode::InvalidImmutUse: {
    suggestions.push_back("Cannot modify immutable variable '{1}' with '{0}'");
    suggestions.push_back(
        "Add 'mut' to the variable declaration: let mut x = value");
    suggestions.push_back("Create a mutable copy: let mut y = x");
    return suggestions;
  }
  case ErrorCode::InvalidUninitUse: {
    suggestions.push_back("Cannot use uninitialized variable '{1}' with '{0}'");
    suggestions.push_back("Initialize the variable before using it");
    suggestions.push_back("Check all code paths to ensure initialization");
    return suggestions;
  }
  case ErrorCode::InvalidAddrOperand: {
    suggestions.push_back("Cannot get address of temporary value '{0}'");
    suggestions.push_back("Store the value in a variable first: let temp = "
                          "expr; let ptr = &temp");
    suggestions.push_back(
        "Function calls return temporary values with no permanent address");
    suggestions.push_back("Literals and expressions are temporary in nature");
    return suggestions;
  }
  case ErrorCode::NoneBitcastableType: {
    suggestions.push_back(
        "Bitcastable types: pointers, integers, floats, opaque");
    suggestions.push_back(
        "Cannot bitcast from or to nullable types (remove '?')");
    suggestions.push_back("References and arrays cannot be bitcast targets");
    suggestions.push_back("Custom types need 'ptr MyType' to be bitcastable");
    return suggestions;
  }
  case ErrorCode::NoneCastableType: {
    suggestions.push_back("Cannot cast from or to nullable types (remove '?')");
    suggestions.push_back("Use bitcast for pointer conversions");
    suggestions.push_back(
        "References are aliases, cast the target variable directly");
    suggestions.push_back(
        "Arrays cannot be cast, cast individual elements instead");
    suggestions.push_back("Custom types need conversion methods");
    suggestions.push_back("Valid cast targets: integers, floats, bool");
    return suggestions;
  }
  case ErrorCode::UnwrappableType: {
    suggestions.push_back("Cannot unwrap type '{0}' with '!' or '??'");
    suggestions.push_back("Only nullable types (with '?') can be unwrapped");
    suggestions.push_back("Add '?' to the type to make it nullable if needed");
    return suggestions;
  }
  case ErrorCode::InvalidInfix: {
    suggestions.push_back(
        "Right hand side of '.' must be an identifier or a call");
    suggestions.push_back("Use method name after '.'");
    suggestions.push_back("For complex expressions, use parentheses first");
    return suggestions;
  }
  case ErrorCode::FloatingReturns: {
    suggestions.push_back("Return statement cannot be outside a function body");
    suggestions.push_back("Move the return inside a function");
    suggestions.push_back(
        "At top level, use 'return' only in function definitions");
    return suggestions;
  }
  case ErrorCode::NonVoidReturn: {
    suggestions.push_back("Non-void function '{0}' has a return with no value");
    suggestions.push_back("Add a return value of type {0}");
    suggestions.push_back(
        "Or change the return type to 'void' if no value needed");
    return suggestions;
  }
  case ErrorCode::DuplicateName: {
    suggestions.push_back("Name '{0}' already exists in this scope");
    suggestions.push_back("Rename one of the declarations");
    suggestions.push_back("Use a different scope or namespace");
    return suggestions;
  }
  case ErrorCode::InvalidHeapParam: {
    suggestions.push_back("Cannot declare a parameter as heap variable");
    suggestions.push_back("Remove the 'heap' keyword from the parameter");
    suggestions.push_back(
        "Use 'heap' only inside function bodies for allocation");
    return suggestions;
  }
  case ErrorCode::InvalidPersistParam: {
    suggestions.push_back("Parameter '{0}' cannot be marked as 'persist'");
    suggestions.push_back(
        "'persist' only applies to heap variables inside function bodies");
    suggestions.push_back("Remove 'persist' from the parameter");
    return suggestions;
  }
  case ErrorCode::InvalidNullReferenceParam: {
    suggestions.push_back("Reference parameter '{0}' cannot be nullable");
    suggestions.push_back("Remove '?' from the reference type");
    suggestions.push_back("If null is needed, use a pointer: ptr i32?");
    suggestions.push_back("References always point to valid memory by design");
    return suggestions;
  }
  case ErrorCode::InvalidAutoUse: {
    suggestions.push_back("Must explicitly state the parameter's data type");
    suggestions.push_back("'auto' is not allowed for function parameters");
    suggestions.push_back("Specify a concrete type like i32, f64, etc.");
    return suggestions;
  }
  case ErrorCode::NoParamDefaultVal: {
    suggestions.push_back(
        "Function parameters cannot have default values in this language");
    suggestions.push_back("Remove the '=' and default value");
    suggestions.push_back("Use overloading or optional types instead");
    return suggestions;
  }
  case ErrorCode::DanglingReferenceReturn: {
    suggestions.push_back(
        "Returning reference to local variable '{0}' is unsafe");
    suggestions.push_back(
        "The local variable will be freed when the function returns");
    suggestions.push_back("Return the value directly: return x instead of &x");
    suggestions.push_back("Or heap allocate: return heap i32 {x}");
    return suggestions;
  }
  case ErrorCode::IllegalFunctionDeclaration: {
    suggestions.push_back(
        "Function declarations cannot exist inside seals and components");
    suggestions.push_back("Move the declaration to the top level");
    suggestions.push_back(
        "Try providing a full definition instead of just a declaration");
    return suggestions;
  }
  case ErrorCode::IllegalStmtInSeal: {
    suggestions.push_back(
        "Only function definitions are allowed inside a seal");
    suggestions.push_back(
        "Move non-function statements outside the seal block");
    suggestions.push_back("Wrap the statement in a function if needed");
    return suggestions;
  }
  case ErrorCode::InvalidNullReturn: {
    suggestions.push_back(
        "Cannot return null from a function with non-nullable return type");
    suggestions.push_back("Add '?' to the return type: func name() : Type?");
    suggestions.push_back("Return a valid value instead of null");
    return suggestions;
  }
  case ErrorCode::InvalidFinalExpression: {
    suggestions.push_back(
        "A void function cannot have a final expression as its return value");
    suggestions.push_back(
        "Remove the final expression or change return type to non-void");
    suggestions.push_back("Use 'return' for void functions");
    return suggestions;
  }
  case ErrorCode::IllegalFunctionDefinition: {
    suggestions.push_back(
        "Cannot nest function definitions inside other functions");
    suggestions.push_back("Move the inner function to the top level");
    return suggestions;
  }
  case ErrorCode::DefnDeclMismatch: {
    suggestions.push_back("The definition signature must match the earlier "
                          "declaration, check the parameters and return types");
    return suggestions;
  }
  case ErrorCode::AlreadyDefinedFunc: {
    suggestions.push_back(
        "Unnameable does not allow for overloading, please use unique names "
        "for your functions or isolate them via seals and components");
    return suggestions;
  }
  case ErrorCode::MatchExportsToTypes: {
    suggestions.push_back("Mark the type as exportable, exportable functions "
                          "may only use exportable types");
    return suggestions;
  }
  case ErrorCode::InvalidParam: {
    suggestions.push_back(
        "parameters must follow the unified declaration syntax");
    suggestions.push_back("Example: ptr i32 x, mut i64 y");
    return suggestions;
  }
  default: {
    return suggestions;
  }
  }
}

ErrorMessage ErrorHandler::generateErrorMessage(ErrorCode code) {
  ErrorMessage message;
  switch (code) {
  case ErrorCode::UnexpectedChar: {
    message.code = ErrorCode::UnexpectedChar;
    message.message = "unexpected character '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::UnterminatedString: {
    message.code = ErrorCode::UnterminatedString;
    message.message = "unterminated string '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidEscape: {
    message.code = ErrorCode::InvalidEscape;
    message.message = "invalid escape '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::UnterminatedChar: {
    message.code = ErrorCode::UnterminatedString;
    message.message = "unterminated char '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidToken: {
    message.code = ErrorCode::InvalidToken;
    message.message = "invalid token '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::UnterminatedComment: {
    message.code = ErrorCode::UnterminatedComment;
    message.message = "unterminated comment";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::UnexpectedToken: {
    message.code = ErrorCode::UnexpectedToken;
    message.message = "expected token '{0}' but got '{1}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::MissingClosingBracket: {
    message.code = ErrorCode::MissingClosingBracket;
    message.message = "missing closing bracket expected '}' but got '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidReturnType: {
    message.code = ErrorCode::InvalidReturnType;
    message.message = "expected a return type but got '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidType: {
    message.code = ErrorCode::InvalidType;
    message.message = "expected a type but got '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidModifier: {
    message.code = ErrorCode::InvalidModifier;
    message.message = "cannot apply modifier to this statement";
    message.hints = suggestForError(code);

    return message;
  }
  case ErrorCode::MultipleMutSpecifiers: {
    message.code = ErrorCode::MultipleMutSpecifiers;
    message.message = "cannot apply multiple mutability specifiers";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidConstraint: {
    message.code = ErrorCode::InvalidConstraint;
    message.message = "expected 'in' or 'out' after ':' but got '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::ExpectIntegerToken: {
    message.code = ErrorCode::ExpectIntegerToken;
    message.message = "expected an intenger token but got '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::UnsupportedStatement: {
    message.code = ErrorCode::UnsupportedStatement;
    message.message = "unexpected statement";
    return message;
  }
  case ErrorCode::DuplicateInit: {
    message.code = ErrorCode::DuplicateInit;
    message.message = "duplicate init";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::UndefinedVariable: {
    message.code = ErrorCode::UndefinedVariable;
    message.message = "Undefined variable '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::TypeMismatch: {
    message.code = ErrorCode::TypeMismatch;
    message.message = "type mismatch expected '{0}' but got '{1}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::TypeMismatchArrayLit: {
    message.code = ErrorCode::TypeMismatchArrayLit;
    message.message = "type mismatch for array literal at element '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::NoneIndexableType: {
    message.code = ErrorCode::NoneIndexableType;
    message.message = "type '{0}' is non indexable";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::NoneDereferencableType: {
    message.code = ErrorCode::NoneDereferencableType;
    message.message = "cannot dereference type '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidSelfAccess: {
    message.code = InvalidSelfAccess;
    message.message = "Invalid self access";
    return message;
  }
  case ErrorCode::NotaFuncOrFnPtr: {
    message.code = ErrorCode::NotaFuncOrFnPtr;
    message.message = "'{0}' is not a function or a function pointer";
    return message;
  }
  case ErrorCode::LHSMustBeNull: {
    message.code = ErrorCode::LHSMustBeNull;
    message.message =
        "left hand side of the caolesce operator must be nullable";
    return message;
  }
  case ErrorCode::InvalidUsageOfNull: {
    message.code = ErrorCode::InvalidUsageOfNull;
    message.message = "Invalid usage of a null variable '{name}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::NonExistantMember: {
    message.code = ErrorCode::NonExistantMember;
    message.message = "'{0}' is not a member of type '{1}'";
    return message;
  }
  case ErrorCode::InvalidOperationOnTypes: {
    message.code = ErrorCode::InvalidOperationOnTypes;
    message.message =
        "Cannot carry out '{0}' operation on types '{1}' and '{2}'";
    return message;
  }
  case ErrorCode::InvalidPrefixOrPostfixOps: {
    message.code = ErrorCode::InvalidPrefixOrPostfixOps;
    message.message = "Cannot carry out '{0}' on type '{1}'";
    return message;
  }
  case ErrorCode::ArgumentSizeMismatch: {
    message.code = ErrorCode::ArgumentSizeMismatch;
    message.message = "function '{0}' expected '{1}' arguments but got '{2}'";
    return message;
  }
  case ErrorCode::ArgumentTypeMismatch: {
    message.code = ErrorCode::ArgumentTypeMismatch;
    message.message =
        "type mismatch in argument '{0}' expected '{1}' but got '{2}'";
    return message;
  }
  case ErrorCode::NullPassFailure: {
    message.code = ErrorCode::NullPassFailure;
    message.message =
        "cannot pass null to a non nullable argument '{0}' of type '{1}'";
    return message;
  }
  case ErrorCode::FailedToInfer: {
    message.code = ErrorCode::FailedToInfer;
    message.message = "failed to infer type";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::GlobalHeapVar: {
    message.code = ErrorCode::GlobalHeapVar;
    message.message =
        "cannot declare a heap variable '{0}' in an invalid scope";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidBindOperator: {
    message.code = ErrorCode::InvalidBindOperator;
    message.message = "invalid operator '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidImmutUse: {
    message.code = ErrorCode::InvalidImmutUse;
    message.message = "cannot apply '{0}' to an immutable variable '{1}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidUninitUse: {
    message.code = ErrorCode::InvalidUninitUse;
    message.message = "cannot apply '{0}' to unitialized variable '{1}'";
    return message;
  }
  case ErrorCode::InvalidAddrOperand: {
    message.code = ErrorCode::InvalidAddrOperand;
    message.message = "cannot get the address of a temporary value '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::NoneBitcastableType: {
    message.code = ErrorCode::NoneBitcastableType;
    message.message = "cannot bitcast from or to type '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::NoneCastableType: {
    message.code = ErrorCode::NoneCastableType;
    message.message = "cannot cast from or to type '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::UnwrappableType: {
    message.code = ErrorCode::UnwrappableType;
    message.message = "cannot unwrap type '{0}'";
    return message;
  }
  case ErrorCode::InvalidInfix: {
    message.code = ErrorCode::InvalidInfix;
    message.message =
        "right handside of the '.' must be an identifier or a call";
    return message;
  }
  case ErrorCode::FloatingReturns: {
    message.code = ErrorCode::FloatingReturns;
    message.message = "return statement cannot be outside a function body";
    return message;
  }
  case ErrorCode::NonVoidReturn: {
    message.code = ErrorCode::NonVoidReturn;
    message.message =
        "non-void function '{0}' has a return statement with no value";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::DuplicateName: {
    message.code = ErrorCode::DuplicateName;
    message.message = "name '{0}' already exists in scope";
    return message;
  }
  case ErrorCode::InvalidHeapParam: {
    message.code = ErrorCode::InvalidHeapParam;
    message.message = "cannot heap allocated";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidPersistParam: {
    message.code = ErrorCode::InvalidPersistParam;
    message.message = "parameter '{0}' marked as persist";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidNullReferenceParam: {
    message.code = ErrorCode::InvalidNullReferenceParam;
    message.message = "reference parameter '{0}' cannot be nullable";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidAutoUse: {
    message.code = ErrorCode::InvalidAutoUse;
    message.message = "must explicitly state the parameter's data type";
    return message;
  }
  case ErrorCode::NoParamDefaultVal: {
    message.code = ErrorCode::NoParamDefaultVal;
    message.message = "parameter's cannot have a default value";
    return message;
  }
  case ErrorCode::DanglingReferenceReturn: {
    message.code = ErrorCode::DanglingReferenceReturn;
    message.message = "cannot return a reference to a local variable '{0}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::IllegalFunctionDeclaration: {
    message.code = ErrorCode::IllegalFunctionDeclaration;
    message.message = "illegal function declaration";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::IllegalStmtInSeal: {
    message.code = ErrorCode::IllegalStmtInSeal;
    message.message = "only function definitions are allowed inside a seal";
    return message;
  }
  case ErrorCode::InvalidNullReturn: {
    message.code = ErrorCode::InvalidNullReturn;
    message.message =
        "cannot return null from a function whose return type is not nullable";
    return message;
  }
  case ErrorCode::InvalidFinalExpression: {
    message.code = ErrorCode::InvalidFinalExpression;
    message.message = "A void function cannot have a final expression";
    return message;
  }
  case ErrorCode::IllegalFunctionDefinition: {
    message.code = ErrorCode::IllegalFunctionDefinition;
    message.message = "cannot nest function definitions";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::DefnDeclMismatch: {
    message.code = ErrorCode::DefnDeclMismatch;
    message.message =
        "the definition of '{0}' does not match its prior declaration";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::AlreadyDefinedFunc: {
    message.code = ErrorCode::AlreadyDefinedFunc;
    message.message = "function '{0}' is already defined";
    return message;
  }
  case ErrorCode::MatchExportsToTypes: {
    message.code = ErrorCode::MatchExportsToTypes;
    message.message = "exportable function '{0}' uses non exportable type "
                      "'{1}'";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::InvalidParam: {
    message.code = ErrorCode::InvalidParam;
    message.message = "invalid parameter";
    message.hints = suggestForError(code);
    return message;
  }
  case ErrorCode::FloatingTrace: {
    message.code = ErrorCode::FloatingTrace;
    message.message = "trace statements are not allowed in global scope";
    return message;
  }case ErrorCode::NotDefinedOrDeclared:{
    message.code=ErrorCode::NotDefinedOrDeclared;
    message.message="'{0}' has not been defined or declared anywhere";
    return message;
  }
  default: {
    message.code = ErrorCode::GenericError;
    message.message = "an error occured";
    return message;
  }
  }
}

std::string ErrorHandler::errorCodeName(ErrorCode code, ErrorLevel level) {
  int codeInt = errorCodeToInt(code);
  switch (level) {
  case ErrorLevel::ERROR:
    return "E" + std::to_string(codeInt);
  case ErrorLevel::WARNING:
    return "W" + std::to_string(codeInt);
  case ErrorLevel::FATAL:
    return "F" + std::to_string(codeInt);
  }

  return "Unknown issue";
}

std::string ErrorHandler::getBaseName(const std::string &fullPath) {
  if (fullPath.empty()) {
    std::cerr << COLOR_RED << "[ERROR HANDLER WARNING]" << COLOR_RESET
              << " fullPath is empty!\n";
    return "unknown_file";
  }

  size_t pos = fullPath.find_last_of("/\\");
  if (pos == std::string::npos)
    return fullPath;

  return fullPath.substr(pos + 1);
}

void ErrorHandler::loadSourceLines() {
  std::ifstream in(fileName);
  if (!in.is_open()) {
    std::cerr << COLOR_RED << "[SOURCE LOAD ERROR]" << COLOR_RESET
              << "Cannot open file: " << fileName << "\n";
    return;
  }

  std::string line;
  while (std::getline(in, line)) {
    sourceLines.push_back(line);
  }

  if (sourceLines.empty())
    std::cerr << COLOR_YELLOW << "[SOURCE LOAD WARNING]" << COLOR_RESET
              << "File has no lines: " << fileName << "\n";
}

std::string ErrorHandler::getSourceLine(int line,
                                        std::vector<std::string> sourceLines) {
  if (line - 1 < 0 || line - 1 >= static_cast<int>(sourceLines.size())) {
    return " ";
  }

  return sourceLines[line - 1];
}

ErrorHandler &ErrorHandler::addHint(const std::string &hint) {
  hintBuffer.push_back(hint);
  return *this;
}

int ErrorHandler::getTokenLength(Node *contextNode) {
  if (auto exprStmt = dynamic_cast<ExpressionStatement *>(contextNode))
    return getTokenLength(exprStmt->expression.get());

  if (auto ident = dynamic_cast<Identifier *>(contextNode))
    return ident->identifier.TokenLiteral.length();

  if (auto i8Lit = dynamic_cast<I8Literal *>(contextNode))
    return i8Lit->i8_token.TokenLiteral.length();
  if (auto u8Lit = dynamic_cast<U8Literal *>(contextNode))
    return u8Lit->u8_token.TokenLiteral.length();
  if (auto i16Lit = dynamic_cast<I16Literal *>(contextNode))
    return i16Lit->i16_token.TokenLiteral.length();
  if (auto u16Lit = dynamic_cast<U16Literal *>(contextNode))
    return u16Lit->u16_token.TokenLiteral.length();
  if (auto i32Lit = dynamic_cast<I32Literal *>(contextNode))
    return i32Lit->i32_token.TokenLiteral.length();
  if (auto u32Lit = dynamic_cast<U32Literal *>(contextNode))
    return u32Lit->u32_token.TokenLiteral.length();
  if (auto i64Lit = dynamic_cast<I64Literal *>(contextNode))
    return i64Lit->i64_token.TokenLiteral.length();
  if (auto u64Lit = dynamic_cast<U64Literal *>(contextNode))
    return u64Lit->u64_token.TokenLiteral.length();
  if (auto i128Lit = dynamic_cast<I128Literal *>(contextNode))
    return i128Lit->i128_token.TokenLiteral.length();
  if (auto u128Lit = dynamic_cast<U128Literal *>(contextNode))
    return u128Lit->u128_token.TokenLiteral.length();
  if (auto iSize = dynamic_cast<ISIZELiteral *>(contextNode))
    return iSize->isize_token.TokenLiteral.length();
  if (auto uSizeLit = dynamic_cast<USIZELiteral *>(contextNode))
    return uSizeLit->usize_token.TokenLiteral.length();

  if (auto f32Lit = dynamic_cast<F32Literal *>(contextNode))
    return f32Lit->f32_token.TokenLiteral.length();
  if (auto f64Lit = dynamic_cast<F64Literal *>(contextNode))
    return f64Lit->f64_token.TokenLiteral.length();

  if (auto strLit = dynamic_cast<StringLiteral *>(contextNode))
    return strLit->string_token.TokenLiteral.length();

  if (auto char8Lit = dynamic_cast<Char8Literal *>(contextNode))
    return char8Lit->char8_token.TokenLiteral.length();
  if (auto char16Lit = dynamic_cast<Char16Literal *>(contextNode))
    return char16Lit->char16_token.TokenLiteral.length();
  if (auto char32Lit = dynamic_cast<Char32Literal *>(contextNode))
    return char32Lit->char32_token.TokenLiteral.length();

  if (auto boolLit = dynamic_cast<BooleanLiteral *>(contextNode))
    return boolLit->boolean_token.TokenLiteral.length();

  if (auto arrLit = dynamic_cast<ArrayLiteral *>(contextNode)) {
    auto dimensionCount = 2;
    if (arrLit->array.empty()) {
      for (const auto &expr : arrLit->array) {
        dimensionCount += getTokenLength(expr.get());
      }
    }

    return dimensionCount;
  }

  if (auto fnStmt = dynamic_cast<FunctionStatement *>(contextNode))
    return getTokenLength(fnStmt->funcExpr.get());

  if (auto fnExpr = dynamic_cast<FunctionExpression *>(contextNode))
    return fnExpr->func_key.TokenLiteral.length();

  if (auto fnDeclExpr =
          dynamic_cast<FunctionDeclarationExpression *>(contextNode))
    return getTokenLength(fnDeclExpr->funcDeclrStmt.get());

  if (auto fnDecl = dynamic_cast<FunctionDeclaration *>(contextNode))
    return getTokenLength(fnDecl->function_name.get());

  if (auto basicType = dynamic_cast<BasicType *>(contextNode))
    return basicType->data_token.TokenLiteral.length();

  if (auto fnCall = dynamic_cast<CallExpression *>(contextNode))
    return getTokenLength(fnCall->function_identifier.get());

  if (auto infix = dynamic_cast<InfixExpression *>(contextNode)) {
    auto rightLen = getTokenLength(infix->right_operand.get());
    auto leftLen = getTokenLength(infix->left_operand.get());
    auto operatorLen = infix->operat.TokenLiteral.length();
    return rightLen + operatorLen + leftLen;
  }

  if (auto sizeExpr = dynamic_cast<SizeOfExpression *>(contextNode)) {
    auto sizeKeyLen = sizeExpr->sizeOf.TokenLiteral.length();
    auto typeLen = getTokenLength(sizeExpr->type.get());
    return sizeKeyLen + typeLen + 2;
  }

  if (auto castExpr = dynamic_cast<CastExpression *>(contextNode)) {
    auto castKeyLen = castExpr->cast.TokenLiteral.length();
    auto typeLen = getTokenLength(castExpr->type.get());
    auto exprLen = getTokenLength(castExpr->expr.get());
    return castKeyLen + typeLen + exprLen + 4;
  }

  if (auto bitcastExpr = dynamic_cast<BitcastExpression *>(contextNode)) {
    auto bitcastKeyLen = bitcastExpr->bitcast.TokenLiteral.length();
    auto typeLen = getTokenLength(bitcastExpr->type.get());
    auto exprLen = getTokenLength(bitcastExpr->expr.get());
    return bitcastKeyLen + typeLen + exprLen + 4;
  }

  // Default to 1 if u dont know
  return 1;
}
