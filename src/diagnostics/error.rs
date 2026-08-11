use crate::diagnostics::span::Span;

#[derive(Debug, Clone)]
pub enum Phase {
    Lexer,
    Parser,
    Lowering,
    Semantics,
    ContractVerifier,
    MIRBuilder,
    BytecodeBuilder,
    Codegen,
    None,
}

#[derive(Debug, Clone)]
pub struct CompilerError {
    pub message: String,
    pub span: Option<Span>,
    pub severity: Severity,
    pub phase: Phase,
    pub hint: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Fatal, // Cannot recover
    Ice,   // Internal compiler error
}

impl CompilerError {
    pub fn error(message: String,phase: Phase, span: Option<Span>) -> Self {
        CompilerError {
            message,
            span,
            severity: Severity::Error,
            phase,
            hint: None,
        }
    }

    pub fn warning(message: String, span: Option<Span>) -> Self {
        CompilerError {
            message,
            span,
            severity: Severity::Warning,
            phase: Phase::None,
            hint: None,
        }
    }

    pub fn fatal(message: String, span:Option<Span>) -> Self {
        CompilerError {
            message,
            span,
            severity: Severity::Fatal,
            phase: Phase::None,
            hint: None,
        }
    }

    pub fn ice(message: String,phase: Phase, span: Option<Span>) -> Self {
        CompilerError {
            message,
            span,
            severity: Severity::Ice,
            phase,
            hint: None,
        }
    }
}
