use crate::diagnostics::span::Span;

#[derive(Debug, Clone)]
pub enum Phase {
    Lexer,
    Parser,
    Lowering,
    Semantics,
    ContractVerifier,
    None,
}

#[derive(Debug, Clone)]
pub struct CompilerError {
    pub message: String,
    pub span: Option<Span>,
    pub severity: Severity,
    pub phase: Phase,
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
        }
    }

    pub fn warning(message: String, span: Option<Span>) -> Self {
        CompilerError {
            message,
            span,
            severity: Severity::Warning,
            phase: Phase::None,
        }
    }

    pub fn fatal(message: String, span:Option<Span>) -> Self {
        CompilerError {
            message,
            span,
            severity: Severity::Fatal,
            phase: Phase::None,
        }
    }

    pub fn ice(message: String, span: Option<Span>) -> Self {
        CompilerError {
            message,
            span,
            severity: Severity::Ice,
            phase: Phase::None,
        }
    }
}
