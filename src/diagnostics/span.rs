use crate::lexer::token::Token;

#[derive(Debug, Clone,Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
    pub length: usize,
}

impl Span {
    pub fn from_token(token: &Token) -> Self {
        Span {
            line: token.line,
            col: token.col,
            length: token.lexeme.len(),
        }
    }

    pub fn simple(line: usize, col: usize, length: usize) -> Self {
        Span { line, col, length }
    }

    // Merge two spans
    pub fn merge(start: &Span, end: &Span) -> Self {
        if start.line == end.line {
            Span {
                line: start.line,
                col: start.col,
                length: (end.col + end.length) - start.col,
            }
        } else {
            // Multi-line: from start to end
            Span {
                line: start.line,
                col: start.col,
                length: (end.col + end.length) - start.col,
            }
        }
    }
}
