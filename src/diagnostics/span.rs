use crate::lexer::token::Token;

#[derive(Debug, Clone, Copy, PartialEq)]
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
            let end_col = end.col + end.length;
            if end_col >= start.col {
                Span {
                    line: start.line,
                    col: start.col,
                    length: end_col - start.col,
                }
            } else {
                start.clone()
            }
        } else {
            let end_col = end.col + end.length;
            if end_col >= start.col {
                Span {
                    line: start.line,
                    col: start.col,
                    length: end_col - start.col,
                }
            } else {
                start.clone()
            }
        }
    }

    pub fn fresh() -> Self {
        Span {
            line: 1,
            col: 0,
            length: 0,
        }
    }
}
