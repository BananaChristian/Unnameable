use crate::diagnostics::source::SourceMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize, // Byte offset
    pub end: usize,   // Byte offset
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn line_col(&self, source_map: &SourceMap) -> (usize, usize) {
        source_map.get_line_col(self.start)
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }
}

