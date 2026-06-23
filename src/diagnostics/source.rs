use crate::diagnostics::Span;

pub struct SourceMap{
    pub source: String,
    pub line_starts: Vec<usize>,  // Byte offset of each line start
}

impl SourceMap{
    pub fn new(source: String) -> Self {
        let line_starts = Self::compute_line_starts(&source);
        Self { source, line_starts }
    }
    
    fn compute_line_starts(source: &str) -> Vec<usize> {
        let mut starts = vec![0];
        for (i, ch) in source.chars().enumerate() {
            if ch == '\n' {
                starts.push(i + 1);  // Next line starts after newline
            }
        }
        starts
    }
    
    pub fn get_line_col(&self, byte_pos: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&byte_pos) {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        };
        let col = byte_pos - self.line_starts[line];
        (line + 1, col + 1)
    }
    
    pub fn get_snippet(&self, span: &Span) -> String {
        let start = span.start.min(self.source.len());
        let end = span.end.min(self.source.len());
        self.source[start..end].to_string()
    }
    
    pub fn get_line_snippet(&self, byte_pos: usize) -> String {
        let (line_idx, _) = self.get_line_col(byte_pos);
        let line_start = self.line_starts[line_idx - 1];
        let line_end = if line_idx < self.line_starts.len() {
            self.line_starts[line_idx] - 1  // Exclude newline
        } else {
            self.source.len()
        };
        self.source[line_start..line_end].to_string()
    }
}
