use crate::diagnostics::Span;

pub struct SourceMap {
    pub source: String,
    pub line_starts: Vec<usize>, // Byte offset of each line start
}

impl SourceMap {
    pub fn new(source: String) -> Self {
        let line_starts = Self::compute_line_starts(&source);
        Self {
            source,
            line_starts,
        }
    }

    fn compute_line_starts(source: &str) -> Vec<usize> {
        let mut starts = vec![0];
        for (byte_pos, ch) in source.char_indices() {
            // Returns (byte_offset, char)
            if ch == '\n' {
                starts.push(byte_pos + 1);
            }
        }
        starts
    }

    pub fn get_line_col(&self, byte_pos: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&byte_pos) {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        };
        let line_start = self.line_starts[line];
        // Column is a positional (Unicode scalar) count, not byte-based, so
        // caret underlines stay aligned on lines with multi-byte characters.
        // Counting through `char_indices` never slices a mid-char byte offset.
        let col = self.source[line_start..]
            .char_indices()
            .take_while(|&(off, _)| line_start + off < byte_pos)
            .count()
            + 1;
        (line + 1, col)
    }

    pub fn get_snippet(&self, span: &Span) -> String {
        match self.snapped_range(span.start, span.end) {
            Some((start, end)) => self.source[start..end].to_string(),
            None => String::new(),
        }
    }

    /// Number of Unicode scalar values in the byte range [start, end). The
    /// range is sanitized the same way `get_snippet` sanitizes spans, so it
    /// can never panic, mid-char byte offsets included.
    pub fn char_count(&self, start: usize, end: usize) -> usize {
        match self.snapped_range(start, end) {
            Some((start, end)) => self.source[start..end].chars().count(),
            None => 0,
        }
    }

    /// Clamps a byte range into [0, source.len()], swaps inverted bounds, and
    /// snaps both ends to valid UTF-8 char boundaries so no slicing below
    /// ever panics. Returns `None` when the range is empty after snapping.
    fn snapped_range(&self, mut start: usize, mut end: usize) -> Option<(usize, usize)> {
        let len = self.source.len();
        start = start.min(len);
        end = end.min(len);
        if start > end {
            std::mem::swap(&mut start, &mut end);
        }
        while start < len && !self.source.is_char_boundary(start) {
            start += 1;
        }
        while end > 0 && !self.source.is_char_boundary(end) {
            end -= 1;
        }
        if start >= end {
            None
        } else {
            Some((start, end))
        }
    }

    pub fn get_line_snippet(&self, byte_pos: usize) -> String {
        let (line_idx, _) = self.get_line_col(byte_pos);
        let line_start = self.line_starts[line_idx - 1];
        let mut line_end = if line_idx < self.line_starts.len() {
            self.line_starts[line_idx] - 1 // Exclude newline
        } else {
            self.source.len()
        };
        // A `\r` immediately before the line break (CRLF / CR at EOF) is line
        // terminators, not line content; drop it from the snippet.
        if line_end > line_start && self.source.as_bytes()[line_end - 1] == b'\r' {
            line_end -= 1;
        }
        self.source[line_start..line_end].to_string()
    }
}
