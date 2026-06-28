use colored::*;

use crate::diagnostics::{
    error::{CompilerError, Severity},
    source::SourceMap,
};

pub struct Diagnostics {
    pub source_map: SourceMap,
    pub filename: String,
    pub errors: Vec<CompilerError>,
    pub warnings: Vec<CompilerError>,
}

impl Diagnostics {
    pub fn new(filename: String, source: String) -> Self {
        Diagnostics {
            source_map: SourceMap::new(source),
            filename,
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn report(&mut self, error: CompilerError) {
        match error.severity {
            Severity::Error | Severity::Fatal | Severity::Ice => {
                self.errors.push(error);
            }
            Severity::Warning => {
                self.warnings.push(error);
            }
        }
    }

    pub fn print(&self) {
        // Print ICEs first
        for error in self.errors.iter().filter(|e| e.severity == Severity::Ice) {
            self.print_error(error);
        }

        // Print Fatals
        for error in self.errors.iter().filter(|e| e.severity == Severity::Fatal) {
            self.print_error(error);
        }

        // Print Errors
        for error in self.errors.iter().filter(|e| e.severity == Severity::Error) {
            self.print_error(error);
        }

        // Print Warnings
        for warning in &self.warnings {
            self.print_error(warning);
        }
    }

    fn print_error(&self, error: &CompilerError) {
        let (severity_label, color) = match error.severity {
            Severity::Error => ("error", Color::Red),
            Severity::Warning => ("warning", Color::Yellow),
            Severity::Fatal => ("fatal error", Color::Red),
            Severity::Ice => ("internal compiler error", Color::BrightRed),
        };

        if let Some(span) = &error.span {
            let (line, col) = self.source_map.get_line_col(span.start);
            let length = span.end - span.start;

            // Store values in variables first to avoid borrow issues
            let filename_display = self.filename.color(Color::Cyan);
            let line_display = line.to_string().color(Color::Yellow);
            let col_display = col.to_string().color(Color::Yellow);
            let severity_display = severity_label.color(color);
            let message_display = error.message.color(Color::White);

            println!(
                "{}{}{}{}{}{}{} {}",
                filename_display,
                ":".color(Color::White),
                line_display,
                ":".color(Color::White),
                col_display,
                ":".color(Color::White),
                severity_display,
                message_display
            );

            // Get source lines once
            let source_lines: Vec<&str> = self.source_map.source.lines().collect();
            if let Some(line_content) = source_lines.get(line - 1) {
                let pipe = "|".color(Color::Cyan);
                println!("  {} {}", pipe, line_content);

                let mut underline = String::with_capacity(col + length);
                underline.push_str("  | ");
                for _ in 0..col - 1 {
                    underline.push(' ');
                }
                for _ in 0..length {
                    underline.push('^');
                }
                println!("{}", underline.color(Color::Red));
            }
        } else {
            let severity_display = severity_label.color(color);
            let message_display = error.message.color(Color::White);
            println!("{}: {}", severity_display, message_display);
        }
    }
}
