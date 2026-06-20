use colored::*;

use crate::diagnostics::error::{CompilerError, Severity};

pub struct Diagnostics {
    pub errors: Vec<CompilerError>,
    pub warnings: Vec<CompilerError>,
    filename: String,
}

impl Diagnostics {
    pub fn new(filename: String) -> Self {
        Diagnostics {
            errors: Vec::new(),
            warnings: Vec::new(),
            filename,
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

    pub fn print(&self, source: &String) {
        // Print ICEs first
        for error in self.errors.iter().filter(|e| e.severity == Severity::Ice) {
            self.print_error(error, source);
        }

        // Print Fatals
        for error in self.errors.iter().filter(|e| e.severity == Severity::Fatal) {
            self.print_error(error, source);
        }

        // Print Errors
        for error in self.errors.iter().filter(|e| e.severity == Severity::Error) {
            self.print_error(error, source);
        }

        // Print Warnings
        for warning in &self.warnings {
            self.print_error(warning, source);
        }
    }

    fn print_error(&self, error: &CompilerError, src: &String) {
        let (severity_label, color) = match error.severity {
            Severity::Error => ("error", Color::Red),
            Severity::Warning => ("warning", Color::Yellow),
            Severity::Fatal => ("fatal error", Color::Red),
            Severity::Ice => ("internal compiler error", Color::BrightRed),
        };

        if let Some(span) = &error.span {
            println!(
                "{}{}{}{}{}{}{} {}",
                self.filename.color(Color::Cyan),
                ":".color(Color::White),
                span.line.to_string().color(Color::Yellow),
                ":".color(Color::White),
                span.col.to_string().color(Color::Yellow),
                ":".color(Color::White),
                severity_label.color(color),
                error.message.color(Color::White)
            );

            // Print source line with underline
            let lines: Vec<&str> = src.lines().collect();
            if span.line <= lines.len() {
                let line = lines[span.line - 1];
                println!("  {} {}", "|".color(Color::Cyan), line);

                let mut underline = String::new();
                underline.push_str("  | ");
                for _ in 0..span.col - 1 {
                    underline.push(' ');
                }
                for _ in 0..span.length {
                    underline.push('^');
                }
                println!("{}", underline.color(Color::Red));
            }
        } else {
            println!(
                "{}: {}",
                severity_label.color(color),
                error.message.color(Color::White)
            );
        }
    }
}
