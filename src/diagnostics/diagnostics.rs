use std::{cell::RefCell, rc::Rc};

use colored::*;

use crate::diagnostics::{
    Phase,
    error::{CompilerError, Severity},
    source::SourceMap,
};

pub struct Diagnostics {
    pub source_map: SourceMap,
    pub filename: String,
    pub errors: Vec<CompilerError>,
    pub warnings: Vec<CompilerError>,
}

pub type SharedDiagnostics = Rc<RefCell<Diagnostics>>;

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

    fn print_ice_header(&self, count: usize) {
        eprintln!(
            "\n{}",
            "=================================================================="
                .bold()
                .red()
        );
        eprintln!("{}", "  INTERNAL COMPILER ERROR ".bold().bright_red());
        eprintln!(
            "{}",
            "=================================================================="
                .bold()
                .red()
        );
        eprintln!("The compiler encountered an internal state bug and cannot continue.");
        eprintln!(
            "This is {} in the compiler itself, NOT an error in your code.",
            "a bug".bold().underline()
        );
        eprintln!(
            "Total internal errors hit: {}\n",
            count.to_string().yellow()
        );
    }

    fn print_ice_footer(&self) {
        eprintln!(
            "\n{}",
            "------------------------------------------------------------------".dimmed()
        );
        eprintln!("{}", "Please consider filing a bug report with:".bold());
        eprintln!("  1. Your source code file (`{}`)", self.filename.cyan());
        eprintln!("  2. The exact error trace shown above");
        eprintln!(
            "  3. Compiler version: {}",
            env!("CARGO_PKG_VERSION").yellow()
        );
        eprintln!("4. To: https://github.com/BananaChristian/Unnameable/issues");
        eprintln!(
            "{}",
            "==================================================================\n"
                .bold()
                .red()
        );
    }

    pub fn print(&self) {
        let ices: Vec<&CompilerError> = self
            .errors
            .iter()
            .filter(|e| e.severity == Severity::Ice)
            .collect();

        if !ices.is_empty() {
            self.print_ice_header(ices.len());
            for error in ices {
                self.print_error(error);
            }
            self.print_ice_footer();
            return; // If the compiler crashed internally, don't flood the terminal with downstream errors!
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
        // Dynamically include phase if set
        let phase_suffix = match error.phase {
            Phase::None => String::new(),
            ref phase => format!(" [{:?}]", phase),
        };

        let (severity_label, color) = match error.severity {
            Severity::Error => (format!("error{}", phase_suffix), Color::Red),
            Severity::Warning => (format!("warning{}", phase_suffix), Color::Yellow),
            Severity::Fatal => (format!("fatal error{}", phase_suffix), Color::Red),
            Severity::Ice => (
                format!("internal compiler error{}", phase_suffix),
                Color::BrightRed,
            ),
        };

        if let Some(span) = &error.span {
            let (line, col) = self.source_map.get_line_col(span.start);
            let length = span.length().max(1);

            // Header: filename:line:col: severity: message
            println!(
                "{}:{}:{}: {}: {}",
                self.filename.color(Color::Cyan),
                line.to_string().color(Color::Yellow),
                col.to_string().color(Color::Yellow),
                severity_label.color(color).bold(),
                error.message.color(Color::White).bold()
            );

            let line_snippet = self.source_map.get_line_snippet(span.start);
            let line_num_str = line.to_string();
            let padding = " ".repeat(line_num_str.len());

            // Line snippet rendering
            println!(" {} {}", "|".color(Color::Cyan), line_snippet);

            // Caret underline
            let mut underline = String::new();
            underline.push_str(&format!(" {} | ", padding));
            for _ in 0..col.saturating_sub(1) {
                underline.push(' ');
            }
            for _ in 0..length {
                underline.push('^');
            }

            println!("{}", underline.color(color).bold());
        } else {
            println!(
                "{}: {}",
                severity_label.color(color).bold(),
                error.message.color(Color::White)
            );
        }
    }
}
