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
            Severity::Ice => {
                self.report_ice_and_panic(error);
            }
            Severity::Error | Severity::Fatal => {
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

        let err_count = self
            .errors
            .iter()
            .filter(|e| e.severity == Severity::Error)
            .count();
        let warn_count = self.warnings.len();

        if err_count > 0 || warn_count > 0 {
            println!(
                "{}",
                format!(
                    "aborting due to {} error(s), {} warning(s)",
                    err_count, warn_count
                )
                .bold()
                .red()
            );
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
            let line_num_str = line.to_string();
            let padding = " ".repeat(line_num_str.len());
            let line_snippet = self.source_map.get_line_snippet(span.start); // add this

            // header
            println!(
                "{}:{}:{}: {}: {}",
                self.filename.color(Color::Cyan),
                line.to_string().color(Color::Yellow),
                col.to_string().color(Color::Yellow),
                severity_label.color(color).bold(),
                error.message.color(Color::White).bold()
            );

            // context before, up to 2 lines
            let context_before = 2;
            let start_line = line.saturating_sub(context_before);
            for ctx_line in start_line..line {
                if ctx_line == 0 {
                    continue;
                }
                let snippet = self
                    .source_map
                    .get_line_snippet(self.source_map.line_starts[ctx_line - 1]);
                println!(
                    " {} | {}",
                    ctx_line.to_string().color(Color::Cyan).dimmed(),
                    snippet.dimmed()
                );
            }

            // error line
            println!(
                " {} | {}",
                line_num_str.color(Color::Cyan),
                line_snippet.color(Color::White).bold()
            );

            // caret underline
            let mut underline = format!(" {} | ", padding);
            for _ in 0..col.saturating_sub(1) {
                underline.push(' ');
            }
            for _ in 0..length {
                underline.push('^');
            }
            println!("{}", underline.color(color).bold());

            // context after, up to 2 lines
            let context_after = 2;
            let end_line = (line + context_after).min(self.source_map.line_starts.len());
            for ctx_line in (line + 1)..=end_line {
                if ctx_line > self.source_map.line_starts.len() {
                    continue;
                }
                let snippet = self
                    .source_map
                    .get_line_snippet(self.source_map.line_starts[ctx_line - 1]);
                println!(
                    " {} | {}",
                    ctx_line.to_string().color(Color::Cyan).dimmed(),
                    snippet.dimmed()
                );
            }

            // hint if present
            if let Some(hint) = &error.hint {
                println!(" {} = {}: {}", padding, "hint".bold().cyan(), hint);
            }

            println!(); // blank line between errors
        } else {
            println!(
                "{}: {}",
                severity_label.color(color).bold(),
                error.message.color(Color::White)
            );
        }
    }

    pub fn report_ice_and_panic(&mut self, error: CompilerError) -> ! {
        self.errors.push(error.clone());
        self.print_ice_header(1);
        self.print_error(&error);
        self.print_ice_footer();
        panic!("Encountered internal compiler error");
    }
}
