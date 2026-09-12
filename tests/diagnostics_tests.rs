use std::{cell::RefCell, panic, rc::Rc};

use unnc::diagnostics::{CompilerError, Diagnostics, Phase, SharedDiagnostics, Span};

fn sev(err: &CompilerError) -> String {
    format!("{:?}", err.severity)
}

// ---- Diagnostics::new / SourceMap ----

#[test]
fn new_stores_filename_source_and_line_starts() {
    let d = Diagnostics::new("x.unn".to_string(), "hello\nworld\nfini".to_string());
    assert_eq!(d.filename, "x.unn");
    assert_eq!(d.source_map.source, "hello\nworld\nfini");
    assert_eq!(d.source_map.line_starts, vec![0, 6, 12]);
    assert!(d.errors.is_empty());
    assert!(d.warnings.is_empty());
}

#[test]
fn line_col_basic_multiline() {
    let d = Diagnostics::new("x.unn".to_string(), "hello\nworld\nfini".to_string());
    for (pos, expected) in [
        (0, (1, 1)),
        (4, (1, 5)),
        (5, (1, 6)),
        (6, (2, 1)),
        (11, (2, 6)),
        (12, (3, 1)),
        (15, (3, 4)),
        (16, (3, 5)),
    ] {
        assert_eq!(d.source_map.get_line_col(pos), expected, "pos {pos}");
    }
}

#[test]
fn line_col_is_char_based_for_multibyte_chars() {
    let d = Diagnostics::new("m.unn".to_string(), "ññ\nzz".to_string());
    assert_eq!(d.source_map.line_starts, vec![0, 5]);
    assert_eq!(d.source_map.get_line_col(0), (1, 1));
    assert_eq!(d.source_map.get_line_col(2), (1, 2));
    assert_eq!(d.source_map.get_line_col(4), (1, 3));
    assert_eq!(d.source_map.get_line_col(5), (2, 1));
    assert_eq!(d.source_map.get_line_col(6), (2, 2));
}

#[test]
fn line_col_mid_char_byte_pos_does_not_panic() {
    let d = Diagnostics::new("m.unn".to_string(), "ññ\nzz".to_string());
    // Positions inside a multi-byte char round up to that char's column.
    assert_eq!(d.source_map.get_line_col(1), (1, 2)); // inside first ñ
    assert_eq!(d.source_map.get_line_col(3), (1, 3)); // inside second ñ
}

#[test]
fn empty_source_edge() {
    let d = Diagnostics::new("e.unn".to_string(), String::new());
    assert_eq!(d.source_map.line_starts, vec![0]);
    assert_eq!(d.source_map.get_line_col(0), (1, 1));
    assert_eq!(d.source_map.get_line_snippet(0), "");
}

#[test]
fn consecutive_blank_line() {
    let d = Diagnostics::new("n.unn".to_string(), "a\n\nb".to_string());
    assert_eq!(d.source_map.line_starts, vec![0, 2, 3]);
    assert_eq!(d.source_map.get_line_col(0), (1, 1));
    assert_eq!(d.source_map.get_line_col(1), (1, 2));
    assert_eq!(d.source_map.get_line_col(2), (2, 1));
    assert_eq!(d.source_map.get_line_col(3), (3, 1));
    assert_eq!(d.source_map.get_line_snippet(0), "a");
    assert_eq!(d.source_map.get_line_snippet(2), "");
    assert_eq!(d.source_map.get_line_snippet(3), "b");
}

#[test]
fn line_snippet_middle_and_last_without_trailing_newline() {
    let d = Diagnostics::new("x.unn".to_string(), "hello\nworld\nfini".to_string());
    assert_eq!(d.source_map.get_line_snippet(0), "hello");
    assert_eq!(d.source_map.get_line_snippet(6), "world");
    assert_eq!(d.source_map.get_line_snippet(12), "fini");
}

#[test]
fn crlf_line_snippet_excludes_carriage_return() {
    let d = Diagnostics::new("crlf.unn".to_string(), "a\r\nb".to_string());
    assert_eq!(d.source_map.line_starts, vec![0, 3]);
    assert_eq!(d.source_map.get_line_snippet(0), "a");
    assert_eq!(d.source_map.get_line_snippet(3), "b");
    assert_eq!(d.source_map.get_line_col(3), (2, 1));
}

#[test]
fn crlf_line_snippets_exclude_cr_on_every_line() {
    let d = Diagnostics::new("crlf.unn".to_string(), "aa\r\nbb\r\n".to_string());
    assert_eq!(d.source_map.line_starts, vec![0, 4, 8]);
    assert_eq!(d.source_map.get_line_snippet(0), "aa");
    assert_eq!(d.source_map.get_line_snippet(4), "bb");
    assert_eq!(d.source_map.get_line_snippet(8), "");
}

#[test]
fn lone_cr_at_eof_is_not_line_content() {
    let d = Diagnostics::new("cr.unn".to_string(), "b\r".to_string());
    assert_eq!(d.source_map.get_line_snippet(0), "b");
}

// ---- get_snippet clamping ----

#[test]
fn get_snippet_clamps_out_of_bounds_spans() {
    let d = Diagnostics::new("s.unn".to_string(), "abcde".to_string());
    assert_eq!(d.source_map.get_snippet(&Span::new(0, 100)), "abcde");
    assert_eq!(d.source_map.get_snippet(&Span::new(3, 100)), "de");
    assert_eq!(d.source_map.get_snippet(&Span::new(100, 200)), "");
}

#[test]
fn get_snippet_whole_span() {
    let d = Diagnostics::new("s.unn".to_string(), "hello world".to_string());
    assert_eq!(d.source_map.get_snippet(&Span::new(0, 5)), "hello");
    assert_eq!(d.source_map.get_snippet(&Span::new(6, 11)), "world");
}

#[test]
fn get_snippet_inverted_span_does_not_panic() {
    let d = Diagnostics::new("s.unn".to_string(), "abcde".to_string());
    assert_eq!(d.source_map.get_snippet(&Span::new(4, 1)), "bcd");
    assert_eq!(d.source_map.get_snippet(&Span::new(5, 0)), "abcde");
}

#[test]
fn get_snippet_snaps_to_char_boundaries() {
    let d = Diagnostics::new("m.unn".to_string(), "ññ\nzz".to_string());
    assert_eq!(d.source_map.get_snippet(&Span::new(0, 2)), "ñ");
    assert_eq!(d.source_map.get_snippet(&Span::new(0, 4)), "ññ");
    assert_eq!(d.source_map.get_snippet(&Span::new(2, 4)), "ñ");
    assert_eq!(d.source_map.get_snippet(&Span::new(0, 5)), "ññ\n");
    assert_eq!(d.source_map.get_snippet(&Span::new(1, 3)), "");
    assert_eq!(d.source_map.get_snippet(&Span::new(1, 4)), "ñ");
    assert_eq!(d.source_map.get_snippet(&Span::new(2, 2)), "");
}

#[test]
fn char_count_is_char_based_and_sanitized() {
    let d = Diagnostics::new("m.unn".to_string(), "ññhey".to_string());
    assert_eq!(d.source_map.char_count(0, 4), 2);
    assert_eq!(d.source_map.char_count(1, 4), 1);
    assert_eq!(d.source_map.char_count(4, 7), 3);
    assert_eq!(d.source_map.char_count(7, 2), 4); // swapped bounds -> "ñhey"
    assert_eq!(d.source_map.char_count(3, 1), 0); // empty after snapping
}

// ---- Span ----

#[test]
fn span_new_length_and_traits() {
    let s = Span::new(3, 9);
    assert_eq!(s.start, 3);
    assert_eq!(s.end, 9);
    assert_eq!(s.length(), 6);
    assert_eq!(s, Span::new(3, 9));
    assert_eq!(s, Span { start: 3, end: 9 });
    assert_ne!(s, Span::new(9, 3));
}

#[test]
fn span_default_is_zero_length() {
    let s = Span::default();
    assert_eq!(s, Span::new(0, 0));
    assert_eq!(s.length(), 0);
}

#[test]
fn span_line_col_delegates_to_source_map() {
    let d = Diagnostics::new("x.unn".to_string(), "abc\ndef".to_string());
    assert_eq!(Span::new(0, 1).line_col(&d.source_map), (1, 1));
    assert_eq!(Span::new(4, 5).line_col(&d.source_map), (2, 1));
}

// ---- error constructors ----

#[test]
fn error_constructor_sets_expected_fields() {
    let e = CompilerError::error("boom".to_string(), Phase::Parser, Some(Span::new(1, 4)));
    assert_eq!(e.message, "boom");
    assert_eq!(e.span, Some(Span::new(1, 4)));
    assert_eq!(sev(&e), "Error");
    assert!(matches!(e.phase, Phase::Parser));
    assert_eq!(e.hint, None);
}

#[test]
fn warning_constructor_forces_phase_none() {
    let e = CompilerError::warning("careful".to_string(), Some(Span::new(2, 3)));
    assert_eq!(e.message, "careful");
    assert_eq!(e.span, Some(Span::new(2, 3)));
    assert_eq!(sev(&e), "Warning");
    assert!(matches!(e.phase, Phase::None));
    assert_eq!(e.hint, None);
}

#[test]
fn fatal_constructor_forces_phase_none() {
    let e = CompilerError::fatal("init failed".to_string(), None);
    assert_eq!(e.message, "init failed");
    assert_eq!(e.span, None);
    assert_eq!(sev(&e), "Fatal");
    assert!(matches!(e.phase, Phase::None));
}

#[test]
fn ice_constructor_sets_severity_and_phase() {
    let e = CompilerError::ice(
        "internal boom".to_string(),
        Phase::MIRBuilder,
        Some(Span::new(0, 1)),
    );
    assert_eq!(sev(&e), "Ice");
    assert!(matches!(e.phase, Phase::MIRBuilder));
    assert_eq!(e.span, Some(Span::new(0, 1)));
}

// ---- report() routing ----

#[test]
fn report_routes_error_to_errors() {
    let mut d = Diagnostics::new("r.unn".to_string(), "src".to_string());
    d.report(CompilerError::error(
        "boom".to_string(),
        Phase::Parser,
        Some(Span::new(1, 4)),
    ));
    assert_eq!(d.errors.len(), 1);
    assert!(d.warnings.is_empty());
    assert_eq!(d.errors[0].message, "boom");
    assert_eq!(d.errors[0].span, Some(Span::new(1, 4)));
    assert!(matches!(d.errors[0].phase, Phase::Parser));
    assert_eq!(sev(&d.errors[0]), "Error");
}

#[test]
fn report_routes_warning_to_warnings() {
    let mut d = Diagnostics::new("r.unn".to_string(), "src".to_string());
    d.report(CompilerError::warning(
        "warn me".to_string(),
        Some(Span::new(0, 1)),
    ));
    assert!(d.errors.is_empty());
    assert_eq!(d.warnings.len(), 1);
    assert_eq!(d.warnings[0].message, "warn me");
    assert_eq!(d.warnings[0].span, Some(Span::new(0, 1)));
    assert_eq!(sev(&d.warnings[0]), "Warning");
}

#[test]
fn report_routes_fatal_to_errors() {
    let mut d = Diagnostics::new("r.unn".to_string(), "src".to_string());
    d.report(CompilerError::fatal("deadly".to_string(), None));
    assert_eq!(d.errors.len(), 1);
    assert_eq!(sev(&d.errors[0]), "Fatal");
    assert!(d.warnings.is_empty());
}

#[test]
fn report_preserves_error_order() {
    let mut d = Diagnostics::new("r.unn".to_string(), "src".to_string());
    d.report(CompilerError::error(
        "first".to_string(),
        Phase::Lexer,
        None,
    ));
    d.report(CompilerError::fatal("middle".to_string(), None));
    d.report(CompilerError::error(
        "last".to_string(),
        Phase::Parser,
        None,
    ));
    d.report(CompilerError::warning("w".to_string(), None));
    assert_eq!(d.errors.len(), 3);
    assert_eq!(d.warnings.len(), 1);
    assert_eq!(d.errors[0].message, "first");
    assert_eq!(d.errors[1].message, "middle");
    assert_eq!(d.errors[2].message, "last");
    assert_eq!(sev(&d.errors[0]), "Error");
    assert_eq!(sev(&d.errors[1]), "Fatal");
    assert_eq!(sev(&d.errors[2]), "Error");
}

#[test]
fn error_count_includes_fatal() {
    let mut d = Diagnostics::new("r.unn".to_string(), "src".to_string());
    assert_eq!(d.error_count(), 0);
    d.report(CompilerError::error("e".to_string(), Phase::None, None));
    d.report(CompilerError::fatal("f".to_string(), None));
    d.report(CompilerError::warning("w".to_string(), None));
    assert_eq!(d.error_count(), 2);
}

#[test]
fn error_count_isolates_fatal_only() {
    let mut d = Diagnostics::new("r.unn".to_string(), "src".to_string());
    d.report(CompilerError::fatal("f".to_string(), None));
    d.report(CompilerError::warning("w".to_string(), None));
    assert_eq!(d.error_count(), 1);
}

#[test]
fn ice_report_panics_and_records_the_error() {
    let mut d = Diagnostics::new("i.unn".to_string(), "src".to_string());
    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        d.report(CompilerError::ice(
            "boom internal".to_string(),
            Phase::MIRBuilder,
            None,
        ));
    }));
    assert!(result.is_err(), "Ice must panic");
    assert_eq!(d.errors.len(), 1);
    assert_eq!(sev(&d.errors[0]), "Ice");
    assert_eq!(d.errors[0].message, "boom internal");
    assert!(matches!(d.errors[0].phase, Phase::MIRBuilder));
}

#[test]
fn shared_diagnostics_alias_is_usable() {
    let shared: SharedDiagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "sh.unn".to_string(),
        "src".to_string(),
    )));
    shared.borrow_mut().report(CompilerError::error(
        "via shared".to_string(),
        Phase::None,
        None,
    ));
    assert_eq!(shared.borrow().errors.len(), 1);
    assert_eq!(shared.borrow().errors[0].message, "via shared");
}

// ---- print() smoke tests (no stdout/panic assertions) ----
#[test]
fn print_does_not_panic_when_clean() {
    let d = Diagnostics::new("p.unn".to_string(), "src".to_string());
    d.print();
}

#[test]
fn print_does_not_panic_with_mixed_severities() {
    let mut d = Diagnostics::new("p.unn".to_string(), "hello\nworld".to_string());
    d.report(CompilerError::error(
        "e".to_string(),
        Phase::Lexer,
        Some(Span::new(0, 5)),
    ));
    d.report(CompilerError::warning(
        "w".to_string(),
        Some(Span::new(6, 11)),
    ));
    d.report(CompilerError::fatal("f".to_string(), Some(Span::new(0, 5))));
    d.print();
}

