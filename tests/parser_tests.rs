use std::cell::RefCell;
use std::rc::Rc;

use unnc::ast::{
    BinaryOp, Elif, EnumMember, Expr, ExprKind, InstParam, Literal, PostfixOp, Qualifier,
    QualifierKind, Stmt, StmtKind, Type, TypeKind, UnaryOp, VariantMember,
};
use unnc::diagnostics::{CompilerError, Diagnostics, Phase, Span};
use unnc::lexer::Lexer;
use unnc::parser::Parser;

fn parse_src(src: &str) -> (Vec<Stmt>, Vec<CompilerError>, bool) {
    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "parser_tests.unn".to_string(),
        src.to_string(),
    )));
    let mut lexer = Lexer::new(src, diagnostics.clone());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, diagnostics.clone());
    let stmts = parser.parse();
    let corrupted = parser.corrupted;
    let errors = diagnostics.borrow().errors.clone();
    (stmts, errors, corrupted)
}

fn sp(s: usize, e: usize) -> Span {
    Span::new(s, e)
}

fn st(kind: StmtKind, s: usize, e: usize) -> Stmt {
    Stmt::new(kind, sp(s, e))
}

fn id(name: &str, s: usize, e: usize) -> Expr {
    Expr::new(ExprKind::Identifier(name.to_string()), sp(s, e))
}

fn lit(l: Literal, s: usize, e: usize) -> Expr {
    Expr::new(ExprKind::Literal(l), sp(s, e))
}

fn ex(kind: ExprKind, s: usize, e: usize) -> Expr {
    Expr::new(kind, sp(s, e))
}

fn bin(l: Expr, op: BinaryOp, r: Expr, s: usize, e: usize) -> Expr {
    Expr::new(ExprKind::Binary(Box::new(l), op, Box::new(r)), sp(s, e))
}

fn unr(op: UnaryOp, operand: Expr, s: usize, e: usize) -> Expr {
    Expr::new(ExprKind::Unary(op, Box::new(operand)), sp(s, e))
}

fn post(operand: Expr, op: PostfixOp, s: usize, e: usize) -> Expr {
    Expr::new(ExprKind::Postfix(Box::new(operand), op), sp(s, e))
}

fn call(callee: Expr, args: Vec<Expr>, s: usize, e: usize) -> Expr {
    Expr::new(ExprKind::Call(Box::new(callee), args), sp(s, e))
}

fn index(target: Expr, idx: Expr, s: usize, e: usize) -> Expr {
    Expr::new(
        ExprKind::Index {
            target: Box::new(target),
            index: Box::new(idx),
        },
        sp(s, e),
    )
}

fn ty(kind: TypeKind, s: usize, e: usize) -> Type {
    Type {
        kind,
        span: sp(s, e),
    }
}

fn block(content: Vec<Stmt>, s: usize, e: usize) -> Expr {
    ex(ExprKind::Block(content), s, e)
}

fn ret(expr: Option<Expr>, s: usize, e: usize) -> Stmt {
    st(StmtKind::Return(expr), s, e)
}

fn var_decl(
    qualifiers: Vec<Qualifier>,
    type_annotation: Option<Type>,
    name: Expr,
    init: Expr,
    s: usize,
    e: usize,
) -> Stmt {
    st(
        StmtKind::VarDecl {
            qualifiers,
            type_annotation,
            name: Box::new(name),
            init: Box::new(init),
        },
        s,
        e,
    )
}

fn param_decl(name: Expr, type_annotation: Type, s: usize, e: usize) -> Stmt {
    st(
        StmtKind::ParamDecl {
            qualifiers: vec![],
            name: Box::new(name),
            type_annotation,
            def: None,
        },
        s,
        e,
    )
}

fn qualifier(kind: QualifierKind, s: usize, e: usize) -> Qualifier {
    Qualifier {
        kind,
        span: sp(s, e),
    }
}

// ---------------------------------------------------------------------------
// Variable declarations & literals
// ---------------------------------------------------------------------------

#[test]
fn int_literal_var_decl() {
    let (stmts, errors, corrupted) = parse_src("var x = 42;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        lit(Literal::Int(42), 8, 10),
        0,
        11,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn typed_int_var_decl() {
    // Type annotation is type-FIRST: `var i32 y = 7;` not `var y: i32;`.
    let (stmts, errors, corrupted) = parse_src("var y: i32 = 7;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(TypeKind::I32, 7, 10)),
        id("y", 4, 5),
        lit(Literal::Int(7), 13, 14),
        0,
        15,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn float_literal_var_decl() {
    let (stmts, errors, corrupted) = parse_src("var z: f64 = 1.5;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(TypeKind::F64, 7, 10)),
        id("z", 4, 5),
        lit(Literal::Float(1.5), 13, 16),
        0,
        17,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn string_literal_var_decl() {
    let (stmts, errors, corrupted) = parse_src("var s:str = \"hi\";");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(TypeKind::Str, 6, 9)),
        id("s", 4, 5),
        lit(Literal::Str("hi".to_string()), 12, 16),
        0,
        17,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn char8_literal_var_decl() {
    let (stmts, errors, corrupted) = parse_src("var c:char8 = 'a';");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(TypeKind::Char8, 6, 11)),
        id("c", 4, 5),
        lit(Literal::Char8(97), 14, 17),
        0,
        18,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn bool_literal_var_decl() {
    let (stmts, errors, corrupted) = parse_src("var x = true;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        lit(Literal::Bool(true), 8, 12),
        0,
        13,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn array_literal_var_decl() {
    let (stmts, errors, corrupted) = parse_src("var x = [1, 2, 3];");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        Expr::new(
            ExprKind::Literal(Literal::ArrayLiteral(vec![
                lit(Literal::Int(1), 9, 10),
                lit(Literal::Int(2), 12, 13),
                lit(Literal::Int(3), 15, 16),
            ])),
            sp(8, 17),
        ),
        0,
        18,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn suffixed_integer_literals() {
    for (src, lit_exp, span_s, span_e, decl_e) in [
        ("var x = 2i32;", Literal::Int32(2), 8, 12, 13),
        ("var x = 0u8;", Literal::Uint8(0), 8, 11, 12),
        ("var x = 5iz;", Literal::IntSize(5), 8, 11, 12),
        ("var x = 5uz;", Literal::UintSize(5), 8, 11, 12),
    ] {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let expected = vec![var_decl(
            vec![],
            None,
            id("x", 4, 5),
            lit(lit_exp, span_s, span_e),
            0,
            decl_e,
        )];
        assert_eq!(stmts, expected, "mismatch for {src}");
    }
}

#[test]
fn suffixed_float_and_char_literals() {
    for (src, lit_exp, span_s, span_e, decl_e) in [
        ("var x = 1.5f32;", Literal::F32(1.5), 8, 14, 15),
        ("var x = 'a'c16;", Literal::Char16(97), 8, 14, 15),
        ("var x = 'a'c32;", Literal::Char32('a'), 8, 14, 15),
    ] {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let expected = vec![var_decl(
            vec![],
            None,
            id("x", 4, 5),
            lit(lit_exp, span_s, span_e),
            0,
            decl_e,
        )];
        assert_eq!(stmts, expected, "mismatch for {src}");
    }
}

#[test]
fn radix_literals() {
    for (src, value, span_s, span_e) in [
        ("var x = 0x1F;", 31, 8, 12),
        ("var x = 0b101;", 5, 8, 13),
        ("var x = 0o17;", 15, 8, 12),
    ] {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let expected = vec![var_decl(
            vec![],
            None,
            id("x", 4, 5),
            lit(Literal::Int(value), span_s, span_e),
            0,
            span_e + 1,
        )];
        assert_eq!(stmts, expected, "mismatch for {src}");
    }
}

#[test]
fn return_some_and_none() {
    let (stmts, errors, corrupted) = parse_src("return 5;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    assert_eq!(stmts, vec![ret(Some(lit(Literal::Int(5), 7, 8)), 0, 9)]);

    let (stmts2, errors2, corrupted2) = parse_src("return;");
    assert!(!corrupted2);
    assert!(errors2.is_empty());
    assert_eq!(stmts2, vec![ret(None, 0, 7)]);
}

// ---------------------------------------------------------------------------
// Type annotations
// ---------------------------------------------------------------------------

#[test]
fn ptr_and_ref_types() {
    let (stmts, errors, corrupted) = parse_src("var p: ptr<i32> = q;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::Ptr(Box::new(ty(TypeKind::I32, 11, 14))),
            7,
            14,
        )),
        id("p", 4, 5),
        id("q", 18, 19),
        0,
        20,
    )];
    assert_eq!(stmts, expected);

    let (stmts2, errors2, corrupted2) = parse_src("var r: ref<i32> = q;");
    assert!(!corrupted2);
    assert!(errors2.is_empty());
    let expected2 = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::Ref(Box::new(ty(TypeKind::I32, 11, 14))),
            7,
            14,
        )),
        id("r", 4, 5),
        id("q", 18, 19),
        0,
        20,
    )];
    assert_eq!(stmts2, expected2);
}

#[test]
fn tuple_type() {
    let (stmts, errors, corrupted) = parse_src("var t: (i32, f32) = pair;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::Tuple(vec![ty(TypeKind::I32, 8, 11), ty(TypeKind::F32, 13, 16)]),
            7,
            19,
        )),
        id("t", 4, 5),
        id("pair", 20, 24),
        0,
        25,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn nullable_type() {
    let (stmts, errors, corrupted) = parse_src("var o: (i32)? = p;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::Nullable(Box::new(ty(
                TypeKind::Tuple(vec![ty(TypeKind::I32, 8, 11)]),
                7,
                13,
            ))),
            7,
            15,
        )),
        id("o", 4, 5),
        id("p", 16, 17),
        0,
        18,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn failable_type() {
    // Failable type syntax: `!!(ok, err)` — the `!!` leads the paren group.
    let (stmts, errors, corrupted) = parse_src("var f: !!(i32, str) = g;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::Failable(
                Box::new(ty(TypeKind::I32, 10, 13)),
                Box::new(ty(TypeKind::Str, 15, 18)),
            ),
            7,
            21,
        )),
        id("f", 4, 5),
        id("g", 22, 23),
        0,
        24,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn unit_type() {
    let (stmts, errors, corrupted) = parse_src("var u: ()  = v;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(TypeKind::Unit, 7, 9)),
        id("u", 4, 5),
        id("v", 13, 14),
        0,
        15,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn custom_type() {
    let (stmts, errors, corrupted) = parse_src("var m: MyType= n;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::CustomType(Box::new(id("MyType", 7, 13))),
            7,
            13,
        )),
        id("m", 4, 5),
        id("n", 15, 16),
        0,
        17,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn generic_type() {
    let (stmts, errors, corrupted) = parse_src("var m: Pair<i32, u32> = n;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::GenericType {
                name: Box::new(id("Pair", 7, 11)),
                type_params: vec![ty(TypeKind::I32, 12, 15), ty(TypeKind::U32, 17, 20)],
            },
            7,
            11,
        )),
        id("m", 4, 5),
        id("n", 24, 25),
        0,
        26,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn func_pointer_type() {
    let (stmts, errors, corrupted) = parse_src("var fp: func(i32, f32): bool = g;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::Func(
                vec![ty(TypeKind::I32, 13, 16), ty(TypeKind::F32, 18, 21)],
                Box::new(Some(ty(TypeKind::Bool, 24, 28))),
            ),
            8,
            30,
        )),
        id("fp", 4, 6),
        id("g", 31, 32),
        0,
        33,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn array_type() {
    // Array types use square brackets: `[i32, 4]`.
    let (stmts, errors, corrupted) = parse_src("var a: [i32, 4]  = b;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        Some(ty(
            TypeKind::Array(
                Box::new(ty(TypeKind::I32, 8, 11)),
                Some(lit(Literal::Int(4), 13, 14)),
            ),
            8,
            11,
        )),
        id("a", 4, 5),
        id("b", 19, 20),
        0,
        21,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn type_first_annotation_rejected() {
    // Annotations are `var name: Type = value`. The old type-FIRST spelling
    // (`var Type name = value`) is no longer accepted: the parser reads `i32`
    // where the name belongs and reports it.
    let (_, errors, corrupted) = parse_src("var i32 x = 1;");
    assert!(corrupted);
    assert_eq!(errors.len(), 1, "errors: {:?}", errors);
    assert_eq!(errors[0].message, "Expected Identifier, found I32Key");
    assert_eq!(errors[0].span, Some(sp(4, 7)));
    assert!(matches!(errors[0].phase, Phase::Parser));
}

// ---------------------------------------------------------------------------
// Expressions & precedence
// ---------------------------------------------------------------------------

#[test]
fn add_mul_precedence() {
    // `*` binds tighter than `+`: 1 + 2 * 3 => 1 + (2 * 3)
    let (stmts, errors, corrupted) = parse_src("x = 1 + 2 * 3;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let rhs = bin(
        lit(Literal::Int(1), 4, 5),
        BinaryOp::Add,
        bin(
            lit(Literal::Int(2), 8, 9),
            BinaryOp::Mul,
            lit(Literal::Int(3), 12, 13),
            8,
            13,
        ),
        4,
        13,
    );
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 13)),
        0,
        13,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn mul_add_left_assoc() {
    // Same precedence operators associate left: 1 * 2 + 3 => (1 * 2) + 3
    let (stmts, errors, corrupted) = parse_src("x = 1 * 2 + 3;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let lhs = bin(
        lit(Literal::Int(1), 4, 5),
        BinaryOp::Mul,
        lit(Literal::Int(2), 8, 9),
        4,
        9,
    );
    let rhs = bin(lhs, BinaryOp::Add, lit(Literal::Int(3), 12, 13), 4, 13);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 13)),
        0,
        13,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn assignment_is_right_associative() {
    let (stmts, errors, corrupted) = parse_src("x = a = b = c;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let rhs = bin(
        id("a", 4, 5),
        BinaryOp::Assign,
        bin(id("b", 8, 9), BinaryOp::Assign, id("c", 12, 13), 8, 13),
        4,
        13,
    );
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 13)),
        0,
        13,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn equality_and_logical_precedence() {
    // `&&` is looser than `==`, which is looser than `+`: (a + b) == c && d
    let (stmts, errors, corrupted) = parse_src("x = a + b == c && d;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let summed = bin(id("a", 4, 5), BinaryOp::Add, id("b", 8, 9), 4, 9);
    let equality = bin(summed, BinaryOp::Eq, id("c", 13, 14), 4, 14);
    let rhs = bin(equality, BinaryOp::And, id("d", 18, 19), 4, 19);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 19)),
        0,
        19,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn grouping_overrides_precedence() {
    let (stmts, errors, corrupted) = parse_src("x = (1 + 2) * 3;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let grouped = bin(
        lit(Literal::Int(1), 5, 6),
        BinaryOp::Add,
        lit(Literal::Int(2), 9, 10),
        4,
        11,
    );
    let rhs = bin(grouped, BinaryOp::Mul, lit(Literal::Int(3), 14, 15), 4, 15);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 15)),
        0,
        15,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn shift_operators() {
    for (src, op) in [
        ("var x = 1 shl 2;", BinaryOp::Shl),
        ("var x = 1 shr 2;", BinaryOp::Shr),
    ] {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let expected = vec![var_decl(
            vec![],
            None,
            id("x", 4, 5),
            bin(
                lit(Literal::Int(1), 8, 9),
                op,
                lit(Literal::Int(2), 14, 15),
                8,
                15,
            ),
            0,
            16,
        )];
        assert_eq!(stmts, expected, "mismatch for {src}");
    }
}

#[test]
fn bitwise_and_or_xor() {
    for (src, op, rhs_s, decl_e) in [
        ("var x = 1 and 2;", BinaryOp::BitAnd, 14, 16),
        ("var x = 1 or 2;", BinaryOp::BitOr, 13, 15),
        ("var x = 1 xor 2;", BinaryOp::Xor, 14, 16),
    ] {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let expected = vec![var_decl(
            vec![],
            None,
            id("x", 4, 5),
            bin(
                lit(Literal::Int(1), 8, 9),
                op,
                lit(Literal::Int(2), rhs_s, rhs_s + 1),
                8,
                rhs_s + 1,
            ),
            0,
            decl_e,
        )];
        assert_eq!(stmts, expected, "mismatch for {src}");
    }
}

#[test]
fn bitwise_binds_tighter_than_equality() {
    // `a and b == c or d` parses as (a and b) == (c or d),
    // because bitwise binds tighter than equality.
    let (stmts, errors, corrupted) = parse_src("var x = a and b == c or d;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let lhs = bin(id("a", 8, 9), BinaryOp::BitAnd, id("b", 14, 15), 8, 15);
    let rhs = bin(id("c", 19, 20), BinaryOp::BitOr, id("d", 24, 25), 19, 25);
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(lhs, BinaryOp::Eq, rhs, 8, 25),
        0,
        26,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn comparison_chain_is_left_assoc() {
    let (stmts, errors, corrupted) = parse_src("var x = a <= b >= c;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(
            bin(id("a", 8, 9), BinaryOp::Leq, id("b", 13, 14), 8, 14),
            BinaryOp::Geq,
            id("c", 18, 19),
            8,
            19,
        ),
        0,
        20,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn coalesce_operator() {
    let (stmts, errors, corrupted) = parse_src("x = a ?? b;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let rhs = bin(id("a", 4, 5), BinaryOp::Coalesce, id("b", 9, 10), 4, 10);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 10)),
        0,
        10,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn not_equals() {
    let (stmts, errors, corrupted) = parse_src("x = a != b;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let rhs = bin(id("a", 4, 5), BinaryOp::Neq, id("b", 9, 10), 4, 10);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 10)),
        0,
        10,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn modulo_operator() {
    let (stmts, errors, corrupted) = parse_src("var x = 5 % 2;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(
            lit(Literal::Int(5), 8, 9),
            BinaryOp::Mod,
            lit(Literal::Int(2), 12, 13),
            8,
            13,
        ),
        0,
        14,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn unary_operators() {
    let cases = [
        ("x = -y;", UnaryOp::Neg, 4, 6),
        ("x = !y;", UnaryOp::Not, 4, 6),
        ("x = ^y;", UnaryOp::Dereference, 4, 6),
        ("x = @y;", UnaryOp::AddressOf, 4, 6),
        ("x = not y;", UnaryOp::BitNot, 4, 9),
        ("x = ++y;", UnaryOp::Increment, 4, 7),
        ("x = --y;", UnaryOp::Decrement, 4, 7),
    ];
    for (src, op, us, ue) in cases {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let rhs = unr(op, id("y", ue - 1, ue), us, ue);
        let expected = st(
            StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, ue)),
            0,
            ue,
        );
        assert_eq!(stmts, vec![expected], "mismatch for {src}");
    }
}

#[test]
fn postfix_operators() {
    let cases = [
        ("x = y++;", PostfixOp::Increment),
        ("x = y--;", PostfixOp::Decrement),
    ];
    for (src, op) in cases {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let rhs = post(id("y", 4, 5), op, 4, 7);
        let expected = st(
            StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 7)),
            0,
            7,
        );
        assert_eq!(stmts, vec![expected], "mismatch for {src}");
    }
}

#[test]
fn unary_bitnot_under_equality() {
    let (stmts, errors, corrupted) = parse_src("var x = not a == b;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(
            unr(UnaryOp::BitNot, id("a", 12, 13), 8, 13),
            BinaryOp::Eq,
            id("b", 17, 18),
            8,
            18,
        ),
        0,
        19,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn indexing() {
    let (stmts, errors, corrupted) = parse_src("x = a[1];");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let rhs = index(id("a", 4, 5), lit(Literal::Int(1), 6, 7), 4, 8);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 8)),
        0,
        8,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn nested_indexing() {
    let (stmts, errors, corrupted) = parse_src("x = a[1][2];");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let inner = index(id("a", 4, 5), lit(Literal::Int(1), 6, 7), 4, 8);
    let rhs = index(inner, lit(Literal::Int(2), 9, 10), 4, 11);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 11)),
        0,
        11,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn call_with_multiple_args() {
    let (stmts, errors, corrupted) = parse_src("x = f(a, b);");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let rhs = call(id("f", 4, 5), vec![id("a", 6, 7), id("b", 9, 10)], 4, 12);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 12)),
        0,
        12,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn nested_call_chain() {
    let (stmts, errors, corrupted) = parse_src("var x = f(1)(2);");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let inner = call(id("f", 8, 9), vec![lit(Literal::Int(1), 10, 11)], 8, 13);
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        call(inner, vec![lit(Literal::Int(2), 13, 14)], 8, 16),
        0,
        16,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn member_access_chain() {
    let (stmts, errors, corrupted) = parse_src("var x = a.b.c.d;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let a_b = bin(id("a", 8, 9), BinaryOp::Access, id("b", 10, 11), 8, 11);
    let a_b_c = bin(a_b, BinaryOp::Access, id("c", 12, 13), 8, 13);
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(a_b_c, BinaryOp::Access, id("d", 14, 15), 8, 15),
        0,
        16,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn scope_resolution_and_call() {
    let (stmts, errors, corrupted) = parse_src("x = A::B::f();");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let f_call = call(id("f", 10, 11), vec![], 10, 14);
    let b_scope = bin(id("B", 7, 8), BinaryOp::Scope, f_call, 7, 14);
    let rhs = bin(id("A", 4, 5), BinaryOp::Scope, b_scope, 4, 14);
    let expected = st(
        StmtKind::Expr(bin(id("x", 0, 1), BinaryOp::Assign, rhs, 0, 14)),
        0,
        14,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn turbofish_generic_call() {
    let (stmts, errors, corrupted) = parse_src("var p = Pair::<i32, u32>::new();");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let instantiation = Expr::new(
        ExprKind::GenericInstantion {
            name: Box::new(id("Pair", 8, 12)),
            type_params: vec![ty(TypeKind::I32, 15, 18), ty(TypeKind::U32, 20, 23)],
        },
        sp(8, 24),
    );
    let expected = vec![var_decl(
        vec![],
        None,
        id("p", 4, 5),
        bin(
            instantiation,
            BinaryOp::Scope,
            call(id("new", 26, 29), vec![], 26, 32),
            8,
            32,
        ),
        0,
        32,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn turbofish_in_named_scope_chain() {
    let src = "var x = A::B::generic_helper::<i32>(1);";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let instantiation = Expr::new(
        ExprKind::GenericInstantion {
            name: Box::new(id("generic_helper", 14, 28)),
            type_params: vec![ty(TypeKind::I32, 31, 34)],
        },
        sp(14, 35),
    );
    let generic_call = call(instantiation, vec![lit(Literal::Int(1), 36, 37)], 14, 39);
    let b_scope = bin(id("B", 11, 12), BinaryOp::Scope, generic_call, 11, 39);
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(id("A", 8, 9), BinaryOp::Scope, b_scope, 8, 39),
        0,
        39,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn member_access_on_call_result() {
    let (stmts, errors, corrupted) = parse_src("var x = foo().bar;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(
            call(id("foo", 8, 11), vec![], 8, 14),
            BinaryOp::Access,
            id("bar", 14, 17),
            8,
            17,
        ),
        0,
        18,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn tuple_member_access_float_ambiguity() {
    // `a.0.1` lexes as `a` Access `0.1` (a float member), documented fixture behavior.
    let (stmts, errors, corrupted) = parse_src("var x = a.0.1;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        bin(
            id("a", 8, 9),
            BinaryOp::Access,
            lit(Literal::Float(0.1), 10, 13),
            8,
            13,
        ),
        0,
        14,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn tuple_member_access_int() {
    for (src, member, target_name, target_s, target_e, ms, me, end) in [
        ("var x = a.0;", Literal::Int(0), "a", 8, 9, 10, 11, 12),
        (
            "var x = nested.1;",
            Literal::Int(1),
            "nested",
            8,
            14,
            15,
            16,
            17,
        ),
    ] {
        let (stmts, errors, corrupted) = parse_src(src);
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let expected = vec![var_decl(
            vec![],
            None,
            id("x", 4, 5),
            bin(
                id(target_name, target_s, target_e),
                BinaryOp::Access,
                lit(member, ms, me),
                8,
                me,
            ),
            0,
            end,
        )];
        assert_eq!(stmts, expected, "mismatch for {src}");
    }
}

#[test]
fn unwrap_expression() {
    let (stmts, errors, corrupted) = parse_src("var x = unwrap[opt];");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        Expr::new(ExprKind::Unwrap(Box::new(id("opt", 15, 18))), sp(8, 20)),
        0,
        20,
    )];
    assert_eq!(stmts, expected);

    // unwrap of a binary expression inside the brackets
    let (stmts2, errors2, corrupted2) = parse_src("var x = unwrap[a + b];");
    assert!(!corrupted2);
    assert!(errors2.is_empty());
    let expected2 = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        Expr::new(
            ExprKind::Unwrap(Box::new(bin(
                id("a", 15, 16),
                BinaryOp::Add,
                id("b", 19, 20),
                15,
                20,
            ))),
            sp(8, 22),
        ),
        0,
        22,
    )];
    assert_eq!(stmts2, expected2);

    // unwrap of an access expression
    let (stmts3, errors3, corrupted3) = parse_src("var x = unwrap[opt.thing];");
    assert!(!corrupted3);
    assert!(errors3.is_empty());
    let expected3 = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        Expr::new(
            ExprKind::Unwrap(Box::new(bin(
                id("opt", 15, 18),
                BinaryOp::Access,
                id("thing", 19, 24),
                15,
                24,
            ))),
            sp(8, 26),
        ),
        0,
        26,
    )];
    assert_eq!(stmts3, expected3);
}

#[test]
fn cast_expression() {
    let (stmts, errors, corrupted) = parse_src("var x = cast<i32>(y);");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        Expr::new(
            ExprKind::StaticCast(
                Box::new(ty(TypeKind::I32, 13, 16)),
                Box::new(id("y", 18, 19)),
            ),
            sp(8, 20),
        ),
        0,
        21,
    )];
    assert_eq!(stmts, expected);
}

#[test]
fn sizeof_expression() {
    // `sizeof` must be lowercase; `sizeOf` is not recognized.
    let (stmts, errors, corrupted) = parse_src("var x = sizeof<i32>;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![],
        None,
        id("x", 4, 5),
        Expr::new(
            ExprKind::SizeOfExpr(Box::new(ty(TypeKind::I32, 15, 18))),
            sp(8, 20),
        ),
        0,
        20,
    )];
    assert_eq!(stmts, expected);
}

// ---------------------------------------------------------------------------
// Statements & control flow
// ---------------------------------------------------------------------------

#[test]
fn compound_assignment() {
    let src = "var x = 1; x += 2;";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![
        var_decl(
            vec![],
            None,
            id("x", 4, 5),
            lit(Literal::Int(1), 8, 9),
            0,
            10,
        ),
        st(
            StmtKind::Expr(bin(
                id("x", 11, 12),
                BinaryOp::AddAssign,
                lit(Literal::Int(2), 16, 17),
                11,
                17,
            )),
            11,
            17,
        ),
    ];
    assert_eq!(stmts, expected);
}

#[test]
fn simple_assignment_and_read_as_expr_stmt() {
    for (src, op) in [
        ("var x = 1; x = 2;", BinaryOp::Assign),
        ("var x = 1; x += 2;", BinaryOp::AddAssign),
        ("var x = 1; x -= 2;", BinaryOp::SubAssign),
        ("var x = 1; x *= 2;", BinaryOp::MulAssign),
        ("var x = 1; x /= 2;", BinaryOp::DivAssign),
        ("var x = 1; x %= 2;", BinaryOp::ModAssign),
    ] {
        let (stmts, errors, corrupted) = parse_src(src);
        let n = src.len() as usize;
        assert!(!corrupted, "corrupted for {src}");
        assert!(errors.is_empty(), "errors for {src}: {:?}", errors);
        let rhs_start = n - 2; // literal operand is the second-to-last character (`2;`)
        let rhs = st(
            StmtKind::Expr(bin(
                id("x", 11, 12),
                op,
                lit(Literal::Int(2), rhs_start, rhs_start + 1),
                11,
                rhs_start + 1,
            )),
            11,
            rhs_start + 1,
        );
        assert_eq!(&stmts[1], &rhs, "mismatch for {src}");
    }
}

#[test]
fn if_else_statement() {
    let src = "var x = 0; if a { x = 1; } else { x = 2; }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let decl = var_decl(
        vec![],
        None,
        id("x", 4, 5),
        lit(Literal::Int(0), 8, 9),
        0,
        10,
    );
    let then_body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("x", 18, 19),
                BinaryOp::Assign,
                lit(Literal::Int(1), 22, 23),
                18,
                23,
            )),
            18,
            23,
        )],
        16,
        26,
    );
    let else_body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("x", 34, 35),
                BinaryOp::Assign,
                lit(Literal::Int(2), 38, 39),
                34,
                39,
            )),
            34,
            39,
        )],
        32,
        42,
    );
    let if_stmt = st(
        StmtKind::IfStmt {
            condition: Box::new(id("a", 14, 15)),
            body: Box::new(then_body),
            elifs: vec![],
            else_body: Some(Box::new(else_body)),
        },
        11,
        42,
    );
    assert_eq!(stmts, vec![decl, if_stmt]);
}

#[test]
fn if_elif_else_statement() {
    let src = "if a { x = 1; } elif b { y = 2; } else { z = 3; }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let then_body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("x", 7, 8),
                BinaryOp::Assign,
                lit(Literal::Int(1), 11, 12),
                7,
                12,
            )),
            7,
            12,
        )],
        5,
        15,
    );
    let elif_body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("y", 25, 26),
                BinaryOp::Assign,
                lit(Literal::Int(2), 29, 30),
                25,
                30,
            )),
            25,
            30,
        )],
        23,
        33,
    );
    let else_body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("z", 41, 42),
                BinaryOp::Assign,
                lit(Literal::Int(3), 45, 46),
                41,
                46,
            )),
            41,
            46,
        )],
        39,
        49,
    );
    let if_stmt = st(
        StmtKind::IfStmt {
            condition: Box::new(id("a", 3, 4)),
            body: Box::new(then_body),
            elifs: vec![Elif {
                condition: Box::new(id("b", 21, 22)),
                body: Box::new(elif_body),
            }],
            else_body: Some(Box::new(else_body)),
        },
        0,
        49,
    );
    assert_eq!(stmts, vec![if_stmt]);
}

#[test]
fn while_loop() {
    let (stmts, errors, corrupted) = parse_src("while a { x = 1; }");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("x", 10, 11),
                BinaryOp::Assign,
                lit(Literal::Int(1), 14, 15),
                10,
                15,
            )),
            10,
            15,
        )],
        8,
        18,
    );
    let expected = st(
        StmtKind::WhileStmt {
            condition: Box::new(id("a", 6, 7)),
            body: Box::new(body),
        },
        0,
        18,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn while_loop_break_and_continue() {
    // `break`/`continue` may carry an optional trailing semicolon.
    let (stmts, errors, corrupted) = parse_src("while a { break; }");
    assert!(!corrupted, "{:?}", errors);
    assert!(errors.is_empty(), "{:?}", errors);
    let body = block(vec![st(StmtKind::Break, 10, 15)], 8, 18);
    let expected = st(
        StmtKind::WhileStmt {
            condition: Box::new(id("a", 6, 7)),
            body: Box::new(body),
        },
        0,
        18,
    );
    assert_eq!(stmts, vec![expected]);

    let (stmts2, errors2, corrupted2) = parse_src("while a { continue }");
    assert!(!corrupted2, "{:?}", errors2);
    assert!(errors2.is_empty(), "{:?}", errors2);
    let body2 = block(vec![st(StmtKind::Continue, 10, 18)], 8, 20);
    let expected2 = st(
        StmtKind::WhileStmt {
            condition: Box::new(id("a", 6, 7)),
            body: Box::new(body2),
        },
        0,
        20,
    );
    assert_eq!(stmts2, vec![expected2]);
}

#[test]
fn for_loop() {
    let src = "for var i = 0; i < 10; i += 1 { x = 1; }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted, "errors: {:#?}", errors);
    assert!(errors.is_empty(), "errors: {:#?}", errors);
    let init_stmt = st(
        StmtKind::VarDecl {
            qualifiers: vec![],
            type_annotation: None,
            name: Box::new(id("i", 8, 9)),
            init: Box::new(lit(Literal::Int(0), 12, 13)),
        },
        4,
        14,
    );
    let body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("x", 32, 33),
                BinaryOp::Assign,
                lit(Literal::Int(1), 36, 37),
                32,
                37,
            )),
            32,
            37,
        )],
        30,
        40,
    );
    let expected = st(
        StmtKind::ForStmt {
            init: Box::new(init_stmt),
            condition: Box::new(bin(
                id("i", 15, 16),
                BinaryOp::Lt,
                lit(Literal::Int(10), 19, 21),
                15,
                21,
            )),
            update: Box::new(bin(
                id("i", 23, 24),
                BinaryOp::AddAssign,
                lit(Literal::Int(1), 28, 29),
                23,
                29,
            )),
            body: Box::new(body),
        },
        0,
        40,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn for_loop_double_semicolon_now_rejected() {
    // The old `;;` workaround for the parse_var/parse_for bug is no longer valid.
    let (_, errors, corrupted) = parse_src("for var i = 0;; i < 10; i += 1 { x = 1; }");
    assert!(corrupted);
    assert_eq!(errors.len(), 2, "errors: {:#?}", errors);
    assert_eq!(errors[0].message, "Unexpected prefix token: Semicolon");
    assert_eq!(errors[0].span, Some(sp(14, 15)));
    assert_eq!(errors[1].message, "Expected Semicolon, found LBrace");
    assert_eq!(errors[1].span, Some(sp(31, 32)));
    assert!(matches!(errors[0].phase, Phase::Parser));
}

#[test]
fn each_loop() {
    let (stmts, errors, corrupted) = parse_src("each item in list { x = item; }");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let body = block(
        vec![st(
            StmtKind::Expr(bin(
                id("x", 20, 21),
                BinaryOp::Assign,
                id("item", 24, 28),
                20,
                28,
            )),
            20,
            28,
        )],
        18,
        31,
    );
    let expected = st(
        StmtKind::EachStmt {
            item: Box::new(id("item", 5, 9)),
            collection: Box::new(id("list", 13, 17)),
            body: Box::new(body),
        },
        0,
        31,
    );
    assert_eq!(stmts, vec![expected]);
}

// ---------------------------------------------------------------------------
// Functions
// ---------------------------------------------------------------------------

#[test]
fn function_with_params_and_return() {
    let src = "func foo(a: i32, b: u8) : bool { return true; }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::FunctionDef {
            qualifiers: vec![],
            name: Box::new(id("foo", 5, 8)),
            params: vec![
                param_decl(id("a", 9, 10), ty(TypeKind::I32, 12, 15), 9, 16),
                param_decl(id("b", 17, 18), ty(TypeKind::U8, 20, 22), 17, 23),
            ],
            type_annotation: Some(ty(TypeKind::Bool, 26, 30)),
            body: Box::new(block(
                vec![ret(Some(lit(Literal::Bool(true), 40, 44)), 33, 47)],
                31,
                47,
            )),
        },
        0,
        47,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn function_body_tail_expression_parses() {
    // A block's final statement may omit the `;`; it parses as the new
    // TailExpr statement kind (the block's value under tail semantics).
    let src = "func foo: i32 { 5 }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::FunctionDef {
            qualifiers: vec![],
            name: Box::new(id("foo", 5, 8)),
            params: vec![],
            type_annotation: Some(ty(TypeKind::I32, 10, 13)),
            body: Box::new(block(
                vec![st(
                    StmtKind::TailExpr(Box::new(lit(Literal::Int(5), 16, 17))),
                    16,
                    17,
                )],
                14,
                19,
            )),
        },
        0,
        19,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn function_without_parens_return_type() {
    // `func foo: i32 { ... }` — return type may omit parens.
    let src = "func foo: i32 { return 0; }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::FunctionDef {
            qualifiers: vec![],
            name: Box::new(id("foo", 5, 8)),
            params: vec![],
            type_annotation: Some(ty(TypeKind::I32, 10, 13)),
            body: Box::new(block(
                vec![ret(Some(lit(Literal::Int(0), 23, 24)), 16, 27)],
                14,
                27,
            )),
        },
        0,
        27,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn function_returning_ptr() {
    let src = "func foo(a: i32): ptr<i32> { return 0; }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::FunctionDef {
            qualifiers: vec![],
            name: Box::new(id("foo", 5, 8)),
            params: vec![param_decl(id("a", 9, 10), ty(TypeKind::I32, 12, 15), 9, 16)],
            type_annotation: Some(ty(
                TypeKind::Ptr(Box::new(ty(TypeKind::I32, 22, 25))),
                18,
                25,
            )),
            body: Box::new(block(
                vec![ret(Some(lit(Literal::Int(0), 36, 37)), 29, 40)],
                27,
                40,
            )),
        },
        0,
        40,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn function_returning_tuple() {
    let src = "func foo() : (i32, str) { return 1; }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::FunctionDef {
            qualifiers: vec![],
            name: Box::new(id("foo", 5, 8)),
            params: vec![],
            type_annotation: Some(ty(
                TypeKind::Tuple(vec![ty(TypeKind::I32, 14, 17), ty(TypeKind::Str, 19, 22)]),
                13,
                25,
            )),
            body: Box::new(block(
                vec![ret(Some(lit(Literal::Int(1), 33, 34)), 26, 37)],
                24,
                37,
            )),
        },
        0,
        37,
    );
    assert_eq!(stmts, vec![expected]);
}

// ---------------------------------------------------------------------------
// Declarations: struct, enum, variant, seal, methods, generics, contract
// ---------------------------------------------------------------------------

#[test]
fn struct_decl() {
    let src = "struct Point { x: i32, y: f32 }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::StructDecl {
            qualifiers: vec![],
            name: Box::new(id("Point", 7, 12)),
            contracts: vec![],
            contents: Box::new(block(
                vec![
                    param_decl(id("x", 15, 16), ty(TypeKind::I32, 18, 21), 15, 22),
                    param_decl(id("y", 23, 24), ty(TypeKind::F32, 26, 29), 23, 31),
                ],
                13,
                31,
            )),
        },
        0,
        31,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn struct_with_contracts() {
    let src = "struct Point: HasX, HasY { x: i32 }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::StructDecl {
            qualifiers: vec![],
            name: Box::new(id("Point", 7, 12)),
            contracts: vec![
                ty(TypeKind::CustomType(Box::new(id("HasX", 14, 18))), 14, 18),
                ty(TypeKind::CustomType(Box::new(id("HasY", 20, 24))), 20, 24),
            ],
            contents: Box::new(block(
                vec![param_decl(
                    id("x", 27, 28),
                    ty(TypeKind::I32, 30, 33),
                    27,
                    35,
                )],
                25,
                35,
            )),
        },
        0,
        35,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn enum_decl() {
    let src = "enum Color: u8 { Red, Green = 5, Blue }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::EnumStmt {
            qualifiers: vec![],
            name: Box::new(id("Color", 5, 10)),
            underlying: Some(ty(TypeKind::U8, 12, 14)),
            content: vec![
                EnumMember {
                    name: id("Red", 17, 20),
                    value: None,
                    span: sp(17, 21),
                },
                EnumMember {
                    name: id("Green", 22, 27),
                    value: Some(lit(Literal::Int(5), 30, 31)),
                    span: sp(22, 32),
                },
                EnumMember {
                    name: id("Blue", 33, 37),
                    value: None,
                    span: sp(33, 39),
                },
            ],
        },
        0,
        39,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn variant_decl() {
    let src = "variant Shape { Circle(i32), Rect(i32, i32) }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::VariantStmt {
            qualifiers: vec![],
            name: Box::new(id("Shape", 8, 13)),
            contracts: vec![],
            body: vec![
                VariantMember {
                    name: id("Circle", 16, 22),
                    member_types: vec![ty(TypeKind::I32, 23, 26)],
                    span: sp(16, 28),
                },
                VariantMember {
                    name: id("Rect", 29, 33),
                    member_types: vec![ty(TypeKind::I32, 34, 37), ty(TypeKind::I32, 39, 42)],
                    span: sp(29, 45),
                },
            ],
        },
        0,
        45,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn seal_decl() {
    let src = "seal API { func a() {} func b() {} }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::SealStmt {
            qualifiers: vec![],
            name: Box::new(id("API", 5, 8)),
            contents: vec![
                st(
                    StmtKind::FunctionDef {
                        qualifiers: vec![],
                        name: Box::new(id("a", 16, 17)),
                        params: vec![],
                        type_annotation: None,
                        body: Box::new(block(vec![], 20, 22)),
                    },
                    11,
                    27,
                ),
                st(
                    StmtKind::FunctionDef {
                        qualifiers: vec![],
                        name: Box::new(id("b", 28, 29)),
                        params: vec![],
                        type_annotation: None,
                        body: Box::new(block(vec![], 32, 34)),
                    },
                    23,
                    36,
                ),
            ],
        },
        0,
        36,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn generics_block() {
    let src = "generics <T, U> { func foo() {} }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::GenericBlock {
            params: vec![
                ty(TypeKind::CustomType(Box::new(id("T", 10, 11))), 10, 11),
                ty(TypeKind::CustomType(Box::new(id("U", 13, 14))), 13, 14),
            ],
            body: Box::new(block(
                vec![st(
                    StmtKind::FunctionDef {
                        qualifiers: vec![],
                        name: Box::new(id("foo", 23, 26)),
                        params: vec![],
                        type_annotation: None,
                        body: Box::new(block(vec![], 29, 31)),
                    },
                    18,
                    33,
                )],
                16,
                33,
            )),
        },
        0,
        33,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn contract_block() {
    let src = "contract HasX { func get(): i32 }";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::ContractBlock {
            qualifiers: vec![],
            name: Box::new(id("HasX", 9, 13)),
            body: vec![st(
                StmtKind::FunctionDecl {
                    qualifiers: vec![],
                    name: Box::new(id("get", 21, 24)),
                    params: vec![],
                    type_annotation: Some(ty(TypeKind::I32, 28, 31)),
                },
                16,
                33,
            )],
        },
        0,
        33,
    );
    assert_eq!(stmts, vec![expected]);
}

#[test]
fn alias_decl() {
    let (stmts, errors, corrupted) = parse_src("alias i32 as Int");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::AliasStmt {
            original: Box::new(ty(TypeKind::I32, 6, 9)),
            new: Box::new(id("Int", 13, 16)),
        },
        0,
        16,
    );
    assert_eq!(stmts, vec![expected]);
    // `alias` takes no trailing semicolon.
    let (_, errors2, corrupted2) = parse_src("alias i32 as Int;");
    assert!(corrupted2, "expected corrupted with trailing semicolon");
    assert!(!errors2.is_empty());
}

#[test]
fn import_decl() {
    let (stmts, errors, corrupted) = parse_src("import foo::bar");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let path = Expr::new(
        ExprKind::Path(Box::new(id("foo", 7, 10)), Box::new(id("bar", 12, 15))),
        sp(7, 15),
    );
    let expected = st(
        StmtKind::ImportStmt {
            name: Box::new(path),
            alias: None,
        },
        0,
        15,
    );
    assert_eq!(stmts, vec![expected]);

    let (stmts2, errors2, corrupted2) = parse_src("import foo::bar as baz");
    assert!(!corrupted2);
    assert!(errors2.is_empty());
    let path2 = Expr::new(
        ExprKind::Path(Box::new(id("foo", 7, 10)), Box::new(id("bar", 12, 15))),
        sp(7, 18),
    );
    let expected2 = st(
        StmtKind::ImportStmt {
            name: Box::new(path2),
            alias: Some(id("baz", 19, 22)),
        },
        0,
        22,
    );
    assert_eq!(stmts2, vec![expected2]);
    // `import` takes no trailing semicolon.
    let (_, errors3, corrupted3) = parse_src("import foo::bar;");
    assert!(corrupted3);
    assert!(!errors3.is_empty());
}

// ---------------------------------------------------------------------------
// Instantiation & dollar scopes
// ---------------------------------------------------------------------------

#[test]
fn struct_instantiation() {
    let src = "var point = .Point{.x = 1, .y = 2};";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let point_ty = ty(TypeKind::CustomType(Box::new(id("Point", 13, 18))), 13, 18);
    let inst = Expr::new(
        ExprKind::Instantiation {
            init_ty: Box::new(point_ty),
            body: vec![
                InstParam {
                    name: Box::new(id("x", 20, 21)),
                    value: Box::new(lit(Literal::Int(1), 24, 25)),
                    span: sp(19, 25),
                },
                InstParam {
                    name: Box::new(id("y", 28, 29)),
                    value: Box::new(lit(Literal::Int(2), 32, 33)),
                    span: sp(27, 33),
                },
            ],
        },
        sp(12, 34),
    );
    let expected = vec![var_decl(vec![], None, id("point", 4, 9), inst, 0, 35)];
    assert_eq!(stmts, expected);
}

#[test]
fn tuple_instantiation() {
    let src = "var tup = .(1, 2, 3);";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let inst = Expr::new(
        ExprKind::TupleInst {
            body: vec![
                lit(Literal::Int(1), 12, 13),
                lit(Literal::Int(2), 15, 16),
                lit(Literal::Int(3), 18, 19),
            ],
        },
        sp(10, 20),
    );
    let expected = vec![var_decl(vec![], None, id("tup", 4, 7), inst, 0, 21)];
    assert_eq!(stmts, expected);
}

#[test]
fn tuple_instantiation_typed() {
    let src = "var tup = .(2i32, 1000i64);";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let inst = Expr::new(
        ExprKind::TupleInst {
            body: vec![
                lit(Literal::Int32(2), 12, 16),
                lit(Literal::Int64(1000), 18, 25),
            ],
        },
        sp(10, 26),
    );
    let expected = vec![var_decl(vec![], None, id("tup", 4, 7), inst, 0, 27)];
    assert_eq!(stmts, expected);
}

#[test]
fn nested_tuple_instantiation() {
    let src = "var deep = .(.(2i32, 1000i64), 42u32);";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let inner = Expr::new(
        ExprKind::TupleInst {
            body: vec![
                lit(Literal::Int32(2), 15, 19),
                lit(Literal::Int64(1000), 21, 28),
            ],
        },
        sp(13, 29),
    );
    let inst = Expr::new(
        ExprKind::TupleInst {
            body: vec![inner, lit(Literal::Uint32(42), 31, 36)],
        },
        sp(11, 37),
    );
    let expected = vec![var_decl(vec![], None, id("deep", 4, 8), inst, 0, 38)];
    assert_eq!(stmts, expected);
}

#[test]
fn dollar_scope_no_params() {
    let src = "var f = $${ return 5; };";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let scope = Expr::new(
        ExprKind::DollarScope {
            params: vec![],
            body: Box::new(block(
                vec![ret(Some(lit(Literal::Int(5), 19, 20)), 12, 23)],
                10,
                23,
            )),
        },
        sp(8, 24),
    );
    let expected = vec![var_decl(vec![], None, id("f", 4, 5), scope, 0, 24)];
    assert_eq!(stmts, expected);
}

#[test]
fn dollar_scope_with_params_expr() {
    let src = "var g = $$|a, b|{ a + b; };";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let scope = Expr::new(
        ExprKind::DollarScope {
            params: vec![id("a", 11, 12), id("b", 14, 15)],
            body: Box::new(block(
                vec![st(
                    StmtKind::Expr(bin(id("a", 18, 19), BinaryOp::Add, id("b", 22, 23), 18, 23)),
                    18,
                    23,
                )],
                16,
                26,
            )),
        },
        sp(8, 27),
    );
    let expected = vec![var_decl(vec![], None, id("g", 4, 5), scope, 0, 27)];
    assert_eq!(stmts, expected);
}

#[test]
fn dollar_scope_with_params_return() {
    let src = "var h = $$|a, b|{ return a; };";
    let (stmts, errors, corrupted) = parse_src(src);
    assert!(!corrupted);
    assert!(errors.is_empty());
    let scope = Expr::new(
        ExprKind::DollarScope {
            params: vec![id("a", 11, 12), id("b", 14, 15)],
            body: Box::new(block(vec![ret(Some(id("a", 25, 26)), 18, 29)], 16, 29)),
        },
        sp(8, 30),
    );
    let expected = vec![var_decl(vec![], None, id("h", 4, 5), scope, 0, 30)];
    assert_eq!(stmts, expected);
}

// ---------------------------------------------------------------------------
// Qualifiers
// ---------------------------------------------------------------------------

#[test]
fn var_qualifiers() {
    let (stmts, errors, corrupted) = parse_src("mut var x = 1;");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = vec![var_decl(
        vec![qualifier(QualifierKind::Mut, 0, 3)],
        None,
        id("x", 8, 9),
        lit(Literal::Int(1), 12, 13),
        4,
        14,
    )];
    assert_eq!(stmts, expected);

    let (stmts2, errors2, corrupted2) = parse_src("const var x = 1;");
    assert!(!corrupted2);
    assert!(errors2.is_empty());
    let expected2 = vec![var_decl(
        vec![qualifier(QualifierKind::Const, 0, 5)],
        None,
        id("x", 10, 11),
        lit(Literal::Int(1), 14, 15),
        6,
        16,
    )];
    assert_eq!(stmts2, expected2);

    let (stmts3, errors3, corrupted3) = parse_src("$ var x = 1;");
    assert!(!corrupted3);
    assert!(errors3.is_empty());
    let expected3 = vec![var_decl(
        vec![qualifier(QualifierKind::DollarRead, 0, 1)],
        None,
        id("x", 6, 7),
        lit(Literal::Int(1), 10, 11),
        2,
        12,
    )];
    assert_eq!(stmts3, expected3);
}

#[test]
fn expose_qualifier_on_function() {
    let (stmts, errors, corrupted) = parse_src("expose func foo() {}");
    assert!(!corrupted);
    assert!(errors.is_empty());
    let expected = st(
        StmtKind::FunctionDef {
            qualifiers: vec![qualifier(QualifierKind::Exposed, 0, 6)],
            name: Box::new(id("foo", 12, 15)),
            params: vec![],
            type_annotation: None,
            body: Box::new(block(vec![], 18, 20)),
        },
        7,
        20,
    );
    assert_eq!(stmts, vec![expected]);
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

fn assert_single_error(errors: &[CompilerError], msg: &str, span: Option<Span>) {
    assert_eq!(errors.len(), 1, "expected 1 error, got {:#?}", errors);
    assert_eq!(errors[0].message, msg);
    assert_eq!(errors[0].span, span);
    assert!(matches!(errors[0].phase, Phase::Parser));
}

#[test]
fn missing_semicolon() {
    let (_, errors, corrupted) = parse_src("var x = 42");
    assert!(corrupted);
    assert_single_error(&errors, "Expected Semicolon, found End", Some(sp(10, 10)));
}

#[test]
fn dangling_binary_operator() {
    let (_, errors, corrupted) = parse_src("var x = 1 + ;");
    assert!(corrupted);
    assert_single_error(
        &errors,
        "Unexpected prefix token: Semicolon",
        Some(sp(12, 13)),
    );
}

#[test]
fn bind_operator_rejected() {
    // The `:=` bind operator is gone; `x = 1;` is an ordinary assignment and
    // parses, but `x := 1;` must still be a parse error.
    let (stmts, errors, corrupted) = parse_src("x = 1;");
    assert!(!corrupted, "{:?}", errors);
    assert!(errors.is_empty());
    assert_eq!(stmts.len(), 1);

    let (_, errors, corrupted) = parse_src("x := 1;");
    assert!(corrupted);
    assert_eq!(errors.len(), 1, "errors: {:#?}", errors);
    assert_eq!(errors[0].message, "Expected Semicolon, found Colon");
    assert_eq!(errors[0].span, Some(sp(2, 3)));
    assert!(matches!(errors[0].phase, Phase::Parser));
}

#[test]
fn break_continue_trailing_semicolon_optional() {
    let (stmts, errors, corrupted) = parse_src("break;");
    assert!(!corrupted, "{:?}", errors);
    assert!(errors.is_empty(), "{:?}", errors);
    assert_eq!(stmts, vec![st(StmtKind::Break, 0, 5)]);

    let (stmts2, errors2, corrupted2) = parse_src("continue;");
    assert!(!corrupted2, "{:?}", errors2);
    assert!(errors2.is_empty(), "{:?}", errors2);
    assert_eq!(stmts2, vec![st(StmtKind::Continue, 0, 8)]);

    let (stmts3, errors3, corrupted3) = parse_src("break\nx = 1;");
    assert!(!corrupted3, "{:?}", errors3);
    assert!(errors3.is_empty(), "{:?}", errors3);
    assert_eq!(
        stmts3,
        vec![
            st(StmtKind::Break, 0, 5),
            st(
                StmtKind::Expr(bin(
                    id("x", 6, 7),
                    BinaryOp::Assign,
                    lit(Literal::Int(1), 10, 11),
                    6,
                    11
                )),
                6,
                11,
            ),
        ]
    );
}

#[test]
fn amp_bitwise_operator_not_supported() {
    // `&` does not lex as a binary operator; bitwise-and is the `and` keyword.
    let (_, errors, corrupted) = parse_src("var x = 1 shl 2 & 3;");
    assert!(corrupted);
    assert_single_error(
        &errors,
        "Expected Semicolon, found Ampersand",
        Some(sp(16, 17)),
    );
}

#[test]
fn lt_gt_are_not_shift_operators() {
    // `<<` / `>>` do not form shift tokens; only `shl`/`shr` are shifts.
    let (_, errors, corrupted) = parse_src("x = 1 << 2;");
    assert!(corrupted);
    assert!(!errors.is_empty());
    assert!(errors[0].message.contains("Lt"));
}

#[test]
fn mod_keyword_not_a_operator() {
    let (_, errors, corrupted) = parse_src("var x = 5 mod 2;");
    assert!(corrupted);
    assert_eq!(errors.len(), 1, "errors: {:#?}", errors);
    assert_eq!(errors[0].message, "Expected Semicolon, found Identifier");
    assert!(matches!(errors[0].phase, Phase::Parser));
}

#[test]
fn turbofish_requires_double_colon() {
    let (_, errors, corrupted) = parse_src("var x = Pair<i32, u32>::new();");
    assert!(corrupted);
    assert_single_error(&errors, "Unexpected prefix token: I32Key", Some(sp(13, 16)));
}

#[test]
fn unwrap_takes_expression_not_type() {
    let (_, errors, corrupted) = parse_src("var x = unwrap[Option<i32>](o);");
    assert!(corrupted);
    assert_single_error(&errors, "Unexpected prefix token: I32Key", Some(sp(22, 25)));
}

#[test]
fn function_as_expression_not_supported() {
    let (_, errors, corrupted) = parse_src("var f = func(a: i32): i32 { return a; };");
    assert!(corrupted);
    assert_eq!(errors.len(), 2, "errors: {:#?}", errors);
    assert_eq!(errors[0].message, "Unexpected prefix token: Func");
    assert_eq!(errors[0].span, Some(sp(8, 12)));
    assert_eq!(errors[1].message, "Unexpected prefix token: Rbrace");
    assert_eq!(errors[1].span, Some(sp(38, 39)));
}

#[test]
fn standalone_instantiation_needs_semicolon() {
    let (_, errors, corrupted) = parse_src(".Point{.x = 1, .y = 2}");
    assert!(corrupted);
    assert_single_error(&errors, "Expected Semicolon, found End", Some(sp(22, 22)));
}

#[test]
fn standalone_dollar_scope_needs_semicolon() {
    let (_, errors, corrupted) = parse_src("$${ return 5; }");
    assert!(corrupted);
    assert_single_error(&errors, "Expected Semicolon, found End", Some(sp(15, 15)));
}

#[test]
fn contract_functions_take_no_semicolon() {
    let (_, errors, corrupted) = parse_src("contract HasX { func get(): i32; }");
    assert!(corrupted);
    assert_eq!(errors.len(), 2, "errors: {:#?}", errors);
    assert_eq!(errors[0].message, "Expected Func, found Semicolon");
    assert_eq!(errors[0].span, Some(sp(31, 32)));
    assert_eq!(errors[1].message, "Expected Rbrace, found End");
    assert_eq!(errors[1].span, Some(sp(34, 34)));
}

#[test]
fn propagate_prefix_index_not_supported() {
    // `!?` is a postfix propagate operator; as a prefix it is an error.
    let (_, errors, corrupted) = parse_src("x = !? a;");
    assert!(corrupted);
    assert_single_error(
        &errors,
        "Unexpected prefix token: Propagate",
        Some(sp(4, 6)),
    );
}

#[test]
fn func_decl_missing_param_name() {
    let (_, errors, corrupted) = parse_src("func foo( {");
    assert!(corrupted);
    assert_single_error(
        &errors,
        "Expected Identifier, found LBrace",
        Some(sp(10, 11)),
    );
}

#[test]
fn nested_plain_parens_not_a_tuple_literal() {
    // Tuple literals must use `.(...)` at every nesting level.
    let (_, errors, corrupted) = parse_src("var deep = .(.((2i32, 1000i64), 42u32), 99i32);");
    assert!(corrupted);
    assert_single_error(&errors, "Expected Rparen, found Comma", Some(sp(20, 21)));
}

#[test]
fn impl_rejects_a_bodyless_signature() {
    // A contract-style declaration inside an impl would satisfy the contract
    // verifier by name lookup while doing nothing, so it is refused at parse
    // time rather than silently accepted.
    let (_, errors, corrupted) =
        parse_src("struct Point { x: i32 }\nimpl Point { func sum(self: Point): i32 }\n");
    assert!(corrupted, "a bodyless impl member must corrupt the parse");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("Only function definitions are allowed in an impl block")),
        "expected the impl-member diagnostic, got {:?}",
        errors.iter().map(|e| &e.message).collect::<Vec<_>>()
    );
}

#[test]
fn impl_block_parses_with_multiple_members() {
    let (stmts, errors, corrupted) = parse_src(
        "struct Point { x: i32, y: i32 }\n\
         impl Point {\n  func sum(self: Point): i32 { return self.x + self.y; }\n  \
         func scaled(self: Point, k: i32): i32 { return (self.x + self.y) * k; }\n}\n",
    );
    assert!(!corrupted, "unexpected errors: {:?}", errors);
    let impls: Vec<&StmtKind> = stmts
        .iter()
        .map(|s| &s.kind)
        .filter(|k| matches!(k, StmtKind::ImplBlock { .. }))
        .collect();
    assert_eq!(impls.len(), 1, "expected exactly one impl block");
    if let StmtKind::ImplBlock { body, .. } = impls[0] {
        assert_eq!(body.len(), 2, "expected two members");
    }
}
