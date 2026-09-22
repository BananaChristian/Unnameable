use std::{cell::RefCell, rc::Rc};

use unnc::diagnostics::{Diagnostics, Span};
use unnc::hir::{
    Conv, HirBinaryOp, HirEnumMember, HirExpr, HirExprKind, HirInstParam, HirLiteral, HirMatchArm,
    HirParam, HirPattern, HirPostfixOp, HirStmt, HirStmtKind, HirType, HirTypeNode, HirUnaryOp,
    HirVariantMember,
};
use unnc::lexer::Lexer;
use unnc::lowering::{Lowering, NodeId};
use unnc::parser::Parser;

fn parse_lower(src: &str) -> Vec<HirStmt> {
    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "test.unn".to_string(),
        src.to_string(),
    )));
    let mut lexer = Lexer::new(src, diagnostics.clone());
    let tokens = lexer.tokenize();
    assert!(!lexer.corrupted, "lexer corrupted for:\n{src}");
    let mut parser = Parser::new(tokens, diagnostics.clone());
    let ast = parser.parse();
    assert!(
        !parser.corrupted,
        "parser corrupted for:\n{src}\nerrors: {:?}",
        diagnostics.borrow().errors
    );
    let mut lowering = Lowering::new(ast, diagnostics.clone());
    let hir = lowering.lower();
    assert!(!lowering.corrupted, "lowering corrupted for:\n{src}");
    hir
}

fn parse_lower_norm(src: &str) -> Vec<HirStmt> {
    parse_lower(src).into_iter().map(norm_stmt).collect()
}

fn zid() -> NodeId {
    NodeId {
        local: 0,
        external: 0,
    }
}

fn stmt(kind: HirStmtKind, span: Span) -> HirStmt {
    HirStmt {
        hir_id: zid(),
        kind,
        span,
    }
}

fn expr(kind: HirExprKind, span: Span) -> HirExpr {
    HirExpr {
        hir_id: zid(),
        kind,
        span,
    }
}

fn lit(lit: HirLiteral, span: Span) -> HirExpr {
    expr(HirExprKind::Literal(lit), span)
}

fn ident(name: &str, span: Span) -> HirExpr {
    expr(HirExprKind::Identifier(name.to_string()), span)
}

fn bin(l: HirExpr, op: HirBinaryOp, r: HirExpr, span: Span) -> HirExpr {
    expr(HirExprKind::Binary(Box::new(l), op, Box::new(r)), span)
}

fn call(callee: HirExpr, args: Vec<HirExpr>, span: Span) -> HirExpr {
    expr(HirExprKind::Call(Box::new(callee), args), span)
}

fn tn(kind: HirType, span: Span) -> HirTypeNode {
    HirTypeNode {
        hir_id: zid(),
        kind,
        span,
    }
}

fn var_decl(
    name: &str,
    mutable: bool,
    constant: bool,
    dollar_read: bool,
    exposed: bool,
    ty: Option<HirTypeNode>,
    init: HirExpr,
    span: Span,
) -> HirStmt {
    stmt(
        HirStmtKind::HirVarDecl {
            name: name.to_string(),
            mutable,
            constant,
            dollar_read,
            exposed,
            ty,
            init: Box::new(init),
        },
        span,
    )
}

fn expr_stmt(e: HirExpr, span: Span) -> HirStmt {
    stmt(HirStmtKind::HirExpr(Box::new(e)), span)
}

fn param(
    name: &str,
    ty: HirTypeNode,
    mutable: bool,
    dollar_read: bool,
    default: Option<HirExpr>,
    span: Span,
) -> HirParam {
    HirParam {
        hir_id: zid(),
        name: name.to_string(),
        ty,
        mutable,
        dollar_read,
        default,
        span,
    }
}

fn func_def(
    name: &str,
    params: Vec<HirParam>,
    return_type: HirTypeNode,
    generic_type_params: Vec<HirTypeNode>,
    exposed: bool,
    conv: Option<Conv>,
    dollar_read: bool,
    body: Vec<HirStmt>,
    span: Span,
) -> HirStmt {
    stmt(
        HirStmtKind::HirFunctionDef {
            name: name.to_string(),
            params,
            return_type,
            generic_type_params,
            exposed,
            dollar_read,
            body,
            conv,
        },
        span,
    )
}

fn norm_stmt(mut s: HirStmt) -> HirStmt {
    s.hir_id = zid();
    s.kind = match s.kind {
        HirStmtKind::HirReturn(Some(inner)) => {
            HirStmtKind::HirReturn(Some(Box::new(norm_expr(*inner))))
        }
        HirStmtKind::HirReturn(None) => HirStmtKind::HirReturn(None),
        HirStmtKind::HirBreak => HirStmtKind::HirBreak,
        HirStmtKind::HirContinue => HirStmtKind::HirContinue,
        HirStmtKind::HirExpr(e) => HirStmtKind::HirExpr(Box::new(norm_expr(*e))),
        HirStmtKind::HirVarDecl {
            name,
            mutable,
            constant,
            dollar_read,
            exposed,
            ty,
            init,
        } => HirStmtKind::HirVarDecl {
            name,
            mutable,
            constant,
            dollar_read,
            exposed,
            ty: ty.map(norm_ty),
            init: Box::new(norm_expr(*init)),
        },
        HirStmtKind::HirFunctionDef {
            name,
            params,
            return_type,
            generic_type_params,
            exposed,
            dollar_read,
            body,
            conv,
        } => HirStmtKind::HirFunctionDef {
            name,
            params: params.into_iter().map(norm_param).collect(),
            return_type: norm_ty(return_type),
            generic_type_params: generic_type_params.into_iter().map(norm_ty).collect(),
            exposed,
            conv,
            dollar_read,
            body: body.into_iter().map(norm_stmt).collect(),
        },
        HirStmtKind::HirFunctionDecl {
            name,
            params,
            return_type,
            generic_type_params,
            exposed,
            conv,
        } => HirStmtKind::HirFunctionDecl {
            name,
            params: params.into_iter().map(norm_param).collect(),
            return_type: norm_ty(return_type),
            generic_type_params: generic_type_params.into_iter().map(norm_ty).collect(),
            exposed,
            conv,
        },
        HirStmtKind::HirStructDecl {
            name,
            contracts,
            generic_type_params,
            fields,
            exposed,
        } => HirStmtKind::HirStructDecl {
            name,
            contracts: contracts.into_iter().map(norm_ty).collect(),
            generic_type_params: generic_type_params.into_iter().map(norm_ty).collect(),
            fields: fields.into_iter().map(norm_param).collect(),
            exposed,
        },
        HirStmtKind::HirIf {
            condition,
            body,
            else_body,
        } => HirStmtKind::HirIf {
            condition: Box::new(norm_expr(*condition)),
            body: body.into_iter().map(norm_stmt).collect(),
            else_body: else_body.map(|b| b.into_iter().map(norm_stmt).collect()),
        },
        HirStmtKind::HirContractDecl {
            name,
            functions,
            generic_type_params,
            exposed,
        } => HirStmtKind::HirContractDecl {
            name,
            functions: functions.into_iter().map(norm_stmt).collect(),
            generic_type_params: generic_type_params.into_iter().map(norm_ty).collect(),
            exposed,
        },
        HirStmtKind::HirAlias { original, alias } => HirStmtKind::HirAlias {
            original: Box::new(norm_ty(*original)),
            alias,
        },
        HirStmtKind::HirImport { name, alias } => HirStmtKind::HirImport { name, alias },
        HirStmtKind::HirWhile { condition, body } => HirStmtKind::HirWhile {
            condition: Box::new(norm_expr(*condition)),
            body: body.into_iter().map(norm_stmt).collect(),
        },
        HirStmtKind::HirEnumDecl {
            name,
            underlying,
            members,
            exposed,
        } => HirStmtKind::HirEnumDecl {
            name,
            underlying: norm_ty(underlying),
            members: members.into_iter().map(norm_enum_member).collect(),
            exposed,
        },
        HirStmtKind::HirVariantDecl {
            name,
            contracts,
            members,
            generic_type_params,
            exposed,
        } => HirStmtKind::HirVariantDecl {
            name,
            contracts: contracts.into_iter().map(norm_ty).collect(),
            members: members.into_iter().map(norm_variant_member).collect(),
            generic_type_params: generic_type_params.into_iter().map(norm_ty).collect(),
            exposed,
        },
    };
    s
}

fn norm_expr(mut e: HirExpr) -> HirExpr {
    e.hir_id = zid();
    e.kind = match e.kind {
        HirExprKind::Literal(HirLiteral::ArrayLiteral(items)) => HirExprKind::Literal(
            HirLiteral::ArrayLiteral(items.into_iter().map(norm_expr).collect()),
        ),
        HirExprKind::Literal(l) => HirExprKind::Literal(l),
        HirExprKind::Identifier(name) => HirExprKind::Identifier(name),
        HirExprKind::Binary(l, op, r) => {
            HirExprKind::Binary(Box::new(norm_expr(*l)), op, Box::new(norm_expr(*r)))
        }
        HirExprKind::Unary(op, inner) => HirExprKind::Unary(op, Box::new(norm_expr(*inner))),
        HirExprKind::GenericInstantion { name, type_params } => HirExprKind::GenericInstantion {
            name,
            type_params: type_params.into_iter().map(norm_ty).collect(),
        },
        HirExprKind::Call(callee, args) => HirExprKind::Call(
            Box::new(norm_expr(*callee)),
            args.into_iter().map(norm_expr).collect(),
        ),
        HirExprKind::Unwrap(inner) => HirExprKind::Unwrap(Box::new(norm_expr(*inner))),
        HirExprKind::Postfix(inner, op) => HirExprKind::Postfix(Box::new(norm_expr(*inner)), op),
        HirExprKind::SizeOf(t) => HirExprKind::SizeOf(norm_ty(t)),
        HirExprKind::StaticCast(t, inner) => {
            HirExprKind::StaticCast(Box::new(norm_ty(*t)), Box::new(norm_expr(*inner)))
        }
        HirExprKind::BitCast(t, inner) => {
            HirExprKind::BitCast(Box::new(norm_ty(*t)), Box::new(norm_expr(*inner)))
        }
        HirExprKind::Instantiation { init_ty, body } => HirExprKind::Instantiation {
            init_ty: Box::new(norm_ty(*init_ty)),
            body: body.into_iter().map(norm_inst_param).collect(),
        },
        HirExprKind::TupleInst { body } => HirExprKind::TupleInst {
            body: body.into_iter().map(norm_expr).collect(),
        },
        HirExprKind::DollarScope {
            params,
            body,
            result,
        } => HirExprKind::DollarScope {
            params: params.into_iter().map(norm_expr).collect(),
            body: body.into_iter().map(norm_stmt).collect(),
            result: result.map(|r| Box::new(norm_expr(*r))),
        },
        HirExprKind::Index { target, index } => HirExprKind::Index {
            target: Box::new(norm_expr(*target)),
            index: Box::new(norm_expr(*index)),
        },
        HirExprKind::Match { scrutinee, arms } => HirExprKind::Match {
            scrutinee: Box::new(norm_expr(*scrutinee)),
            arms: arms.into_iter().map(norm_match_arm).collect(),
        },
        HirExprKind::Block(stmts) => HirExprKind::Block(stmts.into_iter().map(norm_stmt).collect()),
    };
    e
}

fn norm_match_arm(mut arm: HirMatchArm) -> HirMatchArm {
    arm.pattern = norm_pattern(arm.pattern);
    arm.guard = arm.guard.map(norm_expr);
    arm.body = Box::new(norm_expr(*arm.body));
    arm
}

fn norm_pattern(p: HirPattern) -> HirPattern {
    match p {
        HirPattern::Wildcard => HirPattern::Wildcard,
        HirPattern::Literal(expr) => HirPattern::Literal(Box::new(norm_expr(*expr))),
        HirPattern::Path {
            type_name,
            member,
            payloads,
            span,
        } => HirPattern::Path {
            type_name,
            member,
            payloads: payloads.into_iter().map(norm_pattern).collect(),
            span,
        },
        HirPattern::Binding { name, span, .. } => {
            let hir_id = zid();
            HirPattern::Binding { name, hir_id, span }
        }
        HirPattern::Tuple { elements, span } => HirPattern::Tuple {
            elements: elements.into_iter().map(norm_pattern).collect(),
            span,
        },
        HirPattern::StructPattern {
            type_name,
            fields,
            rest,
            span,
        } => HirPattern::StructPattern {
            type_name,
            fields: fields
                .into_iter()
                .map(|mut field| {
                    field.hir_id = zid();
                    field.pattern = norm_pattern(field.pattern);
                    field
                })
                .collect(),
            rest,
            span,
        },
        HirPattern::Or(alts) => HirPattern::Or(alts.into_iter().map(norm_pattern).collect()),
    }
}

fn norm_inst_param(mut p: HirInstParam) -> HirInstParam {
    p.hir_id = zid();
    p.value = Box::new(norm_expr(*p.value));
    p
}

fn norm_param(mut p: HirParam) -> HirParam {
    p.hir_id = zid();
    p.ty = norm_ty(p.ty);
    p.default = p.default.map(norm_expr);
    p
}

fn norm_enum_member(mut m: HirEnumMember) -> HirEnumMember {
    m.hir_id = zid();
    m
}

fn norm_variant_member(mut m: HirVariantMember) -> HirVariantMember {
    m.hir_id = zid();
    m.member_types = m.member_types.into_iter().map(norm_ty).collect();
    m
}

fn norm_ty(mut t: HirTypeNode) -> HirTypeNode {
    t.hir_id = zid();
    t.kind = match t.kind {
        HirType::Ptr(inner) => HirType::Ptr(Box::new(norm_ty(*inner))),
        HirType::Ref(inner) => HirType::Ref(Box::new(norm_ty(*inner))),
        HirType::Array(inner, size) => HirType::Array(Box::new(norm_ty(*inner)), size),
        HirType::Func(params, ret) => HirType::Func(
            params.into_iter().map(norm_ty).collect(),
            Box::new(norm_ty(*ret)),
        ),
        HirType::CustomType(name) => HirType::CustomType(name),
        HirType::GenericPlaceHolder(name) => HirType::GenericPlaceHolder(name),
        HirType::GenericType { name, type_params } => HirType::GenericType {
            name,
            type_params: type_params.into_iter().map(norm_ty).collect(),
        },
        HirType::Nullable(inner) => HirType::Nullable(Box::new(norm_ty(*inner))),
        HirType::Failable(ok, err) => {
            HirType::Failable(Box::new(norm_ty(*ok)), Box::new(norm_ty(*err)))
        }
        HirType::Tuple(elems) => HirType::Tuple(elems.into_iter().map(norm_ty).collect()),
        simple => simple,
    };
    t
}

fn span(start: usize, end: usize) -> Span {
    Span { start, end }
}

// var declarations & qualifiers
#[test]
fn var_plain_lowers_with_deterministic_node_ids() {
    let hir = parse_lower("var x := 1;");
    assert_eq!(
        hir[0].hir_id,
        NodeId {
            local: 1,
            external: 0
        }
    );
    assert_eq!(
        hir[0].kind,
        HirStmtKind::HirVarDecl {
            name: "x".to_string(),
            mutable: false,
            constant: false,
            dollar_read: false,
            exposed: false,
            ty: None,
            init: Box::new(HirExpr {
                hir_id: NodeId {
                    local: 0,
                    external: 0
                },
                kind: HirExprKind::Literal(HirLiteral::Int(1)),
                span: span(9, 10),
            }),
        }
    );
}

#[test]
fn var_with_type() {
    let hir = parse_lower_norm("var i32 y := 5;");
    assert_eq!(
        hir,
        vec![var_decl(
            "y",
            false,
            false,
            false,
            false,
            Some(tn(HirType::I32, span(4, 7))),
            lit(HirLiteral::Int(5), span(13, 14)),
            span(0, 15),
        )]
    );
}

#[test]
fn var_mut() {
    let hir = parse_lower_norm("mut var u8 z := 3;");
    assert_eq!(
        hir,
        vec![var_decl(
            "z",
            true,
            false,
            false,
            false,
            Some(tn(HirType::U8, span(8, 10))),
            lit(HirLiteral::Int(3), span(16, 17)),
            span(4, 18),
        )]
    );
}

#[test]
fn var_const_expose() {
    let hir = parse_lower_norm("expose const var bool ok := true;");
    assert_eq!(
        hir,
        vec![var_decl(
            "ok",
            false,
            true,
            false,
            true,
            Some(tn(HirType::Bool, span(17, 21))),
            lit(HirLiteral::Bool(true), span(28, 32)),
            span(13, 33),
        )]
    );
}

#[test]
fn var_dollar_read() {
    let hir = parse_lower_norm("$ var f32 w := 1.5;");
    assert_eq!(
        hir,
        vec![var_decl(
            "w",
            false,
            false,
            true,
            false,
            Some(tn(HirType::F32, span(6, 9))),
            lit(HirLiteral::Float(1.5), span(15, 18)),
            span(2, 19),
        )]
    );
}

#[test]
fn literal_types() {
    let hir = parse_lower_norm("var a := 1.5f32; var b := 5i32; var c := 0u8; var d := \"hi\";");
    assert_eq!(
        hir,
        vec![
            var_decl(
                "a",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::F32(1.5), span(9, 15)),
                span(0, 20),
            ),
            var_decl(
                "b",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Int32(5), span(26, 30)),
                span(17, 35),
            ),
            var_decl(
                "c",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Uint8(0), span(41, 44)),
                span(32, 49),
            ),
            var_decl(
                "d",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Str("hi".to_string()), span(55, 59)),
                span(46, 60),
            ),
        ]
    );
}

#[test]
fn char_literals() {
    let hir = parse_lower_norm("var g := 'h'c8; var h := 'h'c16; var i := 'h'c32;");
    assert_eq!(
        hir,
        vec![
            var_decl(
                "g",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Char8(104), span(9, 14)),
                span(0, 19),
            ),
            var_decl(
                "h",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Char16(104), span(25, 31)),
                span(16, 36),
            ),
            var_decl(
                "i",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Char32('h'), span(42, 48)),
                span(33, 49),
            ),
        ]
    );
}

#[test]
fn radix_literals() {
    let hir = parse_lower_norm("var r := 0x1F; var s := 0b101; var o := 0o17;");
    assert_eq!(
        hir,
        vec![
            var_decl(
                "r",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Int(31), span(9, 13)),
                span(0, 18),
            ),
            var_decl(
                "s",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Int(5), span(24, 29)),
                span(15, 34),
            ),
            var_decl(
                "o",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Int(15), span(40, 44)),
                span(31, 45),
            ),
        ]
    );
}

#[test]
fn string_literal_is_not_interpolated_yet() {
    let hir = parse_lower_norm("var t := \"hi {name}\";");
    assert_eq!(
        hir,
        vec![var_decl(
            "t",
            false,
            false,
            false,
            false,
            None,
            lit(HirLiteral::Str("hi {name}".to_string()), span(9, 20)),
            span(0, 21),
        )]
    );
}

// binary / unary / postfix expressions
#[test]
fn binary_arith_precedence() {
    let hir = parse_lower_norm("var x := 1 + 2 * 3;");
    let expected_init = bin(
        lit(HirLiteral::Int(1), span(9, 10)),
        HirBinaryOp::Add,
        bin(
            lit(HirLiteral::Int(2), span(13, 14)),
            HirBinaryOp::Mul,
            lit(HirLiteral::Int(3), span(17, 18)),
            span(13, 18),
        ),
        span(9, 18),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "x",
            false,
            false,
            false,
            false,
            None,
            expected_init,
            span(0, 19)
        )]
    );
}

#[test]
fn keyword_and_or_are_binary_ops() {
    let hir = parse_lower_norm("var p := a and b == c or d;");
    let expected_init = bin(
        bin(
            ident("a", span(9, 10)),
            HirBinaryOp::BitAnd,
            ident("b", span(15, 16)),
            span(9, 16),
        ),
        HirBinaryOp::Eq,
        bin(
            ident("c", span(20, 21)),
            HirBinaryOp::BitOr,
            ident("d", span(25, 26)),
            span(20, 26),
        ),
        span(9, 26),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "p",
            false,
            false,
            false,
            false,
            None,
            expected_init,
            span(0, 27)
        )]
    );
}

#[test]
fn shift_operators() {
    let hir = parse_lower_norm("var s := 1 shl 2; var t := 5 shr 1;");
    assert_eq!(
        hir,
        vec![
            var_decl(
                "s",
                false,
                false,
                false,
                false,
                None,
                bin(
                    lit(HirLiteral::Int(1), span(9, 10)),
                    HirBinaryOp::Shl,
                    lit(HirLiteral::Int(2), span(15, 16)),
                    span(9, 16),
                ),
                span(0, 21),
            ),
            var_decl(
                "t",
                false,
                false,
                false,
                false,
                None,
                bin(
                    lit(HirLiteral::Int(5), span(27, 28)),
                    HirBinaryOp::Shr,
                    lit(HirLiteral::Int(1), span(33, 34)),
                    span(27, 34),
                ),
                span(18, 35),
            ),
        ]
    );
}

#[test]
fn coalesce_operator() {
    let hir = parse_lower_norm("var u := a ?? b;");
    assert_eq!(
        hir,
        vec![var_decl(
            "u",
            false,
            false,
            false,
            false,
            None,
            bin(
                ident("a", span(9, 10)),
                HirBinaryOp::Coalesce,
                ident("b", span(14, 15)),
                span(9, 15)
            ),
            span(0, 16),
        )]
    );
}

#[test]
fn xor_operator() {
    let hir = parse_lower_norm("var v := p xor q;");
    assert_eq!(
        hir,
        vec![var_decl(
            "v",
            false,
            false,
            false,
            false,
            None,
            bin(
                ident("p", span(9, 10)),
                HirBinaryOp::Xor,
                ident("q", span(15, 16)),
                span(9, 16)
            ),
            span(0, 17),
        )]
    );
}

#[test]
fn unary_operators() {
    let hir = parse_lower_norm("var n := -5; var m := not ok; var a2 := @p; var pd := ^q;");
    assert_eq!(
        hir,
        vec![
            var_decl(
                "n",
                false,
                false,
                false,
                false,
                None,
                expr(
                    HirExprKind::Unary(
                        HirUnaryOp::Neg,
                        Box::new(lit(HirLiteral::Int(5), span(10, 11)))
                    ),
                    span(9, 11),
                ),
                span(0, 16),
            ),
            var_decl(
                "m",
                false,
                false,
                false,
                false,
                None,
                expr(
                    HirExprKind::Unary(HirUnaryOp::BitNot, Box::new(ident("ok", span(26, 28)))),
                    span(22, 28),
                ),
                span(13, 33),
            ),
            var_decl(
                "a2",
                false,
                false,
                false,
                false,
                None,
                expr(
                    HirExprKind::Unary(HirUnaryOp::AddressOf, Box::new(ident("p", span(41, 42)))),
                    span(40, 42),
                ),
                span(30, 47),
            ),
            var_decl(
                "pd",
                false,
                false,
                false,
                false,
                None,
                expr(
                    HirExprKind::Unary(HirUnaryOp::Dereference, Box::new(ident("q", span(55, 56)))),
                    span(54, 56),
                ),
                span(44, 57),
            ),
        ]
    );
}

#[test]
fn postfix_operators() {
    let hir = parse_lower_norm("p++; q--; z!?;");
    let mk = |inner: HirExpr, op: HirPostfixOp, e: Span, s: Span| {
        expr_stmt(expr(HirExprKind::Postfix(Box::new(inner), op), e), s)
    };
    assert_eq!(
        hir,
        vec![
            mk(
                ident("p", span(0, 1)),
                HirPostfixOp::Increment,
                span(0, 3),
                span(0, 3),
            ),
            mk(
                ident("q", span(5, 6)),
                HirPostfixOp::Decrement,
                span(5, 8),
                span(5, 8),
            ),
            mk(
                ident("z", span(10, 11)),
                HirPostfixOp::Propagate,
                span(10, 13),
                span(10, 13),
            ),
        ]
    );
}

#[test]
fn assignment_operators() {
    let hir = parse_lower_norm("a = 5; b += 1; c *= 2;");
    assert_eq!(
        hir,
        vec![
            expr_stmt(
                bin(
                    ident("a", span(0, 1)),
                    HirBinaryOp::Assign,
                    lit(HirLiteral::Int(5), span(4, 5)),
                    span(0, 5)
                ),
                span(0, 5),
            ),
            expr_stmt(
                bin(
                    ident("b", span(7, 8)),
                    HirBinaryOp::AddAssign,
                    lit(HirLiteral::Int(1), span(12, 13)),
                    span(7, 13),
                ),
                span(7, 13),
            ),
            expr_stmt(
                bin(
                    ident("c", span(15, 16)),
                    HirBinaryOp::MulAssign,
                    lit(HirLiteral::Int(2), span(20, 21)),
                    span(15, 21),
                ),
                span(15, 21),
            ),
        ]
    );
}

#[test]
fn field_access() {
    let hir = parse_lower_norm("var c := obj.field;");
    assert_eq!(
        hir,
        vec![var_decl(
            "c",
            false,
            false,
            false,
            false,
            None,
            bin(
                ident("obj", span(9, 12)),
                HirBinaryOp::Access,
                ident("field", span(13, 18)),
                span(9, 18)
            ),
            span(0, 19),
        )]
    );
}

#[test]
fn method_call_lowers_as_access_plus_call() {
    let hir = parse_lower_norm("var r := obj.method();");
    let expected_init = bin(
        ident("obj", span(9, 12)),
        HirBinaryOp::Access,
        call(ident("method", span(13, 19)), vec![], span(13, 22)),
        span(9, 22),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "r",
            false,
            false,
            false,
            false,
            None,
            expected_init,
            span(0, 22)
        )]
    );
}

#[test]
fn tuple_member_access() {
    let hir = parse_lower_norm("var t := pair.0; var t2 := pair.1;");
    assert_eq!(
        hir,
        vec![
            var_decl(
                "t",
                false,
                false,
                false,
                false,
                None,
                bin(
                    ident("pair", span(9, 13)),
                    HirBinaryOp::Access,
                    lit(HirLiteral::Int(0), span(14, 15)),
                    span(9, 15)
                ),
                span(0, 20),
            ),
            var_decl(
                "t2",
                false,
                false,
                false,
                false,
                None,
                bin(
                    ident("pair", span(27, 31)),
                    HirBinaryOp::Access,
                    lit(HirLiteral::Int(1), span(32, 33)),
                    span(27, 33)
                ),
                span(17, 34),
            ),
        ]
    );
}

#[test]
fn nested_tuple_member_access_spans_each_index_token() {
    // .0 spans (17,18) and .1 spans (19,20) — each index digit its own token.
    let hir = parse_lower_norm("var t3 := nested.0.1;");
    let expected_init = bin(
        bin(
            ident("nested", span(10, 16)),
            HirBinaryOp::Access,
            lit(HirLiteral::Int(0), span(17, 18)),
            span(10, 16),
        ),
        HirBinaryOp::Access,
        lit(HirLiteral::Int(1), span(19, 20)),
        span(10, 20),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "t3",
            false,
            false,
            false,
            false,
            None,
            expected_init,
            span(0, 21)
        )]
    );
}

#[test]
fn function_calls() {
    let hir = parse_lower_norm("f(1, 2); obj.m(4);");
    assert_eq!(
        hir,
        vec![
            expr_stmt(
                call(
                    ident("f", span(0, 1)),
                    vec![
                        lit(HirLiteral::Int(1), span(2, 3)),
                        lit(HirLiteral::Int(2), span(5, 6))
                    ],
                    span(0, 8),
                ),
                span(0, 8),
            ),
            expr_stmt(
                bin(
                    ident("obj", span(9, 12)),
                    HirBinaryOp::Access,
                    call(
                        ident("m", span(13, 14)),
                        vec![lit(HirLiteral::Int(4), span(15, 16))],
                        span(13, 18)
                    ),
                    span(9, 18),
                ),
                span(9, 18),
            ),
        ]
    );
}

#[test]
fn index_expression() {
    let hir = parse_lower_norm("xs[2];");
    assert_eq!(
        hir,
        vec![expr_stmt(
            expr(
                HirExprKind::Index {
                    target: Box::new(ident("xs", span(0, 2))),
                    index: Box::new(lit(HirLiteral::Int(2), span(3, 4))),
                },
                span(0, 5),
            ),
            span(0, 5),
        )]
    );
}

#[test]
fn index_assignment() {
    let hir = parse_lower_norm("ys[i] = 2;");
    let lhs = expr(
        HirExprKind::Index {
            target: Box::new(ident("ys", span(0, 2))),
            index: Box::new(ident("i", span(3, 4))),
        },
        span(0, 5),
    );
    assert_eq!(
        hir,
        vec![expr_stmt(
            bin(
                lhs,
                HirBinaryOp::Assign,
                lit(HirLiteral::Int(2), span(8, 9)),
                span(0, 9)
            ),
            span(0, 9)
        )]
    );
}

#[test]
fn index_after_field_access_in_assignment() {
    let hir = parse_lower_norm("obj.items[1] = 4;");
    let items = expr(
        HirExprKind::Index {
            target: Box::new(ident("items", span(4, 9))),
            index: Box::new(lit(HirLiteral::Int(1), span(10, 11))),
        },
        span(4, 12),
    );
    let lhs = bin(
        ident("obj", span(0, 3)),
        HirBinaryOp::Access,
        items,
        span(0, 12),
    );
    assert_eq!(
        hir,
        vec![expr_stmt(
            bin(
                lhs,
                HirBinaryOp::Assign,
                lit(HirLiteral::Int(4), span(15, 16)),
                span(0, 16)
            ),
            span(0, 16)
        )]
    );
}

#[test]
fn var_init_from_index() {
    let hir = parse_lower_norm("var t := data[0];");
    let init = expr(
        HirExprKind::Index {
            target: Box::new(ident("data", span(9, 13))),
            index: Box::new(lit(HirLiteral::Int(0), span(14, 15))),
        },
        span(9, 16),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "t",
            false,
            false,
            false,
            false,
            None,
            init,
            span(0, 17)
        )]
    );
}

// special expression forms
#[test]
fn unwrap_expression() {
    let hir = parse_lower_norm("var u := unwrap[opt];");
    assert_eq!(
        hir,
        vec![var_decl(
            "u",
            false,
            false,
            false,
            false,
            None,
            expr(
                HirExprKind::Unwrap(Box::new(ident("opt", span(16, 19)))),
                span(9, 21)
            ),
            span(0, 21),
        )]
    );
}

#[test]
fn sizeof_expression() {
    let hir = parse_lower_norm("var sz := sizeof<i32>;");
    assert_eq!(
        hir,
        vec![var_decl(
            "sz",
            false,
            false,
            false,
            false,
            None,
            expr(
                HirExprKind::SizeOf(tn(HirType::I32, span(17, 20))),
                span(10, 22)
            ),
            span(0, 22),
        )]
    );
}

#[test]
fn cast_expression() {
    let hir = parse_lower_norm("var c := cast<f32>(x);");
    let init = expr(
        HirExprKind::StaticCast(
            Box::new(tn(HirType::F32, span(14, 17))),
            Box::new(ident("x", span(19, 20))),
        ),
        span(9, 21),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "c",
            false,
            false,
            false,
            false,
            None,
            init,
            span(0, 22)
        )]
    );
}

#[test]
fn bitcast_expression() {
    let hir = parse_lower_norm("var b := bitcast<u32>(p);");
    let init = expr(
        HirExprKind::BitCast(
            Box::new(tn(HirType::U32, span(17, 20))),
            Box::new(ident("p", span(22, 23))),
        ),
        span(9, 24),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "b",
            false,
            false,
            false,
            false,
            None,
            init,
            span(0, 25)
        )]
    );
}

#[test]
fn struct_instantiation() {
    let hir = parse_lower_norm("var f := .Food{ .power = 100, .health = 1000 };");
    let inst = expr(
        HirExprKind::Instantiation {
            init_ty: Box::new(tn(HirType::CustomType("Food".to_string()), span(10, 14))),
            body: vec![
                HirInstParam {
                    hir_id: zid(),
                    name: "power".to_string(),
                    value: Box::new(lit(HirLiteral::Int(100), span(25, 28))),
                    span: span(16, 28),
                },
                HirInstParam {
                    hir_id: zid(),
                    name: "health".to_string(),
                    value: Box::new(lit(HirLiteral::Int(1000), span(40, 44))),
                    span: span(30, 44),
                },
            ],
        },
        span(9, 46),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "f",
            false,
            false,
            false,
            false,
            None,
            inst,
            span(0, 47)
        )]
    );
}

#[test]
fn tuple_instantiation() {
    let hir = parse_lower_norm("var t := .(1, 2, 3);");
    let inst = expr(
        HirExprKind::TupleInst {
            body: vec![
                lit(HirLiteral::Int(1), span(11, 12)),
                lit(HirLiteral::Int(2), span(14, 15)),
                lit(HirLiteral::Int(3), span(17, 18)),
            ],
        },
        span(9, 19),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "t",
            false,
            false,
            false,
            false,
            None,
            inst,
            span(0, 20)
        )]
    );
}

#[test]
fn dollar_scope() {
    let hir = parse_lower_norm("var w := $${ var y := 5; y; };");
    let scope = expr(
        HirExprKind::DollarScope {
            params: vec![],
            body: vec![var_decl(
                "y",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Int(5), span(22, 23)),
                span(13, 26),
            )],
            result: Some(Box::new(ident("y", span(25, 26)))),
        },
        span(9, 30),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "w",
            false,
            false,
            false,
            false,
            None,
            scope,
            span(0, 30)
        )]
    );
}

#[test]
fn array_literal() {
    let hir = parse_lower_norm("var xs := [1, 2, 3];");
    let init = lit(
        HirLiteral::ArrayLiteral(vec![
            lit(HirLiteral::Int(1), span(11, 12)),
            lit(HirLiteral::Int(2), span(14, 15)),
            lit(HirLiteral::Int(3), span(17, 18)),
        ]),
        span(10, 19),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "xs",
            false,
            false,
            false,
            false,
            None,
            init,
            span(0, 20)
        )]
    );
}

// ---------------------------------------------------------------------------
// control flow
// ---------------------------------------------------------------------------

#[test]
fn if_elif_else_desugars_to_nested_if() {
    let hir = parse_lower_norm("if a > b { x = 1; } elif c { x = 2; } else { x = 3; }");
    let x1 = expr_stmt(
        bin(
            ident("x", span(11, 12)),
            HirBinaryOp::Assign,
            lit(HirLiteral::Int(1), span(15, 16)),
            span(11, 16),
        ),
        span(11, 16),
    );
    let x2 = expr_stmt(
        bin(
            ident("x", span(29, 30)),
            HirBinaryOp::Assign,
            lit(HirLiteral::Int(2), span(33, 34)),
            span(29, 34),
        ),
        span(29, 34),
    );
    let x3 = expr_stmt(
        bin(
            ident("x", span(45, 46)),
            HirBinaryOp::Assign,
            lit(HirLiteral::Int(3), span(49, 50)),
            span(45, 50),
        ),
        span(45, 50),
    );
    let elif = stmt(
        HirStmtKind::HirIf {
            condition: Box::new(ident("c", span(25, 26))),
            body: vec![x2],
            else_body: Some(vec![x3]),
        },
        span(27, 37),
    );
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirIf {
                condition: Box::new(bin(
                    ident("a", span(3, 4)),
                    HirBinaryOp::Gt,
                    ident("b", span(7, 8)),
                    span(3, 8)
                )),
                body: vec![x1],
                else_body: Some(vec![elif]),
            },
            span(0, 53),
        )]
    );
}

#[test]
fn while_loop() {
    let hir = parse_lower_norm("while i < 10 { i += 1; }");
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirWhile {
                condition: Box::new(bin(
                    ident("i", span(6, 7)),
                    HirBinaryOp::Lt,
                    lit(HirLiteral::Int(10), span(10, 12)),
                    span(6, 12)
                )),
                body: vec![expr_stmt(
                    bin(
                        ident("i", span(15, 16)),
                        HirBinaryOp::AddAssign,
                        lit(HirLiteral::Int(1), span(20, 21)),
                        span(15, 21)
                    ),
                    span(15, 21),
                )],
            },
            span(0, 24),
        )]
    );
}

#[test]
fn for_loop_desugars_to_var_plus_while() {
    let hir = parse_lower_norm("for var i := 0; i < 10; i += 1 { x = i; }");
    let x_assign = expr_stmt(
        bin(
            ident("x", span(33, 34)),
            HirBinaryOp::Assign,
            ident("i", span(37, 38)),
            span(33, 38),
        ),
        span(33, 38),
    );
    let update = expr_stmt(
        bin(
            ident("i", span(24, 25)),
            HirBinaryOp::AddAssign,
            lit(HirLiteral::Int(1), span(29, 30)),
            span(24, 30),
        ),
        span(24, 30),
    );
    assert_eq!(
        hir,
        vec![
            var_decl(
                "i",
                false,
                false,
                false,
                false,
                None,
                lit(HirLiteral::Int(0), span(13, 14)),
                span(4, 17)
            ),
            stmt(
                HirStmtKind::HirWhile {
                    condition: Box::new(bin(
                        ident("i", span(16, 17)),
                        HirBinaryOp::Lt,
                        lit(HirLiteral::Int(10), span(20, 22)),
                        span(16, 22)
                    )),
                    body: vec![x_assign, update],
                },
                span(0, 41),
            ),
        ]
    );
}

#[test]
fn each_desugars_to_variables_and_while_with_next() {
    // Synthesized scaffolding is anchored to the collection expr + implicit `.next()` (13,24);
    // the item var decl to the item name (5,9); the while keeps the whole each stmt span (0,32).
    let list_ident = ident("list", span(13, 17));
    let init_call = expr(
        HirExprKind::Call(
            Box::new(expr(
                HirExprKind::Binary(
                    Box::new(list_ident.clone()),
                    HirBinaryOp::Access,
                    Box::new(ident("next", span(13, 24))),
                ),
                span(13, 24),
            )),
            vec![],
        ),
        span(13, 24),
    );
    let init_var = var_decl(
        "__iter_val_0",
        true,
        false,
        false,
        false,
        None,
        init_call,
        span(13, 24),
    );
    let cond = expr(
        HirExprKind::Binary(
            Box::new(ident("__iter_val_0", span(13, 24))),
            HirBinaryOp::Neq,
            Box::new(lit(HirLiteral::Null, span(13, 24))),
        ),
        span(13, 24),
    );
    let item_var = var_decl(
        "item",
        false,
        false,
        false,
        false,
        None,
        expr(
            HirExprKind::Unwrap(Box::new(ident("__iter_val_0", span(13, 24)))),
            span(13, 24),
        ),
        span(5, 9),
    );
    let use_call = expr_stmt(
        call(
            ident("use", span(20, 23)),
            vec![ident("item", span(24, 28))],
            span(20, 30),
        ),
        span(20, 30),
    );
    let progress = expr_stmt(
        expr(
            HirExprKind::Binary(
                Box::new(ident("__iter_val_0", span(13, 24))),
                HirBinaryOp::Assign,
                Box::new(expr(
                    HirExprKind::Call(
                        Box::new(expr(
                            HirExprKind::Binary(
                                Box::new(list_ident.clone()),
                                HirBinaryOp::Access,
                                Box::new(ident("next", span(13, 24))),
                            ),
                            span(13, 24),
                        )),
                        vec![],
                    ),
                    span(13, 24),
                )),
            ),
            span(13, 24),
        ),
        span(13, 24),
    );
    let while_stmt = stmt(
        HirStmtKind::HirWhile {
            condition: Box::new(cond),
            body: vec![item_var, use_call, progress],
        },
        span(0, 32),
    );
    assert_eq!(
        parse_lower_norm("each item in list { use(item); }"),
        vec![norm_stmt(init_var), norm_stmt(while_stmt)]
    );
}

#[test]
fn each_lowers_collection_once_per_call_site_with_distinct_ids() {
    // The `list` collection must be lowered separately for the init and the advance
    // call sites; a cloned node would reuse a single NodeId for both identifiers.
    let hir = parse_lower("each item in list { use(item); }");

    fn expr_list_ids(e: &HirExpr, acc: &mut Vec<NodeId>) {
        if matches!(&e.kind, HirExprKind::Identifier(n) if n == "list") {
            acc.push(e.hir_id);
        }
        match &e.kind {
            HirExprKind::Call(callee, args) => {
                expr_list_ids(callee, acc);
                for a in args {
                    expr_list_ids(a, acc);
                }
            }
            HirExprKind::Binary(l, _, r) => {
                expr_list_ids(l, acc);
                expr_list_ids(r, acc);
            }
            HirExprKind::Unwrap(inner) => expr_list_ids(inner, acc),
            _ => {}
        }
    }

    fn stmt_list_ids(s: &HirStmt, acc: &mut Vec<NodeId>) {
        match &s.kind {
            HirStmtKind::HirVarDecl { init, .. } => expr_list_ids(init, acc),
            HirStmtKind::HirExpr(e) => expr_list_ids(e, acc),
            HirStmtKind::HirWhile { condition, body } => {
                expr_list_ids(condition, acc);
                for b in body {
                    stmt_list_ids(b, acc);
                }
            }
            _ => {}
        }
    }

    let mut ids = Vec::new();
    for s in &hir {
        stmt_list_ids(s, &mut ids);
    }
    assert_eq!(
        ids.len(),
        2,
        "expected two `list` identifiers, got: {ids:?}"
    );
    assert_ne!(
        ids[0], ids[1],
        "init and advance `list` nodes must not share a NodeId"
    );
}

// functions
#[test]
fn function_def() {
    let hir = parse_lower_norm("func add(a: i32, b: i32): i32 { return a + b; }");
    assert_eq!(
        hir,
        vec![func_def(
            "add",
            vec![
                param(
                    "a",
                    tn(HirType::I32, span(12, 15)),
                    false,
                    false,
                    None,
                    span(9, 16)
                ),
                param(
                    "b",
                    tn(HirType::I32, span(20, 23)),
                    false,
                    false,
                    None,
                    span(17, 24)
                ),
            ],
            tn(HirType::I32, span(26, 29)),
            vec![],
            false,
            None,
            false,
            vec![stmt(
                HirStmtKind::HirReturn(Some(Box::new(bin(
                    ident("a", span(39, 40)),
                    HirBinaryOp::Add,
                    ident("b", span(43, 44)),
                    span(39, 44)
                )))),
                span(32, 47),
            )],
            span(0, 47),
        )]
    );
}

#[test]
fn function_def_multiple_body_statements() {
    let hir = parse_lower_norm(
        "func fib(): i32 { mut var x := 0; while x < 10 { x += 1; } if x == 10 { return x; } else { return 0; } }",
    );
    let body_x = var_decl(
        "x",
        true,
        false,
        false,
        false,
        None,
        lit(HirLiteral::Int(0), span(31, 32)),
        span(22, 39),
    );
    let while_body = expr_stmt(
        bin(
            ident("x", span(49, 50)),
            HirBinaryOp::AddAssign,
            lit(HirLiteral::Int(1), span(54, 55)),
            span(49, 55),
        ),
        span(49, 55),
    );
    let while_stmt = stmt(
        HirStmtKind::HirWhile {
            condition: Box::new(bin(
                ident("x", span(40, 41)),
                HirBinaryOp::Lt,
                lit(HirLiteral::Int(10), span(44, 46)),
                span(40, 46),
            )),
            body: vec![while_body],
        },
        span(34, 61),
    );
    let ret_x = stmt(
        HirStmtKind::HirReturn(Some(Box::new(ident("x", span(79, 80))))),
        span(72, 83),
    );
    let ret_0 = stmt(
        HirStmtKind::HirReturn(Some(Box::new(lit(HirLiteral::Int(0), span(98, 99))))),
        span(91, 102),
    );
    let if_stmt = stmt(
        HirStmtKind::HirIf {
            condition: Box::new(bin(
                ident("x", span(62, 63)),
                HirBinaryOp::Eq,
                lit(HirLiteral::Int(10), span(67, 69)),
                span(62, 69),
            )),
            body: vec![ret_x],
            else_body: Some(vec![ret_0]),
        },
        span(59, 104),
    );
    assert_eq!(
        hir,
        vec![func_def(
            "fib",
            vec![],
            tn(HirType::I32, span(12, 15)),
            vec![],
            false,
            None,
            false,
            vec![body_x, while_stmt, if_stmt],
            span(0, 104),
        )]
    );
}

#[test]
fn function_def_empty_body() {
    let hir = parse_lower_norm("func noop(): u32 { }");
    assert_eq!(
        hir,
        vec![func_def(
            "noop",
            vec![],
            tn(HirType::U32, span(13, 16)),
            vec![],
            false,
            None,
            false,
            vec![],
            span(0, 20)
        )]
    );
}

#[test]
fn function_def_no_return_type_becomes_unit_type_node_with_full_span() {
    let hir = parse_lower_norm("func noop2() { return; }");
    assert_eq!(
        hir,
        vec![func_def(
            "noop2",
            vec![],
            tn(HirType::Unit, span(10, 10)),
            vec![],
            false,
            None,
            false,
            vec![stmt(HirStmtKind::HirReturn(None), span(15, 22))],
            span(0, 24),
        )]
    );
}

#[test]
fn function_param_default_value() {
    let hir = parse_lower_norm("func fw(a: i32 := 7): i32 { return a; }");
    assert_eq!(
        hir,
        vec![func_def(
            "fw",
            vec![param(
                "a",
                tn(HirType::I32, span(11, 14)),
                false,
                false,
                Some(lit(HirLiteral::Int(7), span(18, 19))),
                span(8, 20),
            )],
            tn(HirType::I32, span(22, 25)),
            vec![],
            false,
            None,
            false,
            vec![stmt(
                HirStmtKind::HirReturn(Some(Box::new(ident("a", span(35, 36))))),
                span(28, 39)
            )],
            span(0, 39),
        )]
    );
}

#[test]
fn contract_function_decl() {
    let hir = parse_lower_norm("contract HasX { func get(): i32 }");
    let func = stmt(
        HirStmtKind::HirFunctionDecl {
            name: "get".to_string(),
            params: vec![],
            return_type: tn(HirType::I32, span(28, 31)),
            generic_type_params: vec![],
            exposed: false,
            conv: None,
        },
        span(16, 33),
    );
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirContractDecl {
                name: "HasX".to_string(),
                functions: vec![func],
                generic_type_params: vec![],
                exposed: false,
            },
            span(0, 33),
        )]
    );
}

// structs, enums, variants
#[test]
fn struct_decl() {
    let hir = parse_lower_norm("struct Point { x: i32, y: i32 }");
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirStructDecl {
                name: "Point".to_string(),
                contracts: vec![],
                generic_type_params: vec![],
                fields: vec![
                    param(
                        "x",
                        tn(HirType::I32, span(18, 21)),
                        false,
                        false,
                        None,
                        span(15, 22)
                    ),
                    param(
                        "y",
                        tn(HirType::I32, span(26, 29)),
                        false,
                        false,
                        None,
                        span(23, 31)
                    ),
                ],
                exposed: false,
            },
            span(0, 31),
        )]
    );
}

#[test]
fn struct_decl_with_contract() {
    let hir = parse_lower_norm("struct Pl: HasX { x: i32 }");
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirStructDecl {
                name: "Pl".to_string(),
                contracts: vec![tn(HirType::CustomType("HasX".to_string()), span(11, 15))],
                generic_type_params: vec![],
                fields: vec![param(
                    "x",
                    tn(HirType::I32, span(21, 24)),
                    false,
                    false,
                    None,
                    span(18, 26)
                )],
                exposed: false,
            },
            span(0, 26),
        )]
    );
}

#[test]
fn enum_decl_resolves_member_values() {
    let hir = parse_lower_norm("enum Color: u8 { RED, GREEN = 5, BLUE }");
    let member = |name: &str, value: isize, s: Span| HirEnumMember {
        hir_id: zid(),
        name: name.to_string(),
        value,
        span: s,
    };
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirEnumDecl {
                name: "Color".to_string(),
                underlying: tn(HirType::U8, span(12, 14)),
                members: vec![
                    member("RED", 0, span(17, 21)),
                    member("GREEN", 5, span(22, 32)),
                    member("BLUE", 6, span(33, 39)),
                ],
                exposed: false,
            },
            span(0, 39),
        )]
    );
}

#[test]
fn enum_neg_explicit_value_pins_and_next_member_auto_increments() {
    let hir = parse_lower_norm("enum E { A = -1, B }");
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirEnumDecl {
                name: "E".to_string(),
                underlying: tn(HirType::U32, span(6, 6)),
                members: vec![
                    HirEnumMember {
                        hir_id: zid(),
                        name: "A".to_string(),
                        value: -1,
                        span: span(9, 16)
                    },
                    HirEnumMember {
                        hir_id: zid(),
                        name: "B".to_string(),
                        value: 0,
                        span: span(17, 20)
                    },
                ],
                exposed: false,
            },
            span(0, 20),
        )]
    );
}

#[test]
fn variant_decl_assigns_tags() {
    let hir = parse_lower_norm("variant Shape { Circle(i8, i64), Square }");
    let member = |name: &str, types: Vec<HirTypeNode>, tag: u32, s: Span| HirVariantMember {
        hir_id: zid(),
        name: name.to_string(),
        member_types: types,
        tag,
        span: s,
    };
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirVariantDecl {
                name: "Shape".to_string(),
                contracts: vec![],
                members: vec![
                    member(
                        "Circle",
                        vec![
                            tn(HirType::I8, span(23, 25)),
                            tn(HirType::I64, span(27, 30))
                        ],
                        0,
                        span(16, 32),
                    ),
                    member("Square", vec![], 1, span(33, 41)),
                ],
                generic_type_params: vec![],
                exposed: false,
            },
            span(0, 41),
        )]
    );
}

// ---------------------------------------------------------------------------
// alias, import, methods, seal, generics
// ---------------------------------------------------------------------------

#[test]
fn alias_decl() {
    let hir = parse_lower_norm("alias i32 as MyInt");
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirAlias {
                original: Box::new(tn(HirType::I32, span(6, 9))),
                alias: "MyInt".to_string(),
            },
            span(0, 18),
        )]
    );
}

#[test]
fn import_decl() {
    let hir = parse_lower_norm("import A::B");
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirImport {
                name: "A_B".to_string(),
                alias: None,
            },
            span(0, 11),
        )]
    );
}

#[test]
fn import_decl_with_alias() {
    let hir = parse_lower_norm("import std as s");
    assert_eq!(
        hir,
        vec![stmt(
            HirStmtKind::HirImport {
                name: "std".to_string(),
                alias: Some("s".to_string()),
            },
            span(0, 15),
        )]
    );
}

#[test]
fn seal_desugars_to_mangled_function() {
    let hir = parse_lower_norm("seal IntExt { func double(): i32 { return 0; } }");
    assert_eq!(
        hir,
        vec![func_def(
            "IntExt_double",
            vec![],
            tn(HirType::I32, span(29, 32)),
            vec![],
            false,
            None,
            false,
            vec![stmt(
                HirStmtKind::HirReturn(Some(Box::new(lit(HirLiteral::Int(0), span(42, 43))))),
                span(35, 46),
            )],
            span(14, 48),
        )]
    );
}

#[test]
fn generics_block_annotates_function_with_type_params() {
    let hir = parse_lower_norm("generics <T> { func identity(v: T): T { return v; } }");
    assert_eq!(
        hir,
        vec![func_def(
            "identity",
            vec![param(
                "v",
                tn(HirType::GenericPlaceHolder("T".to_string()), span(32, 33)),
                false,
                false,
                None,
                span(29, 34),
            )],
            tn(HirType::GenericPlaceHolder("T".to_string()), span(36, 37)),
            vec![tn(
                HirType::GenericPlaceHolder("T".to_string()),
                span(10, 11)
            )],
            false,
            None,
            false,
            vec![stmt(
                HirStmtKind::HirReturn(Some(Box::new(ident("v", span(47, 48))))),
                span(40, 51)
            )],
            span(15, 53),
        )]
    );
}

// ---------------------------------------------------------------------------
// type annotations
// ---------------------------------------------------------------------------

#[test]
fn pointer_type_annotation() {
    let hir = parse_lower_norm("var ptr<i32> x := p;");
    assert_eq!(
        hir,
        vec![var_decl(
            "x",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Ptr(Box::new(tn(HirType::I32, span(8, 11)))),
                span(4, 11)
            )),
            ident("p", span(18, 19)),
            span(0, 20),
        )]
    );
}

#[test]
fn ref_type_annotation() {
    let hir = parse_lower_norm("var ref<u8> y := r;");
    assert_eq!(
        hir,
        vec![var_decl(
            "y",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Ref(Box::new(tn(HirType::U8, span(8, 10)))),
                span(4, 10)
            )),
            ident("r", span(17, 18)),
            span(0, 19),
        )]
    );
}

#[test]
fn array_type_annotation_with_size() {
    let hir = parse_lower_norm("var arr[i32, 4] big := small;");
    assert_eq!(
        hir,
        vec![var_decl(
            "big",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Array(Box::new(tn(HirType::I32, span(8, 11))), Some(4)),
                span(8, 11)
            )),
            ident("small", span(23, 28)),
            span(0, 29),
        )]
    );
}

#[test]
fn array_type_annotation_without_size() {
    let hir = parse_lower_norm("var arr[i32] big2 := small2;");
    assert_eq!(
        hir,
        vec![var_decl(
            "big2",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Array(Box::new(tn(HirType::I32, span(8, 11))), None),
                span(8, 11)
            )),
            ident("small2", span(21, 27)),
            span(0, 28),
        )]
    );
}

#[test]
fn nullable_type_annotation_single_type_is_not_wrapped_in_tuple() {
    let hir = parse_lower_norm("var (i32)? n := x;");
    assert_eq!(
        hir,
        vec![var_decl(
            "n",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Nullable(Box::new(tn(HirType::I32, span(5, 8)))),
                span(4, 12),
            )),
            ident("x", span(16, 17)),
            span(0, 18),
        )]
    );
}

#[test]
fn failable_type_annotation() {
    let hir = parse_lower_norm("var !!(i32, str) fr := g;");
    assert_eq!(
        hir,
        vec![var_decl(
            "fr",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Failable(
                    Box::new(tn(HirType::I32, span(7, 10))),
                    Box::new(tn(HirType::Str, span(12, 15))),
                ),
                span(4, 19),
            )),
            ident("g", span(23, 24)),
            span(0, 25),
        )]
    );
}

#[test]
fn tuple_type_annotation() {
    let hir = parse_lower_norm("var (i32, u8) pr := pair;");
    assert_eq!(
        hir,
        vec![var_decl(
            "pr",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Tuple(vec![
                    tn(HirType::I32, span(5, 8)),
                    tn(HirType::U8, span(10, 12))
                ]),
                span(4, 16),
            )),
            ident("pair", span(20, 24)),
            span(0, 25),
        )]
    );
}

#[test]
fn funcptr_type_annotation() {
    let hir = parse_lower_norm("var func(i32): i32 fp := g;");
    assert_eq!(
        hir,
        vec![var_decl(
            "fp",
            false,
            false,
            false,
            false,
            Some(tn(
                HirType::Func(
                    vec![tn(HirType::I32, span(9, 12))],
                    Box::new(tn(HirType::I32, span(15, 18)))
                ),
                span(4, 21),
            )),
            ident("g", span(25, 26)),
            span(0, 27),
        )]
    );
}

// scoped paths and generic instantiation calls
#[test]
fn scoped_path_expression_flattens_to_mangled_identifier() {
    let hir = parse_lower_norm("var s := A::B;");
    assert_eq!(
        hir,
        vec![var_decl(
            "s",
            false,
            false,
            false,
            false,
            None,
            ident("A_B", span(9, 13)),
            span(0, 14),
        )]
    );
}

#[test]
fn namespaced_path_expression_flattens_to_mangled_identifier() {
    let hir = parse_lower_norm("var p := A::B::C;");
    assert_eq!(
        hir,
        vec![var_decl(
            "p",
            false,
            false,
            false,
            false,
            None,
            ident("A_B_C", span(9, 16)),
            span(0, 17),
        )]
    );
}

#[test]
fn namespaced_function_call_flattens_callee() {
    let hir = parse_lower_norm("A::B::make();");
    assert_eq!(
        hir,
        vec![expr_stmt(
            call(ident("A_B_make", span(0, 13)), vec![], span(0, 13)),
            span(0, 13),
        )]
    );
}

#[test]
fn generic_associated_call_keeps_type_params_in_callee() {
    let hir = parse_lower_norm("var g := Pair::<i32, u8>::make();");
    let callee = expr(
        HirExprKind::GenericInstantion {
            name: "Pair_make".to_string(),
            type_params: vec![
                tn(HirType::I32, span(16, 19)),
                tn(HirType::U8, span(21, 23)),
            ],
        },
        span(9, 33),
    );
    assert_eq!(
        hir,
        vec![var_decl(
            "g",
            false,
            false,
            false,
            false,
            None,
            call(callee, vec![], span(9, 33)),
            span(0, 33),
        )]
    );
}
