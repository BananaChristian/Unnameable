use crate::{
    ast::{Precedence, Qualifier, Stmt, StmtKind},
    diagnostics::Span,
    lexer::TType,
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Option<Stmt> {
        let token = self.current_token()?.clone();
        match token.token_type {
            TType::Var => self.parse_var_declaration(),
            TType::Func => self.parse_func_decl(),
            TType::Struct => self.parse_struct(),
            TType::Seal => self.parse_seal_decl(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let span = expr.span;
        Some(Stmt::new(StmtKind::Expr(expr), span))
    }

    fn parse_var_declaration(&mut self) -> Option<Stmt> {
        let mut span = Span::from_token(self.current_token()?);
        // Expect 'var' keyword
        self.expect_token(TType::Var)?;

        // Collect qualifiers (mut, const, heap)
        let mut qualifiers = Vec::new();
        if Qualifier::is_valid(self.current_token()?) {
            qualifiers = self.collect_qualifiers();
            for qualifier in &qualifiers {
                span = Span::merge(&span, &qualifier.span)
            }
        }

        // Parse the type annotation (optional)
        let ty = self.parse_type();
        if let Some(annot) = &ty {
            span = Span::merge(&span, &annot.span);
        }

        // Parse the name (identifier)
        let name = self.parse_identifier()?;
        span = Span::merge(&span, &name.span);

        // Check for initialization
        if self.current_token()?.token_type == TType::Bind {
            span = Span::merge(&span, &Span::from_token(self.current_token()?));
            self.advance(); // Consume the := token

            // Parse the initializer expression
            let init = self.parse_expression(Precedence::Lowest)?;
            span = Span::merge(&span, &init.span);

            Some(Stmt::new(
                StmtKind::VarDecl {
                    qualifiers,
                    type_annotation: ty,
                    name: Box::new(name),
                    init: Some(Box::new(init)),
                },
                span,
            ))
        } else {
            // Declaration without initialization
            Some(Stmt::new(
                StmtKind::VarDecl {
                    qualifiers,
                    type_annotation: ty,
                    name: Box::new(name),
                    init: None,
                },
                span,
            ))
        }
    }

    pub fn parse_struct_body(&mut self) -> Option<Stmt> {
        let mut span = Span::from_token(self.current_token()?);
        let mut fields = Vec::new();

        self.expect_token(TType::LBrace)?;

        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(param) = self.parse_param_decl() {
                span = Span::merge(&span, &param.span);
                fields.push(param);
            } else {
                let token = self.current_token()?.clone();
                self.report(
                    "Expected field declaration".to_string(),
                    Some(Span::from_token(&token)),
                );
                self.advance(); // Skip
            }

            // Optional comma
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
            }
        }

        self.expect_token(TType::Rbrace)?;
        span = Span::merge(&span, &Span::from_token(self.current_token()?));

        Some(Stmt::new(StmtKind::Block { content: fields }, span))
    }

    pub fn parse_struct(&mut self) -> Option<Stmt> {
        let mut span = Span::from_token(self.current_token()?);
        self.expect_token(TType::Struct)?;

        let name = self.parse_identifier()?;
        span = Span::merge(&span, &name.span);

        let body = self.parse_struct_body()?;
        span = Span::merge(&span, &body.span);

        Some(Stmt::new(
            StmtKind::StructDecl {
                name: Box::new(name),
                contents: Box::new(body),
            },
            span,
        ))
    }

    pub fn parse_seal_decl(&mut self) -> Option<Stmt> {
        let mut span = Span::from_token(self.current_token()?);
        self.expect_token(TType::Seal)?;

        let name = self.parse_identifier()?;
        span = Span::merge(&span, &name.span);

        let body = self.parse_body()?;
        span = Span::merge(&span, &body.span);

        Some(Stmt::new(
            StmtKind::SealStmt {
                name: Box::new(name),
                contents: Box::new(body),
            },
            span,
        ))
    }

    pub fn parse_body(&mut self) -> Option<Stmt> {
        let mut span = Span::from_token(self.current_token()?);
        let mut stmts = Vec::new();
        self.expect_token(TType::LBrace)?;

        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_stmt() {
                span = Span::merge(&span, &stmt.span);
                stmts.push(stmt);
            } else {
                self.advance();
            }
        }

        if self.current_token()?.token_type == TType::Rbrace {
            span = Span::merge(&span, &Span::from_token(self.current_token()?));
            self.advance(); // Consume '}'
        } else {
            return None; // Expected '}'
        }

        Some(Stmt::new(StmtKind::Block { content: stmts }, span))
    }
}
