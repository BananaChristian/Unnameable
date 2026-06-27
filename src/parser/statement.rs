use crate::{
    ast::{Elif, Precedence, Qualifier, Stmt, StmtKind},
    diagnostics::Span,
    lexer::TType,
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Option<Stmt> {
        let token = self.current_token()?.clone();
        match token.token_type {
            TType::Var => self.parse_var(),
            TType::Func => self.parse_func(),
            TType::Struct => self.parse_struct(),
            TType::Seal => self.parse_seal(),
            TType::Methods => self.parse_methods(),
            TType::If => self.parse_if_stmt(),
            TType::While => self.parse_while(),
            TType::Each => self.parse_each(),
            TType::For => self.parse_for(),
            TType::Generics => self.parse_generics(),
            TType::Contract => self.parse_contract(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let span = expr.clone().span;
        Some(Stmt::new(StmtKind::Expr(expr), span))
    }

    fn parse_var(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Var)?;

        // Collect qualifiers (mut, const, heap)
        let mut qualifiers = Vec::new();
        if Qualifier::is_valid(self.current_token()?) {
            qualifiers = self.collect_qualifiers();
        }

        // Parse the type annotation (optional)
        let ty = self.parse_type();

        // Parse the name (identifier)
        let name = self.parse_identifier()?;

        // Check for initialization
        let init = if self.current_token()?.token_type == TType::Bind {
            self.advance(); // Consume the := token
            Some(Box::new(self.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::VarDecl {
                qualifiers,
                type_annotation: ty,
                name: Box::new(name),
                init,
            },
            span,
        ))
    }

    pub fn parse_struct_body(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        let mut fields = Vec::new();

        self.expect_token(TType::LBrace)?;

        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(param) = self.parse_param_decl() {
                fields.push(param);
            } else {
                let token = self.current_token()?.clone();
                self.report("Expected field declaration".to_string(), Some(token.span));
                self.advance(); // Skip
            }

            // Optional comma
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
            }
        }

        self.expect_token(TType::Rbrace)?;
        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(StmtKind::Block { content: fields }, span))
    }

    pub fn parse_struct(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Struct)?;

        let name = self.parse_identifier()?;
        let mut contracts = Vec::new();
        if self.current_token()?.token_type == TType::Colon {
            self.advance();
            while self.current_token()?.token_type != TType::LBrace
                && self.current_token()?.token_type != TType::End
            {
                let contract = self.parse_identifier()?;
                contracts.push(contract);
                if self.current_token()?.token_type == TType::Comma {
                    self.advance();
                    continue;
                }
            }
        }

        let body = self.parse_struct_body()?;
        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::StructDecl {
                name: Box::new(name),
                contracts,
                contents: Box::new(body),
            },
            span,
        ))
    }

    fn parse_seal(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Seal)?;

        let name = self.parse_identifier()?;
        self.expect_token(TType::LBrace)?;
        let mut contents = Vec::new();
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_func() {
                match &stmt.kind {
                    StmtKind::FunctionDef { .. } => {
                        contents.push(stmt);
                    }
                    _ => {
                        self.report(
                            "Only function definitions are allowed in a seal".to_string(),
                            Some(stmt.span),
                        );
                    }
                }
            } else {
                self.advance();
            }
        }

        self.expect_token(TType::Rbrace)?;
        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::SealStmt {
                name: Box::new(name),
                contents,
            },
            span,
        ))
    }

    fn parse_methods(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Methods)?;
        let name = self.parse_identifier()?;
        let mut contents = Vec::new();
        self.expect_token(TType::LBrace)?;
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_func() {
                match stmt.kind {
                    StmtKind::FunctionDef { .. } => {
                        contents.push(stmt);
                    }
                    _ => {
                        self.report(
                            "Only function definitions are allowed in methods".to_string(),
                            Some(stmt.span),
                        );
                    }
                }
            } else {
                self.advance();
            }
        }
        self.expect_token(TType::Rbrace)?;
        let end = self.current_token()?.span.end;
        let span = Span { start, end };
        Some(Stmt::new(
            StmtKind::MethodsStmt {
                name: Box::new(name),
                contents,
            },
            span,
        ))
    }

    fn parse_elif(&mut self) -> Option<Elif> {
        let condition = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        Some(Elif {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_if_stmt(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::If)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;

        let mut elifs = Vec::new();
        if self.current_token()?.token_type == TType::Elif {
            while self.current_token()?.token_type == TType::Elif {
                self.advance(); //Consume the elif
                let elif = self.parse_elif()?;
                elifs.push(elif);
            }
        }

        let mut else_body = None;
        if self.current_token()?.token_type == TType::Else {
            self.advance(); //Consume the else
            else_body = self.parse_body();
        }

        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::IfStmt {
                condition: Box::new(condition),
                body: Box::new(body),
                elifs,
                else_body: else_body.map(Box::new),
            },
            span,
        ))
    }

    fn parse_generics(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Generics)?;
        self.expect_token(TType::Lt)?;

        let mut params = Vec::new();
        while self.current_token()?.token_type != TType::Gt
            && self.current_token()?.token_type != TType::End
        {
            let ty = self.parse_type()?;
            params.push(ty);
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
                continue;
            }
        }
        self.expect_token(TType::Gt)?;

        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;

        Some(Stmt::new(
            StmtKind::GenericBlock {
                params,
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    fn parse_contract(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Contract)?;
        let name = self.parse_identifier()?;
        let mut contents = Vec::new();
        self.expect_token(TType::LBrace)?;
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_func() {
                match stmt.kind {
                    StmtKind::FunctionDecl { .. } => contents.push(stmt),
                    _ => {
                        self.report(
                            "Only function declarations are allowed in a contract".to_string(),
                            Some(stmt.span),
                        );
                    }
                }
            } else {
                self.advance();
            }
        }
        self.expect_token(TType::Rbrace)?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::ContractBlock {
                name: Box::new(name),
                body: contents,
            },
            Span { start, end },
        ))
    }

    fn parse_while(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::While)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::WhileStmt {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    fn parse_for(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::For)?;
        let init = self.parse_var()?;
        self.expect_token(TType::Semicolon)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(TType::Semicolon)?;
        let update = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::ForStmt {
                init: Box::new(init),
                condition: Box::new(condition),
                update: Box::new(update),
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    pub fn parse_each(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Each)?;
        let item = self.parse_identifier()?;
        self.expect_token(TType::In)?;
        let collection = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::EachStmt {
                item: Box::new(item),
                collection: Box::new(collection),
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    pub fn parse_body(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        let mut stmts = Vec::new();
        self.expect_token(TType::LBrace)?;

        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                self.advance();
            }
        }

        self.expect_token(TType::Rbrace)?;
        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(StmtKind::Block { content: stmts }, span))
    }
}
