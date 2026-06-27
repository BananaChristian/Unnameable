use crate::{
    ast::{Precedence, Qualifier, Stmt, StmtKind},
    diagnostics::Span,
    lexer::TType,
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_param_decl(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;

        // Collect qualifiers
        let mut qualifiers = Vec::new();
        while let Some(token) = self.current_token() {
            if Qualifier::is_valid(token) {
                let q = Qualifier::new(token);
                qualifiers.push(q);
                self.advance();
            } else {
                break;
            }
        }

        // Parse name (must be identifier)
        let name = self.parse_identifier()?;

        // Expect colon
        if self.current_token()?.token_type != TType::Colon {
            return None;
        }
        self.advance(); // Consume colon

        // Parse type
        let ty = self.parse_type()?;

        // Parse default value (optional)
        let default = if self.current_token()?.token_type == TType::Bind {
            self.advance(); // Consume :=
            let value = self.parse_expression(Precedence::Lowest)?;
            Some(Box::new(value))
        } else {
            None
        };

        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::ParamDecl {
                qualifiers,
                name: Box::new(name),
                type_annotation: ty,
                def: default,
            },
            span,
        ))
    }

    fn parse_parameters(&mut self) -> Option<Vec<Stmt>> {
        let mut params = Vec::new();

        // Must start with '('
        if self.current_token()?.token_type != TType::Lparen {
            return Some(params);
        }

        self.advance(); // Consume '('

        // Empty parameters: ()
        if self.current_token()?.token_type == TType::Rparen {
            self.advance(); // Consume ')'
            return Some(params);
        }

        // Parse parameters with commas
        loop {
            let param = self.parse_param_decl()?;
            params.push(param);

            if self.match_token(TType::Comma) {
                continue;
            } else {
                break;
            }
        }

        // Expect ')'
        if self.current_token()?.token_type != TType::Rparen {
            return None;
        }
        self.advance(); // Consume ')'

        Some(params)
    }

    pub fn parse_func(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Func)?;

        // Parse name
        let name = self.parse_identifier()?;

        // Parse parameters (only if '(' is present)
        let params = if self.current_token()?.token_type == TType::Lparen {
            self.parse_parameters()?
        } else {
            Vec::new()
        };

        // Parse return type (ONLY if ':' is present)
        let return_type = if self.current_token()?.token_type == TType::Colon {
            self.advance(); // Consume ':'
            let ty = self.parse_type()?;
            Some(ty)
        } else {
            None
        };

        // Parse body (only if '{' is present)
        if self.current_token()?.token_type == TType::LBrace {
            let body = self.parse_body()?;
            let end = self.current_token()?.span.end;
            let span = Span { start, end };

            Some(Stmt::new(
                StmtKind::FunctionDef {
                    name: Box::new(name),
                    params,
                    type_annotation: return_type,
                    body: Box::new(body),
                },
                span,
            ))
        } else {
            let end = self.current_token()?.span.end;
            let span = Span { start, end };

            Some(Stmt::new(
                StmtKind::FunctionDecl {
                    name: Box::new(name),
                    params,
                    type_annotation: return_type,
                },
                span,
            ))
        }
    }
}
