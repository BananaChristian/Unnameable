use crate::{
    ast::{Precedence, Qualifier, Stmt, StmtKind, Type},
    diagnostics::Span,
    lexer::TType,
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_param_decl(&mut self) -> Option<Stmt> {
        let start_token = self.current_token()?;
        let mut span = Span::from_token(start_token);

        // Collect qualifiers
        let mut qualifiers = Vec::new();
        while let Some(token) = self.current_token() {
            if Qualifier::is_valid(token) {
                let q = Qualifier::new(token);
                span = Span::merge(&span, &q.span);
                qualifiers.push(q);
                self.advance();
            } else {
                break;
            }
        }

        // Parse name (must be identifier)
        let name = self.parse_identifier()?;
        span = Span::merge(&span, &name.span);

        // Expect colon
        if self.current_token()?.token_type != TType::Colon {
            // Error: Expected colon
            return None;
        }
        let colon_span = Span::from_token(self.current_token()?);
        span = Span::merge(&span, &colon_span);
        self.advance(); // Consume colon

        // Parse type
        let ty = self.parse_type()?;
        span = Span::merge(&span, &ty.span);

        // Parse default value (optional)
        let default = if self.current_token()?.token_type == TType::Bind {
            let bind_span = Span::from_token(self.current_token()?);
            span = Span::merge(&span, &bind_span);
            self.advance(); // Consume :=

            let value = self.parse_expression(Precedence::Lowest)?;
            span = Span::merge(&span, &value.span);
            Some(Box::new(value))
        } else {
            None
        };

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
            // No parentheses = no parameters
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
            return None; // Error: expected ')'
        }
        self.advance(); // Consume ')'

        Some(params)
    }

    pub fn parse_func_decl(&mut self) -> Option<Stmt> {
        let start_token = self.current_token()?;
        let mut span = Span::from_token(start_token);
        self.expect_token(TType::Func)?;

        // Parse name
        let name = self.parse_identifier()?;
        span = Span::merge(&span, &name.span);

        // Parse parameters (only if '(' is present)
        let params = if self.current_token()?.token_type == TType::Lparen {
            let params = self.parse_parameters()?;
            for param in &params {
                span = Span::merge(&span, &param.span);
            }
            params
        } else {
            Vec::new()
        };

        // Parse return type (ONLY if ':' is present)
        let return_type = if self.current_token()?.token_type == TType::Colon {
            let colon_span = Span::from_token(self.current_token()?);
            span = Span::merge(&span, &colon_span);
            self.advance(); // Consume ':'

            let ty = self.parse_type()?;
            span = Span::merge(&span, &ty.span);
            Some(ty)
        } else {
            None // ⭐ No colon = void return
        };

        // Parse body (only if '{' is present)
        let body = if self.current_token()?.token_type == TType::LBrace {
            let body = self.parse_body()?;
            span = Span::merge(&span, &body.span);
            Some(Box::new(body))
        } else {
            None
        };

        Some(Stmt::new(
            StmtKind::FunctionDecl {
                name: Box::new(name),
                params,
                type_annotation: return_type, // None means void
                body,
            },
            span,
        ))
    }
}
