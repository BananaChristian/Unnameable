use crate::parser::{Parser, Stmt, precedence::Precedence};

impl Parser{
    pub fn parse_stmt(&mut self) -> Result<Stmt,String>{
        let expr=self.parse_expression(Precedence::Lowest)?;
        Ok(Stmt::Expr(expr))
    }
}
