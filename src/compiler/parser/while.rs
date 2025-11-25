use crate::compiler::Parser;
use crate::compiler::parser::Node;
use crate::compiler::parser::NodeKind;
use crate::compiler::parser::node::ExprKind;
use crate::compiler::parser::precedence::BindingPower;

use generational_arena::Index;
use rootcause::Report;

use crate::compiler::tokens::Token;

impl<'a> Parser<'a> {
    pub fn parse_while(&mut self, while_token: Token) -> Result<Index, Report> {
        self.advance(); // consume 'while'

        // the value is an expression, parse it with full precedence.
        let cond = self.pratt_parse_expression(BindingPower::None)?;
        let loop_content = self.parse_block_body()?;
        let start_span = while_token.span;

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::While { cond, body: loop_content },
                type_: None,
            },
            start_span.connect_new(&self.current().span),
        )))
    }
}
