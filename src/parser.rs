#![allow(dead_code)]
use crate::ast::{Node, Primative, StringLiteral};
use crate::lexer::{Lexer, Token, TokenKind, TokenResult};

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    curr_token: Option<TokenResult<'source>>,
    peek_token: Option<TokenResult<'source>>,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Parser {
        let mut lexer = Lexer::new(source);
        let curr_token = lexer.next();
        let peek_token = lexer.next();

        Self {
            lexer,
            curr_token,
            peek_token,
        }
    }

    pub fn parse_program(&mut self) -> Vec<Node<'source>> {
        let mut nodes = Vec::new();

        while let Some(token_res) = self.curr_token.take() {
            let node = self.generate_node(token_res);
            nodes.push(node);
            self.next_token();
        }
        nodes
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    fn generate_node(&mut self, token_res: TokenResult<'source>) -> Node<'source> {
        if let Err(err) = token_res {
            return Node::Error(err);
        }

        let token = token_res.unwrap();
        match token.kind {
            TokenKind::Let => self.parse_let_statement(token),
            TokenKind::Return => self.parse_return_statement(token),
            _ => self.parse_expression_statement(token),
        }
    }

    fn parse_let_statement(&mut self, token: Token) -> Node<'source> {
        let _ = token;
        todo!("let statement")
    }

    fn parse_return_statement(&mut self, token: Token) -> Node<'source> {
        let _ = token;
        todo!("return statement")
    }

    fn parse_expression_statement(&mut self, token: Token<'source>) -> Node<'source> {
        match token.kind {
            TokenKind::Int | TokenKind::True | TokenKind::False | TokenKind::Nil => {
                Primative::from(token).into()
            }
            TokenKind::Str => StringLiteral::from(token).into(),
            _ => todo!("expression statement"),
        }
    }
}

#[cfg(test)]
mod test {
    use std::ops::Range;

    use super::*;
    use crate::lexer::*;
    use crate::types::*;

    pub fn debug_new(
        start: Position,
        end: Position,
        span: Range<usize>,
        kind: TokenKind,
        slice: &'_ str,
    ) -> Token<'_> {
        Spanned::new(start, end, span, _Token::new(kind, slice))
    }

    #[test]
    fn new_parser() {
        let source = "let a = 123;";
        let parser = Parser::new(source);

        let first = debug_new(
            Position::new(0, 0),
            Position::new(0, 3),
            0..3,
            TokenKind::Let,
            "let",
        );
        let second = debug_new(
            Position::new(0, 4),
            Position::new(0, 5),
            4..5,
            TokenKind::Identifier,
            "a",
        );

        assert_eq!(parser.curr_token, Some(Ok(first)));
        assert_eq!(parser.peek_token, Some(Ok(second)));
    }

    macro_rules! debug_snapshot {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let stmts = Parser::new($input).parse_program();
                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_debug_snapshot!(stmts);
                })
            }
        };
    }

    debug_snapshot!(lexer_erro, "@");
    debug_snapshot!(bool_expr, "true");
    debug_snapshot!(int_expr, "123");
    debug_snapshot!(nil_expr, "nil");
    debug_snapshot!(string_literal, "\"hello world\"");
}
