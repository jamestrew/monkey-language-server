use crate::ast::Node;
use crate::lexer::{Lexer, TokenResult};

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    curr_token: Option<TokenResult<'source>>,
    next_token: Option<TokenResult<'source>>,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Parser {
        let mut lexer = Lexer::new(source);
        let curr_token = lexer.next();
        let next_token = lexer.next();

        Self {
            lexer,
            curr_token,
            next_token,
        }
    }

    pub fn parse_program(&mut self) -> Vec<Node> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use std::ops::Range;

    use super::*;
    use crate::lexer::*;
    use crate::types::*;

    pub fn debug_new<'source>(
        start: Position,
        end: Position,
        span: Range<usize>,
        kind: TokenKind,
        slice: &'source str,
    ) -> Token<'source> {
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
        assert_eq!(parser.next_token, Some(Ok(second)));
    }
}
