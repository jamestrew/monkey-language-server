#![allow(dead_code)]
use crate::ast::{
    ExprResult, Identifier, Let, Node, Primative, Program, Return, StmtResult, StringLiteral,
};
use crate::errors::{MonkeyError, SpannedError};
use crate::lexer::{token_result_span, Lexer, Token, TokenKind, TokenResult};
use crate::types::Spanned;

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    curr_token: Option<TokenResult<'source>>,
    peek_token: Option<TokenResult<'source>>,
    last_span: Spanned<()>,
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
            last_span: Spanned::default(),
        }
    }

    pub fn parse_program(&mut self) -> Program<'source> {
        let mut nodes = Vec::new();

        while self.curr_token.is_some() {
            match self.parse_statement() {
                Ok(stmt) => nodes.push(Node::Statement(stmt)),
                Err(err) => nodes.push(Node::Error(err)),
            }
        }

        Program { nodes }
    }

    fn next_token(&mut self) -> TokenResult<'source> {
        let ret = match self.curr_token.take() {
            Some(token_res) => {
                self.last_span = token_result_span(&token_res, ());
                token_res
            }
            None => Err(self.last_span.map(MonkeyError::UnexpectedEof)),
        };

        self.curr_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
        ret
    }

    fn current_token_is<T: AsRef<TokenKind>>(
        &mut self,
        match_token: T,
    ) -> Result<bool, SpannedError> {
        match self.curr_token.as_ref() {
            Some(Ok(token)) => Ok(token.kind == *match_token.as_ref()),
            Some(Err(err)) => Err(err.clone_inner()),
            None => Ok(false),
        }
    }

    fn peek_token_is<T: AsRef<TokenKind>>(&mut self, match_token: T) -> Result<bool, SpannedError> {
        match &self.curr_token {
            Some(Ok(ref token)) => Ok(*match_token.as_ref() == token.kind),
            Some(Err(err)) => Err(err.clone_inner()),
            None => Ok(false),
        }
    }

    fn expect_current<T: AsRef<TokenKind>>(&mut self, expect_token: T) -> TokenResult<'source> {
        match &self.curr_token {
            Some(Ok(token)) => {
                if token.kind == *expect_token.as_ref() {
                    self.next_token()
                } else {
                    Err(token.map(MonkeyError::ExpectedTokenNotFound(
                        expect_token.as_ref().to_string(),
                    )))
                }
            }
            Some(Err(err)) => Err(err.clone_inner()),
            None => Err(self.last_span.map(MonkeyError::UnexpectedEof)),
        }
    }

    fn expect_peek<T: AsRef<TokenKind>>(&mut self, expect_token: T) -> TokenResult<'source> {
        match &self.peek_token {
            Some(Ok(ref token)) => {
                if token.kind == *expect_token.as_ref() {
                    self.next_token()
                } else {
                    Err(token.map(MonkeyError::ExpectedTokenNotFound(token.slice.into())))
                }
            }
            Some(Err(err)) => Err(err.clone_inner()),
            None => {
                // token_result_span(&self.curr_token.expect("have valid curr_token"), ());
                todo!();
            }
        }
    }

    fn eat_semicolons(&mut self) -> Result<(), SpannedError> {
        while self.peek_token_is(TokenKind::Semicolon)? {
            self.next_token()?;
        }
        Ok(())
    }

    fn parse_statement(&mut self) -> StmtResult<'source> {
        let token = self.next_token()?;

        let result = match token.kind {
            TokenKind::Let => self.parse_let_statement(token),
            TokenKind::Return => self.parse_return_statement(token),
            _ => self
                .parse_expression_statement(token)
                .map(|expr| expr.into()),
        };

        if result.is_err() {
            self.eat_semicolons()?;
        }
        result
    }

    fn parse_let_statement(&mut self, token: Token<'source>) -> StmtResult<'source> {
        let ident_token = self.expect_current(TokenKind::Identifier)?;
        self.expect_current(TokenKind::Assign)?;
        let value_token = self.next_token()?;

        let let_stmt = Let::new(
            token,
            Identifier::from(ident_token),
            self.parse_expression_statement(value_token)?,
        )
        .into();
        self.eat_semicolons()?;
        Ok(let_stmt)
    }

    fn parse_return_statement(&mut self, token: Token<'source>) -> StmtResult<'source> {
        let value;
        if self.current_token_is(TokenKind::Semicolon)? {
            value = None;
        } else {
            let value_token = self.next_token()?;
            value = Some(self.parse_expression_statement(value_token)?);
        }
        let return_stmt = Return::new(token, value).into();
        self.eat_semicolons()?;
        Ok(return_stmt)
    }

    fn parse_expression_statement(&mut self, token: Token<'source>) -> ExprResult<'source> {
        match &token.kind {
            TokenKind::Identifier => Ok(Identifier::from(token).into()),
            TokenKind::Int | TokenKind::True | TokenKind::False | TokenKind::Nil => {
                Ok(Primative::from(token).into())
            }
            TokenKind::Str => Ok(StringLiteral::from(token).into()),
            _ => Err(token.map(MonkeyError::UnexpectedToken(token.slice.into()))),
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

    // maybe these are warnings? "Expression value is unused" <- python
    debug_snapshot!(int_expr, "123");
    debug_snapshot!(nil_expr, "nil");
    debug_snapshot!(string_literal, "\"hello world\"");
    debug_snapshot!(identifier, "x");

    debug_snapshot!(let_statement_1, "let x = 1;");
    debug_snapshot!(let_statement_2, "let x = 2;;;");
    debug_snapshot!(let_statement_broken_1, "let x = ;");
    debug_snapshot!(let_statement_broken_2, "let x;");
    debug_snapshot!(let_statement_broken_3, "let;");
    // debug_snapshot!(let_statement_broken_4, "le a = 1;");

    debug_snapshot!(return_happy_1, "return 1;");
    debug_snapshot!(return_happy_2, "return nil;");
    debug_snapshot!(return_happy_3, "return;");
}
