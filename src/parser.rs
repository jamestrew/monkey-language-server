#![allow(dead_code)]
use crate::ast::*;
use crate::diagnostics::{MonkeyError, SpannedError};
use crate::lexer::{token_result_span, Lexer, Token, TokenKind, TokenResult};
use crate::precedence::Precedence;
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
                    self.next_token()?;
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

    fn curr_precendence(&self) -> Precedence {
        match &self.curr_token {
            Some(Ok(token)) => Precedence::from(token),
            _ => Precedence::Lowest,
        }
    }

    fn peek_precendence(&self) -> Precedence {
        match &self.peek_token {
            Some(Ok(token)) => Precedence::from(token),
            _ => Precedence::Lowest,
        }
    }

    fn parse_statement(&mut self) -> StmtResult<'source> {
        let token = self.next_token()?;

        let result = match token.kind {
            TokenKind::Let => self.parse_let_statement(token),
            TokenKind::Return => self.parse_return_statement(token),
            _ => self
                .parse_expression_statement(token, Precedence::Lowest)
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
            self.parse_expression_statement(value_token, Precedence::Lowest)?,
        )
        .into();
        self.eat_semicolons()?;
        Ok(let_stmt)
    }

    fn parse_return_statement(&mut self, token: Token<'source>) -> StmtResult<'source> {
        let value = if self.current_token_is(TokenKind::Semicolon)? {
            None
        } else {
            let value_token = self.next_token()?;
            Some(self.parse_expression_statement(value_token, Precedence::Lowest)?)
        };
        let return_stmt = Return::new(token, value).into();
        self.eat_semicolons()?;
        Ok(return_stmt)
    }

    fn parse_expression_statement(
        &mut self,
        token: Token<'source>,
        precedence: Precedence,
    ) -> ExprResult<'source> {
        let mut expr = match &token.kind {
            TokenKind::Identifier => Identifier::from(token).into(),
            TokenKind::Int | TokenKind::True | TokenKind::False | TokenKind::Nil => {
                Primative::from(token).into()
            }
            TokenKind::Str => StringLiteral::from(token).into(),
            TokenKind::Minus | TokenKind::Bang => self.parse_prefix(token)?,
            TokenKind::If => self.parse_if(token)?,
            TokenKind::LParen => self.parse_grouped()?,
            _ => return Err(token.map(MonkeyError::UnexpectedToken(token.slice.into()))),
        };

        while precedence < self.curr_precendence() {
            let op_precedence = self.curr_precendence();
            let op_token = self.next_token()?;
            expr = match op_token.kind {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterisk
                | TokenKind::ForwardSlash
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::LT
                | TokenKind::GT => self.parse_infix(expr, op_token, op_precedence)?,
                _ => todo!(),
            };
        }

        self.eat_semicolons()?;
        Ok(expr)
    }

    fn parse_prefix(&mut self, token: Token<'source>) -> ExprResult<'source> {
        let right_token = self.next_token()?;
        let ret = Ok(Prefix::new(
            token,
            self.parse_expression_statement(right_token, Precedence::Prefix)?,
        )
        .into());

        ret
    }

    fn parse_infix(
        &mut self,
        left: Expression<'source>,
        next: Token<'source>,
        op_precedence: Precedence,
    ) -> ExprResult<'source> {
        let right_token = self.next_token()?;
        let ret = Ok(Infix::new(
            next,
            left,
            self.parse_expression_statement(right_token, op_precedence)?,
        )
        .into());

        ret
    }

    fn parse_if(&mut self, token: Token<'source>) -> ExprResult<'source> {
        let condition = self.expect_current(TokenKind::LParen)?;
        let condition = self.parse_expression_statement(condition, Precedence::Lowest)?;

        let block = self.expect_current(TokenKind::LBrace)?;
        let consequence = self.parse_block(block)?;

        let alternative = if self.current_token_is(TokenKind::Else)? {
            let block = self.expect_peek(TokenKind::LBrace)?;
            Some(self.parse_block(block)?)
        } else {
            None
        };
        Ok(If::new(token, condition, consequence, alternative).into())
    }

    fn parse_grouped(&mut self) -> ExprResult<'source> {
        self.current_token_is(TokenKind::LParen)?;
        let expr_start = self.next_token()?;
        let result = self.parse_expression_statement(expr_start, Precedence::Lowest);
        self.expect_current(TokenKind::RParen)?;
        result
    }

    fn parse_block(&mut self, token: Token<'source>) -> Result<Block<'source>, SpannedError> {
        let mut stmts = Vec::new();
        while !self.current_token_is(TokenKind::RBrace)? {
            stmts.push(self.parse_statement())
        }

        self.expect_current(TokenKind::RBrace)?;
        Ok(Block::new(token, stmts))
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

    debug_snapshot!(prefix_happy_1, "-1");
    debug_snapshot!(prefix_happy_2, "!true");
    debug_snapshot!(prefix_happy_3, "let b = !true");

    debug_snapshot!(infix_happy_1, "1 + 1;\na - 2;\n1 * 1;\n1 / 2;");
    debug_snapshot!(
        infix_happy_2,
        "\"foo\" == a;\n1 == 1;\n2 != 1;\n4 < 5;\n 5 > 4;"
    );
    debug_snapshot!(infix_unhappy_1, "1 + +;");
    debug_snapshot!(infix_unhappy_2, "1 +;");
    debug_snapshot!(infix_unhappy_3, "1 + /");

    debug_snapshot!(grouped_expr, "(1 + 1)");

    debug_snapshot!(operator_precedence_1, "-a * b");
    debug_snapshot!(operator_precedence_2, "!-a");
    debug_snapshot!(operator_precedence_3, "a + b + c");
    debug_snapshot!(operator_precedence_4, "a + b - c");
    debug_snapshot!(operator_precedence_5, "a * b * c");
    debug_snapshot!(operator_precedence_6, "a * b / c");
    debug_snapshot!(operator_precedence_7, "a + b / c");
    debug_snapshot!(operator_precedence_8, "a / b + c");
    debug_snapshot!(operator_precedence_9, "a + b * c + d / e - f");
    debug_snapshot!(operator_precedence_10, "5 > 4 == 3 < 4");
    debug_snapshot!(operator_precedence_11, "5 < 4 != 3 > 4");
    debug_snapshot!(operator_precedence_12, "3 + 4 * 5 == 3 * 1 + 4 * 5");
    debug_snapshot!(operator_precedence_13, "true");
    debug_snapshot!(operator_precedence_14, "false");
    debug_snapshot!(operator_precedence_15, "3 > 5 == false");
    debug_snapshot!(operator_precedence_16, "3 < 5 == true");
    debug_snapshot!(operator_precedence_17, "1 + (2 + 3) + 4");
    debug_snapshot!(operator_precedence_19, "(5 + 5) * 2");
    debug_snapshot!(operator_precedence_21, "2 / (5 + 5)");
    debug_snapshot!(operator_precedence_23, "-(5 + 5)");
    debug_snapshot!(operator_precedence_25, "!(true == true)");
    // debug_snapshot!(operator_precedence_27, "a + add(b * c) + d");
    // debug_snapshot!(
    //     operator_precedence_22,
    //     "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"
    // );
    // debug_snapshot!(operator_precedence_24, "add(a + b + c * d / f + g)");
    // debug_snapshot!(operator_precedence_26, "a * [1, 2, 3, 4][b * c] * d");
    // debug_snapshot!(operator_precedence_28, "add(a * b[2], b[1], 2 * [1, 2][1])");

    debug_snapshot!(if_expr_1, "if (x) { x }");
    debug_snapshot!(if_expr_2, "if (x < y) { x }");
    debug_snapshot!(if_expr_3, "if (x < y) { x } else { y }");
    debug_snapshot!(if_expr_4, "if (x < y) { x } else { let z = x + y; z }");
}
