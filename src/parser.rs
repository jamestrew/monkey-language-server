#![allow(dead_code)]
use std::collections::VecDeque;

use crate::ast::*;
use crate::diagnostics::{MonkeyError, SpannedError};
use crate::lexer::{token_result_span, Lexer, Token, TokenKind, TokenResult};
use crate::precedence::Precedence;
use crate::types::Spanned;

pub trait TokenProvider<'source> {
    fn next(&mut self) -> Option<TokenResult<'source>>;
}

impl<'source> TokenProvider<'source> for VecDeque<TokenResult<'source>> {
    fn next(&mut self) -> Option<TokenResult<'source>> {
        self.pop_front()
    }
}

pub struct Parser<'source, TP: TokenProvider<'source>> {
    token_provider: TP,
    curr_token: Option<TokenResult<'source>>,
    peek_token: Option<TokenResult<'source>>,
    prev_span: Spanned<()>,
    fallback_tokens: Vec<TokenKind>,
    parent_fallback: Option<TokenKind>,
}

impl<'source> Parser<'source, Lexer<'source>> {
    pub fn from_source(source: &'source str) -> Self {
        let lexer = Lexer::new(source);
        Self::from_token_provider(lexer, None)
    }
}

impl<'source, TP: TokenProvider<'source>> Parser<'source, TP> {
    fn from_token_provider(mut token_provider: TP, parent_fallback: Option<TokenKind>) -> Self {
        let curr_token = token_provider.next();
        let peek_token = token_provider.next();

        Self {
            token_provider,
            curr_token,
            peek_token,
            prev_span: Spanned::default(),
            fallback_tokens: Vec::new(),
            parent_fallback,
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

    fn premature_nil_curr_token_err(&self) -> SpannedError {
        match self.parent_fallback {
            Some(kind) => self
                .prev_span
                .map(MonkeyError::UnexpectedToken(kind.to_string())),
            None => self.prev_span.map(MonkeyError::UnexpectedEof),
        }
    }

    fn premature_nil_peek_token_err(&self) -> SpannedError {
        let err = match self.parent_fallback {
            Some(kind) => MonkeyError::UnexpectedToken(kind.to_string()),
            None => MonkeyError::UnexpectedEof,
        };

        if let Some(token_res) = &self.curr_token {
            match token_res {
                Ok(token) => token.map(err),
                Err(orig_err) => orig_err.map(err),
            }
        } else {
            self.prev_span.map(err)
        }
    }

    fn next_token(&mut self) -> TokenResult<'source> {
        let ret = match self.curr_token.take() {
            Some(token_res) => {
                self.prev_span = token_result_span(&token_res, ());
                token_res
            }
            None => Err(self.premature_nil_curr_token_err()),
        };

        self.curr_token = self.peek_token.take();
        self.peek_token = self.token_provider.next();
        ret
    }

    fn curr_token_ref(&self) -> Result<&Token, SpannedError> {
        match &self.curr_token {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err.clone_inner()),
            None => Err(self.premature_nil_curr_token_err()),
        }
    }

    fn curr_token_kind(&self) -> Result<TokenKind, SpannedError> {
        Ok(self.curr_token_ref()?.kind)
    }

    fn curr_token_is<T: AsRef<TokenKind>>(&mut self, match_kind: T) -> Result<bool, SpannedError> {
        Ok(self.curr_token_kind()? == *match_kind.as_ref())
    }

    fn unsafe_curr_token_is<T: AsRef<TokenKind>>(
        &mut self,
        match_kind: T,
    ) -> Result<bool, SpannedError> {
        if self.curr_token.is_none() {
            return Ok(false);
        }
        Ok(self.curr_token_kind()? == *match_kind.as_ref())
    }

    fn peek_token_ref(&self) -> Result<&Token, SpannedError> {
        match &self.peek_token {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err.clone_inner()),
            None => Err(self.premature_nil_peek_token_err()),
        }
    }

    fn peek_token_kind(&self) -> Result<TokenKind, SpannedError> {
        Ok(self.peek_token_ref()?.kind)
    }

    fn peek_token_is<T: AsRef<TokenKind>>(&mut self, match_kind: T) -> Result<bool, SpannedError> {
        Ok(self.peek_token_kind()? == *match_kind.as_ref())
    }

    fn unsafe_peek_token_is<T: AsRef<TokenKind>>(
        &mut self,
        match_kind: T,
    ) -> Result<bool, SpannedError> {
        if self.peek_token.is_none() {
            return Ok(false);
        }
        Ok(self.peek_token_kind()? == *match_kind.as_ref())
    }

    fn expect_curr<T: AsRef<TokenKind>>(&mut self, expect_kind: T) -> TokenResult<'source> {
        match self.curr_token_ref() {
            Ok(token) => {
                if token.kind == *expect_kind.as_ref() {
                    self.next_token()
                } else {
                    Err(token.map(MonkeyError::ExpectedTokenNotFound(
                        expect_kind.as_ref().to_string(),
                    )))
                }
            }
            Err(err) => Err(err),
        }
    }

    fn expect_peek<T: AsRef<TokenKind>>(&mut self, expect_kind: T) -> TokenResult<'source> {
        match &self.peek_token {
            Some(Ok(ref token)) => {
                if token.kind == *expect_kind.as_ref() {
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

    fn take_semicolons(&mut self) -> Result<(), SpannedError> {
        while self.curr_token.is_some() && self.unsafe_curr_token_is(TokenKind::Semicolon)? {
            self.next_token()?;
        }
        Ok(())
    }

    fn take_util<T: AsRef<TokenKind>>(&mut self, kind: T) -> Result<(), SpannedError> {
        while self.curr_token.is_some() && self.unsafe_curr_token_is(&kind)? {
            self.next_token()?;
        }
        Ok(())
    }

    fn recover(&mut self) -> Result<(), SpannedError> {
        match self.fallback_tokens.last() {
            Some(token) => self.take_util(*token),
            None => Ok(()),
        }
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
            self.recover()?;
        }
        result
    }

    fn parse_let_statement(&mut self, token: Token<'source>) -> StmtResult<'source> {
        self.fallback_tokens.push(TokenKind::Semicolon);

        let ident_token = self.expect_curr(TokenKind::Identifier)?;
        self.expect_curr(TokenKind::Assign)?;
        let value_token = self.next_token()?;

        let let_stmt = Let::new(
            token,
            Identifier::from(ident_token),
            self.parse_expression_statement(value_token, Precedence::Lowest)?,
        )
        .into();

        self.take_semicolons()?;
        self.fallback_tokens.pop();
        Ok(let_stmt)
    }

    fn parse_return_statement(&mut self, token: Token<'source>) -> StmtResult<'source> {
        self.fallback_tokens.push(TokenKind::Semicolon);

        let value = if self.unsafe_curr_token_is(TokenKind::Semicolon)? {
            None
        } else {
            let value_token = self.next_token()?;
            Some(self.parse_expression_statement(value_token, Precedence::Lowest)?)
        };
        let return_stmt = Return::new(token, value).into();
        self.take_semicolons()?; // hmmmm.... not sure about how i'm doing this

        self.fallback_tokens.pop();
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
            TokenKind::Function => self.parse_function(token)?,
            kind => return Err(token.map(MonkeyError::UnexpectedToken(kind.to_string()))),
        };

        while precedence < self.curr_precendence() {
            let op_precedence = self.curr_precendence();
            let op_token = self.next_token()?;
            expr = match &op_token.kind {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterisk
                | TokenKind::ForwardSlash
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::LT
                | TokenKind::GT => self.parse_infix(expr, op_token, op_precedence)?,
                kind => todo!("infix for {kind:?}"),
            };
        }

        self.take_semicolons()?;
        Ok(expr)
    }

    fn parse_prefix(&mut self, token: Token<'source>) -> ExprResult<'source> {
        self.fallback_tokens.push(TokenKind::Semicolon);
        let right_token = self.next_token()?;
        let ret = Ok(Prefix::new(
            token,
            self.parse_expression_statement(right_token, Precedence::Prefix)?,
        )
        .into());

        self.fallback_tokens.pop();
        ret
    }

    fn parse_infix(
        &mut self,
        left: Expression<'source>,
        next: Token<'source>,
        op_precedence: Precedence,
    ) -> ExprResult<'source> {
        self.fallback_tokens.push(TokenKind::Semicolon);

        let right_token = self.next_token()?;
        let ret = Ok(Infix::new(
            next,
            left,
            self.parse_expression_statement(right_token, op_precedence)?,
        )
        .into());

        self.fallback_tokens.pop();
        ret
    }

    fn parse_if(&mut self, token: Token<'source>) -> ExprResult<'source> {
        let condition = self.parse_if_condition();
        if condition.is_err() {
            self.recover()?;
        }
        let consequence = self.parse_block();
        if consequence.is_err() {
            self.recover()?;
        }
        let alternative = self.parse_if_alternative();
        if alternative.is_err() {
            self.recover()?;
        }
        Ok(If::new(token, condition, consequence, alternative).into())
    }

    fn parse_if_condition(&mut self) -> ExprResult<'source> {
        self.fallback_tokens.push(TokenKind::RParen);
        let condition = self.expect_curr(TokenKind::LParen)?;
        let condition = self.parse_expression_statement(condition, Precedence::Lowest);
        self.fallback_tokens.pop();
        condition
    }

    fn parse_if_alternative(&mut self) -> Result<Option<Block<'source>>, SpannedError> {
        self.fallback_tokens.push(TokenKind::RBrace);
        let alternative = if self.unsafe_curr_token_is(TokenKind::Else)? {
            self.next_token()?;
            Some(self.parse_block()?)
        } else {
            None
        };
        self.fallback_tokens.pop();
        Ok(alternative)
    }

    fn parse_grouped(&mut self) -> ExprResult<'source> {
        self.fallback_tokens.push(TokenKind::RParen);
        self.curr_token_is(TokenKind::LParen)?;
        let expr_start = self.next_token()?;
        let result = self.parse_expression_statement(expr_start, Precedence::Lowest)?;
        self.expect_curr(TokenKind::RParen)?;
        self.fallback_tokens.pop();
        Ok(result)
    }

    fn parse_block(&mut self) -> BlockResult<'source> {
        self.fallback_tokens.push(TokenKind::RBrace);
        let block_token = self.expect_curr(TokenKind::LBrace)?;

        let mut block_tokens = VecDeque::new();
        while self.curr_token.is_some() && !self.curr_token_is(TokenKind::RBrace)? {
            block_tokens.push_back(self.next_token());
        }

        let mut sub_parser = Parser::from_token_provider(block_tokens, Some(TokenKind::RBrace));
        let stmts = sub_parser.parse_program().nodes;

        self.expect_curr(TokenKind::RBrace)?;
        self.fallback_tokens.pop();
        Ok(Block::new(block_token, stmts))
    }

    fn parse_function(&mut self, fn_token: Token<'source>) -> ExprResult<'source> {
        let params = self.parse_fn_params();
        if params.is_err() {
            self.recover()?;
        }

        let body = self.parse_block();
        if body.is_err() {
            self.recover()?;
        }
        Ok(Function::new(fn_token, params, body).into())
    }

    fn parse_fn_params(&mut self) -> Result<Vec<ExprResult<'source>>, SpannedError> {
        self.fallback_tokens.push(TokenKind::RParen);
        self.expect_curr(TokenKind::LParen)?;

        let mut param_tokens = VecDeque::new();
        let rparen_not_found = self
            .prev_span
            .map(MonkeyError::ExpectedTokenNotFound(")".to_string()));
        loop {
            if self.curr_token_is(TokenKind::LBrace)? {
                return Err(rparen_not_found);
            }
            if self.curr_token_is(TokenKind::RParen)? {
                break;
            }
            if self.curr_token.is_none() {
                return Err(rparen_not_found);
            }

            let token = self.next_token()?;
            match token.kind {
                TokenKind::Identifier | TokenKind::Comma => param_tokens.push_back(Ok(token)),
                kind => param_tokens.push_back(Err(
                    token.map(MonkeyError::UnexpectedToken(kind.to_string()))
                )),
            }
        }

        let params = if param_tokens.is_empty() {
            Ok(Vec::new())
        } else {
            let mut sub_parser = Parser::from_token_provider(param_tokens, Some(TokenKind::RParen));
            sub_parser.parse_comma_sep_idents()
        };

        self.expect_curr(TokenKind::RParen)?;
        self.fallback_tokens.pop();
        params
    }

    fn parse_comma_sep_idents(&mut self) -> Result<Vec<ExprResult<'source>>, SpannedError> {
        let mut idents = Vec::new();

        while self.curr_token.is_some() {
            match self.expect_curr(TokenKind::Identifier) {
                Ok(token) => idents.push(Ok(Identifier::from(token).into())),
                Err(err) => idents.push(Err(err)),
            };

            if self.unsafe_curr_token_is(TokenKind::Comma)? {
                self.next_token()?;
            }
        }

        Ok(idents)
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
        let parser = Parser::from_source(source);

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
                let stmts = Parser::from_source($input).parse_program();
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
    debug_snapshot!(let_statement_broken_4, "le a = 1;");

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

    debug_snapshot!(if_expr_happy_1, "if (x) { x }");
    debug_snapshot!(if_expr_happy_2, "if (x < y) { x }");
    debug_snapshot!(if_expr_happy_3, "if (x < y) { x } else { y }");
    debug_snapshot!(
        if_expr_happy_4,
        "if (x < y) { x } else { let z = x + y; z }"
    );

    debug_snapshot!(if_expr_unhappy_1, "if (x +) { x }");
    debug_snapshot!(if_expr_unhappy_2, "if (x +) { x } else { x + 1 }");
    debug_snapshot!(if_expr_unhappy_3, "if (x) { let x = 1; x < }");
    debug_snapshot!(if_expr_unhappy_4, "if (x) { let x = 1");

    debug_snapshot!(fn_expr_happy_1, "fn() {}");
    debug_snapshot!(fn_expr_happy_2, "fn(x) {}");
    debug_snapshot!(fn_expr_happy_3, "fn(x, y, z) {}");
    debug_snapshot!(fn_expr_happy_4, "fn(x) { return x; }");
    debug_snapshot!(fn_expr_happy_5, "fn(x, y) { let z = x + y; return z; }");

    debug_snapshot!(fn_expr_unhappy_1, "fn( {}");
    debug_snapshot!(fn_expr_unhappy_2, "fn(1+1) {}");
}
