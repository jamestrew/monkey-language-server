mod precedence;
#[cfg(test)]
mod test;

use std::collections::VecDeque;

use self::precedence::Precedence;
use crate::ast::*;
use crate::diagnostics::{MonkeyError, SpannedError};
use crate::lexer::{token_result_span, Lexer, Token, TokenKind, TokenResult};
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
    prev_span: Spanned<Option<TokenKind>>,
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
        self.parent_fallback
            .map(|kind| {
                self.prev_span
                    .map(MonkeyError::UnexpectedToken(kind.to_string()))
            })
            .unwrap_or_else(|| self.prev_span.map(MonkeyError::UnexpectedEof))
    }

    fn next_token(&mut self) -> TokenResult<'source> {
        let ret = match self.curr_token.take() {
            Some(token_res) => {
                let prev_inner = match token_res {
                    Ok(ref token) => Some(token.kind),
                    Err(_) => None,
                };
                self.prev_span = token_result_span(&token_res, prev_inner);
                token_res
            }
            None => Err(self.premature_nil_curr_token_err()),
        };

        self.curr_token = self.peek_token.take();
        self.peek_token = self.token_provider.next();
        ret
    }

    fn prev_token_is<T: AsRef<TokenKind>>(&self, match_kind: T) -> bool {
        match *self.prev_span {
            Some(kind) => kind == *match_kind.as_ref(),
            None => false,
        }
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

    fn curr_token_is<T: AsRef<TokenKind>>(&self, match_kind: T) -> Result<bool, SpannedError> {
        Ok(self.curr_token_kind()? == *match_kind.as_ref())
    }

    fn unsafe_curr_token_is<T: AsRef<TokenKind>>(&mut self, match_kind: T) -> bool {
        match &self.curr_token {
            Some(Ok(token)) => token.kind == *match_kind.as_ref(),
            Some(Err(_)) => false,
            None => false,
        }
    }

    fn expect_curr<T: AsRef<TokenKind>>(&mut self, expect_kind: T) -> TokenResult<'source> {
        if self.curr_token.is_none() {
            return Err(self.prev_span.map(MonkeyError::ExpectedTokenNotFound(
                expect_kind.as_ref().to_string(),
            )));
        }

        let token = self.curr_token_ref()?;
        if token.kind == *expect_kind.as_ref() {
            self.next_token()
        } else {
            Err(token.map(MonkeyError::ExpectedTokenNotFound(
                expect_kind.as_ref().to_string(),
            )))
        }
    }

    fn take_semicolons(&mut self) -> Result<(), SpannedError> {
        while self.curr_token.is_some() && self.unsafe_curr_token_is(TokenKind::Semicolon) {
            self.next_token()?;
        }
        Ok(())
    }

    fn take_token<T: AsRef<TokenKind>>(&mut self, kind: T) -> Result<(), SpannedError> {
        while self.curr_token.is_some() && !self.unsafe_curr_token_is(kind.as_ref()) {
            self.next_token()?;
        }
        if self.curr_token.is_some() {
            self.next_token()?;
        }
        Ok(())
    }

    fn take_bad_tokens(&mut self) {
        while matches!(self.curr_token, Some(Err(_))) {
            let _ = self.next_token();
        }
    }

    fn sync(&mut self) -> Result<(), SpannedError> {
        self.take_bad_tokens();
        match self.fallback_tokens.pop() {
            Some(token) => self.take_token(token),
            None => self.take_token(TokenKind::Semicolon),
        }
    }

    fn curr_precendence(&self) -> Precedence {
        match &self.curr_token {
            Some(Ok(token)) => Precedence::from(token),
            _ => Precedence::Lowest,
        }
    }

    fn parse_statement(&mut self) -> StmtResult<'source> {
        let token = self.next_token()?;

        let result = match token.kind {
            TokenKind::Let => self.parse_let_statement(token),
            TokenKind::Return => self.parse_return_statement(token),
            _ => {
                let expr = self
                    .parse_expression_statement(token, Precedence::Lowest)
                    .map(|expr| expr.into());

                self.take_semicolons()?;

                expr
            }
        };

        if result.is_err() {
            self.sync()?;
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

        println!("{:?}", self.curr_token);

        // if self.curr_token.is_some() {
        self.expect_curr(TokenKind::Semicolon)?;
        self.take_semicolons()?;
        // }

        self.fallback_tokens.pop();
        Ok(let_stmt)
    }

    fn parse_return_statement(&mut self, token: Token<'source>) -> StmtResult<'source> {
        self.fallback_tokens.push(TokenKind::Semicolon);

        let value = if self.unsafe_curr_token_is(TokenKind::Semicolon) {
            None
        } else {
            match self.next_token() {
                Ok(token) => Some(self.parse_expression_statement(token, Precedence::Lowest)),
                Err(err) => Some(Err(err)),
            }
        };

        let return_stmt = Return::new(token, value).into();
        if self.curr_token.is_some() {
            self.expect_curr(TokenKind::Semicolon)?;
            self.take_semicolons()?;
        }

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
            TokenKind::LBracket => self.parse_array(token)?,
            TokenKind::LBrace => self.parse_hash(token)?,
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
                TokenKind::LParen => self.parse_fn_call(expr, op_token)?,
                TokenKind::LBracket => self.parse_index(expr, op_token)?,
                kind => todo!("infix for {kind:?}"),
            };
        }

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
        let condition = self.parse_if_condition();
        if condition.is_err() && !self.prev_token_is(TokenKind::RParen) {
            self.sync()?;
        }
        let consequence = self.parse_block();
        if consequence.is_err() && !self.prev_token_is(TokenKind::RBrace) {
            self.sync()?;
        }
        let alternative = self.parse_if_alternative();
        if alternative.is_err() && !self.prev_token_is(TokenKind::RBrace) {
            self.sync()?;
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
        let alternative = if self.unsafe_curr_token_is(TokenKind::Else) {
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
        let mut brace_count = 1;
        loop {
            if self.curr_token.is_none() {
                return Err(self.premature_nil_curr_token_err());
            }
            if self.curr_token_is(TokenKind::LBrace)? {
                brace_count += 1;
            }
            if self.curr_token_is(TokenKind::RBrace)? {
                brace_count -= 1;
                if brace_count == 0 {
                    break;
                }
            }
            block_tokens.push_back(self.next_token())
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
            self.sync()?;
        }

        let body = self.parse_block();
        if body.is_err() {
            self.sync()?;
        }
        Ok(Function::new(fn_token, params, body).into())
    }

    fn parse_fn_params(&mut self) -> Result<Vec<ExprResult<'source>>, SpannedError> {
        self.fallback_tokens.push(TokenKind::RParen);
        self.expect_curr(TokenKind::LParen)?;

        let mut param_tokens = VecDeque::new();
        loop {
            if self.curr_token_is(TokenKind::LBrace)? || self.curr_token.is_none() {
                return Err(self
                    .prev_span
                    .map(MonkeyError::ExpectedTokenNotFound(")".to_string())));
            }
            if self.curr_token_is(TokenKind::RParen)? {
                break;
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
                Err(err) => {
                    idents.push(Err(err));
                    break;
                }
            };

            if self.curr_token.is_some() {
                if let Err(err) = self.expect_curr(TokenKind::Comma) {
                    idents.push(Err(err));
                    break;
                }
            }
        }

        Ok(idents)
    }

    fn parse_fn_call(
        &mut self,
        func: Expression<'source>,
        op_token: Token<'source>,
    ) -> ExprResult<'source> {
        let args = self.parse_fn_call_args();
        if args.is_err() {
            self.sync()?;
        }

        Ok(Call::new(op_token, func, args).into())
    }

    fn parse_fn_call_args(&mut self) -> Result<Vec<ExprResult<'source>>, SpannedError> {
        self.fallback_tokens.push(TokenKind::RParen);

        let mut arg_tokens = VecDeque::new();
        let mut paren_count = 1;
        loop {
            if self.curr_token_is(TokenKind::LBrace)? || self.curr_token.is_none() {
                return Err(self
                    .prev_span
                    .map(MonkeyError::ExpectedTokenNotFound(")".to_string())));
            }
            if self.curr_token_is(TokenKind::LParen)? {
                paren_count += 1;
            }
            if self.curr_token_is(TokenKind::RParen)? {
                paren_count -= 1;
                if paren_count == 0 {
                    break;
                }
            }

            arg_tokens.push_back(self.next_token());
        }

        let args = if arg_tokens.is_empty() {
            Ok(Vec::new())
        } else {
            let mut sub_parser = Parser::from_token_provider(arg_tokens, Some(TokenKind::RParen));
            sub_parser.parse_comma_sep_expr()
        };

        self.expect_curr(TokenKind::RParen)?;
        self.fallback_tokens.pop();
        args
    }

    fn parse_comma_sep_expr(&mut self) -> VecExprResult<'source> {
        let mut exprs = Vec::new();

        while self.curr_token.is_some() {
            let token = match self.next_token() {
                Ok(token) => token,
                Err(err) => {
                    exprs.push(Err(err));
                    break;
                }
            };
            match self.parse_expression_statement(token, Precedence::Lowest) {
                Ok(expr) => exprs.push(Ok(expr)),
                Err(err) => {
                    exprs.push(Err(err));
                    break;
                }
            }

            if self.curr_token.is_some() {
                if let Err(err) = self.expect_curr(TokenKind::Comma) {
                    exprs.push(Err(err));
                    break;
                }
            }
        }

        Ok(exprs)
    }

    fn parse_index(
        &mut self,
        expr: Expression<'source>,
        op_token: Token<'source>,
    ) -> ExprResult<'source> {
        self.fallback_tokens.push(TokenKind::RBracket);
        let index_expr = match self.next_token() {
            Ok(token) => self.parse_expression_statement(token, Precedence::Lowest),
            Err(err) => Err(err),
        };
        self.expect_curr(TokenKind::RBracket)?;
        self.fallback_tokens.pop();
        Ok(Index::new(op_token, expr, index_expr).into())
    }

    fn parse_array(&mut self, bracket: Token<'source>) -> ExprResult<'source> {
        self.fallback_tokens.push(TokenKind::RBracket);

        let mut elem_tokens = VecDeque::new();
        let mut bracket_count = 1;
        loop {
            if self.curr_token.is_none() {
                return Err(self
                    .prev_span
                    .map(MonkeyError::ExpectedTokenNotFound("]".to_string())));
            }
            if let Some(Err(ref err)) = self.curr_token {
                elem_tokens.push_back(Err(err.clone_inner()));
                let _ = self.next_token();
                continue;
            }
            if self.curr_token_is(TokenKind::LBracket)? {
                bracket_count += 1;
            }
            if self.curr_token_is(TokenKind::RBracket)? {
                bracket_count -= 1;
                if bracket_count == 0 {
                    break;
                }
            }
            elem_tokens.push_back(self.next_token())
        }

        let elems = if elem_tokens.is_empty() {
            Ok(Vec::new())
        } else {
            let mut sub_parser =
                Parser::from_token_provider(elem_tokens, Some(TokenKind::RBracket));
            sub_parser.parse_comma_sep_expr()
        };

        self.expect_curr(TokenKind::RBracket)?;
        self.fallback_tokens.pop();
        Ok(Array::new(bracket, elems).into())
    }

    fn parse_hash(&mut self, brace: Token<'source>) -> ExprResult<'source> {
        self.fallback_tokens.push(TokenKind::RBrace);

        let mut hash_tokens = VecDeque::new();
        let mut brace_count = 1;
        loop {
            if self.curr_token.is_none() {
                return Err(self
                    .prev_span
                    .map(MonkeyError::ExpectedTokenNotFound("}".to_string())));
            }
            if let Some(Err(ref err)) = self.curr_token {
                hash_tokens.push_back(Err(err.clone_inner()));
                let _ = self.next_token();
                continue;
            }
            if self.curr_token_is(TokenKind::LBrace)? {
                brace_count += 1;
            }
            if self.curr_token_is(TokenKind::RBrace)? {
                brace_count -= 1;
                if brace_count == 0 {
                    break;
                }
            }

            hash_tokens.push_back(self.next_token())
        }

        let kv_pairs = if hash_tokens.is_empty() {
            Ok(Vec::new())
        } else {
            let mut sub_parser = Parser::from_token_provider(hash_tokens, Some(TokenKind::RBrace));
            sub_parser.parse_kv_pairs()
        };

        self.expect_curr(TokenKind::RBrace)?;
        self.fallback_tokens.pop();

        Ok(Hash::new(brace, kv_pairs).into())
    }

    fn parse_kv_pairs(
        &mut self,
    ) -> Result<Vec<Result<ExprPairs<'source>, SpannedError>>, SpannedError> {
        let mut pairs = Vec::new();

        while self.curr_token.is_some() {
            let key_token = match self.next_token() {
                Ok(token) => token,
                Err(err) => {
                    pairs.push(Err(err));
                    break;
                }
            };
            let key = match self.parse_expression_statement(key_token, Precedence::Lowest) {
                Ok(expr) => expr,
                Err(err) => {
                    pairs.push(Err(err));
                    break;
                }
            };

            if let Err(err) = self.expect_curr(TokenKind::Colon) {
                pairs.push(Err(err));
                break;
            }

            let value_token = self.next_token()?;
            let value = match self.parse_expression_statement(value_token, Precedence::Lowest) {
                Ok(expr) => expr,
                Err(err) => {
                    pairs.push(Err(err));
                    break;
                }
            };

            pairs.push(Ok((key, value)));

            if self.curr_token.is_some() {
                if let Err(err) = self.expect_curr(TokenKind::Comma) {
                    pairs.push(Err(err));
                    break;
                }
            }
        }

        Ok(pairs)
    }
}
