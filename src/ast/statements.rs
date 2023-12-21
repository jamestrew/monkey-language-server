use std::fmt::Debug;

use super::*;
use crate::diagnostics::SpannedError;
use crate::lexer::Token;

pub type StmtResult<'source> = Result<Statement<'source>, SpannedError>;
pub type BlockResult<'source> = Result<Block<'source>, SpannedError>;

#[derive(PartialEq)]
pub enum Statement<'source> {
    Let(Let<'source>),
    Return(Return<'source>),
    Block(Block<'source>),
    Expression(Expression<'source>),
}

impl<'source> Debug for Statement<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(arg0) => write!(f, "{:#?}", arg0),
            Self::Return(arg0) => write!(f, "{:#?}", arg0),
            Self::Block(arg0) => write!(f, "{:#?}", arg0),
            Self::Expression(arg0) => write!(f, "{:#?}", arg0),
        }
    }
}

macro_rules! match_methods {
    ($self:tt, $method:ident) => {
        match $self {
            Statement::Let(arg0) => arg0.$method(),
            Statement::Return(arg0) => arg0.$method(),
            Statement::Block(arg0) => arg0.$method(),
            Statement::Expression(arg0) => arg0.$method(),
        }
    };
}

impl<'source> NodeError for Statement<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        match_methods!(self, errors)
    }
}

impl<'source> NodeToken for Statement<'source> {
    fn token(&self) -> &Token {
        match_methods!(self, token)
    }
}

impl<'source> From<Expression<'source>> for Statement<'source> {
    fn from(expr: Expression<'source>) -> Self {
        Statement::Expression(expr)
    }
}

impl<'source> Statement<'source> {}

#[derive(Debug, PartialEq)]
pub struct Let<'source> {
    pub token: Token<'source>,
    pub name: Identifier<'source>,
    pub value: Expression<'source>,
}

impl<'source> Let<'source> {
    pub fn new(
        token: Token<'source>,
        name: Identifier<'source>,
        value: Expression<'source>,
    ) -> Self {
        Self { token, name, value }
    }
}

impl<'source> NodeError for Let<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        self.value.errors()
    }
}

#[derive(Debug, PartialEq)]
pub struct Return<'source> {
    token: Token<'source>,
    pub value: Option<ExprResult<'source>>,
}

impl<'source> Return<'source> {
    pub fn new(token: Token<'source>, value: Option<ExprResult<'source>>) -> Self {
        Self { token, value }
    }
}

impl<'source> NodeError for Return<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        match &self.value {
            Some(Ok(expr)) => expr.errors(),
            Some(Err(err)) => vec![err.clone()],
            None => vec![],
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Block<'source> {
    token: Token<'source>,
    pub statements: Vec<Node<'source>>,
    pub start: Position,
    pub end: Position,
}

impl<'source> Block<'source> {
    pub fn new(token: Token<'source>, statements: Vec<Node<'source>>, end: Position) -> Self {
        let start = token.start;
        Self {
            token,
            statements,
            start,
            end,
        }
    }
}

impl<'source> NodeError for Block<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();
        for node in &self.statements {
            match node {
                Node::Statement(stmt) => errors.extend(stmt.errors()),
                Node::Error(err) => errors.push(err.clone()),
            }
        }
        errors
    }
}

macro_rules! stmt_impls {
    ($($stmt:tt),+) => {$(
        impl<'source> From<$stmt<'source>> for Node<'source> {
            fn from(stmt: $stmt<'source>) -> Self {
                let statement: Statement = stmt.into();
                statement.into()
            }
        }

        impl<'source> From<$stmt<'source>> for Statement<'source> {
            fn from(stmt: $stmt<'source>) -> Self {
                Statement::$stmt(stmt)
            }
        }
    )+}
}

macro_rules! token_getter {
    ($($expr:tt),+) => {$(
        impl<'source> $expr<'source> {
            pub fn token(&self) -> &Token<'source> {
                &self.token
            }
        }
    )+}
}

stmt_impls!(Let, Return, Block);
token_getter!(Let, Return, Block);
