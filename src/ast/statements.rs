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

impl<'source> NodeError for Statement<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        match self {
            Statement::Let(arg0) => arg0.errors(),
            Statement::Return(arg0) => arg0.errors(),
            Statement::Block(arg0) => arg0.errors(),
            Statement::Expression(arg0) => arg0.errors(),
        }
    }
}

impl<'source> From<Expression<'source>> for Statement<'source> {
    fn from(expr: Expression<'source>) -> Self {
        Statement::Expression(expr)
    }
}

#[derive(Debug, PartialEq)]
pub struct Let<'source> {
    token: Token<'source>,
    name: Identifier<'source>,
    value: Expression<'source>,
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
    value: Option<ExprResult<'source>>,
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
            Some(Err(err)) => vec![err.clone_inner()],
            None => vec![],
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Block<'source> {
    token: Token<'source>,
    statements: Vec<Node<'source>>,
}

impl<'source> Block<'source> {
    pub fn new(token: Token<'source>, statements: Vec<Node<'source>>) -> Self {
        Self { token, statements }
    }
}

impl<'source> NodeError for Block<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();
        for node in &self.statements {
            match node {
                Node::Statement(stmt) => errors.extend(stmt.errors()),
                Node::Error(err) => errors.push(err.clone_inner())
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

stmt_impls!(Let, Return, Block);
