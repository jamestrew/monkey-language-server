use std::fmt::Debug;

use tower_lsp::lsp_types::Range;

use super::*;
use crate::diagnostics::PosError;
use crate::lexer::Token;

pub type StmtResult<'source> = Result<Statement<'source>, PosError>;
pub type BlockResult<'source> = Result<Block<'source>, PosError>;

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
    fn errors(&self) -> Vec<PosError> {
        match_methods!(self, errors)
    }
}

impl<'source> Nodes for Statement<'source> {
    fn token(&self) -> &Token {
        match_methods!(self, token)
    }

    fn range(&self) -> Range {
        match_methods!(self, range)
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
    pub range: Range,
}

impl<'source> Let<'source> {
    pub fn new(
        token: Token<'source>,
        name: Identifier<'source>,
        value: Expression<'source>,
        end: Position,
    ) -> Self {
        let start = token.start;
        Self {
            token,
            name,
            value,
            range: Range::new(start, end),
        }
    }

    pub fn range(&self) -> Range {
        self.range
    }
}

impl<'source> NodeError for Let<'source> {
    fn errors(&self) -> Vec<PosError> {
        self.value.errors()
    }
}

#[derive(Debug, PartialEq)]
pub struct Return<'source> {
    token: Token<'source>,
    pub value: Option<ExprResult<'source>>,
    range: Range,
}

impl<'source> Return<'source> {
    pub fn new(token: Token<'source>, value: Option<ExprResult<'source>>, end: Position) -> Self {
        let start = token.start;
        Self {
            token,
            value,
            range: Range::new(start, end),
        }
    }

    pub fn range(&self) -> Range {
        self.range
    }
}

impl<'source> NodeError for Return<'source> {
    fn errors(&self) -> Vec<PosError> {
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
    range: Range,
}

impl<'source> Block<'source> {
    pub fn new(token: Token<'source>, statements: Vec<Node<'source>>, end: Position) -> Self {
        let range = Range::new(token.start, end);
        Self {
            token,
            statements,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
    }
}

impl<'source> NodeError for Block<'source> {
    fn errors(&self) -> Vec<PosError> {
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
