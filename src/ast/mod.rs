mod expressions;
mod statements;

use std::fmt::Debug;

pub use expressions::*;
pub use statements::*;
use tower_lsp::lsp_types::{Position, Range};

use crate::diagnostics::PosError;
use crate::lexer::Token;
use crate::pos::Pos;

pub struct Program<'source> {
    pub nodes: Vec<Node<'source>>,
    pub range: Range,
}

impl<'source> Program<'source> {
    pub fn new(nodes: Vec<Node<'source>>, end: Position) -> Self {
        let range = Range::new(Default::default(), end);
        Program { nodes, range }
    }
}

impl<'source> Debug for Program<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.nodes)
        } else {
            write!(f, "{:?}", self.nodes)
        }
    }
}

pub trait NodeError {
    fn errors(&self) -> Vec<PosError>;
}

pub trait Nodes {
    fn token(&self) -> &Token;
    fn range(&self) -> Range;
    fn pos_wrap<T>(&self, data: T) -> Pos<T>;
}

#[derive(PartialEq)]
pub enum Node<'source> {
    Statement(Statement<'source>),
    Error(PosError),
}

impl<'source> Debug for Node<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Statement(arg0) => write!(f, "{:#?}", arg0),
            Self::Error(arg0) => write!(f, "{:#?}", arg0),
        }
    }
}

impl<'source> NodeError for Node<'source> {
    fn errors(&self) -> Vec<PosError> {
        match &self {
            Node::Statement(stmt) => stmt.errors(),
            Node::Error(err) => vec![err.clone()],
        }
    }
}

impl<'source> From<Statement<'source>> for Node<'source> {
    fn from(stmt: Statement<'source>) -> Self {
        Node::Statement(stmt)
    }
}

impl<'source> From<Expression<'source>> for Node<'source> {
    fn from(expr: Expression<'source>) -> Self {
        Node::Statement(Statement::Expression(expr))
    }
}
