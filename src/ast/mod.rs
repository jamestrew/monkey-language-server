mod expressions;
mod statements;

use std::fmt::Debug;

pub use expressions::*;
pub use statements::*;
use tower_lsp::lsp_types::{Position, Range};

use crate::diagnostics::SpannedError;
use crate::lexer::Token;

pub struct Program<'source> {
    pub nodes: Vec<Node<'source>>,
    pub range: Range,
}

impl<'source> Program<'source> {
    pub fn new(nodes: Vec<Node<'source>>, end: Position) -> Self {
        let range = Range::new(Default::default(), end);
        Program {
            nodes,
            range,
        }
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
    fn errors(&self) -> Vec<SpannedError>;
}

pub trait NodeToken {
    fn token(&self) -> &Token;
}

#[derive(PartialEq)]
pub enum Node<'source> {
    Statement(Statement<'source>),
    Error(SpannedError),
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
    fn errors(&self) -> Vec<SpannedError> {
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
