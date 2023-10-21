use std::fmt::Debug;

use crate::errors::SpannedError;
use crate::lexer::{Token, TokenKind};

pub enum Node<'source> {
    Statement(Statement<'source>),
    Error(SpannedError),
}

impl<'source> Debug for Node<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Statement(arg0) => write!(f, "{:?}", arg0),
            Self::Error(arg0) => write!(f, "{:?}", arg0),
        }
    }
}

impl<'source> From<Statement<'source>> for Node<'source> {
    fn from(stmt: Statement<'source>) -> Self {
        Node::Statement(stmt)
    }
}

impl<'source> From<Expression<'source>> for Statement<'source> {
    fn from(expr: Expression<'source>) -> Self {
        Statement::ExpressionStatement(expr)
    }
}

impl<'source> From<Expression<'source>> for Node<'source> {
    fn from(expr: Expression<'source>) -> Self {
        Node::Statement(Statement::ExpressionStatement(expr))
    }
}

#[derive(PartialEq)]
pub enum Statement<'source> {
    // Let(Let),
    // Return(Return),
    // Block(Block),
    ExpressionStatement(Expression<'source>),
}

impl<'source> Debug for Statement<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Self::Let(arg0) => write!(f, "{:?}", arg0),
            // Self::Return(arg0) => write!(f, "{:?}", arg0),
            // Self::Block(arg0) => write!(f, "{:?}", arg0),
            Self::ExpressionStatement(arg0) => write!(f, "{:?}", arg0),
        }
    }
}

#[derive(PartialEq)]
pub enum Expression<'source> {
    // Identifier(Identifier),
    Primative(Primative<'source>),
    StringLiteral(StringLiteral<'source>),
    // Prefix(Prefix),
    // Infix(Infix),
    // If(If),
    // Function(Function),
    // Call(Call),
    // Array(Vec<Expression>),
    // Hash(Hash),
    // Index(Index),
}

impl<'source> Debug for Expression<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Self::Identifier(arg0) => write!(f, "{:?}", arg0),
            Self::Primative(arg0) => write!(f, "{:?}", arg0),
            Self::StringLiteral(arg0) => write!(f, "{:?}", arg0),
            // Self::Prefix(arg0) => write!(f, "{:?}", arg0),
            // Self::Infix(arg0) => write!(f, "{:?}", arg0),
            // Self::If(arg0) => write!(f, "{:?}", arg0),
            // Self::Function(arg0) => write!(f, "{:?}", arg0),
            // Self::Call(arg0) => write!(f, "{:?}", arg0),
            // Self::Array(arg0) => write!(f, "{:?}", arg0),
            // Self::Hash(arg0) => write!(f, "{:?}", arg0),
            // Self::Index(arg0) => write!(f, "{:?}", arg0),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Primative<'source> {
    Int { token: Token<'source>, value: i64 },
    Bool { token: Token<'source>, value: bool },
    Nil { token: Token<'source> },
}

impl<'source> From<Token<'source>> for Primative<'source> {
    fn from(token: Token<'source>) -> Self {
        match token.kind {
            TokenKind::Int => {
                let value: i64 = token.slice.parse().unwrap();
                Self::Int { token, value }
            }
            TokenKind::True => Self::Bool { token, value: true },
            TokenKind::False => Self::Bool {
                token,
                value: false,
            },
            TokenKind::Nil => Self::Nil { token },
            _ => unreachable!("Primative expects int/bool/nil. got {:?}", token),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral<'source> {
    token: Token<'source>,
    value: &'source str,
}

impl<'source> From<Token<'source>> for StringLiteral<'source> {
    fn from(token: Token<'source>) -> Self {
        let value = token.slice;
        match token.kind {
            TokenKind::Str => Self { token, value },
            _ => unreachable!("StringLiteral expects Str token. got {:?}", token),
        }
    }
}

macro_rules! to_expr {
    ($($expr:tt),+) => {$(
        impl<'source> From<$expr<'source>> for Expression<'source> {
            fn from(expr: $expr<'source>) -> Self {
                Expression::$expr(expr)
            }
        }
    )+}
}

macro_rules! expr_to_node {
    ($($expr:tt),+) => {$(
        impl<'source> From<$expr<'source>> for Node<'source> {
            fn from(expr: $expr<'source>) -> Self {
                let expression: Expression = expr.into();
                expression.into()
            }
        }
    )+}
}

to_expr!(Primative, StringLiteral);
expr_to_node!(Primative, StringLiteral);
