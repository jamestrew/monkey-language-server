use std::fmt::Debug;

use crate::diagnostics::SpannedError;
use crate::lexer::{Token, TokenKind};

// pub type NodeResult<'source> = Result<Node<'source>, SpannedError>;
pub type StmtResult<'source> = Result<Statement<'source>, SpannedError>;
pub type ExprResult<'source> = Result<Expression<'source>, SpannedError>;

pub struct Program<'source> {
    pub nodes: Vec<Node<'source>>,
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
    Let(Let<'source>),
    Return(Return<'source>),
    Block(Block<'source>),
    ExpressionStatement(Expression<'source>),
}

impl<'source> Debug for Statement<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(arg0) => write!(f, "{:#?}", arg0),
            Self::Return(arg0) => write!(f, "{:#?}", arg0),
            Self::Block(arg0) => write!(f, "{:#?}", arg0),
            Self::ExpressionStatement(arg0) => write!(f, "{:#?}", arg0),
        }
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

#[derive(Debug, PartialEq)]
pub struct Return<'source> {
    token: Token<'source>,
    value: Option<Expression<'source>>,
}

impl<'source> Return<'source> {
    pub fn new(token: Token<'source>, value: Option<Expression<'source>>) -> Self {
        Self { token, value }
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

#[derive(PartialEq)]
pub enum Expression<'source> {
    Identifier(Identifier<'source>),
    Primative(Primative<'source>),
    StringLiteral(StringLiteral<'source>),
    Prefix(Prefix<'source>),
    Infix(Infix<'source>),
    If(If<'source>),
    // Function(Function),
    // Call(Call),
    // Array(Vec<Expression>),
    // Hash(Hash),
    // Index(Index),
}

impl<'source> Debug for Expression<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(arg0) => write!(f, "{:#?}", arg0),
            Self::Primative(arg0) => write!(f, "{:#?}", arg0),
            Self::StringLiteral(arg0) => write!(f, "{:#?}", arg0),
            Self::Prefix(arg0) => write!(f, "{:#?}", arg0),
            Self::Infix(arg0) => write!(f, "{:#?}", arg0),
            Self::If(arg0) => write!(f, "{:#?}", arg0),
            // Self::Function(arg0) => write!(f, "{:#?}", arg0),
            // Self::Call(arg0) => write!(f, "{:#?}", arg0),
            // Self::Array(arg0) => write!(f, "{:#?}", arg0),
            // Self::Hash(arg0) => write!(f, "{:#?}", arg0),
            // Self::Index(arg0) => write!(f, "{:#?}", arg0),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'source> {
    token: Token<'source>,
    name: &'source str,
}

impl<'source> From<Token<'source>> for Identifier<'source> {
    fn from(token: Token<'source>) -> Self {
        let name = token.slice;
        match token.kind {
            TokenKind::Identifier => Self { token, name },
            _ => unreachable!("Identifier expects Identifier token. got {:?}", token),
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

#[derive(Debug, PartialEq)]
pub struct Prefix<'source> {
    token: Token<'source>,
    right: Box<Expression<'source>>,
}

impl<'source> Prefix<'source> {
    pub fn new(token: Token<'source>, right: Expression<'source>) -> Self {
        Self {
            token,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Infix<'source> {
    token: Token<'source>,
    left: Box<Expression<'source>>,
    right: Box<Expression<'source>>,
}

impl<'source> Infix<'source> {
    pub fn new(
        token: Token<'source>,
        left: Expression<'source>,
        right: Expression<'source>,
    ) -> Self {
        Self {
            token,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct If<'source> {
    token: Token<'source>,
    condition: Result<Box<Expression<'source>>, SpannedError>,
    consequence: Result<Block<'source>, SpannedError>,
    alternative: Result<Option<Block<'source>>, SpannedError>,
}

impl<'source> If<'source> {
    pub fn new(
        token: Token<'source>,
        condition: Result<Expression<'source>, SpannedError>,
        consequence: Result<Block<'source>, SpannedError>,
        alternative: Result<Option<Block<'source>>, SpannedError>,
    ) -> Self {
        let condition = match condition {
            Ok(expr) => Ok(Box::new(expr)),
            Err(err) => Err(err),
        };

        Self {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}

macro_rules! expr_impls {
    ($($expr:tt),+) => {$(
        impl<'source> From<$expr<'source>> for Node<'source> {
            fn from(expr: $expr<'source>) -> Self {
                let expression: Expression = expr.into();
                expression.into()
            }
        }

        impl<'source> From<$expr<'source>> for Expression<'source> {
            fn from(expr: $expr<'source>) -> Self {
                Expression::$expr(expr)
            }
        }
    )+}
}

expr_impls!(Identifier, Primative, StringLiteral, Prefix, Infix, If);
