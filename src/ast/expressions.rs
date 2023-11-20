use std::fmt::Debug;

use super::*;
use crate::diagnostics::SpannedError;
use crate::lexer::{Token, TokenKind};

pub type ExprResult<'source> = Result<Expression<'source>, SpannedError>;

#[derive(PartialEq)]
pub enum Expression<'source> {
    Identifier(Identifier<'source>),
    Primative(Primative<'source>),
    StringLiteral(StringLiteral<'source>),
    Prefix(Prefix<'source>),
    Infix(Infix<'source>),
    If(If<'source>),
    Function(Function<'source>),
    Call(Call<'source>),
    Array(Array<'source>),
    Hash(Hash<'source>),
    Index(Index<'source>),
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
            Self::Function(arg0) => write!(f, "{:#?}", arg0),
            Self::Call(arg0) => write!(f, "{:#?}", arg0),
            Self::Array(arg0) => write!(f, "{:#?}", arg0),
            Self::Hash(arg0) => write!(f, "{:#?}", arg0),
            Self::Index(arg0) => write!(f, "{:#?}", arg0),
        }
    }
}

impl<'source> NodeError for Expression<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        match self {
            Self::Identifier(arg0) => arg0.errors(),
            Self::Primative(arg0) => arg0.errors(),
            Self::StringLiteral(arg0) => arg0.errors(),
            Self::Prefix(arg0) => arg0.errors(),
            Self::Infix(arg0) => arg0.errors(),
            Self::If(arg0) => arg0.errors(),
            Self::Function(arg0) => arg0.errors(),
            Self::Call(arg0) => arg0.errors(),
            Self::Array(arg0) => arg0.errors(),
            Self::Hash(arg0) => arg0.errors(),
            Self::Index(arg0) => arg0.errors(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'source> {
    token: Token<'source>,
    pub name: &'source str,
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

impl<'source> NodeError for Identifier<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        vec![]
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

impl<'source> NodeError for Primative<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        vec![]
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

impl<'source> NodeError for StringLiteral<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        vec![]
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

impl<'source> NodeError for Prefix<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        self.right.errors()
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

impl<'source> NodeError for Infix<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();
        errors.extend(self.left.errors());
        errors.extend(self.right.errors());
        errors
    }
}

#[derive(Debug, PartialEq)]
pub struct If<'source> {
    token: Token<'source>,
    condition: Result<Box<Expression<'source>>, SpannedError>,
    consequence: BlockResult<'source>,
    alternative: Result<Option<Block<'source>>, SpannedError>,
}

impl<'source> If<'source> {
    pub fn new(
        token: Token<'source>,
        condition: ExprResult<'source>,
        consequence: BlockResult<'source>,
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

impl<'source> NodeError for If<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();
        if let Err(ref err) = self.condition {
            errors.push(err.clone_inner())
        }

        match &self.consequence {
            Ok(block) => errors.extend(block.errors()),
            Err(err) => errors.push(err.clone_inner()),
        }

        match &self.alternative {
            Ok(Some(block)) => errors.extend(block.errors()),
            Err(err) => errors.push(err.clone_inner()),
            Ok(None) => (),
        }

        errors
    }
}

pub type VecExprResult<'source> = Result<Vec<ExprResult<'source>>, SpannedError>;

#[derive(Debug, PartialEq)]
pub struct Function<'source> {
    token: Token<'source>,
    params: VecExprResult<'source>,
    body: BlockResult<'source>,
}

impl<'source> Function<'source> {
    pub fn new(
        token: Token<'source>,
        params: VecExprResult<'source>,
        body: BlockResult<'source>,
    ) -> Self {
        Self {
            token,
            params,
            body,
        }
    }
}

impl<'source> NodeError for Function<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();

        match &self.params {
            Ok(exprs) => {
                for expr in exprs {
                    match expr {
                        Ok(expr) => errors.extend(expr.errors()),
                        Err(err) => errors.push(err.clone_inner()),
                    }
                }
            }
            Err(err) => errors.push(err.clone_inner()),
        }

        match &self.body {
            Ok(body) => errors.extend(body.errors()),
            Err(err) => errors.push(err.clone_inner()),
        }

        errors
    }
}

#[derive(Debug, PartialEq)]
pub struct Call<'source> {
    token: Token<'source>,
    func: Box<Expression<'source>>,
    args: VecExprResult<'source>,
}

impl<'source> Call<'source> {
    pub fn new(
        token: Token<'source>,
        func: Expression<'source>,
        args: VecExprResult<'source>,
    ) -> Self {
        Self {
            token,
            func: Box::new(func),
            args,
        }
    }
}

impl<'source> NodeError for Call<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();

        match &self.args {
            Ok(args) => {
                for arg in args {
                    match arg {
                        Ok(arg) => errors.extend(arg.errors()),
                        Err(err) => errors.push(err.clone_inner()),
                    }
                }
            }
            Err(err) => errors.push(err.clone_inner()),
        }

        errors
    }
}

#[derive(Debug, PartialEq)]
pub struct Array<'source> {
    token: Token<'source>,
    elems: VecExprResult<'source>,
}

impl<'source> Array<'source> {
    pub fn new(token: Token<'source>, elems: VecExprResult<'source>) -> Self {
        Self { token, elems }
    }
}

impl<'source> NodeError for Array<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();

        match &self.elems {
            Ok(elems) => {
                for elem in elems {
                    match elem {
                        Ok(elem) => errors.extend(elem.errors()),
                        Err(err) => errors.push(err.clone_inner()),
                    }
                }
            }
            Err(err) => errors.push(err.clone_inner()),
        }

        errors
    }
}

pub type ExprPairs<'source> = (Expression<'source>, Expression<'source>);

#[derive(Debug, PartialEq)]
pub struct Hash<'source> {
    token: Token<'source>,
    kv_pairs: Result<Vec<Result<ExprPairs<'source>, SpannedError>>, SpannedError>,
}

impl<'source> Hash<'source> {
    pub fn new(
        token: Token<'source>,
        kv_pairs: Result<Vec<Result<ExprPairs<'source>, SpannedError>>, SpannedError>,
    ) -> Self {
        Self { token, kv_pairs }
    }
}

impl<'source> NodeError for Hash<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();

        match &self.kv_pairs {
            Ok(pairs) => {
                for pair in pairs {
                    match pair {
                        Ok((key, value)) => {
                            errors.extend(key.errors());
                            errors.extend(value.errors());
                        }
                        Err(err) => errors.push(err.clone_inner()),
                    }
                }
            }

            Err(err) => errors.push(err.clone_inner()),
        }

        errors
    }
}

#[derive(Debug, PartialEq)]
pub struct Index<'source> {
    token: Token<'source>,
    object: Box<Expression<'source>>,
    index: Box<ExprResult<'source>>,
}

impl<'source> Index<'source> {
    pub fn new(
        token: Token<'source>,
        object: Expression<'source>,
        index: ExprResult<'source>,
    ) -> Self {
        Self {
            token,
            object: Box::new(object),
            index: Box::new(index),
        }
    }
}

impl<'source> NodeError for Index<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();
        errors.extend(self.object.errors());

        match &*self.index {
            Ok(expr) => errors.extend(expr.errors()),
            Err(err) => errors.push(err.clone_inner()),
        }

        errors
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

expr_impls!(
    Identifier,
    Primative,
    StringLiteral,
    Prefix,
    Infix,
    If,
    Function,
    Call,
    Array,
    Hash,
    Index
);
