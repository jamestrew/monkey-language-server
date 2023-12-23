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

macro_rules! match_methods {
    ($self:tt, $method:ident) => {
        match $self {
            Expression::Identifier(arg0) => arg0.$method(),
            Expression::Primative(arg0) => arg0.$method(),
            Expression::StringLiteral(arg0) => arg0.$method(),
            Expression::Prefix(arg0) => arg0.$method(),
            Expression::Infix(arg0) => arg0.$method(),
            Expression::If(arg0) => arg0.$method(),
            Expression::Function(arg0) => arg0.$method(),
            Expression::Call(arg0) => arg0.$method(),
            Expression::Array(arg0) => arg0.$method(),
            Expression::Hash(arg0) => arg0.$method(),
            Expression::Index(arg0) => arg0.$method(),
        }
    };
}

impl<'source> NodeError for Expression<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        match_methods!(self, errors)
    }
}

impl<'source> Nodes for Expression<'source> {
    fn token(&self) -> &Token {
        match_methods!(self, token)
    }

    fn range(&self) -> Range {
        match self {
            Expression::Identifier(_) | Expression::Primative(_) | Expression::StringLiteral(_) => {
                (self.token()).into()
            }
            Expression::Prefix(expr) => expr.range(),
            Expression::Infix(expr) => expr.range(),
            Expression::If(expr) => expr.range(),
            Expression::Function(expr) => expr.range(),
            Expression::Call(expr) => expr.range(),
            Expression::Array(expr) => expr.range(),
            Expression::Hash(expr) => expr.range(),
            Expression::Index(expr) => expr.range(),
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

impl<'source> Identifier<'source> {
    pub fn range(&self) -> Range {
        (&self.token).into()
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

impl<'source> Primative<'source> {
    pub fn token(&self) -> &Token<'source> {
        match self {
            Primative::Int { token, .. } => token,
            Primative::Bool { token, .. } => token,
            Primative::Nil { token } => token,
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Mult,
    Div,
    Bang,
    Eq,
    NotEq,
    Lt,
    Gt,
}

impl Operator {
    pub fn as_str(&self) -> &'static str {
        match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Mult => "*",
            Operator::Div => "/",
            Operator::Bang => "!",
            Operator::Eq => "==",
            Operator::NotEq => "!=",
            Operator::Lt => "<",
            Operator::Gt => ">",
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&TokenKind> for Operator {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Assign => Operator::Plus,
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Asterisk => Operator::Mult,
            TokenKind::ForwardSlash => Operator::Div,
            TokenKind::Bang => Operator::Bang,
            TokenKind::Equal => Operator::Eq,
            TokenKind::NotEqual => Operator::NotEq,
            TokenKind::LT => Operator::Lt,
            TokenKind::GT => Operator::Gt,
            _ => unreachable!("{value} not an operator"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Prefix<'source> {
    token: Token<'source>,
    pub right: Box<Expression<'source>>,
    pub operator: Operator,
    range: Range,
}

impl<'source> Prefix<'source> {
    pub fn new(token: Token<'source>, right: Expression<'source>, end: Position) -> Self {
        let operator = Operator::from(&token.kind);
        let range = Range::new(token.start, end);
        Self {
            token,
            right: Box::new(right),
            operator,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
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
    pub left: Box<Expression<'source>>,
    pub right: Box<Expression<'source>>,
    pub operator: Operator,
    range: Range,
}

impl<'source> Infix<'source> {
    pub fn new(
        token: Token<'source>,
        left: Expression<'source>,
        right: Expression<'source>,
        end: Position,
    ) -> Self {
        let operator = Operator::from(&token.kind);
        let range = Range::new(token.start, end);
        Self {
            token,
            left: Box::new(left),
            right: Box::new(right),
            operator,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
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
    pub condition: Result<Box<Expression<'source>>, SpannedError>,
    pub consequence: BlockResult<'source>,
    pub alternative: Result<Option<Block<'source>>, SpannedError>,
    range: Range,
}

impl<'source> If<'source> {
    pub fn new(
        token: Token<'source>,
        condition: ExprResult<'source>,
        consequence: BlockResult<'source>,
        alternative: Result<Option<Block<'source>>, SpannedError>,
        end: Position,
    ) -> Self {
        let condition = match condition {
            Ok(expr) => Ok(Box::new(expr)),
            Err(err) => Err(err),
        };

        let range = Range::new(token.start, end);

        Self {
            token,
            condition,
            consequence,
            alternative,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
    }
}

impl<'source> NodeError for If<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();
        if let Err(ref err) = self.condition {
            errors.push(err.clone())
        }

        match &self.consequence {
            Ok(block) => errors.extend(block.errors()),
            Err(err) => errors.push(err.clone()),
        }

        match &self.alternative {
            Ok(Some(block)) => errors.extend(block.errors()),
            Err(err) => errors.push(err.clone()),
            Ok(None) => (),
        }

        errors
    }
}

pub type VecExprResult<'source> = Result<Vec<ExprResult<'source>>, SpannedError>;

#[derive(Debug, PartialEq)]
pub struct Function<'source> {
    token: Token<'source>,
    pub params: VecExprResult<'source>,
    pub body: BlockResult<'source>,
    range: Range,
}

impl<'source> Function<'source> {
    pub fn new(
        token: Token<'source>,
        params: VecExprResult<'source>,
        body: BlockResult<'source>,
        end: Position,
    ) -> Self {
        let range = Range::new(token.start, end);
        Self {
            token,
            params,
            body,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
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
                        Err(err) => errors.push(err.clone()),
                    }
                }
            }
            Err(err) => errors.push(err.clone()),
        }

        match &self.body {
            Ok(body) => errors.extend(body.errors()),
            Err(err) => errors.push(err.clone()),
        }

        errors
    }
}

#[derive(Debug, PartialEq)]
pub struct Call<'source> {
    token: Token<'source>,
    pub func: Box<Expression<'source>>,
    pub args: VecExprResult<'source>,
    range: Range,
}

impl<'source> Call<'source> {
    pub fn new(
        token: Token<'source>,
        func: Expression<'source>,
        args: VecExprResult<'source>,
        end: Position,
    ) -> Self {
        let range = Range::new(token.start, end);
        Self {
            token,
            func: Box::new(func),
            args,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
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
                        Err(err) => errors.push(err.clone()),
                    }
                }
            }
            Err(err) => errors.push(err.clone()),
        }

        errors
    }
}

#[derive(Debug, PartialEq)]
pub struct Array<'source> {
    token: Token<'source>,
    pub elems: VecExprResult<'source>,
    range: Range,
}

impl<'source> Array<'source> {
    pub fn new(token: Token<'source>, elems: VecExprResult<'source>, end: Position) -> Self {
        let range = Range::new(token.start, end);
        Self {
            token,
            elems,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
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
                        Err(err) => errors.push(err.clone()),
                    }
                }
            }
            Err(err) => errors.push(err.clone()),
        }

        errors
    }
}

pub type ExprPairs<'source> = (Expression<'source>, Expression<'source>);

#[derive(Debug, PartialEq)]
pub struct Hash<'source> {
    token: Token<'source>,
    pub kv_pairs: Result<Vec<Result<ExprPairs<'source>, SpannedError>>, SpannedError>,
    range: Range,
}

impl<'source> Hash<'source> {
    pub fn new(
        token: Token<'source>,
        kv_pairs: Result<Vec<Result<ExprPairs<'source>, SpannedError>>, SpannedError>,
        end: Position,
    ) -> Self {
        let range = Range::new(token.start, end);
        Self {
            token,
            kv_pairs,
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
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
                        Err(err) => errors.push(err.clone()),
                    }
                }
            }

            Err(err) => errors.push(err.clone()),
        }

        errors
    }
}

#[derive(Debug, PartialEq)]
pub struct Index<'source> {
    token: Token<'source>,
    pub object: Box<Expression<'source>>,
    pub index: Box<ExprResult<'source>>,
    range: Range,
}

impl<'source> Index<'source> {
    pub fn new(
        token: Token<'source>,
        object: Expression<'source>,
        index: ExprResult<'source>,
        end: Position,
    ) -> Self {
        let range = Range::new(object.range().start, end);
        Self {
            token,
            object: Box::new(object),
            index: Box::new(index),
            range,
        }
    }

    pub fn range(&self) -> Range {
        self.range
    }
}

impl<'source> NodeError for Index<'source> {
    fn errors(&self) -> Vec<SpannedError> {
        let mut errors = Vec::new();
        errors.extend(self.object.errors());

        match &*self.index {
            Ok(expr) => errors.extend(expr.errors()),
            Err(err) => errors.push(err.clone()),
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

macro_rules! token_getter {
    ($($expr:tt),+) => {$(
        impl<'source> $expr<'source> {
            pub fn token(&self) -> &Token<'source> {
                &self.token
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

token_getter!(
    Identifier,
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
