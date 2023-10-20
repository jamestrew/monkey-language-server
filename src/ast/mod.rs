use crate::lexer::{Token, TokenKind};

pub enum Node<'source> {
    Statement(Statement<'source>),
    Error,
}

#[derive(PartialEq)]
pub enum Statement<'source> {
    // Let(Let),
    // Return(Return),
    // Block(Block),
    ExpressionStatement(Expression<'source>),
}

#[derive(PartialEq)]
pub enum Expression<'source> {
    // Identifier(Identifier),
    Primative(Primative<'source>),
    // StringLiteral(StringLiteral),
    // Prefix(Prefix),
    // Infix(Infix),
    // If(If),
    // Function(Function),
    // Call(Call),
    // Array(Vec<Expression>),
    // Hash(Hash),
    // Index(Index),
}

#[derive(PartialEq)]
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
