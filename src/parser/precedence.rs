use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<&TokenKind> for Precedence {
    fn from(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::Equal | TokenKind::NotEqual => Self::Equals,
            TokenKind::LT | TokenKind::GT => Self::LessGreater,
            TokenKind::Plus | TokenKind::Minus => Self::Sum,
            TokenKind::Asterisk | TokenKind::ForwardSlash => Self::Product,
            TokenKind::LParen => Self::Call,
            TokenKind::LBracket => Self::Index,
            _ => Self::Lowest,
        }
    }
}

impl From<&Token<'_>> for Precedence {
    fn from(token: &Token) -> Self {
        Self::from(&token.kind)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn precedence_order() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals < Precedence::LessGreater);
        assert!(Precedence::LessGreater < Precedence::Sum);
        assert!(Precedence::Sum < Precedence::Product);
        assert!(Precedence::Product < Precedence::Prefix);
        assert!(Precedence::Prefix < Precedence::Call);
        assert!(Precedence::Call < Precedence::Index);
    }
}
