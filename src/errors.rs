use thiserror::Error;

use crate::types::Spanned;

pub type SpannedError = Spanned<MonkeyError>;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MonkeyError {
    #[error("Unexpected token '{0}'.")]
    UnexpectedToken(String),

    #[error("Unexpected eof.")]
    UnexpectedEof,

    #[error("Expected token '{0}' not found.")]
    ExpectedTokenNotFound(String),
}

impl std::fmt::Debug for SpannedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err = match &**self {
            MonkeyError::UnexpectedToken(_) => format!("UnexpectedToken(\"{}\")", **self),
            MonkeyError::UnexpectedEof => format!("UnexpectedEof(\"{}\")", **self),
            MonkeyError::ExpectedTokenNotFound(_) => {
                format!("ExpectedTokenNotFound(\"{}\")", **self)
            }
        };
        write!(f, "Err({err}, {})", self.pos_rng_str())
    }
}
