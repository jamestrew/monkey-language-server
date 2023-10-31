use thiserror::Error;

use crate::types::Spanned;

// pub type SpannedDiagnostic = Spanned<Diagnostics>;
pub type SpannedError = Spanned<MonkeyError>;

pub enum Diagnostics {
    Error(MonkeyError),
    // Warning,
    // Information,
    // Hint,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MonkeyError {
    #[error("SyntaxError: Unexpected '{0}'.")]
    UnexpectedToken(String),

    #[error("SyntaxError: Unexpected eof.")]
    UnexpectedEof,

    #[error("SyntaxError: Expected '{0}' not found.")]
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

impl From<MonkeyError> for Diagnostics {
    fn from(err: MonkeyError) -> Self {
        Diagnostics::Error(err)
    }
}