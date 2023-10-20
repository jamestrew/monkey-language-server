use thiserror::Error;

use crate::types::Spanned;

pub type SpannedError = Spanned<MonkeyError>;

#[derive(Error, Debug, Clone)]
pub enum MonkeyError {
    #[error("Unexpected token '{0}'.")]
    UnexpectedToken(String),
}

impl std::fmt::Debug for SpannedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err = match &**self {
            MonkeyError::UnexpectedToken(_) => format!("UnexpectedToken(\"{}\")", **self),
        };
        write!(f, "Err({err}, {})", self.pos_rng_str())
    } }
