use thiserror::Error;

use crate::types::Spanned;

pub type SpannedError = Spanned<MonkeyError>;

#[derive(Error, Debug, Clone)]
pub enum MonkeyError {
    #[error("Unexpected token '{0}'.")]
    UnexpectedToken(String),
}
