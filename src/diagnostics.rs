use thiserror::Error;
use tower_lsp::lsp_types::DiagnosticSeverity;

use crate::ast::Operator;
use crate::eval::Object;
use crate::types::Spanned;

pub type SpannedDiagnostic = Spanned<Diagnostics>;
pub type SpannedError = Spanned<MonkeyError>;
pub type SpannedWarning = Spanned<MonkeyWarning>;

#[derive(Debug, Clone, PartialEq)]
pub enum Diagnostics {
    Error(MonkeyError),
    Warning(MonkeyWarning),
    // Information,
    // Hint,
}

impl Diagnostics {
    pub fn severity(&self) -> DiagnosticSeverity {
        match self {
            Diagnostics::Error(_) => DiagnosticSeverity::ERROR,
            Diagnostics::Warning(_) => DiagnosticSeverity::WARNING,
        }
    }
}

impl std::fmt::Display for Diagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Diagnostics::Error(err) => write!(f, "{err}"),
            Diagnostics::Warning(warn) => write!(f, "{warn}"),
        }
    }
}

impl std::fmt::Debug for SpannedDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match &**self {
            Diagnostics::Error(err) => format!("{:?}", err),
            Diagnostics::Warning(warn) => format!("{:?}", warn),
        };
        write!(f, "{:?}({msg}, {})", self.severity(), self.pos_rng_str())
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MonkeyError {
    #[error("SyntaxError: Unexpected '{0}'.")]
    UnexpectedToken(String),

    #[error("SyntaxError: Unexpected eof.")]
    UnexpectedEof,

    #[error("SyntaxError: Expected '{0}' not found.")]
    ExpectedTokenNotFound(String),

    #[error("NameError: '{0}' is not defined.")]
    UnknownIdentifier(String),

    #[error("TypeError: bad operand type for unary +: '{0}'")]
    BadPrefixType(&'static str),

    #[error("TypeError: <{0}> {2} <{1}>")]
    UnknownOperator(&'static str, &'static str, &'static str),

    #[error("TypeError: object of type '{0}' is not callable")]
    BadFunctionCall(&'static str),
}

impl MonkeyError {
    pub fn new_unknown_op<T>(
        span: &Spanned<T>,
        left_obj: &Object,
        right_obj: &Object,
        op: Operator,
    ) -> SpannedError {
        span.map(Self::UnknownOperator(
            left_obj.typename(),
            right_obj.typename(),
            op.as_str(),
        ))
    }
}

impl std::fmt::Debug for SpannedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err = match &**self {
            MonkeyError::UnexpectedToken(_) => format!("UnexpectedToken(\"{}\")", **self),
            MonkeyError::UnexpectedEof => format!("UnexpectedEof(\"{}\")", **self),
            MonkeyError::ExpectedTokenNotFound(_) => {
                format!("ExpectedTokenNotFound(\"{}\")", **self)
            }
            MonkeyError::UnknownIdentifier(_) => format!("UnknownIdentifier(\"{}\")", **self),
            MonkeyError::BadPrefixType(_) => format!("BadPrefixType(\"{}\")", **self),
            MonkeyError::UnknownOperator(..) => format!("UnknownOperator(\"{}\")", **self),
            MonkeyError::BadFunctionCall(..) => format!("BadFunctionCall(\"{}\")", **self),
        };
        write!(f, "Err({err}, {})", self.pos_rng_str())
    }
}

impl From<MonkeyError> for Diagnostics {
    fn from(err: MonkeyError) -> Self {
        Diagnostics::Error(err)
    }
}

impl From<SpannedError> for SpannedDiagnostic {
    fn from(value: SpannedError) -> Self {
        value.transform()
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MonkeyWarning {
    #[error("Expression value is unused")]
    UnusedExpression,

    #[error("Code is unreachable")]
    UnreachableCode
}

impl std::fmt::Debug for SpannedWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err = match &**self {
            MonkeyWarning::UnusedExpression => format!("UnusedExpression(\"{}\")", **self),
            MonkeyWarning::UnreachableCode => format!("UnreachableCode(\"{}\")", **self),
        };
        write!(f, "Warn({err}, {})", self.pos_rng_str())
    }
}

impl From<MonkeyWarning> for Diagnostics {
    fn from(err: MonkeyWarning) -> Self {
        Diagnostics::Warning(err)
    }
}

impl From<SpannedWarning> for SpannedDiagnostic {
    fn from(value: SpannedWarning) -> Self {
        value.transform()
    }
}
