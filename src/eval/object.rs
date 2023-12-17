use std::rc::Rc;

use crate::ast::{Call, ExprResult, Expression};
use crate::diagnostics::{MonkeyError, SpannedDiagnostic};
use crate::types::Spanned;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Object {
    Int,
    Bool,
    String,
    Return,
    Function(Option<usize>, Box<Object>),
    Builtin(Builtin),
    Array,
    Hash,
    Nil,
    Unknown,
}

impl Object {
    pub fn typename(&self) -> &'static str {
        match self {
            Object::Int => "int",
            Object::Bool => "bool",
            Object::String => "str",
            Object::Return => "return",
            Object::Function(_, _) => "function",
            Object::Builtin(_) => "builtin",
            Object::Array => "array",
            Object::Hash => "hash",
            Object::Nil => "nil",
            Object::Unknown => "unknown",
        }
    }

    pub fn function_return(&self) -> Option<Object> {
        match self {
            Self::Function(_, ret_obj) => Some(*ret_obj.clone()),
            _ => None,
        }
    }

    pub fn is_hashable(&self) -> bool {
        matches!(
            self,
            Object::Bool | Object::Int | Object::String | Object::Unknown
        )
    }
}

impl std::fmt::Debug for Spanned<Object> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Spanned({:?}, {})", **self, self.pos_rng_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Builtin {
    Len,
    Puts,
    First,
    Last,
    Rest,
    Push,
}

impl Builtin {
    pub fn object_wrap(&self) -> Rc<Spanned<Object>> {
        Rc::new(Spanned::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Object::Builtin(self.clone()),
        ))
    }

    pub fn ident(&self) -> &'static str {
        match self {
            Builtin::Len => "len",
            Builtin::Puts => "puts",
            Builtin::First => "first",
            Builtin::Last => "last",
            Builtin::Rest => "rest",
            Builtin::Push => "push",
        }
    }

    pub fn variants() -> &'static [Self] {
        &[
            Builtin::Len,
            Builtin::Puts,
            Builtin::First,
            Builtin::Last,
            Builtin::Rest,
            Builtin::Push,
        ]
    }

    pub fn includes(ident: &str) -> bool {
        Self::variants()
            .iter()
            .any(|builtin| builtin.ident() == ident)
    }

    pub fn arg_count(&self) -> Option<usize> {
        match self {
            Builtin::Len => Some(1),
            Builtin::Puts => None,
            Builtin::First => Some(1),
            Builtin::Last => Some(1),
            Builtin::Rest => Some(2),
            Builtin::Push => Some(2),
        }
    }

    pub fn return_type(&self) -> Object {
        match self {
            Builtin::Len => Object::Int,
            Builtin::Puts => Object::Nil,
            Builtin::First => Object::Unknown,
            Builtin::Last => Object::Unknown,
            Builtin::Rest => Object::Array,
            Builtin::Push => Object::Array,
        }
    }

    pub fn eval(
        self,
        call_expr: &Call,
        args: &[Option<Object>],
    ) -> (Object, Option<SpannedDiagnostic>) {
        match self {
            Builtin::Len => Self::len_eval(call_expr, args),
            Builtin::Puts => (Object::Nil, None),
            Builtin::First => Self::first_last_eval(call_expr, args),
            Builtin::Last => Self::first_last_eval(call_expr, args),
            Builtin::Rest => Self::rest_eval(call_expr, args),
            Builtin::Push => Self::push_eval(call_expr, args),
        }
    }

    fn len_eval(call_expr: &Call, args: &[Option<Object>]) -> (Object, Option<SpannedDiagnostic>) {
        if args.len() != 1 {
            return (
                Object::Int,
                Some(
                    call_expr
                        .token()
                        .map(MonkeyError::MismatchArgs(1, args.len()).into()),
                ),
            );
        }

        if let Some(arg) = &args[0] {
            match arg {
                Object::String | Object::Array | Object::Hash | Object::Unknown => {
                    return (Object::Int, None);
                }
                obj => {
                    return (
                        Object::Int,
                        Some(
                            call_expr.token().map(
                                MonkeyError::GenericTypeError(format!(
                                    "unable to get the length of '{}'",
                                    obj.typename()
                                ))
                                .into(),
                            ),
                        ),
                    )
                }
            };
        }
        (Object::Int, None)
    }

    fn first_last_eval(
        call_expr: &Call,
        args: &[Option<Object>],
    ) -> (Object, Option<SpannedDiagnostic>) {
        if args.len() != 1 {
            return (
                Object::Unknown,
                Some(
                    call_expr
                        .token()
                        .map(MonkeyError::MismatchArgs(1, args.len()).into()),
                ),
            );
        }

        if let Some(arg) = &args[0] {
            match arg {
                Object::String | Object::Array | Object::Hash | Object::Unknown => {
                    return (Object::Unknown, None);
                }
                obj => {
                    return (
                        Object::Int,
                        Some(
                            call_expr.token().map(
                                MonkeyError::GenericTypeError(format!(
                                    "unable to get the first of '{}'",
                                    obj.typename()
                                ))
                                .into(),
                            ),
                        ),
                    )
                }
            };
        }
        (Object::Unknown, None)
    }

    fn rest_eval(call_expr: &Call, args: &[Option<Object>]) -> (Object, Option<SpannedDiagnostic>) {
        (Object::Array, None)
    }

    fn push_eval(call_expr: &Call, args: &[Option<Object>]) -> (Object, Option<SpannedDiagnostic>) {
        (Object::Array, None)
    }
}
