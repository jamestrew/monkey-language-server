use crate::ast::Call;
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

    fn is_sequence(&self) -> bool {
        matches!(
            self,
            Object::String | Object::Array | Object::Hash | Object::Unknown
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
    pub fn spanned_obj(&self) -> Spanned<Object> {
        Spanned::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Object::Builtin(self.clone()),
        )
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
            Builtin::Len => self.len_eval(call_expr, args),
            Builtin::Puts => (Object::Nil, None),
            Builtin::First => self.first_last_eval(call_expr, args),
            Builtin::Last => self.first_last_eval(call_expr, args),
            Builtin::Rest => self.rest_eval(call_expr, args),
            Builtin::Push => self.push_eval(call_expr, args),
        }
    }

    fn len_eval(
        &self,
        call_expr: &Call,
        args: &[Option<Object>],
    ) -> (Object, Option<SpannedDiagnostic>) {
        let ret_obj = Object::Int;
        if let Some(diag) = Self::check_arg_length(call_expr, args, 1) {
            return (ret_obj, Some(diag));
        }

        match &args[0] {
            None => (ret_obj, None),
            Some(obj) if obj.is_sequence() => (ret_obj, None),
            Some(obj) => (
                ret_obj,
                Some(
                    call_expr.token().map(
                        MonkeyError::GenericTypeError(format!(
                            "unable to get the {} of '{}'",
                            self.ident(),
                            obj.typename()
                        ))
                        .into(),
                    ),
                ),
            ),
        }
    }

    fn first_last_eval(
        &self,
        call_expr: &Call,
        args: &[Option<Object>],
    ) -> (Object, Option<SpannedDiagnostic>) {
        let ret_obj = Object::Unknown;
        if let Some(diag) = Self::check_arg_length(call_expr, args, 1) {
            return (ret_obj, Some(diag));
        }

        match &args[0] {
            None => (ret_obj, None),
            Some(Object::Array) => (ret_obj, None),
            Some(obj) => (
                ret_obj,
                Some(
                    call_expr.token().map(
                        MonkeyError::GenericTypeError(format!(
                            "unable to get the {} of '{}'",
                            self.ident(),
                            obj.typename()
                        ))
                        .into(),
                    ),
                ),
            ),
        }
    }

    fn rest_eval(
        &self,
        call_expr: &Call,
        args: &[Option<Object>],
    ) -> (Object, Option<SpannedDiagnostic>) {
        let ret_obj = Object::Array;
        if let Some(diag) = Self::check_arg_length(call_expr, args, 1) {
            return (ret_obj, Some(diag));
        }

        match &args[0] {
            None => (ret_obj, None),
            Some(Object::Array) => (ret_obj, None),
            Some(obj) => (
                ret_obj,
                Some(
                    call_expr.token().map(
                        MonkeyError::GenericTypeError(format!(
                            "unable to get the {} of '{}'",
                            self.ident(),
                            obj.typename()
                        ))
                        .into(),
                    ),
                ),
            ),
        }
    }

    fn push_eval(
        &self,
        call_expr: &Call,
        args: &[Option<Object>],
    ) -> (Object, Option<SpannedDiagnostic>) {
        let ret_obj = Object::Array;
        if let Some(diag) = Self::check_arg_length(call_expr, args, 2) {
            return (ret_obj, Some(diag));
        }

        match &args[0] {
            None => (ret_obj, None),
            Some(Object::Array) => (ret_obj, None),
            Some(obj) => (
                ret_obj,
                Some(
                    call_expr.token().map(
                        MonkeyError::GenericTypeError(format!(
                            "unable to push value into '{}'",
                            obj.typename()
                        ))
                        .into(),
                    ),
                ),
            ),
        }
    }

    fn check_arg_length(
        call_expr: &Call,
        args: &[Option<Object>],
        len: usize,
    ) -> Option<SpannedDiagnostic> {
        if args.len() != len {
            Some(
                call_expr
                    .token()
                    .map(MonkeyError::MismatchArgs(len, args.len()).into()),
            )
        } else {
            None
        }
    }
}
