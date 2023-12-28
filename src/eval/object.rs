use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

use crate::ast::Call;
use crate::diagnostics::{MonkeyError, PosDiagnostic};
use crate::eval::env::Value;
use crate::pos::Pos;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Object {
    Int,
    Bool,
    String,
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

impl std::fmt::Debug for Pos<Object> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pos({:?}, {})", **self, self.pos_rng_str())
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
    pub fn as_value(&self) -> Value {
        Value {
            ident_rng: Default::default(),
            stmt_rng: Default::default(),
            obj: Object::Builtin(self.clone()),
        }
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

    fn detail(&self) -> &'static str {
        match self {
            Builtin::Len => "Returns the length of a collection or string.",
            Builtin::Puts => "Outputs a string to the console or standard output.",
            Builtin::First => "Retrieves the first element from a collection.",
            Builtin::Last => "Retrieves the last element from a collection.",
            Builtin::Rest => "Returns a collection containing all but the first element.",
            Builtin::Push => "Appends an element to the end of a collection.",
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

    pub fn eval(
        self,
        call_expr: &Call,
        args: &[Option<Object>],
    ) -> (Object, Option<PosDiagnostic>) {
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
    ) -> (Object, Option<PosDiagnostic>) {
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
    ) -> (Object, Option<PosDiagnostic>) {
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
    ) -> (Object, Option<PosDiagnostic>) {
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
    ) -> (Object, Option<PosDiagnostic>) {
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
    ) -> Option<PosDiagnostic> {
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

    pub fn completion_items() -> Vec<CompletionItem> {
        Self::variants()
            .iter()
            .map(|func| CompletionItem {
                label: func.ident().to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(func.detail().to_string()),
                ..Default::default()
            })
            .collect()
    }
}
