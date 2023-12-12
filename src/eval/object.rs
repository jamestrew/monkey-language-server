use crate::types::Spanned;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Object {
    Int,
    Bool,
    String,
    Return,
    Function(Option<usize>, Box<Object>),
    Builtin,
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
            Object::Return => todo!(),
            Object::Function(_, _) => "function",
            Object::Builtin => todo!(),
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

#[derive(Clone, Copy)]
pub enum Builtin {
    Len,
    Puts,
    First,
    Last,
    Rest,
    Push,
}

impl Builtin {
    pub fn object_wrap(self) -> Object {
        match self {
            Builtin::Len => Object::Function(Some(1), Box::new(Object::Nil)),
            Builtin::Puts => Object::Function(None, Box::new(Object::Nil)),
            Builtin::First => Object::Function(Some(1), Box::new(Object::Unknown)),
            Builtin::Last => Object::Function(Some(1), Box::new(Object::Unknown)),
            Builtin::Rest => Object::Function(Some(1), Box::new(Object::Array)),
            Builtin::Push => Object::Function(Some(2), Box::new(Object::Array)),
        }
    }
}
