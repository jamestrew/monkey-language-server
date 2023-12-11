use crate::types::Spanned;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Object {
    Int,
    Bool,
    String,
    Return,
    Function(usize, Box<Object>),
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
