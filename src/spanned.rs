use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, Range};

use tower_lsp::lsp_types::{Position, Range as LspRange};

#[derive(PartialEq, Default, Eq)]
pub struct Spanned<T> {
    pub start: Position,
    pub end: Position,
    pub span: Range<usize>,
    data: T,
}

impl<T> Spanned<T> {
    pub fn new(start: Position, end: Position, span: Range<usize>, data: T) -> Self {
        Self {
            start,
            end,
            span,
            data,
        }
    }

    pub fn map<S>(&self, new_data: S) -> Spanned<S> {
        Spanned {
            start: self.start,
            end: self.end,
            span: self.span(),
            data: new_data,
        }
    }

    pub fn transform<S>(self) -> Spanned<S>
    where
        S: From<T>,
    {
        let transformed = S::from(self.data);
        Spanned {
            start: self.start,
            end: self.end,
            span: self.span,
            data: transformed,
        }
    }

    pub fn span(&self) -> Range<usize> {
        self.span.to_owned()
    }

    pub fn take(self) -> T {
        self.data
    }

    pub fn pos_rng_str(&self) -> String {
        pos_rng_str(&self.start, &self.end)
    }

    pub fn contains_pos(&self, pos: &Position) -> bool {
        (self.start.line..self.end.line).contains(&pos.line)
            && (self.start.character..self.end.character).contains(&pos.character)
    }
}

pub fn pos_rng_str(start: &Position, end: &Position) -> String {
    format!(
        "({},{})->({},{})",
        start.line, start.character, end.line, end.character
    )
}

pub fn rng_str(range: &LspRange) -> String {
    pos_rng_str(&range.start, &range.end)
}

impl<T> From<&Spanned<T>> for LspRange {
    fn from(value: &Spanned<T>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl<T> Clone for Spanned<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        let inner = &**self;
        self.map(inner.clone())
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> PartialOrd for Spanned<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.start.partial_cmp(&other.start) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.end.partial_cmp(&other.end) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.span.start.partial_cmp(&other.span.start) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.span.end.partial_cmp(&other.span.end) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.data.partial_cmp(&other.data)
    }
}

impl<T> Ord for Spanned<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.start
            .cmp(&other.start)
            .then_with(|| self.end.cmp(&other.end))
            .then_with(|| self.span.start.cmp(&other.span.start))
            .then_with(|| self.span.end.cmp(&other.span.end))
            .then_with(|| self.data.cmp(&other.data))
    }
}

impl<T> Hash for Spanned<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.span.hash(state);
    }
}

impl std::fmt::Debug for Spanned<String> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Spanned({:?}, {:?})", self.data, self.pos_rng_str())
    }
}
