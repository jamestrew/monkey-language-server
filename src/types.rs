use std::cmp::Ordering;
use std::ops::{Deref, Range};

use tower_lsp::lsp_types::{Position as LspPosition, Range as LspRange};

use crate::eval::Variable;

#[derive(PartialEq, Default, Eq, Hash)]
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

    pub fn start(&self) -> usize {
        self.span.start
    }

    pub fn end(&self) -> usize {
        self.span.end
    }

    pub fn lsp_range(&self) -> LspRange {
        LspRange {
            start: LspPosition::new(self.start.row as u32, self.start.col as u32),
            end: LspPosition::new(self.end.row as u32, self.end.col as u32),
        }
    }

    pub fn take(self) -> T {
        self.data
    }

    pub fn pos_rng_str(&self) -> String {
        format!("{:?}->{:?}", self.start, self.end)
    }
}

impl<T> Spanned<T>
where
    T: Clone,
{
    pub fn clone_inner(&self) -> Self {
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

impl std::fmt::Debug for Spanned<Variable<'_>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Spanned({:?}, {}, {})",
            self.data.0,
            self.data.1,
            self.pos_rng_str()
        )
    }
}

#[derive(Clone, Copy, PartialEq, Default, Eq, Hash, Ord, PartialOrd)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.row, self.col)
    }
}
