#![allow(dead_code)]
use std::ops::{Deref, Range};

#[derive(PartialEq, Default)]
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

    pub fn span(&self) -> Range<usize> {
        self.span.to_owned()
    }

    pub fn start(&self) -> usize {
        self.span.start
    }

    pub fn end(&self) -> usize {
        self.span.end
    }

    pub fn take(self) -> T {
        self.data
    }

    pub fn pos_rng_str(&self) -> String {
        format!("{:?}->{:?}", self.start, self.end)
}
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

#[derive(Clone, Copy, PartialEq, Default)]
pub struct Position {
    row: usize,
    col: usize,
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
