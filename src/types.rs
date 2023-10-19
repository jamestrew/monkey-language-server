use std::ops::{Deref, Range};


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T>(usize, Range<usize>, T);

impl<T> Spanned<T> {
    pub fn new(row_num: usize, span: Range<usize>, spannee: T) -> Self {
        Self(row_num, span, spannee)
    }

    pub fn map<S>(&self, spannee: S) -> Spanned<S> {
        Spanned(self.row_num(), self.span(), spannee)
    }

    pub fn row_num(&self) -> usize {
        self.0
    }

    pub fn span(&self) -> Range<usize> {
        self.1.to_owned()
    }

    pub fn start(&self) -> usize {
        self.1.start
    }

    pub fn end(&self) -> usize {
        self.1.end
    }

    pub fn inner(&self) -> &T {
        &self.2
    }

    pub fn take(self) -> T {
        self.2
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.2
    }
}

