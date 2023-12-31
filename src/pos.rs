use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc;

use tower_lsp::lsp_types::{Position, Range};

#[derive(PartialEq, Default, Eq)]
pub struct Pos<T> {
    pub start: Position,
    pub end: Position,
    data: T,
}

impl<T> Pos<T> {
    pub fn new(start: Position, end: Position, data: T) -> Self {
        Self { start, end, data }
    }

    pub fn map<S>(&self, new_data: S) -> Pos<S> {
        Pos {
            start: self.start,
            end: self.end,
            data: new_data,
        }
    }

    pub fn transform<S>(self) -> Pos<S>
    where
        S: From<T>,
    {
        let transformed = S::from(self.data);
        Pos {
            start: self.start,
            end: self.end,
            data: transformed,
        }
    }

    pub fn range(&self) -> Range {
        Range::new(self.start, self.end)
    }

    pub fn take(self) -> T {
        self.data
    }

    pub fn pos_rng_str(&self) -> String {
        pos_rng_str(&self.start, &self.end)
    }

    pub fn contains_pos(&self, pos: &Position) -> bool {
        if pos.line < self.start.line || pos.line > self.end.line {
            return false;
        }

        if pos.line == self.start.line && pos.character < self.start.character {
            return false;
        }

        if pos.line == self.end.line && pos.character >= self.end.character {
            return false;
        }

        true
    }
}

pub fn pos_rng_str(start: &Position, end: &Position) -> String {
    format!(
        "({},{})->({},{})",
        start.line, start.character, end.line, end.character
    )
}

pub fn rng_str(range: &Range) -> String {
    pos_rng_str(&range.start, &range.end)
}

impl<T> From<&Pos<T>> for Range {
    fn from(value: &Pos<T>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl<T> Clone for Pos<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        let inner = &**self;
        self.map(inner.clone())
    }
}

impl<T> Deref for Pos<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> PartialOrd for Pos<T>
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
        self.data.partial_cmp(&other.data)
    }
}

impl<T> Ord for Pos<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.start
            .cmp(&other.start)
            .then_with(|| self.end.cmp(&other.end))
            .then_with(|| self.data.cmp(&other.data))
    }
}

impl<T> Hash for Pos<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start.line.hash(state);
        self.start.character.hash(state);
        self.end.line.hash(state);
        self.end.character.hash(state);
    }
}

impl std::fmt::Debug for Pos<Arc<str>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pos({:?}, {:?})", self.data, self.pos_rng_str())
    }
}

pub trait OpsRange {
    fn contains(&self, pos: &Position) -> bool;
    fn is_empty(&self) -> bool;
}

impl OpsRange for Range {
    fn contains(&self, pos: &Position) -> bool {
        if pos.line < self.start.line || pos.line > self.end.line {
            return false;
        }

        if pos.line == self.start.line && pos.character < self.start.character {
            return false;
        }

        if pos.line == self.end.line && pos.character >= self.end.character {
            return false;
        }

        true
    }

    fn is_empty(&self) -> bool {
        self.start.line == self.end.line && self.start.character == self.end.character
    }
}

#[cfg(test)]
mod test {
    use tower_lsp::lsp_types::{Position, Range};

    use super::*;

    #[test]
    fn range_contains_easy() {
        let range = Range::new(Position::new(0, 0), Position::new(23, 22));
        let pos = Position::new(2, 5);
        assert!(range.contains(&pos));
    }

    #[test]
    fn range_contains_start() {
        let range = Range::new(Position::new(2, 5), Position::new(2, 8));
        let pos = Position::new(2, 5);
        assert!(range.contains(&pos));
    }
}
