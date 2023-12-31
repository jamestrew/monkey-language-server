#[cfg(test)]
mod test;

use std::sync::{Arc, RwLock, Weak};

use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Position, Range};

use crate::ast::Block;
use crate::eval::object::{Builtin, Object};
use crate::lexer::keyword_completions;
use crate::pos::{rng_str, OpsRange, Pos};

#[derive(PartialEq, Eq)]
pub struct Value {
    pub ident_rng: Range,
    pub stmt_rng: Range,
    pub obj: Object,
    pub ident: Arc<str>,
}

impl Value {
    pub fn new(ident_rng: Range, stmt_rng: Range, obj: Object, ident: Arc<str>) -> Self {
        Self {
            ident_rng,
            stmt_rng,
            obj,
            ident,
        }
    }

    pub fn arc_new(ident_rng: Range, stmt_rng: Range, obj: Object, ident: Arc<str>) -> Arc<Self> {
        Arc::new(Self::new(ident_rng, stmt_rng, obj, ident))
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Value({:?}, {:?}, {}, {})",
            self.ident,
            self.obj,
            rng_str(&self.ident_rng),
            rng_str(&self.stmt_rng)
        )
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.stmt_rng
            .start
            .cmp(&other.stmt_rng.start)
            .then_with(|| self.stmt_rng.end.cmp(&other.stmt_rng.end))
    }
}

pub struct Scope {
    pub store: Vec<Arc<Value>>,
    pub refs: Vec<Pos<Arc<str>>>,
    pub range: Range,
    parent: Option<Weak<RwLock<Scope>>>,
    pub children: Vec<Env>,
}

impl std::fmt::Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let store = self
            .store
            .iter()
            .filter(|value| !Builtin::includes(value.ident.as_ref()))
            .collect::<Vec<_>>();

        f.debug_struct("Environment")
            .field("store", &store)
            .field("refs", &self.refs)
            .field("scope_range", &rng_str(&self.range))
            .field(
                "parent_range",
                &self
                    .parent
                    .as_ref()
                    .and_then(|weak| weak.upgrade())
                    .map(|parent| {
                        let parent = parent.read().unwrap();
                        rng_str(&parent.range)
                    }),
            )
            .field("children", &self.children)
            .finish()
    }
}

pub struct Env(Arc<RwLock<Scope>>);

impl Env {
    pub fn new(range: Range) -> Self {
        Env(Arc::new(RwLock::new(Scope {
            store: Vec::new(),
            refs: vec![],
            range,
            parent: None,
            children: vec![],
        })))
    }

    pub fn seed_builtin(&self) {
        let store = &mut self.0.write().unwrap().store;

        for func in Builtin::variants() {
            store.push(Arc::new(func.as_value()))
        }
    }

    pub fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }

    pub fn new_child(parent: Env, block: &Block) -> Self {
        let child = Self::new(block.range());
        parent.add_child(child.clone());
        child
    }

    fn add_child(&self, child: Env) {
        let weak_parent = Arc::downgrade(&self.0);
        child.0.write().unwrap().parent = Some(weak_parent);
        self.0.write().unwrap().children.push(child);
    }

    pub fn insert_store(&self, value: Value) {
        self.0.write().unwrap().store.push(Arc::new(value))
    }

    pub fn find_def(&self, ident: &str, pos: Option<&Position>) -> Option<Arc<Value>> {
        let env = self.0.read().ok()?;
        match env
            .store
            .iter()
            .rfind(|&value| Self::is_last_value(ident, value, pos))
        {
            Some(value) => Some(Arc::clone(value)),
            None => match &env.parent {
                Some(weak_parent) => {
                    if let Some(parent) = weak_parent.upgrade() {
                        Env(parent).find_def(ident, pos)
                    } else {
                        None
                    }
                }
                None => None,
            },
        }
    }

    fn is_last_value(ident: &str, value: &Arc<Value>, pos: Option<&Position>) -> bool {
        if let Some(pos) = pos {
            value.ident.as_ref() == ident
                && (Builtin::includes(ident) || value.stmt_rng.start <= *pos)
        } else {
            value.ident.as_ref() == ident
        }
    }

    fn find_pos_ident(&self, pos: &Position) -> Option<Pos<Arc<str>>> {
        let env = self.0.read().unwrap();
        for reference in &env.refs {
            if reference.contains_pos(pos) {
                return Some(reference.clone());
            }
        }

        None
    }

    pub fn insert_ref(&self, ident_pos: Pos<Arc<str>>) {
        self.0.write().unwrap().refs.push(ident_pos)
    }

    pub fn range(&self) -> Range {
        let env = self.0.read().unwrap();
        env.range
    }

    fn pos_env(&self, pos: &Position) -> Option<Env> {
        let env = self.0.read().unwrap();

        for env in &env.children {
            if env.range().contains(pos) {
                if let Some(env) = env.pos_env(pos) {
                    return Some(env.clone());
                }
            }
        }

        if env.range.contains(pos) {
            return Some(self.clone());
        }

        None
    }

    fn def_env(&self, ident: &str, pos: &Position) -> Option<Env> {
        let env = self.0.read().ok()?;
        match env
            .store
            .iter()
            .rfind(|&value| Self::is_last_value(ident, value, Some(pos)))
        {
            Some(_) => Some(self.clone()),
            None => match &env.parent {
                Some(weak_parent) => {
                    if let Some(parent) = weak_parent.upgrade() {
                        Env(parent).def_env(ident, pos)
                    } else {
                        None
                    }
                }
                None => None,
            },
        }
    }

    pub fn find_pos_def(&self, pos: &Position) -> Option<Range> {
        let pos_env = self.pos_env(pos)?;
        let pos_ident = pos_env.find_pos_ident(pos)?;
        let ident = pos_ident.as_ref();

        if Builtin::includes(ident) {
            return None;
        }

        let def_env = pos_env.def_env(ident, pos)?;
        let ident_rng = def_env.find_def(ident, Some(pos))?.ident_rng;
        Some(ident_rng)
    }

    pub fn find_references(&self, pos: &Position) -> Option<Vec<Range>> {
        let pos_env = self.pos_env(pos)?;
        let pos_ident = pos_env.find_pos_ident(pos)?;
        let ident = pos_ident.as_ref();
        let def_env = pos_env.def_env(ident, pos)?;

        Some(def_env.collect_ident_refs(ident))
    }

    fn collect_ident_refs(&self, ident: &str) -> Vec<Range> {
        let env = self.0.read().unwrap();
        let mut refs: Vec<Range> = env
            .refs
            .iter()
            .filter(|&ref_pos| ref_pos.as_ref() == ident)
            .map(|ref_pos| Range::new(ref_pos.start, ref_pos.end))
            .collect();

        env.children
            .iter()
            .for_each(|child| refs.extend(child.collect_ident_refs(ident)));
        refs
    }

    pub fn get_completions(&self, pos: &Position) -> Vec<CompletionItem> {
        let mut items = Builtin::completion_items();
        items.extend(keyword_completions());
        if let Some(pos_env) = self.pos_env(pos) {
            items.extend(pos_env.collect_def_items(pos));
        }
        items
    }

    fn collect_def_items(&self, pos: &Position) -> Vec<CompletionItem> {
        let env = self.0.read().unwrap();
        let mut items = env
            .store
            .iter()
            .filter(|&value| !Builtin::includes(value.ident.as_ref()) && value.stmt_rng.end <= *pos)
            .map(|value| {
                let kind = if matches!(value.obj, Object::Function(_, _)) {
                    CompletionItemKind::FUNCTION
                } else {
                    CompletionItemKind::VALUE
                };
                CompletionItem {
                    label: value.ident.to_string(),
                    kind: Some(kind),
                    ..Default::default()
                }
            })
            .collect::<Vec<_>>();

        if let Some(weak_parent) = &env.parent {
            if let Some(parent) = weak_parent.upgrade() {
                items.extend(Env(parent).collect_def_items(pos));
            }
        }
        items
    }

    pub fn pos_value(&self, pos: &Position) -> Option<Arc<Value>> {
        let pos_env = self.pos_env(pos)?;
        let pos_ident = pos_env.find_pos_ident(pos)?;
        let ident = pos_ident.as_ref();
        let def_env = pos_env.def_env(ident, pos)?;
        let def = def_env.find_def(ident, Some(pos))?;
        Some(Arc::clone(&def))
    }
}

impl std::fmt::Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.read().unwrap())
    }
}
