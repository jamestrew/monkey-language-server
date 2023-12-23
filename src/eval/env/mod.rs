#[cfg(test)]
mod test;

use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};

use tower_lsp::lsp_types::{Position, Range};

use crate::ast::Block;
use crate::eval::object::{Builtin, Object};
use crate::spanned::{rng_str, OpsRange, Spanned};

pub struct Scope {
    pub store: HashMap<String, Arc<Spanned<Object>>>,
    pub refs: Vec<Arc<Spanned<String>>>,
    pub range: Range,
    parent: Option<Weak<RwLock<Scope>>>,
    pub children: Vec<Env>,
}

impl std::fmt::Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut store = self
            .store
            .iter()
            .filter(|(ident, _)| !Builtin::includes(ident))
            .collect::<Vec<_>>();
        store.sort_unstable();

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
            store: HashMap::new(),
            refs: vec![],
            range,
            parent: None,
            children: vec![],
        })))
    }

    pub fn seed_builtin(&self) {
        let store = &mut self.0.write().unwrap().store;

        for func in Builtin::variants() {
            let ident = &func.ident();
            store.insert(ident.to_string(), Arc::new(func.spanned_obj()));
        }
    }

    pub fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }

    pub fn new_child(parent: Env, block: &Block) -> Self {
        let child = Self::new(block.range);
        parent.add_child(child.clone());
        child
    }

    fn add_child(&self, child: Env) {
        let weak_parent = Arc::downgrade(&self.0);
        child.0.write().unwrap().parent = Some(weak_parent);
        self.0.write().unwrap().children.push(child);
    }

    pub fn insert_store(&self, ident: &str, obj: &Arc<Spanned<Object>>) {
        self.0
            .write()
            .unwrap()
            .store
            .insert(ident.to_string(), Arc::clone(obj));
    }

    pub fn find_def(&self, ident: &str) -> Option<Arc<Spanned<Object>>> {
        let env = self.0.read().unwrap();
        match env.store.get(ident) {
            Some(obj) => Some(Arc::clone(obj)),
            None => match &env.parent {
                Some(weak_parent) => {
                    if let Some(parent) = weak_parent.upgrade() {
                        Env(parent).find_def(ident)
                    } else {
                        None
                    }
                }
                None => None,
            },
        }
    }

    fn find_pos_ident(&self, pos: &Position) -> Option<Arc<Spanned<String>>> {
        let env = self.0.read().unwrap();
        for reference in &env.refs {
            if reference.contains_pos(pos) {
                return Some(Arc::clone(reference));
            }
        }

        None
    }

    pub fn insert_ref(&self, ident: &Arc<Spanned<String>>) {
        self.0.write().unwrap().refs.push(Arc::clone(ident))
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

    fn def_env(&self, ident: &str) -> Option<Env> {
        let env = self.0.read().unwrap();
        match env.store.get(ident) {
            Some(_) => Some(self.clone()),
            None => match &env.parent {
                Some(weak_parent) => {
                    if let Some(parent) = weak_parent.upgrade() {
                        Env(parent).def_env(ident)
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
        let ident = pos_env.find_pos_ident(pos)?;
        let ident = ident.as_str();

        if Builtin::includes(ident) {
            return None;
        }

        let def_env = pos_env.def_env(ident)?;
        let def = def_env.find_def(ident)?;
        Some(Range::new(def.start, def.end))
    }

    pub fn find_references(&self, pos: &Position) -> Option<Vec<Range>> {
        let pos_env = self.pos_env(pos)?;
        let ident = pos_env.find_pos_ident(pos)?;
        let ident = ident.as_str();
        let def_env = pos_env.def_env(ident)?;

        Some(def_env.collect_scope_refs(ident))
    }

    fn collect_scope_refs(&self, ident: &str) -> Vec<Range> {
        let env = self.0.read().unwrap();
        let mut refs: Vec<Range> = env
            .refs
            .iter()
            .filter(|ref_span| ref_span.as_str() == ident)
            .map(|ref_span| Range::new(ref_span.start, ref_span.end))
            .collect();

        env.children
            .iter()
            .for_each(|child| refs.extend(child.collect_scope_refs(ident)));
        refs
    }
}

impl std::fmt::Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.read().unwrap())
    }
}
