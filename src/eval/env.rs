use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};

use crate::eval::object::{Builtin, Object};
use crate::spanned::Spanned;

pub struct Environment {
    id: usize,
    pub store: HashMap<String, Arc<Spanned<Object>>>,
    pub refs: Vec<Arc<Spanned<String>>>,
    parent: Option<Weak<RwLock<Environment>>>,
    pub children: Vec<Env>,
}

impl std::fmt::Debug for Environment {
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
            .field(
                "parent_id",
                &self
                    .parent
                    .as_ref()
                    .and_then(|weak| weak.upgrade())
                    .map(|parent| format!("{}", parent.read().unwrap().id)),
            )
            .field("children", &self.children)
            .field("id", &self.id)
            .finish()
    }
}

pub struct Env(Arc<RwLock<Environment>>);

impl Env {
    pub fn new(id: usize) -> Self {
        Env(Arc::new(RwLock::new(Environment {
            id,
            store: HashMap::new(),
            refs: vec![],
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

    pub fn env_id(&self) -> usize {
        self.0.read().unwrap().id
    }

    pub fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }

    pub fn new_child(parent: Env, id: usize) -> Self {
        let child = Self::new(id);
        parent.add_child(child.clone());
        child
    }

    pub fn add_child(&self, child: Env) {
        let weak_parent = Arc::downgrade(&self.0);
        child.0.write().unwrap().parent = Some(weak_parent);
        self.0.write().unwrap().children.push(child);
    }

    pub fn insert_store(&self, ident: String, obj: &Arc<Spanned<Object>>) {
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

    pub fn insert_ref(&self, ident: &Arc<Spanned<String>>) {
        self.0.write().unwrap().refs.push(Arc::clone(ident))
    }
}

impl std::fmt::Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.read().unwrap())
    }
}
