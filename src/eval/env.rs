use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::eval::object::{Builtin, Object};
use crate::types::Spanned;

struct Environment<'source> {
    id: usize,
    store: HashMap<&'source str, Rc<Spanned<Object>>>,
    refs: Vec<Rc<Spanned<&'source str>>>,
    parent: Option<Weak<RefCell<Environment<'source>>>>,
    children: Vec<Env<'source>>,
}

impl<'source> std::fmt::Debug for Environment<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut store = self
            .store
            .iter()
            .filter(|(&ident, _)| !Builtin::includes(ident))
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
                    .map(|parent| format!("{}", parent.borrow().id)),
            )
            .field("children", &self.children)
            .field("id", &self.id)
            .finish()
    }
}

pub struct Env<'source>(Rc<RefCell<Environment<'source>>>);

impl<'source> Env<'source> {
    pub fn new(id: usize) -> Self {
        Env(Rc::new(RefCell::new(Environment {
            id,
            store: HashMap::new(),
            refs: vec![],
            parent: None,
            children: vec![],
        })))
    }

    pub fn seed_builtin(&self) {
        let store = &mut self.0.borrow_mut().store;

        for func in Builtin::variants() {
            let ident = &func.ident();
            store.insert(ident, func.object_wrap());
        }
    }

    pub fn env_id(&self) -> usize {
        self.0.borrow().id
    }

    pub fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }

    pub fn new_child(parent: Env<'source>, id: usize) -> Self {
        let child = Self::new(id);
        parent.add_child(child.clone());
        child
    }

    fn add_child(&self, child: Env<'source>) {
        let weak_parent = Rc::downgrade(&self.0);
        child.0.borrow_mut().parent = Some(weak_parent);
        self.0.borrow_mut().children.push(child);
    }

    pub fn insert_store(&self, ident: &'source str, obj: &Rc<Spanned<Object>>) {
        self.0.borrow_mut().store.insert(ident, Rc::clone(obj));
    }

    pub fn find_def(&self, ident: &'source str) -> Option<Rc<Spanned<Object>>> {
        let env = self.0.borrow();
        match env.store.get(ident) {
            Some(obj) => Some(Rc::clone(obj)),
            None => match &self.0.borrow().parent {
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

    pub fn insert_ref(&self, ident: &Rc<Spanned<&'source str>>) {
        self.0.borrow_mut().refs.push(Rc::clone(ident))
    }
}

impl<'source> std::fmt::Debug for Env<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.borrow())
    }
}
