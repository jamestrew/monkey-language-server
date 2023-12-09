use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

use crate::eval::object::Object;
use crate::types::Spanned;

pub struct Env<'source>(Rc<RefCell<Environment<'source>>>);

impl<'source> Env<'source> {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            parent: None,
            children: vec![],
        })))
    }

    pub fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }

    pub fn new_child(parent: Env<'source>) -> Self {
        let child = Self::new();
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

    fn push_children(&self, env: Env<'source>) {
        self.0.borrow_mut().children.push(env);
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
}

impl<'source> std::fmt::Debug for Env<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.borrow())
    }
}

struct Environment<'source> {
    store: HashMap<&'source str, Rc<Spanned<Object>>>,
    parent: Option<Weak<RefCell<Environment<'source>>>>,
    children: Vec<Env<'source>>,
}

impl<'source> std::fmt::Debug for Environment<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut store = self.store.iter().collect::<Vec<_>>();
        store.sort_unstable();

        f.debug_struct("Environment")
            .field("store", &store)
            .field(
                "parent",
                &self
                    .parent
                    .as_ref()
                    .and_then(|weak| weak.upgrade())
                    .map(|parent| {
                        let parent = parent.borrow();
                        let mut store = parent.store.iter().collect::<Vec<_>>();
                        store.sort_unstable();
                        format!("{:?}", store)
                    }),
            )
            .field("children", &self.children)
            .finish()
    }
}
