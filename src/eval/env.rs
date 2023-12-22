use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};

use tower_lsp::lsp_types::*;

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

    pub fn insert_ref(&self, ident: &Arc<Spanned<String>>) {
        self.0.write().unwrap().refs.push(Arc::clone(ident))
    }

    pub fn find_pos_def(&self, pos: &Position) -> Option<Range> {
        let env = self.0.read().unwrap();
        for ref_store in &env.refs {
            if ref_store.contains_pos(pos) {
                let ident = &***ref_store.as_ref();
                if Builtin::includes(ident) {
                    return None;
                }
                return self.find_def(ident).map(|span| Range::from(&*span));
            }
        }

        for env in &env.children {
            if let Some(range) = env.find_pos_def(pos) {
                return Some(range);
            }
        }

        None
    }
}

impl std::fmt::Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.read().unwrap())
    }
}

#[cfg(test)]
mod test {
    use tower_lsp::lsp_types::*;

    use crate::analyze_source;

    const SOURCE: &str = r#"
let foo = 1 == 2;
puts(foo)
let a = 69;
let x = if(foo) {
    let b = 42;
    let bar = !foo;
    b
} else {
    let b = 420 + a;
    let foo = true;
    b
};

let add_maybe = fn(x, y) {
    if (foo) {
        return x + y;
    } else {
        puts("sike!");
        return x - y;
    }
};

puts(add_maybe(a, x));
    "#;

    #[test]
    fn no_diags_message() {
        let (diags, _) = analyze_source(SOURCE);
        assert!(diags.is_empty());
    }

    #[ignore = "this will be re-implemented"]
    #[test]
    fn find_same_scope_outer_def() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(
            env.find_pos_def(&Position::new(2, 5)),
            Some(Range::new(Position::new(1, 4), Position::new(1, 7),))
        );
    }

    #[ignore = "this will be re-implemented"]
    #[test]
    fn def_is_in_outer_scope() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(
            env.find_pos_def(&Position::new(6, 15)),
            Some(Range::new(Position::new(1, 4), Position::new(1, 7),))
        );
    }

    #[test]
    fn no_def_since_literal() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(env.find_pos_def(&Position::new(3, 8)), None,);
    }

    #[test]
    fn builtin_def() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(env.find_pos_def(&Position::new(23, 0)), None,);
    }
}
