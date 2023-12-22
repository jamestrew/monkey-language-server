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
            println!(
                "{:#?} <- ({}, {}) ==> {}",
                reference,
                pos.line,
                pos.character,
                reference.contains_pos(pos)
            );
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

    pub fn find_pos_def(&self, pos: &Position) -> Option<Range> {
        let pos_env = self.pos_env(pos)?;
        let ident = pos_env.find_pos_ident(pos)?;
        let ident = ident.as_str();

        if Builtin::includes(ident) {
            return None;
        }

        let def = pos_env.find_def(ident)?;
        Some(Range::new(def.start, def.end))
    }

    #[allow(dead_code)]
    pub fn find_references(&self, pos: &Position) -> Vec<Range> {
        let mut refs = Vec::new();

        let env = self.0.read().unwrap();
        for ref_store in &env.refs {
            if ref_store.contains_pos(pos) {
                let ident = ref_store.as_str();

                for env in &env.children {
                    refs.extend(env.collect_scope_refs(ident));
                }
            }
        }

        refs
    }

    fn collect_scope_refs(&self, ident: &str) -> Vec<Range> {
        self.0
            .read()
            .unwrap()
            .refs
            .iter()
            .filter(|ref_span| {
                println!("{:?}", ref_span);
                ref_span.as_str() == ident
            })
            .map(|ref_span| ref_span.as_ref().into())
            .collect()
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
        puts(foo);
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

    #[test]
    fn find_same_scope_outer_def() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(
            env.find_pos_def(&Position::new(2, 5)),
            Some(Range::new(Position::new(1, 4), Position::new(1, 7)))
        );
    }

    #[test]
    fn def_is_in_outer_scope() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(
            env.find_pos_def(&Position::new(6, 15)),
            Some(Range::new(Position::new(1, 4), Position::new(1, 7)))
        );
    }

    #[test]
    fn def_is_in_deep_outer_scope() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(
            env.find_pos_def(&Position::new(16, 15)),
            Some(Range::new(Position::new(1, 4), Position::new(1, 7)))
        );
    }

    #[test]
    fn no_def_since_literal() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(env.find_pos_def(&Position::new(3, 8)), None);
    }

    #[test]
    fn builtin_def() {
        let (_, env) = analyze_source(SOURCE);
        assert_eq!(env.find_pos_def(&Position::new(23, 0)), None);
    }

    #[test]
    fn no_ref_since_litera() {
        let (_, env) = analyze_source(SOURCE);
        assert!(env.find_references(&Position::new(3, 8)).is_empty());
    }

    #[ignore]
    #[test]
    fn references_from_outer_scope() {
        let (_, env) = analyze_source(SOURCE);
        let actual = env.find_references(&Position::new(2, 5));
        let expected = vec![
            Range::new(Position::new(1, 4), Position::new(1, 7)),
            Range::new(Position::new(2, 5), Position::new(2, 8)),
            Range::new(Position::new(4, 11), Position::new(4, 14)),
            Range::new(Position::new(6, 15), Position::new(6, 18)),
            Range::new(Position::new(10, 8), Position::new(10, 11)),
            Range::new(Position::new(15, 8), Position::new(15, 11)),
        ];
        assert_eq!(actual.len(), expected.len());
        assert_eq!(actual, expected);
    }

    #[ignore]
    #[test]
    fn references_inside_out() {
        let (_, env) = analyze_source(SOURCE);
        let actual = env.find_references(&Position::new(6, 16));
        let expected = vec![
            Range::new(Position::new(1, 4), Position::new(1, 7)),
            Range::new(Position::new(2, 5), Position::new(2, 8)),
            Range::new(Position::new(4, 11), Position::new(4, 14)),
            Range::new(Position::new(6, 15), Position::new(6, 18)),
            Range::new(Position::new(10, 8), Position::new(10, 11)),
            Range::new(Position::new(15, 8), Position::new(15, 11)),
        ];
        assert_eq!(actual.len(), expected.len());
        assert_eq!(actual, expected);
    }
}
