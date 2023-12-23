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

puts(add_maybe(a, x));"#;

fn foo_references() -> Vec<Range> {
    vec![
        Range::new(Position::new(1, 4), Position::new(1, 7)),
        Range::new(Position::new(2, 5), Position::new(2, 8)),
        Range::new(Position::new(4, 11), Position::new(4, 14)),
        Range::new(Position::new(6, 15), Position::new(6, 18)),
        Range::new(Position::new(10, 8), Position::new(10, 11)),
        Range::new(Position::new(15, 8), Position::new(15, 11)),
        Range::new(Position::new(16, 13), Position::new(16, 16)),
    ]
}

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
    assert!(env.find_references(&Position::new(3, 8)).is_none());
}

#[test]
fn references_from_outer_scope() {
    let (_, env) = analyze_source(SOURCE);
    let actual = env.find_references(&Position::new(2, 5));
    let expected = foo_references();
    assert!(actual.is_some());
    if let Some(actual) = actual {
        assert_eq!(actual.len(), expected.len());
        assert_eq!(actual, expected);
    }
}

#[test]
fn references_inside_out() {
    let (_, env) = analyze_source(SOURCE);
    let actual = env.find_references(&Position::new(6, 16));
    let expected = foo_references();
    assert!(actual.is_some());
    if let Some(actual) = actual {
        assert_eq!(actual.len(), expected.len());
        assert_eq!(actual, expected);
    }
}

#[test]
fn deep_nested_reference() {
    let (_, env) = analyze_source(SOURCE);
    let actual = env.find_references(&Position::new(16, 15));
    let expected = foo_references();
    assert!(actual.is_some());
    if let Some(actual) = actual {
        assert_eq!(actual.len(), expected.len());
        assert_eq!(actual, expected);
    }
}
