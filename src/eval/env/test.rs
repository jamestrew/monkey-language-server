use tower_lsp::lsp_types::*;

use crate::analyze_source;
use crate::eval::object::Builtin;
use crate::eval::Object;

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

fn builtin_keyword_completions() -> Vec<CompletionItem> {
    use crate::lexer::keyword_completions;
    Builtin::completion_items()
        .into_iter()
        .chain(keyword_completions())
        .collect()
}

macro_rules! assert_comp_items {
    ($expected:expr, $actual:expr) => {
        assert_eq!($expected.len(), $actual.len());
        for item in $expected.iter() {
            assert!(
                $actual.contains(item),
                "Expected item not found in actual {:#?}",
                item
            );
        }
    };
}

fn comp_item(label: &str, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(kind),
        ..Default::default()
    }
}

#[test]
fn completion_for_empty_source() {
    let (_, env) = analyze_source("");
    let comps = env.get_completions(&Position::new(0, 0));
    let expected = builtin_keyword_completions();
    assert_comp_items!(comps, expected);
}

#[test]
fn top_of_the_file_completions() {
    let (_, env) = analyze_source(SOURCE);
    let comps = env.get_completions(&Position::new(0, 0));
    let expected = builtin_keyword_completions();
    assert_comp_items!(comps, expected);
}

#[test]
fn bottom_of_the_file_completions() {
    let (_, env) = analyze_source(SOURCE);
    let comps = env.get_completions(&Position::new(24, 0));
    let mut expected = builtin_keyword_completions();
    expected.extend(vec![
        comp_item("foo", CompletionItemKind::VALUE),
        comp_item("a", CompletionItemKind::VALUE),
        comp_item("x", CompletionItemKind::VALUE),
        comp_item("add_maybe", CompletionItemKind::FUNCTION),
    ]);
    assert_comp_items!(comps, expected);
}

#[test]
fn inner_scope_completions() {
    let (_, env) = analyze_source(SOURCE);
    let comps = env.get_completions(&Position::new(7, 0));
    let mut expected = builtin_keyword_completions();
    expected.extend(vec![
        comp_item("foo", CompletionItemKind::VALUE),
        comp_item("a", CompletionItemKind::VALUE),
        comp_item("b", CompletionItemKind::VALUE),
        comp_item("bar", CompletionItemKind::VALUE),
    ]);
    assert_comp_items!(comps, expected);
}

#[test]
fn literal_definition_hover() {
    let (_, env) = analyze_source(SOURCE);
    let value = env
        .pos_value(&Position::new(1, 4))
        .expect("obj should be some");
    assert_eq!(value.obj, Object::Bool);
    assert_eq!(
        value.ident_rng,
        Range::new(Position::new(1, 4), Position::new(1, 7))
    );
}

#[test]
fn literal_reference_hover() {
    let (_, env) = analyze_source(SOURCE);
    let value = env
        .pos_value(&Position::new(2, 7))
        .expect("obj should be some");

    assert_eq!(value.obj, Object::Bool);
    assert_eq!(
        value.ident_rng,
        Range::new(Position::new(1, 4), Position::new(1, 7))
    );
}

#[test]
fn no_hover_on_literal() {
    let (_, env) = analyze_source(SOURCE);
    let value = env.pos_value(&Position::new(1, 7));
    assert!(value.is_none());
}

#[test]
fn function_definition_hover() {
    let (_, env) = analyze_source(SOURCE);
    let value = env
        .pos_value(&Position::new(14, 9))
        .expect("obj should be some");

    assert_eq!(value.obj, Object::Function(2, Box::new(Object::Unknown)));
    assert_eq!(
        value.ident_rng,
        Range::new(Position::new(14, 4), Position::new(14, 13))
    );
}

#[test]
fn builtin_hover() {
    let (_, env) = analyze_source(SOURCE);
    let value = env
        .pos_value(&Position::new(2, 0))
        .expect("obj should be some");

    assert_eq!(value.obj, Object::Builtin(Builtin::Puts));
    assert_eq!(
        value.ident_rng,
        Range::new(Position::new(0, 0), Position::new(0, 0))
    );
}
