use super::*;
use crate::test_util::input_diagnostics;
use crate::Parser;

macro_rules! debug_snapshot {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            let program = Parser::from_source($input).parse_program();

            let (env, diags) = Eval::eval_program(program);
            let diags = diags
                .iter()
                .map(|diag| format!("{} {}",diag.to_string(), diag.pos_rng_str()))
                .collect::<Vec<_>>();

            insta::with_settings!({
                description => $input,
            }, {
                insta::assert_snapshot!(
                    format!("{:#?}\n\n========================\n\n{:#?}", env, diags)
                );
            });
        }
    };
}

debug_snapshot!(let_stmt_happy, "let a = 12;");
debug_snapshot!(let_stmt_unhappy_1, "let a = ;");

debug_snapshot!(lone_ident, "a");
debug_snapshot!(lone_valid_ident, "let a = 12; a;");
debug_snapshot!(valid_ident, "let a = 12; let b = a;");
debug_snapshot!(bad_token, "@");

// literals
debug_snapshot!(lone_bool, "true");
debug_snapshot!(lone_string, "\"hello world\"");
debug_snapshot!(lone_nil, "nil");

debug_snapshot!(good_minus_prefix_1, "let x = -12;");
debug_snapshot!(bad_minus_prefix_1, "-true");
debug_snapshot!(bad_minus_prefix_2, "let x = -\"foo\";");
debug_snapshot!(curious_minus_prefix, "let x = -[0, true][1];");

debug_snapshot!(good_bang_prefix_1, "let x = !true;");
debug_snapshot!(good_bang_prefix_2, "let x = !12;");
debug_snapshot!(good_bang_prefix_3, "let x = !\"foo\";");
debug_snapshot!(good_bang_prefix_4, "let x = ![];");
debug_snapshot!(good_bang_prefix_5, "let x = !{};");
debug_snapshot!(good_bang_prefix_6, "let x = ![1,2][0];");
debug_snapshot!(good_bang_prefix_7, "let x = !{1: true, 2: false}[1];");

debug_snapshot!(int_add, "let x = 1 + 1;");
debug_snapshot!(int_minus, "let x = 1 - 1;");
debug_snapshot!(int_div, "let x = 1 / 1;");
debug_snapshot!(int_mult, "let x = 1 * 1;");
debug_snapshot!(int_eq, "let x = 1 == 1;");
debug_snapshot!(int_not_eq, "let x = 1 != 1;");
debug_snapshot!(int_lt, "let x = 1 < 1;");
debug_snapshot!(int_gt, "let x = 1 > 1;");
debug_snapshot!(int_bang, "let x = 1 ! 1;");

debug_snapshot!(string_add, "let x = \"foo\" + \"foo\";");
debug_snapshot!(string_minus, "let x = \"foo\" - \"foo\";");
debug_snapshot!(string_div, "let x = \"foo\" / \"foo\";");
debug_snapshot!(string_mult, "let x = \"foo\" * \"foo\";");
debug_snapshot!(string_eq, "let x = \"foo\" == \"foo\";");
debug_snapshot!(string_not_eq, "let x = \"foo\" != \"foo\";");
debug_snapshot!(string_lt, "let x = \"foo\" < \"foo\";");
debug_snapshot!(string_gt, "let x = \"foo\" > \"foo\";");
debug_snapshot!(string_bang, "let x = \"foo\" ! \"foo\";");

debug_snapshot!(bool_add, "let x = true + true;");
debug_snapshot!(bool_minus, "let x = true - true;");
debug_snapshot!(bool_div, "let x = true / true;");
debug_snapshot!(bool_mult, "let x = true * true;");
debug_snapshot!(bool_eq, "let x = true == true;");
debug_snapshot!(bool_not_eq, "let x = true != true;");
debug_snapshot!(bool_lt, "let x = true < true;");
debug_snapshot!(bool_gt, "let x = true > true;");
debug_snapshot!(bool_bang, "let x = true ! true;");

debug_snapshot!(
    bad_mix_infix,
    r#"
let x = "foo" + 1;
let x = true + 1;
let x = "foo" + true;
"#
);

// TODO: last expression is implicit return?
debug_snapshot!(if_ternary, "let x = if (true) { 1 } else { 2 };");
debug_snapshot!(
    if_complex_ternary,
    r#"
let a = 12;
let x = if (a == 12) {
    let b = 42;
    a * b
} else {
    100
};
"#
);
debug_snapshot!(if_condition_not_bool, "let x = if (1) { 1 } else { 2 };");
debug_snapshot!(if_unknown_idents, "let x = if (a) { b } else { c };");
debug_snapshot!(
    inner_scope_references,
    r#"
let x = if (true) {
    let b = 42;
} else {
    let a = b;
};
let a = b;
"#
);
debug_snapshot!(if_bad_condition, "let x = if (@) { 1 } else { 2 };");
debug_snapshot!(if_bad_consq_expr, "if (true) { @ }");

debug_snapshot!(basic_func_1, "let foo = fn() { return 1; };");
debug_snapshot!(basic_func_2, "let add = fn(x,y) { return x + y; };");
debug_snapshot!(
    func_nil_return,
    r#"
let f = fn() { };
let a = f();
"#
);
debug_snapshot!(
    expr_after_func_return,
    r#"
let add = fn(x,y) {
    return x + y;
    let a = "foo";
};
"#
);
debug_snapshot!(
    nested_scope_funct,
    r#"
let add = fn(x,y) {
    let a = x + y;
    let b = 12;
    return fn() {
        return a * b;
    }
};

let mult = add(3, 2);
"#
);

debug_snapshot!(
    basic_func_call,
    r#"
let add = fn(x,y) { return x + y; };

let x = add(2, 4);
"#
);

debug_snapshot!(basic_array, r#"let x = [1, "foo", true];"#);
debug_snapshot!(array_syntax_err, r#"let x = [1, "foo, true];"#);
debug_snapshot!(array_unknown_ident, r#"let x = [1, foo, true];"#);

debug_snapshot!(basic_hash, r#"let x = {1: "foo", "a": 2};"#);
debug_snapshot!(hash_syntax_err, r#"let x = {1: "foo, "a": 2};"#);
debug_snapshot!(hash_unknown_ident, r#"let x = {a: "foo", "a": b};"#);
debug_snapshot!(
    hash_known_ident,
    r#"
let a = 1;
let x = {a: "foo", "a": 2};
"#
);
debug_snapshot!(non_hashable_hash, r#"let x = {{}: 1};"#);

debug_snapshot!(basic_array_index, "let x = [1,2,3][0];");
debug_snapshot!(lone_array_index, "[1,2,3][0]");
debug_snapshot!(basic_hash_index, r#"let x = {1: "foo", "a": 2}[1];"#);
debug_snapshot!(basic_string_index, r#"let x = "foo"[0];"#);

debug_snapshot!(bad_type_index, "let x = 1[1];");
debug_snapshot!(non_int_array_index, "let x = [1,2,3][\"foo\"];");
debug_snapshot!(unhashable_hash_index, "let x = {1: 2}[{}];");

debug_snapshot!(builtin_len, "let x = len([1,2]);");
debug_snapshot!(builtin_len_bad_arg_count_2, "let x = len([1,2], [1]);");
debug_snapshot!(builtin_len_bad_arg_count_0, "let x = len();");
debug_snapshot!(builtin_len_bad_arg, "let x = len(1);");

debug_snapshot!(builtin_puts_1, "puts(\"hello world\")");
debug_snapshot!(builtin_puts_2, "puts(\"hello world\", 1)");
debug_snapshot!(builtin_puts_3, "puts()");

debug_snapshot!(builtin_first_eval_happy, "let x = first([1,2]);");
debug_snapshot!(builtin_first_bad_arg_count_0, "let x = first();");
debug_snapshot!(builtin_first_bad_arg_count_2, "let x = first([1], [1]);");
debug_snapshot!(builtin_first_bad_arg, "let x = first(1);");

debug_snapshot!(builtin_last_eval_happy, "let x = last([1,2]);");
debug_snapshot!(builtin_last_bad_arg_count_0, "let x = last();");
debug_snapshot!(builtin_last_bad_arg_count_2, "let x = last([1], [1]);");
debug_snapshot!(builtin_last_bad_arg, "let x = last(1);");

debug_snapshot!(builtin_rest_eval_happy, "let x = rest([1,2]);");
debug_snapshot!(builtin_rest_bad_arg_count_0, "let x = rest();");
debug_snapshot!(builtin_rest_bad_arg_count_2, "let x = rest([1], [1]);");
debug_snapshot!(builtin_rest_bad_arg, "let x = rest(1);");

debug_snapshot!(builtin_push_happy, "let x = push([1,2,3], 4);");
debug_snapshot!(builtin_push_bad_arg_count, "let x = push([1,2,3]);");
debug_snapshot!(builtin_push_bad_arg, "let x = push(\"foo\", 4);");

#[test]
fn errors() {
    let input = r#"
let foo = true - true;

let x = if (true) {
    let b = 42;
} else {
    let a = b;
};
let a = b;

let add = fn(x,y) {
    return x + y;
    let a = "foo";
};

let ret = add(2);
let x = {fn(x) { return x + 2; }: 1};
"#;

    let program = Parser::from_source(input).parse_program();
    let (_, diags) = Eval::eval_program(program);
    insta::with_settings!({
        description => input,
    }, {
        insta::assert_snapshot!(input_diagnostics(input, diags));
    })
}

#[test]
fn syntax_error_program_pos_1() {
    let s = "let x = -true + 12;";
    let program = Parser::from_source(s).parse_program();
    let (env, diags) = Eval::eval_program(program);
    assert_eq!(diags.len(), 1);
    assert_eq!(env.range().start.line, 0);
    assert_eq!(env.range().start.character, 0);
    assert_eq!(env.range().end.line, 0);
    assert_eq!(env.range().end.character, 19);
}

#[test]
fn syntax_error_program_pos_2() {
    let s = "-true";
    let program = Parser::from_source(s).parse_program();

    assert_eq!(program.range.start.line, 0);
    assert_eq!(program.range.start.character, 0);
    assert_eq!(program.range.end.line, 0);
    assert_eq!(program.range.end.character, 5);

    let (env, diags) = Eval::eval_program(program);
    assert_eq!(diags.len(), 2);
    assert_eq!(env.range().start.line, 0);
    assert_eq!(env.range().start.character, 0);
    assert_eq!(env.range().end.line, 0);
    assert_eq!(env.range().end.character, 5);
}
