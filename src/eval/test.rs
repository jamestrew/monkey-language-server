use super::*;
use crate::test_util::input_diagnostics;
use crate::Parser;

macro_rules! debug_snapshot {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            let program = Parser::from_source($input).parse_program();

            let (env, diags) = Eval::eval_program(program.nodes);

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

// TODO: unknown identifier (check nested blocks)
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
// debug_snapshot!(curious_minus_prefix, "let x = -[0, true][1];");

debug_snapshot!(good_bang_prefix_1, "let x = !true;");
debug_snapshot!(good_bang_prefix_2, "let x = !12;");
debug_snapshot!(good_bang_prefix_3, "let x = !\"foo\";");
// debug_snapshot!(good_bang_prefix_4, "let x = ![];");
// debug_snapshot!(good_bang_prefix_5, "let x = !{};");
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
    bad_mix_infox,
    r#"
let x = "foo" + 1;
let x = true + 1;
let x = "foo" + true;
"#
);

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

debug_snapshot!(basic_func, "let add = fn(x,y) { return x + y; };");
// TODO: add more func expression tests

debug_snapshot!(
    basic_func_call,
    r#"
let add = fn(x,y) { return x + y; };

let x = add(2, 4);
"#
);

#[test]
fn errors() {
    let input = "
let foo = true - true;

let x = if (true) {
    let b = 42;
} else {
    let a = b;
};
let a = b;
";

    let program = Parser::from_source(input).parse_program();
    let (_, diags) = Eval::eval_program(program.nodes);
    insta::with_settings!({
        description => input,
    }, {
        insta::assert_snapshot!(input_diagnostics(input, diags));
    })
}
