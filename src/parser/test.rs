use std::ops::Range;

use paste::paste;

use super::*;
use crate::diagnostics::SpannedDiagnostic;
use crate::lexer::*;
use crate::test_util::input_diagnostics;
use crate::types::*;

pub fn debug_new(
    start: Position,
    end: Position,
    span: Range<usize>,
    kind: TokenKind,
    slice: &'_ str,
) -> Token<'_> {
    Spanned::new(start, end, span, _Token::new(kind, slice))
}

#[test]
fn new_parser() {
    let source = "let a = 123;";
    let parser = Parser::from_source(source);

    let first = debug_new(
        Position::new(0, 0),
        Position::new(0, 3),
        0..3,
        TokenKind::Let,
        "let",
    );
    let second = debug_new(
        Position::new(0, 4),
        Position::new(0, 5),
        4..5,
        TokenKind::Identifier,
        "a",
    );

    assert_eq!(parser.curr_token, Some(Ok(first)));
    assert_eq!(parser.peek_token, Some(Ok(second)));
}

macro_rules! debug_snapshot {
    ($name:ident, $input:expr) => {
        paste! {
            #[test]
            fn $name() {
                let program = Parser::from_source($input).parse_program();
                let errors = program
                    .collect_errors()
                    .into_iter()
                    .map(|err| err.into())
                    .collect::<Vec<SpannedDiagnostic>>();
                let errors = input_diagnostics($input, errors);
                let result = format!("{:#?}\n\n===================================\n\n{}", program, errors);
                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_snapshot!(result);
                });
            }
        }
    };
}

debug_snapshot!(lexer_erro, "@");
debug_snapshot!(bool_expr, "true");

// maybe these are warnings? "Expression value is unused" <- python
debug_snapshot!(int_expr, "123");
debug_snapshot!(nil_expr, "nil");
debug_snapshot!(string_literal, "\"hello world\"");
debug_snapshot!(identifier, "x");

debug_snapshot!(let_statement_1, "let x = 1;");
debug_snapshot!(let_statement_2, "let x = 2;;;");
debug_snapshot!(let_statement_broken_1, "let x = ;");
debug_snapshot!(let_statement_broken_2, "let x;");
debug_snapshot!(let_statement_broken_3, "let;");
debug_snapshot!(let_statement_broken_4, "le a = 1;");
debug_snapshot!(let_statement_broken_5, "let a = @;");
debug_snapshot!(let_statement_broken_6, "let a = @;42;");
debug_snapshot!(let_statement_broken_7, "let a = 1 2;");

debug_snapshot!(return_happy_1, "return 1;");
debug_snapshot!(return_happy_2, "return nil;");
debug_snapshot!(return_happy_3, "return;");
debug_snapshot!(return_happy_4, "return 1 + 1;");

debug_snapshot!(return_unhappy_1, "return @;");
debug_snapshot!(return_unhappy_2, "return ];");
debug_snapshot!(return_unhappy_3, "return [;");
debug_snapshot!(return_unhappy_4, "return x y;");

debug_snapshot!(prefix_happy_1, "-1");
debug_snapshot!(prefix_happy_2, "!true");
debug_snapshot!(prefix_happy_3, "let b = !true");

debug_snapshot!(prefix_unhappy_1, "-@");

debug_snapshot!(infix_happy_1, "1 + 1;\na - 2;\n1 * 1;\n1 / 2;");
debug_snapshot!(
    infix_happy_2,
    "\"foo\" == a;\n1 == 1;\n2 != 1;\n4 < 5;\n 5 > 4;"
);

// NEEDS IMPROVEMENT
// error position is on the operator
debug_snapshot!(infix_unhappy_1, "1 + +;");
debug_snapshot!(infix_unhappy_2, "1 +;");
debug_snapshot!(infix_unhappy_3, "1 + /");
debug_snapshot!(infix_unhappy_4, "1 + @");
debug_snapshot!(infix_unhappy_5, "@ + 1");

debug_snapshot!(grouped_expr, "(1 + 1)");

debug_snapshot!(operator_precedence_1, "-a * b");
debug_snapshot!(operator_precedence_2, "!-a");
debug_snapshot!(operator_precedence_3, "a + b + c");
debug_snapshot!(operator_precedence_4, "a + b - c");
debug_snapshot!(operator_precedence_5, "a * b * c");
debug_snapshot!(operator_precedence_6, "a * b / c");
debug_snapshot!(operator_precedence_7, "a + b / c");
debug_snapshot!(operator_precedence_8, "a / b + c");
debug_snapshot!(operator_precedence_9, "a + b * c + d / e - f");
debug_snapshot!(operator_precedence_10, "5 > 4 == 3 < 4");
debug_snapshot!(operator_precedence_11, "5 < 4 != 3 > 4");
debug_snapshot!(operator_precedence_12, "3 + 4 * 5 == 3 * 1 + 4 * 5");
debug_snapshot!(operator_precedence_13, "true");
debug_snapshot!(operator_precedence_14, "false");
debug_snapshot!(operator_precedence_15, "3 > 5 == false");
debug_snapshot!(operator_precedence_16, "3 < 5 == true");
debug_snapshot!(operator_precedence_17, "1 + (2 + 3) + 4");
debug_snapshot!(operator_precedence_19, "(5 + 5) * 2");
debug_snapshot!(operator_precedence_21, "2 / (5 + 5)");
debug_snapshot!(operator_precedence_23, "-(5 + 5)");
debug_snapshot!(operator_precedence_25, "!(true == true)");
debug_snapshot!(operator_precedence_27, "a + add(b * c) + d");
debug_snapshot!(operator_precedence_22, "add(a, 2 * 3, add(6, 7 * 8))");
debug_snapshot!(operator_precedence_24, "add(a + b + c * d / f + g)");
debug_snapshot!(operator_precedence_26, "a * [1, 2, 3, 4][b * c] * d");
debug_snapshot!(operator_precedence_28, "add(a * b[2], b[1], 2 * [1, 2][1])");

debug_snapshot!(if_expr_happy_1, "if (x) { x }");
debug_snapshot!(if_expr_happy_2, "if (x < y) { x }");
debug_snapshot!(if_expr_happy_3, "if (x < y) { x } else { y }");
debug_snapshot!(
    if_expr_happy_4,
    "if (x < y) { x } else { let z = x + y; z }"
);

debug_snapshot!(if_expr_unhappy_1, "if (x +) { x }");
debug_snapshot!(if_expr_unhappy_2, "if (x +) { x } else { x + 1 }");
debug_snapshot!(if_expr_unhappy_3, "if (x) { let x = 1; x < }");
debug_snapshot!(if_expr_unhappy_4, "if (x) { let x = 1");
debug_snapshot!(if_expr_unhappy_5, "if (x");
debug_snapshot!(if_expr_unhappy_6, "if (x { x }"); // NEEDS IMPROVEMENT
debug_snapshot!(if_expr_unhappy_7, "if (x) { x else { x }");
debug_snapshot!(if_expr_unhappy_8, "if (x) { x + } ");
debug_snapshot!(if_expr_unhappy_9, "if (x) { x + } else { x - 1; }");
debug_snapshot!(if_expr_unhappy_10, "if (@) { x }");
debug_snapshot!(if_expr_unhappy_11, "if (x) { @ }");
debug_snapshot!(if_expr_unhappy_12, "if (x) { x } else { @ }");

debug_snapshot!(fn_expr_happy_1, "fn() {}");
debug_snapshot!(fn_expr_happy_2, "fn(x) {}");
debug_snapshot!(fn_expr_happy_3, "fn(x, y, z) {}");
debug_snapshot!(fn_expr_happy_4, "fn(x) { return x; }");
debug_snapshot!(fn_expr_happy_5, "fn(x, y) { let z = x + y; return z; }");
debug_snapshot!(fn_expr_happy_6, "fn(x, y, z,) {}");
debug_snapshot!(fn_expr_happy_7, "fn(x, y, z,) { if (x) { y } else { z } }");

debug_snapshot!(fn_expr_unhappy_1, "fn( {}"); // NEEDS IMPROVEMENT
debug_snapshot!(fn_expr_unhappy_2, "fn(1+1) {}");
debug_snapshot!(fn_expr_unhappy_3, "fn(x y) {}");
debug_snapshot!(fn_expr_unhappy_4, "fn(x y z) {}");
debug_snapshot!(fn_expr_unhappy_5, "fn(x y, z) }{}");
debug_snapshot!(fn_expr_unhappy_6, "fn(, x) {}");
debug_snapshot!(
    fn_expr_unhappy_7,
    "fn(x, y, z,) { if (x) { y } else { z  }; 5"
); // NEEDS IMPROVEMENT
debug_snapshot!(fn_expr_unhappy_8, "fn(1+) {}");
debug_snapshot!(fn_expr_unhappy_9, "fn(x+) {}");
debug_snapshot!(fn_expr_unhappy_10, "fn(@) { return x; }");
debug_snapshot!(fn_expr_unhappy_11, "fn(@, x) { return x; }");
debug_snapshot!(fn_expr_unhappy_12, "fn(x) { @ }");
debug_snapshot!(fn_expr_unhappy_13, "fn(x) { x + 1; @ }");

debug_snapshot!(fn_call_happy_1, "add()");
debug_snapshot!(fn_call_happy_2, "add(x)");
debug_snapshot!(fn_call_happy_3, "add(x, y);");
debug_snapshot!(fn_call_happy_4, "fn(x, y){ return x + y; }(1, 2);");
debug_snapshot!(fn_call_happy_5, "add(x, y,);");
debug_snapshot!(fn_call_happy_6, "add(6, 7 * 8); 2");
debug_snapshot!(fn_call_happy_7, "add(2, add(6, 7 * 8))");

debug_snapshot!(fn_call_unhappy_1, "add(");
debug_snapshot!(fn_call_unhappy_2, "add)");
debug_snapshot!(fn_call_unhappy_3, "add(x y)");
debug_snapshot!(fn_call_unhappy_4, "add(x y, z)");
debug_snapshot!(fn_call_unhappy_5, "add(, x)");
debug_snapshot!(fn_call_unhappy_6, "add(x +)");
debug_snapshot!(fn_call_unhappy_7, "add(+)");
debug_snapshot!(fn_call_unhappy_8, "add(@)");
debug_snapshot!(fn_call_unhappy_9, "add(2, @)");
debug_snapshot!(fn_call_unhappy_10, "add(@, 2)");

debug_snapshot!(array_happy_1, "[1,2,3]; 5");
debug_snapshot!(array_happy_2, "[1,\"foo\",3];");
debug_snapshot!(array_happy_3, "[1,\"foo\",fn(x) { x + 1 }];");
debug_snapshot!(array_happy_4, "[1,2,3, [4]]; 5");

debug_snapshot!(array_unhappy_1, "[1,2 3]; 5");
debug_snapshot!(array_unhappy_2, "[1,2");
debug_snapshot!(array_unhappy_3, "[1,+]");
debug_snapshot!(array_unhappy_4, "[1,@]");

debug_snapshot!(hash_happy_1, "{}");
debug_snapshot!(hash_happy_2, r#"{"foo": "bar"}"#);
debug_snapshot!(hash_happy_3, r#"{"foo": "bar", "eggs": "spam"}"#);
debug_snapshot!(hash_happy_4, r#"{true: "bar", 2: "baz"}"#);
debug_snapshot!(hash_happy_5, r#"{"foo": "bar",}"#);

debug_snapshot!(hash_unhappy_1, "{A}");
debug_snapshot!(hash_unhappy_2, "{1:}");
debug_snapshot!(hash_unhappy_3, "{1:2 2:3}");
debug_snapshot!(hash_unhappy_4, "{1:2 2}");
debug_snapshot!(hash_unhappy_5, "{1:2");
debug_snapshot!(hash_unhappy_6, "{1:2; 5;");
debug_snapshot!(hash_unhappy_7, "{,1:2}");
debug_snapshot!(hash_unhappy_8, "{1:@}");
debug_snapshot!(hash_unhappy_9, "{@:2}");
debug_snapshot!(hash_unhappy_10, "{1:2, @:2}");

debug_snapshot!(array_index_happy_1, "foo[1]");
debug_snapshot!(array_index_happy_2, "foo[add(3)]");
debug_snapshot!(array_index_happy_3, "foo[1+2]");
debug_snapshot!(array_index_happy_4, "[1,2,3,4][1+2]");

debug_snapshot!(hash_index_happy_1, r#"{"foo": "bar"}["foo"]"#);
debug_snapshot!(hash_index_happy_2, r#"{true: "bar", 2: "baz"}[true]"#);
debug_snapshot!(hash_index_happy_3, r#"{true: "bar", 2: "baz"}[1+1]"#);

debug_snapshot!(array_index_unhappy_1, "foo[1");
debug_snapshot!(array_index_unhappy_2, "foo[1+");
debug_snapshot!(array_index_unhappy_3, "foo[1,");
debug_snapshot!(array_index_unhappy_4, "foo[1,2]");
debug_snapshot!(array_index_unhappy_5, "foo[@]");

debug_snapshot!(mixed, "let foo = 1;\n[1, 2];");
debug_snapshot!(mixed_err, "let foo;\n[1 2, 3];");

#[test]
fn errors() {
    let input = "
let x = 12;
let y;

let add = fn(x y) {
    return x y;
}

return y
x
";

    let program = Parser::from_source(input).parse_program();
    let errors = program
        .collect_errors()
        .into_iter()
        .map(|err| err.into())
        .collect::<Vec<SpannedDiagnostic>>();
    insta::with_settings!({
        description => input,
    }, {
        insta::assert_snapshot!(input_diagnostics(input, errors));
    })
}
