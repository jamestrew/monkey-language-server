use super::*;
use crate::Parser;

macro_rules! debug_snapshot {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            let program = Parser::from_source($input).parse_program();

            let (env, diags) = Eval::eval_program(program.nodes, None);

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

debug_snapshot!(bad_minus_prefix, "-true");
