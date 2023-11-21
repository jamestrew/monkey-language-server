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
