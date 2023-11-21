use super::*;
use crate::Parser;

macro_rules! debug_snapshot {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            let program = Parser::from_source($input).parse_program();

            let res = Eval::eval_program(program.nodes, None);

            insta::with_settings!({
                description => $input,
            }, {
                insta::assert_debug_snapshot!(res);
            });
        }
    };
}


debug_snapshot!(foo, "let a = 12;");
