mod ast;
mod diagnostics;
mod eval;
mod lexer;
mod parser;
mod types;

#[cfg(test)]
mod test_util;

use std::collections::HashMap;
use std::sync::Arc;

pub use ast::Node;
pub use eval::Eval;
use eval::{Env as EvalEnv, Object};
pub use parser::Parser;
use tower_lsp::lsp_types::Diagnostic;
use types::Spanned;

pub struct Env {
    store: HashMap<String, Arc<Spanned<Object>>>,
    refs: Vec<Arc<Spanned<String>>>,
    children: Vec<Env>,
}

impl From<EvalEnv> for Env {
    fn from(env: EvalEnv) -> Self {
        let env = env.take_environment();
        Self {
            store: env.store,
            refs: env.refs,
            children: env
                .children
                .into_iter()
                .map(|child| child.into())
                .collect::<Vec<_>>(),
        }
    }
}

pub fn analyze_source(source: &str) -> (Vec<Diagnostic>, Env) {
    let program = Parser::from_source(source).parse_program();
    let (eval_env, diags) = Eval::eval_program(program.nodes);

    let diags = diags
        .iter()
        .map(|diag| {
            Diagnostic::new(
                diag.lsp_range(),
                Some(diag.severity()),
                None,
                None,
                diag.to_string(),
                None,
                None,
            )
        })
        .collect::<Vec<_>>();

    (diags, eval_env.into())
}
