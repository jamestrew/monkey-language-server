#![allow(unused)]

use crate::ast::Statement;
use crate::diagnostics::{SpannedDiagnostic, SpannedError};
use crate::Node;

pub enum Object {}

pub struct Env {}

pub struct Eval {
    env: Env,
}

impl<'source> Eval {
    pub fn eval_program(nodes: Vec<Node<'source>>) -> Vec<SpannedDiagnostic> {
        let mut eval = Eval { env: Env {} };

        let mut diagnostics = Vec::new();
        for node in nodes {
            match node {
                Node::Statement(stmt) => diagnostics.extend(eval.eval_statements(stmt)),
                Node::Error(err) => diagnostics.push(err.into()),
            }
        }
        diagnostics
    }

    fn eval_statements(&mut self, stmt: Statement) -> Vec<SpannedDiagnostic> {
        todo!()
    }
}
