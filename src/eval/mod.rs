#![allow(unused)]

#[cfg(test)]
mod test;

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::ast::*;
use crate::diagnostics::SpannedDiagnostic;
use crate::Node;

#[derive(Debug)]
pub enum Object {}

type Env<'source> = Rc<RefCell<Environment<'source>>>;
type Store<'source> = HashSet<&'source str>;

#[derive(Debug, Default)]
pub struct Environment<'source> {
    store: Store<'source>,
    outer: Option<Env<'source>>,
    inner: Vec<Env<'source>>,
}

impl<'source> Environment<'source> {
    pub fn new_env(outer: Option<Env<'source>>) -> Env {
        let env = Self {
            outer,
            ..Default::default()
        };
        Rc::new(RefCell::new(env))
    }
}

pub struct Eval<'source> {
    env: Env<'source>,
}

impl<'source> Eval<'source> {
    pub fn eval_program(
        nodes: Vec<Node<'source>>,
        outer_env: Option<Env<'source>>,
    ) -> (Env<'source>, Vec<SpannedDiagnostic>) {
        let mut eval = Eval {
            env: Environment::new_env(outer_env),
        };

        let mut diagnostics = Vec::new();
        for node in nodes {
            match node {
                Node::Statement(stmt) => diagnostics.extend(eval.eval_statements(stmt)),
                Node::Error(err) => diagnostics.push(err.into()),
            }
        }
        (eval.env, diagnostics)
    }

    fn eval_statements(&mut self, stmt: Statement<'source>) -> Vec<SpannedDiagnostic> {
        match stmt {
            Statement::Let(stmt) => self.eval_let_stmt(stmt),
            Statement::Return(stmt) => self.eval_return_stmt(stmt),
            Statement::Block(stmt) => self.eval_block_stmt(stmt),
            Statement::Expression(expr) => self.eval_expression_stmt(expr),
        }
    }

    fn eval_let_stmt(&mut self, stmt: Let<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();
        diags.extend(self.eval_expression_stmt(stmt.value));

        let ident = stmt.name.name;
        self.env.borrow_mut().store.insert(ident);
        diags
    }

    fn eval_return_stmt(&mut self, stmt: Return<'source>) -> Vec<SpannedDiagnostic> {
        match stmt.value {
            Some(val) => match val {
                Ok(val) => self.eval_expression_stmt(val),
                Err(err) => vec![err.into()],
            },
            None => vec![],
        }
    }

    fn eval_block_stmt(&mut self, stmt: Block<'source>) -> Vec<SpannedDiagnostic> {
        let (inner_env, diags) = Self::eval_program(stmt.statements, Some(Rc::clone(&self.env)));
        self.env.borrow_mut().inner.push(inner_env);
        // TODO: handle return
        // statemnts/expressions after return not allowed
        // probably need to stop using `eval_program`
        diags
    }

    fn eval_expression_stmt(&mut self, expr: Expression) -> Vec<SpannedDiagnostic> {
        match expr {
            Expression::Identifier(_) => vec![],
            Expression::Primative(_) => vec![],
            Expression::StringLiteral(_) => vec![],
            Expression::Prefix(_) => todo!(),
            Expression::Infix(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Hash(_) => todo!(),
            Expression::Index(_) => todo!(),
        }
    }

    fn eval_identifier(&mut self, expr: Identifier) -> Vec<SpannedDiagnostic> {
        todo!()
    }

    fn eval_primative(&mut self, expr: Primative) -> Vec<SpannedDiagnostic> {
        todo!()
    }

    fn eval_string_literal(&mut self, expr: Primative) -> Vec<SpannedDiagnostic> {
        todo!()
    }
}
