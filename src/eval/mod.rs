#![allow(unused)]

#[cfg(test)]
mod test;

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::ast::*;
use crate::diagnostics::{MonkeyError, MonkeyWarning, SpannedDiagnostic};
use crate::types::Spanned;
use crate::Node;

#[derive(Debug)]
pub enum Object {}

type Store<'source> = HashSet<Rc<Spanned<&'source str>>>;

#[derive(Default)]
pub struct Environment<'source> {
    store: Store<'source>,
    outer: Option<Env<'source>>,
    inner: Vec<Env<'source>>,
}

impl<'source> std::fmt::Debug for Environment<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut store = self.store.iter().collect::<Vec<_>>();
        store.sort_unstable();

        f.debug_struct("Environment")
            .field("store", &store)
            .field("outer", &self.outer)
            .field("inner", &self.inner)
            .finish()
    }
}

pub struct Env<'source>(Rc<RefCell<Environment<'source>>>);

impl<'source> Env<'source> {
    pub fn new_env(outer: Option<Env<'source>>) -> Self {
        let env = Environment {
            outer,
            ..Default::default()
        };
        Self(Rc::new(RefCell::new(env)))
    }

    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }

    fn insert_store(&self, item: Spanned<&'source str>) {
        self.0.borrow_mut().store.insert(Rc::new(item));
    }

    fn push_inner_env(&self, env: Env<'source>) {
        self.0.borrow_mut().inner.push(env);
    }

    fn find_def(&self, ident: &'source str) -> Option<Rc<Spanned<&'source str>>> {
        let val = self
            .0
            .borrow()
            .store
            .iter()
            .find(|&i| ***i == ident)
            .map(Rc::clone);

        match val {
            Some(t) => Some(t),
            None => {
                let outer = &self.0.borrow().outer;
                match outer {
                    Some(outer) => outer.find_def(ident),
                    None => None,
                }
            }
        }
    }
}

impl<'source> std::fmt::Debug for Env<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.borrow())
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
            env: Env::new_env(outer_env),
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
            Statement::Expression(expr) => self.eval_expression_stmt(expr, true),
        }
    }

    fn eval_let_stmt(&mut self, stmt: Let<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();
        diags.extend(self.eval_expression_stmt(stmt.value, false));

        let ident = stmt.name.token().map(stmt.name.name);
        self.env.insert_store(ident);
        diags
    }

    fn eval_return_stmt(&mut self, stmt: Return<'source>) -> Vec<SpannedDiagnostic> {
        match stmt.value {
            Some(val) => match val {
                Ok(val) => self.eval_expression_stmt(val, false),
                Err(err) => vec![err.into()],
            },
            None => vec![],
        }
    }

    fn eval_block_stmt(&mut self, stmt: Block<'source>) -> Vec<SpannedDiagnostic> {
        let (inner_env, diags) = Self::eval_program(stmt.statements, Some(self.env.clone()));
        self.env.push_inner_env(inner_env);
        // TODO: handle return
        // statemnts/expressions after return not allowed
        // probably need to stop using `eval_program`
        diags
    }

    fn eval_expression_stmt(
        &mut self,
        expr: Expression<'source>,
        is_alone: bool,
    ) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();

        if is_alone {
            diags.push(expr.token().map(MonkeyWarning::UnusedExpression.into()));
        }

        diags.extend(match expr {
            Expression::Identifier(ident) => self.eval_identifier(ident),
            Expression::Primative(_) | Expression::StringLiteral(_) => vec![],
            Expression::Prefix(expr) => self.eval_prefix(expr),
            Expression::Infix(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Hash(_) => todo!(),
            Expression::Index(_) => todo!(),
        });

        diags
    }

    fn eval_identifier(&mut self, expr: Identifier<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();
        if self.env.find_def(expr.name).is_none() {
            diags.push(
                expr.token()
                    .map(MonkeyError::UnknownIdentifier(expr.name.to_string()).into()),
            );
        }

        diags
    }

    fn eval_prefix(&mut self, expr: Prefix) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();

        match expr.operator {
            Operator::Minus => {
                if !matches!(*expr.right, Expression::Primative(Primative::Int { .. })) {
                    diags.push(
                        expr.token()
                            .map(MonkeyError::BadPrefixType("bool".into()).into()),
                    )
                }
            }
            Operator::Bang => todo!(),
            op => unreachable!("{op} not valid prefix operator"),
        }
        diags
    }
}
