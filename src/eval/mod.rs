#![allow(unused)]

mod env;
mod object;
#[cfg(test)]
mod test;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

use env::Env;
pub use object::Object;

use crate::ast::{Node, NodeToken, *};
use crate::diagnostics::{MonkeyError, MonkeyWarning, SpannedDiagnostic};
use crate::types::Spanned;

pub struct Eval<'source> {
    env: Env<'source>,
    env_id: usize,
}

impl<'source> Eval<'source> {
    pub fn new(env: Env<'source>, env_id: usize) -> Self {
        Self { env, env_id }
    }

    fn new_env_id(&mut self) -> usize {
        self.env_id += 1;
        self.env_id
    }

    pub fn eval_program(nodes: Vec<Node<'source>>) -> (Env<'source>, Vec<SpannedDiagnostic>) {
        let env_id = 0;
        let mut eval = Self::new(Env::new(env_id), env_id);

        let mut diagnostics = Vec::new();
        for node in nodes {
            match node {
                Node::Statement(stmt) => {
                    let (_, diags) = eval.eval_statements(&stmt);
                    diagnostics.extend(diags);
                }
                Node::Error(err) => diagnostics.push(err.into()),
            }
        }
        (eval.env, diagnostics)
    }

    fn eval_statements(&mut self, stmt: &Statement<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        match stmt {
            Statement::Let(stmt) => (Object::Unknown, self.eval_let_stmt(stmt)),
            Statement::Return(stmt) => self.eval_return_stmt(stmt),
            Statement::Block(stmt) => unreachable!("really don't think this is necessary"),
            Statement::Expression(expr) => self.eval_expression_stmt(expr, true),
        }
    }

    fn eval_let_stmt(&mut self, stmt: &Let<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();
        let (obj, expr_diags) = self.eval_expression_stmt(&stmt.value, false);
        diags.extend(expr_diags);

        let obj = Rc::new(stmt.name.token().map(obj));
        let ident = stmt.name.name;
        self.env.insert_store(ident, &obj);

        let span_ident = Rc::new(stmt.name.token().map(ident));
        self.env.insert_ref(&span_ident);
        diags
    }

    fn eval_return_stmt(&mut self, stmt: &Return<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        match &stmt.value {
            Some(val) => match val {
                Ok(val) => self.eval_expression_stmt(val, false),
                Err(err) => (Object::Unknown, vec![err.clone().into()]),
            },
            None => (Object::Nil, vec![]),
        }
    }

    fn eval_block_stmt(
        block: &Block<'source>,
        child_env: Env<'source>,
    ) -> (Object, Vec<SpannedDiagnostic>) {
        let env_id = child_env.env_id();
        let mut eval = Eval {
            env: child_env,
            env_id,
        };

        let stmt_count = block.statements.len();

        let mut obj = Object::Nil;
        let mut diags = Vec::new();

        for (idx, node) in block.statements.iter().enumerate() {
            match node {
                Node::Statement(stmt) => match stmt {
                    Statement::Let(stmt) => diags.extend(eval.eval_let_stmt(stmt)),
                    Statement::Return(stmt) => {
                        let (obj, ret_diags) = eval.eval_return_stmt(stmt);
                        diags.extend(ret_diags);
                        return (obj, diags);
                    }
                    Statement::Block(_) => unreachable!("i don't think this should happen?"),
                    Statement::Expression(expr) => {
                        let last_expr = idx == stmt_count - 1;
                        let (expr_obj, expr_diags) = eval.eval_expression_stmt(expr, !last_expr);
                        diags.extend(expr_diags);
                        obj = expr_obj;
                    }
                },
                Node::Error(err) => diags.push(err.clone().into()),
            }
        }
        (obj, diags)
    }

    fn eval_expression_stmt(
        &mut self,
        expr: &Expression<'source>,
        is_alone: bool,
    ) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();

        if is_alone {
            diags.push(expr.token().map(MonkeyWarning::UnusedExpression.into()));
        }

        let (obj, expr_diags) = match expr {
            Expression::Identifier(ident) => self.eval_identifier(ident),
            Expression::Primative(val) => {
                let obj = match val {
                    Primative::Int { .. } => Object::Int,
                    Primative::Bool { .. } => Object::Bool,
                    Primative::Nil { .. } => Object::Nil,
                };
                (obj, vec![])
            }
            Expression::StringLiteral(_) => (Object::String, vec![]),
            Expression::Prefix(expr) => self.eval_prefix(expr),
            Expression::Infix(expr) => self.eval_infix(expr),
            Expression::If(expr) => self.eval_if(expr),
            Expression::Function(expr) => self.eval_func(expr),
            Expression::Call(_) => (Object::Unknown, vec![]),
            Expression::Array(_) => todo!(),
            Expression::Hash(_) => todo!(),
            Expression::Index(_) => (Object::Unknown, vec![]),
        };

        diags.extend(expr_diags);

        (obj, diags)
    }

    fn eval_identifier(&mut self, expr: &Identifier<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();
        let ident = expr.name;
        let obj = match self.env.find_def(ident) {
            Some(span) => {
                let span_ident = Rc::new(expr.token().map(ident));
                self.env.insert_ref(&span_ident);
                **span
            }
            None => {
                diags.push(
                    expr.token()
                        .map(MonkeyError::UnknownIdentifier(expr.name.to_string()).into()),
                );
                Object::Unknown
            }
        };

        (obj, diags)
    }

    fn eval_prefix(&mut self, expr: &Prefix<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();

        let obj = match expr.operator {
            Operator::Minus => {
                let (right_obj, right_diag) = self.eval_expression_stmt(&expr.right, false);
                diags.extend(right_diag);
                if !matches!(right_obj, Object::Int | Object::Unknown) {
                    diags.push(
                        expr.right
                            .token()
                            .map(MonkeyError::BadPrefixType(right_obj.typename()))
                            .into(),
                    );
                    Object::Unknown
                } else {
                    Object::Int
                }
            }
            Operator::Bang => {
                let (right_obj, right_diag) = self.eval_expression_stmt(&expr.right, false);
                diags.extend(right_diag);
                Object::Bool
            }
            op => unreachable!("{op} not valid prefix operator"),
        };

        (obj, diags)
    }

    fn eval_infix(&mut self, expr: &Infix<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        use crate::ast::Operator as Op;
        let mut diags = Vec::new();

        let (left_obj, left_diags) = self.eval_expression_stmt(&expr.left, false);
        diags.extend(left_diags);
        let (right_obj, right_diags) = self.eval_expression_stmt(&expr.right, false);
        diags.extend(right_diags);

        let obj = match (left_obj, right_obj) {
            (Object::Int, Object::Int) => match expr.operator {
                Op::Plus | Op::Minus | Op::Mult | Op::Div => Object::Int,
                Op::Eq | Op::NotEq | Op::Lt | Op::Gt => Object::Bool,
                op => {
                    diags.push(
                        MonkeyError::new_unknown_op(expr.token(), &left_obj, &right_obj, op).into(),
                    );
                    Object::Unknown
                }
            },
            (Object::String, Object::String) => match expr.operator {
                Op::Plus => Object::String,
                Op::Eq | Op::NotEq => Object::Bool,
                op => {
                    diags.push(
                        MonkeyError::new_unknown_op(expr.token(), &left_obj, &right_obj, op).into(),
                    );
                    Object::Unknown
                }
            },
            (Object::Bool, Object::Bool) => match expr.operator {
                Op::Eq | Op::NotEq => Object::Bool,
                op => {
                    diags.push(
                        MonkeyError::new_unknown_op(expr.token(), &left_obj, &right_obj, op).into(),
                    );
                    Object::Unknown
                }
            },
            (Object::Unknown, Object::Unknown) => Object::Unknown,
            (_, _) => {
                diags.push(
                    MonkeyError::new_unknown_op(expr.token(), &left_obj, &right_obj, expr.operator)
                        .into(),
                );
                Object::Unknown
            }
        };

        (obj, diags)
    }

    fn eval_if(&mut self, expr: &If<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();

        if let Ok(condition) = &expr.condition {
            let (_, cond_diags) = self.eval_expression_stmt(condition, false);
            diags.extend(cond_diags);
        }

        if let Ok(consq_block) = &expr.consequence {
            let child_env = Env::new_child(self.env.clone(), self.new_env_id());
            let (_, consq_diags) = Self::eval_block_stmt(consq_block, child_env);
            diags.extend(consq_diags);
        }

        if let Ok(Some(alt_block)) = &expr.alternative {
            let child_env = Env::new_child(self.env.clone(), self.new_env_id());
            let (_, alt_diags) = Self::eval_block_stmt(alt_block, child_env);
            diags.extend(alt_diags);
        }

        (Object::Unknown, diags)
    }

    fn eval_func(&mut self, expr: &Function<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();
        let mut obj = Object::Unknown;

        match &expr.params {
            Ok(params) => {
                obj = Object::Function(params.len());

                let child_env = Env::new_child(self.env.clone(), self.new_env_id());
                for param in params {
                    match param {
                        Ok(Expression::Identifier(ident_expr)) => {
                            let ident = ident_expr.name;
                            let span_ident = Rc::new(ident_expr.token().map(Object::Unknown));
                            child_env.insert_store(ident, &span_ident);
                        }
                        Ok(_) => unreachable!(),
                        Err(err) => diags.push(err.clone().into()),
                    }
                }

                match &expr.body {
                    Ok(body) => {
                        let (_, body_diags) = Self::eval_block_stmt(body, child_env);
                        diags.extend(body_diags);
                    }
                    Err(err) => diags.push(err.clone().into()),
                }
            }
            Err(err) => diags.push(err.clone().into()),
        }

        (obj, diags)
    }
}
