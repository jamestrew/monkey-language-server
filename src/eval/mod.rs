#![allow(unused)]

#[cfg(test)]
mod test;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

use crate::ast::*;
use crate::diagnostics::{MonkeyError, MonkeyWarning, SpannedDiagnostic};
use crate::types::Spanned;
use crate::Node;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Object {
    Int,
    Bool,
    String,
    Return,
    Function,
    Builtin,
    Array,
    Hash,
    Nil,
    Unknown,
}

impl Object {
    pub fn typename(&self) -> &'static str {
        match self {
            Object::Int => "int",
            Object::Bool => "bool",
            Object::String => "str",
            Object::Return => todo!(),
            Object::Function => todo!(),
            Object::Builtin => todo!(),
            Object::Array => "array",
            Object::Hash => "hash",
            Object::Nil => "nil",
            Object::Unknown => "unknown",
        }
    }
}

impl std::fmt::Debug for Spanned<Object> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Spanned({:?}, {})", **self, self.pos_rng_str())
    }
}

pub struct Env<'source>(Rc<RefCell<Environment<'source>>>);

impl<'source> Env<'source> {
    fn new() -> Self {
        Env(Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            parent: None,
            children: vec![],
        })))
    }

    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }

    fn new_child(parent: Env<'source>) -> Self {
        let child = Self::new();
        parent.add_child(child.clone());
        child
    }

    fn add_child(&self, child: Env<'source>) {
        let weak_parent = Rc::downgrade(&self.0);
        child.0.borrow_mut().parent = Some(weak_parent);
        self.0.borrow_mut().children.push(child);
    }

    fn insert_store(&self, ident: &'source str, obj: &Rc<Spanned<Object>>) {
        self.0.borrow_mut().store.insert(ident, Rc::clone(obj));
    }

    fn push_children(&self, env: Env<'source>) {
        self.0.borrow_mut().children.push(env);
    }

    fn find_def(&self, ident: &'source str) -> Option<Rc<Spanned<Object>>> {
        let env = self.0.borrow();
        match env.store.get(ident) {
            Some(obj) => Some(Rc::clone(obj)),
            None => match &self.0.borrow().parent {
                Some(weak_parent) => {
                    if let Some(parent) = weak_parent.upgrade() {
                        Env(parent).find_def(ident)
                    } else {
                        None
                    }
                }
                None => None,
            },
        }
    }
}

impl<'source> std::fmt::Debug for Env<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0.borrow())
    }
}

struct Environment<'source> {
    store: HashMap<&'source str, Rc<Spanned<Object>>>,
    parent: Option<Weak<RefCell<Environment<'source>>>>,
    children: Vec<Env<'source>>,
}

impl<'source> std::fmt::Debug for Environment<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut store = self.store.iter().collect::<Vec<_>>();
        store.sort_unstable();

        f.debug_struct("Environment")
            .field("store", &store)
            .field(
                "parent",
                &self
                    .parent
                    .as_ref()
                    .and_then(|weak| weak.upgrade())
                    .map(|parent| {
                        // Safely attempt to access the parent's data for debugging purposes
                        // You might need to adjust this based on how you want to display the parent
                        format!("{:?}", parent.borrow().store)
                    }),
            )
            .field("children", &self.children)
            .finish()
    }
}

type Store<'source> = HashMap<&'source str, Spanned<Object>>;

// #[derive(Default)]
// pub struct Environment<'source> {
//     store: Store<'source>,
//     outer: Option<Env<'source>>,
//     inner: Vec<Weak<RefCell<Environment<'source>>>>,
// }

// impl<'source> std::fmt::Debug for Environment<'source> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut store = self.store.iter().collect::<Vec<_>>();
//         store.sort_unstable();

//         let inner = self
//             .inner
//             .iter()
//             .map(|weak| {
//                 weak.upgrade().map_or("Dropped".to_string(), |rc_env| {
//                     format!("{:?}", rc_env.borrow())
//                 })
//             })
//             .collect::<Vec<_>>();

//         f.debug_struct("Environment")
//             .field("store", &store)
//             .field("outer", &self.outer)
//             .field("inner", &inner)
//             .finish()
//     }
// }

// pub struct Env<'source>(Rc<RefCell<Environment<'source>>>);

// impl<'source> Env<'source> {
//     pub fn new(outer: Option<Env<'source>>) -> Self {
//         let env = Environment {
//             outer,
//             ..Default::default()
//         };
//         Self(Rc::new(RefCell::new(env)))
//     }

//     fn clone(&self) -> Self {
//         Self(Rc::clone(&self.0))
//     }

//     fn insert_store(&self, ident: &'source str, obj: Spanned<Object>) {
//         self.0.borrow_mut().store.insert(ident, obj);
//     }

//     // fn push_inner_env(&self, env: Env<'source>) {
//     //     self.0.borrow_mut().inner.push(env);
//     // }

//     fn find_def(&self, ident: &'source str) -> Option<Spanned<Object>> {
//         let val = self.0.borrow().store.get(ident).cloned();

//         match val {
//             Some(t) => Some(t),
//             None => {
//                 let outer = &self.0.borrow().outer;
//                 match outer {
//                     Some(outer) => outer.find_def(ident),
//                     None => None,
//                 }
//             }
//         }
//     }
// }

// impl<'source> std::fmt::Debug for Env<'source> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:#?}", self.0.borrow())
//     }
// }

pub struct Eval<'source> {
    env: Env<'source>,
}

impl<'source> Eval<'source> {
    pub fn eval_program(nodes: Vec<Node<'source>>) -> (Env<'source>, Vec<SpannedDiagnostic>) {
        let mut eval = Eval { env: Env::new() };

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
        parent_env: Env<'source>,
    ) -> (Object, Vec<SpannedDiagnostic>) {
        let child_env = Env::new_child(parent_env);
        let mut eval = Eval { env: child_env };

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
            Expression::Function(_) => todo!(),
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
        let obj = match self.env.find_def(expr.name) {
            Some(span) => **span,
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
            let (_, consq_diags) = Self::eval_block_stmt(consq_block, self.env.clone());
            diags.extend(consq_diags);
        }

        if let Ok(Some(alt_block)) = &expr.alternative {
            let (_, alt_diags) = Self::eval_block_stmt(alt_block, self.env.clone());
            diags.extend(alt_diags);
        }

        (Object::Unknown, diags)
    }
}
