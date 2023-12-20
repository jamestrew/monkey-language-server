mod env;
mod object;
#[cfg(test)]
mod test;

use std::sync::Arc;

pub use env::Env;
pub use object::Object;

use crate::ast::{Node, NodeToken, *};
use crate::diagnostics::{MonkeyError, MonkeyWarning, SpannedDiagnostic};

pub struct Eval {
    env: Env,
    env_id: usize,
}

impl<'source> Eval {
    pub fn new(env: Env, env_id: usize) -> Self {
        Self { env, env_id }
    }

    fn new_env_id(&mut self) -> usize {
        self.env_id += 1;
        self.env_id
    }

    pub fn eval_program(nodes: Vec<Node<'source>>) -> (Env, Vec<SpannedDiagnostic>) {
        let env_id = 0;
        let env = Env::new(env_id);
        env.seed_builtin();
        let mut eval = Self::new(env, env_id);

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
            Statement::Block(_stmt) => unreachable!("really don't think this is necessary"),
            Statement::Expression(expr) => self.eval_expression_stmt(expr, true),
        }
    }

    fn eval_let_stmt(&mut self, stmt: &Let<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();
        let (obj, expr_diags) = self.eval_expression_stmt(&stmt.value, false);
        diags.extend(expr_diags);

        let obj = Arc::new(stmt.name.token().map(obj));
        let ident = stmt.name.name;
        self.env.insert_store(ident, &obj);

        let span_ident = Arc::new(stmt.name.token().map(ident.to_string()));
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

    fn eval_block_stmt(block: &Block<'source>, child_env: Env) -> (Object, Vec<SpannedDiagnostic>) {
        let env_id = child_env.env_id();
        let mut eval = Eval {
            env: child_env,
            env_id,
        };

        let stmt_count = block.statements.len();

        let mut obj = Object::Nil;
        let mut diags = Vec::new();

        let mut block_returned = false;
        for (idx, node) in block.statements.iter().enumerate() {
            match node {
                Node::Statement(stmt) => {
                    if block_returned {
                        diags.push(stmt.token().map(MonkeyWarning::UnreachableCode).into());
                        continue;
                    }

                    match stmt {
                        Statement::Let(stmt) => diags.extend(eval.eval_let_stmt(stmt)),
                        Statement::Return(stmt) => {
                            let (ret_obj, ret_diags) = eval.eval_return_stmt(stmt);
                            diags.extend(ret_diags);
                            obj = ret_obj;
                            block_returned = true;
                        }
                        Statement::Block(_) => unreachable!("i don't think this should happen?"),
                        Statement::Expression(expr) => {
                            let last_expr = idx == stmt_count - 1;
                            let (expr_obj, expr_diags) =
                                eval.eval_expression_stmt(expr, !last_expr);
                            diags.extend(expr_diags);
                            obj = expr_obj;
                        }
                    }
                }
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
            Expression::Call(expr) => self.eval_call(expr),
            Expression::Array(expr) => (Object::Array, self.eval_array(expr)),
            Expression::Hash(expr) => (Object::Hash, self.eval_hash(expr)),
            Expression::Index(expr) => (Object::Unknown, self.eval_index(expr)),
        };

        if (!matches!(expr, Expression::Call(_)) || !matches!(obj, Object::Nil)) && is_alone {
            diags.push(expr.token().map(MonkeyWarning::UnusedExpression.into()));
        }

        diags.extend(expr_diags);

        (obj, diags)
    }

    fn eval_identifier(&mut self, expr: &Identifier<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();
        let ident = expr.name.to_string();
        let obj = match self.env.find_def(&ident) {
            Some(span) => {
                let span_ident = Arc::new(expr.token().map(ident));
                self.env.insert_ref(&span_ident);
                (*span).clone().take()
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
                let (_, right_diag) = self.eval_expression_stmt(&expr.right, false);
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

        let obj = match (left_obj.clone(), right_obj.clone()) {
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
            (Object::Unknown, _) => Object::Unknown,
            (_, Object::Unknown) => Object::Unknown,
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

        match &expr.condition {
            Ok(condition) => {
                let (_, cond_diags) = self.eval_expression_stmt(condition, false);
                diags.extend(cond_diags);
            }
            Err(err) => diags.push(err.clone().into()),
        };

        match &expr.consequence {
            Ok(consq_block) => {
                let child_env = Env::new_child(self.env.clone(), self.new_env_id());
                let (_, consq_diags) = Self::eval_block_stmt(consq_block, child_env);
                diags.extend(consq_diags);
            }
            Err(err) => diags.push(err.clone().into()),
        };

        match &expr.alternative {
            Ok(Some(alt_block)) => {
                let child_env = Env::new_child(self.env.clone(), self.new_env_id());
                let (_, alt_diags) = Self::eval_block_stmt(alt_block, child_env);
                diags.extend(alt_diags);
            }
            Ok(_) => {}
            Err(err) => diags.push(err.clone().into()),
        }

        (Object::Unknown, diags)
    }

    fn eval_func(&mut self, expr: &Function<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();
        let mut obj = Object::Unknown;

        match &expr.params {
            Ok(params) => {
                let child_env = Env::new_child(self.env.clone(), self.new_env_id());
                for param in params {
                    match param {
                        Ok(Expression::Identifier(ident_expr)) => {
                            let ident = ident_expr.name;
                            let span_ident = Arc::new(ident_expr.token().map(Object::Unknown));
                            child_env.insert_store(ident, &span_ident);
                        }
                        Ok(_) => unreachable!(),
                        Err(err) => diags.push(err.clone().into()),
                    }
                }

                match &expr.body {
                    Ok(body) => {
                        let (body_obj, body_diags) = Self::eval_block_stmt(body, child_env);
                        obj = Object::Function(Some(params.len()), Box::new(body_obj));
                        diags.extend(body_diags);
                    }
                    Err(err) => diags.push(err.clone().into()),
                }
            }
            Err(err) => diags.push(err.clone().into()),
        }

        (obj, diags)
    }

    fn eval_call(&mut self, expr: &Call<'source>) -> (Object, Vec<SpannedDiagnostic>) {
        let mut diags = Vec::new();

        let (func_obj, func_diags) = self.eval_expression_stmt(&expr.func, false);
        diags.extend(func_diags);

        let arg_objs = match self.call_arg_objs(expr, &mut diags) {
            Some(objs) => objs,
            None => return (Object::Unknown, diags),
        };

        let arg_count;
        let ret_type;
        match func_obj {
            Object::Function(count, r_type) => {
                arg_count = count;
                ret_type = *r_type;
            }
            Object::Builtin(func) => {
                let (obj, diag) = func.eval(expr, &arg_objs);
                if let Some(diag) = diag {
                    diags.push(diag);
                }
                return (obj, diags);
            }
            Object::Unknown => {
                return (Object::Unknown, diags);
            }
            _ => {
                diags.push(
                    expr.func
                        .token()
                        .map(MonkeyError::BadFunctionCall(func_obj.typename()).into()),
                );
                return (Object::Unknown, diags);
            }
        }

        if let Some(count) = arg_count {
            if arg_objs.len() != count {
                diags.push(
                    expr.token()
                        .map(MonkeyError::MismatchArgs(count, arg_objs.len()).into()),
                );
                return (Object::Unknown, diags);
            }
        }

        if let Expression::Identifier(ident) = &*expr.func {
            if let Some(func) = self.env.find_def(ident.name) {
                if let Some(func_obj) = (*func).clone().take().function_return() {
                    return (func_obj, diags);
                }
            }
        }

        (ret_type, diags)
    }

    fn call_arg_objs(
        &mut self,
        expr: &Call<'source>,
        diags: &mut Vec<SpannedDiagnostic>,
    ) -> Option<Vec<Option<Object>>> {
        let args = match &expr.args {
            Ok(args) => args,
            Err(err) => {
                diags.push(err.clone().into());
                return None;
            }
        };

        Some(
            args.iter()
                .map(|arg| match arg {
                    Ok(arg_expr) => {
                        let (obj, arg_diags) = self.eval_expression_stmt(arg_expr, false);
                        diags.extend(arg_diags);
                        Some(obj)
                    }
                    Err(err) => {
                        diags.push(err.clone().into());
                        None
                    }
                })
                .collect::<Vec<_>>(),
        )
    }

    fn eval_array(&mut self, expr: &Array<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();

        match &expr.elems {
            Ok(elems) => elems.iter().for_each(|elem| match elem {
                Ok(elem) => {
                    let (_, elem_diags) = self.eval_expression_stmt(elem, false);
                    diags.extend(elem_diags);
                }
                Err(err) => diags.push(err.clone().into()),
            }),
            Err(err) => diags.push(err.clone().into()),
        };

        diags
    }

    fn eval_hash(&mut self, expr: &Hash<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();

        match &expr.kv_pairs {
            Ok(pairs) => pairs.iter().for_each(|res| match res {
                Ok((key, value)) => {
                    let (key_obj, key_diags) = self.eval_expression_stmt(key, false);
                    if !key_obj.is_hashable() {
                        diags.push(
                            key.token()
                                .map(MonkeyError::Unhashable(key_obj.typename()).into()),
                        );
                    }
                    diags.extend(key_diags);
                    let (_, value_diags) = self.eval_expression_stmt(value, false);
                    diags.extend(value_diags);
                }
                Err(err) => diags.push(err.clone().into()),
            }),
            Err(err) => diags.push(err.clone().into()),
        };

        diags
    }

    fn eval_index(&mut self, expr: &Index<'source>) -> Vec<SpannedDiagnostic> {
        let mut diags = Vec::new();

        let (main_obj, obj_diags) = self.eval_expression_stmt(&expr.object, false);
        diags.extend(obj_diags);

        match &*expr.index {
            Ok(index) => {
                let (index_obj, index_diags) = self.eval_expression_stmt(index, false);
                diags.extend(index_diags);
                match main_obj {
                    Object::String | Object::Array => {
                        if !matches!(index_obj, Object::Int) {
                            diags.push(
                                index.token().map(
                                    MonkeyError::ExpectedIntIndex(
                                        main_obj.typename(),
                                        index_obj.typename(),
                                    )
                                    .into(),
                                ),
                            );
                        }
                    }
                    Object::Hash => {
                        if !index_obj.is_hashable() {
                            diags.push(
                                index
                                    .token()
                                    .map(MonkeyError::Unhashable(index_obj.typename()).into()),
                            );
                        }
                    }
                    obj => diags.push(
                        expr.object
                            .token()
                            .map(MonkeyError::BadIndex(obj.typename()).into()),
                    ),
                }
            }
            Err(err) => diags.push(err.clone().into()),
        }

        diags
    }
}
