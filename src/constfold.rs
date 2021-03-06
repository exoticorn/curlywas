use std::collections::HashMap;

use crate::{
    ast,
    parser::{Sources, Span},
    typecheck::{report_duplicate_definition, report_error},
};

type Result<T> = std::result::Result<T, ()>;

pub fn fold_script(script: &mut ast::Script, sources: &Sources) -> Result<()> {
    let mut context = Context {
        consts: HashMap::new(),
        sources,
    };
    fold_consts(&mut context, &mut script.consts)?;

    for var in &mut script.global_vars {
        fold_expr(&context, &mut var.value);
    }

    for func in &mut script.functions {
        fold_expr(&context, &mut func.body);
    }

    for data in &mut script.data {
        fold_expr(&context, &mut data.offset);
        for values in &mut data.data {
            match values {
                ast::DataValues::Array { values, .. } => {
                    for value in values {
                        fold_expr(&context, value);
                    }
                }
                ast::DataValues::String(_) | ast::DataValues::File { .. } => (),
            }
        }
    }

    Ok(())
}

struct Context<'a> {
    consts: HashMap<String, ast::Expr>,
    sources: &'a Sources,
}

fn fold_consts(context: &mut Context, consts: &mut [ast::GlobalConst]) -> Result<()> {
    let mut spans: HashMap<&str, Span> = HashMap::new();

    for cnst in consts.iter_mut() {
        if let Some(prev_span) = spans.insert(&cnst.name, cnst.span.clone()) {
            report_duplicate_definition(
                "Const already defined",
                &cnst.span,
                &prev_span,
                context.sources,
            )?;
        }
    }

    while context.consts.len() < consts.len() {
        let mut making_progress = false;
        for cnst in consts.iter_mut() {
            if !context.consts.contains_key(&cnst.name) {
                fold_expr(context, &mut cnst.value);
                if cnst.value.is_const() {
                    context
                        .consts
                        .insert(cnst.name.clone(), cnst.value.expr.clone());
                    making_progress = true;
                }
            }
        }

        if !making_progress {
            break;
        }
    }

    let mut result = Ok(());
    for cnst in consts {
        if !context.consts.contains_key(&cnst.name) {
            result = report_error(
                &format!("Failed to fold const '{}'", cnst.name),
                &cnst.span,
                context.sources,
            );
        }
    }

    result
}

fn fold_mem_location(context: &Context, mem_location: &mut ast::MemoryLocation) {
    fold_expr(context, &mut mem_location.left);
    fold_expr(context, &mut mem_location.right);
}

fn fold_expr(context: &Context, expr: &mut ast::Expression) {
    use ast::BinOp::*;
    match expr.expr {
        ast::Expr::Block {
            ref mut statements,
            ref mut final_expression,
        } => {
            for stmt in statements {
                fold_expr(context, stmt);
            }
            if let Some(ref mut expr) = final_expression {
                fold_expr(context, expr);
            }
        }
        ast::Expr::Let { ref mut value, .. } => {
            if let Some(ref mut expr) = value {
                fold_expr(context, expr);
            }
        }
        ast::Expr::Poke {
            ref mut mem_location,
            ref mut value,
            ..
        } => {
            fold_mem_location(context, mem_location);
            fold_expr(context, value);
        }
        ast::Expr::Peek(ref mut mem_location) => fold_mem_location(context, mem_location),
        ast::Expr::UnaryOp { op, ref mut value } => {
            fold_expr(context, value);
            let result = match (op, &value.expr) {
                (ast::UnaryOp::Negate, ast::Expr::I32Const(value)) => {
                    Some(ast::Expr::I32Const(-*value))
                }
                (ast::UnaryOp::Negate, ast::Expr::I64Const(value)) => {
                    Some(ast::Expr::I64Const(-*value))
                }
                (ast::UnaryOp::Negate, ast::Expr::F32Const(value)) => {
                    Some(ast::Expr::F32Const(-*value))
                }
                (ast::UnaryOp::Negate, ast::Expr::F64Const(value)) => {
                    Some(ast::Expr::F64Const(-*value))
                }
                (ast::UnaryOp::Negate, ast::Expr::Cast { value, type_ }) => {
                    if let ast::Expr::I32Const(v) = value.expr {
                        Some(ast::Expr::Cast {
                            value: Box::new(ast::Expression {
                                expr: ast::Expr::I32Const(-v),
                                span: value.span.clone(),
                                type_: value.type_,
                            }),
                            type_: *type_,
                        })
                    } else {
                        None
                    }
                }
                (ast::UnaryOp::Not, ast::Expr::I32Const(value)) => {
                    Some(ast::Expr::I32Const((*value == 0) as i32))
                }
                (ast::UnaryOp::Not, ast::Expr::I64Const(value)) => {
                    Some(ast::Expr::I32Const((*value == 0) as i32))
                }
                _ => None,
            };
            if let Some(result) = result {
                expr.expr = result;
            }
        }
        ast::Expr::BinOp {
            ref mut left,
            op,
            ref mut right,
            ..
        } => {
            fold_expr(context, left);
            fold_expr(context, right);
            match (&left.expr, &right.expr) {
                (&ast::Expr::I32Const(left), &ast::Expr::I32Const(right)) => {
                    let result = match op {
                        Add => left.wrapping_add(right),
                        Sub => left.wrapping_sub(right),
                        Mul => left.wrapping_mul(right),
                        Div => {
                            if let Some(result) = left.checked_div(right) {
                                result
                            } else {
                                return;
                            }
                        }
                        DivU => {
                            if let Some(result) = (left as u32).checked_div(right as u32) {
                                result as i32
                            } else {
                                return;
                            }
                        }
                        Rem => {
                            if let Some(result) = left.checked_rem(right) {
                                result
                            } else {
                                return;
                            }
                        }
                        RemU => {
                            if let Some(result) = (left as u32).checked_rem(right as u32) {
                                result as i32
                            } else {
                                return;
                            }
                        }
                        And => left & right,
                        Or => left | right,
                        Xor => left ^ right,
                        Eq => (left == right) as i32,
                        Ne => (left != right) as i32,
                        Lt => (left < right) as i32,
                        LtU => ((left as u32) < (right as u32)) as i32,
                        Le => (left <= right) as i32,
                        LeU => ((left as u32) <= (right as u32)) as i32,
                        Gt => (left > right) as i32,
                        GtU => ((left as u32) > (right as u32)) as i32,
                        Ge => (left >= right) as i32,
                        GeU => ((left as u32) >= (right as u32)) as i32,
                        Shl => left << right,
                        ShrU => ((left as u32) >> right) as i32,
                        ShrS => left >> right,
                    };
                    expr.expr = ast::Expr::I32Const(result);
                }
                (&ast::Expr::I64Const(left), &ast::Expr::I64Const(right)) => {
                    use ast::Expr::*;
                    expr.expr = match op {
                        Add => I64Const(left.wrapping_add(right)),
                        Sub => I64Const(left.wrapping_sub(right)),
                        Mul => I64Const(left.wrapping_mul(right)),
                        Div => {
                            if let Some(result) = left.checked_div(right) {
                                I64Const(result)
                            } else {
                                return;
                            }
                        }
                        DivU => {
                            if let Some(result) = (left as u64).checked_div(right as u64) {
                                I64Const(result as i64)
                            } else {
                                return;
                            }
                        }
                        Rem => {
                            if let Some(result) = left.checked_rem(right) {
                                I64Const(result)
                            } else {
                                return;
                            }
                        }
                        RemU => {
                            if let Some(result) = (left as u64).checked_rem(right as u64) {
                                I64Const(result as i64)
                            } else {
                                return;
                            }
                        }
                        And => I64Const(left & right),
                        Or => I64Const(left | right),
                        Xor => I64Const(left ^ right),
                        Eq => I32Const((left == right) as i32),
                        Ne => I32Const((left != right) as i32),
                        Lt => I32Const((left < right) as i32),
                        LtU => I32Const(((left as u64) < (right as u64)) as i32),
                        Le => I32Const((left <= right) as i32),
                        LeU => I32Const(((left as u64) <= (right as u64)) as i32),
                        Gt => I32Const((left > right) as i32),
                        GtU => I32Const(((left as u64) > (right as u64)) as i32),
                        Ge => I32Const((left >= right) as i32),
                        GeU => I32Const(((left as u64) >= (right as u64)) as i32),
                        Shl => I64Const(left << right),
                        ShrU => I64Const(((left as u64) >> right) as i64),
                        ShrS => I64Const(left >> right),
                    };
                }
                (&ast::Expr::F32Const(left), &ast::Expr::F32Const(right)) => {
                    use ast::Expr::*;
                    expr.expr = match op {
                        Add => F32Const(left + right),
                        Sub => F32Const(left - right),
                        Mul => F32Const(left * right),
                        Div => F32Const(left / right),
                        Rem | And | Or | Xor | Shl | ShrU | ShrS | DivU | RemU | LtU | LeU
                        | GtU | GeU => return,
                        Eq => I32Const((left == right) as i32),
                        Ne => I32Const((left != right) as i32),
                        Lt => I32Const((left < right) as i32),
                        Le => I32Const((left <= right) as i32),
                        Gt => I32Const((left > right) as i32),
                        Ge => I32Const((left >= right) as i32),
                    };
                }
                (&ast::Expr::F64Const(left), &ast::Expr::F64Const(right)) => {
                    use ast::Expr::*;
                    expr.expr = match op {
                        Add => F64Const(left + right),
                        Sub => F64Const(left - right),
                        Mul => F64Const(left * right),
                        Div => F64Const(left / right),
                        Rem | And | Or | Xor | Shl | ShrU | ShrS | DivU | RemU | LtU | LeU
                        | GtU | GeU => return,
                        Eq => I32Const((left == right) as i32),
                        Ne => I32Const((left != right) as i32),
                        Lt => I32Const((left < right) as i32),
                        Le => I32Const((left <= right) as i32),
                        Gt => I32Const((left > right) as i32),
                        Ge => I32Const((left >= right) as i32),
                    };
                }
                _ => (),
            }
        }
        ast::Expr::I32Const(_)
        | ast::Expr::I64Const(_)
        | ast::Expr::F32Const(_)
        | ast::Expr::F64Const(_) => (),
        ast::Expr::Variable { ref name, .. } => {
            if let Some(value) = context.consts.get(name) {
                expr.expr = value.clone();
            }
        }
        ast::Expr::Assign { ref mut value, .. } => fold_expr(context, value),
        ast::Expr::LocalTee { ref mut value, .. } => fold_expr(context, value),
        ast::Expr::Loop { ref mut block, .. } => fold_expr(context, block),
        ast::Expr::LabelBlock { ref mut block, .. } => fold_expr(context, block),
        ast::Expr::Branch(_) => (),
        ast::Expr::BranchIf {
            ref mut condition, ..
        } => fold_expr(context, condition),
        ast::Expr::Cast { ref mut value, .. } => fold_expr(context, value),
        ast::Expr::FuncCall {
            ref name,
            ref mut params,
            ..
        } => {
            for param in params.iter_mut() {
                fold_expr(context, param);
            }
            use ast::Expr::*;
            let params: Vec<_> = params.iter().map(|e| &e.expr).collect();
            expr.expr = match (name.as_str(), params.as_slice()) {
                ("sqrt", [F32Const(v)]) if *v >= 0.0 => F32Const(v.sqrt()),
                _ => return,
            };
        }
        ast::Expr::Select {
            ref mut condition,
            ref mut if_true,
            ref mut if_false,
            ..
        } => {
            fold_expr(context, condition);
            fold_expr(context, if_true);
            fold_expr(context, if_false);
        }
        ast::Expr::If {
            ref mut condition,
            ref mut if_true,
            ref mut if_false,
        } => {
            fold_expr(context, condition);
            fold_expr(context, if_true);
            if let Some(ref mut if_false) = if_false {
                fold_expr(context, if_false);
            }
        }
        ast::Expr::Return {
            value: Some(ref mut value),
        } => fold_expr(context, value),
        ast::Expr::Return { value: None } => (),
        ast::Expr::First {
            ref mut value,
            ref mut drop,
        } => {
            fold_expr(context, value);
            fold_expr(context, drop);
        }
        ast::Expr::Error => unreachable!(),
    }
}
