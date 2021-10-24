use crate::ast;

pub fn fold_script(script: &mut ast::Script) {
    for func in &mut script.functions {
        fold_block(&mut func.body);
    }
}

fn fold_block(block: &mut ast::Block) {
    for stmt in &mut block.statements {
        match stmt {
            ast::Statement::LocalVariable(lv) => {
                if let Some(ref mut expr) = lv.value {
                    fold_expr(expr);
                }
            }
            ast::Statement::Expression(expr) => fold_expr(expr),
            ast::Statement::Poke {
                mem_location,
                value,
                ..
            } => {
                fold_mem_location(mem_location);
                fold_expr(value);
            }
        }
    }
    if let Some(ref mut expr) = block.final_expression {
        fold_expr(expr);
    }
}

fn fold_mem_location(mem_location: &mut ast::MemoryLocation) {
    fold_expr(&mut mem_location.left);
    fold_expr(&mut mem_location.right);
}

fn fold_expr(expr: &mut ast::Expression) {
    use ast::BinOp::*;
    match expr.expr {
        ast::Expr::BinOp {
            ref mut left,
            op,
            ref mut right,
            ..
        } => {
            fold_expr(left);
            fold_expr(right);
            dbg!(&left.expr, &right.expr);
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
                        Rem => {
                            if let Some(result) = left.checked_rem(right) {
                                result
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
                        Le => (left <= right) as i32,
                        Gt => (left > right) as i32,
                        Ge => (left >= right) as i32,
                    };
                    expr.expr = ast::Expr::I32Const(result);
                }
                _ => (),
            }
        }
        ast::Expr::I32Const(_) | ast::Expr::Variable { .. } => (),
        ast::Expr::LocalTee { ref mut value, .. } => fold_expr(value),
        ast::Expr::Loop { ref mut block, .. } => fold_block(block),
        ast::Expr::BranchIf {
            ref mut condition, ..
        } => fold_expr(condition),
    }
}
