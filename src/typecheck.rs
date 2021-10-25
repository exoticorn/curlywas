use std::collections::HashMap;

use crate::ast;
use ast::Type::*;

#[derive(Debug)]
pub struct Error {
    pub position: ast::Position,
    pub message: String,
}

type Result<T> = std::result::Result<T, Error>;

type Vars<'a> = HashMap<&'a str, ast::Type>;

pub fn tc_script(script: &mut ast::Script) -> Result<()> {
    let mut context = Context {
        global_vars: HashMap::new(),
        local_vars: HashMap::new(),
    };

    for import in &script.imports {
        match import.type_ {
            ast::ImportType::Variable { name, type_ } => {
                if context.global_vars.contains_key(name) {
                    return Err(Error {
                        position: import.position,
                        message: "Duplicate global variable".into(),
                    });
                }
                context.global_vars.insert(name, type_);
            }
            // ast::ImportType::Function { .. } => todo!(),
            ast::ImportType::Memory( .. ) => ()
        }
    }

    for v in &script.global_vars {
        if context.global_vars.contains_key(v.name) {
            return Err(Error {
                position: v.position,
                message: "Duplicate global variable".into(),
            });
        }
        context.global_vars.insert(v.name, v.type_);
    }

    for f in &mut script.functions {
        context.local_vars.clear();
        for (name, type_) in &f.params {
            if context.local_vars.contains_key(name) || context.global_vars.contains_key(name) {
                return Err(Error {
                    position: f.position,
                    message: format!("Variable already defined '{}'", name),
                });
            }
            context.local_vars.insert(name, *type_);
        }

        tc_block(&mut context, &mut f.body)?;
    }

    Ok(())
}

struct Context<'a> {
    global_vars: Vars<'a>,
    local_vars: Vars<'a>,
}

fn tc_block<'a>(context: &mut Context<'a>, block: &mut ast::Block<'a>) -> Result<()> {
    for stmt in &mut block.statements {
        match *stmt {
            ast::Statement::Expression(ref mut expr) => tc_expression(context, expr)?,
            ast::Statement::LocalVariable(ref mut lv) => {
                if let Some(ref mut value) = lv.value {
                    tc_expression(context, value)?;
                    if lv.type_.is_none() {
                        lv.type_ = value.type_;
                    } else if lv.type_ != value.type_ {
                        return Err(Error {
                            position: lv.position,
                            message: "Mismatched types".into(),
                        });
                    }
                }
                if let Some(type_) = lv.type_ {
                    if context.local_vars.contains_key(lv.name)
                        || context.global_vars.contains_key(lv.name)
                    {
                        return Err(Error {
                            position: lv.position,
                            message: format!("Variable '{}' already defined", lv.name),
                        });
                    }
                    context.local_vars.insert(lv.name, type_);
                } else {
                    return Err(Error {
                        position: lv.position,
                        message: "Missing type".into(),
                    });
                }
            }
            ast::Statement::Poke {
                position,
                ref mut mem_location,
                ref mut value,
            } => {
                tc_mem_location(context, mem_location)?;
                tc_expression(context, value)?;
                if value.type_ != Some(I32) {
                    return Err(Error {
                        position,
                        message: "Type mismatch".into(),
                    });
                }
            }
        }
    }
    if let Some(ref mut expr) = block.final_expression {
        tc_expression(context, expr)?;
    }
    Ok(())
}

fn tc_expression<'a>(context: &mut Context<'a>, expr: &mut ast::Expression<'a>) -> Result<()> {
    expr.type_ = match expr.expr {
        ast::Expr::I32Const(_) => Some(ast::Type::I32),
        ast::Expr::BinOp {
            position,
            op,
            ref mut left,
            ref mut right,
        } => {
            tc_expression(context, left)?;
            tc_expression(context, right)?;
            if left.type_.is_none() || left.type_ != right.type_ {
                return Err(Error {
                    position,
                    message: "Type mismatch".into(),
                });
            }
            use ast::BinOp::*;
            match op {
                Add | Sub | Mul | Div => left.type_,
                Rem | And | Or | Xor => {
                    if left.type_ != Some(I32) {
                        return Err(Error {
                            position,
                            message: "Unsupported type".into(),
                        });
                    } else {
                        left.type_
                    }
                }
                Eq | Ne | Lt | Le | Gt | Ge => Some(I32),
            }
        }
        ast::Expr::Variable { position, name } => {
            if let Some(&type_) = context
                .global_vars
                .get(name)
                .or_else(|| context.local_vars.get(name))
            {
                Some(type_)
            } else {
                return Err(Error {
                    position,
                    message: "Variable not found".into(),
                });
            }
        }
        ast::Expr::LocalTee {
            position,
            name,
            ref mut value,
        } => {
            tc_expression(context, value)?;
            if let Some(&type_) = context.local_vars.get(name) {
                if value.type_ != Some(type_) {
                    return Err(Error {
                        position,
                        message: "Type mismatch".into(),
                    });
                }
                Some(type_)
            } else {
                return Err(Error {
                    position,
                    message: format!("No local variable '{}' found", name),
                });
            }
        }
        ast::Expr::Loop {
            position: _,
            label: _,
            ref mut block,
        } => {
            tc_block(context, block)?;
            block.final_expression.as_ref().and_then(|e| e.type_)
        }
        ast::Expr::BranchIf {
            position,
            ref mut condition,
            label: _,
        } => {
            tc_expression(context, condition)?;
            if condition.type_ != Some(I32) {
                return Err(Error {
                    position,
                    message: "Condition has to be i32".into(),
                });
            }
            None
        }
    };
    Ok(())
}

fn tc_mem_location<'a>(
    context: &mut Context<'a>,
    mem_location: &mut ast::MemoryLocation<'a>,
) -> Result<()> {
    tc_expression(context, &mut mem_location.left)?;
    tc_expression(context, &mut mem_location.right)?;
    if mem_location.left.type_ != Some(I32) || mem_location.right.type_ != Some(I32) {
        return Err(Error {
            position: mem_location.position,
            message: "Type mismatch".into(),
        });
    }
    Ok(())
}
