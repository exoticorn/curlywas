use ariadne::{Color, Label, Report, ReportKind, Source};
use std::collections::HashMap;

use crate::ast;
use crate::intrinsics::Intrinsics;
use crate::Span;
use ast::Type::*;

type Result<T> = std::result::Result<T, ()>;

struct Var {
    span: Span,
    type_: ast::Type,
    mutable: bool,
}
type Vars = HashMap<String, Var>;

pub fn tc_script(script: &mut ast::Script, source: &str) -> Result<()> {
    let mut context = Context {
        source,
        global_vars: HashMap::new(),
        functions: HashMap::new(),
        local_vars: HashMap::new(),
        block_stack: Vec::new(),
        return_type: None,
        intrinsics: Intrinsics::new(),
    };

    let mut result = Ok(());

    for import in &script.imports {
        match import.type_ {
            ast::ImportType::Variable {
                ref name,
                type_,
                mutable,
            } => {
                if let Some(Var { span, .. }) = context.global_vars.get(name) {
                    result = report_duplicate_definition(
                        "Global already defined",
                        &import.span,
                        span,
                        source,
                    );
                } else {
                    context.global_vars.insert(
                        name.clone(),
                        Var {
                            type_,
                            span: import.span.clone(),
                            mutable,
                        },
                    );
                }
            }
            ast::ImportType::Function {
                ref name,
                ref params,
                result: ref result_type,
            } => {
                if let Some(fnc) = context.functions.get(name) {
                    result = report_duplicate_definition(
                        "Function already defined",
                        &import.span,
                        &fnc.span,
                        source,
                    );
                } else {
                    context.functions.insert(
                        name.clone(),
                        FunctionType {
                            span: import.span.clone(),
                            params: params.clone(),
                            type_: *result_type,
                        },
                    );
                }
            }
            ast::ImportType::Memory(..) => (),
        }
    }

    for v in &mut script.global_vars {
        if let Some(Var { span, .. }) = context.global_vars.get(&v.name) {
            result = report_duplicate_definition("Global already defined", &v.span, span, source);
        } else {
            tc_const(&mut v.value, source)?;
            if v.type_ != v.value.type_ {
                if v.type_.is_some() {
                    result = type_mismatch(v.type_, &v.span, v.value.type_, &v.value.span, source);
                } else {
                    v.type_ = v.value.type_;
                }
            }
            context.global_vars.insert(
                v.name.clone(),
                Var {
                    type_: v.type_.unwrap(),
                    span: v.span.clone(),
                    mutable: v.mutable,
                },
            );
        }
    }

    for f in &script.functions {
        let params = f.params.iter().map(|(_, t)| *t).collect();
        if let Some(fnc) = context.functions.get(&f.name) {
            result =
                report_duplicate_definition("Function already defined", &f.span, &fnc.span, source);
        } else {
            context.functions.insert(
                f.name.clone(),
                FunctionType {
                    params,
                    type_: f.type_,
                    span: f.span.clone(),
                },
            );
        }
    }

    for f in &mut script.functions {
        context.local_vars.clear();
        for (name, type_) in &f.params {
            if let Some(Var { span, .. }) = context
                .local_vars
                .get(name)
                .or_else(|| context.global_vars.get(name))
            {
                result =
                    report_duplicate_definition("Variable already defined", &f.span, span, source);
            } else {
                context.local_vars.insert(
                    name.clone(),
                    Var {
                        type_: *type_,
                        span: f.span.clone(),
                        mutable: true,
                    },
                );
            }
        }
        context.return_type = f.type_;

        tc_expression(&mut context, &mut f.body)?;

        if f.body.type_ != f.type_ {
            result = type_mismatch(f.type_, &f.span, f.body.type_, &f.body.span, source);
        }
    }

    result
}

struct FunctionType {
    span: Span,
    params: Vec<ast::Type>,
    type_: Option<ast::Type>,
}

struct Context<'a> {
    source: &'a str,
    global_vars: Vars,
    functions: HashMap<String, FunctionType>,
    local_vars: Vars,
    block_stack: Vec<String>,
    return_type: Option<ast::Type>,
    intrinsics: Intrinsics,
}

fn report_duplicate_definition(
    msg: &str,
    span: &Span,
    prev_span: &Span,
    source: &str,
) -> Result<()> {
    Report::build(ReportKind::Error, (), span.start)
        .with_message(msg)
        .with_label(
            Label::new(span.clone())
                .with_message(msg)
                .with_color(Color::Red),
        )
        .with_label(
            Label::new(prev_span.clone())
                .with_message("Previous definition was here")
                .with_color(Color::Yellow),
        )
        .finish()
        .eprint(Source::from(source))
        .unwrap();
    Err(())
}

fn type_mismatch(
    type1: Option<ast::Type>,
    span1: &Span,
    type2: Option<ast::Type>,
    span2: &Span,
    source: &str,
) -> Result<()> {
    Report::build(ReportKind::Error, (), span2.start)
        .with_message("Type mismatch")
        .with_label(
            Label::new(span1.clone())
                .with_message(format!(
                    "Expected type {:?}...",
                    type1
                        .map(|t| format!("{:?}", t))
                        .unwrap_or("void".to_string())
                ))
                .with_color(Color::Yellow),
        )
        .with_label(
            Label::new(span2.clone())
                .with_message(format!(
                    "...but found type {}",
                    type2
                        .map(|t| format!("{:?}", t))
                        .unwrap_or("void".to_string())
                ))
                .with_color(Color::Red),
        )
        .finish()
        .eprint(Source::from(source))
        .unwrap();
    Err(())
}

fn expected_type(span: &Span, source: &str) -> Result<()> {
    Report::build(ReportKind::Error, (), span.start)
        .with_message("Expected value but found expression of type void")
        .with_label(
            Label::new(span.clone())
                .with_message("Expected value but found expression of type void")
                .with_color(Color::Red),
        )
        .finish()
        .eprint(Source::from(source))
        .unwrap();
    Err(())
}

fn unknown_variable(span: &Span, source: &str) -> Result<()> {
    Report::build(ReportKind::Error, (), span.start)
        .with_message("Unknown variable")
        .with_label(
            Label::new(span.clone())
                .with_message("Unknown variable")
                .with_color(Color::Red),
        )
        .finish()
        .eprint(Source::from(source))
        .unwrap();
    Err(())
}

fn immutable_assign(span: &Span, source: &str) -> Result<()> {
    Report::build(ReportKind::Error, (), span.start)
        .with_message("Trying to assign to immutable variable")
        .with_label(
            Label::new(span.clone())
                .with_message("Trying to assign to immutable variable")
                .with_color(Color::Red),
        )
        .finish()
        .eprint(Source::from(source))
        .unwrap();
    Err(())
}

fn missing_label(span: &Span, source: &str) -> Result<()> {
    Report::build(ReportKind::Error, (), span.start)
        .with_message("Label not found")
        .with_label(
            Label::new(span.clone())
                .with_message("Label not found")
                .with_color(Color::Red),
        )
        .finish()
        .eprint(Source::from(source))
        .unwrap();
    return Err(());
}

fn tc_expression(context: &mut Context, expr: &mut ast::Expression) -> Result<()> {
    expr.type_ = match expr.expr {
        ast::Expr::Block {
            ref mut statements,
            ref mut final_expression,
        } => {
            for stmt in statements {
                tc_expression(context, stmt)?;
            }
            if let Some(final_expression) = final_expression {
                tc_expression(context, final_expression)?;
                final_expression.type_
            } else {
                None
            }
        }
        ast::Expr::Let {
            ref mut value,
            ref mut type_,
            ref name,
            ..
        } => {
            if let Some(ref mut value) = value {
                tc_expression(context, value)?;
                if let Some(type_) = type_ {
                    if Some(*type_) != value.type_ {
                        return type_mismatch(
                            Some(*type_),
                            &expr.span,
                            value.type_,
                            &value.span,
                            context.source,
                        );
                    }
                } else if value.type_.is_none() {
                    return expected_type(&value.span, context.source);
                } else {
                    *type_ = value.type_;
                }
            }
            if let Some(type_) = type_ {
                if let Some(Var { span, .. }) = context
                    .local_vars
                    .get(name)
                    .or_else(|| context.global_vars.get(name))
                {
                    return report_duplicate_definition(
                        "Variable already defined",
                        &expr.span,
                        span,
                        context.source,
                    );
                }
                context.local_vars.insert(
                    name.clone(),
                    Var {
                        type_: *type_,
                        span: expr.span.clone(),
                        mutable: true,
                    },
                );
            } else {
                Report::build(ReportKind::Error, (), expr.span.start)
                    .with_message("Type missing")
                    .with_label(
                        Label::new(expr.span.clone())
                            .with_message("Type missing")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(context.source))
                    .unwrap();
                return Err(());
            }
            None
        }
        ast::Expr::Peek(ref mut mem_location) => {
            tc_mem_location(context, mem_location)?;
            Some(I32)
        }
        ast::Expr::Poke {
            ref mut mem_location,
            ref mut value,
        } => {
            tc_mem_location(context, mem_location)?;
            tc_expression(context, value)?;
            if value.type_ != Some(I32) {
                return type_mismatch(
                    Some(I32),
                    &expr.span,
                    value.type_,
                    &value.span,
                    context.source,
                );
            }
            None
        }
        ast::Expr::I32Const(_) => Some(ast::Type::I32),
        ast::Expr::I64Const(_) => Some(ast::Type::I64),
        ast::Expr::F32Const(_) => Some(ast::Type::F32),
        ast::Expr::F64Const(_) => Some(ast::Type::F64),
        ast::Expr::UnaryOp { op, ref mut value } => {
            tc_expression(context, value)?;
            if value.type_.is_none() {
                return expected_type(&value.span, context.source);
            }
            use ast::Type::*;
            use ast::UnaryOp::*;
            Some(match (value.type_.unwrap(), op) {
                (t, Negate) => t,
                (I32 | I64, Not) => I32,
                (_, Not) => {
                    return type_mismatch(
                        Some(I32),
                        &expr.span,
                        value.type_,
                        &value.span,
                        context.source,
                    )
                }
            })
        }
        ast::Expr::BinOp {
            op,
            ref mut left,
            ref mut right,
        } => {
            tc_expression(context, left)?;
            tc_expression(context, right)?;
            if let Some(type_) = left.type_ {
                if left.type_ != right.type_ {
                    return type_mismatch(
                        Some(type_),
                        &left.span,
                        right.type_,
                        &right.span,
                        context.source,
                    );
                }
            } else {
                return expected_type(&left.span, context.source);
            }
            use ast::BinOp::*;
            match op {
                Add | Sub | Mul | Div => left.type_,
                Rem | And | Or | Xor | Shl | ShrU | ShrS | DivU | RemU => {
                    if left.type_ != Some(I32) && left.type_ != Some(I64) {
                        return type_mismatch(
                            Some(I32),
                            &left.span,
                            left.type_,
                            &left.span,
                            context.source,
                        );
                    } else {
                        left.type_
                    }
                }
                Eq | Ne | Lt | Le | Gt | Ge => Some(I32),
                LtU | LeU | GtU | GeU => {
                    if left.type_ != Some(I32) && left.type_ != Some(I64) {
                        return type_mismatch(
                            Some(I32),
                            &left.span,
                            left.type_,
                            &left.span,
                            context.source,
                        );
                    } else {
                        Some(I32)
                    }
                }
            }
        }
        ast::Expr::Variable(ref name) => {
            if let Some(&Var { type_, .. }) = context
                .global_vars
                .get(name)
                .or_else(|| context.local_vars.get(name))
            {
                Some(type_)
            } else {
                return unknown_variable(&expr.span, context.source);
            }
        }
        ast::Expr::Assign {
            ref name,
            ref mut value,
        } => {
            tc_expression(context, value)?;
            if let Some(&Var {
                type_,
                ref span,
                mutable,
            }) = context
                .local_vars
                .get(name)
                .or_else(|| context.global_vars.get(name))
            {
                if value.type_ != Some(type_) {
                    return type_mismatch(
                        Some(type_),
                        span,
                        value.type_,
                        &value.span,
                        context.source,
                    );
                }
                if !mutable {
                    return immutable_assign(&expr.span, context.source);
                }
            } else {
                return unknown_variable(&expr.span, context.source);
            }
            None
        }
        ast::Expr::LocalTee {
            ref name,
            ref mut value,
        } => {
            tc_expression(context, value)?;
            if let Some(&Var {
                type_,
                ref span,
                mutable,
            }) = context.local_vars.get(name)
            {
                if value.type_ != Some(type_) {
                    return type_mismatch(
                        Some(type_),
                        span,
                        value.type_,
                        &value.span,
                        context.source,
                    );
                }
                if !mutable {
                    return immutable_assign(&expr.span, context.source);
                }
                Some(type_)
            } else {
                return unknown_variable(&expr.span, context.source);
            }
        }
        ast::Expr::Loop {
            ref label,
            ref mut block,
        } => {
            context.block_stack.push(label.clone());
            tc_expression(context, block)?;
            context.block_stack.pop();
            block.type_
        }
        ast::Expr::Branch(ref label) => {
            if !context.block_stack.contains(label) {
                return missing_label(&expr.span, context.source);
            }
            None
        }
        ast::Expr::BranchIf {
            ref mut condition,
            ref label,
        } => {
            tc_expression(context, condition)?;
            if condition.type_ != Some(I32) {
                return type_mismatch(
                    Some(I32),
                    &expr.span,
                    condition.type_,
                    &condition.span,
                    context.source,
                );
            }
            if !context.block_stack.contains(label) {
                return missing_label(&expr.span, context.source);
            }
            None
        }
        ast::Expr::Cast {
            ref mut value,
            type_,
        } => {
            tc_expression(context, value)?;
            if value.type_.is_none() {
                return expected_type(&expr.span, context.source);
            }
            Some(type_)
        }
        ast::Expr::FuncCall {
            ref name,
            ref mut params,
        } => {
            for param in params.iter_mut() {
                tc_expression(context, param)?;
                if param.type_.is_none() {
                    return expected_type(&param.span, context.source);
                }
            }
            if let Some(type_map) = context
                .functions
                .get(name)
                .map(|fnc| HashMap::from_iter([(fnc.params.clone(), fnc.type_)]))
                .or_else(|| context.intrinsics.find_types(name))
            {
                if let Some(rtype) =
                    type_map.get(&params.iter().map(|p| p.type_.unwrap()).collect::<Vec<_>>())
                {
                    *rtype
                } else {
                    let mut report = Report::build(ReportKind::Error, (), expr.span.start)
                        .with_message("No matching function found");
                    for (params, rtype) in type_map {
                        let param_str: Vec<_> = params.into_iter().map(|t| t.to_string()).collect();
                        let msg = format!(
                            "Found {}({}){}",
                            name,
                            param_str.join(", "),
                            if let Some(rtype) = rtype {
                                format!(" -> {}", rtype)
                            } else {
                                String::new()
                            }
                        );
                        report = report.with_label(Label::new(expr.span.clone()).with_message(msg));
                    }
                    report
                        .finish()
                        .eprint(Source::from(context.source))
                        .unwrap();
                    return Err(());
                }
            } else {
                Report::build(ReportKind::Error, (), expr.span.start)
                    .with_message(format!("Unknown function {}", name))
                    .with_label(
                        Label::new(expr.span.clone())
                            .with_message(format!("Unknown function {}", name))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(context.source))
                    .unwrap();
                return Err(());
            }
        }
        ast::Expr::Select {
            ref mut condition,
            ref mut if_true,
            ref mut if_false,
        } => {
            tc_expression(context, condition)?;
            tc_expression(context, if_true)?;
            tc_expression(context, if_false)?;
            if condition.type_ != Some(ast::Type::I32) {
                return type_mismatch(
                    Some(I32),
                    &condition.span,
                    condition.type_,
                    &condition.span,
                    context.source,
                );
            }
            if if_true.type_.is_some() {
                if if_true.type_ != if_false.type_ {
                    return type_mismatch(
                        if_true.type_,
                        &if_true.span,
                        if_false.type_,
                        &if_false.span,
                        context.source,
                    );
                }
            } else {
                return expected_type(&if_true.span, context.source);
            }
            if_true.type_
        }
        ast::Expr::If {
            ref mut condition,
            ref mut if_true,
            ref mut if_false,
        } => {
            tc_expression(context, condition)?;
            tc_expression(context, if_true)?;
            if let Some(ref mut if_false) = if_false {
                tc_expression(context, if_false)?;
                if if_true.type_ != if_false.type_ {
                    return type_mismatch(
                        if_true.type_,
                        &if_true.span,
                        if_false.type_,
                        &if_false.span,
                        context.source,
                    );
                } else {
                    if_true.type_
                }
            } else {
                None
            }
        }
        ast::Expr::Return { ref mut value } => {
            if let Some(ref mut value) = value {
                tc_expression(context, value)?;
                if value.type_ != context.return_type {
                    return type_mismatch(
                        context.return_type,
                        &expr.span,
                        value.type_,
                        &value.span,
                        context.source,
                    );
                }
            }
            None
        }
        ast::Expr::First {
            ref mut value,
            ref mut drop,
        } => {
            tc_expression(context, value)?;
            tc_expression(context, drop)?;
            value.type_
        }
        ast::Expr::Error => unreachable!(),
    };
    Ok(())
}

fn tc_mem_location<'a>(
    context: &mut Context<'a>,
    mem_location: &mut ast::MemoryLocation,
) -> Result<()> {
    tc_expression(context, &mut mem_location.left)?;
    tc_const(&mut mem_location.right, context.source)?;
    if mem_location.left.type_ != Some(I32) {
        return type_mismatch(
            Some(I32),
            &mem_location.left.span,
            mem_location.left.type_,
            &mem_location.left.span,
            context.source,
        );
    }
    if mem_location.right.type_ != Some(I32) {
        return type_mismatch(
            Some(I32),
            &mem_location.right.span,
            mem_location.right.type_,
            &mem_location.right.span,
            context.source,
        );
    }
    Ok(())
}

fn tc_const(expr: &mut ast::Expression, source: &str) -> Result<()> {
    use ast::Expr::*;
    expr.type_ = Some(match expr.expr {
        I32Const(_) => I32,
        I64Const(_) => I64,
        F32Const(_) => F32,
        F64Const(_) => F64,
        _ => {
            Report::build(ReportKind::Error, (), expr.span.start)
                .with_message("Expected constant value")
                .with_label(
                    Label::new(expr.span.clone())
                        .with_message("Expected constant value")
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(Source::from(source))
                .unwrap();
            return Err(());
        }
    });
    Ok(())
}
