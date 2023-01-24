use ariadne::{Color, Label, Report, ReportKind};
use std::collections::HashMap;

use crate::ast::{self, MemSize};
use crate::intrinsics::Intrinsics;
use crate::parser::{Sources, Span};
use ast::Type::*;

type Result<T> = std::result::Result<T, ()>;

struct Var {
    span: Span,
    type_: ast::Type,
    mutable: bool,
}
type Vars = HashMap<String, Var>;

pub fn tc_script(script: &mut ast::Script, sources: &Sources) -> Result<()> {
    let mut context = Context {
        sources,
        global_vars: HashMap::new(),
        functions: HashMap::new(),
        locals: ast::Locals::default(),
        local_vars: LocalVars::new(),
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
                        sources,
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
                        sources,
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
            result = report_duplicate_definition("Global already defined", &v.span, span, sources);
        } else {
            tc_const(&mut v.value, sources)?;
            if v.type_ != v.value.type_ {
                if v.type_.is_some() {
                    result = type_mismatch(v.type_, &v.span, v.value.type_, &v.value.span, sources);
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

    for c in &mut script.consts {
        tc_const(&mut c.value, sources)?;
        if c.value.type_ != c.type_ {
            if c.type_.is_some() {
                result = type_mismatch(c.type_, &c.span, c.value.type_, &c.value.span, sources);
            } else {
                c.type_ = c.value.type_;
            }
        }
    }

    for f in &script.functions {
        let params = f.params.iter().map(|(_, t)| *t).collect();
        if let Some(fnc) = context.functions.get(&f.name) {
            result = report_duplicate_definition(
                "Function already defined",
                &f.span,
                &fnc.span,
                sources,
            );
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
        context.local_vars.push_scope();
        for (name, type_) in &f.params {
            if let Some(span) = context
                .local_vars
                .get(name)
                .map(|id| &context.locals[id].span)
                .or_else(|| context.global_vars.get(name).map(|v| &v.span))
            {
                result =
                    report_duplicate_definition("Variable already defined", &f.span, span, sources);
            } else {
                context.local_vars.insert(
                    name.clone(),
                    context
                        .locals
                        .add_param(f.span.clone(), name.clone(), *type_),
                );
            }
        }
        context.return_type = f.type_;

        tc_expression(&mut context, &mut f.body)?;

        let mut local_mapping: Vec<(ast::Type, usize)> = context
            .locals
            .locals
            .iter()
            .enumerate()
            .filter(|(_, local)| local.index.is_some())
            .map(|(index, local)| (local.type_, index))
            .collect();
        local_mapping.sort_by_key(|&(t, _)| t);
        let locals_start = context.locals.params.len();
        for (id, (_, index)) in local_mapping.into_iter().enumerate() {
            context.locals.locals[index].index = Some((locals_start + id) as u32);
        }

        f.locals = std::mem::take(&mut context.locals);

        if f.body.type_ != f.type_ {
            result = type_mismatch(f.type_, &f.span, f.body.type_, &f.body.span, sources);
        }
    }

    let mut start_function: Option<&ast::Function> = None;
    for f in &script.functions {
        if f.start {
            if !f.params.is_empty() || f.type_.is_some() {
                Report::build(ReportKind::Error, f.span.0, f.span.1.start)
                    .with_message("Start function can't have params or a return value")
                    .with_label(
                        Label::new(f.span.clone())
                            .with_message("Start function can't have params or a return value")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(sources)
                    .unwrap();

                result = Err(());
            }
            if let Some(prev) = start_function {
                result = report_duplicate_definition(
                    "Start function already defined",
                    &f.span,
                    &prev.span,
                    sources,
                );
            } else {
                start_function = Some(f);
            }
        }
    }

    for data in &mut script.data {
        tc_const(&mut data.offset, sources)?;
        if data.offset.type_ != Some(I32) {
            result = type_mismatch(
                Some(I32),
                &data.offset.span,
                data.offset.type_,
                &data.offset.span,
                sources,
            );
        }
        for values in &mut data.data {
            match values {
                ast::DataValues::Array { type_, values } => {
                    let needed_type = match type_ {
                        ast::DataType::I8 | ast::DataType::I16 | ast::DataType::I32 => {
                            ast::Type::I32
                        }
                        ast::DataType::I64 => ast::Type::I64,
                        ast::DataType::F32 => ast::Type::F32,
                        ast::DataType::F64 => ast::Type::F64,
                    };
                    for value in values {
                        tc_const(value, sources)?;
                        if value.type_ != Some(needed_type) {
                            result = type_mismatch(
                                Some(needed_type),
                                &value.span,
                                value.type_,
                                &value.span,
                                sources,
                            );
                        }
                    }
                }
                ast::DataValues::String(_) | ast::DataValues::File { .. } => (),
            }
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
    sources: &'a Sources,
    global_vars: Vars,
    functions: HashMap<String, FunctionType>,
    locals: ast::Locals,
    local_vars: LocalVars,
    block_stack: Vec<String>,
    return_type: Option<ast::Type>,
    intrinsics: Intrinsics,
}

struct LocalVars(Vec<HashMap<String, u32>>);

impl LocalVars {
    fn new() -> LocalVars {
        LocalVars(Vec::new())
    }

    fn get(&self, name: &str) -> Option<u32> {
        self.0
            .iter()
            .rev()
            .filter_map(|scope| scope.get(name))
            .next()
            .copied()
    }

    fn get_in_current(&self, name: &str) -> Option<u32> {
        self.0.last().unwrap().get(name).copied()
    }

    fn clear(&mut self) {
        self.0.clear();
    }

    fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.0.pop();
    }

    fn insert(&mut self, name: String, id: u32) {
        self.0.last_mut().unwrap().insert(name, id);
    }
}

pub fn report_duplicate_definition(
    msg: &str,
    span: &Span,
    prev_span: &Span,
    sources: &Sources,
) -> Result<()> {
    Report::build(ReportKind::Error, span.0, span.1.start)
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
        .eprint(sources)
        .unwrap();
    Err(())
}

fn type_mismatch(
    type1: Option<ast::Type>,
    span1: &Span,
    type2: Option<ast::Type>,
    span2: &Span,
    sources: &Sources,
) -> Result<()> {
    Report::build(ReportKind::Error, span2.0, span2.1.start)
        .with_message("Type mismatch")
        .with_label(
            Label::new(span1.clone())
                .with_message(format!(
                    "Expected type {:?}...",
                    type1
                        .map(|t| format!("{:?}", t))
                        .unwrap_or_else(|| "void".to_string())
                ))
                .with_color(Color::Yellow),
        )
        .with_label(
            Label::new(span2.clone())
                .with_message(format!(
                    "...but found type {}",
                    type2
                        .map(|t| format!("{:?}", t))
                        .unwrap_or_else(|| "void".to_string())
                ))
                .with_color(Color::Red),
        )
        .finish()
        .eprint(sources)
        .unwrap();
    Err(())
}

pub fn report_error(msg: &str, span: &Span, sources: &Sources) -> Result<()> {
    Report::build(ReportKind::Error, span.0, span.1.start)
        .with_message(msg)
        .with_label(
            Label::new(span.clone())
                .with_message(msg)
                .with_color(Color::Red),
        )
        .finish()
        .eprint(sources)
        .unwrap();
    Err(())
}

fn expected_type(span: &Span, sources: &Sources) -> Result<()> {
    report_error(
        "Expected value but found expression of type void",
        span,
        sources,
    )
}

fn unknown_variable(span: &Span, sources: &Sources) -> Result<()> {
    report_error("Unknown variable", span, sources)
}

fn immutable_assign(span: &Span, sources: &Sources) -> Result<()> {
    report_error("Trying to assign to immutable variable", span, sources)
}

fn missing_label(span: &Span, sources: &Sources) -> Result<()> {
    report_error("Label not found", span, sources)
}

fn tc_expression(context: &mut Context, expr: &mut ast::Expression) -> Result<()> {
    expr.type_ = match expr.expr {
        ast::Expr::Block {
            ref mut statements,
            ref mut final_expression,
        } => {
            context.local_vars.push_scope();
            for stmt in statements {
                tc_expression(context, stmt)?;
            }
            let type_ = if let Some(final_expression) = final_expression {
                tc_expression(context, final_expression)?;
                final_expression.type_
            } else {
                None
            };
            context.local_vars.pop_scope();
            type_
        }
        ast::Expr::Let {
            ref mut value,
            ref mut type_,
            ref name,
            let_type,
            ref mut local_id,
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
                            context.sources,
                        );
                    }
                } else if value.type_.is_none() {
                    return expected_type(&value.span, context.sources);
                } else {
                    *type_ = value.type_;
                }
            }
            if let Some(type_) = type_ {
                let store = let_type != ast::LetType::Inline;
                let id = context
                    .local_vars
                    .get_in_current(name)
                    .filter(|id| {
                        let local = &context.locals[*id];
                        local.type_ == *type_ && store == local.index.is_some()
                    })
                    .unwrap_or_else(|| {
                        context
                            .locals
                            .add_local(expr.span.clone(), name.clone(), *type_, store)
                    });
                *local_id = Some(id);
                context.local_vars.insert(name.clone(), id);
            } else {
                return report_error("Type missing", &expr.span, context.sources);
            }
            None
        }
        ast::Expr::Peek(ref mut mem_location) => {
            tc_mem_location(context, mem_location)?;
            let ty = match mem_location.size {
                MemSize::Float => F32,
                _ => I32,
            };
            Some(ty)
        }
        ast::Expr::Poke {
            ref mut mem_location,
            ref mut value,
        } => {
            tc_mem_location(context, mem_location)?;
            tc_expression(context, value)?;
            let ty = match mem_location.size {
                MemSize::Float => F32,
                _ => I32,
            };
            if value.type_ != Some(ty) {
                return type_mismatch(
                    Some(ty),
                    &expr.span,
                    value.type_,
                    &value.span,
                    context.sources,
                );
            }
            None
        }
        ast::Expr::I32Const(_) => Some(ast::Type::I32),
        ast::Expr::I64Const(_) => Some(ast::Type::I64),
        ast::Expr::F32Const(_) => Some(ast::Type::F32),
        ast::Expr::F64Const(_) => Some(ast::Type::F64),
        ast::Expr::V128Const(_) => Some(ast::Type::V128),
        ast::Expr::UnaryOp { op, ref mut value } => {
            tc_expression(context, value)?;
            if value.type_.is_none() {
                return expected_type(&value.span, context.sources);
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
                        context.sources,
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
                        context.sources,
                    );
                }
            } else {
                return expected_type(&left.span, context.sources);
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
                            context.sources,
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
                            context.sources,
                        );
                    } else {
                        Some(I32)
                    }
                }
            }
        }
        ast::Expr::Variable {
            ref name,
            ref mut local_id,
        } => {
            if let Some(id) = context.local_vars.get(name) {
                *local_id = Some(id);
                Some(context.locals[id].type_)
            } else if let Some(&Var { type_, .. }) = context.global_vars.get(name) {
                Some(type_)
            } else {
                return unknown_variable(&expr.span, context.sources);
            }
        }
        ast::Expr::Assign {
            ref name,
            ref mut value,
            ref mut local_id,
        } => {
            tc_expression(context, value)?;

            let (type_, span) = if let Some(id) = context.local_vars.get(name) {
                *local_id = Some(id);
                let local = &context.locals[id];
                if local.index.is_none() {
                    return immutable_assign(&expr.span, context.sources);
                }
                (local.type_, &local.span)
            } else if let Some(&Var {
                type_,
                ref span,
                mutable,
            }) = context.global_vars.get(name)
            {
                if !mutable {
                    return immutable_assign(&expr.span, context.sources);
                }
                (type_, span)
            } else {
                return unknown_variable(&expr.span, context.sources);
            };

            if value.type_ != Some(type_) {
                return type_mismatch(Some(type_), span, value.type_, &value.span, context.sources);
            }
            None
        }
        ast::Expr::LocalTee {
            ref name,
            ref mut value,
            ref mut local_id,
        } => {
            tc_expression(context, value)?;
            if let Some(id) = context.local_vars.get(name) {
                *local_id = Some(id);
                let local = &context.locals[id];

                if local.index.is_none() {
                    return immutable_assign(&expr.span, context.sources);
                }

                if value.type_ != Some(local.type_) {
                    return type_mismatch(
                        Some(local.type_),
                        &local.span,
                        value.type_,
                        &value.span,
                        context.sources,
                    );
                }

                Some(local.type_)
            } else {
                return unknown_variable(&expr.span, context.sources);
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
        ast::Expr::LabelBlock {
            ref label,
            ref mut block,
        } => {
            context.block_stack.push(label.clone());
            tc_expression(context, block)?;
            context.block_stack.pop();
            if block.type_ != None {
                // TODO: implement, requires branches to optionally provide values
                return type_mismatch(None, &expr.span, block.type_, &block.span, context.sources);
            }
            None
        }
        ast::Expr::Branch(ref label) => {
            if !context.block_stack.contains(label) {
                return missing_label(&expr.span, context.sources);
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
                    context.sources,
                );
            }
            if !context.block_stack.contains(label) {
                return missing_label(&expr.span, context.sources);
            }
            None
        }
        ast::Expr::Cast {
            ref mut value,
            type_,
        } => {
            tc_expression(context, value)?;
            if value.type_.is_none() {
                return expected_type(&expr.span, context.sources);
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
                    return expected_type(&param.span, context.sources);
                }
            }
            if let Some(load) = context.intrinsics.find_load(name) {
                tc_memarg(context, params.as_mut_slice(), &expr.span)?;
                Some(load.type_)
            } else if let Some(load_lane) = context.intrinsics.find_load_lane(name) {
                if let Some(value) = params.first_mut() {
                    tc_expression(context, value)?;
                    if value.type_ != Some(V128) {
                        type_mismatch(
                            Some(V128),
                            &expr.span,
                            value.type_,
                            &value.span,
                            context.sources,
                        )?;
                    }
                } else {
                    return report_error("Missing parameters", &expr.span, context.sources);
                }
                if let Some(value) = params.get_mut(1) {
                    tc_lane(load_lane.lane_width, context, value, &expr.span)?;
                } else {
                    return report_error("Missing parameters", &expr.span, context.sources);
                }
                tc_memarg(context, &mut params[2..], &expr.span)?;
                Some(V128)
            } else if let Some(store) = context.intrinsics.find_store(name) {
                if let Some(value) = params.first_mut() {
                    tc_expression(context, value)?;
                    if value.type_ != Some(store.type_) {
                        type_mismatch(
                            Some(store.type_),
                            &expr.span,
                            value.type_,
                            &value.span,
                            context.sources,
                        )?;
                    }
                } else {
                    return report_error("Missing parameters", &expr.span, context.sources);
                }
                tc_memarg(context, &mut params[1..], &expr.span)?;
                None
            } else if let Some(store_lane) = context.intrinsics.find_store_lane(name) {
                if let Some(value) = params.first_mut() {
                    tc_expression(context, value)?;
                    if value.type_ != Some(V128) {
                        type_mismatch(
                            Some(V128),
                            &expr.span,
                            value.type_,
                            &value.span,
                            context.sources,
                        )?;
                    }
                } else {
                    return report_error("Missing parameters", &expr.span, context.sources);
                }
                if let Some(value) = params.get_mut(1) {
                    tc_lane(store_lane.lane_width, context, value, &expr.span)?;
                } else {
                    return report_error("Missing parameters", &expr.span, context.sources);
                }
                tc_memarg(context, &mut params[2..], &expr.span)?;
                None
            } else if let Some(lane_instr) = context.intrinsics.find_lane(name) {
                if let Some(value) = params.first_mut() {
                    tc_expression(context, value)?;
                    if value.type_ != Some(V128) {
                        type_mismatch(
                            Some(V128),
                            &expr.span,
                            value.type_,
                            &value.span,
                            context.sources,
                        )?;
                    }
                } else {
                    return report_error("Missing parameters", &expr.span, context.sources);
                }
                if let Some(value) = params.get_mut(1) {
                    tc_lane(lane_instr.lane_width, context, value, &expr.span)?;
                } else {
                    return report_error("Missing parameters", &expr.span, context.sources);
                }
                if let Some(param_type) = lane_instr.param_type {
                    if let Some(value) = params.get_mut(2) {
                        tc_expression(context, value)?;
                        if value.type_ != Some(param_type) {
                            type_mismatch(
                                Some(param_type),
                                &expr.span,
                                value.type_,
                                &value.span,
                                context.sources,
                            )?;
                        }
                    } else {
                        return report_error("Missing parameters", &expr.span, context.sources);
                    }
                }
                Some(lane_instr.return_type)
            } else if let Some(type_map) = context
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
                    let mut report =
                        Report::build(ReportKind::Error, expr.span.0, expr.span.1.start)
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
                    report.finish().eprint(context.sources).unwrap();
                    return Err(());
                }
            } else {
                return report_error(
                    &format!("Unknown function {}", name),
                    &expr.span,
                    context.sources,
                );
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
                    context.sources,
                );
            }
            if if_true.type_.is_some() {
                if if_true.type_ != if_false.type_ {
                    return type_mismatch(
                        if_true.type_,
                        &if_true.span,
                        if_false.type_,
                        &if_false.span,
                        context.sources,
                    );
                }
            } else {
                return expected_type(&if_true.span, context.sources);
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
                        context.sources,
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
                        context.sources,
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
    tc_const(&mut mem_location.right, context.sources)?;
    if mem_location.left.type_ != Some(I32) {
        return type_mismatch(
            Some(I32),
            &mem_location.left.span,
            mem_location.left.type_,
            &mem_location.left.span,
            context.sources,
        );
    }
    if mem_location.right.type_ != Some(I32) {
        return type_mismatch(
            Some(I32),
            &mem_location.right.span,
            mem_location.right.type_,
            &mem_location.right.span,
            context.sources,
        );
    }
    Ok(())
}

fn tc_const(expr: &mut ast::Expression, sources: &Sources) -> Result<()> {
    use ast::Expr::*;
    expr.type_ = Some(match expr.expr {
        I32Const(_) => I32,
        I64Const(_) => I64,
        F32Const(_) => F32,
        F64Const(_) => F64,
        V128Const(_) => V128,
        _ => return report_error("Expected constant value", &expr.span, sources),
    });
    Ok(())
}

fn tc_memarg(context: &mut Context, params: &mut [ast::Expression], span: &Span) -> Result<()> {
    if params.is_empty() || params.len() > 3 {
        let msg = if params.is_empty() {
            "Missing base address parameter"
        } else {
            "Too many MemArg parameters"
        };
        return report_error(msg, span, context.sources);
    }

    for (index, param) in params.iter_mut().enumerate() {
        tc_expression(context, param)?;
        if param.type_ != Some(I32) {
            return type_mismatch(Some(I32), &span, param.type_, &param.span, context.sources);
        }
        if index > 0 {
            tc_const(param, context.sources)?;
        }
        if index == 2 {
            let align = param.const_i32();
            if align < 0 || align > 4 {
                return report_error(
                    &format!("Alignment {} out of range (0-4)", align),
                    &param.span,
                    context.sources,
                );
            }
        }
    }

    Ok(())
}

fn tc_lane(lane_width: u32, context: &mut Context, param: &mut ast::Expression, span: &Span) -> Result<()> {
    if param.type_ != Some(I32) {
        return type_mismatch(Some(I32), &span, param.type_, &param.span, context.sources);
    }
    let lane = param.const_i32();
    let max_lane = (16 >> lane_width) - 1;
    if lane < 0 || lane > max_lane {
        return report_error(
            &format!("Lane {} out of range (0-{})", lane, max_lane),
            &param.span,
            context.sources,
        );
    }
    Ok(())
}
