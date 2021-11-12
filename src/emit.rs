use std::collections::HashMap;

use wasm_encoder::{
    BlockType, CodeSection, DataSection, EntityType, Export, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction, MemArg, MemoryType,
    Module, TypeSection, ValType,
};

use crate::{ast, intrinsics::Intrinsics};

pub fn emit(script: &ast::Script) -> Vec<u8> {
    let mut module = Module::new();

    let function_types = collect_function_types(script);
    {
        let mut types = TypeSection::new();
        let mut type_vec: Vec<_> = function_types.iter().map(|(k, v)| (*v, k)).collect();
        type_vec.sort();
        for (_, (params, result)) in type_vec {
            let params: Vec<_> = params.iter().cloned().map(map_type).collect();
            let result: Vec<_> = result.iter().cloned().map(map_type).collect();
            types.function(params, result);
        }
        module.section(&types);
    }

    let mut globals: HashMap<&str, u32> = HashMap::new();
    let mut function_map = HashMap::new();

    {
        let mut imports = ImportSection::new();

        for import in &script.imports {
            let (module, name) = if let Some(dot_index) = import.import.find('.') {
                (
                    &import.import[..dot_index],
                    Some(&import.import[(dot_index + 1)..]),
                )
            } else {
                (import.import.as_str(), None)
            };
            let type_: EntityType = match import.type_ {
                ast::ImportType::Memory(min_size) => MemoryType {
                    minimum: min_size as u64,
                    maximum: None,
                    memory64: false,
                }
                .into(),
                ast::ImportType::Variable {
                    type_,
                    ref name,
                    mutable,
                } => {
                    globals.insert(name, globals.len() as u32);
                    GlobalType {
                        val_type: map_type(type_),
                        mutable,
                    }
                    .into()
                }
                ast::ImportType::Function {
                    ref name,
                    ref params,
                    ref result,
                } => {
                    function_map.insert(name.clone(), function_map.len() as u32);
                    EntityType::Function(
                        *function_types
                            .get(&(params.clone(), result.clone()))
                            .unwrap() as u32,
                    )
                }
            };
            imports.import(module, name, type_);
        }

        module.section(&imports);
    }

    let mut global_section = GlobalSection::new();
    for var in &script.global_vars {
        global_section.global(
            GlobalType {
                val_type: map_type(var.type_.unwrap()),
                mutable: var.mutable,
            },
            &const_instr(&var.value),
        );
        globals.insert(&var.name, globals.len() as u32);
    }

    {
        let mut functions = FunctionSection::new();
        let mut exports = ExportSection::new();
        let mut code = CodeSection::new();

        let intrinsics = Intrinsics::new();

        for func in script.functions.iter() {
            function_map.insert(func.name.clone(), function_map.len() as u32);
        }

        for func in script.functions.iter() {
            let type_ = *function_types.get(&function_type_key(func)).unwrap();
            functions.function(type_ as u32);
            if func.export {
                exports.export(
                    &func.name,
                    Export::Function(*function_map.get(&func.name).unwrap() as u32),
                );
            }

            code.function(&emit_function(func, &globals, &function_map, &intrinsics));
        }

        module.section(&functions);
        if !script.global_vars.is_empty() {
            module.section(&global_section);
        }
        module.section(&exports);
        module.section(&code);
    }

    if !script.data.is_empty() {
        let mut data_section = DataSection::new();

        for data in &script.data {
            let mut segment_data: Vec<u8> = vec![];
            for values in &data.data {
                match values {
                    ast::DataValues::Array { type_, values } => {
                        let width = match *type_ {
                            ast::DataType::I8 => 1,
                            ast::DataType::I16 => 2,
                            ast::DataType::I32 => 4,
                            ast::DataType::I64 => 8,
                            ast::DataType::F32 => 4,
                            ast::DataType::F64 => 8,
                        };
                        while segment_data.len() % width != 0 {
                            segment_data.push(0);
                        }
                        for value in values {
                            match *type_ {
                                ast::DataType::I8 => segment_data.push(value.const_i32() as u8),
                                ast::DataType::I16 => segment_data
                                    .extend_from_slice(&(value.const_i32() as u16).to_le_bytes()),
                                ast::DataType::I32 => segment_data
                                    .extend_from_slice(&(value.const_i32() as u32).to_le_bytes()),
                                ast::DataType::I64 => segment_data
                                    .extend_from_slice(&(value.const_i64() as u64).to_le_bytes()),
                                ast::DataType::F32 => {
                                    segment_data.extend_from_slice(&value.const_f32().to_le_bytes())
                                }
                                ast::DataType::F64 => {
                                    segment_data.extend_from_slice(&value.const_f64().to_le_bytes())
                                }
                            }
                        }
                    }
                    ast::DataValues::String(s) => {
                        for c in s.chars() {
                            segment_data.push(c as u8);
                        }
                    }
                }
            }
            data_section.active(
                0,
                &wasm_encoder::Instruction::I32Const(data.offset.const_i32()),
                segment_data,
            );
        }

        module.section(&data_section);
    }

    module.finish()
}

type FunctionTypeKey = (Vec<ast::Type>, Option<ast::Type>);

fn collect_function_types(script: &ast::Script) -> HashMap<FunctionTypeKey, usize> {
    let mut types: HashMap<FunctionTypeKey, usize> = HashMap::new();

    for import in &script.imports {
        if let ast::ImportType::Function {
            ref params,
            ref result,
            ..
        } = import.type_
        {
            let index = types.len();
            types
                .entry((params.clone(), result.clone()))
                .or_insert(index);
        }
    }

    for func in &script.functions {
        let index = types.len();
        types.entry(function_type_key(func)).or_insert(index);
    }

    types
}

fn function_type_key(func: &ast::Function) -> FunctionTypeKey {
    let param_types: Vec<_> = func.params.iter().map(|(_, type_)| *type_).collect();
    (param_types, func.type_)
}

fn const_instr(expr: &ast::Expression) -> Instruction {
    match expr.expr {
        ast::Expr::I32Const(v) => Instruction::I32Const(v),
        ast::Expr::F32Const(v) => Instruction::F32Const(v),
        ast::Expr::I64Const(v) => Instruction::I64Const(v),
        ast::Expr::F64Const(v) => Instruction::F64Const(v),
        _ => unreachable!(),
    }
}

struct FunctionContext<'a> {
    function: &'a mut Function,
    globals: &'a HashMap<&'a str, u32>,
    functions: &'a HashMap<String, u32>,
    locals: &'a HashMap<String, u32>,
    labels: Vec<String>,
    let_values: HashMap<&'a str, (&'a ast::Expression, ast::LetType)>,
    intrinsics: &'a Intrinsics,
}

fn emit_function(
    func: &ast::Function,
    globals: &HashMap<&str, u32>,
    functions: &HashMap<String, u32>,
    intrinsics: &Intrinsics,
) -> Function {
    let mut locals = Vec::new();
    collect_locals_expr(&func.body, &mut locals);
    locals.sort_by_key(|(_, t)| *t);

    let mut function = Function::new_with_locals_types(locals.iter().map(|(_, t)| map_type(*t)));

    let mut local_map: HashMap<String, u32> = HashMap::new();

    for (ref name, _) in func.params.iter() {
        local_map.insert(name.clone(), local_map.len() as u32);
    }

    for (name, _) in locals {
        local_map.insert(name, local_map.len() as u32);
    }

    let mut context = FunctionContext {
        function: &mut function,
        globals,
        functions,
        locals: &local_map,
        labels: vec![],
        let_values: HashMap::new(),
        intrinsics,
    };

    emit_expression(&mut context, &func.body);
    if func.type_.is_none() && func.body.type_.is_some() {
        function.instruction(&Instruction::Drop);
    }
    function.instruction(&Instruction::End);

    function
}

fn collect_locals_expr<'a>(expr: &ast::Expression, locals: &mut Vec<(String, ast::Type)>) {
    match &expr.expr {
        ast::Expr::Block {
            statements,
            final_expression,
        } => {
            for stmt in statements {
                collect_locals_expr(stmt, locals);
            }
            if let Some(ref expr) = final_expression {
                collect_locals_expr(expr, locals);
            }
        }
        ast::Expr::Let {
            name, type_, value, ..
        } => {
            locals.push((name.clone(), type_.unwrap()));
            if let Some(ref value) = value {
                collect_locals_expr(value, locals);
            }
        }
        ast::Expr::Peek(mem_location) => collect_locals_expr(&mem_location.left, locals),
        ast::Expr::Poke {
            mem_location,
            value,
            ..
        } => {
            collect_locals_expr(&mem_location.left, locals);
            collect_locals_expr(value, locals);
        }
        ast::Expr::Variable { .. }
        | ast::Expr::I32Const(_)
        | ast::Expr::I64Const(_)
        | ast::Expr::F32Const(_)
        | ast::Expr::F64Const(_) => (),
        ast::Expr::UnaryOp { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::BinOp { left, right, .. } => {
            collect_locals_expr(left, locals);
            collect_locals_expr(right, locals);
        }
        ast::Expr::Branch(_) => (),
        ast::Expr::BranchIf { condition, .. } => collect_locals_expr(condition, locals),
        ast::Expr::Assign { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::LocalTee { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::Loop { block, .. } => collect_locals_expr(block, locals),
        ast::Expr::Cast { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::FuncCall { params, .. } => {
            for param in params {
                collect_locals_expr(param, locals);
            }
        }
        ast::Expr::Select {
            condition,
            if_true,
            if_false,
            ..
        } => {
            collect_locals_expr(condition, locals);
            collect_locals_expr(if_true, locals);
            collect_locals_expr(if_false, locals);
        }
        ast::Expr::If {
            condition,
            if_true,
            if_false,
        } => {
            collect_locals_expr(condition, locals);
            collect_locals_expr(if_true, locals);
            if let Some(if_false) = if_false {
                collect_locals_expr(if_false, locals);
            }
        }
        ast::Expr::Return { value: Some(value) } => collect_locals_expr(value, locals),
        ast::Expr::Return { value: None } => (),
        ast::Expr::First { value, drop } => {
            collect_locals_expr(value, locals);
            collect_locals_expr(drop, locals);
        }
        ast::Expr::Error => unreachable!(),
    }
}

fn mem_arg_for_location(mem_location: &ast::MemoryLocation) -> MemArg {
    let offset = if let ast::Expr::I32Const(v) = mem_location.right.expr {
        v as u32 as u64
    } else {
        unreachable!()
    };
    match mem_location.size {
        ast::MemSize::Byte => MemArg {
            align: 0,
            memory_index: 0,
            offset,
        },
        ast::MemSize::Word => MemArg {
            align: 2,
            memory_index: 0,
            offset,
        },
    }
}

fn emit_expression<'a>(ctx: &mut FunctionContext<'a>, expr: &'a ast::Expression) {
    match &expr.expr {
        ast::Expr::Block {
            statements,
            final_expression,
        } => {
            for stmt in statements {
                emit_expression(ctx, stmt);
                if stmt.type_.is_some() {
                    ctx.function.instruction(&Instruction::Drop);
                }
            }
            if let Some(ref expr) = final_expression {
                emit_expression(ctx, expr);
            }
        }
        ast::Expr::Let {
            value,
            name,
            let_type,
            ..
        } => {
            if let Some(ref value) = value {
                match let_type {
                    ast::LetType::Normal => {
                        emit_expression(ctx, value);
                        ctx.function
                            .instruction(&Instruction::LocalSet(*ctx.locals.get(name).unwrap()));
                    }
                    ast::LetType::Lazy | ast::LetType::Inline => {
                        ctx.let_values.insert(name, (value, *let_type));
                    }
                }
            }
        }
        ast::Expr::Peek(mem_location) => {
            emit_expression(ctx, &mem_location.left);
            let mem_arg = mem_arg_for_location(mem_location);
            ctx.function.instruction(&match mem_location.size {
                ast::MemSize::Byte => Instruction::I32Load8_U(mem_arg),
                ast::MemSize::Word => Instruction::I32Load(mem_arg),
            });
        }
        ast::Expr::Poke {
            mem_location,
            value,
        } => {
            emit_expression(ctx, &mem_location.left);
            emit_expression(ctx, value);
            let mem_arg = mem_arg_for_location(mem_location);
            ctx.function.instruction(&match mem_location.size {
                ast::MemSize::Byte => Instruction::I32Store8(mem_arg),
                ast::MemSize::Word => Instruction::I32Store(mem_arg),
            });
        }
        ast::Expr::UnaryOp { op, value } => {
            use ast::Type::*;
            use ast::UnaryOp::*;
            match (value.type_.unwrap(), op) {
                (I32, Negate) => {
                    ctx.function.instruction(&Instruction::I32Const(0));
                    emit_expression(ctx, value);
                    ctx.function.instruction(&Instruction::I32Sub);
                }
                (I64, Negate) => {
                    ctx.function.instruction(&Instruction::I64Const(0));
                    emit_expression(ctx, value);
                    ctx.function.instruction(&Instruction::I64Sub);
                }
                (F32, Negate) => {
                    emit_expression(ctx, value);
                    ctx.function.instruction(&Instruction::F32Neg);
                }
                (F64, Negate) => {
                    emit_expression(ctx, value);
                    ctx.function.instruction(&Instruction::F64Neg);
                }
                (I32, Not) => {
                    emit_expression(ctx, value);
                    ctx.function.instruction(&Instruction::I32Eqz);
                }
                (I64, Not) => {
                    emit_expression(ctx, value);
                    ctx.function.instruction(&Instruction::I64Eqz);
                }
                (_, Not) => unreachable!(),
            };
        }
        ast::Expr::BinOp {
            left, op, right, ..
        } => {
            emit_expression(ctx, left);
            emit_expression(ctx, right);
            use ast::BinOp::*;
            use ast::Type::*;
            ctx.function.instruction(&match (left.type_.unwrap(), op) {
                (I32, Add) => Instruction::I32Add,
                (I32, Sub) => Instruction::I32Sub,
                (I32, Mul) => Instruction::I32Mul,
                (I32, Div) => Instruction::I32DivS,
                (I32, DivU) => Instruction::I32DivU,
                (I32, Rem) => Instruction::I32RemS,
                (I32, RemU) => Instruction::I32RemU,
                (I32, And) => Instruction::I32And,
                (I32, Or) => Instruction::I32Or,
                (I32, Xor) => Instruction::I32Xor,
                (I32, Eq) => Instruction::I32Eq,
                (I32, Ne) => Instruction::I32Neq,
                (I32, Lt) => Instruction::I32LtS,
                (I32, LtU) => Instruction::I32LtU,
                (I32, Le) => Instruction::I32LeS,
                (I32, LeU) => Instruction::I32LeU,
                (I32, Gt) => Instruction::I32GtS,
                (I32, GtU) => Instruction::I32GtU,
                (I32, Ge) => Instruction::I32GeS,
                (I32, GeU) => Instruction::I32GeU,
                (I32, Shl) => Instruction::I32Shl,
                (I32, ShrU) => Instruction::I32ShrU,
                (I32, ShrS) => Instruction::I32ShrS,

                (I64, Add) => Instruction::I64Add,
                (I64, Sub) => Instruction::I64Sub,
                (I64, Mul) => Instruction::I64Mul,
                (I64, Div) => Instruction::I64DivS,
                (I64, DivU) => Instruction::I64DivU,
                (I64, Rem) => Instruction::I64RemS,
                (I64, RemU) => Instruction::I64RemU,
                (I64, And) => Instruction::I64And,
                (I64, Or) => Instruction::I64Or,
                (I64, Xor) => Instruction::I64Xor,
                (I64, Eq) => Instruction::I64Eq,
                (I64, Ne) => Instruction::I64Neq,
                (I64, Lt) => Instruction::I64LtS,
                (I64, LtU) => Instruction::I64LtU,
                (I64, Le) => Instruction::I64LeS,
                (I64, LeU) => Instruction::I64LeU,
                (I64, Gt) => Instruction::I64GtS,
                (I64, GtU) => Instruction::I64GtU,
                (I64, Ge) => Instruction::I64GeS,
                (I64, GeU) => Instruction::I64GeU,
                (I64, Shl) => Instruction::I64Shl,
                (I64, ShrU) => Instruction::I64ShrU,
                (I64, ShrS) => Instruction::I64ShrS,

                (F32, Add) => Instruction::F32Add,
                (F32, Sub) => Instruction::F32Sub,
                (F32, Mul) => Instruction::F32Mul,
                (F32, Div) => Instruction::F32Div,
                (
                    F32,
                    DivU | Rem | RemU | And | Or | Xor | Shl | ShrU | ShrS | LtU | LeU | GtU | GeU,
                ) => unreachable!(),
                (F32, Eq) => Instruction::F32Eq,
                (F32, Ne) => Instruction::F32Neq,
                (F32, Lt) => Instruction::F32Lt,
                (F32, Le) => Instruction::F32Le,
                (F32, Gt) => Instruction::F32Gt,
                (F32, Ge) => Instruction::F32Ge,

                (F64, Add) => Instruction::F64Add,
                (F64, Sub) => Instruction::F64Sub,
                (F64, Mul) => Instruction::F64Mul,
                (F64, Div) => Instruction::F64Div,
                (
                    F64,
                    DivU | Rem | RemU | And | Or | Xor | Shl | ShrU | ShrS | LtU | LeU | GtU | GeU,
                ) => unreachable!(),
                (F64, Eq) => Instruction::F64Eq,
                (F64, Ne) => Instruction::F64Neq,
                (F64, Lt) => Instruction::F64Lt,
                (F64, Le) => Instruction::F64Le,
                (F64, Gt) => Instruction::F64Gt,
                (F64, Ge) => Instruction::F64Ge,
            });
        }
        ast::Expr::Branch(label) => {
            let depth = ctx
                .labels
                .iter()
                .rev()
                .enumerate()
                .find(|(_, l)| *l == label)
                .unwrap()
                .0;
            ctx.function.instruction(&Instruction::Br(depth as u32));
        }
        ast::Expr::BranchIf {
            condition, label, ..
        } => {
            emit_expression(ctx, condition);
            let depth = ctx
                .labels
                .iter()
                .rev()
                .enumerate()
                .find(|(_, l)| *l == label)
                .unwrap()
                .0;
            ctx.function.instruction(&Instruction::BrIf(depth as u32));
        }
        ast::Expr::I32Const(v) => {
            ctx.function.instruction(&Instruction::I32Const(*v));
        }
        ast::Expr::I64Const(v) => {
            ctx.function.instruction(&Instruction::I64Const(*v));
        }
        ast::Expr::F32Const(v) => {
            ctx.function.instruction(&Instruction::F32Const(*v));
        }
        ast::Expr::F64Const(v) => {
            ctx.function.instruction(&Instruction::F64Const(*v));
        }
        ast::Expr::Assign { name, value, .. } => {
            emit_expression(ctx, value);
            if let Some(local_index) = ctx.locals.get(name) {
                ctx.function
                    .instruction(&Instruction::LocalSet(*local_index));
            } else if let Some(global_index) = ctx.globals.get(name.as_str()) {
                ctx.function
                    .instruction(&Instruction::GlobalSet(*global_index));
            } else {
                unreachable!();
            }
        }
        ast::Expr::LocalTee { name, value, .. } => {
            emit_expression(ctx, value);
            let index = ctx.locals.get(name).unwrap();
            ctx.function.instruction(&Instruction::LocalTee(*index));
        }
        ast::Expr::Loop { label, block, .. } => {
            ctx.labels.push(label.to_string());
            ctx.function
                .instruction(&Instruction::Loop(map_block_type(block.type_)));
            emit_expression(ctx, block);
            ctx.labels.pop();
            ctx.function.instruction(&Instruction::End);
        }
        ast::Expr::Variable(name) => {
            if let Some(index) = ctx.locals.get(name) {
                if let Some((expr, let_type)) = ctx.let_values.get(name.as_str()) {
                    match let_type {
                        ast::LetType::Lazy => {
                            let expr = ctx.let_values.remove(name.as_str()).unwrap().0;
                            emit_expression(ctx, expr);
                            ctx.function.instruction(&Instruction::LocalTee(*index));
                        }
                        ast::LetType::Inline => {
                            let expr = *expr;
                            emit_expression(ctx, expr);
                        }
                        _ => unreachable!(),
                    }
                } else {
                    ctx.function.instruction(&Instruction::LocalGet(*index));
                }
            } else if let Some(index) = ctx.globals.get(name.as_str()) {
                ctx.function.instruction(&Instruction::GlobalGet(*index));
            } else {
                dbg!(name);
                unreachable!()
            }
        }
        ast::Expr::Cast { value, type_, .. } => {
            emit_expression(ctx, value);
            use ast::Type::*;
            let inst = match (value.type_.unwrap(), *type_) {
                (I32, I64) => Some(Instruction::I64ExtendI32S),
                (I64, I32) => Some(Instruction::I32WrapI64),
                (I32, F32) => Some(Instruction::F32ConvertI32S),
                (F32, I32) => Some(Instruction::I32TruncF32S),
                (I64, F32) => Some(Instruction::F32ConvertI64S),
                (F32, I64) => Some(Instruction::I64TruncF32S),
                (I32, F64) => Some(Instruction::F64ConvertI32S),
                (F64, I32) => Some(Instruction::I32TruncF64S),
                (I64, F64) => Some(Instruction::F64ConvertI64S),
                (F64, I64) => Some(Instruction::I64TruncF64S),
                (F32, F64) => Some(Instruction::F64PromoteF32),
                (F64, F32) => Some(Instruction::F32DemoteF64),

                (I32, I32) | (I64, I64) | (F32, F32) | (F64, F64) => None,
            };
            if let Some(inst) = inst {
                ctx.function.instruction(&inst);
            }
        }
        ast::Expr::FuncCall { name, params, .. } => {
            for param in params {
                emit_expression(ctx, param);
            }

            if let Some(index) = ctx.functions.get(name) {
                ctx.function.instruction(&Instruction::Call(*index));
            } else {
                let mut types = vec![];
                for param in params {
                    types.push(param.type_.unwrap());
                }
                ctx.function
                    .instruction(&ctx.intrinsics.get_instr(name, &types).unwrap());
            }
        }
        ast::Expr::Select {
            condition,
            if_true,
            if_false,
            ..
        } => {
            emit_expression(ctx, if_true);
            emit_expression(ctx, if_false);
            emit_expression(ctx, condition);
            ctx.function.instruction(&Instruction::Select);
        }
        ast::Expr::If {
            condition,
            if_true,
            if_false,
        } => {
            emit_expression(ctx, condition);
            ctx.function
                .instruction(&Instruction::If(map_block_type(expr.type_)));
            ctx.labels.push(String::new());
            emit_expression(ctx, if_true);
            if if_true.type_.is_some() && if_true.type_ != expr.type_ {
                ctx.function.instruction(&Instruction::Drop);
            }
            if let Some(if_false) = if_false {
                ctx.function.instruction(&Instruction::Else);
                emit_expression(ctx, if_false);
                if if_false.type_.is_some() && if_false.type_ != expr.type_ {
                    ctx.function.instruction(&Instruction::Drop);
                }
            }
            ctx.labels.pop();
            ctx.function.instruction(&Instruction::End);
        }
        ast::Expr::Return { value } => {
            if let Some(value) = value {
                emit_expression(ctx, value);
            }
            ctx.function.instruction(&Instruction::Return);
        }
        ast::Expr::First { value, drop } => {
            emit_expression(ctx, value);
            emit_expression(ctx, drop);
            if drop.type_.is_some() {
                ctx.function.instruction(&Instruction::Drop);
            }
        }
        ast::Expr::Error => unreachable!(),
    }
}

fn map_type(t: ast::Type) -> ValType {
    match t {
        ast::Type::I32 => ValType::I32,
        ast::Type::I64 => ValType::I64,
        ast::Type::F32 => ValType::F32,
        ast::Type::F64 => ValType::F64,
    }
}

fn map_block_type(t: Option<ast::Type>) -> BlockType {
    if let Some(t) = t {
        BlockType::Result(map_type(t))
    } else {
        BlockType::Empty
    }
}
