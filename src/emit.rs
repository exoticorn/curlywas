use std::collections::HashMap;

use wasm_encoder::{
    BlockType, CodeSection, DataSection, EntityType, Export, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, IndirectNameMap, Instruction,
    MemArg, MemoryType, Module, NameMap, NameSection, StartSection, TypeSection, ValType,
};

use crate::{
    ast,
    intrinsics::{Intrinsics, LaneInstruction, MemInstruction, MemLaneInstruction, ShuffleInstruction},
    Options,
};

pub fn emit(script: &ast::Script, module_name: &str, options: &Options) -> Vec<u8> {
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
                        *function_types.get(&(params.clone(), *result)).unwrap() as u32
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

        let mut start_index = None;

        let intrinsics = Intrinsics::new();

        for func in script.functions.iter() {
            if func.start {
                start_index = Some(function_map.len() as u32);
            }
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

        if let Some(function_index) = start_index {
            module.section(&StartSection { function_index });
        }

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
                            ast::DataType::I128 => 16,
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
                                ast::DataType::I128 => segment_data
                                    .extend_from_slice(&(value.const_v128() as u128).to_le_bytes()),
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
                    ast::DataValues::File { data, .. } => {
                        segment_data.extend_from_slice(data);
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

    if options.debug {
        let mut names = NameSection::new();

        names.module(module_name);

        let mut functions = HashMap::new();
        for (name, index) in &function_map {
            functions.insert(*index, name);
        }
        let mut keys: Vec<_> = functions.keys().collect();
        keys.sort();

        let mut function_names = NameMap::new();
        for i in keys {
            function_names.append(*i, functions[i]);
        }
        names.functions(&function_names);

        let mut functions = HashMap::new();
        for function in &script.functions {
            let mut local_names = NameMap::new();
            for param in &function.locals.params {
                local_names.append(param.index.unwrap(), &param.name);
            }

            let mut locals = HashMap::new();
            for local in &function.locals.locals {
                if let Some(index) = local.index {
                    locals.insert(index, &local.name);
                }
            }
            let mut keys: Vec<_> = locals.keys().collect();
            keys.sort();
            for i in keys {
                local_names.append(*i, locals[i]);
            }
            functions.insert(*function_map.get(&function.name).unwrap(), local_names);
        }
        let mut keys: Vec<_> = functions.keys().collect();
        keys.sort();

        let mut locals = IndirectNameMap::new();
        for i in keys {
            locals.append(*i, &functions[i]);
        }
        names.locals(&locals);

        module.section(&names);
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
            types.entry((params.clone(), *result)).or_insert(index);
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
        ast::Expr::V128Const(v) => Instruction::V128Const(v),
        _ => unreachable!(),
    }
}

struct FunctionContext<'a> {
    function: &'a mut Function,
    globals: &'a HashMap<&'a str, u32>,
    functions: &'a HashMap<String, u32>,
    locals: &'a ast::Locals,
    labels: Vec<String>,
    let_values: HashMap<u32, Vec<(&'a ast::Expression, ast::LetType)>>,
    intrinsics: &'a Intrinsics,
}

fn emit_function(
    func: &ast::Function,
    globals: &HashMap<&str, u32>,
    functions: &HashMap<String, u32>,
    intrinsics: &Intrinsics,
) -> Function {
    let mut function = Function::new_with_locals_types({
        let mut locals: Vec<(u32, ast::Type)> = func
            .locals
            .locals
            .iter()
            .filter_map(|local| local.index.map(|i| (i, local.type_)))
            .collect();
        locals.sort();
        locals.into_iter().map(|(_, t)| map_type(t))
    });

    let mut context = FunctionContext {
        function: &mut function,
        globals,
        functions,
        locals: &func.locals,
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
        ast::MemSize::Float => MemArg {
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
            let_type,
            local_id,
            ..
        } => {
            let local = &ctx.locals[local_id.unwrap()];
            if let Some(ref value) = value {
                match let_type {
                    ast::LetType::Normal => {
                        emit_expression(ctx, value);
                        ctx.function
                            .instruction(&Instruction::LocalSet(local.index.unwrap()));
                    }
                    ast::LetType::Lazy | ast::LetType::Inline => {
                        ctx.let_values
                            .entry(local_id.unwrap())
                            .or_default()
                            .push((value, *let_type));
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
                ast::MemSize::Float => Instruction::F32Load(mem_arg),
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
                ast::MemSize::Float => Instruction::F32Store(mem_arg),
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
                (V128, Negate) => unreachable!(),
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
                (I32, Ne) => Instruction::I32Ne,
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
                (I64, Ne) => Instruction::I64Ne,
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
                (F32, Ne) => Instruction::F32Ne,
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
                (F64, Ne) => Instruction::F64Ne,
                (F64, Lt) => Instruction::F64Lt,
                (F64, Le) => Instruction::F64Le,
                (F64, Gt) => Instruction::F64Gt,
                (F64, Ge) => Instruction::F64Ge,

                (V128, _) => unreachable!(),
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
        ast::Expr::V128Const(v) => {
            ctx.function.instruction(&Instruction::V128Const(*v));
        }
        ast::Expr::Assign {
            name,
            value,
            local_id,
            ..
        } => {
            emit_expression(ctx, value);
            if let &Some(id) = local_id {
                ctx.function
                    .instruction(&Instruction::LocalSet(ctx.locals[id].index.unwrap()));
            } else if let Some(global_index) = ctx.globals.get(name.as_str()) {
                ctx.function
                    .instruction(&Instruction::GlobalSet(*global_index));
            } else {
                unreachable!();
            }
        }
        ast::Expr::LocalTee {
            value, local_id, ..
        } => {
            emit_expression(ctx, value);
            ctx.function.instruction(&Instruction::LocalTee(
                ctx.locals[local_id.unwrap()].index.unwrap(),
            ));
        }
        ast::Expr::Loop { label, block, .. } => {
            ctx.labels.push(label.to_string());
            ctx.function
                .instruction(&Instruction::Loop(map_block_type(block.type_)));
            emit_expression(ctx, block);
            ctx.labels.pop();
            ctx.function.instruction(&Instruction::End);
        }
        ast::Expr::LabelBlock { label, block } => {
            ctx.labels.push(label.to_string());
            ctx.function
                .instruction(&Instruction::Block(map_block_type(block.type_)));
            emit_expression(ctx, block);
            ctx.labels.pop();
            ctx.function.instruction(&Instruction::End);
        }
        ast::Expr::Variable { name, local_id } => {
            if let &Some(id) = local_id {
                if let Some((expr, let_type)) = ctx.let_values.get_mut(&id).and_then(|s| s.pop()) {
                    match let_type {
                        ast::LetType::Lazy => {
                            emit_expression(ctx, expr);
                            ctx.let_values.get_mut(&id).unwrap().clear();
                            ctx.function
                                .instruction(&Instruction::LocalTee(ctx.locals[id].index.unwrap()));
                        }
                        ast::LetType::Inline => {
                            emit_expression(ctx, expr);
                            ctx.let_values.get_mut(&id).unwrap().push((expr, let_type));
                        }
                        _ => unreachable!(),
                    }
                } else {
                    ctx.function
                        .instruction(&Instruction::LocalGet(ctx.locals[id].index.unwrap()));
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

                (I32, I32) | (I64, I64) | (F32, F32) | (F64, F64) | (V128, V128) => None,
                (V128, _) | (_, V128) => unreachable!(),
            };
            if let Some(inst) = inst {
                ctx.function.instruction(&inst);
            }
        }
        ast::Expr::FuncCall { name, params, .. } => {
            fn mem_instruction(
                inst: MemInstruction,
                params: &[ast::Expression],
            ) -> Instruction<'static> {
                let offset = params
                    .get(0)
                    .map(|e| e.const_i32() as u32 as u64)
                    .unwrap_or(0);
                let alignment = params.get(1).map(|e| e.const_i32() as u32);
                (inst.instruction)(MemArg {
                    offset,
                    align: alignment.unwrap_or(inst.natural_alignment),
                    memory_index: 0,
                })
            }
            fn mem_lane_instruction(
                inst: MemLaneInstruction,
                lane: u8,
                params: &[ast::Expression],
            ) -> Instruction<'static> {
                let offset = params
                    .get(0)
                    .map(|e| e.const_i32() as u32 as u64)
                    .unwrap_or(0);
                let alignment = params.get(1).map(|e| e.const_i32() as u32);
                (inst.instruction)(MemArg {
                    offset,
                    align: alignment.unwrap_or(inst.natural_alignment),
                    memory_index: 0,
                }, lane)
            }
            fn lane_instruction(
                inst: LaneInstruction,
                lane: u8
            ) -> Instruction<'static> {
                (inst.instruction)(lane)
            }
            fn shuffle_instruction(
                inst: ShuffleInstruction,
                instr_lanes: &[ast::Expression],
            ) -> Instruction<'static> {
                let mut lanes: [u8; 16] = [0; 16];
                for (elem, i) in lanes.iter_mut().zip(0..16) {
                    *elem = instr_lanes.get(i).map(|e| e.const_i32() as u8).unwrap_or(i as u8);
                }
                (inst.instruction)(lanes)
            }
            if let Some(load) = ctx.intrinsics.find_load(name) {
                emit_expression(ctx, &params[0]);
                ctx.function
                    .instruction(&mem_instruction(load, &params[1..]));
            } else if let Some(load_lane) = ctx.intrinsics.find_load_lane(name) {
                emit_expression(ctx, &params[2]);
                emit_expression(ctx, &params[0]);
                let lane = params
                    .get(1)
                    .map(|e| e.const_i32() as u8)
                    .unwrap();
                ctx.function
                    .instruction(&mem_lane_instruction(load_lane, lane, &params[3..]));
            } else if let Some(store) = ctx.intrinsics.find_store(name) {
                emit_expression(ctx, &params[1]);
                emit_expression(ctx, &params[0]);
                ctx.function
                    .instruction(&mem_instruction(store, &params[2..]));
            } else if let Some(store_lane) = ctx.intrinsics.find_store_lane(name) {
                emit_expression(ctx, &params[2]);
                emit_expression(ctx, &params[0]);
                let lane = params
                    .get(1)
                    .map(|e| e.const_i32() as u8)
                    .unwrap();
                ctx.function
                    .instruction(&mem_lane_instruction(store_lane, lane, &params[3..]));
            } else if let Some(lane_instr) = ctx.intrinsics.find_lane(name) {
                emit_expression(ctx, &params[0]);
                let lane = params
                    .get(1)
                    .map(|e| e.const_i32() as u8)
                    .unwrap();
                if let Some(_) = lane_instr.param_type {
                    emit_expression(ctx, &params[2]);
                }
                ctx.function
                    .instruction(&lane_instruction(lane_instr, lane));
            } else if let Some(shuffle) = ctx.intrinsics.find_shuffle(name) {
                emit_expression(ctx, &params[0]);
                emit_expression(ctx, &params[1]);
                ctx.function
                    .instruction(&shuffle_instruction(shuffle, &params[2..]));
            } else {
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
        ast::Type::V128 => ValType::V128,
    }
}

fn map_block_type(t: Option<ast::Type>) -> BlockType {
    if let Some(t) = t {
        BlockType::Result(map_type(t))
    } else {
        BlockType::Empty
    }
}
