use crate::ast::Type;
use std::collections::HashMap;
use wasm_encoder as enc;

pub struct Intrinsics(HashMap<String, HashMap<Vec<Type>, (Type, enc::Instruction<'static>)>>);

impl Intrinsics {
    pub fn new() -> Intrinsics {
        let mut i = Intrinsics(HashMap::new());
        i.add_instructions();
        i
    }

    pub fn find_types(
        &self,
        name: &str,
    ) -> Option<HashMap<Vec<Type>, Option<Type>>> {
        self.0.get(name).map(|types| {
            types
                .iter()
                .map(|(params, (ret, _))| (params.clone(), Some(*ret)))
                .collect()
        })
    }

    pub fn get_instr(&self, name: &str, params: &[Type]) -> Option<enc::Instruction<'static>> {
        self.0
            .get(name)
            .and_then(|types| types.get(params))
            .map(|(_, i)| i.clone())
    }

    fn add_instructions(&mut self) {
        use enc::Instruction as I;
        use Type::*;
        self.inst("i32.rotl", &[I32, I32], I32, I::I32Rotl);
        self.inst("i32.rotr", &[I32, I32], I32, I::I32Rotr);
        self.inst("i32.clz", &[I32], I32, I::I32Clz);
        self.inst("i32.ctz", &[I32], I32, I::I32Ctz);
        self.inst("i32.popcnt", &[I32], I32, I::I32Popcnt);

        self.inst("i64.rotl", &[I64, I64], I64, I::I64Rotl);
        self.inst("i64.rotr", &[I64, I64], I64, I::I64Rotr);
        self.inst("i64.clz", &[I64], I64, I::I64Clz);
        self.inst("i64.ctz", &[I64], I64, I::I64Ctz);
        self.inst("i64.popcnt", &[I64], I64, I::I64Popcnt);

        self.inst("f32/sqrt", &[F32], F32, I::F32Sqrt);
        self.inst("f32/min", &[F32, F32], F32, I::F32Min);
        self.inst("f32/max", &[F32, F32], F32, I::F32Max);
        self.inst("f32/ceil", &[F32], F32, I::F32Ceil);
        self.inst("f32/floor", &[F32], F32, I::F32Floor);
        self.inst("f32/trunc", &[F32], F32, I::F32Trunc);
        self.inst("f32/nearest", &[F32], F32, I::F32Nearest);
        self.inst("f32/abs", &[F32], F32, I::F32Abs);
        self.inst("f32.copysign", &[F32, F32], F32, I::F32Copysign);

        self.inst("f64/sqrt", &[F64], F64, I::F64Sqrt);
        self.inst("f64/min", &[F64, F64], F64, I::F64Min);
        self.inst("f64/max", &[F64, F64], F64, I::F64Max);
        self.inst("f64/ceil", &[F64], F64, I::F64Ceil);
        self.inst("f64/floor", &[F64], F64, I::F64Floor);
        self.inst("f64/trunc", &[F64], F64, I::F64Trunc);
        self.inst("f64/nearest", &[F64], F64, I::F64Nearest);
        self.inst("f64/abs", &[F64], F64, I::F64Abs);
        self.inst("f64.copysign", &[F64, F64], F64, I::F64Copysign);

        self.inst("i32.wrap_i64", &[I64], I32, I::I32WrapI64);
        self.inst("i64.extend_i32_s", &[I32], I64, I::I64ExtendI32S);
        self.inst("i64.extend_i32_u", &[I32], I64, I::I64ExtendI32U);

        self.inst("i32.trunc_f32_s", &[F32], I32, I::I32TruncF32S);
        self.inst("i32.trunc_f64_s", &[F64], I32, I::I32TruncF64S);
        self.inst("i64.trunc_f32_s", &[F32], I64, I::I64TruncF32S);
        self.inst("i64.trunc_f64_s", &[F64], I64, I::I64TruncF64S);

        self.inst("i32.trunc_f32_u", &[F32], I32, I::I32TruncF32U);
        self.inst("i32.trunc_f64_u", &[F64], I32, I::I32TruncF64U);
        self.inst("i64.trunc_f32_u", &[F32], I64, I::I64TruncF32U);
        self.inst("i64.trunc_f64_u", &[F64], I64, I::I64TruncF64U);

        self.inst("f32.demote_f64", &[F64], F32, I::F32DemoteF64);
        self.inst("f64.promote_f32", &[F32], F64, I::F64PromoteF32);

        self.inst("f32.convert_i32_s", &[I32], F32, I::F32ConvertI32S);
        self.inst("f32.convert_i64_s", &[I64], F32, I::F32ConvertI32S);
        self.inst("f64.convert_i32_s", &[I32], F64, I::F32ConvertI32S);
        self.inst("f64.convert_i64_s", &[I64], F64, I::F32ConvertI32S);

        self.inst("f32.convert_i32_u", &[I32], F32, I::F32ConvertI32U);
        self.inst("f32.convert_i64_u", &[I64], F32, I::F32ConvertI32U);
        self.inst("f64.convert_i32_u", &[I32], F64, I::F32ConvertI32U);
        self.inst("f64.convert_i64_u", &[I64], F64, I::F32ConvertI32U);

        self.inst("i32.reinterpret_f32", &[F32], I32, I::I32ReinterpretF32);
        self.inst("i64.reinterpret_f64", &[F64], I64, I::I64ReinterpretF64);
        self.inst("f32.reinterpret_i32", &[I32], F32, I::F32ReinterpretI32);
        self.inst("f64.reinterpret_i64", &[I64], F64, I::F64ReinterpretI64);

        self.inst("i32.extend8_s", &[I32], I32, I::I32Extend8S);
        self.inst("i32.extend16_s", &[I32], I32, I::I32Extend16S);
        self.inst("i64.extend8_s", &[I64], I64, I::I64Extend8S);
        self.inst("i64.extend16_s", &[I64], I64, I::I64Extend16S);
        self.inst("i64.extend32_s", &[I64], I64, I::I64Extend32S);

        self.inst("i32.trunc_sat_f32_s", &[F32], I32, I::I32TruncSatF32S);
        self.inst("i32.trunc_sat_f32_u", &[F32], I32, I::I32TruncSatF32U);
        self.inst("i32.trunc_sat_f64_s", &[F64], I32, I::I32TruncSatF64S);
        self.inst("i32.trunc_sat_f64_u", &[F64], I32, I::I32TruncSatF64U);
        self.inst("i64.trunc_sat_f32_s", &[F32], I64, I::I64TruncSatF32S);
        self.inst("i64.trunc_sat_f32_u", &[F32], I64, I::I64TruncSatF32U);
        self.inst("i64.trunc_sat_f64_s", &[F64], I64, I::I64TruncSatF64S);
        self.inst("i64.trunc_sat_f64_u", &[F64], I64, I::I64TruncSatF64U);
    }

    fn inst(&mut self, name: &str, params: &[Type], ret: Type, ins: enc::Instruction<'static>) {
        if let Some(slash_idx) = name.find('/') {
            self.insert(name[(slash_idx + 1)..].to_string(), params, ret, &ins);
            let mut full_name = name[..slash_idx].to_string();
            full_name.push('.');
            full_name += &name[(slash_idx + 1)..];
            self.insert(full_name, params, ret, &ins);
        } else {
            self.insert(name.to_string(), params, ret, &ins);
        }
    }

    fn insert(
        &mut self,
        name: String,
        params: &[Type],
        ret: Type,
        ins: &enc::Instruction<'static>,
    ) {
        self.0
            .entry(name)
            .or_default()
            .insert(params.to_vec(), (ret, ins.clone()));
    }
}
