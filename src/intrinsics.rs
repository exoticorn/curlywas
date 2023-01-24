use crate::ast::Type;
use enc::MemArg;
use enc::Lane;
use std::collections::HashMap;
use wasm_encoder as enc;

pub struct Intrinsics(
    HashMap<String, HashMap<Vec<Type>, (Option<Type>, enc::Instruction<'static>)>>,
);

impl Intrinsics {
    pub fn new() -> Intrinsics {
        let mut i = Intrinsics(HashMap::new());
        i.add_instructions();
        i
    }

    pub fn find_types(&self, name: &str) -> Option<HashMap<Vec<Type>, Option<Type>>> {
        self.0.get(name).map(|types| {
            types
                .iter()
                .map(|(params, (ret, _))| (params.clone(), *ret))
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
        self.inst("i32.rotl", &[I32, I32], Some(I32), I::I32Rotl);
        self.inst("i32.rotr", &[I32, I32], Some(I32), I::I32Rotr);
        self.inst("i32.clz", &[I32], Some(I32), I::I32Clz);
        self.inst("i32.ctz", &[I32], Some(I32), I::I32Ctz);
        self.inst("i32.popcnt", &[I32], Some(I32), I::I32Popcnt);

        self.inst("i64.rotl", &[I64, I64], Some(I64), I::I64Rotl);
        self.inst("i64.rotr", &[I64, I64], Some(I64), I::I64Rotr);
        self.inst("i64.clz", &[I64], Some(I64), I::I64Clz);
        self.inst("i64.ctz", &[I64], Some(I64), I::I64Ctz);
        self.inst("i64.popcnt", &[I64], Some(I64), I::I64Popcnt);

        self.inst("f32/sqrt", &[F32], Some(F32), I::F32Sqrt);
        self.inst("f32/min", &[F32, F32], Some(F32), I::F32Min);
        self.inst("f32/max", &[F32, F32], Some(F32), I::F32Max);
        self.inst("f32/ceil", &[F32], Some(F32), I::F32Ceil);
        self.inst("f32/floor", &[F32], Some(F32), I::F32Floor);
        self.inst("f32/trunc", &[F32], Some(F32), I::F32Trunc);
        self.inst("f32/nearest", &[F32], Some(F32), I::F32Nearest);
        self.inst("f32/abs", &[F32], Some(F32), I::F32Abs);
        self.inst("f32.copysign", &[F32, F32], Some(F32), I::F32Copysign);

        self.inst("f64/sqrt", &[F64], Some(F64), I::F64Sqrt);
        self.inst("f64/min", &[F64, F64], Some(F64), I::F64Min);
        self.inst("f64/max", &[F64, F64], Some(F64), I::F64Max);
        self.inst("f64/ceil", &[F64], Some(F64), I::F64Ceil);
        self.inst("f64/floor", &[F64], Some(F64), I::F64Floor);
        self.inst("f64/trunc", &[F64], Some(F64), I::F64Trunc);
        self.inst("f64/nearest", &[F64], Some(F64), I::F64Nearest);
        self.inst("f64/abs", &[F64], Some(F64), I::F64Abs);
        self.inst("f64.copysign", &[F64, F64], Some(F64), I::F64Copysign);

        self.inst("i32.wrap_i64", &[I64], Some(I32), I::I32WrapI64);
        self.inst("i64.extend_i32_s", &[I32], Some(I64), I::I64ExtendI32S);
        self.inst("i64.extend_i32_u", &[I32], Some(I64), I::I64ExtendI32U);

        self.inst("i32.trunc_f32_s", &[F32], Some(I32), I::I32TruncF32S);
        self.inst("i32.trunc_f64_s", &[F64], Some(I32), I::I32TruncF64S);
        self.inst("i64.trunc_f32_s", &[F32], Some(I64), I::I64TruncF32S);
        self.inst("i64.trunc_f64_s", &[F64], Some(I64), I::I64TruncF64S);

        self.inst("i32.trunc_f32_u", &[F32], Some(I32), I::I32TruncF32U);
        self.inst("i32.trunc_f64_u", &[F64], Some(I32), I::I32TruncF64U);
        self.inst("i64.trunc_f32_u", &[F32], Some(I64), I::I64TruncF32U);
        self.inst("i64.trunc_f64_u", &[F64], Some(I64), I::I64TruncF64U);

        self.inst("f32.demote_f64", &[F64], Some(F32), I::F32DemoteF64);
        self.inst("f64.promote_f32", &[F32], Some(F64), I::F64PromoteF32);

        self.inst("f32.convert_i32_s", &[I32], Some(F32), I::F32ConvertI32S);
        self.inst("f32.convert_i64_s", &[I64], Some(F32), I::F32ConvertI32S);
        self.inst("f64.convert_i32_s", &[I32], Some(F64), I::F32ConvertI32S);
        self.inst("f64.convert_i64_s", &[I64], Some(F64), I::F32ConvertI32S);

        self.inst("f32.convert_i32_u", &[I32], Some(F32), I::F32ConvertI32U);
        self.inst("f32.convert_i64_u", &[I64], Some(F32), I::F32ConvertI32U);
        self.inst("f64.convert_i32_u", &[I32], Some(F64), I::F32ConvertI32U);
        self.inst("f64.convert_i64_u", &[I64], Some(F64), I::F32ConvertI32U);

        self.inst(
            "i32.reinterpret_f32",
            &[F32],
            Some(I32),
            I::I32ReinterpretF32,
        );
        self.inst(
            "i64.reinterpret_f64",
            &[F64],
            Some(I64),
            I::I64ReinterpretF64,
        );
        self.inst(
            "f32.reinterpret_i32",
            &[I32],
            Some(F32),
            I::F32ReinterpretI32,
        );
        self.inst(
            "f64.reinterpret_i64",
            &[I64],
            Some(F64),
            I::F64ReinterpretI64,
        );

        self.inst("i32.extend8_s", &[I32], Some(I32), I::I32Extend8S);
        self.inst("i32.extend16_s", &[I32], Some(I32), I::I32Extend16S);
        self.inst("i64.extend8_s", &[I64], Some(I64), I::I64Extend8S);
        self.inst("i64.extend16_s", &[I64], Some(I64), I::I64Extend16S);
        self.inst("i64.extend32_s", &[I64], Some(I64), I::I64Extend32S);

        self.inst("i32.trunc_sat_f32_s", &[F32], Some(I32), I::I32TruncSatF32S);
        self.inst("i32.trunc_sat_f32_u", &[F32], Some(I32), I::I32TruncSatF32U);
        self.inst("i32.trunc_sat_f64_s", &[F64], Some(I32), I::I32TruncSatF64S);
        self.inst("i32.trunc_sat_f64_u", &[F64], Some(I32), I::I32TruncSatF64U);
        self.inst("i64.trunc_sat_f32_s", &[F32], Some(I64), I::I64TruncSatF32S);
        self.inst("i64.trunc_sat_f32_u", &[F32], Some(I64), I::I64TruncSatF32U);
        self.inst("i64.trunc_sat_f64_s", &[F64], Some(I64), I::I64TruncSatF64S);
        self.inst("i64.trunc_sat_f64_u", &[F64], Some(I64), I::I64TruncSatF64U);

        self.inst("i8x16.splat", &[I32], Some(V128), I::I8x16Splat);
        self.inst("i16x8.splat", &[I32], Some(V128), I::I16x8Splat);
        self.inst("i32x4.splat", &[I32], Some(V128), I::I32x4Splat);
        self.inst("i64x2.splat", &[I64], Some(V128), I::I64x2Splat);
        self.inst("f32x4.splat", &[F32], Some(V128), I::F32x4Splat);
        self.inst("f64x2.splat", &[F64], Some(V128), I::F64x2Splat);

        // skipped: i8x16.shuffle (requires special handling for 16 literal lane values)
        self.inst("i8x16.swizzle", &[V128, V128], Some(V128), I::I8x16Swizzle);

        self.inst("i8x16.add", &[V128, V128], Some(V128), I::I8x16Add);
        self.inst("i16x8.add", &[V128, V128], Some(V128), I::I16x8Add);
        self.inst("i32x4.add", &[V128, V128], Some(V128), I::I32x4Add);
        self.inst("i64x2.add", &[V128, V128], Some(V128), I::I64x2Add);
        self.inst("f32x4.add", &[V128, V128], Some(V128), I::F32x4Add);
        self.inst("f64x2.add", &[V128, V128], Some(V128), I::F64x2Add);

        self.inst("i8x16.sub", &[V128, V128], Some(V128), I::I8x16Sub);
        self.inst("i16x8.sub", &[V128, V128], Some(V128), I::I16x8Sub);
        self.inst("i32x4.sub", &[V128, V128], Some(V128), I::I32x4Sub);
        self.inst("i64x2.sub", &[V128, V128], Some(V128), I::I64x2Sub);
        self.inst("f32x4.sub", &[V128, V128], Some(V128), I::F32x4Sub);
        self.inst("f64x2.sub", &[V128, V128], Some(V128), I::F64x2Sub);

        self.inst("i16x8.mul", &[V128, V128], Some(V128), I::I16x8Mul);
        self.inst("i32x4.mul", &[V128, V128], Some(V128), I::I32x4Mul);
        self.inst("i64x2.mul", &[V128, V128], Some(V128), I::I64x2Mul);
        self.inst("f32x4.mul", &[V128, V128], Some(V128), I::F32x4Mul);
        self.inst("f64x2.mul", &[V128, V128], Some(V128), I::F64x2Mul);

        self.inst("i32x4.dot_i16x8_s", &[V128, V128], Some(V128), I::I32x4DotI16x8S);

        self.inst("i8x16.neg", &[V128, V128], Some(V128), I::I8x16Neg);
        self.inst("i16x8.neg", &[V128, V128], Some(V128), I::I16x8Neg);
        self.inst("i32x4.neg", &[V128, V128], Some(V128), I::I32x4Neg);
        self.inst("i64x2.neg", &[V128, V128], Some(V128), I::I64x2Neg);
        self.inst("f32x4.neg", &[V128, V128], Some(V128), I::F32x4Neg);
        self.inst("f64x2.neg", &[V128, V128], Some(V128), I::F64x2Neg);

        self.inst("i16x8.extmul_low_i8x16_s", &[V128, V128], Some(V128), I::I16x8ExtMulLowI8x16S);
        self.inst("i16x8.extmul_high_i8x16_s", &[V128, V128], Some(V128), I::I16x8ExtMulHighI8x16S);
        self.inst("i16x8.extmul_low_i8x16_u", &[V128, V128], Some(V128), I::I16x8ExtMulLowI8x16U);
        self.inst("i16x8.extmul_high_i8x16_u", &[V128, V128], Some(V128), I::I16x8ExtMulHighI8x16U);
        self.inst("i32x4.extmul_low_i16x8_s", &[V128, V128], Some(V128), I::I32x4ExtMulLowI16x8S);
        self.inst("i32x4.extmul_high_i16x8_s", &[V128, V128], Some(V128), I::I32x4ExtMulHighI16x8S);
        self.inst("i32x4.extmul_low_i16x8_u", &[V128, V128], Some(V128), I::I32x4ExtMulLowI16x8U);
        self.inst("i32x4.extmul_high_i16x8_u", &[V128, V128], Some(V128), I::I32x4ExtMulHighI16x8U);
        self.inst("i64x2.extmul_low_i32x4_s", &[V128, V128], Some(V128), I::I64x2ExtMulLowI32x4S);
        self.inst("i64x2.extmul_high_i32x4_s", &[V128, V128], Some(V128), I::I64x2ExtMulHighI32x4S);
        self.inst("i64x2.extmul_low_i32x4_u", &[V128, V128], Some(V128), I::I64x2ExtMulLowI32x4U);
        self.inst("i64x2.extmul_high_i32x4_u", &[V128, V128], Some(V128), I::I64x2ExtMulHighI32x4U);

        self.inst("i16x8.extadd_pairwise_i8x16_s", &[V128], Some(V128), I::I16x8ExtAddPairwiseI8x16S);
        self.inst("i16x8.extadd_pairwise_i8x16_u", &[V128], Some(V128), I::I16x8ExtAddPairwiseI8x16U);
        self.inst("i32x4.extadd_pairwise_i16x8_s", &[V128], Some(V128), I::I32x4ExtAddPairwiseI16x8S);
        self.inst("i32x4.extadd_pairwise_i16x8_u", &[V128], Some(V128), I::I32x4ExtAddPairwiseI16x8U);

        self.inst("i8x16.add_sat_s", &[V128, V128], Some(V128), I::I16x8AddSatS);
        self.inst("i8x16.add_sat_u", &[V128, V128], Some(V128), I::I16x8AddSatU);
        self.inst("i16x8.add_sat_s", &[V128, V128], Some(V128), I::I16x8AddSatS);
        self.inst("i16x8.add_sat_u", &[V128, V128], Some(V128), I::I16x8AddSatU);

        self.inst("i8x16.sub_sat_s", &[V128, V128], Some(V128), I::I16x8SubSatS);
        self.inst("i8x16.sub_sat_u", &[V128, V128], Some(V128), I::I16x8SubSatU);
        self.inst("i16x8.sub_sat_s", &[V128, V128], Some(V128), I::I16x8SubSatS);
        self.inst("i16x8.sub_sat_u", &[V128, V128], Some(V128), I::I16x8SubSatU);

        self.inst("i16x8.q15mulr_sat_s", &[V128, V128], Some(V128), I::I16x8Q15MulrSatS);

        self.inst("i8x16.min_s", &[V128, V128], Some(V128), I::I8x16MinS);
        self.inst("i8x16.min_u", &[V128, V128], Some(V128), I::I8x16MinU);
        self.inst("i16x8.min_s", &[V128, V128], Some(V128), I::I16x8MinS);
        self.inst("i16x8.min_u", &[V128, V128], Some(V128), I::I16x8MinU);
        self.inst("i32x4.min_s", &[V128, V128], Some(V128), I::I32x4MinS);
        self.inst("i32x4.min_u", &[V128, V128], Some(V128), I::I32x4MinU);
        self.inst("f32x4.min", &[V128, V128], Some(V128), I::F32x4Min);
        self.inst("f64x2.min", &[V128, V128], Some(V128), I::F64x2Min);
        self.inst("f32x4.pmin", &[V128, V128], Some(V128), I::F32x4PMin);
        self.inst("f64x2.pmin", &[V128, V128], Some(V128), I::F64x2PMin);

        self.inst("i8x16.max_s", &[V128, V128], Some(V128), I::I8x16MaxS);
        self.inst("i8x16.max_u", &[V128, V128], Some(V128), I::I8x16MaxU);
        self.inst("i16x8.max_s", &[V128, V128], Some(V128), I::I16x8MaxS);
        self.inst("i16x8.max_u", &[V128, V128], Some(V128), I::I16x8MaxU);
        self.inst("i32x4.max_s", &[V128, V128], Some(V128), I::I32x4MaxS);
        self.inst("i32x4.max_u", &[V128, V128], Some(V128), I::I32x4MaxU);
        self.inst("f32x4.max", &[V128, V128], Some(V128), I::F32x4Max);
        self.inst("f64x2.max", &[V128, V128], Some(V128), I::F64x2Max);
        self.inst("f32x4.pmax", &[V128, V128], Some(V128), I::F32x4PMax);
        self.inst("f64x2.pmax", &[V128, V128], Some(V128), I::F64x2PMax);

        self.inst("i8x16.avgr_u", &[V128, V128], Some(V128), I::I8x16RoundingAverageU);
        self.inst("i16x8.avgr_u", &[V128, V128], Some(V128), I::I16x8RoundingAverageU);

        self.inst("i8x16.abs", &[V128], Some(V128), I::I8x16Abs);
        self.inst("i16x8.abs", &[V128], Some(V128), I::I16x8Abs);
        self.inst("i32x4.abs", &[V128], Some(V128), I::I32x4Abs);
        self.inst("i64x2.abs", &[V128], Some(V128), I::I64x2Abs);
        self.inst("f32x4.abs", &[V128], Some(V128), I::F32x4Abs);
        self.inst("f64x2.abs", &[V128], Some(V128), I::F64x2Abs);

        self.inst("i8x16.shl", &[V128, I32], Some(V128), I::I8x16Shl);
        self.inst("i16x8.shl", &[V128, I32], Some(V128), I::I16x8Shl);
        self.inst("i32x4.shl", &[V128, I32], Some(V128), I::I32x4Shl);
        self.inst("i64x2.shl", &[V128, I32], Some(V128), I::I64x2Shl);

        self.inst("i8x16.shr_s", &[V128, I32], Some(V128), I::I8x16ShrS);
        self.inst("i8x16.shr_u", &[V128, I32], Some(V128), I::I8x16ShrU);
        self.inst("i16x8.shr_s", &[V128, I32], Some(V128), I::I16x8ShrS);
        self.inst("i16x8.shr_u", &[V128, I32], Some(V128), I::I16x8ShrU);
        self.inst("i32x4.shr_s", &[V128, I32], Some(V128), I::I32x4ShrS);
        self.inst("i32x4.shr_u", &[V128, I32], Some(V128), I::I32x4ShrU);
        self.inst("i64x2.shr_s", &[V128, I32], Some(V128), I::I64x2ShrS);
        self.inst("i64x2.shr_u", &[V128, I32], Some(V128), I::I64x2ShrU);

        self.inst("v128.and", &[V128, V128], Some(V128), I::V128Not);
        self.inst("v128.or", &[V128, V128], Some(V128), I::V128Or);
        self.inst("v128.xor", &[V128, V128], Some(V128), I::V128Xor);
        self.inst("v128.not", &[V128], Some(V128), I::V128Not);
        self.inst("v128.andnot", &[V128, V128], Some(V128), I::V128AndNot);

        self.inst("v128.bitselect", &[V128, V128, V128], Some(V128), I::V128Bitselect);

        self.inst("i8x16.popcnt", &[V128], Some(V128), I::I8x16Popcnt);

        self.inst("v128.any_true", &[V128], Some(I32), I::V128AnyTrue);
        self.inst("i8x16.all_true", &[V128], Some(I32), I::I8x16AllTrue);
        self.inst("i16x8.all_true", &[V128], Some(I32), I::I16x8AllTrue);
        self.inst("i32x4.all_true", &[V128], Some(I32), I::I32x4AllTrue);
        self.inst("i64x2.all_true", &[V128], Some(I32), I::I64x2AllTrue);

        self.inst("i8x16.bitmask", &[V128], Some(I32), I::I8x16Bitmask);
        self.inst("i16x8.bitmask", &[V128], Some(I32), I::I16x8Bitmask);
        self.inst("i32x4.bitmask", &[V128], Some(I32), I::I32x4Bitmask);
        self.inst("i64x2.bitmask", &[V128], Some(I32), I::I64x2Bitmask);

        self.inst("i8x16.eq", &[V128, V128], Some(V128), I::I8x16Eq);
        self.inst("i16x8.eq", &[V128, V128], Some(V128), I::I16x8Eq);
        self.inst("i32x4.eq", &[V128, V128], Some(V128), I::I32x4Eq);
        self.inst("i64x2.eq", &[V128, V128], Some(V128), I::I64x2Eq);
        self.inst("f32x4.eq", &[V128, V128], Some(V128), I::F32x4Eq);
        self.inst("f64x2.eq", &[V128, V128], Some(V128), I::F64x2Eq);

        self.inst("i8x16.ne", &[V128, V128], Some(V128), I::I8x16Ne);
        self.inst("i16x8.ne", &[V128, V128], Some(V128), I::I16x8Ne);
        self.inst("i32x4.ne", &[V128, V128], Some(V128), I::I32x4Ne);
        self.inst("i64x2.ne", &[V128, V128], Some(V128), I::I64x2Ne);
        self.inst("f32x4.ne", &[V128, V128], Some(V128), I::F32x4Ne);
        self.inst("f64x2.ne", &[V128, V128], Some(V128), I::F64x2Ne);

        self.inst("i8x16.lt_s", &[V128, V128], Some(V128), I::I8x16LtS);
        self.inst("i8x16.lt_u", &[V128, V128], Some(V128), I::I8x16LtU);
        self.inst("i16x8.lt_s", &[V128, V128], Some(V128), I::I16x8LtS);
        self.inst("i16x8.lt_u", &[V128, V128], Some(V128), I::I16x8LtU);
        self.inst("i32x4.lt_s", &[V128, V128], Some(V128), I::I32x4LtS);
        self.inst("i32x4.lt_u", &[V128, V128], Some(V128), I::I32x4LtU);
        self.inst("f32x4.lt", &[V128, V128], Some(V128), I::F32x4Lt);
        self.inst("f64x2.lt", &[V128, V128], Some(V128), I::F64x2Lt);

        self.inst("i8x16.le_s", &[V128, V128], Some(V128), I::I8x16LeS);
        self.inst("i8x16.le_u", &[V128, V128], Some(V128), I::I8x16LeU);
        self.inst("i16x8.le_s", &[V128, V128], Some(V128), I::I16x8LeS);
        self.inst("i16x8.le_u", &[V128, V128], Some(V128), I::I16x8LeU);
        self.inst("i32x4.le_s", &[V128, V128], Some(V128), I::I32x4LeS);
        self.inst("i32x4.le_u", &[V128, V128], Some(V128), I::I32x4LeU);
        self.inst("f32x4.le", &[V128, V128], Some(V128), I::F32x4Le);
        self.inst("f64x2.le", &[V128, V128], Some(V128), I::F64x2Le);

        self.inst("i8x16.gt_s", &[V128, V128], Some(V128), I::I8x16GtS);
        self.inst("i8x16.gt_u", &[V128, V128], Some(V128), I::I8x16GtU);
        self.inst("i16x8.gt_s", &[V128, V128], Some(V128), I::I16x8GtS);
        self.inst("i16x8.gt_u", &[V128, V128], Some(V128), I::I16x8GtU);
        self.inst("i32x4.gt_s", &[V128, V128], Some(V128), I::I32x4GtS);
        self.inst("i32x4.gt_u", &[V128, V128], Some(V128), I::I32x4GtU);
        self.inst("f32x4.gt", &[V128, V128], Some(V128), I::F32x4Gt);
        self.inst("f64x2.gt", &[V128, V128], Some(V128), I::F64x2Gt);

        self.inst("i8x16.ge_s", &[V128, V128], Some(V128), I::I8x16GeS);
        self.inst("i8x16.ge_u", &[V128, V128], Some(V128), I::I8x16GeU);
        self.inst("i16x8.ge_s", &[V128, V128], Some(V128), I::I16x8GeS);
        self.inst("i16x8.ge_u", &[V128, V128], Some(V128), I::I16x8GeU);
        self.inst("i32x4.ge_s", &[V128, V128], Some(V128), I::I32x4GeS);
        self.inst("i32x4.ge_u", &[V128, V128], Some(V128), I::I32x4GeU);
        self.inst("f32x4.ge", &[V128, V128], Some(V128), I::F32x4Ge);
        self.inst("f64x2.ge", &[V128, V128], Some(V128), I::F64x2Ge);

        self.inst("f32x4.div", &[V128, V128], Some(V128), I::F32x4Div);
        self.inst("f64x2.div", &[V128, V128], Some(V128), I::F64x2Div);

        self.inst("f32x4.sqrt", &[V128], Some(V128), I::F32x4Sqrt);
        self.inst("f64x2.sqrt", &[V128], Some(V128), I::F64x2Sqrt);

        self.inst("f32x4.ceil", &[V128], Some(V128), I::F32x4Ceil);
        self.inst("f64x2.ceil", &[V128], Some(V128), I::F64x2Ceil);
        self.inst("f32x4.floor", &[V128], Some(V128), I::F32x4Floor);
        self.inst("f64x2.floor", &[V128], Some(V128), I::F64x2Floor);
        self.inst("f32x4.trunc", &[V128], Some(V128), I::F32x4Trunc);
        self.inst("f64x2.trunc", &[V128], Some(V128), I::F64x2Trunc);
        self.inst("f32x4.nearest", &[V128], Some(V128), I::F32x4Nearest);
        self.inst("f64x2.nearest", &[V128], Some(V128), I::F64x2Nearest);

        self.inst("f32x4.convert_i32x4_s", &[V128], Some(V128), I::F32x4ConvertI32x4S);
        self.inst("f32x4.convert_i32x4_u", &[V128], Some(V128), I::F32x4ConvertI32x4U);

        self.inst("f64x2.convert_low_i32x4_s", &[V128], Some(V128), I::F64x2ConvertLowI32x4S);
        self.inst("f64x2.convert_low_i32x4_u", &[V128], Some(V128), I::F64x2ConvertLowI32x4U);

        self.inst("i32x4.trunc_sat_f32x4_s", &[V128], Some(V128), I::I32x4TruncSatF32x4S);
        self.inst("i32x4.trunc_sat_f32x4_u", &[V128], Some(V128), I::I32x4TruncSatF32x4U);

        self.inst("i32x4.trunc_sat_f64x2_s_zero", &[V128], Some(V128), I::I32x4TruncSatF64x2SZero);
        self.inst("i32x4.trunc_sat_f64x2_u_zero", &[V128], Some(V128), I::I32x4TruncSatF64x2UZero);

        self.inst("f32x4.demote_f64x2_zero", &[V128], Some(V128), I::F32x4DemoteF64x2Zero);
        self.inst("f64x2.promote_low_f32x4", &[V128], Some(V128), I::F64x2PromoteLowF32x4);

        self.inst("i8x16.narrow_i16x8_s", &[V128, V128], Some(V128), I::I8x16NarrowI16x8S);
        self.inst("i8x16.narrow_i16x8_u", &[V128, V128], Some(V128), I::I8x16NarrowI16x8U);
        self.inst("i16x8.narrow_i32x4_s", &[V128, V128], Some(V128), I::I16x8NarrowI32x4S);
        self.inst("i16x8.narrow_i32x4_u", &[V128, V128], Some(V128), I::I16x8NarrowI32x4U);

        self.inst("i16x8.extend_low_i8x16_s", &[V128], Some(V128), I::I16x8ExtendLowI8x16S);
        self.inst("i16x8.extend_high_i8x16_s", &[V128], Some(V128), I::I16x8ExtendHighI8x16S);
        self.inst("i16x8.extend_low_i8x16_u", &[V128], Some(V128), I::I16x8ExtendLowI8x16U);
        self.inst("i16x8.extend_high_i8x16_u", &[V128], Some(V128), I::I16x8ExtendHighI8x16U);
        self.inst("i32x4.extend_low_i16x8_s", &[V128], Some(V128), I::I32x4ExtendLowI16x8S);
        self.inst("i32x4.extend_high_i16x8_s", &[V128], Some(V128), I::I32x4ExtendHighI16x8S);
        self.inst("i32x4.extend_low_i16x8_u", &[V128], Some(V128), I::I32x4ExtendLowI16x8U);
        self.inst("i32x4.extend_high_i16x8_u", &[V128], Some(V128), I::I32x4ExtendHighI16x8U);
        self.inst("i64x2.extend_low_i32x4_s", &[V128], Some(V128), I::I64x2ExtendLowI32x4S);
        self.inst("i64x2.extend_high_i32x4_s", &[V128], Some(V128), I::I64x2ExtendHighI32x4S);
        self.inst("i64x2.extend_low_i32x4_u", &[V128], Some(V128), I::I64x2ExtendLowI32x4U);
        self.inst("i64x2.extend_high_i32x4_u", &[V128], Some(V128), I::I64x2ExtendHighI32x4U);

        self.inst(
            "memory.copy",
            &[I32, I32, I32],
            None,
            I::MemoryCopy { src: 0, dst: 0 },
        );
        self.inst("memory.fill", &[I32, I32, I32], None, I::MemoryFill(0));
    }

    fn inst(
        &mut self,
        name: &str,
        params: &[Type],
        ret: Option<Type>,
        ins: enc::Instruction<'static>,
    ) {
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
        ret: Option<Type>,
        ins: &enc::Instruction<'static>,
    ) {
        self.0
            .entry(name)
            .or_default()
            .insert(params.to_vec(), (ret, ins.clone()));
    }

    pub fn find_load(&self, name: &str) -> Option<MemInstruction> {
        use enc::Instruction as I;
        use Type::*;
        let ins = match name {
            "i32.load" => MemInstruction::new(I32, I::I32Load, 2),
            "i32.load8_s" => MemInstruction::new(I32, I::I32Load8_S, 0),
            "i32.load8_u" => MemInstruction::new(I32, I::I32Load8_U, 0),
            "i32.load16_s" => MemInstruction::new(I32, I::I32Load16_S, 1),
            "i32.load16_u" => MemInstruction::new(I32, I::I32Load16_U, 1),
            "i64.load" => MemInstruction::new(I64, I::I64Load, 3),
            "i64.load8_s" => MemInstruction::new(I64, I::I64Load8_S, 0),
            "i64.load8_u" => MemInstruction::new(I64, I::I64Load8_U, 0),
            "i64.load16_s" => MemInstruction::new(I64, I::I64Load16_S, 1),
            "i64.load16_u" => MemInstruction::new(I64, I::I64Load16_U, 1),
            "i64.load32_s" => MemInstruction::new(I64, I::I64Load32_S, 2),
            "i64.load32_u" => MemInstruction::new(I64, I::I64Load32_U, 2),
            "f32.load" => MemInstruction::new(F32, I::F32Load, 2),
            "f64.load" => MemInstruction::new(F64, I::F64Load, 3),
            "v128.load" => MemInstruction::new(V128, |memarg| I::V128Load { memarg: memarg }, 4),
            "v128.load8x8_s" => MemInstruction::new(V128, |memarg| I::V128Load8x8S { memarg: memarg }, 3),
            "v128.load8x8_u" => MemInstruction::new(V128, |memarg| I::V128Load8x8U { memarg: memarg }, 3),
            "v128.load16x4_s" => MemInstruction::new(V128, |memarg| I::V128Load16x4S { memarg: memarg }, 3),
            "v128.load16x4_u" => MemInstruction::new(V128, |memarg| I::V128Load16x4U { memarg: memarg }, 3),
            "v128.load32x2_s" => MemInstruction::new(V128, |memarg| I::V128Load32x2S { memarg: memarg }, 3),
            "v128.load32x2_u" => MemInstruction::new(V128, |memarg| I::V128Load32x2U { memarg: memarg }, 3),
            "v128.load8_splat" => MemInstruction::new(V128, |memarg| I::V128Load8Splat { memarg: memarg }, 0),
            "v128.load16_splat" => MemInstruction::new(V128, |memarg| I::V128Load16Splat { memarg: memarg }, 1),
            "v128.load32_splat" => MemInstruction::new(V128, |memarg| I::V128Load32Splat { memarg: memarg }, 2),
            "v128.load64_splat" => MemInstruction::new(V128, |memarg| I::V128Load64Splat { memarg: memarg }, 3),
            "v128.load32_zero" => MemInstruction::new(V128, |memarg| I::V128Load32Zero { memarg: memarg }, 2),
            "v128.load64_zero" => MemInstruction::new(V128, |memarg| I::V128Load64Zero { memarg: memarg }, 3),
            _ => return None,
        };
        return Some(ins);
    }

    pub fn find_load_lane(&self, name: &str) -> Option<MemLaneInstruction> {
        use enc::Instruction as I;
        let ins = match name {
            "v128.load8_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Load8Lane { memarg: memarg, lane: lane }, 0),
            "v128.load16_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Load16Lane { memarg: memarg, lane: lane }, 1),
            "v128.load32_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Load32Lane { memarg: memarg, lane: lane }, 2),
            "v128.load64_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Load64Lane { memarg: memarg, lane: lane }, 3),
            _ => return None,
        };
        return Some(ins);
    }

    pub fn find_store(&self, name: &str) -> Option<MemInstruction> {
        use enc::Instruction as I;
        use Type::*;
        let ins = match name {
            "i32.store" => MemInstruction::new(I32, I::I32Store, 2),
            "i32.store8" => MemInstruction::new(I32, I::I32Store8, 0),
            "i32.store16" => MemInstruction::new(I32, I::I32Store16, 1),
            "i64.store" => MemInstruction::new(I64, I::I64Store, 3),
            "i64.store8" => MemInstruction::new(I64, I::I64Store8, 0),
            "i64.store16" => MemInstruction::new(I64, I::I64Store16, 1),
            "i64.store32" => MemInstruction::new(I64, I::I64Store32, 2),
            "f32.store" => MemInstruction::new(F32, I::F32Store, 2),
            "f64.store" => MemInstruction::new(F64, I::F64Store, 3),
            "v128.store" => MemInstruction::new(V128, |memarg| I::V128Store { memarg: memarg }, 4),
            _ => return None,
        };
        return Some(ins);
    }

    pub fn find_store_lane(&self, name: &str) -> Option<MemLaneInstruction> {
        use enc::Instruction as I;
        let ins = match name {
            "v128.store8_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Store8Lane { memarg: memarg, lane: lane }, 0),
            "v128.store16_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Store16Lane { memarg: memarg, lane: lane }, 1),
            "v128.store32_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Store32Lane { memarg: memarg, lane: lane }, 2),
            "v128.store64_lane" => MemLaneInstruction::new(|memarg, lane| I::V128Store64Lane { memarg: memarg, lane: lane }, 3),
            _ => return None,
        };
        return Some(ins);
    }

    pub fn find_lane(&self, name: &str) -> Option<LaneInstruction> {
        use enc::Instruction as I;
        use Type::*;
        let ins = match name {
            "i8x16.extract_lane_s" => LaneInstruction::new(None, I32, |lane| I::I8x16ExtractLaneS { lane: lane }),
            "i8x16.extract_lane_u" => LaneInstruction::new(None, I32, |lane| I::I8x16ExtractLaneU { lane: lane }),
            "i8x16.replace_lane" => LaneInstruction::new(Some(I32), V128, |lane| I::I8x16ReplaceLane { lane: lane }),
            "i16x8.extract_lane_s" => LaneInstruction::new(None, I32, |lane| I::I16x8ExtractLaneS { lane: lane }),
            "i16x8.extract_lane_u" => LaneInstruction::new(None, I32, |lane| I::I16x8ExtractLaneU { lane: lane }),
            "i16x8.replace_lane" => LaneInstruction::new(Some(I32), V128, |lane| I::I16x8ReplaceLane { lane: lane }),
            "i32x4.extract_lane" => LaneInstruction::new(None, I32, |lane| I::I32x4ExtractLane { lane: lane }),
            "i32x4.replace_lane" => LaneInstruction::new(Some(I32), V128, |lane| I::I32x4ReplaceLane { lane: lane }),
            "i64x2.extract_lane" => LaneInstruction::new(None, I64, |lane| I::I64x2ExtractLane { lane: lane }),
            "i64x2.replace_lane" => LaneInstruction::new(Some(I64), V128, |lane| I::I64x2ReplaceLane { lane: lane }),
            "f32x4.extract_lane" => LaneInstruction::new(None, F64, |lane| I::F32x4ExtractLane { lane: lane }),
            "f32x4.replace_lane" => LaneInstruction::new(Some(F64), V128, |lane| I::F32x4ReplaceLane { lane: lane }),
            "f64x2.extract_lane" => LaneInstruction::new(None, F64, |lane| I::F64x2ExtractLane { lane: lane }),
            "f64x2.replace_lane" => LaneInstruction::new(Some(F64), V128, |lane| I::F64x2ReplaceLane { lane: lane }),
            _ => return None,
        };
        return Some(ins);
    }
}

pub struct MemInstruction {
    pub type_: Type,
    pub instruction: fn(MemArg) -> enc::Instruction<'static>,
    pub natural_alignment: u32,
}

impl MemInstruction {
    fn new(
        type_: Type,
        instruction: fn(MemArg) -> enc::Instruction<'static>,
        natural_alignment: u32,
    ) -> MemInstruction {
        MemInstruction {
            type_,
            instruction,
            natural_alignment,
        }
    }
}

pub struct MemLaneInstruction {
    pub instruction: fn(MemArg, Lane) -> enc::Instruction<'static>,
    pub natural_alignment: u32,
}

impl MemLaneInstruction {
    fn new(
        instruction: fn(MemArg, Lane) -> enc::Instruction<'static>,
        natural_alignment: u32,
    ) -> MemLaneInstruction {
        MemLaneInstruction {
            instruction,
            natural_alignment,
        }
    }
}

pub struct LaneInstruction {
    pub param_type: Option<Type>,
    pub return_type: Type,
    pub instruction: fn(Lane) -> enc::Instruction<'static>,
}

impl LaneInstruction {
    fn new(
        param_type: Option<Type>,
        return_type: Type,
        instruction: fn(Lane) -> enc::Instruction<'static>,
    ) -> LaneInstruction {
        LaneInstruction {
            param_type,
            return_type,
            instruction,
        }
    }
}
