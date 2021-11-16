use anyhow::{bail, Result};
use std::io::prelude::*;
use std::{fs::File, path::Path};

mod ast;
mod constfold;
mod emit;
mod intrinsics;
mod parser;
mod typecheck;

type Span = std::ops::Range<usize>;

pub fn compile_file<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    let mut input = String::new();
    File::open(path)?.read_to_string(&mut input)?;

    compile_str(&input)
}

pub fn compile_str(input: &str) -> Result<Vec<u8>> {
    let mut script = match parser::parse(&input) {
        Ok(script) => script,
        Err(_) => bail!("Parse failed"),
    };

    constfold::fold_script(&mut script);
    if let Err(_) = typecheck::tc_script(&mut script, &input) {
        bail!("Type check failed");
    }
    let wasm = emit::emit(&script);
    Ok(wasm)
}
