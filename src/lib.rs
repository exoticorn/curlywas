use anyhow::{bail, Result};
use std::ffi::OsStr;
use std::io::prelude::*;
use std::{fs::File, path::Path};

mod ast;
mod constfold;
mod emit;
mod includes;
mod intrinsics;
mod parser;
mod typecheck;

type Span = std::ops::Range<usize>;

#[derive(Default)]
pub struct Options {
    pub(crate) debug: bool,
}

impl Options {
    pub fn with_debug(self) -> Self {
        Options {
            debug: true,
            ..self
        }
    }
}

pub fn compile_file<P: AsRef<Path>>(path: P, options: Options) -> Result<Vec<u8>> {
    let path = path.as_ref();
    let mut input = String::new();
    File::open(path)?.read_to_string(&mut input)?;

    compile_str(&input, path, options)
}

pub fn compile_str(input: &str, path: &Path, options: Options) -> Result<Vec<u8>> {
    let mut script = match parser::parse(&input) {
        Ok(script) => script,
        Err(_) => bail!("Parse failed"),
    };

    includes::resolve_includes(&mut script, path)?;

    constfold::fold_script(&mut script);
    if let Err(_) = typecheck::tc_script(&mut script, &input) {
        bail!("Type check failed");
    }
    let wasm = emit::emit(
        &script,
        &path
            .file_stem()
            .unwrap_or_else(|| OsStr::new("unknown"))
            .to_string_lossy(),
        &options,
    );
    Ok(wasm)
}
