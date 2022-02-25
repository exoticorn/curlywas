use anyhow::{bail, Result};
use std::ffi::OsStr;
use std::path::Path;
use parser::Sources;

mod ast;
mod constfold;
mod emit;
mod includes;
mod intrinsics;
mod parser;
mod typecheck;

#[derive(Default)]
pub struct Options {
    pub(crate) debug: bool,
}

impl Options {
    pub fn with_debug(self) -> Self {
        Options { debug: true }
    }
}

pub fn compile_file<P: AsRef<Path>>(path: P, options: Options) -> Result<Vec<u8>> {
    let path = path.as_ref();

    let mut sources = Sources::new();
    let id = sources.add(path)?;
 
    let mut script = match parser::parse(&sources, id) {
        Ok(script) => script,
        Err(_) => bail!("Parse failed"),
    };

    includes::resolve_includes(&mut script, path)?;

    constfold::fold_script(&mut script);
    if typecheck::tc_script(&mut script, &sources).is_err() {
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
