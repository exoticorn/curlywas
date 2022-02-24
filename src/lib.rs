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
    let mut script = ast::Script::default();

    let mut sources = Sources::new();

    let mut pending_files = vec![(path.to_path_buf(), None)];
    while let Some((path, span)) = pending_files.pop() {
        match sources.add(&path) {
            Ok((id, true)) => {
                let mut new_script = match parser::parse(&sources, id) {
                    Ok(script) => script,
                    Err(_) => bail!("Parse failed"),
                };
            
                includes::resolve_includes(&mut new_script, &path)?;
    
                for include in std::mem::take(&mut new_script.includes) {
                    let mut path = path.parent().expect("Script path has no parent").to_path_buf();
                    path.push(include.path);
                    pending_files.push((path, Some(include.span)));
                }
    
                script.merge(new_script);
            }
            Ok((_, false)) => (), // already parsed this include
            Err(err) => {
                if let Some(span) = span {
                    let _ = typecheck::report_error(&err.to_string(), &span, &sources);
                } else {
                    eprintln!("Failed to load script {}: {}", path.display(), err);
                }
                bail!("Parse failed");
            }
        }
    }


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
