use anyhow::{anyhow, bail, Result};
use std::io::prelude::*;
use std::{fs::File, path::PathBuf};

mod ast;
mod constfold;
mod emit;
mod parser;
mod typecheck;

type Span = std::ops::Range<usize>;

fn main() -> Result<()> {
    let mut filename = PathBuf::from(
        std::env::args()
            .nth(1)
            .ok_or_else(|| anyhow!("Path to .hw file missing"))?,
    );
    let mut input = String::new();
    File::open(&filename)?.read_to_string(&mut input)?;

    let mut script = match parser::parse(&input) {
        Ok(script) => script,
        Err(_) => bail!("Parse failed")
    };

    constfold::fold_script(&mut script);
    if let Err(_) = typecheck::tc_script(&mut script, &input) {
        bail!("Type check failed");
    }
    let wasm = emit::emit(&script);

    wasmparser::validate(&wasm)?;

    filename.set_extension("wasm");
    File::create(filename)?.write_all(&wasm)?;

    println!("Size of code section: {} bytes", code_section_size(&wasm)?);

    Ok(())
}

fn code_section_size(wasm: &[u8]) -> Result<usize> {
    for payload in wasmparser::Parser::new(0).parse_all(wasm) {
        match payload? {
            wasmparser::Payload::CodeSectionStart { range, .. } => {
                let size = range.end - range.start;
                let section_header_size = match size {
                    0..=127 => 2,
                    128..=16383 => 3,
                    _ => 4,
                };
                return Ok(size + section_header_size);
            }
            _ => (),
        }
    }

    bail!("No code section found");
}
