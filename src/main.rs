use anyhow::{anyhow, Result};
use std::io::prelude::*;
use std::{fs::File, path::PathBuf};

use curlywas::compile_file;

fn main() -> Result<()> {
    let mut filename = PathBuf::from(
        std::env::args()
            .nth(1)
            .ok_or_else(|| anyhow!("Path to .hw file missing"))?,
    );

    let wasm = compile_file(&filename)?;

    wasmparser::validate(&wasm)?;

    filename.set_extension("wasm");
    File::create(filename)?.write_all(&wasm)?;

    Ok(())
}
