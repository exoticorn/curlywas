use anyhow::Result;
use std::io::prelude::*;
use std::{fs::File, path::PathBuf};

use curlywas::{compile_file, Options};

fn main() -> Result<()> {
    let mut args = pico_args::Arguments::from_env();

    let mut options = Options::default();

    if args.contains(["-d", "--debug"]) {
        options = options.with_debug();
    }

    let mut filename = args.free_from_os_str::<PathBuf, bool>(|s| Ok(s.into()))?;

    let wasm = compile_file(&filename, options)?;

    wasmparser::validate(&wasm)?;

    filename.set_extension("wasm");
    File::create(filename)?.write_all(&wasm)?;

    Ok(())
}
