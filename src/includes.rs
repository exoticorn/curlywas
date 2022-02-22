use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use crate::ast;
use anyhow::{anyhow, Result};

pub fn resolve_includes(script: &mut ast::Script, path: &Path) -> Result<()> {
    let script_dir = path.parent().expect("Script path has no parent");
    for data in &mut script.data {
        for values in &mut data.data {
            if let ast::DataValues::File {
                ref path,
                ref mut data,
            } = values
            {
                let mut full_path = script_dir.to_path_buf();
                full_path.push(path);
                File::open(&full_path)
                    .map_err(|e| {
                        anyhow!("Failed to load data from {}: {}", full_path.display(), e)
                    })?
                    .read_to_end(data)?;
            }
        }
    }

    Ok(())
}
