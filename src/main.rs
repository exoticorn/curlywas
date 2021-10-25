use std::fs::File;
use std::io::prelude::*;

mod parser;
mod ast;
mod typecheck;
mod constfold;
mod emit;


fn main() {
    let input = include_str!("../test.hw");
    let result = parser::parse(input);
    match result {
        Ok(mut script) => {
            constfold::fold_script(&mut script);
            typecheck::tc_script(&mut script).unwrap();
            let wasm = emit::emit(&script);
            let mut file = File::create("test.wasm").unwrap();
            file.write_all(&wasm).unwrap();
        },
        Err(err) => println!("error: {}", nom::error::convert_error(input, err))
    }
}
