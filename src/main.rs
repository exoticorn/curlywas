mod parser;
mod ast;
mod typecheck;
mod constfold;

fn main() {
    let input = include_str!("../test.hw");
    let result = parser::parse(input);
    match result {
        Ok(mut script) => {
            constfold::fold_script(&mut script);
            typecheck::tc_script(&mut script).unwrap();
            dbg!(script);
        },
        Err(err) => println!("error: {}", nom::error::convert_error(input, err))
    }
}
