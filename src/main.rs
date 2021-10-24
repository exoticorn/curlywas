mod parser;
mod ast;

fn main() {
    let input = include_str!("../test.hw");
    let result = parser::parse(input);
    match result {
        Ok(script) => {dbg!(script);},
        Err(err) => println!("error: {}", nom::error::convert_error(input, err))
    }
}
