
mod emitter;
mod lexer;
mod parser;

use std::env;
use std::fs;

#[macro_export]
macro_rules! err {

    ($pos: expr, $msg: literal) => {
        Err(($pos, String::from($msg)))
    };

}

fn main() {

    let contents = env::args()
        .nth(1)
        .ok_or((0, String::from("file path not specified")))
        .and_then(|path| fs::read_to_string(path).map_err(|e| (0, e.to_string())))
        .and_then(compile);

    match contents {

        Ok(o) => println!("{}", print_tokens(o)),
        Err(e) => println!("error: {:?}", e),

    };

}

fn compile(program: String) -> Result<Vec<(usize, lexer::Token)>, (usize, String)> {

    let iter = program.chars().enumerate().peekable();

    Ok(iter)
        .and_then(lexer::lex)

}

fn print_tokens(vec: Vec<(usize, lexer::Token)>) -> String {

    vec.into_iter().map(|(_, x)| format!("{:?} ", x)).collect()
    
}
