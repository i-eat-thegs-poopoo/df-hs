
#![allow(dead_code)]

mod emitter;
mod lexer;
mod parser;

use std::{ env, fs };

#[macro_export]
macro_rules! err {

    (@t $( $pos: expr ),+; $msg: literal) => {
        (vec![$( $pos ),+], String::from($msg))
    };

    (@t $( $pos: expr ),+; $msg: expr) => {
        (vec![$( $pos ),+], $msg)
    };

    ($( $pos: expr ),+; $msg: literal) => {
        Err((vec![$( $pos ),+], String::from($msg)))
    };

    ($( $pos: expr ),+; $msg: expr) => {
        Err((vec![$( $pos ),+], $msg))
    };

}

#[macro_export]
macro_rules! bang {
    ($e: expr) => {
        match $e {
            Ok(o) => o,
            Err(e) => return Some(Err(e)),
        }
    };
}

#[macro_export]
macro_rules! bang_ref {
    ($e: expr) => {
        match $e {
            Ok(o) => o,
            Err(e) => return Some(Err(e.clone())),
        }
    };
}

pub type Error = (Vec<usize>, String);

trait TransposeRef<'a, T, E> {
    fn transpose(self) -> Result<Option<&'a T>, E>;
}

impl <'a, T, E: Clone> TransposeRef<'a, T, E> for Option<&'a Result<T, E>> {
    fn transpose(self) -> Result<Option<&'a T>, E> {
        match self {
            Some(Ok(o)) => Ok(Some(o)),
            Some(Err(e)) => Err(e.clone()),
            None => Ok(None),
        }
    }
}

fn main() {

    let contents = env::args()
        .nth(1)
        .ok_or((vec![0], String::from("file path not specified")))
        .and_then(|path| fs::read_to_string(path).map_err(|e| (vec![0], e.to_string())))
        .and_then(compile);

    match contents {

        // Ok(_) => println!("ok"),
        Ok(o) => println!("{}", print_tok(o)),
        Err(e) => println!("error at {:?}: {}", e.0, e.1),

    };

}

// fn compile(program: String) -> Result<(), Error> {
// fn compile(program: String) -> Result<Vec<(Vec<usize>, lexer::Token)>, Error> {
fn compile(program: String) -> Result<Vec<(usize, lexer::Token)>, Error> {

    let iter = program.chars().enumerate().peekable();

    Ok(iter)
        .and_then(lexer::lex)
        // .map(|x| x.into_iter().peekable())
        // .and_then(parser::parse)

}

fn print_tok(vec: Vec<(usize, lexer::Token)>) -> String {

    vec.into_iter().map(|(_, x)| format!("{:?} ", x)).collect()
    
}
