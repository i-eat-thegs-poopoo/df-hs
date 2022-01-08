
use std::iter;
use std::iter::Peekable as Peek;
use std::vec::IntoIter as VecIter;
use std::collections::HashMap;

use crate::Error;
use crate::lexer::Token;
use crate::parser::ast;
use crate::parser::ast::Program;
use crate::parser::ast::Assoc;
use crate::eat;
use crate::eat_if;

use crate::err;

pub fn run(mut iter: Peek<VecIter<(usize, Token)>>) -> Result<Program, Error> {

    eat_if!(iter, Semicolon);

    let mut program = ast::Program {
        module: None,
        fixitys: iter::repeat_with(|| HashMap::new()).take(10).collect(),
        defs: Vec::new(),
    };

    module(&mut iter, &mut program)?;
    println!("{:?}", &program);

    Ok(program)

}

fn module(iter: &mut Peek<VecIter<(usize, Token)>>, program: &mut ast::Program) -> Result<(), Error> {

    if eat_if!(iter, Module).is_some() {
    
        let name = if let (_, Token::ConsId(name)) = eat!(iter, ConsId) { name } else { unreachable!() };

        eat!(iter, Where);
        eat!(iter, BraceL);
        eat_if!(iter, Semicolon);

        program.module = Some((name, Vec::new()));
        
        if eat_if!(iter, BraceR).is_none() {
            top_level(iter, program)?;
            return Ok(());
        }

    }

    top_level(iter, program)?;

    Ok(())

}

fn top_level(iter: &mut Peek<VecIter<(usize, Token)>>, program: &mut ast::Program) -> Result<(), Error> {

    while iter.peek().is_some() {

        if eat_if!(iter, Semicolon).is_some() { continue }

        if let Some((i, token)) = eat_if!(iter, Infix, InfixL, InfixR) {

            let assoc = match token {
                Token::Infix => Assoc::None,
                Token::InfixL => Assoc::Left,
                Token::InfixR => Assoc::Right,
                _ => unreachable!(),
            };

            let fix = if let (_, Token::Int(x)) = eat!(iter, Int) { x } else { unreachable!() }
                .parse::<usize>()
                .or(err!(i; "internal parse number error"))?;

            let (_, op) = eat!(iter, VarOp, ConsOp);
            let op = match op {
                Token::VarOp(x) => x,
                Token::ConsOp(x) => x,
                _ => unreachable!(),
            };

            let table = program.fixitys
                .get_mut(fix)
                .ok_or(err!(@t i; "operator precedence must be between 0 and 9 (inclusive)"))?;

            if let Some((pos, _)) = table.get(&op) {
                return err!(i, *pos; "duplicate fixity declaration");
            }

            table.insert(op, (i, assoc));
            eat!(iter, Semicolon);

            continue;

        }

        if eat_if!(iter, EoF).is_some() { break }

        if let Some(stmt) = def(iter)? {
            program.defs.push(stmt);
        }
        
    }

    Ok(())

}

fn def(iter: &mut Peek<VecIter<(usize, Token)>>) -> Result<Option<ast::Def>, Error> {

    if eat_if!(iter, Semicolon).is_some() {
        return Ok(None);
    }

    let pos = if let Some((i, _)) = iter.peek() { *i } else { return Ok(None) };

    let mut vec = Vec::new();
    let mut braces: usize = 0;

    while let Some((i, token)) = iter.next() {

        match token {
            Token::Semicolon if braces == 0 => break,
            Token::BraceL => braces += 1,
            Token::BraceR if braces == 0 => return err!(i; "unbalanced braces"),
            Token::BraceR => braces -= 1,
            _ => (),
        }

        vec.push((i, token));

    }

    if braces != 0 {
        return err!(pos; "unterminated braces in statement");
    }

    Ok(Some(ast::Def::Raw(pos, vec)))

}
