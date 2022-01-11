
use std::{
    iter,
    iter::Peekable as Peek,
    vec::IntoIter as VecIter,
    collections::HashMap,
};
use crate::{
    err, eat, eat_if,
    Error,
    lexer::Token,
    parser::{ ast, ast::{ Program, Assoc } },
};

pub fn run(mut iter: Peek<VecIter<(usize, Token)>>) -> Result<Program, Error> {

    eat_if!(iter, Semicolon);

    let mut used = Vec::new();
    let mut program = ast::Program {
        module: None,
        fixitys: iter::repeat_with(|| HashMap::new()).take(10).collect(),
        defs: Vec::new(),
    };

    module(&mut iter, &mut program, &mut used)?;

    for op in used.into_iter() {

        let has = program.fixitys.iter()
            .any(|x| x.get(&op).is_some());

        if !has {
            unsafe { program.fixitys.get_unchecked_mut(9).insert(op, (0, Assoc::Left)); }
        }

    }

    println!("{:?}", &program);

    Ok(program)

}

fn module(
    iter: &mut Peek<VecIter<(usize, Token)>>, 
    program: &mut ast::Program, 
    used: &mut Vec<String>,
) -> Result<(), Error> {

    if eat_if!(iter, Module).is_some() {
    
        let name = if let (_, Token::ConsId(name)) = eat!(iter, ConsId) { name } else { unreachable!() };

        eat!(iter, Where);
        eat!(iter, BraceL);
        eat_if!(iter, Semicolon);

        program.module = Some((name, Vec::new()));
        
        if eat_if!(iter, BraceR).is_none() {
            top_level(iter, program, used)?;
            return Ok(());
        }

    }

    top_level(iter, program, used)?;

    Ok(())

}

fn top_level(
    iter: &mut Peek<VecIter<(usize, Token)>>, 
    program: &mut ast::Program,
    used: &mut Vec<String>
) -> Result<(), Error> {

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

        if let Some(stmt) = def(iter, used)? {
            program.defs.push(stmt);
        }
        
    }

    Ok(())

}

fn def(
    iter: &mut Peek<VecIter<(usize, Token)>>,
    used: &mut Vec<String>,
) -> Result<Option<ast::Def>, Error> {

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
            Token::VarOp(ref x) => used.push(x.clone()),
            Token::ConsOp(ref x) => used.push(x.clone()),
            _ => (),
        }

        vec.push((i, token));

    }

    if braces != 0 {
        return err!(pos; "unterminated braces in statement");
    }

    Ok(Some(ast::Def::Raw(pos, vec)))

}
