
use std::fmt::Debug;
use std::iter::Enumerate as Enum;
use std::iter::Peekable as Peek;
use std::str::Chars;

#[derive(PartialEq)]
pub enum Token {

    Number(String),
    ConsId(String),
    VarId(String),
    ConsOp(String),
    VarOp(String),
    String(String),

    ParenL,
    ParenR,
    Comma,
    Semicolon,
    BracketL,
    BracketR,
    BraceL,
    BraceR,

    Case,
    Class,
    Data,
    Deriving,
    Do,
    Else,
    If,
    Import,
    In,
    Infix,
    InfixL,
    InfixR,
    Instance,
    Let,
    Module,
    Newtype,
    Of,
    Then,
    Type,
    Where,
    Range,
    Typedef,
    Assign,
    Lambda,
    Pipe,
    ArrowL,
    ArrowR,
    As,
    Constraint,

    Wildcard,

    EoF,

}

impl Debug for Token {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        match self {

            Token::Number(x) => f.write_str(&x),
            Token::ConsId(x) => f.write_str(&x),
            Token::VarId(x) => f.write_str(&x),
            Token::ConsOp(x) => f.write_str(&x),
            Token::VarOp(x) => f.write_str(&x),
            Token::String(x) => f.write_str(&x),

            Token::ParenL => f.write_str("("),
            Token::ParenR => f.write_str(")"),
            Token::Comma => f.write_str(","),
            Token::Semicolon => f.write_str("\n;"),
            Token::BracketL => f.write_str("["),
            Token::BracketR => f.write_str("]"),
            Token::BraceL => f.write_str("\n{"),
            Token::BraceR => f.write_str("\n}"),
            Token::Case => f.write_str("case"),
            Token::Class => f.write_str("class"),
            Token::Data => f.write_str("data"),
            Token::Deriving => f.write_str("deriving"),
            Token::Do => f.write_str("do"),
            Token::Else => f.write_str("else"),
            Token::If => f.write_str("if"),
            Token::Import => f.write_str("import"),
            Token::In => f.write_str("in"),
            Token::Infix => f.write_str("infix"),
            Token::InfixL => f.write_str("infixl"),
            Token::InfixR => f.write_str("infixr"),
            Token::Instance => f.write_str("instance"),
            Token::Let => f.write_str("let"),
            Token::Module => f.write_str("module"),
            Token::Newtype => f.write_str("newtype"),
            Token::Of => f.write_str("of"),
            Token::Then => f.write_str("then"),
            Token::Type => f.write_str("type"),
            Token::Where => f.write_str("where"),
            Token::Range => f.write_str(".."),
            Token::Typedef => f.write_str("::"),
            Token::Assign => f.write_str("="),
            Token::Lambda => f.write_str("\\"),
            Token::Pipe => f.write_str("|"),
            Token::ArrowL => f.write_str("<-"),
            Token::ArrowR => f.write_str("->"),
            Token::As => f.write_str("@"),
            Token::Constraint => f.write_str("=>"),
            Token::Wildcard => f.write_str("_"),
            Token::EoF => f.write_str("EOF"),

        }

    }

}

pub fn lex(iter: Peek<Enum<Chars>>) -> Result<Vec<(usize, Token)>, (usize, String)> {

    Ok(iter)
        .and_then(lexeme::run)
        .map(|x| x.into_iter().peekable())
        .and_then(reserved::run)
        .map(|mut x| {
            x.push((0, Token::EoF, 0, true)); 
            x.into_iter().peekable()
        })
        .and_then(braces::run)

}

mod lexeme {

    use peeking_take_while::PeekableExt;
    use std::iter::Enumerate as Enum;
    use std::iter::Peekable as Peek;
    use std::str::Chars;

    use crate::err;

    pub enum Token {

        Number(String),
        Upper(String),
        Lower(String),
        Symbol(String),
        ColSymbol(String),
        String(String),

        Punc(char),

    }

    const SYMBOL_CHARS: [char; 20] = [
        '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '^', '|', '-', '~', '\\', ':'
    ];

    pub fn run(mut iter: Peek<Enum<Chars>>) -> Result<Vec<(usize, Token, usize, bool)>, (usize, String)> {

        let mut vec = Vec::new();

        'outer: loop {

            let line_pos = if let Some((i, _)) = iter.peek() { 
                *i
            }
            else {
                break;
            };

            let mut first = true;

            while let Some((i, x)) = iter.next() {

                if x == '\n' {
                    continue 'outer;
                }

                if x.is_ascii_whitespace() {
                    continue;
                }

                macro_rules! lex {

                    ($predicate: expr) => {
                        lex_string(&mut iter, x, $predicate)
                    };

                }

                let indent = i - line_pos;

                let token = match x {

                    '{' | '-' if matches!(iter.peek().map(|(_, x)| x), Some('-')) => match x {

                        '{' => {

                            loop {

                                iter.by_ref().take_while(|&(_, x)| x != '-').for_each(drop);

                                match iter.next() {

                                    Some((_, '}')) => break,
                                    None => return err!(i, "block comment unterminated"),
                                    _ => (),

                                }

                            }

                            continue;

                        }

                        '-' => {
                            
                            iter.by_ref().take_while(|&(_, x)| x != '\n').for_each(drop);

                            continue 'outer;

                        }

                        _ => unreachable!(),

                    },

                    '0'..='9' => Token::Number(lex!(|x| x.is_ascii_digit())),
                    'A'..='Z' => Token::Upper(lex!(|&x| x.is_ascii_alphanumeric() || x == '_')),

                    'a'..='z' | '_' => {

                        let token = lex!(|&x| x.is_ascii_alphanumeric() || x == '_');

                        match token.as_str() {

                            "do" | "let" | "of" | "where" => {

                                vec.push((i, Token::Lower(token), indent, first));
                                first = true;
    
                                continue;
    
                            },

                            _ => Token::Lower(token),

                        }
                        
                    },

                    _ if SYMBOL_CHARS.contains(&x) => {

                        let token = lex!(|x| SYMBOL_CHARS.contains(x));

                        if token.as_str() == "|" {
                            first = true;
                        }

                        if x == ':' {
                            Token::ColSymbol(token)
                        }
                        else {
                            Token::Symbol(token)
                        }

                    },

                    '(' | ')' | ',' | '[' | ']' /* | ';' | '{' | '}' */ => Token::Punc(x),

                    '"' => Token::String(lex_quotes(i, &mut iter)?),

                    _ => return err!(i, "character is unexpected or invalid"),

                };

                vec.push((i, token, indent, first));
                first = false;

            }

        }

        Ok(vec)
        
    }

    fn lex_string<F>(iter: &mut Peek<Enum<Chars>>, head: char, predicate: F) -> String
    where
        F: Fn(&char) -> bool,
    {

        let mut out = head.to_string();

        let tail: String = iter
            .peeking_take_while(|(_, x)| predicate(x))
            .map(|(_, x)| x)
            .collect();

        out.push_str(&tail);

        out

    }

    #[inline(always)]
    fn lex_quotes(i: usize, iter: &mut Peek<Enum<Chars>>) -> Result<String, (usize, String)> {

        let mut out = String::new();

        loop {

            let span: String = iter
                .peeking_take_while(|&(_, x)| x != '"' && x != '\\')
                .map(|(_, x)| x)
                .collect();

            out.push_str(&span);

            match iter.next() {

                Some((_, '"')) => return Ok(out),

                Some((_, '\\')) => {

                    if let Some((_, x)) = iter.next() {
                        out.push(x);
                    } 
                    else {
                        return err!(i, "string literal unterminated");
                    }

                    continue;

                }

                _ => return err!(i, "string literal unterminated"),

            }

        }

    }

}

mod reserved {

    use std::iter::Peekable as Peek;
    use std::vec::IntoIter as VecIter;

    use crate::lexer::lexeme::Token as LexToken;
    use crate::lexer::Token;

    pub fn run(mut iter: Peek<VecIter<(usize, LexToken, usize, bool)>>) -> Result<Vec<(usize, Token, usize, bool)>, (usize, String)> {

        let mut vec = Vec::new();

        while let Some((i, lexeme, indent, first)) = iter.next() {

            let token = match lexeme {

                LexToken::Number(mut val) => match iter.peek() {

                    Some((_, LexToken::Symbol(dot), ..)) if dot == "." => {

                        iter.next();

                        if let Some((_, LexToken::Number(frac), ..)) = iter.peek() {

                            val.push('.');
                            val.push_str(&frac);

                            iter.next();

                            Token::Number(val)

                        } 
                        else {

                            vec.push((i, Token::Number(val), indent, first));
                            Token::VarOp(String::from("."))

                        }

                    }

                    _ => Token::Number(val),

                },

                LexToken::Punc(val) => match val {

                    '(' => Token::ParenL,
                    ')' => Token::ParenR,
                    ',' => Token::Comma,
                    ';' => Token::Semicolon,
                    '[' => Token::BracketL,
                    ']' => Token::BracketR,
                    '{' => Token::BraceL,
                    '}' => Token::BraceR,

                    _ => unreachable!(),

                },

                LexToken::Symbol(val) => match &*val {

                    ".." => Token::Range,
                    "=" => Token::Assign,
                    "\\" => Token::Lambda,
                    "|" => Token::Pipe,
                    "<-" => Token::ArrowL,
                    "->" => Token::ArrowR,
                    "@" => Token::As,
                    "=>" => Token::Constraint,

                    _ => Token::VarOp(val),

                },

                LexToken::ColSymbol(val) => match &*val {

                    "::" => Token::Typedef,

                    _ => Token::ConsOp(val),

                }

                LexToken::Lower(val) => match &*val {

                    "case" => Token::Case,
                    "class" => Token::Class,
                    "data" => Token::Data,
                    "deriving" => Token::Deriving,
                    "do" => Token::Do,
                    "else" => Token::Else,
                    "if" => Token::If,
                    "import" => Token::Import,
                    "in" => Token::In,
                    "infix" => Token::Infix,
                    "infixl" => Token::InfixL,
                    "infixr" => Token::InfixR,
                    "instance" => Token::Instance,
                    "let" => Token::Let,
                    "module" => Token::Module,
                    "newtype" => Token::Newtype,
                    "of" => Token::Of,
                    "then" => Token::Then,
                    "type" => Token::Type,
                    "where" => Token::Where,

                    "_" => Token::Wildcard,

                    _ => Token::VarId(val),

                },

                LexToken::Upper(val) => Token::ConsId(val),
                LexToken::String(val) => Token::String(val),

            };

            vec.push((i, token, indent, first));

        }

        Ok(vec)

    }

}

mod braces {

    use std::iter::Peekable as Peek;
    use std::vec::IntoIter as VecIter;

    use crate::lexer::Token;

    pub fn run(mut iter: Peek<VecIter<(usize, Token, usize, bool)>>) -> Result<Vec<(usize, Token)>, (usize, String)> {

        let mut vec = Vec::new();
        let mut indents = Vec::new();

        impl_braces(0, &mut iter, &mut vec, &mut indents, false, false)?;

        Ok(vec)

    }

    fn parens(

        iter: &mut Peek<VecIter<(usize, Token, usize, bool)>>,
        vec: &mut Vec<(usize, Token)>,

    ) -> Result<(), (usize, String)> {

        let mut indents = Vec::new();

        impl_braces(0, iter, vec, &mut indents, false, false)?;

        if let Some((i, token, _, _)) = iter.next() {
            vec.push((i, token));
        }

        Ok(())

    }

    fn impl_braces(

        base: usize,
        iter: &mut Peek<VecIter<(usize, Token, usize, bool)>>,
        vec: &mut Vec<(usize, Token)>,
        indents: &mut Vec<usize>,
        let_expr: bool,
        where_clause: bool,

    ) -> Result<Option<usize>, (usize, String)> {

        let mut guards = true;

        if let Some(val) = exit(base, iter, vec, indents, let_expr, where_clause, guards) {
            return val;
        }

        while let Some((i, token, indent, first)) = iter.next() {

            if first && indent == base {
                guards = true;
                vec.push((0, Token::Semicolon));
            }

            let paren = matches!(token, Token::ParenL);
            let block = matches!(token, Token::Do | Token::Let | Token::Of | Token::Where);
            let is_let = token == Token::Let;
            let isnt_where = token != Token::Where;

            if let Token::Assign | Token::ArrowR = token {
                guards = false;
            }

            vec.push((i, token));

            if paren {
                parens(iter, vec)?;
            }

            if block {

                vec.push((0, Token::BraceL));
                indents.push(base);

                let let_in = if let Some((_, _, indent, _)) = iter.peek() {
                    impl_braces(*indent, iter, vec, indents, is_let, isnt_where)?
                }
                else {
                    None
                };

                vec.push((0, Token::BraceR));
                indents.pop();

                if let Some(i) = let_in {
                    vec.push((i, Token::In));
                }
                
            }

            if let Some(val) = exit(base, iter, vec, indents, let_expr, where_clause, guards) {
                return val;
            }

        }

        Ok(None)

    }

    fn exit(

        base: usize,
        iter: &mut Peek<VecIter<(usize, Token, usize, bool)>>,
        vec: &mut Vec<(usize, Token)>,
        indents: &mut Vec<usize>,
        let_expr: bool,
        where_clause: bool,
        guards: bool,

    ) -> Option<Result<Option<usize>, (usize, String)>> {

        if let Some((_, token, indent, first)) = iter.peek() {

            match token {

                Token::ParenR => return Some(Ok(None)),
                Token::In if let_expr => return Some(Ok(Some(iter.next().unwrap().0))),
                Token::In => return Some(Ok(None)),
                Token::Where if where_clause => return Some(Ok(None)),

                Token::Pipe if guards && !indents.contains(indent) && indent != &base => {

                    vec.push((0, Token::BraceL));
                    indents.push(base);

                    let let_in = if let Some((_, _, indent, _)) = iter.peek() {

                        match impl_braces(*indent, iter, vec, indents, false, true) {

                            Ok(o) => o,
                            Err(e) => return Some(Err(e)),

                        }

                    }
                    else {
                        None
                    };
    
                    vec.push((0, Token::BraceR));
                    indents.pop();
    
                    if let Some(i) = let_in {
                        vec.push((i, Token::In));
                    }

                    if let Some(val) = exit(base, iter, vec, indents, let_expr, where_clause, guards) {
                        return Some(val);
                    }

                },
                
                _ if indents.contains(indent) && *first => return Some(Ok(None)),
                _ => (),

            }

        }

        None

    }

}
