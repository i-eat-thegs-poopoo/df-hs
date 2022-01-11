
use peeking_take_while::PeekableExt;
use std::{
    iter::{ Enumerate as Enum, Peekable as Peek },
    str::Chars,
};
use crate::{
    err, bang,
    Error,
};

pub enum Token {

    Number(String),
    Upper(String),
    Lower(String),
    Symbol(String),
    ColSymbol(String),
    Char(char),
    String(String),

    Punc(char),

}

const SYMBOL_CHARS: [char; 20] = [
    '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '^', '|', '-', '~', '\\', ':'
];

pub struct LexIter<'a> {
    inner: Peek<Enum<Chars<'a>>>,
    line_pos: usize,
    first: bool,
}

impl <'a> LexIter<'a> {

    pub fn new(iter: Peek<Enum<Chars<'a>>>) -> Self {
        LexIter { inner: iter, line_pos: 0, first: true }
    }

    fn newline(&mut self) {
        self.line_pos = self.inner.peek().map(|(i, _)| *i).unwrap_or_default();
        self.first = true;
    }

    fn lex_string<F>(&mut self, head: char, predicate: F) -> String
    where F: Fn(&char) -> bool {

        let mut out = head.to_string();
        let tail: String = self.inner
            .peeking_take_while(|(_, x)| predicate(x))
            .map(|(_, x)| x)
            .collect();

        out.push_str(&tail);
        out

    }

    fn lex_quotes(&mut self, i: usize) -> Result<String, Error> {

        let mut out = String::new();
    
        loop {
    
            let span: String = self.inner
                .peeking_take_while(|&(_, x)| x != '"' && x != '\\')
                .map(|(_, x)| x)
                .collect();
    
            out.push_str(&span);
    
            match self.inner.next() {
    
                Some((_, '"')) => return Ok(out),
                Some((_, '\\')) => {
    
                    if let Some((_, x)) = self.inner.next() {
                        out.push(x);
                    } else {
                        return err!(i; "unterminated string literal at end of file");
                    }
    
                    continue;
    
                }
                _ => return err!(i; "unterminated string literal at end of file"),
    
            }
    
        }
    
    }

}

impl <'a> Iterator for LexIter<'a> {

    type Item = Result<(usize, Token, usize, bool), Error>;

    fn next(&mut self) -> Option<Self::Item> {

        let (i, x) = self.inner.next()?;

        if x == '\n' { self.newline(); return self.next() }
        if x.is_ascii_whitespace() { return self.next() }

        macro_rules! lex {
            ($predicate: expr) => {
                self.lex_string(x, $predicate)
            };
        }

        let indent = i - self.line_pos;
        let token = match x {

            '{' | '-' if matches!(self.inner.peek().map(|(_, x)| x), Some('-')) => match x {

                '{' => {

                    loop {

                        self.inner.by_ref().take_while(|&(_, x)| x != '-').for_each(drop);

                        match self.inner.next() {

                            Some((_, '}')) => break,
                            None => return Some(err!(i; "unterminated block comment at end of file")),
                            _ => (),

                        }

                    }

                    return self.next();

                }

                '-' => {
                    self.inner.by_ref().take_while(|&(_, x)| x != '\n').for_each(drop);
                    self.newline();

                    return self.next();
                }

                _ => unreachable!(),

            },

            '0'..='9' => Token::Number(lex!(|x| x.is_ascii_digit())),
            'A'..='Z' => Token::Upper(lex!(|&x| x.is_ascii_alphanumeric() || x == '_')),
            'a'..='z' | '_' => {

                let token = lex!(|&x| x.is_ascii_alphanumeric() || x == '_');

                match token.as_str() {

                    "do" | "let" | "of" | "where" => {
                        let old = self.first;
                        self.first = true;
                        return Some(Ok((i, Token::Lower(token), indent, old)));
                    },

                    _ => Token::Lower(token),

                }
                
            },

            _ if SYMBOL_CHARS.contains(&x) => {
                let token = lex!(|x| SYMBOL_CHARS.contains(x));
                self.first = self.first || token.as_str() == "|";
                if x == ':' { Token::ColSymbol(token) } else { Token::Symbol(token) }
            },

            '(' | ')' | ',' | '[' | ']' /* | ';' | '{' | '}' */ => Token::Punc(x),
            '"' => Token::String(bang!(self.lex_quotes(i))),
            '\'' => {

                macro_rules! next { () => {
                    bang!(self.inner.next().ok_or(err!(@t i; "character literal unterminated"))).1
                }; }

                let first = next!();
                let out = if first == '\\' { next!() } else { first };

                if next!() != '\'' {
                    return Some(err!(i; "character literals can only be one character"));
                }

                Token::Char(out)

            }
            _ => return Some(err!(i; "character is unexpected or invalid")),

        };

        let old = self.first;
        self.first = false;

        Some(Ok((i, token, indent, old)))
        
    }

}
