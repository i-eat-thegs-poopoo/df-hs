
use std::iter::Peekable as Peek;
use crate::{
    bang,
    Error,
    lexer::{ lexeme::{ LexIter, Token as LexToken }, Token }, TransposeRef,
};

pub struct ResIter<'a> {
    inner: Peek<LexIter<'a>>,
    extra: Option<<Self as Iterator>::Item>,
}

impl <'a> ResIter<'a> {

    pub fn new(iter: LexIter<'a>) -> Self {
        ResIter { inner: iter.peekable(), extra: None }
    }

}

impl <'a> Iterator for ResIter<'a> {

    type Item = Result<(usize, Token, usize, bool), Error>;

    fn next(&mut self) -> Option<Self::Item> {

        if self.extra.is_some() {
            return self.extra.take();
        }

        let mut extra = None;
        let (i, lexeme, indent, first) = bang!(self.inner.next()?);
        let token = match lexeme {

            LexToken::Number(mut val) => match bang!(self.inner.peek().transpose()) {

                Some((_, LexToken::Symbol(dot), ..)) if dot == "." => {

                    self.inner.next();

                    if let Some((_, LexToken::Number(frac), ..)) = bang!(self.inner.peek().transpose()) {
                        val.push('.');
                        val.push_str(&frac);
                        self.inner.next();
                        Token::Frac(val)
                    } else {
                        extra = Some(Ok((i, Token::Int(val), indent, first)));
                        Token::VarOp(String::from("."))
                    }

                }

                _ => Token::Int(val),

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
            LexToken::Char(val) => Token::Char(val),

        };

        let out = Some(Ok((i, token, indent, first)));

        if extra.is_some() { self.extra = out; extra } else { out }

    }

}
