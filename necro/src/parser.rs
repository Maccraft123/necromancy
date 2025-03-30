use chumsky::{
    prelude::*,
    text::ascii::{ident},
    text::{whitespace, newline, keyword, inline_whitespace},
    inspector::Inspector,
    input::{InputRef, Checkpoint, Cursor},
    extra::ParserExtra,
};

use crate::lexer::Token as LToken;

#[derive(Debug, Clone)]
pub enum Token {
    Asdf
}

pub fn shed<'a>() -> impl Parser<'a, &'a [LToken<'a>], Vec<Token>, extra::Err<Rich<'a, LToken<'a>>>> {
    just(LToken::Cpu(""))
        .map(|_| Token::Asdf)
        .repeated()
        .collect()
}
