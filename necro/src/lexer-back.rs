use chumsky::{
    extra::{Context, ParserExtra},
    input::{Checkpoint, Cursor, InputRef},
    inspector::Inspector,
    prelude::*,
    text::ascii::ident,
    text::{inline_whitespace, keyword, newline, whitespace},
};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Cpu(&'a str),
    Os(&'a str),
    Section(&'a str),
    Define(&'a str, usize),
    Import(&'a str),
    Export(&'a str),
    StrUtf8(&'a str),
    OsStr(&'a str),
    Label(&'a str),
    Comment(&'a str),
    U8(&'a str),
    U16(&'a str),
    U32(&'a str),
    Instruction(crate::cpu::DynInstruction),
}

pub trait ParseNum: Sized {
    fn parse_radix<'a>(
        base: u32,
    ) -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone;
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone;
}

macro_rules! impl_parse_num {
    ($($ty: ty),*) => {
        $(
            impl ParseNum for $ty {
                fn parse_radix<'a>(base: u32) -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone {
                    chumsky::text::digits(base)
                        .collect()
                        .try_map(move |v: String, span|
                            <$ty>::from_str_radix(&v, base).map_err(|e| Rich::custom(span, e))
                        )
                }
                fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone {
                    choice((
                        just("0x").ignore_then(<$ty>::parse_radix(16)),
                        just("0o").ignore_then(<$ty>::parse_radix(8)),
                        just("0b").ignore_then(<$ty>::parse_radix(2)),
                        <$ty>::parse_radix(16).then_ignore(just("$")),
                        <$ty>::parse_radix(16).then_ignore(just("h")),
                        <$ty>::parse_radix(10),
                    )).labelled(concat!(stringify!($ty), " literal"))
                }
            }
        )*
    }
}

impl_parse_num!(u8, u16, u32, u64, i8, i16, i32, i64, usize, isize);

pub fn number<'a, T: ParseNum>() -> impl Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>> + Clone
{
    T::parse()
}

#[derive(Debug, Copy, Clone)]
enum Cpu {
    None,
    Intel8080,
    Mos6502,
}

#[derive(Debug, Copy, Clone)]
pub struct ParseContext {
    cpu: Cpu,
}


use chumsky::inspector::SimpleState;
impl Default for ParseContext {
    fn default() -> Self {
        Self { cpu: Cpu::None }
    }
}

pub fn new_state() -> ParseContext {
    ParseContext {
        cpu: Cpu::None
    }
}

impl<'src, I: Input<'src>> Inspector<'src, I> for ParseContext {
    type Checkpoint = ();

    fn on_token(&mut self, token: &I::Token) {}
    fn on_save<'parse>(
        &self,
        cursor: &Cursor<'src, 'parse, I>,
    ) -> Self::Checkpoint {}
    fn on_rewind<'parse>(
        &mut self,
        marker: &Checkpoint<'src, 'parse, I, Self::Checkpoint>,
    ) {}
}

pub fn shed<'a>() -> impl for<'s> Parser<'a, &'a str, Token<'a>, extra::Full<Rich<'a, char>, ParseContext, ()>>
{
    let comment = just(";").then(none_of("\r\n").repeated().to_slice());

    let to_end_line = none_of("\r\n").repeated().to_slice();

    let string = just('"')
        .ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .labelled("string");

    let newlines = newline()
        .repeated()
        .at_least(1)
        .ignored()
        .labelled("newline");
    macro_rules! directive {
        ($name: literal, $parser: expr, $variant: expr) => {
            just(concat!(".", $name))
                .labelled("directive")
                .then_ignore(inline_whitespace().at_least(1))
                .ignore_then($parser)
                .then_ignore(inline_whitespace())
                .then_ignore(newlines)
                .map($variant)
        };
    }

    //let padded_ident = ident().padded_by(inline_whitespace()).labelled("identifier");

    let os = directive!("os", string, Token::Os);
    let section = directive!("section", ident(), Token::Section);

    let import = directive!("import", ident(), Token::Import);
    let export = directive!("export", ident(), Token::Export);
    let str_utf8 = directive!("str_utf8", string, Token::StrUtf8);
    let os_str = directive!("os_str", string, Token::OsStr);
    let u8_ = directive!("u8", to_end_line, Token::U8);
    let u16_ = directive!("u16", to_end_line, Token::U16);
    let u32_ = directive!("u32", to_end_line, Token::U32);

    let cpu = just(".cpu")
        .labelled("directive")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(string)
        .then_ignore(inline_whitespace())
        .then_ignore(newlines)
        //.ignore_with_ctx(map_(|inp| panic!("{:?}", inp.ctx())))
        .map(Token::Cpu);

    let define = just(".define")
        .labelled("directive")
        .ignore_then(inline_whitespace().at_least(1))
        .ignore_then(ident())
        .then_ignore(inline_whitespace().at_least(1))
        .then(number())
        .then_ignore(newlines)
        .map(|(i, v)| Token::Define(i, v));

    let label = ident()
        .then_ignore(just(":"))
        .labelled("label")
        .then_ignore(newlines.or_not())
        .map(Token::Label);

    let instruction = crate::cpu::InstructionParser::new()
        .labelled("instruction")
        .padded_by(inline_whitespace())
        .map(Token::Instruction);

    choice((
        instruction,
        cpu,
        os,
        section,
        import,
        export,
        str_utf8,
        os_str,
        label,

        u8_,
        u16_,
        u32_,
        define,
    ))
        .with_state(ParseContext {cpu: Cpu::None})
        .padded_by(comment.padded().repeated())
        .repeated()
        .collect()
}
