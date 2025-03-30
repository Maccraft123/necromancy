
use chumsky::{
    prelude::*,
    text::ascii::{ident},
    text::{whitespace, newline, keyword},
    inspector::Inspector,
    input::{InputRef, Checkpoint, Cursor},
    extra::ParserExtra,
};
use ariadne::{sources, Color, Label, Report, ReportKind};

#[derive(Debug, Clone)]
pub enum StringKind {
}

#[derive(Debug, Clone)]
pub enum Token<'a> {
    Directive,
    SetCpu(&'a str),
    SetOs(&'a str),
    Define(&'a str, &'a str),
    Section(&'a str),
    Label(&'a str),
    Comment(&'a str),
    Instruction(CpuInstruction<'a>),
    Temp(&'a str),
}

#[derive(Debug, Clone)]
pub enum CpuInstruction<'a> {
    Intel8080(&'a str),
    Mos6502(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub enum Cpu {
    Intel8080,
    Mos6502,
    None,
}

pub fn parser<'a>() -> impl Parser<'a, &'a str, Vec<Token<'a>>, extra::Err<Rich<'a, char>>> {
    recursive(|recurse| {
        let string = just('"')
            .ignore_then(none_of("\"\n\r").repeated().to_slice())
            .then_ignore(just('"'))
            .labelled("string");

        let cpu = keyword("cpu")
            .ignore_then(string.padded())
            .map(|s| Token::SetCpu(s))
            .ignore_with_ctx(recurse);

        let os = keyword("os")
            .ignore_then(string.padded())
            .map(|s| Token::SetOs(s));

        let define = keyword("define")
            .ignore_then(ident().padded())
            .then_ignore(just("->").padded())
            .then(none_of("\r\n\t ").repeated().to_slice())
            .map(|(i, s)| Token::Define(i, s));

        let section = keyword("section")
            .ignore_then(ident().padded())
            .map(Token::Section);

        let label = ident()
            .then_ignore(just(':'))
            .map(Token::Label)
            .labelled("label");

        let directive = just(".")
            .ignore_then(choice((
                    cpu,
                    os,
                    define,
                    section,
            )))
            .labelled("directive");

        let comment = just(";")
            .padded()
            .ignore_then(none_of("\r\n").repeated().to_slice())
            .map(Token::Comment)
            .labelled("comment");

        let inst = custom(|inp| todo!("{:?}", inp.ctx()));

        choice((
            directive,
            label,
            comment,
            inst,
        )).then_ignore(newline().repeated())
    })
        .repeated()
        .collect()
        .with_ctx("")
}
