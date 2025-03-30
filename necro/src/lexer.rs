use winnow::{
    Stateful, prelude::*,
    combinator::{
        alt, repeat, delimited, dispatch, cut_err, fail, preceded, opt, separated_pair,
        terminated,
    },
    error::{
        ContextError, StrContext, ParseError, ParserError, FromExternalError, AddContext
    },
    ascii::{
        alpha1, alphanumeric1, space0, multispace1, space1, till_line_ending, multispace0,
    },
    token::{
        take_till, take_while, one_of, none_of, take_until, rest, take,
    },
};

use std::num::ParseIntError;
use std::borrow::Cow;
use std::collections::BTreeMap;
use crate::cpu::{Cpu, DynInstruction};

#[derive(Debug, Clone)]
pub enum Token<'a> {
    SetStr(&'a str, &'a str),
    SetNum(&'a str, u32),
    Section(&'a str),
    Import(&'a str),
    Export(&'a str),
    Label(&'a str),
    Comment(&'a str),
    SetCpu(Cpu),
    Instruction(crate::cpu::DynInstruction<'a>),
}

//#[derive(Debug, Clone)]
//struct SpannedToken<'a>(pub Token, (usize, Range<usize>));

#[derive(Debug)]
pub struct ParseState<'a> {
    cpu: Cpu,
    imports: Vec<&'a str>,
    exports: Vec<&'a str>,
    sets: BTreeMap<&'a str, Cow<'a, str>>,
}

impl<'a> ParseState<'a> {
    fn new() -> Self {
        Self {
            cpu: Cpu::None,
            imports: Vec::new(),
            exports: Vec::new(),
            sets: BTreeMap::new(),
        }
    }
}

pub type Stream<'input, 'state> = Stateful<&'input str, &'state mut ParseState<'input>>;

fn string<'a>(input: &mut Stream<'a, '_>) -> ModalResult<&'a str> {
    delimited('"', take_until(0.., '"'), '"')
        .context(StrContext::Label("string"))
        .parse_next(input)
}

pub fn ident<'a>(input: &mut Stream<'a, '_>) -> ModalResult<&'a str> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_')
    )
    .take()
    .context(StrContext::Label("identifier"))
    .parse_next(input)
}

pub trait ParseNum: Sized + Copy + Clone {
    fn parse<'a>(_: &mut Stream<'a, '_>) -> ModalResult<Self>;
}

macro_rules! impl_parse_num {
    ($($t: ty),*) => {
        $(
            impl ParseNum for $t {
                fn parse<'a>(input: &mut Stream<'a, '_>) -> ModalResult<$t> {
                    alt((
                        dispatch! {take(2usize);
                            "0x" => take_while(1.., |v| char::is_digit(v, 16)).try_map(|s| <$t>::from_str_radix(s, 16))
                                .context(StrContext::Label("hex digit")),
                            "0o" => take_while(1.., |v| char::is_digit(v, 8)).try_map(|s| <$t>::from_str_radix(s, 8))
                                .context(StrContext::Label("octal digit")),
                            "0d" => take_while(1.., |v| char::is_digit(v, 10)).try_map(|s| <$t>::from_str_radix(s, 10))
                                .context(StrContext::Label("decimal digit")),
                            "0b" => take_while(1.., |v| char::is_digit(v, 2)).try_map(|s| <$t>::from_str_radix(s, 2))
                                .context(StrContext::Label("binary digit")),
                            _ => fail,
                        },
                        take_while(1.., |v| char::is_digit(v, 2)).try_map(|s| <$t>::from_str_radix(s, 2))
                                .context(StrContext::Label("decimal digit")),
                    )).parse_next(input)
                }
            }
        )*
    }
}

impl_parse_num!(u8, u16, u32, u64);

pub fn number<'a, T: ParseNum>(input: &mut Stream<'a, '_>) -> ModalResult<T> {
    T::parse(input)
}

fn parse_set<'a>(input: &mut Stream<'a, '_>) -> ModalResult<Token<'a>> {
    let ret = alt((
        preceded(("cpu", space1), cut_err(Cpu::parse)).map(Token::SetCpu),
        separated_pair(ident, space1, string).map(|(n, v)| Token::SetStr(n, v)),
        separated_pair(ident, space1, number).map(|(n, v)| Token::SetNum(n, v)),
    )).parse_next(input)?;

    match &ret {
        Token::SetCpu(c) => input.state.cpu = *c,
        Token::SetStr(name, value) => { input.state.sets.insert(name, Cow::Borrowed(value)); },
        Token::SetNum(name, value) => { input.state.sets.insert(name, Cow::Owned(value.to_string())); },
        _ => (),
    }

    Ok(ret)
}

fn instruction<'a, 'b>(input: &'b mut Stream<'a, '_>) -> ModalResult<Token<'a>> {
    let cpu = input.state.cpu;
    let ret = cpu.instruction(input).map(Token::Instruction);
    ret
}

fn parse_token<'a>(input: &mut Stream<'a, '_>) -> ModalResult<Token<'a>> {
    delimited(
        multispace0,
        alt((
            preceded((".set", space1), cut_err(parse_set)),
            preceded((".section", space1), cut_err(ident)).map(Token::Section),
            terminated(ident, ":").map(Token::Label),
            preceded(";", till_line_ending).map(Token::Comment),
            cut_err(instruction.context(StrContext::Label("instruction"))),
        )),
        multispace1,
    ).parse_next(input)
}

pub fn parse(input: &str) -> (ModalResult<Vec<Token>>, ParseState) {
    let mut state = ParseState::new();
    let mut stream = Stateful {input, state: &mut state};
    let ret = repeat(
        0..,
        parse_token,
    ).parse_next(&mut stream);

    (ret, state)
}
