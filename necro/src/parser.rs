use winnow::{
    Stateful, prelude::*,
    combinator::{
        alt, repeat, delimited, dispatch, cut_err, fail, preceded, opt, separated_pair,
        terminated,
    },
    error::{
        ContextError,
    },
    ascii::{
        multispace1, space1, till_line_ending, multispace0,
    },
    token::{
        take_while, one_of, take_until, take,
    },
};

use std::mem;
use std::ops::Range;
use std::borrow::Cow;
use std::collections::BTreeMap;
use crate::cpu::Cpu;
use annotate_snippets::{Level, Message};

#[derive(Debug)]
pub struct ParseError<'a> {
    header: String,
    annotations: Vec<Message<'a>>,
    span: Range<usize>,
}

impl<'a> ParseError<'a> {
    pub fn header(&self) -> &str {
        &self.header
    }
    pub fn take_annotations(&mut self) -> Vec<Message<'a>> {
        mem::take(&mut self.annotations)
    }
    pub fn span(&self) -> &Range<usize> {
        &self.span
    }
}

#[derive(Debug)]
pub enum Token<'a> {
    SetStr(&'a str, &'a str),
    SetNum(&'a str, u32),
    Section(&'a str, Option<u64>),
    Label(&'a str),
    Comment,
    SetCpu(Spanned<Cpu>),
    //Instruction(Spanned<crate::cpu::DynInstruction<'a>>),
    Instruction(crate::cpu::DynInstruction<'a>),
    Error(ParseError<'a>),
}


#[derive(Debug, Clone)]
pub struct Spanned<T: Sized>(T, Range<usize>);

impl<T: Sized> Spanned<T> {
    pub fn inner(&self) -> &T {
        &self.0
    }
    pub fn span(&self) -> &Range<usize> {
        &self.1
    }
}

#[derive(Debug)]
pub struct ParseState<'a> {
    cpu: Option<Spanned<Cpu>>,
    sets: BTreeMap<&'a str, Cow<'a, str>>,
}

impl<'a> ParseState<'a> {
    fn new() -> Self {
        Self {
            cpu: None,
            sets: BTreeMap::new(),
        }
    }
}

use winnow::LocatingSlice;
pub type Stream<'input, 'state> = Stateful<LocatingSlice<&'input str>, &'state mut ParseState<'input>>;


#[derive(Debug, Clone)]
pub enum ParseContext {
    Label(&'static str),
    CurrentCpu(Spanned<Cpu>),
    Annotation(Level, Range<usize>, String),
}

pub type Error = ContextError<ParseContext>;

fn string<'a>(input: &mut Stream<'a, '_>) -> ModalResult<&'a str, Error> {
    delimited('"', take_until(0.., '"'), '"')
        .context(ParseContext::Label("string"))
        .parse_next(input)
}

pub fn ident<'a>(input: &mut Stream<'a, '_>) -> ModalResult<&'a str, Error> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_')
    )
    .take()
    .context(ParseContext::Label("identifier"))
    .parse_next(input)
}

pub trait ParseNum: Sized + Copy + Clone {
    fn parse<'a>(_: &mut Stream<'a, '_>) -> ModalResult<Self, Error>;
}

macro_rules! impl_parse_num {
    ($($t: ty),*) => {
        $(
            impl ParseNum for $t {
                fn parse<'a>(input: &mut Stream<'a, '_>) -> ModalResult<$t, Error> {
                    alt((
                        dispatch! {take(2usize);
                            "0x" => take_while(1.., |v| char::is_digit(v, 16)).try_map(|s| <$t>::from_str_radix(s, 16))
                                .context(ParseContext::Label("hex digit")),
                            "0o" => take_while(1.., |v| char::is_digit(v, 8)).try_map(|s| <$t>::from_str_radix(s, 8))
                                .context(ParseContext::Label("octal digit")),
                            "0d" => take_while(1.., |v| char::is_digit(v, 10)).try_map(|s| <$t>::from_str_radix(s, 10))
                                .context(ParseContext::Label("decimal digit")),
                            "0b" => take_while(1.., |v| char::is_digit(v, 2)).try_map(|s| <$t>::from_str_radix(s, 2))
                                .context(ParseContext::Label("binary digit")),
                            _ => fail,
                        },
                        take_while(1.., |v| char::is_digit(v, 10)).try_map(|s| <$t>::from_str_radix(s, 10))
                                .context(ParseContext::Label("decimal digit")),
                    )).parse_next(input)
                }
            }
        )*
    }
}

impl_parse_num!(u8, u16, u32, u64);

pub fn number<'a, T: ParseNum>(input: &mut Stream<'a, '_>) -> ModalResult<T, Error> {
    T::parse(input)
}

fn parse_section<'a>(input: &mut Stream<'a, '_>) -> ModalResult<Token<'a>, Error> {
    (
        ident,
        opt(preceded(space1, number)),
    )
        .map(|(name, base)| Token::Section(name, base))
        .parse_next(input)
}

fn cpu<'a>(input: &mut Stream<'a, '_>) -> ModalResult<Token<'a>, Error> {
    let cpu = Cpu::parse
        .with_span()
        .map(|(cpu, span)| Spanned(cpu, span))
        .parse_next(input)?;

    input.state.cpu = Some(cpu.clone());

    Ok(Token::SetCpu(cpu))
}

fn parse_set<'a>(input: &mut Stream<'a, '_>) -> ModalResult<Token<'a>, Error> {
    let ret = alt((
        separated_pair(ident, space1, string).map(|(n, v)| Token::SetStr(n, v)),
        separated_pair(ident, space1, number).map(|(n, v)| Token::SetNum(n, v)),
    ))
        .parse_next(input)?;

    match &ret {
        Token::SetStr(name, value) => { input.state.sets.insert(name, Cow::Borrowed(value)); },
        Token::SetNum(name, value) => { input.state.sets.insert(name, Cow::Owned(value.to_string())); },
        _ => (),
    }

    Ok(ret)
}

fn instruction<'a, 'b>(input: &'b mut Stream<'a, '_>) -> ModalResult<Token<'a>, Error> {
    let ret = if let Some(cpu) = input.state.cpu.clone() {
        cpu.0
            .map(Token::Instruction)
            .context(ParseContext::CurrentCpu(cpu))
            .parse_next(input)
    } else {
        till_line_ending
            .span()
            .map(|span| {
                let annotations = vec![
                    Level::Error.title("No CPU architecture set"),
                    Level::Note.title("There is no default CPU architecture set"),
                ];

                let e = ParseError {
                    header: "Failed to parse an instruction".to_string(),
                    span: span,
                    annotations,
                };

                Token::Error(e)
            })
            .parse_next(input)
    };
    ret
}

fn directive<'a>(input: &mut Stream<'a, '_>) -> ModalResult<Token<'a>, Error> {
    use winnow::combinator::peek;
    use winnow::ascii::space0;

    // we always are before a dot and don't get that dot in input.
    // to make error messages modify our span a lil bit
    let span = peek(till_line_ending.span())
        .map(|range| Range { start: range.start - 1, end: range.end })
        .parse_next(input)?;
    dispatch! {terminated(ident, space0);
        "set" => cut_err(parse_set),
        "cpu" => cut_err(cpu),
        "section" => cut_err(parse_section),
        _ => fail.context(ParseContext::Annotation(Level::Error, span.clone(), "Invalid directive".to_string())),
    }.parse_next(input)
}

fn parse_token<'a>(input: &mut Stream<'a, '_>) -> ModalResult<Token<'a>, Error> {
    delimited(
        multispace0,
        alt((
            preceded('.', cut_err(directive)),

            terminated(ident, ":")
                .map(Token::Label),

            preceded(";", till_line_ending)
                .map(|_| Token::Comment),

            cut_err(instruction.context(ParseContext::Label("instruction"))),
        )),
        multispace1,
    ).parse_next(input)
}

pub fn parse(input: &str) -> (ModalResult<Vec<Token>, Error>, ParseState) {
    let mut state = ParseState::new();
    let mut stream = Stateful {
        input: LocatingSlice::new(input),
        state: &mut state,
    };
    let ret = repeat(
        0..,
        parse_token,
    ).parse_next(&mut stream);

    (ret, state)
}
