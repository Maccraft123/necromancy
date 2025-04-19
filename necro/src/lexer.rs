use std::borrow::Cow;
use std::fmt;
use std::ops::Range;

use winnow::{
    ascii::{alphanumeric1, escaped, line_ending, multispace0, multispace1, till_line_ending},
    combinator::{
        alt, cut_err, delimited, dispatch, fail, not, opt, preceded, repeat, separated, terminated,
        repeat_till, eof,
    },
    error::{ContextError, ModalResult, ParseError, StrContext, StrContextValue},
    prelude::*,
    token::{none_of, one_of, take, take_while},
    LocatingSlice,
};

#[derive(Debug)]
pub struct Error {
    messages: Vec<String>,
    span: Range<usize>,
}

impl Error {
    fn from_parse(error: ParseError<LocatingSlice<&str>, ContextError>) -> Error {
        let span = error.char_span();
        let ctx_err = error.into_inner();
        let messages = ctx_err.context().map(|e| e.to_string()).collect();

        Self { messages, span }
    }
    pub fn display(&self, src: &str) -> String {
        use annotate_snippets::{Annotation, Level, Renderer, Snippet};
        let msg = if self.messages.is_empty() {
            Level::Error
                .title("sorry, there's no error message")
                .snippet(
                    Snippet::source(src).fold(true).annotation(
                        Level::Error
                            .span(self.span.clone())
                            .label("but i know something's wrong around here"),
                    ),
                )
        } else {
            let title = self.messages.iter().last().unwrap();

            let annotations: Vec<Annotation> = self
                .messages
                .iter()
                .rev()
                .skip(1)
                .map(|msg| Level::Error.span(self.span.clone()).label(msg))
                .collect();

            let snip = Snippet::source(src).fold(true).annotations(annotations);

            Level::Error.title(title).snippet(snip)
        };

        let ret = Renderer::styled().render(msg).to_string();
        ret
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NumKind {
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
}

impl NumKind {
    fn from_str_radix(s: &str, r: u32) -> Result<Self, std::num::ParseIntError> {
        match u64::from_str_radix(s, r) {
            Ok(v) => {
                if let Some(v8) = v.try_into().ok() {
                    Ok(Self::U8(v8))
                } else if let Some(v16) = v.try_into().ok() {
                    Ok(Self::U16(v16))
                } else if let Some(v32) = v.try_into().ok() {
                    Ok(Self::U32(v32))
                } else {
                    Ok(Self::U64(v))
                }
            },
            Err(uerr) => {
                match i64::from_str_radix(s, r) {
                    Ok(v) => {
                        if let Some(v8) = v.try_into().ok() {
                            Ok(Self::I8(v8))
                        } else if let Some(v16) = v.try_into().ok() {
                            Ok(Self::I16(v16))
                        } else if let Some(v32) = v.try_into().ok() {
                            Ok(Self::I32(v32))
                        } else {
                            Ok(Self::I64(v))
                        }
                    },
                    Err(_) => Err(uerr),
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum BracketKind {
    Round,
    Square,
    Curly,
    Angle,
}

#[derive(Debug, Clone)]
pub enum BracketDir {
    Opening,
    Closing,
}

#[derive(Debug, Clone)]
pub enum TokenKind<'src> {
    /// An assembly directive, `.set`
    Directive(&'src str),
    /// A label, `entry:`
    Label(&'src str),
    /// A literal string, `"string"``
    LiteralStr(Cow<'src, str>),
    /// A literal number, `0xcafecafe`
    LiteralNum(NumKind),
    /// `+` character
    Plus,
    /// `-` character
    Minus,
    /// `*` character
    Mult,
    /// `/` character
    Div,
    /// A bracket, opening or closing
    Bracket(BracketKind, BracketDir),
    /// A comma, ','. Sometimes architecture syntax mandates a comma
    Comma,
    /// An identifier, kinda a "catch-all" for instructions, operands, etc
    Ident(&'src str),
}

impl<'src> TokenKind<'src> {
    fn to_directive(self) -> Self {
        match self {
            Self::Ident(inner) => Self::Directive(inner),
            _ => unreachable!(),
        }
    }
    fn to_label(self) -> Self {
        match self {
            Self::Ident(inner) => Self::Directive(inner),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token<'src> {
    kind: TokenKind<'src>,
    span: Range<usize>,
}

impl<'src> Token<'src> {
    fn new(kind: TokenKind<'src>, span: Range<usize>) -> Self {
        Self {
            kind,
            span,
        }
    }
}

fn ctx_label(name: &'static str) -> StrContext {
    StrContext::Label(name)
}

fn ctx_desc(desc: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(desc))
}

fn string<'a>(input: &mut LocatingSlice<&'a str>) -> ModalResult<TokenKind<'a>> {
    alt((
        terminated(
            none_of(['\r', '\n', '\t', '\\', '"']).take(),
            '"',
        ).map(|v| Cow::Borrowed(v)),
        terminated(
            repeat(
                0..,
                alt((
                    none_of(['\r', '\n', '\t', '\\', '"']),
                    "\\r".value('\r'),
                    "\\n".value('\n'),
                    "\\t".value('\t'),
                    "\\\\".value('\\'),
                )),
            ),
            '"',
        ).map(|v| Cow::Owned(v)),
    ))
    .context(ctx_label("string"))
    .map(TokenKind::LiteralStr)
    .parse_next(input)
}

fn number<'a>(input: &mut LocatingSlice<&'a str>) -> ModalResult<TokenKind<'a>> {
    alt((
        dispatch! {take(2usize);
            "0x" => cut_err(take_while(1.., |v| char::is_digit(v, 16))
                    .try_map(|n| NumKind::from_str_radix(n, 16))
                )
                .context(ctx_label("digit"))
                .context(ctx_desc("hexadecimal")),

            "0o" => cut_err(take_while(1.., |v| char::is_digit(v, 8))
                    .try_map(|n| NumKind::from_str_radix(n, 8))
                )
                .context(ctx_label("digit"))
                .context(ctx_desc("octal")),

            "0d" => cut_err(take_while(1.., |v| char::is_digit(v, 10))
                    .try_map(|n| NumKind::from_str_radix(n, 10))
                )
                .context(ctx_label("digit"))
                .context(ctx_desc("decimal")),
            
            "0b" => cut_err(take_while(1.., |v| char::is_digit(v, 2))
                    .try_map(|n| NumKind::from_str_radix(n, 2))
                )
                .context(ctx_label("digit"))
                .context(ctx_desc("binary")),
            _ => fail,
        },
        take_while(1.., |v| char::is_digit(v, 10))
            .try_map(|n| NumKind::from_str_radix(n, 10))
            .context(ctx_label("digit"))
            .context(ctx_desc("decimal")),
    ))
    .map(TokenKind::LiteralNum)
    .parse_next(input)
}

fn ident<'a>(input: &mut LocatingSlice<&'a str>) -> ModalResult<TokenKind<'a>> {
    (
        one_of(|c: char| c.is_alphabetic() || c == '_')
            .context(ctx_desc("alphabetical character or _")),
        take_while(0.., |c: char| c.is_alphanumeric() || c == '_')
            .context(ctx_desc("alphanumeric character or '_'")),
    )
        .context(ctx_label("identifier"))
        .take()
        .map(TokenKind::Ident)
        .parse_next(input)
}

pub fn lex<'src>(input: &'src str) -> Result<Vec<Token<'src>>, Error> {
    let mut input = LocatingSlice::new(input);
    repeat_till(
        0..,
        terminated(
            alt((
                preceded('.', cut_err(ident)).map(TokenKind::to_directive),
                preceded('"', cut_err(string)),
                number,
                terminated(ident, ':').map(TokenKind::to_label),
                '('.value(TokenKind::Bracket(BracketKind::Round, BracketDir::Opening)),
                ')'.value(TokenKind::Bracket(BracketKind::Round, BracketDir::Closing)),
                '['.value(TokenKind::Bracket(BracketKind::Square, BracketDir::Opening)),
                ']'.value(TokenKind::Bracket(BracketKind::Square, BracketDir::Closing)),
                '<'.value(TokenKind::Bracket(BracketKind::Angle, BracketDir::Opening)),
                '>'.value(TokenKind::Bracket(BracketKind::Angle, BracketDir::Closing)),
                '{'.value(TokenKind::Bracket(BracketKind::Curly, BracketDir::Opening)),
                '}'.value(TokenKind::Bracket(BracketKind::Curly, BracketDir::Closing)),
                '+'.value(TokenKind::Plus),
                '-'.value(TokenKind::Minus),
                '*'.value(TokenKind::Mult),
                '/'.value(TokenKind::Div),
                ','.value(TokenKind::Comma),
                cut_err(ident),
            )).with_span().map(|(c, s)| Token::new(c, s)),
            repeat(
                0..,
                alt((multispace1.void(), (';', till_line_ending).void())),
            )
            .map(|()| ()),
        ),
        eof,
    )
        .map(|(tokens, _rest)| tokens)
    .parse(input)
    .map_err(|e| Error::from_parse(e))
}

#[cfg(test)]
mod tests {
    use super::NumKind;
    #[test]
    fn num_radices() {
        assert_eq!(NumKind::from_str_radix("10", 10), Ok(NumKind::U8(10)));
        assert_eq!(NumKind::from_str_radix("10", 16), Ok(NumKind::U8(0x10)));
        assert_eq!(NumKind::from_str_radix("10", 2), Ok(NumKind::U8(0b10)));
        assert_eq!(NumKind::from_str_radix("10", 8), Ok(NumKind::U8(0o10)));
    }
    #[test]
    fn num_coertion() {
        assert_eq!(NumKind::from_str_radix(&(u8::MAX).to_string(), 10), Ok(NumKind::U8(u8::MAX)));
        assert_eq!(NumKind::from_str_radix(&(u16::MAX).to_string(), 10), Ok(NumKind::U16(u16::MAX)));
        assert_eq!(NumKind::from_str_radix(&(u32::MAX).to_string(), 10), Ok(NumKind::U32(u32::MAX)));
        assert_eq!(NumKind::from_str_radix(&(u64::MAX).to_string(), 10), Ok(NumKind::U64(u64::MAX)));

        assert_eq!(NumKind::from_str_radix(&(i8::MIN).to_string(), 10), Ok(NumKind::I8(i8::MIN)));
        assert_eq!(NumKind::from_str_radix(&(i16::MIN).to_string(), 10), Ok(NumKind::I16(i16::MIN)));
        assert_eq!(NumKind::from_str_radix(&(i32::MIN).to_string(), 10), Ok(NumKind::I32(i32::MIN)));
        assert_eq!(NumKind::from_str_radix(&(i64::MIN).to_string(), 10), Ok(NumKind::I64(i64::MIN)));
    }
}
