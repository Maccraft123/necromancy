use std::borrow::Cow;
use crate::parser;
use regex::Regex;

use nom::{
    IResult, Parser,
    error::context,
    character::complete::{
        not_line_ending, space1, line_ending, none_of, multispace1, space0, one_of,
    },
    bytes::complete::tag,
    combinator::{
        map, recognize, opt, value, cut, map_opt, verify, map_res,
    },
    branch::alt,
    multi::{
        many0, many1, many, many_till,
    },
    sequence::{
        preceded, pair, delimited, terminated,
    },
};

macro_rules! deb {
    ($parser: expr) => {
        map($parser, |v| dbg!(v))
    };
}

#[derive(Debug)]
enum TemplateToken<'input> {
    /// A literal value to paste
    Literal(&'input str),
    /// Substitution that would take an argument and paste it
    Sub(&'input str),
    /// Substitution with an index that would index into an argument and paste that
    Index(&'input str, u32),
}

impl TemplateToken<'_> {
    fn parse(input: &str) -> IResult<&str, TemplateToken> {
        alt((
            map(
                delimited(
                    tag("{"),
                    pair(
                        parser::ident,
                        opt(delimited(
                            tag("["),
                            nom::character::complete::u32,
                            tag("]"),
                        )),
                    ),
                    cut(tag("}")),
                ),
                |(v1, v2): (&str, Option<u32>)| {
                    dbg!((v1, v2));
                    if let Some(idx) = v2 {
                        TemplateToken::Index(v1, idx)
                    } else {
                        TemplateToken::Sub(v1)
                    }
                },
            ),
            map(
                recognize(many1(none_of("\r\n{}[]\\"))),
                |v: &str| TemplateToken::Literal(dbg!(v)),
            ),
        )).parse(input)
    }
}

#[derive(Debug)]
struct TemplateLine<'input> {
    line: &'input str,
    //vals: Vec<TemplateToken<'input>>,
}

impl TemplateLine<'_> {
    fn parse(input: &str) -> IResult<&str, TemplateLine> {
        map(
            preceded(
                verify(
                    pair(
                        deb!(opt((tag("->"), space1))),
                        deb!(opt((tag(r"\"), space0, line_ending))),
                    ),
                    |(v1, v2)| v1.is_some() || v2.is_some(),
                ),
                deb!(recognize(many1(none_of("\r\n\\")))),
            ),
            |line: &str| TemplateLine { line },
        ).parse(input)
    }
}

#[derive(Debug)]
struct Macro {
    regex: Regex,
    template: String,
}

impl Macro {
    fn replace_all<'me, 'cow>(&'me self, input: &mut Cow<'cow, str>) {
        let ret = self.regex.replace_all(&input, &self.template);
        match ret {
            Cow::Owned(v) => *input = Cow::Owned(v),
            Cow::Borrowed(_) => (),
        }
    }
    fn parse(input: &str) -> IResult<&str, Macro> {
        map_res(
            (
                parser::ident,
                opt(delimited(
                    tag("("),
                    many0(
                        (
                            deb!(parser::ident),
                            preceded(
                                (tag(":"), space0),
                                parser::ident,
                            ),
                            (opt(tag(",")), space0),
                        ),
                    ),
                    tag(")"),
                )),
                map(
                    many1(
                        preceded(
                            verify(
                                pair(
                                    opt((space1, tag("->"), space1)),
                                    opt((space0, tag(r"\"), space0, line_ending)),
                                ),
                                |(v1, v2)| v1.is_some() || v2.is_some(),
                            ),
                            recognize(many1(none_of("\r\n\\"))),
                        )
                    ),
                    |v: Vec<&str>| v.iter().fold(String::new(), |acc, val| acc + val),
                ),
            ),
            |(name, args, template)| {
                let mut pattern = String::new();
                // This matches the macro invocation:
                // We always have a name
                pattern += name;
                // Every argument has to be specified
                if let Some(args) = args {
                    for (i, arg) in args.iter().enumerate() {
                        // Allow a comma if it's not the first argument
                        if i != 0 {
                            pattern += ",?";
                        }
                        // There alwaus has to be a space
                        pattern += r"[[:space:]]+";
                        // Add a capture group for the argument
                        pattern += &match arg.1 {
                            "str" => format!("(?<{}>[[:word:]]+)", arg.0),
                            "int" => format!("(?<{}>[[:xdigit:]]+)", arg.0),
                            _ => panic!(),
                        };
                    }
                }

                Regex::new(&pattern)
                    .map(|regex| {
                        Self {
                            regex,
                            template,
                        }
                    })
            },
        ).parse(input)
    }
}

#[derive(Debug, Clone)]
enum ArgType {
    /// Argument is a string, if max length is specified then macros are
    /// allowed to index into it.
    Str(Option<u32>),
    /// Argument type is an int or an array of integers if length is specified
    Int(Option<u32>),
}

#[derive(Debug)]
struct MacroArg<'input> {
    name: &'input str,
    ty: ArgType,
}

impl<'input> MacroArg<'input> {
    fn parse(input: &'input str) -> IResult<&'input str, MacroArg<'input>> {
        map(
            pair(
                parser::ident,
                map_opt(
                    delimited(
                        tag("("),
                        pair(
                            parser::ident,
                            delimited(
                                tag("["),
                                opt(nom::character::complete::u32),
                                tag("]"),
                            ),
                        ),
                        tag(")"),
                    ),
                    |(ty, idx)| {
                        match ty {
                            "str" => Some(ArgType::Str(idx)),
                            "int" => Some(ArgType::Int(idx)),
                            _ => None,
                        }
                    },
                ),
            ),
            |(name, ty)| Self { name, ty },
        ).parse(input)
    }
}

#[derive(Debug)]
enum PreToken<'input> {
    Macro(Macro),
    Text(&'input str),
}

fn parse_macros(input: &str) -> IResult<&str, Vec<PreToken>> {
    many0(
        alt((
            map(
                preceded(
                    (tag(".define"), space1),
                    cut(Macro::parse),
                ),
                PreToken::Macro
            ),
            map(
                recognize(pair(not_line_ending, line_ending)),
                PreToken::Text,
            ),
        )),
    ).parse(input)
}

pub fn process(input: &str) -> Cow<str> {
    let parsed = parse_macros(input).unwrap().1;

    // Return early if there's no macros
    if parsed.iter().all(|v| matches!(v, PreToken::Text(_)) ) {
        panic!("No macro replacements needed???");
        return Cow::Borrowed(input);
    }
    dbg!(&parsed);

    let mut ret = Cow::Owned(String::new());
    let mut macros = Vec::new();
    for tok in parsed.into_iter() {
        match tok {
            PreToken::Text(v) => ret += v,
            PreToken::Macro(m) => macros.push(m),
        }
    }

    for m in macros {
        m.replace_all(&mut ret);
    }
    
    ret
}
