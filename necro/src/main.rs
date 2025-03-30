#![allow(unused_imports)]
use std::fs;
use std::borrow::Borrow;
use std::path::PathBuf;
use std::fmt::Debug;
use std::collections::{BTreeMap, BTreeSet};

use clap::Parser;

mod cpu;
//use cpu::{Cpu, Endian, ParseUtils, match_cpu};

//mod parser;
//use parser::{StringKind, Token};
//mod parser2;
//use parser2::Token;

mod lexer;
use lexer::Token;

//mod templater;

#[derive(Debug, Parser)]
struct Args {
    #[arg(short = 'v')]
    verbose: bool,

    file: PathBuf,
}

trait IntoBytes: Debug + Sized + Copy + Clone {
    type Output: IntoIterator<Item = u8>;
    fn to_be_bytes(self) -> Self::Output;
    fn to_le_bytes(self) -> Self::Output;
}

macro_rules! impl_traits {
    ($($ty: ty),*) => {
        $(
            impl IntoBytes for $ty {
                type Output = [u8; core::mem::size_of::<$ty>()];
                fn to_be_bytes(self) -> Self::Output { <$ty>::to_be_bytes(self) }
                fn to_le_bytes(self) -> Self::Output { <$ty>::to_le_bytes(self) }
            }
            impl IntoBytes for &$ty {
                type Output = [u8; core::mem::size_of::<$ty>()];
                fn to_be_bytes(self) -> Self::Output { <$ty>::to_be_bytes(*self) }
                fn to_le_bytes(self) -> Self::Output { <$ty>::to_le_bytes(*self) }
            }
/*            impl Operand for $ty {
                fn parse_emit<'i, 's>(input: &'i str, s: &'s mut impl SymbolResolver, k: SymbolKind, seg_off: usize) -> nom::IResult<&'i str, > {
                    nom::branch::alt((
                        <$ty>::parse_no_symbol_ref(input),
                        s.parse_and_reg_symbol(seg_off),
                    )).parse(input)
                }
            }*/
        )*
    }
}

impl_traits!(u8, u16, u32, u64, i8, i16, i32, i64);

trait Os: Debug {
    fn encode_str(&self, _: &str) -> Vec<u8>;
}
#[derive(Debug)]
struct Cpm;
impl Os for Cpm {
    fn encode_str(&self, s: &str) -> Vec<u8> {
        let mut ret: Vec<u8> = s.bytes().collect();
        ret.push(b'$');
        ret
    }
}

fn match_os(c: &str) -> Option<Box<dyn Os>> {
    match c {
        "dr,cp/m" => Some(Box::new(Cpm)),
        _ => None,
    }
}

#[derive(Debug)]
struct Section<'a> {
    name: &'a str,
    base: Option<usize>,
    data: Vec<u8>,
}

impl<'a> Section<'a> {
    fn new(name: &'a str, base: Option<usize>) -> Section<'a> {
        Section {
            name,
            base,
            data: Vec::new(),
        }
    }
    /*fn add_data<T, U>(&mut self, cpu: &Box<dyn Cpu>, d: T)
    where
        T: IntoIterator<Item = U>,
        U: IntoBytes,
    {
        match cpu.endian() {
            Endian::Big => {
                self.data.extend(
                    d.into_iter().map(|v| v.to_be_bytes()).flatten()
                );
            },
            Endian::Little => {
                self.data.extend(
                    d.into_iter().map(|v| v.to_le_bytes()).flatten()
                );
            },
        }
    }*/
}

fn escape<T: ToString>(t: T) -> String {
    let mut ret = String::new();
    for ch in t.to_string().chars() {
        if ret.ends_with(r"'\n'") {
            ret.push(ch);
        } else if ch.is_control() {
            ret.extend(ch.escape_default());
        } else {
            ret.push(ch)
        }
    }
    ret
}

fn main() {
            use ariadne::{
                sources, Color, Label, Report, ReportKind,
            };
    let args = Args::parse();
    let filename = args.file.to_string_lossy().to_string();

    let src = fs::read_to_string(args.file).unwrap();
    //let f = templater::process(&f);
    let (tokens, state) = lexer::parse(&src);
    dbg!(tokens);
    dbg!(state);
    //let (tokens, errs2) = p.parse(&ltokens).into_output_errors();


    /*
    let mut cpu = None;
    let mut os = None;
    let mut const_symbols = BTreeMap::new();
    let mut imports = BTreeSet::new();
    let mut exports = BTreeMap::new();
    let mut labels = BTreeSet::new();

    // Register symbols and labels first 
    for tok in &tokens {
        match tok {
            Token::SetCpu(compat) => cpu = match_cpu(compat),
            Token::SetOs(compat) => os = match_os(compat),
            Token::DefineSym(name, addr) => { const_symbols.insert(name, addr); },
            Token::ImportSym(name) => { imports.insert(name); },
            Token::ExportSym(name) => { exports.insert(name, None::<usize>); },
            Token::Label(name) => { labels.insert(name); },
            Token::Instruction(..) => (),
            _ => (),
        }
    }

    let cpu = cpu.take().unwrap();
    let os = os.take().unwrap();

    let names_iter = const_symbols.iter()
        .map(|s| s.0)
        .chain(imports.iter())
        .chain(labels.iter())
        .chain(exports.iter().map(|s| s.0));

    let mut pu = ParseUtils::new();
    for sym in names_iter {
        if !cpu.symbol_name_ok(sym) {
            panic!("Symbol name '{}' is invalid for {:?} CPU", sym, cpu);
        }
        pu.add_symbol(sym);
    }

    /* And now we can populate section data */
    let mut sections = Vec::new();
    let mut cur_section = None;
    macro_rules! to_cur_section {
        ($data: expr) => {
            cur_section.as_mut().unwrap().add_data(&cpu, $data)
        }
    }
    for tok in &tokens {
        match tok {
            Token::BeginSection(name, start) => {
                if let Some(section) = cur_section.take() {
                    sections.push(section);
                }
                cur_section = Some(Section::new(name, *start));
            },
            Token::Str(kind, d) => {
                match kind {
                    StringKind::ByteStr => {
                        let borrowed: &str = d.borrow();
                        to_cur_section!(borrowed.bytes());
                    },
                    StringKind::OsStr => {
                        to_cur_section!(os.encode_str(d));
                    },
                }
            },
            Token::Instruction(raw) => {
                let parsed = cpu.parse_instruction(raw, &pu).unwrap();
                println!("{:x?}", parsed);
                //to_cur_section!(parsed.1.encode(&mut pu));
            },
            Token::Imm8(..) => {
            },
            Token::Imm16(..) => {
            },
            Token::Imm32(..) => {
            },
            Token::SetCpu(..) | Token::SetOs(..) | Token::DefineSym(..) | Token::ImportSym(..) |
                Token::ExportSym(..) | Token::Label(..) | Token::Comment(..) => (),
        }
    }

    if let Some(section) = cur_section.take() {
        sections.push(section);
    }


    println!("CPU: {cpu:?}");
    println!("OS/ABI: {os:?}");
    println!("Constant symbols:");
    println!("{:#x?}", const_symbols);
    println!("Imported symbols:");
    println!("{:#x?}", imports);
    println!("Exported symbols:");
    println!("{:#x?}", exports);
    println!("Labels:");
    println!("{:#x?}", labels);
    println!("Sections:");
    println!("{:#x?}", sections);*/

}
