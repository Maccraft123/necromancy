#![allow(unused_imports)]
use std::fs;
use std::borrow::Borrow;
use std::path::PathBuf;
use std::fmt::Debug;
use std::collections::{BTreeMap, BTreeSet};
use std::mem;

use clap::Parser;
use num_traits::FromPrimitive;

mod cpu;
use cpu::MaybeSymbol;
use cpu::TypeEndian;
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
pub struct SymbolRegistry<'a, 'b> {
    strings: BTreeMap<&'a str, &'b str>,
    numbers: BTreeMap<&'a str, u32>,
    imports: BTreeSet<&'a str>,
}

impl<'a, 'b> SymbolRegistry<'a, 'b> {
    fn new() -> Self {
        Self {
            strings: BTreeMap::new(),
            numbers: BTreeMap::new(),
            imports: BTreeSet::new(),
        }
    }
    fn get<'me, T: FromPrimitive>(&'me mut self, name: &str) -> T {
        if let Some(val) = self.numbers.get(name) {
            T::from_u32(*val).unwrap()
        } else {
            eprintln!("todo: adding symbols, requested {name}");
            T::from_u32(0).unwrap()
        }
    }
    fn import<'me>(&'me mut self, name: &'a str) {
        self.imports.insert(name);
    }
    fn insert_num<'me>(&'me mut self, name: &'a str, value: u32) {
        self.numbers.insert(name, value);
    }
    fn insert_str<'me>(&'me mut self, name: &'a str, value: &'b str) {
        self.strings.insert(name, value);
    }
}

#[derive(Debug)]
struct SymbolReference {
    name: String,
    endian: cpu::Endian,
    mask: u32,
    len: u8,
    correction: i8,
    shift: i8,
}

impl SymbolReference {
    fn simple_len(name: String, endian: cpu::Endian, len: u8) -> Self {
        Self {
            name,
            endian,
            len,
            correction: 0,
            shift: 0,
            mask: 0xffff_ffff,
        }
    }
    fn simple_u8(name: String, endian: cpu::Endian) -> Self {
        Self::simple_len(name, endian, mem::size_of::<u8>() as u8)
    }
}

/*impl<'a, E: TypeEndian, T: Sized + Clone> From<MaybeSymbol<'a, E, T>> for SymbolReference<'a> {
    fn from(t: MaybeSymbol<'a, E, T>) -> Self {
        let MaybeSymbol::Sym(name, _) = t else { unreachable!() };
        Self {
            name,
            endian: E::endian(),
            len: mem::size_of::<T>() as u8,
            correction: 0,
            shift: 0,
            mask: 0xffff_ffff,
        }
    }
}*/

#[derive(Debug)]
struct Section<'a> {
    name: &'a str,
    base: Option<usize>,
    data: Vec<u8>,
    // usize is the offset from start of the segment
    syms: Vec<(usize, SymbolReference)>
}

impl<'a> Extend<u8> for Section<'a> {
    fn extend<I: IntoIterator<Item = u8>>(&mut self, i: I) {
        self.data.extend(i)
    }
}

impl<'a, 'i> Extend<&'i u8> for Section<'a> {
    fn extend<I: IntoIterator<Item = &'i u8>>(&mut self, i: I) {
        self.data.extend(i)
    }
}

impl<'a> Section<'a> {
    fn new(name: &'a str, base: Option<usize>) -> Section<'a> {
        Section {
            name,
            base,
            data: Vec::new(),
            syms: Vec::new(),
        }
    }
    fn add_sym<T, E: TypeEndian>(&mut self, name: String) {
        let s = SymbolReference::simple_len(name, E::endian(), mem::size_of::<T>() as u8);
        self.add_sym_ref(s);
    }
    fn add_sym_ref(&mut self, s: SymbolReference) {
        self.syms.push((self.data.len(), s));
    }
    fn push(&mut self, d: u8) {
        self.data.push(d);
    }
    fn last_mut(&mut self) -> &mut u8 {
        self.data.last_mut().unwrap()
    }
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
    let tokens = tokens.unwrap();

    let mut reg = SymbolRegistry::new();

    for t in tokens.iter() {
        match &t {
            Token::SetStr(name, value) => reg.insert_str(name, value),
            Token::SetNum(name, value) => reg.insert_num(name, *value),
            Token::Import(name) => reg.import(name),
            Token::Label(name) => reg.import(name),
            _ => (),
        }
    }

    println!("symbols: {:?}", reg);

    let mut sections = Vec::new();
    let mut cur_section = None;
    use cpu::EncodeInstruction;
    for t in tokens.iter() {
        match t {
            Token::Section(name, base) => {
                if let Some(s) = cur_section.take() {
                    sections.push(s);
                }
                let base = base.map(|v| v as usize);
                cur_section = Some(Section::new(name, base));
            },
            Token::Instruction(i) => {
                if let Some(s) = cur_section.as_mut() {
                    i.encode(s, &mut reg);
                } else {
                    eprintln!("no section but tried to emit instruction");
                    todo!("error handling");
                }
            },
            _ => (),
        }
    }
    if let Some(s) = cur_section.take() {
        sections.push(s);
    }


    println!("{:#x?}", sections);

/*    println!("CPU: {cpu:?}");
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
