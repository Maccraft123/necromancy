use std::fmt::Debug;
use std::marker::PhantomData;
use crate::parser::{self, ParseNum};
use num_traits::{FromPrimitive, ToBytes};
use crate::section::{SymbolRegistry, Section};

mod i8080;

use necro_derive::ParseOperand;
/// A common trait for parsing any kind of operand
pub trait ParseOperand<'a>: Sized {
    fn parse(input: &mut parser::Stream<'a, '_>) -> ModalResult<Self, crate::parser::Error>;
}

impl<'a, T: ParseNum> ParseOperand<'a> for T {
    fn parse(input: &mut parser::Stream<'a, '_>) -> ModalResult<T, crate::parser::Error> {
        parser::number.parse_next(input)
    }
}

impl<'a, T: ParseNum> ParseOperand<'a> for MaybeSymbolNoEndian<'a, T> {
    fn parse(input: &mut parser::Stream<'a, '_>) -> ModalResult<MaybeSymbolNoEndian<'a, T>, crate::parser::Error> {
        alt((
            parser::number.map(MaybeSymbolNoEndian::new_literal),
            parser::ident.map(MaybeSymbolNoEndian::new_symbol),
        )).parse_next(input)
    }
}

impl<'a, E: TypeEndian, T: ParseNum> ParseOperand<'a> for MaybeSymbol<'a, E, T> {
    fn parse(input: &mut parser::Stream<'a, '_>) -> ModalResult<MaybeSymbol<'a, E, T>, crate::parser::Error> {
        alt((
            parser::number.map(MaybeSymbol::new_literal),
            parser::ident.map(MaybeSymbol::new_symbol),
        )).parse_next(input)
    }
}

use necro_derive::EncodeOperand;
/// A shared trait for encoding an operand.
/// T is the primitive type the operand encodes into.
///
/// There is a derive macro for implementing this trait on enums.
/// Type attributes:
/// #[repr(type)]: Required, specifies the primitive type parameter.
///
/// Variant attributes:
/// #[operand(alias = "aaa")] Adds "aaa" and "AAA" as aliases for the variant
/// #[operand(rename = "aaa")] Uses "aaa" "AAA" instead of variant name for parsing
pub trait EncodeOperand<T>: Sized + Clone {
    /// Encodes self.
    /// target: The output section
    /// take the reference from last element and unwrap it without error handling
    /// shift: Amount of left-shifts for bitfield-operands
    /// map: Optional mapping function applied before shifting
    /// reg: Symbol registration struct
    fn encode_into(&self,
        target: &mut Section,
        shift: u32,
        map: Option<fn(Self) -> T>,
        reg: &mut SymbolRegistry,
    );
}

use std::ops::Shl;
impl<'a, E: TypeEndian, T: ToBytes + Clone + Shl<usize, Output = T> + FromPrimitive> EncodeOperand<T> for MaybeSymbol<'a, E, T> {
    fn encode_into(&self, tgt: &mut Section, shift: u32, map: Option<fn(Self) -> T>, reg: &mut SymbolRegistry) {
        let mut lit: T = match self {
            Self::Sym(symbol, _) => {
                tgt.symbol::<T, E>(reg, symbol.to_string())
                //tgt.add_sym::<T, E>(symbol.to_string());
                //reg.get(symbol)
            }
            Self::Literal(lit) => lit.clone(),
        };
        if let Some(m) = map {
            lit = m(self.clone());
        }
        tgt.extend(
            E::to_bytes(lit << shift as usize).as_ref()
        );
    }
}

use necro_derive::ParseInstruction;
/// Common trait for parsing instructions.
///
/// Derive macro is available for instructions that are represented as enums.
///
/// Variant attributes:
/// #[parse(fuse_first_field)]:
///     Skips space for the first field, and comma for second field.
///     When parsing this gives the appearance of instruction name and first field being fused
///     together into one word
///
pub trait ParseInstruction<'a>: Sized {
    fn parse<'b>(input: &'b mut parser::Stream<'a, '_>) -> ModalResult<Self, crate::parser::Error> where Self: 'a;
}

use necro_derive::EncodeInstruction;
/// Common trait for encoding instructions.
///
/// Derive macro is available for instructions that are represented as enums.
///
/// Type attributes:
/// #[repr(primitive)]: Required, sets the base instruction size
/// #[encode(endian = "little"/"big")]: Required, sets endian for encoding variant discriminant
///
/// Field attributes:
/// #[encode(shift = expr)]: Sets the left-shift amount to expr. Can be a literal or const variable
/// #[encode(with = path)]: Calls path when encoding instructions for manual implementation of
/// operand encoding
/// #[encode(map = expr)]: Passes map closure when encoding the operand to EncodeOperand trait
/// implementation
///
pub trait EncodeInstruction: Sized {
    fn encode(&self, s: &mut Section, r: &mut SymbolRegistry);
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Big;
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Little;

pub trait TypeEndian: Clone {
    fn to_bytes<T: ToBytes>(_: T) -> T::Bytes;
    fn endian() -> Endian;
}

impl TypeEndian for Big {
    fn to_bytes<T: ToBytes>(val: T) -> T::Bytes {
        val.to_be_bytes()
    }
    fn endian() -> Endian {
        Endian::Big
    }
}

impl TypeEndian for Little {
    fn to_bytes<T: ToBytes>(val: T) -> T::Bytes {
        val.to_le_bytes()
    }
    fn endian() -> Endian {
        Endian::Little
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum MaybeSymbolNoEndian<'a, T: Sized + Clone> {
    Sym(&'a str),
    Literal(T),
}

impl<'a, T: Sized + Clone> MaybeSymbolNoEndian<'a, T> {
    pub fn new_symbol(s: &'a str) -> Self {
        Self::Sym(s)
    }
    pub fn new_literal(l: T) -> Self {
        Self::Literal(l)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MaybeSymbol<'a, E: TypeEndian, T: Sized + Clone> {
    Sym(&'a str, PhantomData<E>),
    Literal(T),
}

impl<'a, E: TypeEndian,  T: Sized + Clone> MaybeSymbol<'a, E, T> {
    pub fn new_symbol(s: &'a str) -> Self {
        Self::Sym(s, PhantomData)
    }
    pub fn new_literal(l: T) -> Self {
        Self::Literal(l)
    }
}

impl<'a, E: TypeEndian, T: Sized + Clone> From<MaybeSymbolNoEndian<'a, T>> for MaybeSymbol<'a, E, T> {
    fn from(v: MaybeSymbolNoEndian<'a, T>) -> Self {
        match v {
            MaybeSymbolNoEndian::Sym(s) => Self::Sym(s, PhantomData),
            MaybeSymbolNoEndian::Literal(l) => Self::Literal(l),
        }
    }
}

pub type LeSymbol<'a, T> = MaybeSymbol<'a, Little, T>;
#[allow(dead_code)]
pub type BeSymbol<'a, T> = MaybeSymbol<'a, Big, T>;

#[derive(Copy, Clone, Debug)]
pub enum Endian {
    Big,
    Little,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DynInstruction<'a> {
    Intel8080(i8080::Instruction<'a>),
    Mos6502(String),
}

impl<'a> EncodeInstruction for DynInstruction<'a> {
    fn encode(&self, sec: &mut Section, reg: &mut SymbolRegistry) {
        match self {
            DynInstruction::Intel8080(i) => i.encode(sec, reg),
            DynInstruction::Mos6502(_) => (),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Cpu {
    Intel8080,
    Mos6502,
}

use winnow::{Stateful, ModalResult, Parser};
use winnow::error::StrContext;
use winnow::error::StrContextValue;
use winnow::combinator::{delimited, alt, fail};
use winnow::ascii::till_line_ending;
use crate::parser::{ParseContext, Stream};

impl Cpu {
    pub fn parse(input: &mut Stream<'_, '_>) -> ModalResult<Cpu, crate::parser::Error> {
        delimited(
            '"',
            alt((
                "intel,8080".value(Cpu::Intel8080),
                "mos,6502".value(Cpu::Mos6502),
            )),
            '"',
        ).context(ParseContext::Label("cpu"))
            .parse_next(input)
    }

    /*pub fn instruction<'a, 'b>(&self, input: &'b mut parser::Stream<'a, '_>) -> ModalResult<DynInstruction<'a>, crate::parser::Error> {
        match self {
            Cpu::Intel8080 => {
                i8080::Instruction::parse(input)
                    .map(DynInstruction::Intel8080)
            }
            Cpu::Mos6502 => {
                till_line_ending
                    .map(|v: &str| {
                        DynInstruction::Mos6502(v.to_string())
                    })
                .parse_next(input)
            },
        }
    }*/
}

use crate::parser::{Error};
use winnow::error::ErrMode;

impl<'a> Parser<Stream<'a, '_>, DynInstruction<'a>, ErrMode<Error>> for Cpu {
    fn parse_next(&mut self, input: &mut Stream<'a, '_>) -> Result<DynInstruction<'a>, ErrMode<Error>> {
        use annotate_snippets::Level;
        use winnow::combinator::peek;
        let span = peek(till_line_ending.span()).parse_next(input)?;
        match self {
            Cpu::Intel8080 => {
                i8080::Instruction::parse
                    .context(ParseContext::Annotation(Level::Error, span, "".to_string()))
                    .map(DynInstruction::Intel8080)
                    .parse_next(input)
            }
            Cpu::Mos6502 => {
                till_line_ending
                    .map(|v: &str| {
                        DynInstruction::Mos6502(v.to_string())
                    })
                .parse_next(input)
            },
        }
    }
}
