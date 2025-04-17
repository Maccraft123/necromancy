use crate::cpu;
use std::mem;
use std::collections::{BTreeSet, BTreeMap};
use num_traits::FromPrimitive;
use crate::cpu::TypeEndian;


#[derive(Debug)]
pub struct SymbolReference {
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

#[derive(Debug)]
pub struct SymbolRegistry<'a, 'b> {
    strings: BTreeMap<&'a str, &'b str>,
    numbers: BTreeMap<&'a str, u32>,
    imports: BTreeSet<&'a str>,
}

impl<'a, 'b> SymbolRegistry<'a, 'b> {
    pub fn new() -> Self {
        Self {
            strings: BTreeMap::new(),
            numbers: BTreeMap::new(),
            imports: BTreeSet::new(),
        }
    }
    pub fn get<'me, T: FromPrimitive>(&'me mut self, name: &str) -> Option<T> {
        self.numbers.get(name).and_then(|v| T::from_u32(*v))
    }
    pub fn import<'me>(&'me mut self, name: &'a str) {
        self.imports.insert(name);
    }
    pub fn insert_num<'me>(&'me mut self, name: &'a str, value: u32) {
        self.numbers.insert(name, value);
    }
    pub fn insert_str<'me>(&'me mut self, name: &'a str, value: &'b str) {
        self.strings.insert(name, value);
    }
}

#[derive(Debug)]
pub struct Section<'a> {
    /// Name of the section
    name: &'a str,
    /// Start virtual address
    base: Option<usize>,
    /// Binary data of the section
    data: Vec<u8>,
    /// Imported symbols
    imports: Vec<(usize, SymbolReference)>,
    /// Exported symbols
    exports: Vec<(usize, &'a str)>,
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
    pub fn new(name: &'a str, base: Option<usize>) -> Section<'a> {
        Section {
            name,
            base,
            data: Vec::new(),
            imports: Vec::new(),
            exports: Vec::new(),
        }
    }
    fn cur_off(&self) -> usize {
        self.data.len()
    }
    pub fn define_label(&mut self, name: &'a str) {
        self.exports.push((self.cur_off(), name));
    }
    pub fn symbol<T: FromPrimitive, E: TypeEndian>(&mut self, reg: &mut SymbolRegistry, name: String) -> T {
        if let Some(val) = reg.get(&name) {
            val
        } else {
            let s = SymbolReference::simple_len(name, E::endian(), mem::size_of::<T>() as u8);
            self.add_sym_ref(s);

            T::from_u32(0).unwrap()
        }
    }
    fn add_sym_ref(&mut self, s: SymbolReference) {
        self.imports.push((self.cur_off(), s));
    }
    pub fn push(&mut self, d: u8) {
        self.data.push(d);
    }
    pub fn last_mut(&mut self) -> &mut u8 {
        self.data.last_mut().unwrap()
    }
}
