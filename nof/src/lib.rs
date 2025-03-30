use std::fmt;
use std::marker::PhantomData;
use std::iter::repeat_n;

use core::mem::size_of;

pub trait Endian {
    fn is_big() -> bool;
}
#[derive(Copy, Clone)]
struct Little;
#[derive(Copy, Clone)]
struct Big;
impl Endian for Little {
    fn is_big() -> bool { false }
}
impl Endian for Big {
    fn is_big() -> bool { true }
}

pub trait AddrSize<T: Endian + ?Sized>: Copy + Clone + fmt::Debug {
    fn bytes(self) -> impl IntoIterator<Item = u8>;
    fn size() -> usize { size_of::<Self>() }
    fn new(v: usize) -> Self;
}

macro_rules! impl_addrsize {
    ($t: ty) => {
        impl AddrSize<Little> for $t {
            fn bytes(self) -> impl IntoIterator<Item = u8> {
                self.to_le_bytes()
            }
            fn new(v: usize) -> $t {
                v as $t
            }
        }
        impl AddrSize<Big> for $t {
            fn bytes(self) -> impl IntoIterator<Item = u8> {
                self.to_be_bytes()
            }
            fn new(v: usize) -> $t {
                v as $t
            }
        }
    }
}

impl_addrsize!(u16);
impl_addrsize!(u32);
impl_addrsize!(u64);

pub trait Encode {
    fn align() -> usize;
    fn do_encode(&self, _: &mut Vec<u8>);
    fn encode_into(&self, v: &mut Vec<u8>) {
        let start_len = v.len();
        let align = Self::align();

        self.do_encode(v);
        
        let len = v.len() - start_len;
        let diff = len % align;
        v.extend(repeat_n(0, diff));
    }
    fn encode(&self) -> Vec<u8> {
        let mut vec = Vec::new();
        self.encode_into(&mut vec);
        vec
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Cpu {
    /* for compatible families: [ is_be, match_by_family, 3 family bits, 3 model bits ] */
    /* for one-off chips:       [ is_be, match_by_family, 6 bits of cpu id] */
    Intel8080   = 0b01_001_000,
    Intel8085   = 0b01_001_001,
    Intel8086   = 0b01_010_000,
    Intel80186  = 0b01_010_001,
    Intel80386  = 0b01_010_010,
    Intel80486  = 0b01_010_011,

    HitachiSh1  = 0b01_011_000,
    HitachiSh2  = 0b01_011_001,
    HitachiSh3  = 0b01_011_010,

    Motorola6809 = 0x81,
    NecV20 = 0x82,
}

/*impl Cpu {
    const fn ptr_size(&self) -> usize {
        match self {
            Self::Intel8080 | Self::Intel8085 | Self::Motorola6809 => 8,
            Self::Intel8086 | Self::Intel80186 | Self::NecV20 => 16,
            Self::Intel80386 | Self::Intel80486 |
                Self::HitachiSh1 | Self::HitachiSh2 | Self::HitachiSh3 => 32,
        }
    }
}*/

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Os {
    Cpm80,
    Linux,
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum StringTermination {
    Nul = 0x00,
    Dollar = 0x08,
}

#[derive(Debug, Clone)]
pub struct Nof<E: Endian, T: AddrSize<E>> {
    version: u8,
    cpu: Cpu,
    os: Os,
    segments: Vec<Segment<E, T>>,
    imports: Vec<Symbol<E, T>>,
    exports: Vec<Symbol<E, T>>,
    const_symbols: Vec<Symbol<E, T>>,
    //relocations: Vec<Relocation<E, T>>,
}

impl<E: Endian, T: AddrSize<E>> Nof<E, T> {
    pub fn new(
        cpu: Cpu,
        os: Os,
        segments: Vec<Segment<E, T>>,
        imports: Vec<Symbol<E, T>>,
        exports: Vec<Symbol<E, T>>,
        const_symbols: Vec<Symbol<E, T>>,
    ) -> Self {
        Self {
            version: 1,
            cpu,
            os,
            segments,
            imports,
            exports,
            const_symbols,
        }
    }
}

impl<E: Endian, T: AddrSize<E>> Encode for Nof<E, T> {
    fn align() -> usize { 1 }
    fn do_encode(&self, v: &mut Vec<u8>) {
        let mut data: Vec<u8> = Vec::new();
        let has_exports = !self.exports.is_empty();
        let has_imports = !self.imports.is_empty();
        let has_const_symbols = !self.const_symbols.is_empty();
        let import_offset;
        let export_offset;
        let const_sym_offset;

        v.extend(b"NOF");
        v.push(self.version);
        v.push(self.cpu as u8);
        v.push(self.os as u8);

        v.push(
            has_exports as u8 * 0x80 |
            has_imports as u8 * 0x40 |
            //has_relocs as u8 * 0x20 |
            has_const_symbols as u8 * 0x10,
        );

        assert_eq!(v.len(), 16);

        for seg in self.segments.iter() {
            seg.encode_into(&mut data);
        }

        import_offset = data.len() + v.len();
        for im in self.imports.iter() {
            im.encode_into(&mut data);
        }

        export_offset = data.len() + v.len();
        for ex in self.exports.iter() {
            ex.encode_into(&mut data);
        }

        const_sym_offset = data.len() + v.len();
        for sym in self.const_symbols.iter() {
            sym.encode_into(&mut data);
        }

        if has_exports {
            v.extend(T::new(export_offset).bytes());
        }

        if has_imports {
            v.extend(T::new(import_offset).bytes());
        }

        if has_const_symbols {
            v.extend(T::new(const_sym_offset).bytes());
        }
    }
}

#[derive(Debug, Clone)]
pub struct Segment<E: Endian, T: AddrSize<E>> {
    load_addr: T,
    read: bool,
    write: bool,
    execute: bool,
    bss: bool,
    data: Vec<u8>,
    _e: PhantomData<E>,
}

impl<E: Endian, T: AddrSize<E>> Encode for Segment<E, T> {
    fn align() -> usize { T::size() }
    fn do_encode(&self, v: &mut Vec<u8>) {
        v.extend(self.load_addr.bytes());
        v.extend(T::new(self.data.len()).bytes());

        v.push(
            self.read as u8 * 0x10 |
            self.write as u8 * 0x20 |
            self.execute as u8 * 0x40 |
            self.bss as u8 * 0x80
        );

        v.extend(self.data.iter());
    }
}

impl<E: Endian, T: AddrSize<E>> Segment<E, T> {
    pub fn new(load_addr: T, data: Vec<u8>) -> Self {
        Self {
            load_addr,
            read: true,
            write: true,
            execute: true,
            bss: false,
            data,
            _e: PhantomData,
        }
    }
}

/*impl<E: Endian, T: AddrSize<E>> Symbol<E, T> {
    fn new(addr: T, ty: SymbolType, name: String) -> Segmtn<E, T> {
        Self { addr, ty, name, _e: PhantomData }
    }
}*/

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum SymbolType {
    Absolute = 0x00,
    Relative = 0x80,
}

#[derive(Debug, Clone)]
pub struct Symbol<E: Endian, T: AddrSize<E>> {
    addr: T,
    ty: SymbolType,
    name: String,
    _e: PhantomData<E>,
}

impl<E: Endian, T: AddrSize<E>> Encode for Symbol<E, T> {
    fn align() -> usize { T::size() }
    fn do_encode(&self, v: &mut Vec<u8>) {
        v.extend(self.addr.bytes());
        assert!(self.name.len() < 128);
        v.push(self.name.len() as u8 | self.ty as u8);
        for c in self.name.bytes() {
            v.push(c);
        }
    }
}

impl<E: Endian, T: AddrSize<E>> Symbol<E, T> {
    pub fn new(addr: T, ty: SymbolType, name: String) -> Symbol<E, T> {
        Self { addr, ty, name, _e: PhantomData }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_symbol() {
        let sym = Symbol::<Little, u16>::new(0x100, SymbolType::Absolute, "entry".into());
        let bin = [
            0x00, 0x01, 0x05,
            b'e', b'n', b't', b'r', b'y',
        ];
        assert_eq!(sym.encode(), bin);

        let sym = Symbol::<Little, u16>::new(0x100, SymbolType::Relative, "entry".into());
        let bin = [
            0x00, 0x01, 0x85,
            b'e', b'n', b't', b'r', b'y',
        ];
        assert_eq!(sym.encode(), bin);

        let sym = Symbol::<Little, u32>::new(0x01020304, SymbolType::Absolute, "unaligned".into());
        let bin = [
            0x04, 0x03, 0x02, 0x01,
            0x09,
            b'u', b'n', b'a', b'l', b'i', b'g', b'n', b'e', b'd', 0, 0,
        ];
        assert_eq!(sym.encode(), bin);

        let sym = Symbol::<Big, u32>::new(0x01020304, SymbolType::Absolute, "unaligned".into());
        let bin = [
            0x01, 0x02, 0x03, 0x04,
            0x09,
            b'u', b'n', b'a', b'l', b'i', b'g', b'n', b'e', b'd', 0, 0,
        ];
        assert_eq!(sym.encode(), bin);
    }
}
