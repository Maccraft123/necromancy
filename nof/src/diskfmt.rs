
trait AddrSize: Sized {}
impl AddrSize for u8 {}
impl AddrSize for u16 {}
impl AddrSize for u32 {}

/// The on-disk format for NOF
pub struct Nof<T: AddrSize> {
    /// Magic bytes, "NOF"
    magic: [u8; 3],
    /// File format version: This is 0x01
    version: u8,
    /// 7th bit: If set - Big endian, otherwise little endian
    /// 6th bit: Has exports and export_offset field
    /// 5th bit: Has imports and import_offset field
    /// 4th bit: Has relocations and reloc_offset field
    /// 3rd bit: String termination: 0 -> NUL, 1 -> $
    /// Bits 1 and 0: Address size
    ///     00: 8, 1 << (0 + 3)
    ///     01: 16, 1 << (1 + 3)
    ///     10: 32, 1 << (2 + 3)
    ///     11: 64, 1 << (3 + 3)
    flags: u8,
    cpu: [u8; 6],
    os: [u8; 5],
    export_offset: T,
    import_offset: T,
    segments: Vec<Segment<T>>,
    imports: Vec<Symbol<T>>,
    exports: Vec<Symbol<T>>,
}

pub struct Segment<T: AddrSize> {
    /// Address in memory for the segment
    load_addr: T,
    /// Length of the segment
    len: T,
    /// Segment type and permissions. Permissions can be ignored on systems without an MPU
    /// 0 -> Code
    /// 1 -> Data
    /// 0x10 -> Read
    /// 0x20 -> Write
    /// 0x40 -> Execute
    /// 0x80 -> No data in file for this segment
    ty: u8,
    /// Data, with padding to align this struct to T bytes
    data: Vec<u8>,
}

pub struct Symbol<T: AddrSize> {
    /// Address of reference of the symbol. All 0xff bytes signify end of list
    addr: T,
    /// Length of name
    /// For imports, most significant byte is for setting relative or absolute type.
    /// name_len & 0x80 for relative
    /// name_len & 0x00 for absolute
    name_len: u8,
    /// ASCII Name of the import, unterminated
    name: Vec<u8>,
}
