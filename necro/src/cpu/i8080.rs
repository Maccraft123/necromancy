use super::{LeSymbol, ParseOperand, EncodeOperand, EncodeInstruction, ParseInstruction};

#[derive(Debug, Eq, PartialEq, Copy, Clone, EncodeOperand, ParseOperand)]
#[repr(u8)]
pub enum Reg {
    B = 0,
    C,
    D,
    E,
    H,
    L,
    M,
    A,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, EncodeOperand, ParseOperand)]
#[repr(u8)]
pub enum RegPair {
    /// BC register pair
    Bc = 00,
    /// DE register pair
    De,
    /// HL register pair
    Hl,
    /// Stack pointer or PSW
    #[operand(alias = "psw")]
    Sp,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, EncodeOperand, ParseOperand)]
#[repr(u8)]
pub enum Condition {
    #[operand(rename = "nz")] NotZero = 0,
    #[operand(rename = "z")] Zero,
    #[operand(rename = "nc")] NotCarry,
    #[operand(rename = "c")] Carry,
    #[operand(rename = "o")] Odd,
    #[operand(rename = "e")] Even,
    #[operand(rename = "p")] Plus,
    #[operand(rename = "m")] Minus,
}

const DST: u32 = 3;
const RP: u32 = 4;
const CC: u32 = 3;

#[derive(Debug, Eq, PartialEq, Clone, EncodeInstruction, ParseInstruction)]
#[encode(endian = "little")]
#[repr(u8)]
pub enum Instruction<'a> {
    /// Move data from register to register
    Mov(#[encode(shift = DST)] Reg, Reg) = 0b01000000,
    /// Move immediate to register
    Mvi(#[encode(shift = DST)] Reg, LeSymbol<'a, u8>) = 0b00000110,
    /// Move immediate to register pair
    Lxi(#[encode(shift = RP)] RegPair, LeSymbol<'a, u16>) = 0b00000001,
    /// Load accumulator directly from memory
    Lda(LeSymbol<'a, u16>) = 0b00111010,
    /// Store Accumulator directly in memory
    Sta(LeSymbol<'a, u16>) = 0b00110010,
    /// Load H and L registers directly from memory
    Lhld(LeSymbol<'a, u16>) = 0b00101010,
    /// Store H and L registers directly to memory
    Shld(LeSymbol<'a, u16>) = 0b00100010,
    /// Load accumulator from address in register pair
    Ldax(#[encode(shift = RP)] RegPair) = 0b00001010,
    /// Store accumulator in address in register pair
    Stax(#[encode(shift = RP)] RegPair) = 0b00000010,
    /// Exchange HL with DE
    Xchg = 0xeb,
    /// Add to accumulator
    Add(Reg) = 0b10000000,
    /// Add immediate to accumulator
    Adi(LeSymbol<'a, u8>) = 0b11000110,
    /// Add to accumulator using carry
    Adc(Reg) = 0b10001000,
    /// Add immediate to accumulator using carry
    Aci(LeSymbol<'a, u8>) = 0b11001110,
    /// Subtract from accumulator
    Sub(Reg) = 0b10010000,
    /// Subtract immediate from accumulator
    Sui(LeSymbol<'a, u8>) = 0b11010110,
    /// Subtract from accumulator using borrow(carry)
    Sbb(Reg) = 0b10011000,
    /// Subtract immediate from accumulator using borrow(carry)
    Sbi(LeSymbol<'a, u8>) = 0b11011110,
    /// Increment register
    Inr(Reg) = 0b00000100,
    /// Decrement register
    Dcr(Reg) = 0b00000101,
    /// Increment register pair
    Inx(#[encode(shift = RP)] RegPair) = 0b00000011,
    /// Decrement register pair
    Dcx(#[encode(shift = RP)] RegPair) = 0b00001011,
    /// Add register pair to HL
    Dad(#[encode(shift = RP)] RegPair) = 0b00001001,
    Daa = 0x27,
    /// Logical AND with accumulator and register
    Ana(Reg) = 0b10100000,
    /// Logical AND with accumulator and immediate
    Ani(LeSymbol<'a, u8>) = 0b11100110,
    /// Logical OR with accumulator and register
    Ora(Reg) = 0b10110000,
    /// Logical OR with accumulator and immediate
    Ori(LeSymbol<'a, u8>) = 0b11110110,
    /// Logical XOR with accumulator and register
    Xra(Reg) = 0b10101000,
    /// Logical XOR with accumulator and immediate
    Xri(LeSymbol<'a, u8>) = 0b11101110,
    /// Compare register and accumulator
    Cmp(Reg) = 0b10111000,
    /// Compare immediate and accumulator
    Cpi(LeSymbol<'a, u8>) = 0b11111110,
    /// Rotate accumulator left
    Rlc = 0x07,
    /// Rotate accumulator right
    Rrc = 0x0f,
    /// Rotate accumulator left through carry
    Ral = 0x17,
    /// Rotate accumulator right through carry
    Rar = 0x1f,
    /// Complement accumulator
    Cma = 0x2f,
    /// Complement carry flag
    Cmc = 0x3f,
    /// Set carry flag
    Stc = 0x37,
    /// Jump to immediate
    Jmp(LeSymbol<'a, u16>) = 0b11000011,
    #[parse(fuse_first_field)]
    /// Conditional jump to immediate
    J(#[encode(shift = CC)] Condition, LeSymbol<'a, u16>) = 0b11000010,
    /// Call to immediate
    Call(LeSymbol<'a, u16>) = 0b11001101,
    #[parse(fuse_first_field)]
    /// Conditional call to immediate
    C(#[encode(shift = CC)] Condition, LeSymbol<'a, u16>) = 0b11000100,
    /// Return from subroutine
    Ret = 0xc9,
    #[parse(fuse_first_field)]
    /// Conditional return from subroutine
    R(#[encode(shift = CC)] Condition, LeSymbol<'a, u16>) = 0b11000000,
    /// Call 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30 or 0x38
    Rst(#[encode(shift = 3, with = rst_encode)] u8) = 0b11000111,
    /// Jump to address in HL register pair
    Pchl = 0xe9,
    /// Push a register pair to stack
    Push(#[encode(shift = RP)] RegPair) = 0b11000101,
    /// Pop a register pair from stack
    Pop(#[encode(shift = RP)] RegPair) = 0b11000001,
    /// Exchange top of stack with H and L
    Xthl = 0xe3,
    /// Move HL to stack pointer
    Sphl = 0xf9,
    /// Initiate input operation
    In(LeSymbol<'a, u8>) = 0b11011011,
    /// Initiate output operation
    Out(LeSymbol<'a, u8>) = 0b11010011,
    /// Enable interrupts
    Ei = 0xfb,
    /// Disable interrupts
    Di = 0xf3,
    /// Halt
    Hlt = 0x76,
    /// No operation
    Nop = 0x00,
}

fn rst_encode(v: u8, target: &mut crate::section::Section, _: &mut crate::section::SymbolRegistry) {
    let last = target.last_mut();
    *last |= v & 0o70;
}

#[cfg(test)]
mod tests {
    use crate::cpu::{LeSymbol, EncodeInstruction};
    use super::Instruction::*;
    use super::Reg;
    #[test]
    fn encode_nop() {
        let mut vec = Vec::new();
        Nop.encode(&mut vec);
        assert_eq!(vec, &[0x00]);
    }
    #[test]
    fn encode_mov() {
        let mut vec = Vec::new();
        Mov(Reg::A, Reg::B).encode(&mut vec);
        assert_eq!(vec, &[0x78]);
    }
    #[test]
    fn encode_mvi() {
        let mut vec = Vec::new();
        Mvi(Reg::M, LeSymbol::new_literal(0x55)).encode(&mut vec);
        assert_eq!(vec, &[0x36, 0x55]);
    }
}
