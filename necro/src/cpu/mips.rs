
#[encoding(length = u32)]
pub enum Instruction {
    I(ImmOpcode, Reg, Reg, u16),
    J(JumpOpcode, #[encoding(size = 24, shift = 2)] u32),
    R(RegOpcode, Reg, Reg, Reg, u8, Funct),
}

pub struct Reg(u8);
