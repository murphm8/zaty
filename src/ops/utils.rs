use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

pub fn u8_immediate(mem: &Memory, pc: &mut Register<u16>) -> u8 {
    let val = mem.read_byte(pc.read());
    pc.increment();
    return val;
}
