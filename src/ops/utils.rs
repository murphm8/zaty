use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

pub fn u8_immediate(mem: &Memory, pc: &mut Register<u16>) -> u8 {
    let val = mem.read_byte(pc.read());
    pc.increment();
    return val;
}

pub fn u16_immediate(mem: &Memory, pc: &mut Register<u16>) -> u16 {
    let val = mem.read_word(pc.read());
    pc.increment();
    pc.increment();
    return val;
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_u16_immediate() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x123);
        mem.write_word(pc.read(), 0xBEEF);

        let val = u16_immediate(&mem, &mut pc);

        assert!(val == 0xBEEF);
        assert!(pc.read() == 0x125);

    }

    #[test]
    fn test_u8_immediate() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x123);
        mem.write_byte(pc.read(), 0x43);

        let val = u8_immediate(&mem, &mut pc);

        assert!(val == 0x43);
        assert!(pc.read() == 0x124);
    }
}
