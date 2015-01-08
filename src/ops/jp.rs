use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};
use ops::utils::{u16_immediate, u8_immediate};

/// Add n to current address and jump to it - n = one byte signed immediate value
pub fn jump_by_signed_immediate(mem: &Memory, pc: &mut Register<u16>) {
    let offset = u8_immediate(mem, pc);
    let current_pc = pc.read();
    let mut new_pc = 0;
    if (offset & 0x80) == 0 {
        new_pc = current_pc + offset as u16;
        debug!("jmp signed immediate new_pc: {:X}", new_pc);
    } else {
        new_pc = (current_pc as i16 + (offset as i8) as i16) as u16;
        debug!("jmp signed immediate new_pc: {:X}", new_pc);
    }
    debug!("jmp {:X}", new_pc);
    pc.write(new_pc);
}

pub fn relative_jmp_by_signed_immediate_if_true(mem: &Memory, pc: &mut Register<u16>, should_jump: bool) {
    if should_jump {
        jump_by_signed_immediate(mem, pc);
    } else {
        pc.increment();
    }
}

pub fn jp_u16_immediate(mem: &Memory, pc: &mut Register<u16>) {
    let addr = u16_immediate(mem, pc);
    debug!("jmp {:X}", addr);
    pc.write(addr);
}

pub fn jp_u16_immediate_if_true(mem: &Memory,pc: &mut Register<u16>, should_jump: bool) {
    if should_jump {
        jp_u16_immediate(mem, pc);
    } else {
        pc.increment();
        pc.increment();
    }
}

pub fn jp(pc: &mut Register<u16>, addr: u16) {
    pc.write(addr);
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_jp_u16_immediate() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x6542);
        mem.write_word(pc.read(), 0x1234);

        pc.write(0x6542);
        jp_u16_immediate(&mem, &mut pc);
        assert!(pc.read() == 0x1234);
    }

    #[test]
    fn test_jp_u16_immediate_if_true() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x6542);
        mem.write_word(pc.read(), 0x1234);

        jp_u16_immediate_if_true(&mem, &mut pc, false);
        assert!(pc.read() == 0x6544);

        pc.write(0x6542);
        jp_u16_immediate_if_true(&mem, &mut pc, true);
        assert!(pc.read() == 0x1234);
    }

    #[test]
    fn test_jump_by_signed_immediate() {
        let mut mem = EmptyMemory::new(0x10000);
        let mut pc = Register::new(0x0101);
        // 0x8A = -10 as i8
        mem.write_byte(0x0101, -10 as u8);

        jump_by_signed_immediate(&mem, &mut pc);

        assert!(pc.read() == 0xF8, "Should jump backwards with negative number");

        mem.write_byte(0xF8, 0x37);

        jump_by_signed_immediate(&mem, &mut pc);

        assert!(pc.read() == 0x130);
    }

    #[test]
    fn test_relative_jmp_by_signed_immediate_if_true() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x1234);

        // Forwards
        mem.write_byte(0x1234, 0x55);
        relative_jmp_by_signed_immediate_if_true(&mem, &mut pc, true);
        assert!(pc.read() == 0x128A, "Should jump forwards");

        // Backwards
        mem.write_byte(0x128A, -10 as u8);
        relative_jmp_by_signed_immediate_if_true(&mem, &mut pc, true);
        assert!(pc.read() == 0x1281, "Should jump back");

        // no jump
        mem.write_byte(0x1281, 0xFF);
        relative_jmp_by_signed_immediate_if_true(&mem, &mut pc, false);
        assert!(pc.read() == 0x1282, "Should not jump if ZeroFlag is not set. PC should increment to go past immediate value");

    }

    #[test]
    fn test_jp() {
        let mut pc = Register::new(0x2311);
        let addr = 0x1223;

        jp(&mut pc, addr);

        assert!(pc.read() == addr);
    }

}
