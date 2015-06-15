use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};
use ops::u8_immediate;

fn half_carry_for_subtract(val1: u8, val2: u8, carry: u8) -> bool {
    if low_nibble(val2) + carry == 0x10 && val1 >= 0x10 {
        return true;
    } else {
        return low_nibble(val1) >= (low_nibble(val2) + carry);
    }

}

pub fn sub_u8_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>, freg: &mut Register<Flags>, with_carry: bool) {
    let val = u8_immediate(mem, pc);
    internal_sub(reg, val, freg, with_carry);
}

fn carry_for_subtract(val1: u8, val2: u8, carry: u8) -> bool {
    return val1 as u16 >= (val2 as u16 + carry as u16);
}

pub fn internal_sub(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>, with_carry: bool) {
    let reg_val = reg.read();
    let mut flags = SubtractFlag;
    let mut carry = 0;

    if freg.read().contains(CarryFlag) && with_carry {
        carry = 1;
    }

    if half_carry_for_subtract(reg_val, val, carry) {
        flags.insert(HalfCarryFlag);
    }

    if carry_for_subtract(reg_val, val, carry) {
        flags.insert(CarryFlag);
    }

    let result = reg_val.wrapping_sub(val).wrapping_sub(carry);

    if result == 0 {
        flags.insert(ZeroFlag);
    }
    debug!("internal sub result {:X}", result);
    reg.write(result);
    freg.write(flags);
}


pub fn sub(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>) {
    internal_sub(reg, val, freg, false);
}

pub fn sbc(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>) {
    internal_sub(reg, val, freg, true);
}

pub fn sub_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    internal_sub(reg, val, freg, false);
}

pub fn sbc_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    internal_sub(reg, val, freg, true);
}

pub fn compare(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>) {
    let reg_val = reg.read();
    let mut flags = SubtractFlag;
    let mut carry = 0;

    if half_carry_for_subtract(reg_val, val, carry) {
        flags.insert(HalfCarryFlag);
    }

    if carry_for_subtract(reg_val, val, carry) {
        flags.insert(CarryFlag);
    }

    if (reg_val == val) {
        flags.insert(ZeroFlag);
    }
    freg.write(flags);
}

pub fn compare_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    compare(reg, val, freg);
}

/// Decrement register by 1
/// Set ZeroFlag if result is 0
/// Set SubtractFlag
/// Set HalfCarryFlag if there is no borrow from bit 4
pub fn decrement_register(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    let mut flags = freg.read() | SubtractFlag;
    flags.remove(ZeroFlag);
    flags.remove(HalfCarryFlag);

    if (val & 0x0F) > 0 {
        flags = flags | HalfCarryFlag;
    }

    reg.decrement();

    if reg.read() == 0x00 {
        flags = flags | ZeroFlag;
    }
    freg.write(flags);
}

pub fn decrement_register_pair(reg1: &mut Register<u8>, reg2: &mut Register<u8>) {
    let val = pack_u16(reg1.read(), reg2.read());
    let ans = val - 1;

    reg1.write(high_byte(ans));
    reg2.write(low_byte(ans));
}

pub fn decrement_value_at_address(mem: &mut Memory, hb: u8, lb: u8, freg: &mut Register<Flags>) {
    let addr = pack_u16(hb, lb);
    let val = mem.read_byte(addr);
    let mut reg = Register::new(val);
    decrement_register(&mut reg, freg);
    mem.write_byte(addr, reg.read());
}


#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_sub_u8_immediate() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x2736);
        let mut reg = Register::new(0x35);
        let mut freg = Register::new(Flags::empty());
        mem.write_byte(pc.read(), 0x11);

        sub_u8_immediate(&mem, &mut pc, &mut reg, &mut freg, false);

        assert!(pc.read() == 0x2737);
        assert!(reg.read() == 0x24);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag | CarryFlag);

        mem.write_byte(pc.read(), 0x0C);
        sub_u8_immediate(&mem, &mut pc, &mut reg, &mut freg, false);

        assert!(pc.read() == 0x2738);
        assert!(reg.read() == 0x18);
        assert!(freg.read() == SubtractFlag | CarryFlag);

        mem.write_byte(pc.read(), 0x0F);
        freg.write(CarryFlag);
        sub_u8_immediate(&mem, &mut pc, &mut reg, &mut freg, true);

        assert!(pc.read() == 0x2739);
        assert!(reg.read() == 0x08);
        assert!(freg.read() == SubtractFlag | CarryFlag | HalfCarryFlag);

    }

    #[test]
    fn test_cp() {
        let mut reg = Register::new(0xAA);
        let mut val = 0xBB;
        let mut freg = Register::new(Flags::empty());

        compare(&mut reg, val, &mut freg);
        assert!(reg.read() == 0xAA);
        assert!(freg.read() == SubtractFlag);

        reg.write(0xF0);
        val = 0xF0;
        compare(&mut reg, val, &mut freg);
        assert!(reg.read() == 0xF0);
        assert!(freg.read() == SubtractFlag | CarryFlag | HalfCarryFlag | ZeroFlag);
    }

    #[test]
    fn test_compare_value_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut reg = Register::new(0xBA);
        let mut freg = Register::new(Flags::empty());
        let addr = 0x1239;
        let mut val = 0xAB;
        mem.write_byte(addr, val);

        compare_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0xBA);
        assert!(freg.read() == SubtractFlag | CarryFlag);

        mem.write_byte(addr, 0xCD);
        reg.write(0xCD);
        compare_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0xCD);
        assert!(freg.read() == SubtractFlag | ZeroFlag | HalfCarryFlag | CarryFlag);

    }

    #[test]
    fn test_sbc_value_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut freg = Register::new(CarryFlag);
        let mut reg = Register::new(0xFF);
        let addr = 0x1234;
        mem.write_byte(addr, 0xCD);

        sbc_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0x31);
        assert!(freg.read() == SubtractFlag | CarryFlag | HalfCarryFlag);
    }

    #[test]
    fn test_sub_value_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut freg = Register::new(CarryFlag);
        let mut reg = Register::new(0xFF);
        let addr = 0x1234;
        mem.write_byte(addr, 0xCD);

        sub_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0x32);
        assert!(freg.read() == SubtractFlag | CarryFlag | HalfCarryFlag);
    }

    #[test]
    fn test_sbc() {
        let mut reg = Register::new(0xFF);
        let mut freg = Register::new(Flags::empty());

        sbc(&mut reg, 0x0f, &mut freg);
        assert!(reg.read() == 0xF0);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag | CarryFlag);

        reg.write(0xFF);
        freg.write(CarryFlag);
        sbc(&mut reg, 0x0f, &mut freg);
        assert!(reg.read() == 0xEF);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag | CarryFlag);

        reg.write(0xFF);
        freg.write(CarryFlag);
        sbc(&mut reg, 0xFF, &mut freg);
        assert!(reg.read() == 0xFF);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag);

        reg.write(0xAB);
        freg.write(CarryFlag);
        sbc(&mut reg, 0x12, &mut freg);
        assert!(reg.read() == 0x98);
        assert!(freg.read() == SubtractFlag | CarryFlag | HalfCarryFlag);
    }

    #[test]
    fn test_sub() {
        let mut reg = Register::new(0xFF);
        let mut freg = Register::new(Flags::empty());

        sub(&mut reg, 0x0f, &mut freg);
        assert!(reg.read() == 0xF0);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag | CarryFlag);

        reg.write(0x11);
        sub(&mut reg, 0x11, &mut freg);
        assert!(reg.read() == 0);
        assert!(freg.read().contains(SubtractFlag));
        assert!(freg.read().contains(ZeroFlag));
        assert!(freg.read().contains(CarryFlag));
        assert!(freg.read().contains(HalfCarryFlag));

        reg.write(0xA0);
        sub(&mut reg, 0xB0, &mut freg);
        assert!(reg.read() == 0xF0);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag);

        reg.write(0x80);
        sub(&mut reg, 0x0f, &mut freg);
        assert!(reg.read() == 0x71);
        assert!(freg.read() == SubtractFlag | CarryFlag);

        reg.write(0x05);
        sub(&mut reg, 0xAB, &mut freg);
        assert!(reg.read() == 0x5A);
        assert!(freg.read() == SubtractFlag);
    }

    #[test]
    fn test_decrement_value_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut freg = Register::new(CarryFlag);
        mem.write_byte(0x1010, 1);

        decrement_value_at_address(&mut mem, 0x10, 0x10, &mut freg);

        assert!(mem.read_byte(0x1010) == 0);
        assert!(freg.read().is_all());

        mem.write_byte(0x01AB, 0x20);

        decrement_value_at_address(&mut mem, 0x01, 0xAB, &mut freg);

        assert!(mem.read_byte(0x01AB) == 0x1F);
        assert!(freg.read() == CarryFlag | SubtractFlag);

        freg.write(ZeroFlag);
        mem.write_byte(0xABCD, 0xED);
        decrement_value_at_address(&mut mem, 0xAB, 0xCD, &mut freg);

        assert!(mem.read_byte(0xABCD) == 0xEC);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag);

    }

    #[test]
    fn test_decrement_register_pair() {
        let mut reg1 = Register::new(0x70);
        let mut reg2 = Register::new(0x00);

        decrement_register_pair(&mut reg1, &mut reg2);

        assert!(reg1.read() == 0x6F);
        assert!(reg2.read() == 0xFF);
    }

    #[test]
    fn test_decrement_register() {
        let mut reg = Register::new(1);
        let mut freg = Register::new(Flags::empty());

        decrement_register(&mut reg, &mut freg);

        assert!(reg.read() == 0);
        assert!(freg.read() == ZeroFlag | SubtractFlag | HalfCarryFlag);

        reg.write(0xF1);
        freg.write(Flags::empty());

        decrement_register(&mut reg, &mut freg);

        assert!(reg.read() == 0xF0);
        assert!(freg.read() == SubtractFlag | HalfCarryFlag);

        reg.write(0xF0);
        freg.write(Flags::empty());

        decrement_register(&mut reg, &mut freg);

        assert!(reg.read() == 0xEF);
        assert!(freg.read() == SubtractFlag);
    }
}
