use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

pub fn rotate_left(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_left(reg.read(), freg);
    reg.write(val);
}

pub fn rotate_left_reset_zeroflag(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_left(reg.read(), freg);
    let mut flags = freg.read();
    flags.remove(ZeroFlag);
    freg.write(flags);
    reg.write(val);
}

pub fn rotate_left_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_rotate_left(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}

fn internal_rotate_left(val: u8, freg: &mut Register<Flags>) -> u8 {
    let mut carry = 0;

    if freg.read().contains(CarryFlag) {
        carry = 1;
    }

    let result = (val << 1) | carry;

    freg.write(Flags::empty());
    if result == 0 {
        freg.write(ZeroFlag);
    }

    if val & 0x80 != 0 {
        freg.write(CarryFlag);
    }

    return result;
}

fn internal_rotate_right(val: u8, freg: &mut Register<Flags>) -> u8 {
    let mut carry = 0;

    if freg.read().contains(CarryFlag) {
        carry = 0x80;
    }

    let result = (val >> 1) | carry;

    freg.write(Flags::empty());

    if result == 0 {
        freg.write(ZeroFlag);
    }

    if val & 0x01 != 0 {
        freg.write(CarryFlag);
    }

    return result;
}

pub fn rotate_right(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_right(reg.read(), freg);
    reg.write(val);
}

pub fn rotate_right_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_rotate_right(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}

pub fn rotate_right_reset_zeroflag(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_right(reg.read(), freg);
    let mut flags = freg.read();
    flags.remove(ZeroFlag);
    freg.write(flags);
    reg.write(val);
}

fn internal_rotate_left_with_carry(val: u8, freg: &mut Register<Flags>) -> u8 {
    freg.write(Flags::empty());

    let bit_7 = (val & 0x80) >> 7;
    let result = (val << 1) | bit_7;

    if result == 0 {
        freg.write(ZeroFlag);
        return val;
    }

    if val & 0x80 != 0 {
        freg.write(CarryFlag);
    }

    return result;
}
fn internal_rotate_right_with_carry(val: u8, freg: &mut Register<Flags>) -> u8 {
    let bit_1 = (val & 0x01) << 7;
    let result = ((val >> 1) | bit_1);

    if result == 0 {
        freg.write(ZeroFlag);
    }

    if val & 0x01 == 1 {
        freg.write(CarryFlag);
    }

    return result;
}

pub fn rotate_right_with_carry(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_right_with_carry(reg.read(), freg);
    reg.write(val);
}

pub fn rotate_right_with_carry_reset_zero(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_right_with_carry(reg.read(), freg);
    let mut flags = freg.read();
    flags.remove(ZeroFlag);
    freg.write(flags);
    reg.write(val);
}

pub fn rotate_right_with_carry_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_rotate_right_with_carry(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}
/// Rotate register left
/// Set ZeroFlag if result is zero
/// Set CarryFlag if bit 7 is 1
pub fn rotate_left_with_carry(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_left_with_carry(reg.read(), freg);
    reg.write(val);
}

pub fn rotate_left_with_carry_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_rotate_left_with_carry(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}

pub fn rotate_left_with_carry_reset_zero(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_rotate_left_with_carry(reg.read(), freg);
    let mut flags = freg.read();
    flags.remove(ZeroFlag);
    freg.write(flags);
    reg.write(val);
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_rotate_left_with_carry() {
        let mut reg = Register::new(0b00001111);
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        rotate_left_with_carry(&mut reg, &mut freg);

        // Rotate should happen
        assert!(reg.read() == 0b00011110);
        assert!(freg.read() == Flags::empty());

        let mut regb = Register::new(0x00);

        rotate_left_with_carry(&mut regb, &mut freg);

        // Zero should return zero with ZeroFlag
        assert!(regb.read() == 0x00);
        assert!(freg.read() == ZeroFlag);

        let mut regc = Register::new(0b11001100);

        rotate_left_with_carry(&mut regc, &mut freg);

        // Carry should get set
        assert!(regc.read() == 0b10011001);
        assert!(freg.read() == CarryFlag);
    }

    #[test]
    fn test_rotate_right_with_carry() {
        let mut reg = Register::new(0b10011001);
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        rotate_right_with_carry(&mut reg, &mut freg);

        assert!(reg.read() == 0b11001100);
        assert!(freg.read() == CarryFlag);

        reg.write(0x0);
        rotate_right_with_carry(&mut reg, &mut freg);

        assert!(reg.read() == 0x0);
        assert!(freg.read() == ZeroFlag);

        reg.write(0x01);
        freg.write(CarryFlag);
        rotate_right_with_carry(&mut reg, &mut freg);
        assert!(reg.read() == 0x80);
        assert!(freg.read() == CarryFlag);
    }

    #[test]
    fn test_rotate_right_at_address() {
        let mut val = 0b00001111;
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1423;
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        mem.write_byte(addr, val);
        rotate_right_at_address(&mut mem, addr, &mut freg);

        // Rotate should happen
        assert!(mem.read_byte(addr) == 0b00000111);
        assert!(freg.read() == CarryFlag);

        mem.write_byte(addr, 0x00);
        freg.write(Flags::empty());

        rotate_right_at_address(&mut mem, addr, &mut freg);

        // Zero should return zero with ZeroFlag
        assert!(mem.read_byte(addr) == 0x00);
        assert!(freg.read() == ZeroFlag);

        mem.write_byte(addr, 0b11001101);

        rotate_right_at_address(&mut mem, addr, &mut freg);

        // Carry should get set
        assert!(mem.read_byte(addr) == 0b01100110);
        assert!(freg.read() == CarryFlag);

        freg.write(CarryFlag);
        mem.write_byte(addr, 0b11001100);

        rotate_right_at_address(&mut mem, addr, &mut freg);

        // Carry should get set
        assert!(mem.read_byte(addr) == 0b11100110);
        assert!(freg.read() == Flags::empty());
    }

    #[test]
    fn test_rotate_right() {
        let mut reg = Register::new(0b10011001);
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        rotate_right(&mut reg, &mut freg);

        assert!(reg.read() == 0b01001100);
        assert!(freg.read() == CarryFlag);

        reg.write(0x0);
        freg.write(Flags::empty());
        rotate_right(&mut reg, &mut freg);

        assert!(reg.read() == 0x0);
        assert!(freg.read() == ZeroFlag);

        reg.write(0x01);
        freg.write(CarryFlag);
        rotate_right(&mut reg, &mut freg);
        assert!(reg.read() == 0x80);
        assert!(freg.read() == CarryFlag);

        reg.write(0x01);
        freg.write(Flags::empty());
        rotate_right(&mut reg, &mut freg);
        assert!(reg.read() == 0x00);
        assert!(freg.read() == CarryFlag);
    }

    #[test]
    fn test_rotate_left_at_address() {
        let mut val = 0b00001111;
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1423;
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        mem.write_byte(addr, val);
        rotate_left_at_address(&mut mem, addr, &mut freg);

        // Rotate should happen
        assert!(mem.read_byte(addr) == 0b00011110);
        assert!(freg.read() == Flags::empty());

        mem.write_byte(addr, 0x00);

        rotate_left_at_address(&mut mem, addr, &mut freg);

        // Zero should return zero with ZeroFlag
        assert!(mem.read_byte(addr) == 0x00);
        assert!(freg.read() == ZeroFlag);

        mem.write_byte(addr, 0b11001100);

        rotate_left_at_address(&mut mem, addr, &mut freg);

        // Carry should get set
        assert!(mem.read_byte(addr) == 0b10011000);
        assert!(freg.read() == CarryFlag);

        freg.write(CarryFlag);
        mem.write_byte(addr, 0b11001100);

        rotate_left_at_address(&mut mem, addr, &mut freg);

        // Carry should get set
        assert!(mem.read_byte(addr) == 0b10011001);
        assert!(freg.read() == CarryFlag);
    }

    #[test]
    fn test_rotate_left() {
        let mut reg = Register::new(0b00001111);
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        rotate_left(&mut reg, &mut freg);

        // Rotate should happen
        assert!(reg.read() == 0b00011110);
        assert!(freg.read() == Flags::empty());

        let mut regb = Register::new(0x00);

        rotate_left(&mut regb, &mut freg);

        // Zero should return zero with ZeroFlag
        assert!(regb.read() == 0x00);
        assert!(freg.read() == ZeroFlag);

        let mut regc = Register::new(0b11001100);
        freg.write(CarryFlag);
        rotate_left(&mut regc, &mut freg);

        assert!(regc.read() == 0b10011001);
        assert!(freg.read() == CarryFlag);

        regc.write(0b00110011);
        freg.write(Flags::empty());
        rotate_left(&mut regc, &mut freg);

        assert!(regc.read() == 0b01100110);
        assert!(freg.read() == Flags::empty());


    }

    #[test]
    fn test_rotate_right_with_carry_at_address() {
        let mut val = 0b10011001;
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1237;
        mem.write_byte(addr, val);
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        rotate_right_with_carry_at_address(&mut mem, addr, &mut freg);

        assert!(mem.read_byte(addr) == 0b11001100);
        assert!(freg.read() == CarryFlag);

        mem.write_byte(addr, 0x0);
        rotate_right_with_carry_at_address(&mut mem, addr, &mut freg);

        assert!(mem.read_byte(addr) == 0x0);
        assert!(freg.read() == ZeroFlag);

        mem.write_byte(addr, 0x01);
        freg.write(CarryFlag);
        rotate_right_with_carry_at_address(&mut mem, addr, &mut freg);
        assert!(mem.read_byte(addr) == 0x80);
        assert!(freg.read() == CarryFlag);

        mem.write_byte(addr, 0x01);
        rotate_right_with_carry_at_address(&mut mem, addr, &mut freg);
        assert!(mem.read_byte(addr) == 0x80);
        assert!(freg.read() == CarryFlag);
    }

    #[test]
    fn test_rotate_left_with_carry_at_address() {
        let mut val = 0b00001111;
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1423;
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        mem.write_byte(addr, val);
        rotate_left_with_carry_at_address(&mut mem, addr, &mut freg);

        // Rotate should happen
        assert!(mem.read_byte(addr) == 0b00011110);
        assert!(freg.read() == Flags::empty());

        mem.write_byte(addr, 0x00);

        rotate_left_with_carry_at_address(&mut mem, addr, &mut freg);

        // Zero should return zero with ZeroFlag
        assert!(mem.read_byte(addr) == 0x00);
        assert!(freg.read() == ZeroFlag);

        mem.write_byte(addr, 0b11001100);

        rotate_left_with_carry_at_address(&mut mem, addr, &mut freg);

        // Carry should get set
        assert!(mem.read_byte(addr) == 0b10011001);
        assert!(freg.read() == CarryFlag);
    }
}
