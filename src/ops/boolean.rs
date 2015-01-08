use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

pub fn and(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>) {
    let mut flags = HalfCarryFlag;

    let result = reg.read() & val;

    if result == 0 {
        flags.insert(ZeroFlag);
    }

    reg.write(result);
    freg.write(flags);
}

pub fn xor(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>) {
    let mut flags = Flags::empty();

    let result = reg.read() ^ val;

    if result == 0 {
        flags.insert(ZeroFlag);
    }

    reg.write(result);
    freg.write(flags);
}

pub fn and_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    and(reg, val, freg);
}

pub fn xor_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    xor(reg, val, freg);
}


pub fn or(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>) {
    let mut flags = Flags::empty();

    let result = reg.read() | val;

    if result == 0 {
        flags.insert(ZeroFlag);
    }

    reg.write(result);
    freg.write(flags);
}

pub fn or_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    or(reg, val, freg);
}

pub fn complement(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    reg.write(!val);

    let mut flags = freg.read();
    flags.insert(HalfCarryFlag);
    flags.insert(SubtractFlag);
    freg.write(flags);
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_complement() {
        let mut a = Register::new(0x11);
        let mut freg = Register::new(ZeroFlag | CarryFlag);

        complement(&mut a, &mut freg);

        assert!(a.read() == !0x11);
        assert!(freg.read().is_all());

        freg.write(Flags::empty());
        complement(&mut a, &mut freg);

        assert!(a.read() == 0x11);
        assert!(freg.read().contains(HalfCarryFlag));
        assert!(freg.read().contains(SubtractFlag));
    }

    #[test]
    fn test_or() {
        let mut reg = Register::new(0b01010101);
        let mut val = 0b11110000;
        let mut freg = Register::new(Flags::empty());

        or(&mut reg, val, &mut freg);
        assert!(reg.read() == 0b11110101);
        assert!(freg.read() == Flags::empty());

        reg.write(0b00000000);
        val = 0x00;
        or(&mut reg, val, &mut freg);
        assert!(reg.read() == 0b00000000);
        assert!(freg.read() == ZeroFlag);

    }

    #[test]
    fn test_or_value_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut reg = Register::new(0b01000110);
        let mut freg = Register::new(Flags::empty());
        let addr = 0x1239;
        let mut val = 0b11000011;
        mem.write_byte(addr, val);

        or_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0b11000111);
        assert!(freg.read() == Flags::empty());

        mem.write_byte(addr, 0b00000000);
        reg.write(0x00);
        or_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0b00000000);
        assert!(freg.read() == ZeroFlag);
    }


    #[test]
    fn test_and_value_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut reg = Register::new(0b01000110);
        let mut freg = Register::new(Flags::empty());
        let addr = 0x1239;
        let mut val = 0b11000011;
        mem.write_byte(addr, val);

        and_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0b01000010);
        assert!(freg.read() == HalfCarryFlag);

        mem.write_byte(addr, 0b00000000);
        and_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0b00000000);
        assert!(freg.read() == HalfCarryFlag | ZeroFlag);
    }

    #[test]
    fn test_and() {
        let mut reg = Register::new(0b01010101);
        let mut val = 0b11110000;
        let mut freg = Register::new(Flags::empty());

        and(&mut reg, val, &mut freg);
        assert!(reg.read() == 0b01010000);
        assert!(freg.read() == HalfCarryFlag);

        reg.write(0b00001111);
        and(&mut reg, val, &mut freg);
        assert!(reg.read() == 0b00000000);
        assert!(freg.read() == HalfCarryFlag | ZeroFlag);
    }

    #[test]
    fn test_xor_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut reg = Register::new(0b00111100);
        let mut freg = Register::new(Flags::empty());
        let addr = 0x1239;
        let mut val = 0b11000011;
        mem.write_byte(addr, val);

        xor_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0b11111111);
        assert!(freg.read() == Flags::empty());

        mem.write_byte(addr, 0b11111111);
        xor_value_at_address(&mem, &mut reg, addr, &mut freg);
        assert!(reg.read() == 0b00000000);
        assert!(freg.read() == ZeroFlag);
    }

    #[test]
    fn test_xor() {
        let mut reg = Register::new(0b01010101);
        let mut freg = Register::new(Flags::empty());
        let mut val = 0b11111111;

        xor(&mut reg, val, &mut freg);
        assert!(reg.read() == 0b10101010);
        assert!(freg.read() == Flags::empty());

        val = 0b10101010;
        xor(&mut reg, val, &mut freg);
        assert!(reg.read() == 0b00000000);
        assert!(freg.read() == ZeroFlag);
    }
}
