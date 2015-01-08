fn internal_srl(val: u8, freg: &mut Register<Flags>) -> u8 {
    freg.write(Flags::empty());
    let result = val >> 1;

    if result == 0 {
        freg.write(ZeroFlag);
    }

    if val & 0x01 == 1 {
        freg.write(CarryFlag);
    }

    return result;
}

pub fn srl(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_sra(reg.read(), freg);
    reg.write(val);
}

pub fn srl_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_sra(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}



pub fn sla(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_sla(reg.read(), freg);
    reg.write(val);
}

pub fn sla_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_sla(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}

fn internal_sra(val: u8, freg: &mut Register<Flags>) -> u8 {
    freg.write(Flags::empty());

    let bit_7 = val & 0x80;
    let result = (val >> 1) | bit_7;

    if result == 0 {
        freg.write(ZeroFlag);
        return 0;
    }

    if val & 0x01 == 1 {
        freg.write(CarryFlag);
    }
    return result;
}

pub fn sra(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_sra(reg.read(), freg);
    reg.write(val);
}

pub fn sra_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_sra(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_sra_at_address() {
        let mut val = 0b10010001;
        let mut freg = Register::new(Flags::empty());
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x2372;

        mem.write_byte(addr, val);
        sra_at_address(&mut mem, addr, &mut freg);

        assert!(mem.read_byte(addr) == 0b11001000);
        assert!(freg.read() == CarryFlag);

        sra_at_address(&mut mem, addr, &mut freg);

        assert!(mem.read_byte(addr) == 0b11100100);
        assert!(freg.read() == Flags::empty());

        mem.write_byte(addr, 0x00);
        sra_at_address(&mut mem, addr, &mut freg);
        assert!(mem.read_byte(addr) == 0x00);
        assert!(freg.read() == ZeroFlag);

        mem.write_byte(addr, 0b00001111);
        sra_at_address(&mut mem, addr, &mut freg);
        assert!(mem.read_byte(addr) == 0b00000111);
        assert!(freg.read() == CarryFlag);

    }

    #[test]
    fn test_sra() {
        let mut reg = Register::new(0b10010001);
        let mut freg = Register::new(Flags::empty());

        sra(&mut reg, &mut freg);

        assert!(reg.read() == 0b11001000);
        assert!(freg.read() == CarryFlag);

        sra(&mut reg, &mut freg);

        assert!(reg.read() == 0b11100100);
        assert!(freg.read() == Flags::empty());

        reg.write(0x00);
        sra(&mut reg, &mut freg);
        assert!(reg.read() == 0x00);
        assert!(freg.read() == ZeroFlag);

        reg.write(0b00001111);
        sra(&mut reg, &mut freg);
        assert!(reg.read() == 0b00000111);
        assert!(freg.read() == CarryFlag);

    }

    #[test]
    fn test_sla_at_address() {
        let mut val = 0b00001111;
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1423;
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        mem.write_byte(addr, val);
        sla_at_address(&mut mem, addr, &mut freg);

        // Rotate should happen
        assert!(mem.read_byte(addr) == 0b00011110);
        assert!(freg.read() == Flags::empty());

        mem.write_byte(addr, 0x00);

        sla_at_address(&mut mem, addr, &mut freg);

        // Zero should return zero with ZeroFlag
        assert!(mem.read_byte(addr) == 0x00);
        assert!(freg.read() == ZeroFlag);

        mem.write_byte(addr, 0b11001100);

        sla_at_address(&mut mem, addr, &mut freg);

        // Carry should get set
        assert!(mem.read_byte(addr) == 0b10011000);
        assert!(freg.read() == CarryFlag);

        freg.write(CarryFlag);
        mem.write_byte(addr, 0b11001100);

        sla_at_address(&mut mem, addr, &mut freg);

        // Carry should get set
        assert!(mem.read_byte(addr) == 0b10011000);
        assert!(freg.read() == CarryFlag);
    }

    #[test]
    fn test_sla() {
        let mut reg = Register::new(0b00001111);
        let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

        sla(&mut reg, &mut freg);

        // Rotate should happen
        assert!(reg.read() == 0b00011110);
        assert!(freg.read() == Flags::empty());

        let mut regb = Register::new(0x00);

        sla(&mut regb, &mut freg);

        // Zero should return zero with ZeroFlag
        assert!(regb.read() == 0x00);
        assert!(freg.read() == ZeroFlag);

        let mut regc = Register::new(0b11001100);
        freg.write(CarryFlag);
        sla(&mut regc, &mut freg);

        assert!(regc.read() == 0b10011000);
        assert!(freg.read() == CarryFlag);

        regc.write(0b00110011);
        sla(&mut regc, &mut freg);

        assert!(regc.read() == 0b01100110);
        assert!(freg.read() == Flags::empty());
    }
}
