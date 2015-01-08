#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_jp() {
        let mut pc = Register::new(0x2311);
        let addr = 0x1223;

        jp(&mut pc, addr);

        assert!(pc.read() == addr);
    }

    #[test]
    fn test_swap() {
        let mut reg = Register::new(0x00);
        let mut freg = Register::new(Flags::empty());

        swap(&mut reg, &mut freg);
        assert!(reg.read() == 0x00);
        assert!(freg.read() == ZeroFlag);

        reg.write(0xAC);
        swap(&mut reg, &mut freg);
        assert!(reg.read() == 0xCA);
        assert!(freg.read() == Flags::empty());
    }

    #[test]
    fn test_set_at_addr() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1235;

        mem.write_byte(addr, 0b11001100);

        set_at_addr(&mut mem, addr, 0);
        assert!(mem.read_byte(addr) == 0b11001101);
    }

    #[test]
    fn test_set() {
        let mut reg = Register::new(0b00110011);

        set(&mut reg, 7);
        assert!(reg.read() == 0b10110011);

        set(&mut reg, 1);
        assert!(reg.read() == 0b10110011);

        set(&mut reg, 4);
        assert!(reg.read() == 0b10110011);

    }

    #[test]
    fn test_res_at_addr() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1235;

        mem.write_byte(addr, 0b11001100);

        res_at_addr(&mut mem, addr, 2);
        assert!(mem.read_byte(addr) == 0b11001000);

    }

    #[test]
    fn test_res() {
        let mut reg = Register::new(0b00110011);

        res(&mut reg, 0);
        assert!(reg.read() == 0b00110010);

        res(&mut reg, 5);
        assert!(reg.read() == 0b00010010);

        res(&mut reg, 7);
        assert!(reg.read() == 0b00010010);
    }

    #[test]
    fn test_reti() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut ime = false;
        let mut pc = Register::new(0x123);
        let mut sp = Register::new(0xFFFC);
        mem.write_word(sp.read(), 0xADCD);

        reti(&mem, &mut pc, &mut sp, &mut ime);
        assert!(pc.read() == 0xADCD);
        assert!(ime == true);
        assert!(sp.read() == 0xFFFE);
    }



    #[test]
    fn test_call() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x4324);
        let mut sp = Register::new(0xFFFE);
        let addr = 0x10;

        call(&mut mem, &mut pc, &mut sp, addr);

        assert!(pc.read() == 0x0010);
        assert!(sp.read() == 0xFFFC);
        assert!(mem.read_word(sp.read()) == 0x4324);
    }


}
