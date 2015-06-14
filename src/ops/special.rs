use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};
use ops::u16_immediate;

/// Performs no operation and consumes a cycle
pub fn nop() {
}

pub fn set_flag(freg: &mut Register<Flags>, flag: Flags) {
    let mut flags = freg.read();
    flags.insert(flag);
    freg.write(flags);
}

pub fn ccf(freg: &mut Register<Flags>) {
    let mut f = freg.read();
    f.remove(SubtractFlag);
    f.remove(HalfCarryFlag);
    f.toggle(CarryFlag);
    freg.write(f);
}

fn pop_internal(mem: &Memory, sp: &mut Register<u16>) -> u16 {
    let val = mem.read_word(sp.read());
    sp.increment();
    sp.increment();
    return val;
}

pub fn pop(mem: &Memory, sp: &mut Register<u16>, hb: &mut Register<u8>, lb: &mut Register<u8>) {
    let val = pop_internal(mem, sp);
    debug!("pop val: {:X}", val);
    hb.write(high_byte(val));
    lb.write(low_byte(val));
}

pub fn ret(mem: &Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, should_execute: bool) {
    if should_execute {
        let addr = pop_internal(mem, sp);
        debug!("ret to {:X}", addr);
        pc.write(addr);
    }
}

pub fn push(mem: &mut Memory, sp: &mut Register<u16>, val: u16) {
    sp.decrement();
    sp.decrement();
    debug!("push sp:{:X} val:{:X}", sp.read(), val);
    mem.write_word(sp.read(), val);
}

pub fn call_immediate_if_true(mem: &mut Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, should_jump: bool) {
    if should_jump {
        let new_addr = u16_immediate(mem, pc);
        call(mem, pc, sp, new_addr);
    } else {
        pc.increment();
        pc.increment();
    }
}

pub fn call(mem: &mut Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, addr: u16) {
    push(mem, sp, pc.read());
    pc.write(addr);
}

pub fn reti(mem: &Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, ime: &mut bool) {
    ret(mem, pc, sp, true);
    *ime = true;
}

pub fn bit(reg_val: u8, pos: u8, freg: &mut Register<Flags>) {
    let mut flags = freg.read();
    flags.remove(SubtractFlag);
    flags.insert(HalfCarryFlag);

    assert!(pos < 8, "Bit positions are 0-7");

    let mask = 0x01 << pos as usize;
    let result = reg_val & mask;

    if result == 0 {
        flags.insert(ZeroFlag);
    } else {
        flags.remove(ZeroFlag);
    }
    freg.write(flags);
}

pub fn byte_at_address(mem: &Memory, addr: u16) -> u8 {
    return mem.read_byte(addr);
}

pub fn res(reg: &mut Register<u8>, pos: u8) {
    let mask = 0x01 << pos as usize;
    let val = reg.read();
    let reset_val = val & (!mask);
    reg.write(reset_val);
}

pub fn res_at_addr(mem: &mut Memory, address: u16, pos: u8) {
    let mask = 0x01 << pos as usize;
    let val = mem.read_byte(address);
    let reset_val = val & (!mask);
    mem.write_byte(address, reset_val);
}

pub fn set(reg: &mut Register<u8>, pos: u8) {
    let mask = 0x01 << pos as usize;
    let val = reg.read();
    let reset_val = val | mask;
    reg.write(reset_val);
}

pub fn set_at_addr(mem: &mut Memory, address: u16, pos: u8) {
    let mask = 0x01 << pos as usize;
    let val = mem.read_byte(address);
    let reset_val = val | mask;
    mem.write_byte(address, reset_val);
}

fn internal_swap(val: u8, freg: &mut Register<Flags>) -> u8 {
    freg.write(Flags::empty());
    let result = (low_nibble(val) << 4) + high_nibble(val);

    if result == 0 {
        freg.write(ZeroFlag);
    }

    return result;
}

pub fn swap(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = internal_swap(reg.read(), freg);
    reg.write(val);
}

pub fn swap_at_address(mem: &mut Memory, addr: u16, freg: &mut Register<Flags>) {
    let val = internal_swap(mem.read_byte(addr), freg);
    mem.write_byte(addr, val);
}

pub fn disable_interrupts(ime: &mut bool) {
    debug!("disable interrupts");
    *ime = false;
}

pub fn pop_flags(mem: &Memory, sp: &mut Register<u16>, a: &mut Register<u8>, f: &mut Register<Flags>) {
    let val = pop_internal(mem, sp);
    debug!("POP AF val: {:X}", val);
    a.write(high_byte(val));
    f.write(Flags::from_bits_truncate(low_byte(val)));
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};



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

    #[test]
    fn test_pop_flags() {
        let mut a = Register::new(0x00);
        let mut f = Register::new(HalfCarryFlag);
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut sp = Register::new(0xFFFC);
        mem.write_word(sp.read(), 0xBCDF);

        pop_flags(&mem, &mut sp, &mut a, &mut f);

        assert!(sp.read() == 0xFFFE);
        assert!(a.read() == 0xBC);
        assert!(f.read() == ZeroFlag | SubtractFlag | CarryFlag);
    }

    #[test]
    fn test_disable_interrupts() {
        let mut a = true;

        disable_interrupts(&mut a);

        assert!(a == false);
    }

    #[test]
    fn test_swap_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x2321;
        let mut freg = Register::new(Flags::empty());
        mem.write_byte(addr, 0xCB);
        swap_at_address(&mut mem, addr, &mut freg);
        assert!(mem.read_byte(addr) == 0xBC);
    }


    #[test]
    fn test_byte_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x1234;

        mem.write_byte(addr, 0xFE);

        let val = byte_at_address(&mem, addr);
        assert!(val == 0xFE);
    }

    #[test]
    fn test_bit() {
        let reg = 0b0001111;
        let mut freg = Register::new(SubtractFlag | CarryFlag);

        bit(reg, 3, &mut freg);

        assert!(freg.read() == HalfCarryFlag | CarryFlag);

        bit(reg, 7, &mut freg);

        assert!(freg.read() == HalfCarryFlag | CarryFlag | ZeroFlag);
    }

    #[test]
    fn test_push() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut sp = Register::new(0xFFAB);

        push(&mut mem, &mut sp, 0x8735);
        assert!(sp.read() == 0xFFA9);
        assert!(mem.read_word(sp.read()) == 0x8735);
    }

    #[test]
    fn test_call_immediate_if_true() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x6542);
        let mut sp = Register::new(0xFFAB);
        mem.write_word(pc.read(), 0x1234);

        call_immediate_if_true(&mut mem, &mut pc, &mut sp, false);
        assert!(pc.read() == 0x6544);

        pc.write(0x6542);
        call_immediate_if_true(&mut mem, &mut pc, &mut sp, true);
        assert!(pc.read() == 0x1234);
        assert!(sp.read() == 0xFFA9);
        assert!(mem.read_word(sp.read()) == 0x6544);
    }

    #[test]
    fn test_pop() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut sp = Register::new(0xABCD);
        let mut hb = Register::new(0x00);
        let mut lb = Register::new(0x00);
        mem.write_word(sp.read(), 0x1234);

        pop(&mem, &mut sp, &mut hb, &mut lb);
        assert!(hb.read() == 0x12);
        assert!(lb.read() == 0x34);
        assert!(sp.read() == 0xABCF);
    }

    #[test]
    fn test_ret() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0xAB);
        let mut sp = Register::new(0xABCD);
        mem.write_word(sp.read(), 0x1234);

        ret(&mem, &mut pc, &mut sp, false);
        assert!(pc.read() == 0xAB);
        assert!(sp.read() == 0xABCD);

        ret(&mem, &mut pc, &mut sp, true);
        assert!(pc.read() == 0x1234);
        assert!(sp.read() == 0xABCF);
    }

    #[test]
    fn test_reset_flag() {
        let mut freg = Register::new(CarryFlag | ZeroFlag);

        ccf(&mut freg);

        assert!(!freg.read().contains(CarryFlag));
        assert!(freg.read().contains(ZeroFlag));

        freg.write(SubtractFlag);
        ccf(&mut freg);

        assert!(freg.read() == CarryFlag);
    }

    #[test]
    fn test_set_flag() {
        let mut freg = Register::new(Flags::empty());

        set_flag(&mut freg, CarryFlag);

        assert!(freg.read().contains(CarryFlag));
    }


}
