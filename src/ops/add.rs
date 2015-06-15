use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};
use ops::utils::u8_immediate;

fn half_carry_for_add(val1: u8, val2: u8, with_carry: bool) -> bool {
    let mut c = 0;
    if with_carry { c = 1; }
    return (low_nibble(val1) + low_nibble(val2) + c) > 0x0F
}

fn carry_for_add(val1: u8, val2: u8, with_carry: bool) -> bool {
    let mut c = 0;
    if with_carry { c = 1; }
    return val1 as u16 + val2 as u16 + c > 0xFF;
}

fn add_internal(first: &mut Register<u8>, second: u8, freg: &mut Register<Flags>, with_carry: bool) {
    let val1 = first.read();
    let val2 = second;
    let mut result = val1.wrapping_add(val2);
    let mut do_carry = false;

    if freg.read().contains(CarryFlag) && with_carry {
        do_carry = true;
        result = result.wrapping_add(1);
    }

    let mut flags = Flags::empty();

    if half_carry_for_add(val1, val2, do_carry) {
        flags = HalfCarryFlag;
    }

    if carry_for_add(val1, val2, do_carry) {
        flags = flags | CarryFlag;
    }

    if result == 0 {
        flags = ZeroFlag;
    }

    first.write(result);
    freg.write(flags);
}

pub fn add_value_at_address(mem: &Memory, reg: &mut Register<u8>, hb: u8, lb: u8, freg: &mut Register<Flags>) {
   let val = mem.read_byte(pack_u16(hb, lb));
   add(reg, val, freg);
}

pub fn adc(reg: &mut Register<u8>, val: u8, freg: &mut Register<Flags>) {
    add_internal(reg, val, freg, true);
}

pub fn adc_value_at_address(mem: &Memory, reg: &mut Register<u8>, address: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(address);
    adc(reg, val, freg);
}

pub fn add_u8_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>, freg: &mut Register<Flags>, with_carry: bool) {
    let val = u8_immediate(mem, pc);
    add_internal(reg, val, freg, with_carry);
}

/// Add the value of two registers and store it in the first register
pub fn add(first: &mut Register<u8>, second: u8, freg: &mut Register<Flags>) {
    add_internal(first, second, freg, false);
}

/// Adds two sets of registers as 16 bit numbers with carries counted on bit 11 and 15
pub fn add_register_pair_to_register_pair(rega: &mut Register<u8>, regb: &mut Register<u8>, reg1: u8, reg2: u8, freg: &mut Register<Flags>) {
    let first = pack_u16(rega.read(), regb.read());
    let second = pack_u16(reg1, reg2);

    let sum = first.wrapping_add(second);

    // Reset subtract flag, leave ZeroFlag alone
    let mut flags = freg.read();
    flags.remove(SubtractFlag);
    flags.remove(CarryFlag);
    flags.remove(HalfCarryFlag);

    if high_nibble(rega.read()) + high_nibble(reg1) > 15 {
        flags.insert(CarryFlag)
    }

    if low_nibble(rega.read()) + low_nibble(reg1) > 15 {
        flags.insert(HalfCarryFlag);
    }

    rega.write(high_byte(sum));
    regb.write(low_byte(sum));
    freg.write(flags);
}

/// Increment register by 1
/// Set ZeroFlag if result is 0
/// Set HalfCarryFlag if there is a carry from bit 3
pub fn increment_register(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    let mut flags = freg.read();
    flags.remove(SubtractFlag);
    flags.remove(HalfCarryFlag);
    flags.remove(ZeroFlag);

    if low_nibble(val) == 0xF {
        flags.insert(HalfCarryFlag);
    }
    reg.increment();

    if reg.read() == 0 {
        flags.insert(ZeroFlag);
    }
    debug!("increment reg new_val: {:X}", reg.read());
    freg.write(flags);
}


pub fn increment_value_at_address(mem: &mut Memory, hb: u8, lb: u8, freg: &mut Register<Flags>) {
    let addr = pack_u16(hb, lb);
    let val = mem.read_byte(addr);
    let mut reg = Register::new(val);
    increment_register(&mut reg, freg);
    mem.write_byte(addr, reg.read());
}

/// Increments the pair of registers as if they represent a 16-bit value
pub fn increment_register_pair(msb: &mut Register<u8>,lsb: &mut Register<u8>) {
    debug!("increment register pair");
    let incremented_val = ((msb.read() as usize) << 8).wrapping_add((lsb.read() as usize).wrapping_add(1));
    msb.write(high_byte(incremented_val as u16));
    lsb.write(low_byte(incremented_val as u16));
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_add_u8_immediate() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0x2736);
        let mut reg = Register::new(0x05);
        let mut freg = Register::new(Flags::empty());
        mem.write_byte(pc.read(), 0x11);

        add_u8_immediate(&mem, &mut pc, &mut reg, &mut freg, false);

        assert!(pc.read() == 0x2737);
        assert!(reg.read() == 0x16);
        assert!(freg.read() == Flags::empty());

        mem.write_byte(pc.read(), 0x0C);
        add_u8_immediate(&mem, &mut pc, &mut reg, &mut freg, false);

        assert!(pc.read() == 0x2738);
        assert!(reg.read() == 0x22);
        assert!(freg.read() == HalfCarryFlag);

        mem.write_byte(pc.read(), 0x0F);
        freg.write(CarryFlag);
        add_u8_immediate(&mem, &mut pc, &mut reg, &mut freg, true);

        assert!(pc.read() == 0x2739);
        assert!(reg.read() == 0x32);
        assert!(freg.read() == HalfCarryFlag);

        mem.write_byte(pc.read(), 0x01);
        reg.write(0xFF);
        add_u8_immediate(&mem, &mut pc, &mut reg, &mut freg, false);
        assert!(reg.read() == 0x00);
        assert!(freg.read() == ZeroFlag);
    }

    #[test]
    fn test_add_value_at_address() {
        let mut first = Register::new(0x05);
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut flags = Register::new(SubtractFlag | CarryFlag);

        mem.write_byte(0x8476, 0x0B);
        add_value_at_address(&mut mem, &mut first, 0x84, 0x76, &mut flags);
        assert!(first.read() == 0x10, "Expected: {}, Actual: {}", "16", first.read());
        assert!(flags.read() == HalfCarryFlag, "HalfCarry should be set");

        let mut a = Register::new(0xFA);
        mem.write_byte(0xADCD, 0x07);

        add_value_at_address(&mut mem, &mut a, 0xAD, 0xCD, &mut flags);

        assert!(a.read() == 0x01);
        assert!(flags.read() == CarryFlag | HalfCarryFlag, "HalfCarry and CarryFlag should be set");

        a.write(0);

        add_value_at_address(&mut mem, &mut a, 0x11, 0x11, &mut flags);

        assert!(a.read() == 0x0);
        assert!(flags.read() == ZeroFlag);
    }

    #[test]
    fn test_add_register_pair_to_register_pair() {
        let mut rega = Register::new(0x11);
        let mut regb = Register::new(0x11);

        let mut reg1 = Register::new(0x11);
        let mut reg2 = Register::new(0x11);

        let mut freg = Register::new(ZeroFlag | SubtractFlag | HalfCarryFlag | CarryFlag);

        // Basic add make sure ZeroFlag isn't affected
        add_register_pair_to_register_pair(&mut rega, &mut regb, reg1.read(), reg2.read(), &mut freg);

        assert!(pack_u16(rega.read(), regb.read()) == 0x2222);
        assert!(freg.read() == ZeroFlag);

        rega.write(0xF1);
        regb.write(0xAB);
        reg1.write(0x12);
        reg2.write(0x12);

        // Carry from bit 15
        add_register_pair_to_register_pair(&mut rega, &mut regb, reg1.read(), reg2.read(), &mut freg);

        assert!(pack_u16(rega.read(), regb.read()) == 0x03BD);
        assert!(freg.read() == ZeroFlag | CarryFlag);

        rega.write(0x1E);
        regb.write(0xAB);
        reg1.write(0x12);
        reg2.write(0x16);
        freg.write(ZeroFlag);

        // Carry from bit 11
        add_register_pair_to_register_pair(&mut rega, &mut regb, reg1.read(), reg2.read(), &mut freg);

        assert!(pack_u16(rega.read(), regb.read()) == 0x30C1);
        println!("{}", freg.read().bits());
        assert!(freg.read() == ZeroFlag | HalfCarryFlag);
    }

    #[test]
    fn test_add_reg_with_reg() {
        let mut first = Register::new(0x05);
        let mut second = Register::new(0x0B);
        let mut flags = Register::new(SubtractFlag | CarryFlag);

        add(&mut first, second.read(), &mut flags);
        assert!(first.read() == 0x10, "Expected: {}, Actual: {}", "16", first.read());
        assert!(flags.read() == HalfCarryFlag, "HalfCarry should be set");

        let mut a = Register::new(0xFA);
        let mut b = Register::new(0x07);

        add(&mut a, b.read(), &mut flags);

        assert!(a.read() == 0x01);
        assert!(flags.read() == CarryFlag | HalfCarryFlag, "HalfCarry and CarryFlag should be set");

        a.write(0);
        b.write(0);

        add(&mut a, b.read(), &mut flags);

        assert!(a.read() == 0x0);
        assert!(flags.read() == ZeroFlag);
    }

    #[test]
    fn test_adc_value_at_address() {
        let address = 0x1038;
        let mut mem = EmptyMemory::new(0xFFFF);
        let val = 0x10;
        let mut reg = Register::new(0x00);
        let mut freg = Register::new(CarryFlag);

        mem.write_byte(address, val);

        adc_value_at_address(&mem, &mut reg, address, &mut freg);

        assert!(reg.read() == val + 1);
    }

    #[test]
    fn test_adc() {
        let mut first = Register::new(0x05);
        let mut flags = Register::new(SubtractFlag | CarryFlag);

        adc(&mut first, 0x0A, &mut flags);
        assert!(first.read() == 0x10, "Expected: {}, Actual: {}", 0x10, first.read());
        assert!(flags.read() == HalfCarryFlag, "HalfCarry should be set");

        flags.write(CarryFlag);
        let mut a = Register::new(0xFA);

        adc(&mut a, 0x06, &mut flags);

        assert!(a.read() == 0x01);
        assert!(flags.read() == CarryFlag | HalfCarryFlag, "HalfCarry and CarryFlag should be set");

        flags.write(CarryFlag);
        a.write(0);

        adc(&mut a, 0, &mut flags);

        assert!(a.read() == 0x01);
        assert!(flags.read() == Flags::empty());

        a.write(0);
        adc(&mut a, 0, &mut flags);
        assert!(flags.read() == ZeroFlag);
    }


    #[test]
    fn test_increment_register_pair() {
        let mut msb = Register::new(0x11);
        let mut lsb = Register::new(0x11);

        increment_register_pair(&mut msb, &mut lsb);

        assert!(msb.read() == 0x11);
        assert!(lsb.read() == 0x12);

        let mut msb_2 = Register::new(0x10);
        let mut lsb_2 = Register::new(0xFF);

        increment_register_pair(&mut msb_2, &mut lsb_2);

        assert!(msb_2.read() == 0x11);
        assert!(lsb_2.read() == 0x00);
    }

    #[test]
    fn test_increment_register() {
        let mut reg = Register::new(1);
        let mut freg = Register::new(ZeroFlag | HalfCarryFlag);

        increment_register(&mut reg, &mut freg);

        assert!(reg.read() == 2);
        assert!(freg.read() == Flags::empty());

        let mut regb = Register::new(0x0F);

        increment_register(&mut regb, &mut freg);

        assert!(regb.read() == 0x10);
        assert!(freg.read() == HalfCarryFlag);

        let mut regc = Register::new(0xFF);
        freg.write(Flags::empty());

        increment_register(&mut regc, &mut freg);

        assert!(regc.read() == 0x00);
        assert!(freg.read() == HalfCarryFlag | ZeroFlag);
    }

    #[test]
    fn test_increment_value_at_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut freg = Register::new(CarryFlag);

        increment_value_at_address(&mut mem, 0x10, 0x10, &mut freg);

        assert!(mem.read_byte(0x1010) == 1);
        assert!(freg.read() == CarryFlag);

        mem.write_byte(0x01AB, 0x1F);

        increment_value_at_address(&mut mem, 0x01, 0xAB, &mut freg);

        assert!(mem.read_byte(0x01AB) == 0x20);
        assert!(freg.read().contains(CarryFlag));
        assert!(freg.read().contains(HalfCarryFlag));

        freg.write(SubtractFlag);
        mem.write_byte(0xABCD, 0xED);
        increment_value_at_address(&mut mem, 0xAB, 0xCD, &mut freg);

        assert!(mem.read_byte(0xABCD) == 0xEE);
        assert!(freg.read() == Flags::empty());
    }


}
