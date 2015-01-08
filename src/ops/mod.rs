use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

pub use self::add::adc;
pub use self::add::add;
pub use self::add::add_value_at_address;
pub use self::add::add_u8_immediate;
pub use self::add::adc_value_at_address;
pub use self::add::add_register_pair_to_register_pair;

pub use self::ld::ld_reg_to_reg;
pub use self::ld::ld_u8_immediate;
pub use self::ld::ld_u8;
pub use self::ld::ld_u8_immediate_into_address;
pub use self::ld::ld_u16_immediate;
pub use self::ld::write_u16_immediate_address;
pub use self::ld::ld_from_address;
pub use self::ld::write_val_FF00_plus_immediate;
pub use self::ld::load_val_FF00_plus_immediate;
pub use self::ld::write_value_to_u16_immediate;
pub use self::ld:: write_value_to_memory_at_address;
pub use self::ld::ld_next_two_bytes_into_reg;
pub use self::ld::write_value_to_memory_at_address_and_decrement_register;
pub use self::ld::write_value_to_memory_at_address_and_increment_register;
pub use self::ld::ld_from_address_pointed_to_by_register_pair_and_increment_register_pair;
pub use self::ld::ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair;

pub use self::sub::sbc;
pub use self::sub::sub_value_at_address;
pub use self::sub::sub;
pub use self::sub::sub_u8_immediate;
pub use self::sub::sbc_value_at_address;
pub use self::sub::compare;
pub use self::sub::compare_value_at_address;

pub use self::rotate::rotate_right;
pub use self::rotate::rotate_right_at_address;
pub use self::rotate::rotate_right_with_carry;
pub use self::rotate::rotate_right_with_carry_at_address;
pub use self::rotate::rotate_right_reset_zeroflag;
pub use self::rotate::rotate_right_with_carry_reset_zero;
pub use self::rotate::rotate_left;
pub use self::rotate::rotate_left_at_address;
pub use self::rotate::rotate_left_with_carry;
pub use self::rotate::rotate_left_with_carry_at_address;
pub use self::rotate::rotate_left_reset_zeroflag;
pub use self::rotate::rotate_left_with_carry_reset_zero;

pub use self::utils::u8_immediate;
pub use self::utils::u16_immediate;

mod add;
mod ld;
mod sub;
mod rotate;
mod utils;

// TODO: Rewrite tests to exervise all values, use bigger num sizes to detent overflow and other
// things another way?
// TODO: Split opcodes into separate files ops::add::func
// TODO: Implement functions on types instead of doing logic in method? eg u8.rotate_right(carry)




/// Increments the pair of registers as if they represent a 16-bit value
pub fn increment_register_pair(msb: &mut Register<u8>,lsb: &mut Register<u8>) {
    debug!("increment register pair");
    let incremented_val = ((msb.read() as uint) << 8) + lsb.read() as uint + 1;
    msb.write(high_byte(incremented_val as u16));
    lsb.write(low_byte(incremented_val as u16));
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

/// Performs no operation and consumes a cycle
pub fn nop() {
}

pub fn decrement_register_pair(reg1: &mut Register<u8>, reg2: &mut Register<u8>) {
    let val = pack_u16(reg1.read(), reg2.read());
    let ans = val - 1;

    reg1.write(high_byte(ans));
    reg2.write(low_byte(ans));
}

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

pub fn complement(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    reg.write(!val);

    let mut flags = freg.read();
    flags.insert(HalfCarryFlag);
    flags.insert(SubtractFlag);
    freg.write(flags);
}

pub fn increment_value_at_address(mem: &mut Memory, hb: u8, lb: u8, freg: &mut Register<Flags>) {
    let addr = pack_u16(hb, lb);
    let val = mem.read_byte(addr);
    let mut reg = Register::new(val);
    increment_register(&mut reg, freg);
    mem.write_byte(addr, reg.read());
}

pub fn decrement_value_at_address(mem: &mut Memory, hb: u8, lb: u8, freg: &mut Register<Flags>) {
    let addr = pack_u16(hb, lb);
    let val = mem.read_byte(addr);
    let mut reg = Register::new(val);
    decrement_register(&mut reg, freg);
    mem.write_byte(addr, reg.read());
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

pub fn push(mem: &mut Memory, sp: &mut Register<u16>, val: u16) {
    sp.decrement();
    sp.decrement();
    debug!("push sp:{:X} val:{:X}", sp.read(), val);
    mem.write_word(sp.read(), val);
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

pub fn jp(pc: &mut Register<u16>, addr: u16) {
    pc.write(addr);
}

pub fn bit(reg_val: u8, pos: u8, freg: &mut Register<Flags>) {
    let mut flags = freg.read();
    flags.remove(SubtractFlag);
    flags.insert(HalfCarryFlag);

    assert!(pos < 8, "Bit positions are 0-7");

    let mask = 0x01 << pos as uint;
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
    let mask = 0x01 << pos as uint;
    let val = reg.read();
    let reset_val = val & (!mask);
    reg.write(reset_val);
}

pub fn res_at_addr(mem: &mut Memory, address: u16, pos: u8) {
    let mask = 0x01 << pos as uint;
    let val = mem.read_byte(address);
    let reset_val = val & (!mask);
    mem.write_byte(address, reset_val);
}

pub fn set(reg: &mut Register<u8>, pos: u8) {
    let mask = 0x01 << pos as uint;
    let val = reg.read();
    let reset_val = val | mask;
    reg.write(reset_val);
}

pub fn set_at_addr(mem: &mut Memory, address: u16, pos: u8) {
    let mask = 0x01 << pos as uint;
    let val = mem.read_byte(address);
    let reset_val = val | mask;
    mem.write_byte(address, reset_val);
}

fn internal_sla(val: u8, freg: &mut Register<Flags>) -> u8 {
    freg.write(Flags::empty());
    let result = val << 1;

    if result == 0 {
        freg.write(ZeroFlag);
    }

    if val & 0x80 != 0 {
        freg.write(CarryFlag);
    }

    return result;
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
fn test_srl_at_address() {
    let mut mem = EmptyMemory::new(0xFFFF);
    let mut freg = Register::new(HalfCarryFlag);
    let mut val = 0x00;
    let addr = 0x3727;
    mem.write_byte(addr, val);
    srl_at_address(&mut mem, addr, &mut freg);

    assert!(freg.read() == ZeroFlag);
    assert!(mem.read_byte(addr) == 0x00);

    val = 0b00110011;
    mem.write_byte(addr, val);
    srl_at_address(&mut mem, addr, &mut freg);
    assert!(mem.read_byte(addr) == 0b00011001);
    assert!(freg.read() == CarryFlag);

    srl_at_address(&mut mem, addr, &mut freg);
    assert!(mem.read_byte(addr) == 0b00001100);
    assert!(freg.read() == CarryFlag);
}

#[test]
fn test_srl() {
    let mut reg = Register::new(0x00);
    let mut freg = Register::new(HalfCarryFlag);
    srl(&mut reg, &mut freg);

    assert!(freg.read() == ZeroFlag);
    assert!(reg.read() == 0x00);

    reg.write(0b00110011);
    srl(&mut reg, &mut freg);
    assert!(reg.read() == 0b00011001);
    assert!(freg.read() == CarryFlag);

    srl(&mut reg, &mut freg);
    assert!(reg.read() == 0b00001100);
    assert!(freg.read() == CarryFlag);
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
fn test_jp() {
    let mut pc = Register::new(0x2311);
    let addr = 0x1223;

    jp(&mut pc, addr);

    assert!(pc.read() == addr);
}

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

#[test]
fn test_write_value_to_memory_at_address_and_decrement_register() {
    let mut mem = EmptyMemory::new(0xFFFF);
    let mut val = 0x8;
    let mut high_byte = Register::new(0x12);
    let mut low_byte = Register::new(0x34);

    write_value_to_memory_at_address_and_decrement_register(&mut mem, val, &mut high_byte, &mut low_byte);
    assert!(low_byte.read() == 0x33, "Should increment register");
    assert!(mem.read_byte(0x1234) == 0x8, "Should correctly write value");

    high_byte.write(0x11);
    low_byte.write(0x00);
    write_value_to_memory_at_address_and_decrement_register(&mut mem, val, &mut high_byte, &mut low_byte);
    assert!(mem.read_byte(0x1100) == 0x8);
    assert!(high_byte.read() == 0x10);
    assert!(low_byte.read() == 0xFF);
}

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
fn test_write_value_to_memory_at_address_and_increment_register() {
    let mut mem = EmptyMemory::new(0xFFFF);
    let mut val = 0x8;
    let mut high_byte = Register::new(0x12);
    let mut low_byte = Register::new(0x34);

    write_value_to_memory_at_address_and_increment_register(&mut mem, val, &mut high_byte, &mut low_byte);
    assert!(low_byte.read() == 0x35, "Should increment register");
    assert!(mem.read_byte(0x1234) == 0x8, "Should correctly write value");

    low_byte.write(0xFF);
    write_value_to_memory_at_address_and_increment_register(&mut mem, val, &mut high_byte, &mut low_byte);
    assert!(mem.read_byte(0x12FF) == 0x8);
    assert!(high_byte.read() == 0x13);
    assert!(low_byte.read() == 0x00);
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
fn test_decrement_register_pair() {
    let mut reg1 = Register::new(0x70);
    let mut reg2 = Register::new(0x00);

    decrement_register_pair(&mut reg1, &mut reg2);

    assert!(reg1.read() == 0x6F);
    assert!(reg2.read() == 0xFF);
}


#[test]
fn test_write_stack_pointer_to_address_immediate() {
    let mut sp = Register::new(0xBEEF);
    let mut pc = Register::new(0x111);
    let mut mem = EmptyMemory::new(65647);

    mem.write_byte(0x111, 0xAD);
    mem.write_byte(0x112, 0xDE);

    write_u16_immediate_address(&mut mem, &mut pc, sp.read());
    assert!(pc.read() == 0x113);
    assert!(mem.read_word(0xDEAD) == 0xBEEF);
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
