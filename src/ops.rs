use memory::{Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

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

fn add_internal(first: &mut Register<u8>, second: u8, freg: &mut Register<Flags>, includeCarry: bool) {
    let val1 = first.read();
    let val2 = second;
    let mut result = val1 + val2;
    let mut doCarry = false;

    if freg.read().contains(CarryFlag) && includeCarry {
        doCarry = true;
        result += 1;
    }

    let mut flags = Flags::empty();
    
    if half_carry_for_add(val1, val2, doCarry) {
        flags = HalfCarryFlag;
    }

    if carry_for_add(val1, val2, doCarry) {
        flags = flags | CarryFlag;
    }

    if result == 0 && flags == Flags::empty() {
        flags = ZeroFlag;
    }

    first.write(result);
    freg.write(flags);
}

/// Add the value of two registers and store it in the first register
pub fn add(first: &mut Register<u8>, second: u8, freg: &mut Register<Flags>) {
    add_internal(first, second, freg, false);
}

/// Load the value from one register into another
pub fn ld_reg_to_reg(target: &mut Register<u8>, source: &Register<u8>) {
    debug!("ld_reg_to_reg: {}", source.read());
    let val = source.read();
    target.write(val);
}

/// Loads the memory pointed to by the next two bytes into a register
pub fn ld_u8_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>) {
    let val = mem.read_byte(pc.read());
    debug!("ld_next_byte_to_reg: {} {}", pc.read(), val);
    pc.increment(); 
    reg.write(val);
}

/// Loads the next two bytes into the passed in registers
pub fn ld_u16_immediate(mem: &Memory, pc: &mut Register<u16>, 
                                 hb: &mut Register<u8>, lb: &mut Register<u8>) {
    let val = mem.read_word(pc.read());
    pc.increment();
    pc.increment();
    debug!("ld_next_two_bytes_into_reg_pair: {}", val);
    hb.write(high_byte(val));
    lb.write(low_byte(val));
}

/// Writes the passed in value to memory at the address pointed to by combined address parameters
pub fn write_value_to_memory_at_address(mem: &mut Memory, val: u8, addr_msb: u8, addr_lsb: u8) {
   let addr = pack_u16(addr_msb, addr_lsb);
   mem.write_byte(addr as u16, val);
}

/// Increments the pair of registers as if they represent a 16-bit value
pub fn increment_register_pair(msb: &mut Register<u8>,lsb: &mut Register<u8>) {
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

/// Rotate register left
/// Set ZeroFlag if result is zero
/// Set CarryFlag if bit 7 is 1
pub fn rotate_left_with_carry(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    freg.write(Flags::empty());

    if val == 0 {
        freg.write(ZeroFlag);
        return;
    }

    if val & 0x80 != 0 {
        freg.write(CarryFlag);
    }

    reg.write(val << 1);
}

/// Write sp to address with value of next two bytes
pub fn write_u16_immediate_address(mem: &mut Memory, pc: &mut Register<u16>, val: u16){
    let addr = mem.read_word(pc.read());
    pc.increment();
    pc.increment();

    debug!("Writing {} to {}", val, addr);
    mem.write_word(addr, val);
}

/// Performs no operation and consumes a cycle
pub fn nop() {
    debug!("nop");
}

/// Adds two sets of registers as 16 bit numbers with carries counted on bit 11 and 15
pub fn add_register_pair_to_register_pair(rega: &mut Register<u8>, regb: &mut Register<u8>, reg1: u8, reg2: u8, freg: &mut Register<Flags>) {
    let first = pack_u16(rega.read(), regb.read());
    let second = pack_u16(reg1, reg2);

    let sum = first + second;

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

pub fn ld_from_reg_pair_as_address(mem: &Memory, reg: &mut Register<u8>, reg1: u8, reg2: u8) {
    let addr = pack_u16(reg1, reg2);
    let val = mem.read_byte(addr);
    reg.write(val);
}

pub fn decrement_register_pair(reg1: &mut Register<u8>, reg2: &mut Register<u8>) {
    let val = pack_u16(reg1.read(), reg2.read());
    let ans = val - 1;

    reg1.write(high_byte(ans));
    reg2.write(low_byte(ans));
}

pub fn rotate_right_with_carry(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();

    if val == 0 {
        freg.write(ZeroFlag);
    } else {
        if val & 0x01 == 1 {
            freg.write(CarryFlag);
            reg.write(val >> 1);
        }
    }
}

/// Add n to current address and jump to it - n = one byte signed immediate value
pub fn jump_by_signed_immediate(mem: &Memory, pc: &mut Register<u16>) {
    let offset = mem.read_byte(pc.read());
    pc.increment();
    let current_pc = pc.read();
    let mut new_pc = 0;
    if (offset & 0x80) == 0 {
        new_pc = current_pc + offset as u16;
    } else {
        new_pc = current_pc - (offset & 0x7F) as u16;
    }
    pc.write(new_pc);
}

pub fn relative_jmp_by_signed_immediate_if_not_flag(mem: &Memory, pc: &mut Register<u16>, freg: &Register<Flags>, flag: Flags) {
    if !freg.read().contains(flag) {
        jump_by_signed_immediate(mem, pc);
    } else {
        pc.increment();
    }
}

pub fn write_value_to_memory_at_address_and_increment_register(mem: &mut Memory, val: u8, high_reg: &mut Register<u8>, low_reg: &mut Register<u8>) {
    let address = pack_u16(high_reg.read(), low_reg.read());
    mem.write_byte(address, val);
    let new_address = address + 1;
    high_reg.write(high_byte(new_address));
    low_reg.write(low_byte(new_address));
}

pub fn write_value_to_memory_at_address_and_decrement_register(mem: &mut Memory, val: u8, high_reg: &mut Register<u8>, low_reg: &mut Register<u8>) {
    let address = pack_u16(high_reg.read(), low_reg.read());
    mem.write_byte(address, val);
    let new_address = address - 1;
    high_reg.write(high_byte(new_address));
    low_reg.write(low_byte(new_address));
}

pub fn relative_jmp_by_signed_immediate_if_flag(mem: &Memory, pc: &mut Register<u16>, freg: &Register<Flags>, flag: Flags) {
    if freg.read().contains(flag) {
        jump_by_signed_immediate(mem, pc);
    } else {
        pc.increment();
    }
}

pub fn ld_from_address_pointed_to_by_register_pair_and_increment_register_pair(mem: &Memory, reg: &mut Register<u8>, high_byte: &mut Register<u8>, low_byte: &mut Register<u8>) {
   let address = pack_u16(high_byte.read(), low_byte.read()); 

   let val = mem.read_byte(address);

   reg.write(val);
   increment_register_pair(high_byte, low_byte);
}

pub fn complement(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    reg.write(!val);

    let mut flags = freg.read();
    flags.insert(HalfCarryFlag);
    flags.insert(SubtractFlag);
    freg.write(flags);
}

/// Loads the next two bytes into the passed in register (sp)
pub fn ld_next_two_bytes_into_reg(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u16>) {
    let val = mem.read_word(pc.read());
    pc.increment();
    pc.increment();
    debug!("ld_next_two_bytes_into_reg: {}", val);
    reg.write(val);
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

pub fn ld_u8_immediate_into_address(mem: &mut Memory, pc: &mut Register<u16>, hb: u8, lb: u8) {
    let addr = pack_u16(hb, lb);
    let val = mem.read_byte(pc.read());
    pc.increment();
    mem.write_byte(addr, val);
}

pub fn set_flag(freg: &mut Register<Flags>, flag: Flags) {
    let mut flags = freg.read();
    flags.insert(flag);
    freg.write(flags);
}

pub fn ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair(mem: &Memory, reg: &mut Register<u8>, high_byte: &mut Register<u8>, low_byte: &mut Register<u8>) {
   let address = pack_u16(high_byte.read(), low_byte.read()); 

   let val = mem.read_byte(address);

   reg.write(val);
   decrement_register_pair(high_byte, low_byte);
}

pub fn reset_flag(freg: &mut Register<Flags>, flag: Flags) {
    let mut f = freg.read();
    f.remove(flag);
    freg.write(f);
}

pub fn ld_u8(reg: &mut Register<u8>, val: u8) {
    reg.write(val);
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

fn half_carry_for_subtract(val1: u8, val2: u8, carry: u8) -> bool {
    if low_nibble(val2) + carry == 0x10 && val1 >= 0x10 {
        return true;
    } else {
        return low_nibble(val1) >= (low_nibble(val2) + carry);
    }

}

fn carry_for_subtract(val1: u8, val2: u8, carry: u8) -> bool {
    return val1 >= (val2 + carry);
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

    let result = reg_val - val - carry;
    
    if result == 0 && flags.contains(CarryFlag) {
        flags.insert(ZeroFlag);
    }
    println!("{}", result);
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

    if (reg_val == val) && flags.contains(CarryFlag) {
        flags.insert(ZeroFlag);
    }
    freg.write(flags);
}

pub fn or_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    or(reg, val, freg);
}

pub fn compare_value_at_address(mem: &Memory, reg: &mut Register<u8>, addr: u16, freg: &mut Register<Flags>) {
    let val = mem.read_byte(addr);
    compare(reg, val, freg);
}

pub fn push(mem: &mut Memory, sp: &mut Register<u16>, val: u16) {
    sp.decrement();
    sp.decrement();
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

    hb.write(high_byte(val));
    lb.write(low_byte(val));
}

pub fn ret(mem: &Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, should_execute: bool) {
    if should_execute {
        let addr = pop_internal(mem, sp); 
        pc.write(addr);
    }
}

pub fn jp_u16_immediate(mem: &Memory, pc: &mut Register<u16>) {
    let addr = mem.read_word(pc.read());
    pc.write(addr);
}

pub fn jp_u16_immediate_if_true(mem: &Memory,pc: &mut Register<u16>,should_jump: bool) {
    if should_jump {
        jp_u16_immediate(mem, pc);
    } else {
        pc.increment();
        pc.increment();
    }
}

pub fn call_immediate_if_true(mem: &mut Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, should_jump: bool) {
    if should_jump {
        let new_addr = mem.read_word(pc.read());
        push(mem, sp, pc.read() + 2);
        pc.write(new_addr);
    } else {
        pc.increment();
        pc.increment();
    }
}

pub fn add_u8_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>, freg: &mut Register<Flags>, with_carry: bool) {
    let val = mem.read_byte(pc.read());
    add_internal(reg, val, freg, with_carry);
    pc.increment();
}

pub fn call(mem: &mut Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, addr: u16) {
    push(mem, sp, pc.read());
    pc.write(addr);
}

pub fn sub_u8_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>, freg: &mut Register<Flags>, with_carry: bool) {
    let val = mem.read_byte(pc.read());
    internal_sub(reg, val, freg, with_carry);
    pc.increment();
}

pub fn reti(mem: &Memory, pc: &mut Register<u16>, sp: &mut Register<u16>, ime: &mut bool) {
    ret(mem, pc, sp, true);
    *ime = true;
}

pub fn u8_immediate(mem: &Memory, pc: &mut Register<u16>) -> u8 {
    let val = mem.read_byte(pc.read());
    pc.increment();
    return val;
}

#[test]
fn test_u8_immediate() {
    let mut mem = Memory::new(0xFFFF);
    let mut pc = Register::new(0x123);
    mem.write_byte(pc.read(), 0x43);

    let val = u8_immediate(&mem, &mut pc);

    assert!(val == 0x43);
    assert!(pc.read() == 0x124);
}

#[test]
fn test_reti() {
    let mut mem = Memory::new(0xFFFF);
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
fn test_sub_u8_immediate() {
    let mut mem = Memory::new(0xFFFF);
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
fn test_call() {
    let mut mem = Memory::new(0xFFFF);
    let mut pc = Register::new(0x4324);
    let mut sp = Register::new(0xFFFE);
    let addr = 0x10;

    call(&mut mem, &mut pc, &mut sp, addr);

    assert!(pc.read() == 0x0010);
    assert!(sp.read() == 0xFFFC);
    assert!(mem.read_word(sp.read()) == 0x4324);
}

#[test]
fn test_add_u8_immediate() {
    let mut mem = Memory::new(0xFFFF);
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

}

#[test]
fn test_push() {
    let mut mem = Memory::new(0xFFFF);
    let mut sp = Register::new(0xFFAB);
     
    push(&mut mem, &mut sp, 0x8735);
    assert!(sp.read() == 0xFFA9);
    assert!(mem.read_word(sp.read()) == 0x8735);
}

#[test]
fn test_call_immediate_if_true() {
    let mut mem = Memory::new(0xFFFF);
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
    let mut mem = Memory::new(0xFFFF);
    let mut pc = Register::new(0x6542);
    mem.write_word(pc.read(), 0x1234);
     
    pc.write(0x6542);
    jp_u16_immediate(&mem, &mut pc);
    assert!(pc.read() == 0x1234);
}

#[test]
fn test_jp_u16_immediate_if_true() {
    let mut mem = Memory::new(0xFFFF);
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
    let mut mem = Memory::new(0xFFFF);
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
    let mut mem = Memory::new(0xFFFF);
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
fn test_or_value_at_address() {
    let mut mem = Memory::new(0xFFFF);
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
fn test_compare_value_at_address() {
    let mut mem = Memory::new(0xFFFF);
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
fn test_and_value_at_address() {
    let mut mem = Memory::new(0xFFFF);
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
    let mut mem = Memory::new(0xFFFF);
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
fn test_sbc_value_at_address() {
    let mut mem = Memory::new(0xFFFF);
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
    let mut mem = Memory::new(0xFFFF);
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
    assert!(freg.read() == SubtractFlag | HalfCarryFlag | CarryFlag);

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
fn test_adc_value_at_address() {
    let address = 0x1038;
    let mut mem = Memory::new(0xFFFF);
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
    assert!(first.read() == 0x10, "Expected: {}, Actual: {}", "16", first.read());
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
fn test_add_value_at_address() {
    let mut first = Register::new(0x05);
    let mut mem = Memory::new(0xFFFF);
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
fn test_ld_u8() {
    let mut reg = Register::new(10);

    ld_u8(&mut reg, 0x18);

    assert!(reg.read() == 0x18);
}

#[test]
fn test_reset_flag() {
    let mut freg = Register::new(CarryFlag | ZeroFlag);

    reset_flag(&mut freg, CarryFlag);

    assert!(!freg.read().contains(CarryFlag));
    assert!(freg.read().contains(ZeroFlag));
}

#[test]
fn test_ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair() {
    let mut mem = Memory::new(0xFFFF);
    let mut reg = Register::new(0x12);
    let mut high_byte = Register::new(0xAB);
    let mut low_byte = Register::new(0xCD);

    mem.write_byte(0xABCD, 0x54);
    ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair(&mem, &mut reg, &mut high_byte, &mut low_byte);

    assert!(reg.read() == 0x54);
    assert!(low_byte.read() == 0xCC);
}

#[test]
fn test_set_flag() {
    let mut freg = Register::new(Flags::empty());

    set_flag(&mut freg, CarryFlag);

    assert!(freg.read().contains(CarryFlag));
}

#[test]
fn test_ld_u8_immediate_into_address() {
    let mut mem = Memory::new(0xFFFF);
    let mut pc = Register::new(0xAD12);
    mem.write_byte(0xAD12, 0xBB);

    ld_u8_immediate_into_address(&mut mem, &mut pc, 0x12, 0x34);

    assert!(mem.read_byte(0x1234) == 0xBB);
    assert!(pc.read() == 0xAD13);
}

#[test]
fn test_decrement_value_at_address() {
    let mut mem = Memory::new(0xFFFF);
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
    let mut mem = Memory::new(0xFFFF);
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
    let mut mem = Memory::new(0xFFFF);
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
fn test_ld_next_two_bytes_into_reg() {
    let mut mem = Memory::new(65536);
    let mut pc = Register::new(11);
    let mut reg = Register::new(0);
    
    mem.write_word(11, 0xDEAB);

    ld_next_two_bytes_into_reg(&mem, &mut pc, &mut reg);
    assert!(pc.read() == 13);
    assert!(reg.read() == 0xDEAB);
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
fn test_ld_from_address_pointed_to_by_register_pair_and_increment_register_pair() {
    let mut mem = Memory::new(0xFFFF);
    let mut reg = Register::new(0x12);
    let mut high_byte = Register::new(0xAB);
    let mut low_byte = Register::new(0xCD);

    mem.write_byte(0xABCD, 0x54);
    ld_from_address_pointed_to_by_register_pair_and_increment_register_pair(&mem, &mut reg, &mut high_byte, &mut low_byte);

    assert!(reg.read() == 0x54);
    assert!(low_byte.read() == 0xCE);
}

#[test]
fn test_relative_jmp_by_signed_immediate_if_flag() {
    let mut mem = Memory::new(0xFFFF);
    let mut pc = Register::new(0x1234);
    let mut freg = Register::new(Flags::empty());

    // Forwards
    freg.write(ZeroFlag);
    mem.write_byte(0x1234, 0x55);
    relative_jmp_by_signed_immediate_if_flag(&mem, &mut pc, &freg, ZeroFlag);
    assert!(pc.read() == 0x128A, "Should jump forwards");

    // Backwards
    freg.write(ZeroFlag);
    mem.write_byte(0x128A, 0x81);
    relative_jmp_by_signed_immediate_if_flag(&mem, &mut pc, &freg, ZeroFlag);
    assert!(pc.read() == 0x128A, "Should jump back"); 
    
    // No jump because ZeroFlag is not set
    freg.write(Flags::empty());
    mem.write_byte(0x1288, 0xFF);
    relative_jmp_by_signed_immediate_if_flag(&mem, &mut pc, &freg, ZeroFlag);
    assert!(pc.read() == 0x128B, "Should not jump if ZeroFlag is not set. PC should increment to go past immediate value");

}

#[test]
fn test_write_value_to_memory_at_address_and_increment_register() {
    let mut mem = Memory::new(0xFFFF);
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
fn test_relative_jmp_by_signed_immediate_if_not_flag() {
    let mut mem = Memory::new(0xFFFF);
    let mut pc = Register::new(0x1234);
    let mut freg = Register::new(Flags::empty());

    // Forwards
    mem.write_byte(0x1234, 0x55);
    relative_jmp_by_signed_immediate_if_not_flag(&mem, &mut pc, &freg, ZeroFlag);
    assert!(pc.read() == 0x128A, "Should jump forwards");

    // Backwards
    mem.write_byte(0x1289, 0x85);
    relative_jmp_by_signed_immediate_if_not_flag(&mem, &mut pc, &freg, CarryFlag);
    assert!(pc.read() == 0x128B, "Should jump back"); 
    
    // No jump because ZeroFlag is set
    freg.write(ZeroFlag);
    mem.write_byte(0x128B, 0xFF);
    relative_jmp_by_signed_immediate_if_not_flag(&mem, &mut pc, &freg, ZeroFlag);
    println!("{}", pc.read());
    assert!(pc.read() == 0x128C, "Should not jump if ZeroFlag is set");
}

#[test]
fn test_jump_by_signed_immediate() {
    let mut mem = Memory::new(0x10000);
    let mut pc = Register::new(0x0101);
    // 0x8A = -10 as i8
    mem.write_byte(0x0101, 0x8A);

    jump_by_signed_immediate(&mem, &mut pc);

    assert!(pc.read() == 0xF8, "Should jump backwards with negative number");

    mem.write_byte(0xF8, 0x37);

    jump_by_signed_immediate(&mem, &mut pc);

    assert!(pc.read() == 0x130);
}

#[test]
fn test_rotate_right_with_carry() {
    let mut reg = Register::new(0x99);
    let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

    rotate_right_with_carry(&mut reg, &mut freg);

    assert!(reg.read() == 0x4C);
    assert!(freg.read() == CarryFlag);

    reg.write(0x0);
    rotate_right_with_carry(&mut reg, &mut freg);

    assert!(reg.read() == 0x0);
    assert!(freg.read() == ZeroFlag);
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
fn test_ld_from_reg_pair_as_address() {
    let mut mem = Memory::new(65000);
    let mut rega = Register::new(0x00);

    let mut reg1 = Register::new(0x12);
    let mut reg2 = Register::new(0x34);

    mem.write_byte(0x1234, 0xAA);

    ld_from_reg_pair_as_address(&mem, &mut rega, reg1.read(), reg2.read());

    assert!(rega.read() == 0xAA);
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
fn test_write_stack_pointer_to_address_immediate() {
    let mut sp = Register::new(0xBEEF);
    let mut pc = Register::new(0x111);
    let mut mem = Memory::new(65647);

    mem.write_byte(0x111, 0xAD);
    mem.write_byte(0x112, 0xDE);

    write_u16_immediate_address(&mut mem, &mut pc, sp.read());
    assert!(pc.read() == 0x113);
    assert!(mem.read_word(0xDEAD) == 0xBEEF);
}

#[test]
fn test_rotate_left_with_carry() {
    let mut reg = Register::new(0x0F);
    let mut freg = Register::new(SubtractFlag | HalfCarryFlag);

    rotate_left_with_carry(&mut reg, &mut freg);
    
    // Rotate should happen
    assert!(reg.read() == 0x1E);
    assert!(freg.read() == Flags::empty());
    
    let mut regb = Register::new(0x00);

    rotate_left_with_carry(&mut regb, &mut freg);

    // Zero should return zero with ZeroFlag
    assert!(regb.read() == 0x00);
    assert!(freg.read() == ZeroFlag);

    let mut regc = Register::new(0xFF);

    rotate_left_with_carry(&mut regc, &mut freg);

    // Carry should get set
    assert!(regc.read() == 0xFE);
    assert!(freg.read() == CarryFlag);
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
fn test_ld_u8_immediate() {
    let mut mem = Memory::new(65536);
    let mut pc = Register::new(11);
    let mut reg = Register::new(0);

    mem.write_byte(11, 0xFA);

    ld_u8_immediate(&mem, &mut pc, &mut reg);
    assert!(reg.read() == 0xFA);
    assert!(pc.read() == 12);

}

#[test]
fn test_ld_reg_to_reg() {
    let mut target = Register::new(5);
    let mut source = Register::new(10);

    ld_reg_to_reg(&mut target, &source);

    assert!(target.read() == 10);
    assert!(source.read() == 10);
}

#[test]
fn test_ld_u16_immediate() {
    let mut mem = Memory::new(65536);
    let mut pc = Register::new(11);
    let mut reg = Register::new(0);
    let mut reg2 = Register::new(0);
    
    mem.write_word(11, 0xDEAB);

    ld_u16_immediate(&mem, &mut pc, &mut reg, &mut reg2);
    assert!(pc.read() == 13);
    assert!(reg.read() == 0xDE);
    assert!(reg2.read() == 0xAB);
}

#[test]
fn test_write_value_to_memory_at_address() {
    let mut mem = Memory::new(65536);
    let mut msb = 0xFF;
    let mut lsb = 0x11;
    let val = 100;

    write_value_to_memory_at_address(&mut mem, val, msb, lsb);

    assert!(mem.read_byte(0xFF11) == val, "Memory does match what was written");
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
