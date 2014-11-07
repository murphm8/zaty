use memory::{Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

fn half_carry(val1: u8, val2: u8) -> bool {
    return (low_nibble(val1) + low_nibble(val2)) > 0x0F
}

fn carry(val1: u8, val2: u8) -> bool {
    return val1 as u16 + val2 as u16 > 0xFF;
}

/// Add the value of two registers and store it in the first register
pub fn add(first: &mut Register<u8>, second: &Register<u8>, freg: &mut Register<Flags>) {
    debug!("add: {} {}", first.read(), second.read());
    let val1 = first.read();
    let val2 = second.read();
    let result = val1 + val2;

    let mut flags = Flags::empty();
    
    if half_carry(val1, val2) {
        flags = HalfCarryFlag;
    }

    if carry(val1, val2) {
        flags = flags | CarryFlag;
    }

    if result == 0 && flags == Flags::empty() {
        flags = ZeroFlag;
    }

    first.write(result);
    freg.write(flags);

}

/// Load the value from one register into another
pub fn ld_reg_to_reg(target: &mut Register<u8>, source: &Register<u8>) {
    debug!("ld_reg_to_reg: {}", source.read());
    let val = source.read();
    target.write(val);
}

/// Loads the memory pointed to by the next two bytes into a register
pub fn ld_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>) {
    let val = mem.read_byte(pc.read());
    debug!("ld_next_byte_to_reg: {} {}", pc.read(), val);
    pc.increment(); 
    reg.write(val);
}

/// Loads the next two bytes into the passed in registers
pub fn ld_next_two_byte_into_reg_pair(mem: &Memory, pc: &mut Register<u16>, 
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
   let addr = ((addr_msb as uint) << 8) + addr_lsb as uint;
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
pub fn write_sp_to_address_immediate(mem: &mut Memory, pc: &mut Register<u16>, sp: &Register<u16>){
    let addr = mem.read_word(pc.read());
    pc.increment();
    pc.increment();

    debug!("Writing {} to {}", sp.read(), addr);
    mem.write_word(addr, sp.read());
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

pub fn ld_a_from_reg_pair_as_address(mem: &Memory, rega: &mut Register<u8>, reg1: &Register<u8>, reg2: &Register<u8>) {
    let addr = pack_u16(reg1.read(), reg2.read());
    let val = mem.read_byte(addr);
    rega.write(val);
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

pub fn ld_immediate_into_address(mem: &mut Memory, pc: &mut Register<u16>, hb: u8, lb: u8) {
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

#[test]
fn test_set_flag() {
    let mut freg = Register::new(Flags::empty());

    set_flag(&mut freg, CarryFlag);

    assert!(freg.read().contains(CarryFlag));
}

#[test]
fn test_ld_immediate_into_address() {
    let mut mem = Memory::new(0xFFFF);
    let mut pc = Register::new(0xAD12);
    mem.write_byte(0xAD12, 0xBB);

    ld_immediate_into_address(&mut mem, &mut pc, 0x12, 0x34);

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
fn test_ld_a_from_reg_pair_as_address() {
    let mut mem = Memory::new(65000);
    let mut rega = Register::new(0x00);

    let mut reg1 = Register::new(0x12);
    let mut reg2 = Register::new(0x34);

    mem.write_byte(0x1234, 0xAA);

    ld_a_from_reg_pair_as_address(&mem, &mut rega, &reg1, &reg2);

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

    write_sp_to_address_immediate(&mut mem, &mut pc, &sp);
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
    let mut flags = Register::new(SubtractFlag);

    add(&mut first, &second, &mut flags);
    assert!(first.read() == 0x10, "Expected: {}, Actual: {}", "16", first.read());
    assert!(flags.read() == HalfCarryFlag, "HalfCarry should be set");

    let mut a = Register::new(0xFA);
    let mut b = Register::new(0x07);
    
    add(&mut a, &b, &mut flags);

    assert!(a.read() == 0x01);
    assert!(flags.read() == CarryFlag | HalfCarryFlag, "HalfCarry and CarryFlag should be set");

    a.write(0);
    b.write(0);

    add(&mut a, &b, &mut flags);

    assert!(a.read() == 0x0);
    assert!(flags.read() == ZeroFlag);
}

#[test]
fn test_ld_immediate() {
    let mut mem = Memory::new(65536);
    let mut pc = Register::new(11);
    let mut reg = Register::new(0);

    mem.write_byte(11, 0xFA);

    ld_immediate(&mem, &mut pc, &mut reg);
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
fn test_ld_next_two_byte_into_reg_pair() {
    let mut mem = Memory::new(65536);
    let mut pc = Register::new(11);
    let mut reg = Register::new(0);
    let mut reg2 = Register::new(0);
    
    mem.write_word(11, 0xDEAB);

    ld_next_two_byte_into_reg_pair(&mem, &mut pc, &mut reg, &mut reg2);
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
