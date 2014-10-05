use memory::{Memory, pack_u16};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

/// Add the value of two registers and store it in the first register
pub fn add(first: &mut Register<u8>, second: &Register<u8>, freg: &mut Register<Flags>) {
    debug!("add: {} {}", first.read(), second.read());
    let new_value = first.read() + second.read();
    first.write(new_value);

    let mut flags = Flags::empty();
    if (new_value < first.read() || new_value < second.read()) {
        // Carry
        flags = flags | CarryFlag
    }
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
    let first = mem.read_byte(pc.read());
    pc.increment();
    let second = mem.read_byte(pc.read());
    pc.increment();
    
    hb.write(first);
    lb.write(second);
}

/// Writes the passed in value to memory at the address pointed to by combined address parameters
pub fn write_value_to_memory_at_address(mem: &mut Memory, val: u8, addr_msb: u8, addr_lsb: u8) {
   let addr = ((addr_msb as uint) << 8) + addr_lsb as uint;
   mem.write_byte(addr as u16, val);
}

/// Increments the pair of registers as if they represent a 16-bit value
pub fn increment_register_pair(msb: &mut Register<u8>,lsb: &mut Register<u8>) {
    let incremented_val = ((msb.read() as uint) << 8) + lsb.read() as uint + 1;
    msb.write(((incremented_val & 0xFF00) >> 8) as u8);
    lsb.write((incremented_val & 0x00FF) as u8);
}

/// Increment register by 1
/// Set ZeroFlag if result is 0
/// Set HalfCarryFlag if there is a carry from bit 3
pub fn increment_register(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    let mut flags = Flags::empty();

    if (val == 0x0F) {
        flags = HalfCarryFlag;
    }
    reg.increment();

    if (reg.read() == 0) {
        flags = flags | ZeroFlag;
    }
    freg.write(flags);
}

/// Decrement register by 1
/// Set ZeroFlag if result is 0
/// Set SubtractFlag
/// Set HalfCarryFlag if there is no borrow from bit 4
pub fn decrement_register(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();
    let mut flags = SubtractFlag;

    if ((val & 0x0F) > 0) {
        flags = flags | HalfCarryFlag;
    }
    
    reg.decrement();

    if (reg.read() == 0x00) {
        flags = flags | ZeroFlag;
    }
    freg.write(flags);
}

/// Rotate register left
/// Set ZeroFlag if result is zero
/// Set CarryFlag if bit 7 is 1
pub fn rotate_left_with_carry(reg: &mut Register<u8>, freg: &mut Register<Flags>) {
    let val = reg.read();

    if (val == 0) {
        freg.write(ZeroFlag);
        return;
    }

    if (val & 0x80 != 0) {
        freg.write(CarryFlag);
    }

    reg.write(val << 1);
}

/// Write sp to address with value of next two bytes
pub fn write_sp_to_address_immediate(mem: &mut Memory, pc: &mut Register<u16>, sp: &Register<u16>){
    let msb = mem.read_byte(pc.read());
    pc.increment();
    let lsb = mem.read_byte(pc.read());
    pc.increment();

    let addr = pack_u16(msb, lsb);
    println!("Writing {} to {}", sp.read(), addr);
    mem.write_word(addr, sp.read());
}

/// Performs no operation and consumes a cycle
pub fn nop() {
    println!("nop");
}

#[test]
fn test_write_stack_pointer_to_address_immediate() {
    let mut sp = Register::new(0xBEEF);
    let mut pc = Register::new(0x111);
    let mut mem = Memory::new(65647);

    mem.write_byte(0x111, 0xDE);
    mem.write_byte(0x112, 0xAD);

    write_sp_to_address_immediate(&mut mem, &mut pc, &sp);
    println!("{}", mem.read_word(0xDEAD));
    assert!(pc.read() == 0x113);
    assert!(mem.read_word(0xDEAD) == 0xBEEF);
}

#[test]
fn test_rotate_left_with_carry() {
    let mut reg = Register::new(0x0F);
    let mut freg = Register::new(Flags::empty());

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
    let mut freg = Register::new(Flags::empty());

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
    assert!(freg.read() == ZeroFlag);
}

#[test]
fn test_add_reg_with_reg() {
    let mut first = Register::new(5);
    let mut second = Register::new(9);
    let mut flags = Register::new(Flags::empty());

    add(&mut first, &second, &mut flags);
    assert!(first.read() == 14);
    assert!(flags.read() == Flags::empty());

    let mut a = Register::new(0xFA);
    let mut b = Register::new(0x07);
    
    add(&mut a, &b, &mut flags);

    assert!(a.read() == 0x01);
    assert!(flags.read().contains(CarryFlag));
}

#[test]
fn test_ld_immediate() {
    let mut mem = Memory::new(65536);
    let mut pc = Register::new(11);
    let mut reg = Register::new(0);

    mem.write_byte(11, 0xFF);

    ld_immediate(&mem, &mut pc, &mut reg);
    assert!(reg.read() == 0xFF);
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
    
    mem.write_byte(11, 0xDE);
    mem.write_byte(12, 0xAD);

    ld_next_two_byte_into_reg_pair(&mem, &mut pc, &mut reg, &mut reg2);
    assert!(pc.read() == 13);
    assert!(reg.read() == 0xDE);
    assert!(reg2.read() == 0xAD);
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
