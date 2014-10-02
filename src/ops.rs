use memory::Memory;
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag};

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
pub fn increment_register_pair(msb: &mut Register<u8>,lsb: &mut Register<u8>, freg: &mut Register<Flags>) {
    let incremented_val = ((msb.read() as uint) << 8) + lsb.read() as uint + 1;
    msb.write(((incremented_val & 0xFF00) >> 8) as u8);
    lsb.write((incremented_val & 0x00FF) as u8);
}

pub fn increment(reg: &mut Register<u8>) {
}

/// Performs no operation and consumes a cycle
pub fn nop() {
    println!("nop");
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
    let mut flag = Register::new(Flags::empty());
    
    increment_register_pair(&mut msb, &mut lsb, &mut flag);

    assert!(msb.read() == 0x11);
    assert!(lsb.read() == 0x12);

    let mut msb_2 = Register::new(0x10);
    let mut lsb_2 = Register::new(0xFF);
    let mut flag2 = Register::new(Flags::empty());

    increment_register_pair(&mut msb_2, &mut lsb_2, &mut flag2);

    assert!(msb_2.read() == 0x11);
    assert!(lsb_2.read() == 0x00);
}
