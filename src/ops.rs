use memory::Memory;
use extensions::Incrementor;
use cpu::Register;

/// Add the value of two registers and store it in the first register
pub fn add(first: &mut Register<u8>, second: &Register<u8>) {
    debug!("add: {} {}", first.read(), second.read());
    let new_value = first.read() + second.read();
    first.write(new_value);
}

/// Load the value from one register into another
pub fn ld_reg_to_reg(target: &mut Register<u8>, source: &Register<u8>) {
    debug!("ld_reg_to_reg: {}", source.read());
    let val = source.read();
    target.write(val);
}

/// Loads the memory pointed to by the next two bytes into a register
pub fn ld_next_byte_to_reg(mem: Memory, pc: &mut Register<u16>, reg: &mut Register<u8>) {
    let val = mem.read_byte(pc.read());
    debug!("ld_next_byte_to_reg: {} {}", pc.read(), val);
    pc.increment(); 
    reg.write(val);
}

/// Performs no operation and consumes a cycle
pub fn nop() {
    println!("nop");
}

#[test]
fn test_add_reg_with_reg() {
    let mut first = Register::new(5);
    let mut second = Register::new(9);

    add(&mut first, &second);
    assert!(first.read() == 14);
}

#[test]
fn test_ld_next_byte_to_reg() {
    let mem = Memory::new();
    let mut pc = Register::new(11);
    let mut reg = Register::new(0);

    mem.write_byte(11, 0xFF);

    ld_next_byte_to_reg(mem, &mut pc, &mut reg);
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
