use std::cell::Cell;
use memory::Memory;
use extensions::Incrementor;


pub fn add(first: &Cell<u8>, second: &Cell<u8>) {
    debug!("add: {} {}", first.get(), second.get());
    let new_value = first.get() + second.get();
    first.set(new_value);
}

pub fn ld_reg_to_reg(target: &Cell<u8>, source: &Cell<u8>) {
    debug!("ld_reg_to_reg: {}", source.get());
    let val = source.get();
    target.set(val);
}

/// Loads the memory pointed to by the next two bytes into a register
pub fn ld_next_byte_to_reg(mem: Memory, pc: &Cell<u16>, reg: &Cell<u8>) {
    let val = mem.read_byte(pc.get());
    debug!("ld_next_byte_to_reg: {} {}", pc.get(), val);
    pc.increment(); 
    reg.set(val);
}

/// Performs no operation and consumes a cycle
pub fn nop() {
    println!("nop");
}

#[test]
fn test_add_reg_with_reg() {
    let first = Cell::new(5);
    let second = Cell::new(9);

    add(&first, &second);
    assert!(first.get() == 14);
}

#[test]
fn test_ld_next_byte_to_reg() {
    let mem = Memory::new();
    let pc = Cell::new(11);
    let reg = Cell::new(0);

    mem.write_byte(11, 0xFF);

    ld_next_byte_to_reg(mem, &pc, &reg);
    assert!(reg.get() == 0xFF);
    assert!(pc.get() == 12);

}

#[test]
fn test_ld_reg_to_reg() {
    let target = Cell::new(5);
    let source = Cell::new(10);

    ld_reg_to_reg(&target, &source);

    assert!(target.get() == 10);
    assert!(source.get() == 10);
}
