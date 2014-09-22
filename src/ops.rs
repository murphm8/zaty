use std::cell::Cell;
use memory::Memory;

pub fn add(first: &Cell<u8>, second: &Cell<u8>) {
    let new_value = first.get() + second.get();
    first.set(new_value);
}

/// Loads the memory pointed to by the next two bytes into a register
pub fn ld_next_byte(mem: Memory, pc: &Cell<u16>, reg: &Cell<u8>) {

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
fn test_load_next_byte_into_register() {
    let mem = Memory::new();
    let pc = Cell::new(10);
    let reg = Cell::new(0);

    mem.write_byte(11, 0xFF);

    ld_next_byte(mem, &pc, &reg);
    assert!(reg.get() == 0xFF);
    assert!(pc.get() == 11);

}
