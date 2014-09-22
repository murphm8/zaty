use std::rand::Rng;
use std::rand;
use std::iter::range_inclusive;
use std::iter::range_step_inclusive;
use std::cell::Cell;
use std::path::Path;

pub struct Memory {
    mem: [Cell<u8>, ..65536] 
}

impl Memory {
    pub fn new() -> Memory {
        return Memory{ mem: [Cell::new(0), ..65536]} 
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        return self.mem[addr as uint].get();
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        let high_byte = self.mem[(addr + 1) as uint].get();
        let low_byte = self.mem[addr as uint].get();
        return ((high_byte as u16) << 8) + (low_byte as u16);
    }

    pub fn write_byte(&self, addr: u16, data: u8) {
        self.mem[addr as uint].set(data);
    }

    pub fn write_word(&self, addr: u16, data: u16) {
        self.mem[addr as uint].set(low_byte(data));
        self.mem[(addr + 1) as uint].set(high_byte(data));
    }

    pub fn load_rom(&self, path: Path) {
    }
}

pub fn high_nibble(num: u8) -> u8 {
    return num >> 4;
}

pub fn low_nibble(num: u8) -> u8 {
    return num & 0x0F
}

pub fn low_byte(num: u16) -> u8 {
    return  (num & 0xFF) as u8;
}

pub fn high_byte(num: u16) -> u8 {
    return (num >> 8) as u8;
}

pub fn pack_u16(high: u8, low: u8) -> u16 {
    return (high as u16 << 4) + low as u16;
}

#[test]
fn test_high_nibble() {
    assert!(high_nibble(0xFA) == 0xF);
    assert!(high_nibble(0x0F) == 0x0);
}

#[test]
fn test_low_nibble() {
    assert!(low_nibble(0x0F) == 0xF);
    assert!(low_nibble(0xF0) == 0x0);
    assert!(low_nibble(0xAA) == 0xA);
}

#[test]
fn test_low_byte() {
    assert!(low_byte(0xFFEE) == 0xEE);
    assert!(low_byte(0xFF00) == 0x00);
    assert!(low_byte(0x00FF) == 0xFF);
}

#[test]
fn test_high_byte() {
    assert!(high_byte(0xAE00) == 0xAE);
    assert!(high_byte(0xF0F0) == 0xF0);
    assert!(high_byte(0x00FF) == 0x00);
}

#[test]
fn test_pack_u16() {
    assert!(pack_u16(0xA, 0xE) == 0xAE);
    assert!(pack_u16(0xF, 0x0) == 0xF0);
    assert!(pack_u16(0x0, 0xF) == 0x0F);
}

#[test]
fn test_Memory_write_byte() {
    let mut memory = Memory::new();
    let mut rng = rand::task_rng();

    for n in range_inclusive(0, 0xFFFF)
    {
        let num = rng.gen::<u8>();
        memory.write_byte(n, num);
        assert!(memory.mem[n as uint].get() == num);
    }
}

#[test]
fn test_Memory_write_word() {
    let mut memory = Memory::new();
    let mut rng = rand::task_rng();

    for n in range_step_inclusive(0, 0xFFFF, 2)
    {
        let num = rng.gen::<u16>();
        memory.write_word(n, num);
        assert!(memory.mem[n as uint].get() == low_byte(num));
        assert!(memory.mem[(n + 1) as uint].get() == high_byte(num));
    }
}

#[test]
fn test_Memory_read_byte() {
    let mut memory = Memory::new();
    
    memory.mem[0].set(0xF1);
    assert!(memory.read_byte(0) == 0xF1);

    memory.mem[0xFFFF].set(0xAB);
    assert!(memory.read_byte(0xFFFF) == 0xAB);

    memory.mem[0xABAB].set(0xBB);
    assert!(memory.read_byte(0xABAB) == 0xBB);
}

#[test]
fn test_Memory_read_word() {
    let mut memory = Memory::new();
    
    memory.mem[0].set(0xF1);
    memory.mem[1].set(0xEA);
    assert!(memory.read_word(0) == 0xEAF1);

    memory.mem[0xFFFE].set(0xBB);
    memory.mem[0xFFFF].set(0xAB);
    assert!(memory.read_word(0xFFFE) == 0xABBB);

    memory.mem[0xABAA].set(0x12);
    memory.mem[0xABAB].set(0xBA);
    assert!(memory.read_word(0xABAA) == 0xBA12);
}
