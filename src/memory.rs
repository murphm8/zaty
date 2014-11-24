use std::rand::Rng;
use std::rand;
use std::iter::range_inclusive;
use std::iter::range_step_inclusive;
use std::path::Path;


pub trait Memory {
    fn read_byte(&self, addr: u16) -> u8;
    fn read_word(&self, addr: u16) -> u16;
    fn write_byte(&mut self, addr: u16, data: u8);
    fn write_word(&mut self, addr: u16, data: u16) ;
}

pub struct EmptyMemory {
    mem: Vec<u8>
}

impl EmptyMemory {
    pub fn new(size: uint) -> EmptyMemory {
        let mut mem = Vec::from_elem(size, 0);
        return EmptyMemory{ mem: mem } 
    }
}

impl Memory for EmptyMemory {
    fn read_byte(&self, addr: u16) -> u8 {
        return self.mem[addr as uint];
    }

    fn read_word(&self, addr: u16) -> u16 {
        let high_byte = self.mem[(addr as uint) + 1];
        let low_byte = self.mem[addr as uint];
        return ((high_byte as u16) << 8) + (low_byte as u16);
    }

    fn write_byte(&mut self, addr: u16, data: u8) {
        self.mem[addr as uint] = data;
    }

    fn write_word(&mut self, addr: u16, data: u16) {
        self.mem[addr as uint] = low_byte(data);
        self.mem[(addr as uint) + 1] = high_byte(data);
    }
}

pub struct GameboyMemory {
    rom: &'static [u8],
    mem: Vec<u8>,
    rom_bank: uint
}

impl Memory for GameboyMemory {
    
    fn read_byte(&self, addr: u16) -> u8 {
        return self.read_internal(addr as uint);
    }

    fn read_word(&self, addr: u16) -> u16 {
        let high_byte = self.read_internal((addr + 1) as uint);
        let low_byte = self.read_internal(addr as uint);
        return ((high_byte as u16) << 8) + (low_byte as u16);
    }

    fn write_byte(&mut self, addr: u16, data: u8) {
        self.write_internal(addr as uint, data);
    }

    fn write_word(&mut self, addr: u16, data: u16) {
        self.write_internal(addr as uint, low_byte(data));
        self.write_internal((addr as uint) + 1, high_byte(data));
    }
}

impl GameboyMemory {
    pub fn new(size: uint) -> GameboyMemory {
        let mut mem = Vec::from_elem(size, 0);
        return GameboyMemory{ mem: mem, rom: include_bin!("test_instrs.gb"), rom_bank: 1 } 
    }

    fn write_internal(&mut self, addr: uint, val: u8) {
        if addr >= 0x0100 && addr <= 0x014F {
            return;
        }
        if addr >= 0x0150 && addr <= 0x7FFF {
            self.rom_bank = val as uint;
            return;
        }
        self.mem[addr] = val;
    }

    fn read_internal(&self, addr: uint) -> u8 {
        if addr >= 0x0100 && addr <= 0x7FFF {
            if addr <= 0x3FFF {
                return self.rom[addr];
            } else {
                let real_addr = addr * self.rom_bank;
                return self.rom[real_addr];
            }
        }
        return self.mem[addr];
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
    return (high as u16 << 8) + low as u16;
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
    assert!(pack_u16(0xAB, 0xEF)== 0xABEF);
    assert!(pack_u16(0xF, 0x01) == 0xF01);
    assert!(pack_u16(0x0, 0xF) == 0x0F);
}

#[test]
fn test_emptymemory_write_byte() {
    let mut memory = EmptyMemory::new(65536);
    let mut rng = rand::task_rng();

    for n in range_inclusive(0, 0xFFFF)
    {
        let num = rng.gen::<u8>();
        memory.write_byte(n, num);
        assert!(memory.mem[n as uint] == num);
    }
}

#[test]
fn test_emptymemory_write_word() {
    let mut memory = EmptyMemory::new(65536);
    let mut rng = rand::task_rng();

    for n in range_step_inclusive(0, 0xFFFF, 2)
    {
        let num = rng.gen::<u16>();
        memory.write_word(n, num);
        assert!(memory.mem[n as uint] == low_byte(num));
        assert!(memory.mem[(n + 1) as uint] == high_byte(num));
    }
}

#[test]
fn test_emptymemory_read_byte() {
    let mut memory = EmptyMemory::new(65536);
    
    memory.mem[0] = 0xF1;
    assert!(memory.read_byte(0) == 0xF1);

    memory.mem[0xFFFF] = 0xAB;
    assert!(memory.read_byte(0xFFFF) == 0xAB);

    memory.mem[0xABAB] = 0xBB;
    assert!(memory.read_byte(0xABAB) == 0xBB);
}

#[test]
fn test_emptymemory_read_word() {
    let mut memory = EmptyMemory::new(65536);
    
    memory.mem[0] = 0xF1;
    memory.mem[1] = 0xEA;
    assert!(memory.read_word(0) == 0xEAF1);

    memory.mem[0xFFFE] = 0xBB;
    memory.mem[0xFFFF] = 0xAB;
    assert!(memory.read_word(0xFFFE) == 0xABBB);

    memory.mem[0xABAA] = 0x12;
    memory.mem[0xABAB] = 0xBA;
    assert!(memory.read_word(0xABAA) == 0xBA12);
}
