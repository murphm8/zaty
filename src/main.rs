use std::rand::Rng;
use std::rand;
use std::iter::range_inclusive;
use std::iter::range_step_inclusive;

fn main() {
    let mut memory = Memory::new();
    let cpu = Cpu::new(memory);

    let mut vec: [u8, ..65535] = [0 as u8, ..65535];
    vec[3] = 5;
    println!("{}", vec[3])

    while (true)
    {
        let clock_cycle = cpu.tick();
    }
}

struct Cpu {
    reg: Registers,
    mem: Memory
}

impl Cpu {
    fn new(memory: Memory) -> Cpu {
        return Cpu{reg: Registers::new(), mem: memory}
    }
    
    fn tick(&self) -> u16 {
        return 0;
    }

    fn ld(source: &u8, target: &mut u8)
    {
        *target = *source;
    }
}


/*
    Register f:

    Zero (0x80): Set if the last operation produced a result of 0;
    Operation (0x40): Set if the last operation was a subtraction;
    Half-carry (0x20): Set if, in the result of the last operation, the lower half of the byte overflowed past 15;
    Carry (0x10): Set if the last operation produced a result over 255 (for additions) or under 0 (for subtractions).

*/

type Register8 = u8;

struct Registers {
    a: u8, b: u8, c: u8, d: u8, e: u8, f: u8, h: u8, l: u8,// 8-bit registers
    pc: u16, sp: u16, // 16-bit registers
    m: u16, t: u16 // clock
}

impl Registers {
    fn new() -> Registers {
        return Registers{
            a: 0, b: 0, c: 0, d: 0, e: 0, f:0, h: 0, l: 0,
            pc: 0, sp: 0,
            m: 0, t: 0
        }
    }
}

struct Memory {
    mem: [u8, ..65536] 
}

impl Memory {
    fn new() -> Memory {
        return Memory{ mem: [0, ..65536]} 
    }

    fn read_byte(addr: u16) -> u8 {
        return 0;
    }

    fn read_word(addr: u16) -> u16 {
        return 0;
    }

    fn write_byte(&mut self, addr: u16, data: u8) {
        self.mem[addr as uint] = data;
    }

    fn write_word(&mut self, addr: u16, data: u16) {
        self.mem[addr as uint] = low_byte(data);
        self.mem[(addr + 1) as uint] = high_byte(data);
    }
}

fn low_byte(num: u16) -> u8 {
    return  (num & 0xFF) as u8;
}

fn high_byte(num: u16) -> u8 {
    return (num >> 8) as u8;
}

fn pack_u16(high: u8, low: u8) -> u16 {
    return (high as u16 << 4) + low as u16;
}

#[test]
fn low_byte_should_return_low_byte() {
    assert!(low_byte(0xFFEE) == 0xEE);
    assert!(low_byte(0xFF00) == 0x00);
    assert!(low_byte(0x00FF) == 0xFF);
}

#[test]
fn high_byte_should_return_high_byte() {
    assert!(high_byte(0xAE00) == 0xAE);
    assert!(high_byte(0xF0F0) == 0xF0);
    assert!(high_byte(0x00FF) == 0x00);
}

#[test]
fn pack_u16_should_pack() {
    assert!(pack_u16(0xA, 0xE) == 0xAE);
    assert!(pack_u16(0xF, 0x0) == 0xF0);
    assert!(pack_u16(0x0, 0xF) == 0x0F);
}

#[test]
fn write_byte_should_write_to_mem() {
    let mut memory = Memory::new();
    let mut rng = rand::task_rng();

    for n in range_inclusive(0, 0xFFFF)
    {
        let num = rng.gen::<u8>();
        memory.write_byte(n, num);
        assert!(memory.mem[n as uint] == num);
    }
}

#[test]
fn write_word_should_write_word_to_mem_little_endian() {
    let mut memory = Memory::new();
    let mut rng = rand::task_rng();

    for n in range_step_inclusive(0, 0xFFFF, 2)
    {
        let num = rng.gen::<u16>();
        memory.write_word(n, num);
        assert!(memory.mem[n as uint] == low_byte(num));
        assert!(memory.mem[(n + 1) as uint] == high_byte(num));
    }
}



