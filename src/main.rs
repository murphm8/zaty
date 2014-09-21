use std::cell::Cell;
use memory::Memory;
use memory::low_nibble;
use memory::high_nibble;

mod memory;

fn main() {
    let mut memory = Memory::new();
    let mut cpu = Cpu::new(memory);

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
    mem: memory::Memory
}

impl Cpu {
    fn new(memory: Memory) -> Cpu {
        return Cpu{reg: Registers::new(), mem: memory}
    }
    
    fn tick(&mut self) {
        let op_code = self.mem.read_byte(self.reg.pc.get());
        match high_nibble(op_code) {
            0x0 => self.zero(op_code),
            _ => return
        }
    }



    fn zero(&self, op_code: u8) {
        match low_nibble(op_code) {
            0x0 => nop(),
            0x1 => ld_immediate(self.mem, self.reg.b, self.reg.c),
            _ => return
        }
    }
}

// Loads the memory pointed to by the next two bytes into a register
fn ld_immediate(mem: Memory, high_byte: Cell<u8>, low_byte: Cell<u8>)
{
}

fn nop() {
    println!("nop");
}

struct Registers {
    a: Cell<u8>, b: Cell<u8>, c: Cell<u8>, d: Cell<u8>, e: Cell<u8>, f: Cell<u8>, h: Cell<u8>, l: Cell<u8>, // 8-bit registers
    pc: Cell<u16>, sp: Cell<u16>, // 16-bit registers
    m: Cell<u16>, t: Cell<u16> // clock
}

impl Registers {
    fn new() -> Registers {
        return Registers{
            a: Cell::new(0), b: Cell::new(0), c: Cell::new(0), d: Cell::new(0), e: Cell::new(0), f: Cell::new(0), h: Cell::new(0), l: Cell::new(0),
            pc: Cell::new(0), sp: Cell::new(0),
            m: Cell::new(0), t: Cell::new(0)
        }
    }
}

