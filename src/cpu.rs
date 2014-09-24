use std::cell::Cell;
use num::integer::Integer;
use memory::Memory;
use memory::low_nibble;
use memory::high_nibble;
use extensions::Incrementor;
use ops::{mod};


pub struct Cpu {
    reg: Registers,
    mem: Memory
}

impl Cpu {
    pub fn new(memory: Memory) -> Cpu {
        return Cpu{reg: Registers::new(), mem: memory}
    }
   
    /// Execute a cycle on the cpu
    pub fn tick(&mut self) {
        let instr = self.fetch_instruction(); 
        self.reg.pc.increment();

        match high_nibble(instr) {
            0x0 => self.zero(instr),
            _ => return
        }
    }
   
    /// Fetches the instruction pointed to by the program counter
    /// and increments the pc by 1
    fn fetch_instruction(&self) -> u8 {
        let instr = self.mem.read_byte(self.reg.pc.get());
        self.reg.pc.increment();
        return instr;     
    }

    fn zero(&self, op_code: u8) {
        match low_nibble(op_code) {
            0x0 => ops::nop(),
            0x6 => ops::ld_next_byte_to_reg(self.mem, &self.reg.pc, &self.reg.b),
            _ => return
        }
    }
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

