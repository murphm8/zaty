use num::integer::Integer;
use memory::Memory;
use memory::low_nibble;
use memory::high_nibble;
use extensions::Incrementor;
use ops::{mod};
use std::num::One;


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
    fn fetch_instruction(&mut self) -> u8 {
        let instr = self.mem.read_byte(self.reg.pc.val);
        self.reg.pc.increment();
        return instr;     
    }

    fn zero(&mut self, op_code: u8) {
        match low_nibble(op_code) {
            0x0 => ops::nop(),
            0x6 => ops::ld_next_byte_to_reg(self.mem, &mut self.reg.pc, &mut self.reg.b),
            _ => return
        }
    }
}


struct Registers {
    a: Register<u8>, b: Register<u8>, c: Register<u8>, 
    d: Register<u8>, e: Register<u8>, f: Register<u8>, 
    h: Register<u8>, l: Register<u8>, // 8-bit registers

    pc: Register<u16>, sp: Register<u16>, // 16-bit registers
    m: Register<u16>, t: Register<u16> // clock
}

impl Registers {
    fn new() -> Registers {
        return Registers{
            a: Register::new(0),
            b: Register::new(0),
            c: Register::new(0),
            d: Register::new(0),
            e: Register::new(0),
            f: Register::new(0),
            h: Register::new(0),
            l: Register::new(0),

            pc: Register::new(0),
            sp: Register::new(0),

            m: Register::new(0),
            t: Register::new(0)
        }
    }
}


pub struct Register<T: Copy + Unsigned> {
    val: T 
}

impl<T: Copy + Unsigned> Register<T> {
    pub fn new(i: T) -> Register<T> {
        return Register { val: i };
    }

    pub fn read(&self) -> T {
        return self.val;
    }

    pub fn write(&mut self, i: T) {
        self.val = i;
    }

    pub fn increment(&mut self) {
        let i = self.val;
        self.write(i + One::one());
    }
}
