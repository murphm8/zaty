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

        match instr {
            0x00 => ops::nop(),
            0x01 => ops::ld_next_two_byte_into_reg_pair(&self.mem, &mut self.reg.pc, &mut self.reg.b, &mut self.reg.c),
            0x02 => ops::write_value_to_memory_at_address(&mut self.mem, self.reg.a.read(), self.reg.b.read(), self.reg.c.read()),
            0x03 => ops::increment_register_pair(&mut self.reg.b, &mut self.reg.c),
            0x04 => self.reg.b.increment(),
            0x05 => self.reg.b.decrement(),
            0x06 => ops::ld_immediate(&self.mem, &mut self.reg.pc, &mut self.reg.b),
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

            pc: Register::new(0x100),
            sp: Register::new(0xFFFE),

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

    pub fn decrement(&mut self) {
        let i = self.val;
        self.write(i - One::one());
    }
}

enum AddSubFlag {
    Add,
    Sub
}

trait FlagRegister<T: Copy + Unsigned> {
    fn zero_flag(self) -> bool;
    fn set_zero_flag(&mut self, val: bool);
    fn add_sub_flag(self) -> AddSubFlag;
    fn set_add_sub_flag(&mut self, val: AddSubFlag);
    fn half_carry_flag(self);
    fn set_half_carry_flag(&mut self, val: bool);
    fn carry_flag(self) -> bool;
    fn set_carry_flag(&mut self, val: bool);
}

impl<T: Copy + Unsigned> FlagRegister<T> for Register<T> {
    fn zero_flag(self) -> bool {
        return false;
    }
    
    fn set_zero_flag(&mut self, val: bool) {
    }

    fn add_sub_flag(self) -> AddSubFlag {
        return Add;
    }

    fn set_add_sub_flag(&mut self, val: AddSubFlag) {
    }

    fn half_carry_flag(self) {
    }

    fn set_half_carry_flag(&mut self, val: bool) {
    }

    fn carry_flag(self) -> bool {
        return false;
    }

    fn set_carry_flag(&mut self, val: bool) {
    }
}

#[test]
fn test_Register_new() {
    let reg: Register<u8> = Register::new(10);
    assert!(reg.val == 10);
}

#[test]
fn test_Register_read() {
    let reg: Register<u32> = Register::new(5);
    assert!(reg.read() == 5);
}

#[test]
fn test_Register_write() {
    let mut reg: Register<u16> = Register::new(483);
    
    reg.write(5);
    assert!(reg.val == 5);
}

#[test]
fn test_Register_increment() {
    let mut reg: Register<u16> = Register::new(483);
    
    reg.increment();
    assert!(reg.val == 484);

}

#[test]
fn test_Register_decrement() {
    let mut reg: Register<u16> = Register::new(401);
    
    reg.decrement();
    assert!(reg.val == 400);

}

#[test]
fn test_Registers_new() {
    let a = Registers::new();
    assert!(a.pc.read() == 0x100);
    assert!(a.sp.read() == 0xFFFE);
}
