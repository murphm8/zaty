use num::integer::Integer;
use memory::Memory;
use memory::low_nibble;
use memory::high_nibble;
use extensions::Incrementor;
use ops::{mod};
use std::num::One;


pub struct Cpu<'a> {
    reg: Registers,
    mem: &'a mut Memory
}

impl<'a> Cpu<'a> {
    pub fn new(memory: &'a mut Memory) -> Cpu<'a> {
        return Cpu{reg: Registers::new(), mem: memory}
    }
   
    /// Execute a cycle on the cpu
    pub fn tick(&mut self) {
        let instr = self.fetch_instruction(); 

        match instr {
            0x00 => ops::nop(), // NOP
            0x01 => ops::ld_next_two_byte_into_reg_pair(self.mem, &mut self.reg.pc, &mut self.reg.b, &mut self.reg.c), // LD BC, nn
            0x02 => ops::write_value_to_memory_at_address(self.mem, self.reg.a.read(), self.reg.b.read(), self.reg.c.read()), // LD (BC), A
            0x03 => ops::increment_register_pair(&mut self.reg.b, &mut self.reg.c), // INC BC
            0x04 => ops::increment_register(&mut self.reg.b, &mut self.reg.f), // INC B
            0x05 => ops::decrement_register(&mut self.reg.b, &mut self.reg.f), // DEC B
            0x06 => ops::ld_immediate(self.mem, &mut self.reg.pc, &mut self.reg.b), // LD B, n
            0x07 => ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), // RLC A
            0x08 => ops::write_sp_to_address_immediate(self.mem, &mut self.reg.pc, &self.reg.sp), // LD (nn), SP
            0x09 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, &self.reg.b, &self.reg.c, &mut self.reg.f), // ADD HL, BC
            0x0A => ops::ld_a_from_reg_pair_as_address(self.mem, &mut self.reg.a, &mut self.reg.b, &mut self.reg.c), // LD A, (BC)
            0x0B => ops::decrement_register_pair(&mut self.reg.b, &mut self.reg.c), // DEC BC
            0x0C => ops::increment_register(&mut self.reg.c, &mut self.reg.f), // INC C
            0x0D => ops::decrement_register(&mut self.reg.c, &mut self.reg.f), // DEC C
            0x0E => ops::ld_immediate(self.mem, &mut self.reg.pc, &mut self.reg.c), // LD C, n
            0x0F => ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), // RRC A
            0x10 => return, // STOP
            0x11 => ops::ld_next_two_byte_into_reg_pair(self.mem, &mut self.reg.pc, &mut self.reg.d, &mut self.reg.e), // LD DE, nn
            0x12 => ops::write_value_to_memory_at_address(self.mem, self.reg.a.read(), self.reg.d.read(), self.reg.e.read()), // LD (DE), A
            0x13 => ops::increment_register_pair(&mut self.reg.d, &mut self.reg.e), // INC DE
            0x14 => ops::increment_register(&mut self.reg.d, &mut self.reg.f), // INC D
            0x15 => ops::decrement_register(&mut self.reg.d, &mut self.reg.f), // DEC D
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
    d: Register<u8>, e: Register<u8>, f: Register<Flags>, 
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
            f: Register::new(Flags::empty()),
            h: Register::new(0),
            l: Register::new(0),

            pc: Register::new(0x100),
            sp: Register::new(0xFFFE),

            m: Register::new(0),
            t: Register::new(0)
        }
    }
}

pub struct Register<T: Copy> {
    val: T 
}

impl<T: Copy> Register<T> {
    pub fn new(i: T) -> Register<T> {
        return Register { val: i };
    }

    pub fn read(&self) -> T {
        return self.val;
    }

    pub fn write(&mut self, i: T) {
        self.val = i;
    }
}


impl<T: Copy + Unsigned>  Register<T> {
    pub fn increment(&mut self) {
        let i = self.val;
        self.write(i + One::one());
    }

    pub fn decrement(&mut self) {
        let i = self.val;
        self.write(i - One::one());
    }
}

bitflags! {
    flags Flags: u8 {
        const ZeroFlag       = 0b10000000,
        const SubtractFlag   = 0b01000000,
        const HalfCarryFlag  = 0b00100000,
        const CarryFlag      = 0b00010000,
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
