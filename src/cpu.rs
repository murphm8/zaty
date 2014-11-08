use memory::{Memory, low_nibble, high_nibble, low_byte, high_byte};
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

        let a = self.reg.a.read();
        let b = self.reg.b.read();
        let c = self.reg.c.read();
        let d = self.reg.d.read();
        let e = self.reg.e.read();
        let h = self.reg.h.read();
        let l = self.reg.l.read();
        let sp = self.reg.sp.read();

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
            0x09 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, b, c, &mut self.reg.f), // ADD HL, BC
            0x0A => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.a, b, c), // LD A, (BC)
            0x0B => ops::decrement_register_pair(&mut self.reg.b, &mut self.reg.c), // DEC BC
            0x0C => ops::increment_register(&mut self.reg.c, &mut self.reg.f), // INC C
            0x0D => ops::decrement_register(&mut self.reg.c, &mut self.reg.f), // DEC C
            0x0E => ops::ld_immediate(self.mem, &mut self.reg.pc, &mut self.reg.c), // LD C, n
            0x0F => ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), // RRC A
            0x10 => error!("STOP Op Code not implemented and is being used"), // STOP
            0x11 => ops::ld_next_two_byte_into_reg_pair(self.mem, &mut self.reg.pc, &mut self.reg.d, &mut self.reg.e), // LD DE, nn
            0x12 => ops::write_value_to_memory_at_address(self.mem, a, d, e), // LD (DE), A
            0x13 => ops::increment_register_pair(&mut self.reg.d, &mut self.reg.e), // INC DE
            0x14 => ops::increment_register(&mut self.reg.d, &mut self.reg.f), // INC D
            0x15 => ops::decrement_register(&mut self.reg.d, &mut self.reg.f), // DEC D
            0x16 => ops::ld_immediate(self.mem, &mut self.reg.pc, &mut self.reg.d), // LD D, n
            0x17 => ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), // RL A
            0x18 => ops::jump_by_signed_immediate(self.mem, &mut self.reg.pc), // JR n
            0x19 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, d, e, &mut self.reg.f), // Add HL, DE
            0x1A => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.a, d, e), // LD A, (DE)
            0x1B => ops::decrement_register_pair(&mut self.reg.d, &mut self.reg.e), // DEC DE
            0x1C => ops::increment_register(&mut self.reg.e, &mut self.reg.f), // INC E
            0x1E => ops::decrement_register(&mut self.reg.e, &mut self.reg.f), // DEC E
            0x1F => ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), // RR A
            0x20 => ops::relative_jmp_by_signed_immediate_if_not_flag(self.mem, &mut self.reg.pc, &self.reg.f, ZeroFlag), // JR NZ, n
            0x21 => ops::ld_next_two_byte_into_reg_pair(self.mem, &mut self.reg.pc, &mut self.reg.h, &mut self.reg.l), // LD HL, nn
            0x22 => ops::write_value_to_memory_at_address_and_increment_register(self.mem, self.reg.a.read(), &mut self.reg.h, &mut self.reg.l), // LDI (HL), A
            0x23 => ops::increment_register_pair(&mut self.reg.h, &mut self.reg.l), // INC HL
            0x24 => ops::increment_register(&mut self.reg.h, &mut self.reg.f), // INC H
            0x25 => ops::decrement_register(&mut self.reg.h, &mut self.reg.f), // DEC H
            0x26 => ops::ld_immediate(self.mem, &mut self.reg.pc, &mut self.reg.h), // LD H, n
            0x27 => error!("DAA instruction not implemented and is being used"),
            0x28 => ops::relative_jmp_by_signed_immediate_if_flag(self.mem, &mut self.reg.pc, &self.reg.f, ZeroFlag), // JR Z, n
            0x29 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, h, l, &mut self.reg.f), // ADD HL, HL 
            0x2A => ops::ld_from_address_pointed_to_by_register_pair_and_increment_register_pair(self.mem, &mut self.reg.a, &mut self.reg.h, &mut self.reg.l), // LDI A, HL
            0x2B => ops::decrement_register_pair(&mut self.reg.h, &mut self.reg.l), // DEC HL 
            0x2C => ops::increment_register(&mut self.reg.l, &mut self.reg.f), // INC L
            0x2D => ops::decrement_register(&mut self.reg.l, &mut self.reg.f), // DEC L
            0x2E => ops::ld_immediate(self.mem, &mut self.reg.pc, &mut self.reg.l), // LD L, d8
            0x2F => ops::complement(&mut self.reg.a, &mut self.reg.f), // CPL 
            0x30 => ops::relative_jmp_by_signed_immediate_if_not_flag(self.mem, &mut self.reg.pc, &self.reg.f, CarryFlag), // JR NC, n
            0x31 => ops::ld_next_two_bytes_into_reg(self.mem, &mut self.reg.pc, &mut self.reg.sp), // LD SP, nn
            0x32 => ops::write_value_to_memory_at_address_and_decrement_register(self.mem, self.reg.a.read(), &mut self.reg.h, &mut self.reg.l), // LDI (HL), A
            0x33 => self.reg.sp.increment(), // INC SP 
            0x34 => ops::increment_value_at_address(self.mem, self.reg.h.read(), self.reg.l.read(), &mut self.reg.f),
            0x35 => ops::decrement_value_at_address(self.mem, self.reg.h.read(), self.reg.l.read(), &mut self.reg.f),
            0x36 => ops::ld_immediate_into_address(self.mem, &mut self.reg.pc, self.reg.h.read(), self.reg.l.read()),
            0x37 => ops::set_flag(&mut self.reg.f, CarryFlag),
            0x38 => ops::relative_jmp_by_signed_immediate_if_flag(self.mem, &mut self.reg.pc, &mut self.reg.f, CarryFlag),
            0x39 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, high_byte(sp), low_byte(sp), &mut self.reg.f), // ADD HL, SP 
            0x3A => ops::ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair(self.mem, &mut self.reg.a, &mut self.reg.h, &mut self.reg.l), // LDD A, HL
            0x3B => self.reg.sp.decrement(), // INC SP
            0x3C => ops::increment_register(&mut self.reg.a, &mut self.reg.f), // INC A
            0x3D => ops::decrement_register(&mut self.reg.a, &mut self.reg.f), // DEC A
            0x3E => ops::ld_immediate(self.mem, &mut self.reg.pc, &mut self.reg.a), // LD A, d8
            0x3F => ops::reset_flag(&mut self.reg.f, CarryFlag), // CCF
            0x40 => ops::copy_value_into_register(&mut self.reg.b, b), // LD B, B
            0x41 => ops::copy_value_into_register(&mut self.reg.b, c), // LD B, C
            0x42 => ops::copy_value_into_register(&mut self.reg.b, d), // LD B, D
            0x43 => ops::copy_value_into_register(&mut self.reg.b, e), // LD B, E
            0x44 => ops::copy_value_into_register(&mut self.reg.b, h), // LD B, H
            0x45 => ops::copy_value_into_register(&mut self.reg.b, l), // LD B, L
            0x46 => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD B, (HL)
            0x47 => ops::copy_value_into_register(&mut self.reg.b, a), // LD B, A
            0x48 => ops::copy_value_into_register(&mut self.reg.c, b), // LD C, B
            0x49 => ops::copy_value_into_register(&mut self.reg.c, c), // LD C, C
            0x4A => ops::copy_value_into_register(&mut self.reg.c, d), // LD C, D
            0x4B => ops::copy_value_into_register(&mut self.reg.c, e), // LD C, E
            0x4C => ops::copy_value_into_register(&mut self.reg.c, h), // LD C, H
            0x4D => ops::copy_value_into_register(&mut self.reg.c, l), // LD C, L
            0x4E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD C, (HL) 
            0x4F => ops::copy_value_into_register(&mut self.reg.c, a), // LD C, A 
            0x50 => ops::copy_value_into_register(&mut self.reg.d, b), // LD D, B
            0x51 => ops::copy_value_into_register(&mut self.reg.d, c), // LD D, C
            0x52 => ops::copy_value_into_register(&mut self.reg.d, d), // LD D, D
            0x53 => ops::copy_value_into_register(&mut self.reg.d, e), // LD D, E
            0x54 => ops::copy_value_into_register(&mut self.reg.d, h), // LD D, H
            0x55 => ops::copy_value_into_register(&mut self.reg.d, l), // LD D, L
            0x56 => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.d, h, l), // LD D, (HL)
            0x57 => ops::copy_value_into_register(&mut self.reg.d, a), // LD D, A
            0x58 => ops::copy_value_into_register(&mut self.reg.e, b), // LD E, B
            0x59 => ops::copy_value_into_register(&mut self.reg.e, c), // LD E, C
            0x5A => ops::copy_value_into_register(&mut self.reg.e, d), // LD E, D
            0x5B => ops::copy_value_into_register(&mut self.reg.e, e), // LD E, E
            0x5C => ops::copy_value_into_register(&mut self.reg.e, h), // LD E, H
            0x5D => ops::copy_value_into_register(&mut self.reg.e, l), // LD E, L
            0x5E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD E, (HL) 
            0x5F => ops::copy_value_into_register(&mut self.reg.e, a), // LD E, A          
            0x60 => ops::copy_value_into_register(&mut self.reg.h, b), // LD H, B
            0x61 => ops::copy_value_into_register(&mut self.reg.h, c), // LD H, C
            0x62 => ops::copy_value_into_register(&mut self.reg.h, d), // LD H, D
            0x63 => ops::copy_value_into_register(&mut self.reg.h, e), // LD H, E
            0x64 => ops::copy_value_into_register(&mut self.reg.h, h), // LD H, H
            0x65 => ops::copy_value_into_register(&mut self.reg.h, l), // LD H, L
            0x66 => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.h, h, l), // LD H, (HL)
            0x67 => ops::copy_value_into_register(&mut self.reg.h, a), // LD H, A
            0x68 => ops::copy_value_into_register(&mut self.reg.l, b), // LD L, B
            0x69 => ops::copy_value_into_register(&mut self.reg.l, c), // LD L, C
            0x6A => ops::copy_value_into_register(&mut self.reg.l, d), // LD L, D
            0x6B => ops::copy_value_into_register(&mut self.reg.l, e), // LD L, E
            0x6C => ops::copy_value_into_register(&mut self.reg.l, h), // LD L, H
            0x6D => ops::copy_value_into_register(&mut self.reg.l, l), // LD L, L
            0x6E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD L, (HL) 
            0x6F => ops::copy_value_into_register(&mut self.reg.l, a), // LD L, A          
            0x70 => ops::write_value_to_memory_at_address(self.mem, b, h, l), // LD (HL), B
            0x71 => ops::write_value_to_memory_at_address(self.mem, c, h, l), // LD (HL), C
            0x72 => ops::write_value_to_memory_at_address(self.mem, d, h, l), // LD (HL), D
            0x73 => ops::write_value_to_memory_at_address(self.mem, e, h, l), // LD (HL), E
            0x74 => ops::write_value_to_memory_at_address(self.mem, h, h, l), // LD (HL), H
            0x75 => ops::write_value_to_memory_at_address(self.mem, l, h, l), // LD (HL), L
            0x76 => error!("HALT instruction not implemented"),
            0x77 => ops::write_value_to_memory_at_address(self.mem, a, h, l), // LD (HL), A
            0x78 => ops::copy_value_into_register(&mut self.reg.a, b), // LD A, B
            0x79 => ops::copy_value_into_register(&mut self.reg.a, c), // LD A, C
            0x7A => ops::copy_value_into_register(&mut self.reg.a, d), // LD A, D
            0x7B => ops::copy_value_into_register(&mut self.reg.a, e), // LD A, E
            0x7C => ops::copy_value_into_register(&mut self.reg.a, h), // LD A, H
            0x7D => ops::copy_value_into_register(&mut self.reg.a, l), // LD A, L
            0x7E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD A, (HL) 
            0x7F => ops::copy_value_into_register(&mut self.reg.a, a), // LD A, A          
            _ => return
        }
    }
   
    /// Fetches the instruction pointed to by the program counter
    /// and increments the pc by 1
    fn fetch_instruction(&mut self) -> u8 {
        let instr = self.mem.read_byte(self.reg.pc.read());
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
