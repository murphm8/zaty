use memory::{Memory, low_nibble, high_nibble, low_byte, high_byte, pack_u16};
use extensions::Incrementor;
use ops::{mod};
use std::num::{Unsigned, One};


macro_rules! opcode(
    ($func:expr, $opcode_string:tt, $cpu:ident) => ( // invoke it like `(ops::func(), "OPCode")`
        {
            {
                debug!("a: 0x{:X} b: 0x{:X} c: 0x{:X} d: 0x{:X} e: 0x{:X} f: 0x{:b} h: 0x{:X} l: 0x{:X}", $cpu.reg.a.read(), $cpu.reg.b.read(), $cpu.reg.c.read(), $cpu.reg.d.read(), $cpu.reg.e.read(), $cpu.reg.f.read().bits(), $cpu.reg.h.read(), $cpu.reg.l.read());
                debug!("sp: 0x{:X} pc: 0x{:X}", $cpu.reg.sp.read(), $cpu.reg.pc.read() - 1);
                debug!("n: 0x{:X} nn: 0x{:X}", $cpu.mem.read_byte($cpu.reg.pc.read()), $cpu.mem.read_word($cpu.reg.pc.read()));
                debug!($opcode_string);
            }
            $func;
        }
    );
)


pub struct Cpu<'a> {
    reg: Registers,
    mem: Box<Memory + 'a>
}

impl<'a> Cpu<'a> {
    pub fn new(memory: Box<Memory + 'a>) -> Cpu<'a> {
        return Cpu{reg: Registers::new(), mem: memory}
    }

    fn check_serial(&mut self) {
        let flag_reg = self.mem.read_byte(0xFF02);
        let serial_ready = flag_reg & 0x80 == 0x80;
        if serial_ready {
            let val = self.mem.read_byte(0xFF01);
            print!("{}", val as char);
            debug!("serial: {}", val as char);
            self.mem.write_byte(0xFF02, 0x00);
        }
    }
   
    /// Execute a cycle on the cpu
    pub fn tick(&mut self) {
        self.check_serial();
        let instr = self.fetch_instruction(); 

        let a = self.reg.a.read();
        let b = self.reg.b.read();
        let c = self.reg.c.read();
        let d = self.reg.d.read();
        let e = self.reg.e.read();
        let h = self.reg.h.read();
        let l = self.reg.l.read();
        let f = self.reg.f.read().bits();
        let sp = self.reg.sp.read();

        let zero = self.reg.f.read().contains(ZeroFlag);
        let not_zero = !zero;
        let carry = self.reg.f.read().contains(CarryFlag);
        let no_carry = !carry;
        match instr {
            0x00 => opcode!(ops::nop(), "NOP", self),
            0x01 => opcode!(ops::ld_u16_immediate(&mut *self.mem, &mut self.reg.pc, &mut self.reg.b, &mut self.reg.c), "LD BC, nn", self),
            0x02 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, self.reg.a.read(), self.reg.b.read(), self.reg.c.read()), "LD (BC), A", self),
            0x03 => opcode!(ops::increment_register_pair(&mut self.reg.b, &mut self.reg.c), "INC BC", self),
            0x04 => opcode!(ops::increment_register(&mut self.reg.b, &mut self.reg.f), "INC B", self),
            0x05 => opcode!(ops::decrement_register(&mut self.reg.b, &mut self.reg.f), "DEC B", self),
            0x06 => opcode!(ops::ld_u8(&mut self.reg.b, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc)), "LD B, n", self),
            0x07 => opcode!(ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), "RLC A", self),
            0x08 => opcode!(ops::write_u16_immediate_address(&mut *self.mem, &mut self.reg.pc, sp), "LD (nn), SP", self),
            0x09 => opcode!(ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, b, c, &mut self.reg.f), "ADD HL, BC", self),
            0x0A => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.a, pack_u16(b, c)), "LD A, (BC)", self),
            0x0B => opcode!(ops::decrement_register_pair(&mut self.reg.b, &mut self.reg.c), "DEC BC", self),
            0x0C => opcode!(ops::increment_register(&mut self.reg.c, &mut self.reg.f), "INC C", self),
            0x0D => opcode!(ops::decrement_register(&mut self.reg.c, &mut self.reg.f), "DEC C", self),
            0x0E => opcode!(ops::ld_u8(&mut self.reg.c, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc)), "LD C, n", self),
            0x0F => opcode!(ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), "RRC A", self),

            0x10 => opcode!(error!("STOP Op Code not implemented and is being used"), "STOP", self),
            0x11 => opcode!(ops::ld_u16_immediate(&mut *self.mem, &mut self.reg.pc, &mut self.reg.d, &mut self.reg.e), "LD DE, nn", self),
            0x12 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, a, d, e), "LD (DE), A", self),
            0x13 => opcode!(ops::increment_register_pair(&mut self.reg.d, &mut self.reg.e), "INC DE", self),
            0x14 => opcode!(ops::increment_register(&mut self.reg.d, &mut self.reg.f), "INC D", self),
            0x15 => opcode!(ops::decrement_register(&mut self.reg.d, &mut self.reg.f), "DEC D", self),
            0x16 => opcode!(ops::ld_u8(&mut self.reg.d, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc)), "LD D, n", self),
            0x17 => opcode!(ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), "RL A", self),
            0x18 => opcode!(ops::jump_by_signed_immediate(&mut *self.mem, &mut self.reg.pc), "JR n", self),
            0x19 => opcode!(ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, d, e, &mut self.reg.f), "Add HL, DE", self),
            0x1A => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.a, pack_u16(d, e)), "LD A, (DE)", self),
            0x1B => opcode!(ops::decrement_register_pair(&mut self.reg.d, &mut self.reg.e), "DEC DE", self),
            0x1C => opcode!(ops::increment_register(&mut self.reg.e, &mut self.reg.f), "INC E", self),
            0x1D => opcode!(ops::decrement_register(&mut self.reg.e, &mut self.reg.f), "DEC E", self),
            0x1E => opcode!(ops::ld_u8(&mut self.reg.e, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc)), "LD E, n", self),
            0x1F => opcode!(ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), "RR A", self),

            0x20 => opcode!(ops::relative_jmp_by_signed_immediate_if_not_flag(&mut *self.mem, &mut self.reg.pc, &self.reg.f, ZeroFlag), "JR NZ, n", self),
            0x21 => opcode!(ops::ld_u16_immediate(&mut *self.mem, &mut self.reg.pc, &mut self.reg.h, &mut self.reg.l), "LD HL, nn", self),
            0x22 => opcode!(ops::write_value_to_memory_at_address_and_increment_register(&mut *self.mem, self.reg.a.read(), &mut self.reg.h, &mut self.reg.l), "LDI (HL), A", self),
            0x23 => opcode!(ops::increment_register_pair(&mut self.reg.h, &mut self.reg.l), "INC HL", self),
            0x24 => opcode!(ops::increment_register(&mut self.reg.h, &mut self.reg.f), "INC H", self),
            0x25 => opcode!(ops::decrement_register(&mut self.reg.h, &mut self.reg.f), "DEC H", self),
            0x26 => opcode!(ops::ld_u8(&mut self.reg.h, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc)), "LD H, n", self),
            0x27 => error!("DAA instruction not implemented and is being used"),
            0x28 => opcode!(ops::relative_jmp_by_signed_immediate_if_flag(&mut *self.mem, &mut self.reg.pc, &self.reg.f, ZeroFlag), "JR Z, n", self),
            0x29 => opcode!(ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, h, l, &mut self.reg.f), "ADD HL, HL ", self),
            0x2A => opcode!(ops::ld_from_address_pointed_to_by_register_pair_and_increment_register_pair(&mut *self.mem, &mut self.reg.a, &mut self.reg.h, &mut self.reg.l), "LDI A, HL", self),
            0x2B => opcode!(ops::decrement_register_pair(&mut self.reg.h, &mut self.reg.l), "DEC HL ", self),
            0x2C => opcode!(ops::increment_register(&mut self.reg.l, &mut self.reg.f), "INC L", self),
            0x2D => opcode!(ops::decrement_register(&mut self.reg.l, &mut self.reg.f), "DEC L", self),
            0x2E => opcode!(ops::ld_u8(&mut self.reg.l, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc)), "LD L, d8", self),
            0x2F => opcode!(ops::complement(&mut self.reg.a, &mut self.reg.f), "CPL ", self),

            0x30 => opcode!(ops::relative_jmp_by_signed_immediate_if_not_flag(&mut *self.mem, &mut self.reg.pc, &self.reg.f, CarryFlag), "JR NC, n", self),
            0x31 => opcode!(ops::ld_next_two_bytes_into_reg(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp), "LD SP, nn", self),
            0x32 => opcode!(ops::write_value_to_memory_at_address_and_decrement_register(&mut *self.mem, self.reg.a.read(), &mut self.reg.h, &mut self.reg.l), "LDI (HL), A", self),
            0x33 => opcode!(self.reg.sp.increment(), "INC SP ", self),
            0x34 => opcode!(ops::increment_value_at_address(&mut *self.mem, self.reg.h.read(), self.reg.l.read(), &mut self.reg.f), "INC (HL)", self),
            0x35 => opcode!(ops::decrement_value_at_address(&mut *self.mem, self.reg.h.read(), self.reg.l.read(), &mut self.reg.f), "DEC (HL)", self),
            0x36 => opcode!(ops::ld_u8_immediate_into_address(&mut *self.mem, &mut self.reg.pc, self.reg.h.read(), self.reg.l.read()), "LD (HL), n", self),
            0x37 => opcode!(ops::set_flag(&mut self.reg.f, CarryFlag), "SCF", self),
            0x38 => opcode!(ops::relative_jmp_by_signed_immediate_if_flag(&mut *self.mem, &mut self.reg.pc, &mut self.reg.f, CarryFlag), "JC C, n", self),
            0x39 => opcode!(ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, high_byte(sp), low_byte(sp), &mut self.reg.f), "ADD HL, SP ", self),
            0x3A => opcode!(ops::ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair(&mut *self.mem, &mut self.reg.a, &mut self.reg.h, &mut self.reg.l), "LDD A, HL", self),
            0x3B => opcode!(self.reg.sp.decrement(), "INC SP", self),
            0x3C => opcode!(ops::increment_register(&mut self.reg.a, &mut self.reg.f), "INC A", self),
            0x3D => opcode!(ops::decrement_register(&mut self.reg.a, &mut self.reg.f), "DEC A", self),
            0x3E => opcode!(ops::ld_u8(&mut self.reg.a, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc)), "LD A, d8", self),
            0x3F => opcode!(ops::reset_flag(&mut self.reg.f, CarryFlag), "CCF", self),
            0x40 => opcode!(ops::ld_u8(&mut self.reg.b, b), "LD B, B", self),
            0x41 => opcode!(ops::ld_u8(&mut self.reg.b, c), "LD B, C", self),
            0x42 => opcode!(ops::ld_u8(&mut self.reg.b, d), "LD B, D", self),
            0x43 => opcode!(ops::ld_u8(&mut self.reg.b, e), "LD B, E", self),
            0x44 => opcode!(ops::ld_u8(&mut self.reg.b, h), "LD B, H", self),
            0x45 => opcode!(ops::ld_u8(&mut self.reg.b, l), "LD B, L", self),
            0x46 => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.b, pack_u16(h, l)), "LD B, (HL)", self),
            0x47 => opcode!(ops::ld_u8(&mut self.reg.b, a), "LD B, A", self),
            0x48 => opcode!(ops::ld_u8(&mut self.reg.c, b), "LD C, B", self),
            0x49 => opcode!(ops::ld_u8(&mut self.reg.c, c), "LD C, C", self),
            0x4A => opcode!(ops::ld_u8(&mut self.reg.c, d), "LD C, D", self),
            0x4B => opcode!(ops::ld_u8(&mut self.reg.c, e), "LD C, E", self),
            0x4C => opcode!(ops::ld_u8(&mut self.reg.c, h), "LD C, H", self),
            0x4D => opcode!(ops::ld_u8(&mut self.reg.c, l), "LD C, L", self),
            0x4E => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.b, pack_u16(h, l)), "LD C, (HL) ", self),
            0x4F => opcode!(ops::ld_u8(&mut self.reg.c, a), "LD C, A ", self),
            0x50 => opcode!(ops::ld_u8(&mut self.reg.d, b), "LD D, B", self),
            0x51 => opcode!(ops::ld_u8(&mut self.reg.d, c), "LD D, C", self),
            0x52 => opcode!(ops::ld_u8(&mut self.reg.d, d), "LD D, D", self),
            0x53 => opcode!(ops::ld_u8(&mut self.reg.d, e), "LD D, E", self),
            0x54 => opcode!(ops::ld_u8(&mut self.reg.d, h), "LD D, H", self),
            0x55 => opcode!(ops::ld_u8(&mut self.reg.d, l), "LD D, L", self),
            0x56 => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.d, pack_u16(h, l)), "LD D, (HL)", self),
            0x57 => opcode!(ops::ld_u8(&mut self.reg.d, a), "LD D, A", self),
            0x58 => opcode!(ops::ld_u8(&mut self.reg.e, b), "LD E, B", self),
            0x59 => opcode!(ops::ld_u8(&mut self.reg.e, c), "LD E, C", self),
            0x5A => opcode!(ops::ld_u8(&mut self.reg.e, d), "LD E, D", self),
            0x5B => opcode!(ops::ld_u8(&mut self.reg.e, e), "LD E, E", self),
            0x5C => opcode!(ops::ld_u8(&mut self.reg.e, h), "LD E, H", self),
            0x5D => opcode!(ops::ld_u8(&mut self.reg.e, l), "LD E, L", self),
            0x5E => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.b, pack_u16(h, l)), "LD E, (HL) ", self),
            0x5F => opcode!(ops::ld_u8(&mut self.reg.e, a), "LD E, A          ", self),
            0x60 => opcode!(ops::ld_u8(&mut self.reg.h, b), "LD H, B", self),
            0x61 => opcode!(ops::ld_u8(&mut self.reg.h, c), "LD H, C", self),
            0x62 => opcode!(ops::ld_u8(&mut self.reg.h, d), "LD H, D", self),
            0x63 => opcode!(ops::ld_u8(&mut self.reg.h, e), "LD H, E", self),
            0x64 => opcode!(ops::ld_u8(&mut self.reg.h, h), "LD H, H", self),
            0x65 => opcode!(ops::ld_u8(&mut self.reg.h, l), "LD H, L", self),
            0x66 => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.h, pack_u16(h, l)), "LD H, (HL)", self),
            0x67 => opcode!(ops::ld_u8(&mut self.reg.h, a), "LD H, A", self),
            0x68 => opcode!(ops::ld_u8(&mut self.reg.l, b), "LD L, B", self),
            0x69 => opcode!(ops::ld_u8(&mut self.reg.l, c), "LD L, C", self),
            0x6A => opcode!(ops::ld_u8(&mut self.reg.l, d), "LD L, D", self),
            0x6B => opcode!(ops::ld_u8(&mut self.reg.l, e), "LD L, E", self),
            0x6C => opcode!(ops::ld_u8(&mut self.reg.l, h), "LD L, H", self),
            0x6D => opcode!(ops::ld_u8(&mut self.reg.l, l), "LD L, L", self),
            0x6E => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.b, pack_u16(h, l)), "LD L, (HL) ", self),
            0x6F => opcode!(ops::ld_u8(&mut self.reg.l, a), "LD L, A          ", self),
            0x70 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, b, h, l), "LD (HL), B", self),
            0x71 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, c, h, l), "LD (HL), C", self),
            0x72 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, d, h, l), "LD (HL), D", self),
            0x73 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, e, h, l), "LD (HL), E", self),
            0x74 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, h, h, l), "LD (HL), H", self),
            0x75 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, l, h, l), "LD (HL), L", self),
            0x76 => panic!(),
            0x77 => opcode!(ops::write_value_to_memory_at_address(&mut *self.mem, a, h, l), "LD (HL), A", self),
            0x78 => opcode!(ops::ld_u8(&mut self.reg.a, b), "LD A, B", self),
            0x79 => opcode!(ops::ld_u8(&mut self.reg.a, c), "LD A, C", self),
            0x7A => opcode!(ops::ld_u8(&mut self.reg.a, d), "LD A, D", self),
            0x7B => opcode!(ops::ld_u8(&mut self.reg.a, e), "LD A, E", self),
            0x7C => opcode!(ops::ld_u8(&mut self.reg.a, h), "LD A, H", self),
            0x7D => opcode!(ops::ld_u8(&mut self.reg.a, l), "LD A, L", self),
            0x7E => opcode!(ops::ld_from_address(&mut *self.mem, &mut self.reg.b, pack_u16(h, l)), "LD A, (HL) ", self),
            0x7F => opcode!(ops::ld_u8(&mut self.reg.a, a), "LD A, A          ", self),
            0x80 => opcode!(ops::add(&mut self.reg.a, b, &mut self.reg.f), "ADD A, B", self),
            0x81 => opcode!(ops::add(&mut self.reg.a, c, &mut self.reg.f), "ADD A, C", self),
            0x82 => opcode!(ops::add(&mut self.reg.a, d, &mut self.reg.f), "ADD A, D", self),
            0x83 => opcode!(ops::add(&mut self.reg.a, e, &mut self.reg.f), "ADD A, E", self),
            0x84 => opcode!(ops::add(&mut self.reg.a, h, &mut self.reg.f), "ADD A, H", self),
            0x85 => opcode!(ops::add(&mut self.reg.a, l, &mut self.reg.f), "ADD A, L", self),
            0x86 => opcode!(ops::add_value_at_address(&mut *self.mem, &mut self.reg.a, h, l, &mut self.reg.f), "ADD A, (HL)", self),
            0x87 => opcode!(ops::add(&mut self.reg.a, a, &mut self.reg.f), "ADD A, A", self),
            0x88 => opcode!(ops::adc(&mut self.reg.a, b, &mut self.reg.f), "ADC A, B", self),
            0x89 => opcode!(ops::adc(&mut self.reg.a, c, &mut self.reg.f), "ADC A, C", self),
            0x8A => opcode!(ops::adc(&mut self.reg.a, d, &mut self.reg.f), "ADC A, D", self),
            0x8B => opcode!(ops::adc(&mut self.reg.a, e, &mut self.reg.f), "ADC A, E", self),
            0x8C => opcode!(ops::adc(&mut self.reg.a, h, &mut self.reg.f), "ADC A, H", self),
            0x8D => opcode!(ops::adc(&mut self.reg.a, l, &mut self.reg.f), "ADC A, L", self),
            0x8E => opcode!(ops::adc_value_at_address(&mut *self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), "ADC A, (HL)", self),
            0x8F => opcode!(ops::adc(&mut self.reg.a, a, &mut self.reg.f), "ADC A, A", self),
            0x90 => opcode!(ops::sub(&mut self.reg.a, b, &mut self.reg.f), "SUB A, B", self),
            0x91 => opcode!(ops::sub(&mut self.reg.a, c, &mut self.reg.f), "SUB A, C", self),
            0x92 => opcode!(ops::sub(&mut self.reg.a, d, &mut self.reg.f), "SUB A, D", self),
            0x93 => opcode!(ops::sub(&mut self.reg.a, e, &mut self.reg.f), "SUB A, E", self),
            0x94 => opcode!(ops::sub(&mut self.reg.a, h, &mut self.reg.f), "SUB A, H", self),
            0x95 => opcode!(ops::sub(&mut self.reg.a, l, &mut self.reg.f), "SUB A, L", self),
            0x96 => opcode!(ops::sub_value_at_address(&mut *self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), "SUB A, (HL)", self),
            0x97 => opcode!(ops::sub(&mut self.reg.a, a, &mut self.reg.f), "SUB A, A", self),
            0x98 => opcode!(ops::sbc(&mut self.reg.a, b, &mut self.reg.f), "SBC A, B", self),
            0x99 => opcode!(ops::sbc(&mut self.reg.a, c, &mut self.reg.f), "SBC A, C", self),
            0x9A => opcode!(ops::sbc(&mut self.reg.a, d, &mut self.reg.f), "SBC A, D", self),
            0x9B => opcode!(ops::sbc(&mut self.reg.a, e, &mut self.reg.f), "SBC A, E", self),
            0x9C => opcode!(ops::sbc(&mut self.reg.a, h, &mut self.reg.f), "SBC A, H", self),
            0x9D => opcode!(ops::sbc(&mut self.reg.a, l, &mut self.reg.f), "SBC A, L", self),
            0x9E => opcode!(ops::sbc_value_at_address(&mut *self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), "SBC A, (HL)", self),
            0x9F => opcode!(ops::sbc(&mut self.reg.a, a, &mut self.reg.f), "SBC A, A", self),
            0xA0 => opcode!(ops::and(&mut self.reg.a, b, &mut self.reg.f), "AND A, B", self),
            0xA1 => opcode!(ops::and(&mut self.reg.a, c, &mut self.reg.f), "AND A, C", self),
            0xA2 => opcode!(ops::and(&mut self.reg.a, d, &mut self.reg.f), "AND A, D", self),
            0xA3 => opcode!(ops::and(&mut self.reg.a, e, &mut self.reg.f), "AND A, E", self),
            0xA4 => opcode!(ops::and(&mut self.reg.a, h, &mut self.reg.f), "AND A, H", self),
            0xA5 => opcode!(ops::and(&mut self.reg.a, l, &mut self.reg.f), "AND A, L", self),
            0xA6 => opcode!(ops::and_value_at_address(&mut *self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), "AND A, (HL)", self),
            0xA7 => opcode!(ops::and(&mut self.reg.a, a, &mut self.reg.f), "AND A, A", self),
            0xA8 => opcode!(ops::xor(&mut self.reg.a, b, &mut self.reg.f), "XOR A, B", self),
            0xA9 => opcode!(ops::xor(&mut self.reg.a, c, &mut self.reg.f), "XOR A, C", self),
            0xAA => opcode!(ops::xor(&mut self.reg.a, d, &mut self.reg.f), "XOR A, D", self),
            0xAB => opcode!(ops::xor(&mut self.reg.a, e, &mut self.reg.f), "XOR A, E", self),
            0xAC => opcode!(ops::xor(&mut self.reg.a, h, &mut self.reg.f), "XOR A, H", self),
            0xAD => opcode!(ops::xor(&mut self.reg.a, l, &mut self.reg.f), "XOR A, L", self),
            0xAE => opcode!(ops::xor_value_at_address(&mut *self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), "XOR A, (HL)", self),
            0xAF => opcode!(ops::xor(&mut self.reg.a, a, &mut self.reg.f), "XOR A, A", self),
            0xB0 => opcode!(ops::or(&mut self.reg.a, b, &mut self.reg.f), "OR A, B", self),
            0xB1 => opcode!(ops::or(&mut self.reg.a, c, &mut self.reg.f), "OR A, C", self),
            0xB2 => opcode!(ops::or(&mut self.reg.a, d, &mut self.reg.f), "OR A, D", self),
            0xB3 => opcode!(ops::or(&mut self.reg.a, e, &mut self.reg.f), "OR A, E", self),
            0xB4 => opcode!(ops::or(&mut self.reg.a, h, &mut self.reg.f), "OR A, H", self),
            0xB5 => opcode!(ops::or(&mut self.reg.a, l, &mut self.reg.f), "OR A, L", self),
            0xB6 => opcode!(ops::or_value_at_address(&mut *self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), "OR A, (HL)", self),
            0xB7 => opcode!(ops::or(&mut self.reg.a, a, &mut self.reg.f), "OR A, A", self),
            0xB8 => opcode!(ops::compare(&mut self.reg.a, b, &mut self.reg.f), "CP A, B", self),
            0xB9 => opcode!(ops::compare(&mut self.reg.a, c, &mut self.reg.f), "CP A, C", self),
            0xBA => opcode!(ops::compare(&mut self.reg.a, d, &mut self.reg.f), "CP A, D", self),
            0xBB => opcode!(ops::compare(&mut self.reg.a, e, &mut self.reg.f), "CP A, E", self),
            0xBC => opcode!(ops::compare(&mut self.reg.a, h, &mut self.reg.f), "CP A, H", self),
            0xBD => opcode!(ops::compare(&mut self.reg.a, l, &mut self.reg.f), "CP A, L", self),
            0xBE => opcode!(ops::compare_value_at_address(&mut *self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), "CP A, (HL)", self),
            0xBF => opcode!(ops::compare(&mut self.reg.a, a, &mut self.reg.f), "CP A, A", self),
            0xC0 => opcode!(ops::ret(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, not_zero), "RET NZ", self),
            0xC1 => opcode!(ops::pop(&mut *self.mem, &mut self.reg.sp, &mut self.reg.b, &mut self.reg.c), "POP BC", self),
            0xC2 => opcode!(ops::jp_u16_immediate_if_true(&mut *self.mem, &mut self.reg.pc, not_zero), "JP NZ, nn", self),
            0xC3 => opcode!(ops::jp_u16_immediate(&mut *self.mem, &mut self.reg.pc), "JP nn", self),
            0xC4 => opcode!(ops::call_immediate_if_true(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, not_zero), "CALL NZ, nn", self),
            0xC5 => opcode!(ops::push(&mut *self.mem, &mut self.reg.sp, pack_u16(b, c)), "PUSH BC ", self),
            0xC6 => opcode!(ops::add_u8_immediate(&mut *self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, false), "ADD A, n", self),
            0xC7 => opcode!(ops::call(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x00), "RST 0", self),
            0xC8 => opcode!(ops::ret(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, zero), "RET Z", self),
            0xC9 => opcode!(ops::ret(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, true), "RET", self),
            0xCA => opcode!(ops::jp_u16_immediate_if_true(&mut *self.mem, &mut self.reg.pc, zero), "JP Z, nn", self),
            0xCB => self.extended_opcodes(),
            0xCC => opcode!(ops::call_immediate_if_true(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, zero), "CALL Z, nn", self),
            0xCD => opcode!(ops::call_immediate_if_true(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, true), "CALL nn", self),
            0xCE => opcode!(ops::add_u8_immediate(&mut *self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, true), "ADC A, n", self),
            0xCF => opcode!(ops::call(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x08), "RST 8", self),
            0xD0 => opcode!(ops::ret(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, no_carry), "RET NC", self),
            0xD1 => opcode!(ops::pop(&mut *self.mem, &mut self.reg.sp, &mut self.reg.d, &mut self.reg.e), "POP DE ", self),
            0xD2 => opcode!(ops::jp_u16_immediate_if_true(&mut *self.mem, &mut self.reg.pc, no_carry), "JP NC, nn", self),
            0xD3 => error!("0xD3 should never be executed"), 
            0xD4 => opcode!(ops::call_immediate_if_true(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, no_carry), "CALL NC, nn", self),
            0xD5 => opcode!(ops::push(&mut *self.mem, &mut self.reg.sp, pack_u16(d, e)), "PUSH DE ", self),
            0xD6 => opcode!(ops::sub_u8_immediate(&mut *self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, false), "SUB A, n", self),
            0xD7 => opcode!(ops::call(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x10), "RST 10", self),
            0xD8 => opcode!(ops::ret(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, carry), "RET C", self),
            0xD9 => opcode!(ops::reti(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, &mut self.reg.ime), "RETI", self),
            0xDA => opcode!(ops::jp_u16_immediate_if_true(&mut *self.mem, &mut self.reg.pc, carry), "JP C, nn", self),
            0xDB => error!("0xDB should never be executed"),
            0xDC => opcode!(ops::call_immediate_if_true(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, carry), "CALL C, nn", self),
            0xDD => error!("0xDD should never be executed"), 
            0xDE => opcode!(ops::sub_u8_immediate(&mut *self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, true), "SBC A, n", self),
            0xDF => opcode!(ops::call(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x18), "RST 18", self),
            0xE0 => opcode!(ops::write_val_FF00_plus_immediate(&mut *self.mem, &mut self.reg.pc, a), "LDH (n), A", self),
            0xE1 => opcode!(ops::pop(&mut *self.mem, &mut self.reg.sp, &mut self.reg.h, &mut self.reg.l), "POP HL ", self),
            0xE2 => opcode!(error!("Not Implemented"), "LDH (C), A", self),
            0xE3 => error!("0xE3 should never be executed"), 
            0xE4 => error!("0xE4 should never be executed"), 
            0xE5 => opcode!(ops::push(&mut *self.mem, &mut self.reg.sp, pack_u16(h, l)), "PUSH HL ", self),
            0xE6 => opcode!(ops::and(&mut self.reg.a, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc), &mut self.reg.f), "AND A, n", self),
            0xE7 => opcode!(ops::call(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x20), "RST 20", self),
            0xE8 => opcode!(error!("Not Implemented"), "ADD SP, (signed)n", self),
            0xE9 => opcode!(ops::jp(&mut self.reg.pc, pack_u16(h,l)), "JP (HL)", self),
            0xEA => opcode!(ops::write_value_to_u16_immediate(&mut *self.mem, &mut self.reg.pc, a), "LD (nn), A ", self),
            0xEB => error!("0xEB should never be executed"),
            0xEC => error!("0xEC should never be executed"),
            0xED => error!("0xED should never be executed"), 
            0xEE => opcode!(ops::xor(&mut self.reg.a, ops::u8_immediate(&mut *self.mem, &mut self.reg.pc), &mut self.reg.f), "XOR n", self),
            0xEF => opcode!(ops::call(&mut *self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x28), "RST 28", self),

            0xF0 => opcode!(ops::load_val_FF00_plus_immediate(&*self.mem, &mut self.reg.pc, &mut self.reg.a), "LDH A, (n)", self),
            0xF1 => opcode!(ops::pop_flags(&mut *self.mem, &mut self.reg.sp, &mut self.reg.a, &mut self.reg.f), "POP AF", self),
            0xF2 => error!("Not Implemented"),
            0xF3 => opcode!(ops::disable_interrupts(&mut self.reg.ime), "DI", self),
            0xF4 => error!("Not Implemented"),
            0xF5 => opcode!(ops::push(&mut *self.mem, &mut self.reg.sp, pack_u16(a, f)), "PUSH AF ", self),
            0xF6 => error!("Not Implemented"),
            0xF7 => error!("Not Implemented"),
            0xF8 => error!("Not Implemented"),
            0xF9 => error!("Not Implemented"),
            0xFA => opcode!(ops::ld_from_address(&*self.mem, &mut self.reg.a, ops::u16_immediate(&*self.mem, &mut self.reg.pc)), "LD A, (nn)", self),
            0xFB => error!("Not Implemented"),
            0xFC => error!("Not Implemented"),
            0xFD => error!("Not Implemented"),
            0xFE => opcode!(ops::compare(&mut self.reg.a, ops::u8_immediate(&*self.mem, &mut self.reg.pc), &mut self.reg.f), "CP A, n ", self),
            0xFF => error!("Not Implemented"),

            _ => panic!("Missing opcode!!!! {:X}", instr) 
        }
    }
   
    /// Fetches the instruction pointed to by the program counter
    /// and increments the pc by 1
    fn fetch_instruction(&mut self) -> u8 {
        let instr = self.mem.read_byte(self.reg.pc.read());
        self.reg.pc.increment();
        return instr;     
    }

    fn extended_opcodes(&mut self) {
        let instr = self.fetch_instruction(); 
        
        let a = self.reg.a.read();
        let b = self.reg.b.read();
        let c = self.reg.c.read();
        let d = self.reg.d.read();
        let e = self.reg.e.read();
        let h = self.reg.h.read();
        let l = self.reg.l.read();
        let f = self.reg.f.read();
        let sp = self.reg.sp.read();

        match instr {
            0x00 => opcode!(ops::rotate_left_with_carry(&mut self.reg.b, &mut self.reg.f), "RLC B", self),
            0x01 => opcode!(ops::rotate_left_with_carry(&mut self.reg.c, &mut self.reg.f), "RLC C", self),
            0x02 => opcode!(ops::rotate_left_with_carry(&mut self.reg.d, &mut self.reg.f), "RLC D", self),
            0x03 => opcode!(ops::rotate_left_with_carry(&mut self.reg.e, &mut self.reg.f), "RLC E", self),
            0x04 => opcode!(ops::rotate_left_with_carry(&mut self.reg.h, &mut self.reg.f), "RLC B", self),
            0x05 => opcode!(ops::rotate_left_with_carry(&mut self.reg.l, &mut self.reg.f), "RLC C", self),
            0x06 => opcode!(ops::rotate_left_with_carry_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "RLC (HL) ", self),
            0x07 => opcode!(ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), "RLC E", self),
            0x08 => opcode!(ops::rotate_right_with_carry(&mut self.reg.b, &mut self.reg.f), "RRC B", self),
            0x09 => opcode!(ops::rotate_right_with_carry(&mut self.reg.c, &mut self.reg.f), "RRC C", self),
            0x0A => opcode!(ops::rotate_right_with_carry(&mut self.reg.d, &mut self.reg.f), "RRC D", self),
            0x0B => opcode!(ops::rotate_right_with_carry(&mut self.reg.e, &mut self.reg.f), "RRC E", self),
            0x0C => opcode!(ops::rotate_right_with_carry(&mut self.reg.h, &mut self.reg.f), "RRC B", self),
            0x0D => opcode!(ops::rotate_right_with_carry(&mut self.reg.l, &mut self.reg.f), "RRC C", self),
            0x0E => opcode!(ops::rotate_right_with_carry_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "RRC (HL) ", self),
            0x0F => opcode!(ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), "RRC E", self),

            0x10 => opcode!(ops::rotate_left(&mut self.reg.b, &mut self.reg.f), "RL B", self),
            0x11 => opcode!(ops::rotate_left(&mut self.reg.c, &mut self.reg.f), "RL C", self),
            0x12 => opcode!(ops::rotate_left(&mut self.reg.d, &mut self.reg.f), "RL D", self),
            0x13 => opcode!(ops::rotate_left(&mut self.reg.e, &mut self.reg.f), "RL E", self),
            0x14 => opcode!(ops::rotate_left(&mut self.reg.h, &mut self.reg.f), "RL B", self),
            0x15 => opcode!(ops::rotate_left(&mut self.reg.l, &mut self.reg.f), "RL C", self),
            0x16 => opcode!(ops::rotate_left_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "RL (HL) ", self),
            0x17 => opcode!(ops::rotate_left(&mut self.reg.a, &mut self.reg.f), "RL E", self),
            0x18 => opcode!(ops::rotate_right(&mut self.reg.b, &mut self.reg.f), "RR B", self),
            0x19 => opcode!(ops::rotate_right(&mut self.reg.c, &mut self.reg.f), "RR C", self),
            0x1A => opcode!(ops::rotate_right(&mut self.reg.d, &mut self.reg.f), "RR D", self),
            0x1B => opcode!(ops::rotate_right(&mut self.reg.e, &mut self.reg.f), "RR E", self),
            0x1C => opcode!(ops::rotate_right(&mut self.reg.h, &mut self.reg.f), "RR B", self),
            0x1D => opcode!(ops::rotate_right(&mut self.reg.l, &mut self.reg.f), "RR C", self),
            0x1E => opcode!(ops::rotate_right_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "RR (HL) ", self),
            0x1F => opcode!(ops::rotate_right(&mut self.reg.a, &mut self.reg.f), "RR E", self),

            0x20 => opcode!(ops::sla(&mut self.reg.b, &mut self.reg.f), "SLA B", self),
            0x21 => opcode!(ops::sla(&mut self.reg.c, &mut self.reg.f), "SLA C", self),
            0x22 => opcode!(ops::sla(&mut self.reg.d, &mut self.reg.f), "SLA D", self),
            0x23 => opcode!(ops::sla(&mut self.reg.e, &mut self.reg.f), "SLA E", self),
            0x24 => opcode!(ops::sla(&mut self.reg.h, &mut self.reg.f), "SLA B", self),
            0x25 => opcode!(ops::sla(&mut self.reg.l, &mut self.reg.f), "SLA C", self),
            0x26 => opcode!(ops::sla_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "SLA (HL) ", self),
            0x27 => opcode!(ops::sla(&mut self.reg.a, &mut self.reg.f), "SLA E", self),
            0x28 => opcode!(ops::sra(&mut self.reg.b, &mut self.reg.f), "SRA B", self),
            0x29 => opcode!(ops::sra(&mut self.reg.c, &mut self.reg.f), "SRA C", self),
            0x2A => opcode!(ops::sra(&mut self.reg.d, &mut self.reg.f), "SRA D", self),
            0x2B => opcode!(ops::sra(&mut self.reg.e, &mut self.reg.f), "SRA E", self),
            0x2C => opcode!(ops::sra(&mut self.reg.h, &mut self.reg.f), "SRA B", self),
            0x2D => opcode!(ops::sra(&mut self.reg.l, &mut self.reg.f), "SRA C", self),
            0x2E => opcode!(ops::sra_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "SRA (HL) ", self),
            0x2F => opcode!(ops::sra(&mut self.reg.a, &mut self.reg.f), "SRA E", self),
            
            0x30 => opcode!(ops::swap(&mut self.reg.b, &mut self.reg.f), "SLA B", self),
            0x31 => opcode!(ops::swap(&mut self.reg.c, &mut self.reg.f), "SLA C", self),
            0x32 => opcode!(ops::swap(&mut self.reg.d, &mut self.reg.f), "SLA D", self),
            0x33 => opcode!(ops::swap(&mut self.reg.e, &mut self.reg.f), "SLA E", self),
            0x34 => opcode!(ops::swap(&mut self.reg.h, &mut self.reg.f), "SLA B", self),
            0x35 => opcode!(ops::swap(&mut self.reg.l, &mut self.reg.f), "SLA C", self),
            0x36 => opcode!(ops::swap_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "SLA (HL) ", self),
            0x37 => opcode!(ops::swap(&mut self.reg.a, &mut self.reg.f), "SLA E", self),
            0x38 => opcode!(ops::srl(&mut self.reg.b, &mut self.reg.f), "SRA B", self),
            0x39 => opcode!(ops::srl(&mut self.reg.c, &mut self.reg.f), "SRA C", self),
            0x3A => opcode!(ops::srl(&mut self.reg.d, &mut self.reg.f), "SRA D", self),
            0x3B => opcode!(ops::srl(&mut self.reg.e, &mut self.reg.f), "SRA E", self),
            0x3C => opcode!(ops::srl(&mut self.reg.h, &mut self.reg.f), "SRA B", self),
            0x3D => opcode!(ops::srl(&mut self.reg.l, &mut self.reg.f), "SRA C", self),
            0x3E => opcode!(ops::srl_at_address(&mut *self.mem, pack_u16(h, l), &mut self.reg.f), "SRA (HL) ", self),
            0x3F => opcode!(ops::srl(&mut self.reg.a, &mut self.reg.f), "SRA E", self),

            0x40 => opcode!(ops::bit(b, 0, &mut self.reg.f), "BIT 0, B", self),
            0x41 => opcode!(ops::bit(c, 0, &mut self.reg.f), "BIT 0, C", self),
            0x42 => opcode!(ops::bit(d, 0, &mut self.reg.f), "BIT 0, D", self),
            0x43 => opcode!(ops::bit(e, 0, &mut self.reg.f), "BIT 0, E", self),
            0x44 => opcode!(ops::bit(h, 0, &mut self.reg.f), "BIT 0, H", self),
            0x45 => opcode!(ops::bit(l, 0, &mut self.reg.f), "BIT 0, L", self),
            0x46 => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 0, &mut self.reg.f), "BIT 0, (HL)", self),
            0x47 => opcode!(ops::bit(a, 1, &mut self.reg.f), "BIT 1, A", self),
            0x48 => opcode!(ops::bit(b, 1, &mut self.reg.f), "BIT 1, B", self),
            0x49 => opcode!(ops::bit(c, 1, &mut self.reg.f), "BIT 1, C", self),
            0x4A => opcode!(ops::bit(d, 1, &mut self.reg.f), "BIT 1, D", self),
            0x4B => opcode!(ops::bit(e, 1, &mut self.reg.f), "BIT 1, E", self),
            0x4C => opcode!(ops::bit(h, 1, &mut self.reg.f), "BIT 1, H", self),
            0x4D => opcode!(ops::bit(l, 1, &mut self.reg.f), "BIT 1, L", self),
            0x4E => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 1, &mut self.reg.f), "BIT 1, (HL)", self),
            0x4F => opcode!(ops::bit(a, 1, &mut self.reg.f), "BIT 1, A", self),

            0x50 => opcode!(ops::bit(b, 2, &mut self.reg.f), "BIT 2, B", self),
            0x51 => opcode!(ops::bit(c, 2, &mut self.reg.f), "BIT 2, C", self),
            0x52 => opcode!(ops::bit(d, 2, &mut self.reg.f), "BIT 2, D", self),
            0x53 => opcode!(ops::bit(e, 2, &mut self.reg.f), "BIT 2, E", self),
            0x54 => opcode!(ops::bit(h, 2, &mut self.reg.f), "BIT 2, H", self),
            0x55 => opcode!(ops::bit(l, 2, &mut self.reg.f), "BIT 2, L", self),
            0x56 => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 2, &mut self.reg.f), "BIT 2, (HL)", self),
            0x57 => opcode!(ops::bit(a, 3, &mut self.reg.f), "BIT 3, A", self),
            0x58 => opcode!(ops::bit(b, 3, &mut self.reg.f), "BIT 3, B", self),
            0x59 => opcode!(ops::bit(c, 3, &mut self.reg.f), "BIT 3, C", self),
            0x5A => opcode!(ops::bit(d, 3, &mut self.reg.f), "BIT 3, D", self),
            0x5B => opcode!(ops::bit(e, 3, &mut self.reg.f), "BIT 3, E", self),
            0x5C => opcode!(ops::bit(h, 3, &mut self.reg.f), "BIT 3, H", self),
            0x5D => opcode!(ops::bit(l, 3, &mut self.reg.f), "BIT 3, L", self),
            0x5E => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 3, &mut self.reg.f), "BIT 3, (HL)", self),
            0x5F => opcode!(ops::bit(a, 3, &mut self.reg.f), "BIT 3, A", self),

            0x60 => opcode!(ops::bit(b, 4, &mut self.reg.f), "BIT 4, B", self),
            0x61 => opcode!(ops::bit(c, 4, &mut self.reg.f), "BIT 4, C", self),
            0x62 => opcode!(ops::bit(d, 4, &mut self.reg.f), "BIT 4, D", self),
            0x63 => opcode!(ops::bit(e, 4, &mut self.reg.f), "BIT 4, E", self),
            0x64 => opcode!(ops::bit(h, 4, &mut self.reg.f), "BIT 4, H", self),
            0x65 => opcode!(ops::bit(l, 4, &mut self.reg.f), "BIT 4, L", self),
            0x66 => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 4, &mut self.reg.f), "BIT 4, (HL)", self),
            0x67 => opcode!(ops::bit(a, 5, &mut self.reg.f), "BIT 5, A", self),
            0x68 => opcode!(ops::bit(b, 5, &mut self.reg.f), "BIT 5, B", self),
            0x69 => opcode!(ops::bit(c, 5, &mut self.reg.f), "BIT 5, C", self),
            0x6A => opcode!(ops::bit(d, 5, &mut self.reg.f), "BIT 5, D", self),
            0x6B => opcode!(ops::bit(e, 5, &mut self.reg.f), "BIT 5, E", self),
            0x6C => opcode!(ops::bit(h, 5, &mut self.reg.f), "BIT 5, H", self),
            0x6D => opcode!(ops::bit(l, 5, &mut self.reg.f), "BIT 5, L", self),
            0x6E => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 5, &mut self.reg.f), "BIT 5, (HL)", self),
            0x6F => opcode!(ops::bit(a, 5, &mut self.reg.f), "BIT 5, A", self),

            0x70 => opcode!(ops::bit(b, 6, &mut self.reg.f), "BIT 6, B", self),
            0x71 => opcode!(ops::bit(c, 6, &mut self.reg.f), "BIT 6, C", self),
            0x72 => opcode!(ops::bit(d, 6, &mut self.reg.f), "BIT 6, D", self),
            0x73 => opcode!(ops::bit(e, 6, &mut self.reg.f), "BIT 6, E", self),
            0x74 => opcode!(ops::bit(h, 6, &mut self.reg.f), "BIT 6, H", self),
            0x75 => opcode!(ops::bit(l, 6, &mut self.reg.f), "BIT 6, L", self),
            0x76 => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 6, &mut self.reg.f), "BIT 6, (HL)", self),
            0x77 => opcode!(ops::bit(a, 7, &mut self.reg.f), "BIT 7, A", self),
            0x78 => opcode!(ops::bit(b, 7, &mut self.reg.f), "BIT 7, B", self),
            0x79 => opcode!(ops::bit(c, 7, &mut self.reg.f), "BIT 7, C", self),
            0x7A => opcode!(ops::bit(d, 7, &mut self.reg.f), "BIT 7, D", self),
            0x7B => opcode!(ops::bit(e, 7, &mut self.reg.f), "BIT 7, E", self),
            0x7C => opcode!(ops::bit(h, 7, &mut self.reg.f), "BIT 7, H", self),
            0x7D => opcode!(ops::bit(l, 7, &mut self.reg.f), "BIT 7, L", self),
            0x7E => opcode!(ops::bit(ops::byte_at_address(&mut *self.mem, pack_u16(h,l)), 7, &mut self.reg.f), "BIT 7, (HL)", self),
            0x7F => opcode!(ops::bit(a, 7, &mut self.reg.f), "BIT 7, A", self),

            0x80 => opcode!(ops::res(&mut self.reg.b, 0), "RES 0, B", self),
            0x81 => opcode!(ops::res(&mut self.reg.c, 0), "RES 0, C", self),
            0x82 => opcode!(ops::res(&mut self.reg.d, 0), "RES 0, D", self),
            0x83 => opcode!(ops::res(&mut self.reg.e, 0), "RES 0, E", self),
            0x84 => opcode!(ops::res(&mut self.reg.h, 0), "RES 0, H", self),
            0x85 => opcode!(ops::res(&mut self.reg.l, 0), "RES 0, L", self),
            0x86 => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 0), "RES 0, (HL)", self),
            0x87 => opcode!(ops::res(&mut self.reg.a, 0), "RES 0, A", self),
            0x88 => opcode!(ops::res(&mut self.reg.b, 1), "RES 1, B", self),
            0x89 => opcode!(ops::res(&mut self.reg.c, 1), "RES 1, C", self),
            0x8A => opcode!(ops::res(&mut self.reg.d, 1), "RES 1, D", self),
            0x8B => opcode!(ops::res(&mut self.reg.e, 1), "RES 1, E", self),
            0x8C => opcode!(ops::res(&mut self.reg.h, 1), "RES 1, H", self),
            0x8D => opcode!(ops::res(&mut self.reg.l, 1), "RES 1, L", self),
            0x8E => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 1), "RES 1, (HL)", self),
            0x8F => opcode!(ops::res(&mut self.reg.a, 1), "RES 1, A", self),

            0x90 => opcode!(ops::res(&mut self.reg.b, 2), "RES 2, B", self),
            0x91 => opcode!(ops::res(&mut self.reg.c, 2), "RES 2, C", self),
            0x92 => opcode!(ops::res(&mut self.reg.d, 2), "RES 2, D", self),
            0x93 => opcode!(ops::res(&mut self.reg.e, 2), "RES 2, E", self),
            0x94 => opcode!(ops::res(&mut self.reg.h, 2), "RES 2, H", self),
            0x95 => opcode!(ops::res(&mut self.reg.l, 2), "RES 2, L", self),
            0x96 => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 2), "RES 2, (HL)", self),
            0x97 => opcode!(ops::res(&mut self.reg.a, 2), "RES 2, A", self),
            0x98 => opcode!(ops::res(&mut self.reg.b, 3), "RES 3, B", self),
            0x99 => opcode!(ops::res(&mut self.reg.c, 3), "RES 3, C", self),
            0x9A => opcode!(ops::res(&mut self.reg.d, 3), "RES 3, D", self),
            0x9B => opcode!(ops::res(&mut self.reg.e, 3), "RES 3, E", self),
            0x9C => opcode!(ops::res(&mut self.reg.h, 3), "RES 3, H", self),
            0x9D => opcode!(ops::res(&mut self.reg.l, 3), "RES 3, L", self),
            0x9E => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 3), "RES 3, (HL)", self),
            0x9F => opcode!(ops::res(&mut self.reg.a, 3), "RES 3, A", self),
            
            0xA0 => opcode!(ops::res(&mut self.reg.b, 4), "RES 4, B", self),
            0xA1 => opcode!(ops::res(&mut self.reg.c, 4), "RES 4, C", self),
            0xA2 => opcode!(ops::res(&mut self.reg.d, 4), "RES 4, D", self),
            0xA3 => opcode!(ops::res(&mut self.reg.e, 4), "RES 4, E", self),
            0xA4 => opcode!(ops::res(&mut self.reg.h, 4), "RES 4, H", self),
            0xA5 => opcode!(ops::res(&mut self.reg.l, 4), "RES 4, L", self),
            0xA6 => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 4), "RES 4, (HL)", self),
            0xA7 => opcode!(ops::res(&mut self.reg.a, 4), "RES 4, A", self),
            0xA8 => opcode!(ops::res(&mut self.reg.b, 5), "RES 5, B", self),
            0xA9 => opcode!(ops::res(&mut self.reg.c, 5), "RES 5, C", self),
            0xAA => opcode!(ops::res(&mut self.reg.d, 5), "RES 5, D", self),
            0xAB => opcode!(ops::res(&mut self.reg.e, 5), "RES 5, E", self),
            0xAC => opcode!(ops::res(&mut self.reg.h, 5), "RES 5, H", self),
            0xAD => opcode!(ops::res(&mut self.reg.l, 5), "RES 5, L", self),
            0xAE => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 5), "RES 5, (HL)", self),
            0xAF => opcode!(ops::res(&mut self.reg.a, 5), "RES 5, A", self),
 
            0xB0 => opcode!(ops::res(&mut self.reg.b, 6), "RES 6, B", self),
            0xB1 => opcode!(ops::res(&mut self.reg.c, 6), "RES 6, C", self),
            0xB2 => opcode!(ops::res(&mut self.reg.d, 6), "RES 6, D", self),
            0xB3 => opcode!(ops::res(&mut self.reg.e, 6), "RES 6, E", self),
            0xB4 => opcode!(ops::res(&mut self.reg.h, 6), "RES 6, H", self),
            0xB5 => opcode!(ops::res(&mut self.reg.l, 6), "RES 6, L", self),
            0xB6 => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 6), "RES 6, (HL)", self),
            0xB7 => opcode!(ops::res(&mut self.reg.a, 6), "RES 6, A", self),
            0xB8 => opcode!(ops::res(&mut self.reg.b, 7), "RES 7, B", self),
            0xB9 => opcode!(ops::res(&mut self.reg.c, 7), "RES 7, C", self),
            0xBA => opcode!(ops::res(&mut self.reg.d, 7), "RES 7, D", self),
            0xBB => opcode!(ops::res(&mut self.reg.e, 7), "RES 7, E", self),
            0xBC => opcode!(ops::res(&mut self.reg.h, 7), "RES 7, H", self),
            0xBD => opcode!(ops::res(&mut self.reg.l, 7), "RES 7, L", self),
            0xBE => opcode!(ops::res_at_addr(&mut *self.mem, pack_u16(h,l), 7), "RES 7, (HL)", self),
            0xBF => opcode!(ops::res(&mut self.reg.a, 7), "RES 7, A", self),

            0xC0 => opcode!(ops::set(&mut self.reg.b, 0), "SET 0, B", self),
            0xC1 => opcode!(ops::set(&mut self.reg.c, 0), "SET 0, C", self),
            0xC2 => opcode!(ops::set(&mut self.reg.d, 0), "SET 0, D", self),
            0xC3 => opcode!(ops::set(&mut self.reg.e, 0), "SET 0, E", self),
            0xC4 => opcode!(ops::set(&mut self.reg.h, 0), "SET 0, H", self),
            0xC5 => opcode!(ops::set(&mut self.reg.l, 0), "SET 0, L", self),
            0xC6 => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 0), "SET 0, (HL)", self),
            0xC7 => opcode!(ops::set(&mut self.reg.a, 0), "SET 0, A", self),
            0xC8 => opcode!(ops::set(&mut self.reg.b, 1), "SET 1, B", self),
            0xC9 => opcode!(ops::set(&mut self.reg.c, 1), "SET 1, C", self),
            0xCA => opcode!(ops::set(&mut self.reg.d, 1), "SET 1, D", self),
            0xCB => opcode!(ops::set(&mut self.reg.e, 1), "SET 1, E", self),
            0xCC => opcode!(ops::set(&mut self.reg.h, 1), "SET 1, H", self),
            0xCD => opcode!(ops::set(&mut self.reg.l, 1), "SET 1, L", self),
            0xCE => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 1), "SET 1, (HL)", self),
            0xCF => opcode!(ops::set(&mut self.reg.a, 1), "SET 1, A", self),

            0xD0 => opcode!(ops::set(&mut self.reg.b, 2), "SET 2, B", self),
            0xD1 => opcode!(ops::set(&mut self.reg.c, 2), "SET 2, C", self),
            0xD2 => opcode!(ops::set(&mut self.reg.d, 2), "SET 2, D", self),
            0xD3 => opcode!(ops::set(&mut self.reg.e, 2), "SET 2, E", self),
            0xD4 => opcode!(ops::set(&mut self.reg.h, 2), "SET 2, H", self),
            0xD5 => opcode!(ops::set(&mut self.reg.l, 2), "SET 2, L", self),
            0xD6 => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 2), "SET 2, (HL)", self),
            0xD7 => opcode!(ops::set(&mut self.reg.a, 2), "SET 2, A", self),
            0xD8 => opcode!(ops::set(&mut self.reg.b, 3), "SET 3, B", self),
            0xD9 => opcode!(ops::set(&mut self.reg.c, 3), "SET 3, C", self),
            0xDA => opcode!(ops::set(&mut self.reg.d, 3), "SET 3, D", self),
            0xDB => opcode!(ops::set(&mut self.reg.e, 3), "SET 3, E", self),
            0xDC => opcode!(ops::set(&mut self.reg.h, 3), "SET 3, H", self),
            0xDD => opcode!(ops::set(&mut self.reg.l, 3), "SET 3, L", self),
            0xDE => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 3), "SET 3, (HL)", self),
            0xDF => opcode!(ops::set(&mut self.reg.a, 3), "SET 3, A", self),
            
            0xE0 => opcode!(ops::set(&mut self.reg.b, 4), "SET 4, B", self),
            0xE1 => opcode!(ops::set(&mut self.reg.c, 4), "SET 4, C", self),
            0xE2 => opcode!(ops::set(&mut self.reg.d, 4), "SET 4, D", self),
            0xE3 => opcode!(ops::set(&mut self.reg.e, 4), "SET 4, E", self),
            0xE4 => opcode!(ops::set(&mut self.reg.h, 4), "SET 4, H", self),
            0xE5 => opcode!(ops::set(&mut self.reg.l, 4), "SET 4, L", self),
            0xE6 => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 4), "SET 4, (HL)", self),
            0xE7 => opcode!(ops::set(&mut self.reg.a, 4), "SET 4, A", self),
            0xE8 => opcode!(ops::set(&mut self.reg.b, 5), "SET 5, B", self),
            0xE9 => opcode!(ops::set(&mut self.reg.c, 5), "SET 5, C", self),
            0xEA => opcode!(ops::set(&mut self.reg.d, 5), "SET 5, D", self),
            0xEB => opcode!(ops::set(&mut self.reg.e, 5), "SET 5, E", self),
            0xEC => opcode!(ops::set(&mut self.reg.h, 5), "SET 5, H", self),
            0xED => opcode!(ops::set(&mut self.reg.l, 5), "SET 5, L", self),
            0xEE => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 5), "SET 5, (HL)", self),
            0xEF => opcode!(ops::set(&mut self.reg.a, 5), "SET 5, A", self),
 
            0xF0 => opcode!(ops::set(&mut self.reg.b, 6), "SET 6, B", self),
            0xF1 => opcode!(ops::set(&mut self.reg.c, 6), "SET 6, C", self),
            0xF2 => opcode!(ops::set(&mut self.reg.d, 6), "SET 6, D", self),
            0xF3 => opcode!(ops::set(&mut self.reg.e, 6), "SET 6, E", self),
            0xF4 => opcode!(ops::set(&mut self.reg.h, 6), "SET 6, H", self),
            0xF5 => opcode!(ops::set(&mut self.reg.l, 6), "SET 6, L", self),
            0xF6 => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 6), "SET 6, (HL)", self),
            0xF7 => opcode!(ops::set(&mut self.reg.a, 6), "SET 6, A", self),
            0xF8 => opcode!(ops::set(&mut self.reg.b, 7), "SET 7, B", self),
            0xF9 => opcode!(ops::set(&mut self.reg.c, 7), "SET 7, C", self),
            0xFA => opcode!(ops::set(&mut self.reg.d, 7), "SET 7, D", self),
            0xFB => opcode!(ops::set(&mut self.reg.e, 7), "SET 7, E", self),
            0xFC => opcode!(ops::set(&mut self.reg.h, 7), "SET 7, H", self),
            0xFD => opcode!(ops::set(&mut self.reg.l, 7), "SET 7, L", self),
            0xFE => opcode!(ops::set_at_addr(&mut *self.mem, pack_u16(h,l), 7), "SET 7, (HL)", self),
            0xFF => opcode!(ops::set(&mut self.reg.a, 7), "SET 7, A", self),
            _ => panic!("Missing opcode!!!! {:X}", instr) 
        }
    }
}

struct Registers {
    a: Register<u8>, b: Register<u8>, c: Register<u8>, 
    d: Register<u8>, e: Register<u8>, f: Register<Flags>, 
    h: Register<u8>, l: Register<u8>, // 8-bit registers

    pc: Register<u16>, sp: Register<u16>, // 16-bit registers
    m: Register<u16>, t: Register<u16>, // clock

    ime: bool // Master Interrupt Enable
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
            t: Register::new(0),

            ime: true
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
