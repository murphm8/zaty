use memory::{Memory, low_nibble, high_nibble, low_byte, high_byte, pack_u16};
use extensions::Incrementor;
use ops::{mod};
use std::num::{Unsigned, One};


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
        let f = self.reg.f.read();
        let sp = self.reg.sp.read();

        let zero = self.reg.f.read().contains(ZeroFlag);
        let not_zero = !zero;
        let carry = self.reg.f.read().contains(CarryFlag);
        let no_carry = !carry;

        match instr {
            0x00 => ops::nop(), // NOP
            0x01 => ops::ld_u16_immediate(self.mem, &mut self.reg.pc, &mut self.reg.b, &mut self.reg.c), // LD BC, nn
            0x02 => ops::write_value_to_memory_at_address(self.mem, self.reg.a.read(), self.reg.b.read(), self.reg.c.read()), // LD (BC), A
            0x03 => ops::increment_register_pair(&mut self.reg.b, &mut self.reg.c), // INC BC
            0x04 => ops::increment_register(&mut self.reg.b, &mut self.reg.f), // INC B
            0x05 => ops::decrement_register(&mut self.reg.b, &mut self.reg.f), // DEC B
            0x06 => ops::ld_u8(&mut self.reg.b, ops::u8_immediate(self.mem, &mut self.reg.pc)), // LD B, n
            0x07 => ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), // RLC A
            0x08 => ops::write_u16_immediate_address(self.mem, &mut self.reg.pc, sp), // LD (nn), SP
            0x09 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, b, c, &mut self.reg.f), // ADD HL, BC
            0x0A => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.a, b, c), // LD A, (BC)
            0x0B => ops::decrement_register_pair(&mut self.reg.b, &mut self.reg.c), // DEC BC
            0x0C => ops::increment_register(&mut self.reg.c, &mut self.reg.f), // INC C
            0x0D => ops::decrement_register(&mut self.reg.c, &mut self.reg.f), // DEC C
            0x0E => ops::ld_u8(&mut self.reg.c, ops::u8_immediate(self.mem, &mut self.reg.pc)), // LD C, n
            0x0F => ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), // RRC A

            0x10 => error!("STOP Op Code not implemented and is being used"), // STOP
            0x11 => ops::ld_u16_immediate(self.mem, &mut self.reg.pc, &mut self.reg.d, &mut self.reg.e), // LD DE, nn
            0x12 => ops::write_value_to_memory_at_address(self.mem, a, d, e), // LD (DE), A
            0x13 => ops::increment_register_pair(&mut self.reg.d, &mut self.reg.e), // INC DE
            0x14 => ops::increment_register(&mut self.reg.d, &mut self.reg.f), // INC D
            0x15 => ops::decrement_register(&mut self.reg.d, &mut self.reg.f), // DEC D
            0x16 => ops::ld_u8(&mut self.reg.d, ops::u8_immediate(self.mem, &mut self.reg.pc)), // LD D, n
            0x17 => ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), // RL A
            0x18 => ops::jump_by_signed_immediate(self.mem, &mut self.reg.pc), // JR n
            0x19 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, d, e, &mut self.reg.f), // Add HL, DE
            0x1A => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.a, d, e), // LD A, (DE)
            0x1B => ops::decrement_register_pair(&mut self.reg.d, &mut self.reg.e), // DEC DE
            0x1C => ops::increment_register(&mut self.reg.e, &mut self.reg.f), // INC E
            0x1E => ops::decrement_register(&mut self.reg.e, &mut self.reg.f), // DEC E
            0x1F => ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), // RR A

            0x20 => ops::relative_jmp_by_signed_immediate_if_not_flag(self.mem, &mut self.reg.pc, &self.reg.f, ZeroFlag), // JR NZ, n
            0x21 => ops::ld_u16_immediate(self.mem, &mut self.reg.pc, &mut self.reg.h, &mut self.reg.l), // LD HL, nn
            0x22 => ops::write_value_to_memory_at_address_and_increment_register(self.mem, self.reg.a.read(), &mut self.reg.h, &mut self.reg.l), // LDI (HL), A
            0x23 => ops::increment_register_pair(&mut self.reg.h, &mut self.reg.l), // INC HL
            0x24 => ops::increment_register(&mut self.reg.h, &mut self.reg.f), // INC H
            0x25 => ops::decrement_register(&mut self.reg.h, &mut self.reg.f), // DEC H
            0x26 => ops::ld_u8(&mut self.reg.h, ops::u8_immediate(self.mem, &mut self.reg.pc)), // LD H, n
            0x27 => error!("DAA instruction not implemented and is being used"),
            0x28 => ops::relative_jmp_by_signed_immediate_if_flag(self.mem, &mut self.reg.pc, &self.reg.f, ZeroFlag), // JR Z, n
            0x29 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, h, l, &mut self.reg.f), // ADD HL, HL 
            0x2A => ops::ld_from_address_pointed_to_by_register_pair_and_increment_register_pair(self.mem, &mut self.reg.a, &mut self.reg.h, &mut self.reg.l), // LDI A, HL
            0x2B => ops::decrement_register_pair(&mut self.reg.h, &mut self.reg.l), // DEC HL 
            0x2C => ops::increment_register(&mut self.reg.l, &mut self.reg.f), // INC L
            0x2D => ops::decrement_register(&mut self.reg.l, &mut self.reg.f), // DEC L
            0x2E => ops::ld_u8(&mut self.reg.l, ops::u8_immediate(self.mem, &mut self.reg.pc)), // LD L, d8
            0x2F => ops::complement(&mut self.reg.a, &mut self.reg.f), // CPL 

            0x30 => ops::relative_jmp_by_signed_immediate_if_not_flag(self.mem, &mut self.reg.pc, &self.reg.f, CarryFlag), // JR NC, n
            0x31 => ops::ld_next_two_bytes_into_reg(self.mem, &mut self.reg.pc, &mut self.reg.sp), // LD SP, nn
            0x32 => ops::write_value_to_memory_at_address_and_decrement_register(self.mem, self.reg.a.read(), &mut self.reg.h, &mut self.reg.l), // LDI (HL), A
            0x33 => self.reg.sp.increment(), // INC SP 
            0x34 => ops::increment_value_at_address(self.mem, self.reg.h.read(), self.reg.l.read(), &mut self.reg.f),
            0x35 => ops::decrement_value_at_address(self.mem, self.reg.h.read(), self.reg.l.read(), &mut self.reg.f),
            0x36 => ops::ld_u8_immediate_into_address(self.mem, &mut self.reg.pc, self.reg.h.read(), self.reg.l.read()),
            0x37 => ops::set_flag(&mut self.reg.f, CarryFlag),
            0x38 => ops::relative_jmp_by_signed_immediate_if_flag(self.mem, &mut self.reg.pc, &mut self.reg.f, CarryFlag),
            0x39 => ops::add_register_pair_to_register_pair(&mut self.reg.h, &mut self.reg.l, high_byte(sp), low_byte(sp), &mut self.reg.f), // ADD HL, SP 
            0x3A => ops::ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair(self.mem, &mut self.reg.a, &mut self.reg.h, &mut self.reg.l), // LDD A, HL
            0x3B => self.reg.sp.decrement(), // INC SP
            0x3C => ops::increment_register(&mut self.reg.a, &mut self.reg.f), // INC A
            0x3D => ops::decrement_register(&mut self.reg.a, &mut self.reg.f), // DEC A
            0x3E => ops::ld_u8(&mut self.reg.a, ops::u8_immediate(self.mem, &mut self.reg.pc)), // LD A, d8
            0x3F => ops::reset_flag(&mut self.reg.f, CarryFlag), // CCF
            0x40 => ops::ld_u8(&mut self.reg.b, b), // LD B, B
            0x41 => ops::ld_u8(&mut self.reg.b, c), // LD B, C
            0x42 => ops::ld_u8(&mut self.reg.b, d), // LD B, D
            0x43 => ops::ld_u8(&mut self.reg.b, e), // LD B, E
            0x44 => ops::ld_u8(&mut self.reg.b, h), // LD B, H
            0x45 => ops::ld_u8(&mut self.reg.b, l), // LD B, L
            0x46 => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD B, (HL)
            0x47 => ops::ld_u8(&mut self.reg.b, a), // LD B, A
            0x48 => ops::ld_u8(&mut self.reg.c, b), // LD C, B
            0x49 => ops::ld_u8(&mut self.reg.c, c), // LD C, C
            0x4A => ops::ld_u8(&mut self.reg.c, d), // LD C, D
            0x4B => ops::ld_u8(&mut self.reg.c, e), // LD C, E
            0x4C => ops::ld_u8(&mut self.reg.c, h), // LD C, H
            0x4D => ops::ld_u8(&mut self.reg.c, l), // LD C, L
            0x4E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD C, (HL) 
            0x4F => ops::ld_u8(&mut self.reg.c, a), // LD C, A 
            0x50 => ops::ld_u8(&mut self.reg.d, b), // LD D, B
            0x51 => ops::ld_u8(&mut self.reg.d, c), // LD D, C
            0x52 => ops::ld_u8(&mut self.reg.d, d), // LD D, D
            0x53 => ops::ld_u8(&mut self.reg.d, e), // LD D, E
            0x54 => ops::ld_u8(&mut self.reg.d, h), // LD D, H
            0x55 => ops::ld_u8(&mut self.reg.d, l), // LD D, L
            0x56 => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.d, h, l), // LD D, (HL)
            0x57 => ops::ld_u8(&mut self.reg.d, a), // LD D, A
            0x58 => ops::ld_u8(&mut self.reg.e, b), // LD E, B
            0x59 => ops::ld_u8(&mut self.reg.e, c), // LD E, C
            0x5A => ops::ld_u8(&mut self.reg.e, d), // LD E, D
            0x5B => ops::ld_u8(&mut self.reg.e, e), // LD E, E
            0x5C => ops::ld_u8(&mut self.reg.e, h), // LD E, H
            0x5D => ops::ld_u8(&mut self.reg.e, l), // LD E, L
            0x5E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD E, (HL) 
            0x5F => ops::ld_u8(&mut self.reg.e, a), // LD E, A          
            0x60 => ops::ld_u8(&mut self.reg.h, b), // LD H, B
            0x61 => ops::ld_u8(&mut self.reg.h, c), // LD H, C
            0x62 => ops::ld_u8(&mut self.reg.h, d), // LD H, D
            0x63 => ops::ld_u8(&mut self.reg.h, e), // LD H, E
            0x64 => ops::ld_u8(&mut self.reg.h, h), // LD H, H
            0x65 => ops::ld_u8(&mut self.reg.h, l), // LD H, L
            0x66 => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.h, h, l), // LD H, (HL)
            0x67 => ops::ld_u8(&mut self.reg.h, a), // LD H, A
            0x68 => ops::ld_u8(&mut self.reg.l, b), // LD L, B
            0x69 => ops::ld_u8(&mut self.reg.l, c), // LD L, C
            0x6A => ops::ld_u8(&mut self.reg.l, d), // LD L, D
            0x6B => ops::ld_u8(&mut self.reg.l, e), // LD L, E
            0x6C => ops::ld_u8(&mut self.reg.l, h), // LD L, H
            0x6D => ops::ld_u8(&mut self.reg.l, l), // LD L, L
            0x6E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD L, (HL) 
            0x6F => ops::ld_u8(&mut self.reg.l, a), // LD L, A          
            0x70 => ops::write_value_to_memory_at_address(self.mem, b, h, l), // LD (HL), B
            0x71 => ops::write_value_to_memory_at_address(self.mem, c, h, l), // LD (HL), C
            0x72 => ops::write_value_to_memory_at_address(self.mem, d, h, l), // LD (HL), D
            0x73 => ops::write_value_to_memory_at_address(self.mem, e, h, l), // LD (HL), E
            0x74 => ops::write_value_to_memory_at_address(self.mem, h, h, l), // LD (HL), H
            0x75 => ops::write_value_to_memory_at_address(self.mem, l, h, l), // LD (HL), L
            0x76 => error!("HALT instruction not implemented"),
            0x77 => ops::write_value_to_memory_at_address(self.mem, a, h, l), // LD (HL), A
            0x78 => ops::ld_u8(&mut self.reg.a, b), // LD A, B
            0x79 => ops::ld_u8(&mut self.reg.a, c), // LD A, C
            0x7A => ops::ld_u8(&mut self.reg.a, d), // LD A, D
            0x7B => ops::ld_u8(&mut self.reg.a, e), // LD A, E
            0x7C => ops::ld_u8(&mut self.reg.a, h), // LD A, H
            0x7D => ops::ld_u8(&mut self.reg.a, l), // LD A, L
            0x7E => ops::ld_from_reg_pair_as_address(self.mem, &mut self.reg.b, h, l), // LD A, (HL) 
            0x7F => ops::ld_u8(&mut self.reg.a, a), // LD A, A          
            0x80 => ops::add(&mut self.reg.a, b, &mut self.reg.f), // ADD A, B
            0x81 => ops::add(&mut self.reg.a, c, &mut self.reg.f), // ADD A, C
            0x82 => ops::add(&mut self.reg.a, d, &mut self.reg.f), // ADD A, D
            0x83 => ops::add(&mut self.reg.a, e, &mut self.reg.f), // ADD A, E
            0x84 => ops::add(&mut self.reg.a, h, &mut self.reg.f), // ADD A, H
            0x85 => ops::add(&mut self.reg.a, l, &mut self.reg.f), // ADD A, L
            0x86 => ops::add_value_at_address(self.mem, &mut self.reg.a, h, l, &mut self.reg.f), // ADD A, (HL)
            0x87 => ops::add(&mut self.reg.a, a, &mut self.reg.f), // ADD A, A
            0x88 => ops::adc(&mut self.reg.a, b, &mut self.reg.f), // ADC A, B
            0x89 => ops::adc(&mut self.reg.a, c, &mut self.reg.f), // ADC A, C
            0x8A => ops::adc(&mut self.reg.a, d, &mut self.reg.f), // ADC A, D
            0x8B => ops::adc(&mut self.reg.a, e, &mut self.reg.f), // ADC A, E
            0x8C => ops::adc(&mut self.reg.a, h, &mut self.reg.f), // ADC A, H
            0x8D => ops::adc(&mut self.reg.a, l, &mut self.reg.f), // ADC A, L
            0x8E => ops::adc_value_at_address(self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), // ADC A, (HL)
            0x8F => ops::adc(&mut self.reg.a, a, &mut self.reg.f), // ADC A, A
            0x90 => ops::sub(&mut self.reg.a, b, &mut self.reg.f), // SUB A, B
            0x91 => ops::sub(&mut self.reg.a, c, &mut self.reg.f), // SUB A, C
            0x92 => ops::sub(&mut self.reg.a, d, &mut self.reg.f), // SUB A, D
            0x93 => ops::sub(&mut self.reg.a, e, &mut self.reg.f), // SUB A, E
            0x94 => ops::sub(&mut self.reg.a, h, &mut self.reg.f), // SUB A, H
            0x95 => ops::sub(&mut self.reg.a, l, &mut self.reg.f), // SUB A, L
            0x96 => ops::sub_value_at_address(self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), // SUB A, (HL)
            0x97 => ops::sub(&mut self.reg.a, a, &mut self.reg.f), // SUB A, A
            0x98 => ops::sbc(&mut self.reg.a, b, &mut self.reg.f), // SBC A, B
            0x99 => ops::sbc(&mut self.reg.a, c, &mut self.reg.f), // SBC A, C
            0x9A => ops::sbc(&mut self.reg.a, d, &mut self.reg.f), // SBC A, D
            0x9B => ops::sbc(&mut self.reg.a, e, &mut self.reg.f), // SBC A, E
            0x9C => ops::sbc(&mut self.reg.a, h, &mut self.reg.f), // SBC A, H
            0x9D => ops::sbc(&mut self.reg.a, l, &mut self.reg.f), // SBC A, L
            0x9E => ops::sbc_value_at_address(self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f),  // SBC A, (HL)
            0x9F => ops::sbc(&mut self.reg.a, a, &mut self.reg.f), // SBC A, A
            0xA0 => ops::and(&mut self.reg.a, b, &mut self.reg.f), // AND A, B
            0xA1 => ops::and(&mut self.reg.a, c, &mut self.reg.f), // AND A, C
            0xA2 => ops::and(&mut self.reg.a, d, &mut self.reg.f), // AND A, D
            0xA3 => ops::and(&mut self.reg.a, e, &mut self.reg.f), // AND A, E
            0xA4 => ops::and(&mut self.reg.a, h, &mut self.reg.f), // AND A, H
            0xA5 => ops::and(&mut self.reg.a, l, &mut self.reg.f), // AND A, L
            0xA6 => ops::and_value_at_address(self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), // AND A, (HL)
            0xA7 => ops::and(&mut self.reg.a, a, &mut self.reg.f), // AND A, A
            0xA8 => ops::xor(&mut self.reg.a, b, &mut self.reg.f), // XOR A, B
            0xA9 => ops::xor(&mut self.reg.a, c, &mut self.reg.f), // XOR A, C
            0xAA => ops::xor(&mut self.reg.a, d, &mut self.reg.f), // XOR A, D
            0xAB => ops::xor(&mut self.reg.a, e, &mut self.reg.f), // XOR A, E
            0xAC => ops::xor(&mut self.reg.a, h, &mut self.reg.f), // XOR A, H
            0xAD => ops::xor(&mut self.reg.a, l, &mut self.reg.f), // XOR A, L
            0xAE => ops::xor_value_at_address(self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), // XOR A, (HL)
            0xAF => ops::xor(&mut self.reg.a, a, &mut self.reg.f), // XOR A, A
            0xB0 => ops::or(&mut self.reg.a, b, &mut self.reg.f), // OR A, B
            0xB1 => ops::or(&mut self.reg.a, c, &mut self.reg.f), // OR A, C
            0xB2 => ops::or(&mut self.reg.a, d, &mut self.reg.f), // OR A, D
            0xB3 => ops::or(&mut self.reg.a, e, &mut self.reg.f), // OR A, E
            0xB4 => ops::or(&mut self.reg.a, h, &mut self.reg.f), // OR A, H
            0xB5 => ops::or(&mut self.reg.a, l, &mut self.reg.f), // OR A, L
            0xB6 => ops::or_value_at_address(self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), // OR A, (HL)
            0xB7 => ops::or(&mut self.reg.a, a, &mut self.reg.f), // OR A, A
            0xB8 => ops::compare(&mut self.reg.a, b, &mut self.reg.f), // CP A, B
            0xB9 => ops::compare(&mut self.reg.a, c, &mut self.reg.f), // CP A, C
            0xBA => ops::compare(&mut self.reg.a, d, &mut self.reg.f), // CP A, D
            0xBB => ops::compare(&mut self.reg.a, e, &mut self.reg.f), // CP A, E
            0xBC => ops::compare(&mut self.reg.a, h, &mut self.reg.f), // CP A, H
            0xBD => ops::compare(&mut self.reg.a, l, &mut self.reg.f), // CP A, L
            0xBE => ops::compare_value_at_address(self.mem, &mut self.reg.a, pack_u16(h, l), &mut self.reg.f), // CP A, (HL)
            0xBF => ops::compare(&mut self.reg.a, a, &mut self.reg.f), // CP A, A
            0xC0 => ops::ret(self.mem, &mut self.reg.pc, &mut self.reg.sp, not_zero), // RET NZ
            0xC1 => ops::pop(self.mem, &mut self.reg.sp, &mut self.reg.b, &mut self.reg.c), // POP BC
            0xC2 => ops::jp_u16_immediate_if_true(self.mem, &mut self.reg.pc, not_zero), // JP NZ, nn
            0xC3 => ops::jp_u16_immediate(self.mem, &mut self.reg.pc), // JP nn
            0xC4 => ops::call_immediate_if_true(self.mem, &mut self.reg.pc, &mut self.reg.sp, not_zero), // CALL NZ, nn
            0xC5 => ops::push(self.mem, &mut self.reg.sp, pack_u16(b, c)), // PUSH BC 
            0xC6 => ops::add_u8_immediate(self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, false), // ADD A, n
            0xC7 => ops::call(self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x00), // RST 0
            0xC8 => ops::ret(self.mem, &mut self.reg.pc, &mut self.reg.sp, zero), // RET Z
            0xC9 => ops::ret(self.mem, &mut self.reg.pc, &mut self.reg.sp, true), // RET
            0xCA => ops::jp_u16_immediate_if_true(self.mem, &mut self.reg.pc, zero), // JP Z, nn
            0xCB => self.extended_opcodes(),
            0xCC => ops::call_immediate_if_true(self.mem, &mut self.reg.pc, &mut self.reg.sp, zero), // CALL Z, nn
            0xCD => ops::call_immediate_if_true(self.mem, &mut self.reg.pc, &mut self.reg.sp, true), // CALL nn
            0xCE => ops::add_u8_immediate(self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, true), // ADC A, n
            0xCF => ops::call(self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x08), // RST 8
            0xD0 => ops::ret(self.mem, &mut self.reg.pc, &mut self.reg.sp, no_carry), // RET NC
            0xD1 => ops::pop(self.mem, &mut self.reg.sp, &mut self.reg.d, &mut self.reg.e), // POP DE 
            0xD2 => ops::jp_u16_immediate_if_true(self.mem, &mut self.reg.pc, no_carry), // JP NC, nn
            0xD3 => error!("0xD3 should never be executed"), 
            0xD4 => ops::call_immediate_if_true(self.mem, &mut self.reg.pc, &mut self.reg.sp, no_carry), // CALL NC, nn
            0xD5 => ops::push(self.mem, &mut self.reg.sp, pack_u16(d, e)), // PUSH DE 
            0xD6 => ops::sub_u8_immediate(self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, false), // SUB A, n
            0xD7 => ops::call(self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x10), // RST 10
            0xD8 => ops::ret(self.mem, &mut self.reg.pc, &mut self.reg.sp, carry), // RET C
            0xD9 => ops::reti(self.mem, &mut self.reg.pc, &mut self.reg.sp, &mut self.reg.ime), // RETI
            0xDA => ops::jp_u16_immediate_if_true(self.mem, &mut self.reg.pc, carry), // JP C, nn
            0xDB => error!("0xDB should never be executed"),
            0xDC => ops::call_immediate_if_true(self.mem, &mut self.reg.pc, &mut self.reg.sp, carry), // CALL C, nn
            0xDD => error!("0xDD should never be executed"), 
            0xDE => ops::sub_u8_immediate(self.mem, &mut self.reg.pc, &mut self.reg.a, &mut self.reg.f, true), // SBC A, n
            0xDF => ops::call(self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x18), // RST 18
            0xE0 => return, // LDH (n), A
            0xE1 => ops::pop(self.mem, &mut self.reg.sp, &mut self.reg.h, &mut self.reg.l), // POP HL 
            0xE2 => return, // LDH (C), A
            0xE3 => error!("0xE3 should never be executed"), 
            0xE4 => error!("0xE4 should never be executed"), 
            0xE5 => ops::push(self.mem, &mut self.reg.sp, pack_u16(h, l)), // PUSH HL 
            0xE6 => ops::and(&mut self.reg.a, ops::u8_immediate(self.mem, &mut self.reg.pc), &mut self.reg.f), // AND A, n
            0xE7 => ops::call(self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x20), // RST 20
            0xE8 => return, // ADD SP, (signed)n
            0xE9 => ops::jp(&mut self.reg.pc, pack_u16(h,l)), // JP (HL)
            0xEA => return, // LD (nn), A 
            0xEB => error!("0xEB should never be executed"),
            0xEC => error!("0xEC should never be executed"),
            0xED => error!("0xED should never be executed"), 
            0xEE => ops::xor(&mut self.reg.a, ops::u8_immediate(self.mem, &mut self.reg.pc), &mut self.reg.f), // XOR n
            0xEF => ops::call(self.mem, &mut self.reg.pc, &mut self.reg.sp, 0x28), // RST 28
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
            0x00 => ops::rotate_left_with_carry(&mut self.reg.b, &mut self.reg.f), // RLC B
            0x01 => ops::rotate_left_with_carry(&mut self.reg.c, &mut self.reg.f), // RLC C
            0x02 => ops::rotate_left_with_carry(&mut self.reg.d, &mut self.reg.f), // RLC D
            0x03 => ops::rotate_left_with_carry(&mut self.reg.e, &mut self.reg.f), // RLC E
            0x04 => ops::rotate_left_with_carry(&mut self.reg.h, &mut self.reg.f), // RLC B
            0x05 => ops::rotate_left_with_carry(&mut self.reg.l, &mut self.reg.f), // RLC C
            0x06 => ops::rotate_left_with_carry_at_address(self.mem, pack_u16(h, l), &mut self.reg.f), // RLC (HL) 
            0x07 => ops::rotate_left_with_carry(&mut self.reg.a, &mut self.reg.f), // RLC E
            0x08 => ops::rotate_right_with_carry(&mut self.reg.b, &mut self.reg.f), // RRC B
            0x09 => ops::rotate_right_with_carry(&mut self.reg.c, &mut self.reg.f), // RRC C
            0x0A => ops::rotate_right_with_carry(&mut self.reg.d, &mut self.reg.f), // RRC D
            0x0B => ops::rotate_right_with_carry(&mut self.reg.e, &mut self.reg.f), // RRC E
            0x0C => ops::rotate_right_with_carry(&mut self.reg.h, &mut self.reg.f), // RRC B
            0x0D => ops::rotate_right_with_carry(&mut self.reg.l, &mut self.reg.f), // RRC C
            0x0E => ops::rotate_right_with_carry_at_address(self.mem, pack_u16(h, l), &mut self.reg.f), // RRC (HL) 
            0x0F => ops::rotate_right_with_carry(&mut self.reg.a, &mut self.reg.f), // RRC E

            0x10 => ops::rotate_left(&mut self.reg.b, &mut self.reg.f), // RL B
            0x11 => ops::rotate_left(&mut self.reg.c, &mut self.reg.f), // RL C
            0x12 => ops::rotate_left(&mut self.reg.d, &mut self.reg.f), // RL D
            0x13 => ops::rotate_left(&mut self.reg.e, &mut self.reg.f), // RL E
            0x14 => ops::rotate_left(&mut self.reg.h, &mut self.reg.f), // RL B
            0x15 => ops::rotate_left(&mut self.reg.l, &mut self.reg.f), // RL C
            0x16 => ops::rotate_left_at_address(self.mem, pack_u16(h, l), &mut self.reg.f), // RL (HL) 
            0x17 => ops::rotate_left(&mut self.reg.a, &mut self.reg.f), // RL E
            0x18 => ops::rotate_right(&mut self.reg.b, &mut self.reg.f), // RR B
            0x19 => ops::rotate_right(&mut self.reg.c, &mut self.reg.f), // RR C
            0x1A => ops::rotate_right(&mut self.reg.d, &mut self.reg.f), // RR D
            0x1B => ops::rotate_right(&mut self.reg.e, &mut self.reg.f), // RR E
            0x1C => ops::rotate_right(&mut self.reg.h, &mut self.reg.f), // RR B
            0x1D => ops::rotate_right(&mut self.reg.l, &mut self.reg.f), // RR C
            0x1E => ops::rotate_right_at_address(self.mem, pack_u16(h, l), &mut self.reg.f), // RR (HL) 
            0x1F => ops::rotate_right(&mut self.reg.a, &mut self.reg.f), // RR E

            0x20 => ops::sla(&mut self.reg.b, &mut self.reg.f), // RL B
            0x21 => ops::sla(&mut self.reg.c, &mut self.reg.f), // RL C
            0x22 => ops::sla(&mut self.reg.d, &mut self.reg.f), // RL D
            0x23 => ops::sla(&mut self.reg.e, &mut self.reg.f), // RL E
            0x24 => ops::sla(&mut self.reg.h, &mut self.reg.f), // RL B
            0x25 => ops::sla(&mut self.reg.l, &mut self.reg.f), // RL C
            0x26 => ops::sla_at_address(self.mem, pack_u16(h, l), &mut self.reg.f), // RL (HL) 
            0x27 => ops::sla(&mut self.reg.a, &mut self.reg.f), // RL E
            0x28 => ops::sra(&mut self.reg.b, &mut self.reg.f), // RR B
            0x29 => ops::sra(&mut self.reg.c, &mut self.reg.f), // RR C
            0x2A => ops::sra(&mut self.reg.d, &mut self.reg.f), // RR D
            0x2B => ops::sra(&mut self.reg.e, &mut self.reg.f), // RR E
            0x2C => ops::sra(&mut self.reg.h, &mut self.reg.f), // RR B
            0x2D => ops::sra(&mut self.reg.l, &mut self.reg.f), // RR C
            0x2E => ops::sra_at_address(self.mem, pack_u16(h, l), &mut self.reg.f), // RR (HL) 
            0x2F => ops::sra(&mut self.reg.a, &mut self.reg.f), // RR E

            0x40 => ops::bit(b, 0, &mut self.reg.f), // BIT 0, B
            0x41 => ops::bit(c, 0, &mut self.reg.f), // BIT 0, C
            0x42 => ops::bit(d, 0, &mut self.reg.f), // BIT 0, D
            0x43 => ops::bit(e, 0, &mut self.reg.f), // BIT 0, E
            0x44 => ops::bit(h, 0, &mut self.reg.f), // BIT 0, H
            0x45 => ops::bit(l, 0, &mut self.reg.f), // BIT 0, L
            0x46 => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 0, &mut self.reg.f), // BIT 0, (HL)
            0x47 => ops::bit(a, 1, &mut self.reg.f), // BIT 1, A
            0x48 => ops::bit(b, 1, &mut self.reg.f), // BIT 1, B
            0x49 => ops::bit(c, 1, &mut self.reg.f), // BIT 1, C
            0x4A => ops::bit(d, 1, &mut self.reg.f), // BIT 1, D
            0x4B => ops::bit(e, 1, &mut self.reg.f), // BIT 1, E
            0x4C => ops::bit(h, 1, &mut self.reg.f), // BIT 1, H
            0x4D => ops::bit(l, 1, &mut self.reg.f), // BIT 1, L
            0x4E => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 1, &mut self.reg.f), // BIT 1, (HL)
            0x4F => ops::bit(a, 1, &mut self.reg.f), // BIT 1, A

            0x50 => ops::bit(b, 2, &mut self.reg.f), // BIT 2, B
            0x51 => ops::bit(c, 2, &mut self.reg.f), // BIT 2, C
            0x52 => ops::bit(d, 2, &mut self.reg.f), // BIT 2, D
            0x53 => ops::bit(e, 2, &mut self.reg.f), // BIT 2, E
            0x54 => ops::bit(h, 2, &mut self.reg.f), // BIT 2, H
            0x55 => ops::bit(l, 2, &mut self.reg.f), // BIT 2, L
            0x56 => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 2, &mut self.reg.f), // BIT 2, (HL)
            0x57 => ops::bit(a, 3, &mut self.reg.f), // BIT 3, A
            0x58 => ops::bit(b, 3, &mut self.reg.f), // BIT 3, B
            0x59 => ops::bit(c, 3, &mut self.reg.f), // BIT 3, C
            0x5A => ops::bit(d, 3, &mut self.reg.f), // BIT 3, D
            0x5B => ops::bit(e, 3, &mut self.reg.f), // BIT 3, E
            0x5C => ops::bit(h, 3, &mut self.reg.f), // BIT 3, H
            0x5D => ops::bit(l, 3, &mut self.reg.f), // BIT 3, L
            0x5E => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 3, &mut self.reg.f), // BIT 3, (HL)
            0x5F => ops::bit(a, 3, &mut self.reg.f), // BIT 3, A

            0x60 => ops::bit(b, 4, &mut self.reg.f), // BIT 4, B
            0x61 => ops::bit(c, 4, &mut self.reg.f), // BIT 4, C
            0x62 => ops::bit(d, 4, &mut self.reg.f), // BIT 4, D
            0x63 => ops::bit(e, 4, &mut self.reg.f), // BIT 4, E
            0x64 => ops::bit(h, 4, &mut self.reg.f), // BIT 4, H
            0x65 => ops::bit(l, 4, &mut self.reg.f), // BIT 4, L
            0x66 => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 4, &mut self.reg.f), // BIT 4, (HL)
            0x67 => ops::bit(a, 5, &mut self.reg.f), // BIT 5, A
            0x68 => ops::bit(b, 5, &mut self.reg.f), // BIT 5, B
            0x69 => ops::bit(c, 5, &mut self.reg.f), // BIT 5, C
            0x6A => ops::bit(d, 5, &mut self.reg.f), // BIT 5, D
            0x6B => ops::bit(e, 5, &mut self.reg.f), // BIT 5, E
            0x6C => ops::bit(h, 5, &mut self.reg.f), // BIT 5, H
            0x6D => ops::bit(l, 5, &mut self.reg.f), // BIT 5, L
            0x6E => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 5, &mut self.reg.f), // BIT 5, (HL)
            0x6F => ops::bit(a, 5, &mut self.reg.f), // BIT 5, A

            0x70 => ops::bit(b, 6, &mut self.reg.f), // BIT 6, B
            0x71 => ops::bit(c, 6, &mut self.reg.f), // BIT 6, C
            0x72 => ops::bit(d, 6, &mut self.reg.f), // BIT 6, D
            0x73 => ops::bit(e, 6, &mut self.reg.f), // BIT 6, E
            0x74 => ops::bit(h, 6, &mut self.reg.f), // BIT 6, H
            0x75 => ops::bit(l, 6, &mut self.reg.f), // BIT 6, L
            0x76 => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 6, &mut self.reg.f), // BIT 6, (HL)
            0x77 => ops::bit(a, 7, &mut self.reg.f), // BIT 7, A
            0x78 => ops::bit(b, 7, &mut self.reg.f), // BIT 7, B
            0x79 => ops::bit(c, 7, &mut self.reg.f), // BIT 7, C
            0x7A => ops::bit(d, 7, &mut self.reg.f), // BIT 7, D
            0x7B => ops::bit(e, 7, &mut self.reg.f), // BIT 7, E
            0x7C => ops::bit(h, 7, &mut self.reg.f), // BIT 7, H
            0x7D => ops::bit(l, 7, &mut self.reg.f), // BIT 7, L
            0x7E => ops::bit(ops::byte_at_address(self.mem, pack_u16(h,l)), 7, &mut self.reg.f), // BIT 7, (HL)
            0x7F => ops::bit(a, 7, &mut self.reg.f), // BIT 7, A

            0x80 => ops::res(&mut self.reg.b, 0), // RES 0, B
            0x81 => ops::res(&mut self.reg.c, 0), // RES 0, C
            0x82 => ops::res(&mut self.reg.d, 0), // RES 0, D
            0x83 => ops::res(&mut self.reg.e, 0), // RES 0, E
            0x84 => ops::res(&mut self.reg.h, 0), // RES 0, H
            0x85 => ops::res(&mut self.reg.l, 0), // RES 0, L
            0x86 => ops::res_at_addr(self.mem, pack_u16(h,l), 0), // RES 0, (HL)
            0x87 => ops::res(&mut self.reg.a, 0), // RES 0, A
            0x88 => ops::res(&mut self.reg.b, 1), // RES 1, B
            0x89 => ops::res(&mut self.reg.c, 1), // RES 1, C
            0x8A => ops::res(&mut self.reg.d, 1), // RES 1, D
            0x8B => ops::res(&mut self.reg.e, 1), // RES 1, E
            0x8C => ops::res(&mut self.reg.h, 1), // RES 1, H
            0x8D => ops::res(&mut self.reg.l, 1), // RES 1, L
            0x8E => ops::res_at_addr(self.mem, pack_u16(h,l), 1), // RES 1, (HL)
            0x8F => ops::res(&mut self.reg.a, 1), // RES 1, A

            0x90 => ops::res(&mut self.reg.b, 2), // RES 2, B
            0x91 => ops::res(&mut self.reg.c, 2), // RES 2, C
            0x92 => ops::res(&mut self.reg.d, 2), // RES 2, D
            0x93 => ops::res(&mut self.reg.e, 2), // RES 2, E
            0x94 => ops::res(&mut self.reg.h, 2), // RES 2, H
            0x95 => ops::res(&mut self.reg.l, 2), // RES 2, L
            0x96 => ops::res_at_addr(self.mem, pack_u16(h,l), 2), // RES 2, (HL)
            0x97 => ops::res(&mut self.reg.a, 2), // RES 2, A
            0x98 => ops::res(&mut self.reg.b, 3), // RES 3, B
            0x99 => ops::res(&mut self.reg.c, 3), // RES 3, C
            0x9A => ops::res(&mut self.reg.d, 3), // RES 3, D
            0x9B => ops::res(&mut self.reg.e, 3), // RES 3, E
            0x9C => ops::res(&mut self.reg.h, 3), // RES 3, H
            0x9D => ops::res(&mut self.reg.l, 3), // RES 3, L
            0x9E => ops::res_at_addr(self.mem, pack_u16(h,l), 3), // RES 3, (HL)
            0x9F => ops::res(&mut self.reg.a, 3), // RES 3, A
            
            0xA0 => ops::res(&mut self.reg.b, 4), // RES 4, B
            0xA1 => ops::res(&mut self.reg.c, 4), // RES 4, C
            0xA2 => ops::res(&mut self.reg.d, 4), // RES 4, D
            0xA3 => ops::res(&mut self.reg.e, 4), // RES 4, E
            0xA4 => ops::res(&mut self.reg.h, 4), // RES 4, H
            0xA5 => ops::res(&mut self.reg.l, 4), // RES 4, L
            0xA6 => ops::res_at_addr(self.mem, pack_u16(h,l), 4), // RES 4, (HL)
            0xA7 => ops::res(&mut self.reg.a, 4), // RES 4, A
            0xA8 => ops::res(&mut self.reg.b, 5), // RES 5, B
            0xA9 => ops::res(&mut self.reg.c, 5), // RES 5, C
            0xAA => ops::res(&mut self.reg.d, 5), // RES 5, D
            0xAB => ops::res(&mut self.reg.e, 5), // RES 5, E
            0xAC => ops::res(&mut self.reg.h, 5), // RES 5, H
            0xAD => ops::res(&mut self.reg.l, 5), // RES 5, L
            0xAE => ops::res_at_addr(self.mem, pack_u16(h,l), 5), // RES 5, (HL)
            0xAF => ops::res(&mut self.reg.a, 5), // RES 5, A
 
            0xB0 => ops::res(&mut self.reg.b, 6), // RES 6, B
            0xB1 => ops::res(&mut self.reg.c, 6), // RES 6, C
            0xB2 => ops::res(&mut self.reg.d, 6), // RES 6, D
            0xB3 => ops::res(&mut self.reg.e, 6), // RES 6, E
            0xB4 => ops::res(&mut self.reg.h, 6), // RES 6, H
            0xB5 => ops::res(&mut self.reg.l, 6), // RES 6, L
            0xB6 => ops::res_at_addr(self.mem, pack_u16(h,l), 6), // RES 6, (HL)
            0xB7 => ops::res(&mut self.reg.a, 6), // RES 6, A
            0xB8 => ops::res(&mut self.reg.b, 7), // RES 7, B
            0xB9 => ops::res(&mut self.reg.c, 7), // RES 7, C
            0xBA => ops::res(&mut self.reg.d, 7), // RES 7, D
            0xBB => ops::res(&mut self.reg.e, 7), // RES 7, E
            0xBC => ops::res(&mut self.reg.h, 7), // RES 7, H
            0xBD => ops::res(&mut self.reg.l, 7), // RES 7, L
            0xBE => ops::res_at_addr(self.mem, pack_u16(h,l), 7), // RES 7, (HL)
            0xBF => ops::res(&mut self.reg.a, 7), // RES 7, A

            0xC0 => ops::set(&mut self.reg.b, 0), // SET 0, B
            0xC1 => ops::set(&mut self.reg.c, 0), // SET 0, C
            0xC2 => ops::set(&mut self.reg.d, 0), // SET 0, D
            0xC3 => ops::set(&mut self.reg.e, 0), // SET 0, E
            0xC4 => ops::set(&mut self.reg.h, 0), // SET 0, H
            0xC5 => ops::set(&mut self.reg.l, 0), // SET 0, L
            0xC6 => ops::set_at_addr(self.mem, pack_u16(h,l), 0), // SET 0, (HL)
            0xC7 => ops::set(&mut self.reg.a, 0), // SET 0, A
            0xC8 => ops::set(&mut self.reg.b, 1), // SET 1, B
            0xC9 => ops::set(&mut self.reg.c, 1), // SET 1, C
            0xCA => ops::set(&mut self.reg.d, 1), // SET 1, D
            0xCB => ops::set(&mut self.reg.e, 1), // SET 1, E
            0xCC => ops::set(&mut self.reg.h, 1), // SET 1, H
            0xCD => ops::set(&mut self.reg.l, 1), // SET 1, L
            0xCE => ops::set_at_addr(self.mem, pack_u16(h,l), 1), // SET 1, (HL)
            0xCF => ops::set(&mut self.reg.a, 1), // SET 1, A

            0xD0 => ops::set(&mut self.reg.b, 2), // SET 2, B
            0xD1 => ops::set(&mut self.reg.c, 2), // SET 2, C
            0xD2 => ops::set(&mut self.reg.d, 2), // SET 2, D
            0xD3 => ops::set(&mut self.reg.e, 2), // SET 2, E
            0xD4 => ops::set(&mut self.reg.h, 2), // SET 2, H
            0xD5 => ops::set(&mut self.reg.l, 2), // SET 2, L
            0xD6 => ops::set_at_addr(self.mem, pack_u16(h,l), 2), // SET 2, (HL)
            0xD7 => ops::set(&mut self.reg.a, 2), // SET 2, A
            0xD8 => ops::set(&mut self.reg.b, 3), // SET 3, B
            0xD9 => ops::set(&mut self.reg.c, 3), // SET 3, C
            0xDA => ops::set(&mut self.reg.d, 3), // SET 3, D
            0xDB => ops::set(&mut self.reg.e, 3), // SET 3, E
            0xDC => ops::set(&mut self.reg.h, 3), // SET 3, H
            0xDD => ops::set(&mut self.reg.l, 3), // SET 3, L
            0xDE => ops::set_at_addr(self.mem, pack_u16(h,l), 3), // SET 3, (HL)
            0xDF => ops::set(&mut self.reg.a, 3), // SET 3, A
            
            0xE0 => ops::set(&mut self.reg.b, 4), // SET 4, B
            0xE1 => ops::set(&mut self.reg.c, 4), // SET 4, C
            0xE2 => ops::set(&mut self.reg.d, 4), // SET 4, D
            0xE3 => ops::set(&mut self.reg.e, 4), // SET 4, E
            0xE4 => ops::set(&mut self.reg.h, 4), // SET 4, H
            0xE5 => ops::set(&mut self.reg.l, 4), // SET 4, L
            0xE6 => ops::set_at_addr(self.mem, pack_u16(h,l), 4), // SET 4, (HL)
            0xE7 => ops::set(&mut self.reg.a, 4), // SET 4, A
            0xE8 => ops::set(&mut self.reg.b, 5), // SET 5, B
            0xE9 => ops::set(&mut self.reg.c, 5), // SET 5, C
            0xEA => ops::set(&mut self.reg.d, 5), // SET 5, D
            0xEB => ops::set(&mut self.reg.e, 5), // SET 5, E
            0xEC => ops::set(&mut self.reg.h, 5), // SET 5, H
            0xED => ops::set(&mut self.reg.l, 5), // SET 5, L
            0xEE => ops::set_at_addr(self.mem, pack_u16(h,l), 5), // SET 5, (HL)
            0xEF => ops::set(&mut self.reg.a, 5), // SET 5, A
 
            0xF0 => ops::set(&mut self.reg.b, 6), // SET 6, B
            0xF1 => ops::set(&mut self.reg.c, 6), // SET 6, C
            0xF2 => ops::set(&mut self.reg.d, 6), // SET 6, D
            0xF3 => ops::set(&mut self.reg.e, 6), // SET 6, E
            0xF4 => ops::set(&mut self.reg.h, 6), // SET 6, H
            0xF5 => ops::set(&mut self.reg.l, 6), // SET 6, L
            0xF6 => ops::set_at_addr(self.mem, pack_u16(h,l), 6), // SET 6, (HL)
            0xF7 => ops::set(&mut self.reg.a, 6), // SET 6, A
            0xF8 => ops::set(&mut self.reg.b, 7), // SET 7, B
            0xF9 => ops::set(&mut self.reg.c, 7), // SET 7, C
            0xFA => ops::set(&mut self.reg.d, 7), // SET 7, D
            0xFB => ops::set(&mut self.reg.e, 7), // SET 7, E
            0xFC => ops::set(&mut self.reg.h, 7), // SET 7, H
            0xFD => ops::set(&mut self.reg.l, 7), // SET 7, L
            0xFE => ops::set_at_addr(self.mem, pack_u16(h,l), 7), // SET 7, (HL)
            0xFF => ops::set(&mut self.reg.a, 7), // SET 7, A
            
            _ => return
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
