use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};
use ops::utils::{u8_immediate, u16_immediate};
use ops::{increment_register_pair, decrement_register_pair};

/// Load the value from one register into another
pub fn ld_reg_to_reg(target: &mut Register<u8>, source: &Register<u8>) {
    debug!("ld_reg_to_reg: {:X}", source.read());
    let val = source.read();
    target.write(val);
}

/// Loads the memory pointed to by the next two bytes into a register
pub fn ld_u8_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>) {
    let val = u8_immediate(mem, pc);
    debug!("ld_next_byte_to_reg: {:X}", val);
    reg.write(val);
}

/// Loads the next two bytes into the passed in registers
pub fn ld_u16_immediate(mem: &Memory, pc: &mut Register<u16>,
                                 hb: &mut Register<u8>, lb: &mut Register<u8>) {
    let val = u16_immediate(mem, pc);
    debug!("ld_next_two_bytes_into_reg_pair: {:X}", val);
    hb.write(high_byte(val));
    lb.write(low_byte(val));
}

pub fn ld_from_address(mem: &Memory, reg: &mut Register<u8>, addr: u16) {
    let val = mem.read_byte(addr);
    reg.write(val);
}

pub fn write_value_to_memory_at_address_and_increment_register(mem: &mut Memory, val: u8, high_reg: &mut Register<u8>, low_reg: &mut Register<u8>) {
    let address = pack_u16(high_reg.read(), low_reg.read());
    mem.write_byte(address, val);
    let new_address = address + 1;
    high_reg.write(high_byte(new_address));
    low_reg.write(low_byte(new_address));
}

pub fn write_value_to_memory_at_address_and_decrement_register(mem: &mut Memory, val: u8, high_reg: &mut Register<u8>, low_reg: &mut Register<u8>) {
    let address = pack_u16(high_reg.read(), low_reg.read());
    mem.write_byte(address, val);
    let new_address = address - 1;
    high_reg.write(high_byte(new_address));
    low_reg.write(low_byte(new_address));
}

pub fn ld_from_address_pointed_to_by_register_pair_and_increment_register_pair(mem: &Memory, reg: &mut Register<u8>, high_byte: &mut Register<u8>, low_byte: &mut Register<u8>) {
   let address = pack_u16(high_byte.read(), low_byte.read());

   let val = mem.read_byte(address);
   reg.write(val);
   increment_register_pair(high_byte, low_byte);
   debug!("ld from address and inc reg pair - addr: {:X} val: {:X} reg_pair_val: {:X}", address, val, pack_u16(high_byte.read(), low_byte.read()));
}

pub fn ld_u8_immediate_into_address(mem: &mut Memory, pc: &mut Register<u16>, hb: u8, lb: u8) {
    let addr = pack_u16(hb, lb);
    let val = u8_immediate(mem, pc);
    mem.write_byte(addr, val);
}

pub fn ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair(mem: &Memory, reg: &mut Register<u8>, high_byte: &mut Register<u8>, low_byte: &mut Register<u8>) {
   let address = pack_u16(high_byte.read(), low_byte.read());

   let val = mem.read_byte(address);

   reg.write(val);
   decrement_register_pair(high_byte, low_byte);
}

/// Loads the next two bytes into the passed in register (sp)
pub fn ld_next_two_bytes_into_reg(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u16>) {
    let val = u16_immediate(mem, pc);
    debug!("ld_next_two_bytes_into_reg: {:X}", val);
    reg.write(val);
}

pub fn load_val_FF00_plus_immediate(mem: &Memory, pc: &mut Register<u16>, reg: &mut Register<u8>) {
    let lb = u8_immediate(mem, pc);
    let addr = 0xFF00 + lb as u16;
    let val = mem.read_byte(addr);
    debug!("load val: {:X} into a", val);
    reg.write(val);
}

pub fn write_value_to_u16_immediate(mem: &mut Memory, pc: &mut Register<u16>, val: u8) {
    let addr = u16_immediate(mem, pc);
    debug!("write value to u16 immediate val: {:X} addr: {:X}", val, addr);
    mem.write_byte(addr, val);
}

pub fn write_val_FF00_plus_immediate(mem: &mut Memory, pc: &mut Register<u16>, val: u8) {
    let lb = u8_immediate(mem, pc);
    let addr = 0xFF00 + lb as u16;
    debug!("write val: {:X} to addr: {:X}", val, addr);
    mem.write_byte(addr, val);
}

/// Write sp to address with value of next two bytes
pub fn write_u16_immediate_address(mem: &mut Memory, pc: &mut Register<u16>, val: u16){
    let addr = u16_immediate(mem, pc);

    debug!("Writing {} to {}", val, addr);
    mem.write_word(addr, val);
}

pub fn ld_u8(reg: &mut Register<u8>, val: u8) {
    debug!("load val: {:X} into register", val);
    reg.write(val);
}

/// Writes the passed in value to memory at the address pointed to by combined address parameters
pub fn write_value_to_memory_at_address(mem: &mut Memory, val: u8, addr_msb: u8, addr_lsb: u8) {
   let addr = pack_u16(addr_msb, addr_lsb);
   debug!("write val to mem addr: {:X} val: {:X}", addr, val);
   mem.write_byte(addr as u16, val);
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
    use extensions::Incrementor;
    use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

    #[test]
    fn test_write_val_FF00_plus_immediate() {
    }

    #[test]
    fn test_write_value_to_u16_immediate() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let addr = 0x8284;
        let val = 0x92;
        let mut pc = Register::new(0x1234);

        mem.write_word(0x1234, addr);

        write_value_to_u16_immediate(&mut mem, &mut pc, val);
        assert!(pc.read() == 0x1236);
        assert!(mem.read_byte(addr) == val);
    }

    #[test]
    fn test_load_val_FF00_plus_immediate() {
    }

    #[test]
    fn test_ld_u8() {
        let mut reg = Register::new(10);

        ld_u8(&mut reg, 0x18);

        assert!(reg.read() == 0x18);
    }
    #[test]
    fn test_ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut reg = Register::new(0x12);
        let mut high_byte = Register::new(0xAB);
        let mut low_byte = Register::new(0xCD);

        mem.write_byte(0xABCD, 0x54);
        ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair(&mem, &mut reg, &mut high_byte, &mut low_byte);

        assert!(reg.read() == 0x54);
        assert!(low_byte.read() == 0xCC);
    }

    #[test]
    fn test_ld_u8_immediate_into_address() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut pc = Register::new(0xAD12);
        mem.write_byte(0xAD12, 0xBB);

        ld_u8_immediate_into_address(&mut mem, &mut pc, 0x12, 0x34);

        assert!(mem.read_byte(0x1234) == 0xBB);
        assert!(pc.read() == 0xAD13);
    }

    #[test]
    fn test_ld_next_two_bytes_into_reg() {
        let mut mem = EmptyMemory::new(65536);
        let mut pc = Register::new(11);
        let mut reg = Register::new(0);

        mem.write_word(11, 0xDEAB);

        ld_next_two_bytes_into_reg(&mem, &mut pc, &mut reg);
        assert!(pc.read() == 13);
        assert!(reg.read() == 0xDEAB);
    }

    #[test]
    fn test_ld_from_address_pointed_to_by_register_pair_and_increment_register_pair() {
        let mut mem = EmptyMemory::new(0xFFFF);
        let mut reg = Register::new(0x12);
        let mut high_byte = Register::new(0xAB);
        let mut low_byte = Register::new(0xCD);

        mem.write_byte(0xABCD, 0x54);
        ld_from_address_pointed_to_by_register_pair_and_increment_register_pair(&mem, &mut reg, &mut high_byte, &mut low_byte);

        assert!(reg.read() == 0x54);
        assert!(low_byte.read() == 0xCE);
    }

    #[test]
    fn test_ld_from_reg_pair_as_address() {
        let mut mem = EmptyMemory::new(65000);
        let mut rega = Register::new(0x00);

        let mut reg1 = Register::new(0x12);
        let mut reg2 = Register::new(0x34);

        mem.write_byte(0x1234, 0xAA);

        ld_from_address(&mem, &mut rega, pack_u16(reg1.read(), reg2.read()));

        assert!(rega.read() == 0xAA);
    }


    #[test]
    fn test_ld_u8_immediate() {
        let mut mem = EmptyMemory::new(65536);
        let mut pc = Register::new(11);
        let mut reg = Register::new(0);

        mem.write_byte(11, 0xFA);

        ld_u8_immediate(&mem, &mut pc, &mut reg);
        assert!(reg.read() == 0xFA);
        assert!(pc.read() == 12);

    }

    #[test]
    fn test_ld_reg_to_reg() {
        let mut target = Register::new(5);
        let mut source = Register::new(10);

        ld_reg_to_reg(&mut target, &source);

        assert!(target.read() == 10);
        assert!(source.read() == 10);
    }

    #[test]
    fn test_ld_u16_immediate() {
        let mut mem = EmptyMemory::new(65536);
        let mut pc = Register::new(11);
        let mut reg = Register::new(0);
        let mut reg2 = Register::new(0);

        mem.write_word(11, 0xDEAB);

        ld_u16_immediate(&mem, &mut pc, &mut reg, &mut reg2);
        assert!(pc.read() == 13);
        assert!(reg.read() == 0xDE);
        assert!(reg2.read() == 0xAB);
    }

    #[test]
    fn test_write_value_to_memory_at_address() {
        let mut mem = EmptyMemory::new(65536);
        let mut msb = 0xFF;
        let mut lsb = 0x11;
        let val = 100;

        write_value_to_memory_at_address(&mut mem, val, msb, lsb);

        assert!(mem.read_byte(0xFF11) == val, "Memory does match what was written");
    }
}
