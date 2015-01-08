use memory::{EmptyMemory, Memory, pack_u16, high_byte, low_byte, low_nibble, high_nibble};
use extensions::Incrementor;
use cpu::{Register, Flags, CarryFlag, HalfCarryFlag, ZeroFlag, SubtractFlag};

pub use self::add::adc;
pub use self::add::add;
pub use self::add::add_value_at_address;
pub use self::add::add_u8_immediate;
pub use self::add::adc_value_at_address;
pub use self::add::add_register_pair_to_register_pair;
pub use self::add::increment_register;
pub use self::add::increment_register_pair;
pub use self::add::increment_value_at_address;

pub use self::boolean::and;
pub use self::boolean::and_value_at_address;
pub use self::boolean::complement;
pub use self::boolean::or;
pub use self::boolean::or_value_at_address;
pub use self::boolean::xor;
pub use self::boolean::xor_value_at_address;

pub use self::jp::jp;
pub use self::jp::jp_u16_immediate;
pub use self::jp::jp_u16_immediate_if_true;
pub use self::jp::relative_jmp_by_signed_immediate_if_true;
pub use self::jp::jump_by_signed_immediate;

pub use self::ld::ld_reg_to_reg;
pub use self::ld::ld_u8_immediate;
pub use self::ld::ld_u8;
pub use self::ld::ld_u8_immediate_into_address;
pub use self::ld::ld_u16_immediate;
pub use self::ld::write_u16_immediate_address;
pub use self::ld::ld_from_address;
pub use self::ld::write_val_FF00_plus_immediate;
pub use self::ld::load_val_FF00_plus_immediate;
pub use self::ld::write_value_to_u16_immediate;
pub use self::ld:: write_value_to_memory_at_address;
pub use self::ld::ld_next_two_bytes_into_reg;
pub use self::ld::write_value_to_memory_at_address_and_decrement_register;
pub use self::ld::write_value_to_memory_at_address_and_increment_register;
pub use self::ld::ld_from_address_pointed_to_by_register_pair_and_increment_register_pair;
pub use self::ld::ld_from_address_pointed_to_by_register_pair_and_decrement_register_pair;

pub use self::shift::sra;
pub use self::shift::sra_at_address;
pub use self::shift::sla;
pub use self::shift::sla_at_address;
pub use self::shift::srl;
pub use self::shift::srl_at_address;

pub use self::special::bit;
pub use self::special::byte_at_address;
pub use self::special::call;
pub use self::special::call_immediate_if_true;
pub use self::special::ccf;
pub use self::special::disable_interrupts;
pub use self::special::nop;
pub use self::special::pop;
pub use self::special::pop_flags;
pub use self::special::push;
pub use self::special::res;
pub use self::special::res_at_addr;
pub use self::special::ret;
pub use self::special::reti;
pub use self::special::set;
pub use self::special::set_at_addr;
pub use self::special::set_flag;
pub use self::special::swap;
pub use self::special::swap_at_address;

pub use self::sub::sbc;
pub use self::sub::sub_value_at_address;
pub use self::sub::sub;
pub use self::sub::sub_u8_immediate;
pub use self::sub::sbc_value_at_address;
pub use self::sub::compare;
pub use self::sub::compare_value_at_address;
pub use self::sub::decrement_register;
pub use self::sub::decrement_register_pair;
pub use self::sub::decrement_value_at_address;

pub use self::rotate::rotate_right;
pub use self::rotate::rotate_right_at_address;
pub use self::rotate::rotate_right_with_carry;
pub use self::rotate::rotate_right_with_carry_at_address;
pub use self::rotate::rotate_right_reset_zeroflag;
pub use self::rotate::rotate_right_with_carry_reset_zero;
pub use self::rotate::rotate_left;
pub use self::rotate::rotate_left_at_address;
pub use self::rotate::rotate_left_with_carry;
pub use self::rotate::rotate_left_with_carry_at_address;
pub use self::rotate::rotate_left_reset_zeroflag;
pub use self::rotate::rotate_left_with_carry_reset_zero;

pub use self::utils::u8_immediate;
pub use self::utils::u16_immediate;

mod add;
mod boolean;
mod jp;
mod ld;
mod shift;
mod special;
mod sub;
mod rotate;
mod utils;

// TODO: Rewrite tests to exervise all values, use bigger num sizes to detent overflow and other
// things another way?
// TODO: Split opcodes into separate files ops::add::func
// TODO: Implement functions on types instead of doing logic in method? eg u8.rotate_right(carry)
