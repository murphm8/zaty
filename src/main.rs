#![feature(phase)]
#[phase(plugin, link)] extern crate log;
extern crate num;

use std::cell::Cell;
use num::integer::Integer;
use memory::Memory;
use memory::low_nibble;
use memory::high_nibble;
use extensions::Incrementor;
use cpu::Cpu;

mod memory;
mod ops;
mod extensions;
mod cpu;

fn main() {
    let mut memory = Memory::new();
    let mut cpu = Cpu::new(memory);

    while true
    {
        let clock_cycle = cpu.tick();
    }
}



