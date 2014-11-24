#![feature(tuple_indexing)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;

use memory::Memory;
use memory::GameboyMemory;
use cpu::Cpu;

mod memory;
mod ops;
mod extensions;
mod cpu;

fn main() {
    let mut memory = box GameboyMemory::new(0x10000);
    let mut cpu = Cpu::new(memory);

    for n in range(0 as int,10) {
        let clock_cycle = cpu.tick();
    }
   
}
