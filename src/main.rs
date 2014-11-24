#![feature(tuple_indexing)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;

use memory::Memory;
use cpu::Cpu;

mod memory;
mod ops;
mod extensions;
mod cpu;

fn main() {
    let mut memory = Memory::new(0xFFFF + 1);
    let mut cpu = Cpu::new(&mut memory);

    for n in range(0 as int,10) {
        let clock_cycle = cpu.tick();
    }
   
}
