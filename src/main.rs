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
    let mut memory = Memory::new(65536);
    let mut cpu = Cpu::new(&mut memory);

    loop {
        let clock_cycle = cpu.tick();
    }
   
}
