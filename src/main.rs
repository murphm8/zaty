#![feature(tuple_indexing)]
#![feature(struct_inherit)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;
extern crate num;

use memory::Memory;
use cpu::Cpu;

mod memory;
mod ops;
mod extensions;
mod cpu;

fn main() {
    let memory = Memory::new(65536);
    let mut cpu = Cpu::new(memory);

    loop {
        let clock_cycle = cpu.tick();
    }
}
