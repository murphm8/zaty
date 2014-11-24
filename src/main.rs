#![feature(tuple_indexing)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;

use memory::Memory;
use memory::GameboyMemory;
use cpu::Cpu;
use std::io::stdio::stdin;

mod memory;
mod ops;
mod extensions;
mod cpu;

fn main() {
    let mut memory = box GameboyMemory::new(0x10000);
    let mut cpu = Cpu::new(memory);
    
    let mut count: uint = 0;
    let mut handle = stdin();
    loop {
        /* 
        if count % 10 == 0 {
            let val = handle.read_char();
        }
       */
        let clock_cycle = cpu.tick();
        count += 1;
    }
   
}
