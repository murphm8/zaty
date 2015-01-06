#![feature(phase)]
#[phase(plugin, link)] extern crate log;
extern crate zaty;

use zaty::memory::Memory;
use zaty::memory::GameboyMemory;
use zaty::cpu::Cpu;
use std::io::stdio::stdin;

fn main() {
    let mut memory = box GameboyMemory::new(0x10000);
    let mut cpu = Cpu::new(memory);

    let mut count: uint = 0;
    let mut handle = stdin();
    loop {
        debug!("cycle: {}", count);
        /*
        if count % 10 == 0 {
            let val = handle.read_char();
        }
       */
        let clock_cycle = cpu.tick();
        count += 1;
    }

}
