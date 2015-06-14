#![feature(box_syntax)]
#![feature(negate_unsigned)]
#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate num;
extern crate rand;

mod ops;
mod extensions;
mod memory;
mod cpu;

use memory::GameboyMemory;
use cpu::Cpu;

fn main() {
    let mut memory = Box::new(GameboyMemory::new(0x10000));
    let mut cpu = Cpu::new(memory);

    let mut count: u64 = 0;
//    let mut handle = stdin();
    loop {
        debug!("cycle: {}", count);
        /*
        if count % 10 == 0 {
//            let val = handle.read_char();
        }
       */
        let clock_cycle = cpu.tick();
        count += 1;
    }

}
