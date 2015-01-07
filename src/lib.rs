#![feature(macro_rules)]
#![feature(globs)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;

mod ops;
mod extensions;
pub mod memory;
pub mod cpu;
