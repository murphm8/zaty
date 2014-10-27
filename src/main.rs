#![feature(tuple_indexing)]
#![feature(struct_inherit)]
#![feature(phase)]
#[phase(plugin, link)] extern crate log;
extern crate native;
extern crate glfw;

use glfw::Context;
use memory::Memory;
use cpu::Cpu;

mod memory;
mod ops;
mod extensions;
mod cpu;

fn main() {
    let mut memory = Memory::new(65536);
    let mut cpu = Cpu::new(&mut memory);
    let glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    let (window, events) = glfw.create_window(256, 256, "Hello this is window", glfw::Windowed)
        .expect("Failed to create GLFW window.");

    window.set_key_polling(true);
    window.make_current();

    while !window.should_close() {
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handle_window_event(&window, event);
        }


        let clock_cycle = cpu.tick();
    }
}


//#[start]
fn start(argc: int, argv: *const *const u8) -> int {
    native::start(argc, argv, main)
}

fn handle_window_event(window: &glfw::Window, event: glfw::WindowEvent) {
    match event {
        glfw::KeyEvent(glfw::KeyEscape, _, glfw::Press, _) => {
            window.set_should_close(true)
        },
        glfw::KeyEvent(glfw::KeyUp, _, glfw::Press, _) => {
        },
        _ => {}
    }
}
