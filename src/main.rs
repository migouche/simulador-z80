use std::{cell::RefCell, rc::Rc};

mod components;
mod cpu;
mod traits;

use components::memories::mem_64k::Mem64k;

use crate::traits::SyncronousComponent;
fn main() {
    println!("Hello, world!");

    let mut cpu = cpu::Z80A::new(
        Rc::new(RefCell::new(Mem64k::new())) as Rc<RefCell<dyn traits::MemoryMapper>>
    );

    loop {
        cpu.tick();
    }
}
