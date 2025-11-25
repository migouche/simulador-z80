use std::{cell::RefCell, rc::Rc};

use crate::{cpu::Z80A, traits::MemoryMapper};

pub mod decoding;
pub mod instructions;
pub mod alu_tests;
pub mod registers;

struct MockMemory {
    data: [u8; 0xFFFF],
}

impl MockMemory {
    fn new() -> Self {
        Self { data: [0; 0xFFFF] }
    }
}

impl MemoryMapper for MockMemory {
    fn read(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.data[address as usize] = data;
    }
}

fn setup_cpu() -> Z80A {
    let memory =
        Rc::new(RefCell::new(MockMemory { data: [0; 0xFFFF] })) as Rc<RefCell<dyn MemoryMapper>>;
    Z80A::new(memory)
}
