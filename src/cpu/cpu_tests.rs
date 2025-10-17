use crate::cpu::*;
use rstest::rstest;

struct MockMemory {
    data: [u8; 0xFFFF],
}

impl MemoryMapper for MockMemory {
    fn read(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.data[address as usize] = data;
    }
}

use std::cell::RefCell;
use std::rc::Rc;

fn setup_cpu() -> Z80A {
    let memory = Rc::new(RefCell::new(MockMemory { data: [0; 0xFFFF] })) as Rc<RefCell<dyn MemoryMapper>>;
    Z80A::new(memory)
}

#[rstest]
#[case(GPR::A, GPR::B, 0x42)]
fn test_register_register(#[case] src: GPR, #[case] dest: GPR, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.set_register(src, value);
    cpu.ld(AddressingMode::Register(dest), AddressingMode::Register(src));
    assert_eq!(cpu.get_register(dest), value);
}
