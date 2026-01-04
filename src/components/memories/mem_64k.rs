use crate::traits::MemoryMapper;

pub struct Mem64k {
    data: [u8; 0x10000],
}

impl Mem64k {
    pub fn new() -> Self {
        Self { data: [0; 0x10000] }
    }
}

impl MemoryMapper for Mem64k {
    fn read(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.data[address as usize] = data;
    }
}

#[test]
fn test_mem_64k_read_write() {
    let mut memory = Mem64k::new();
    memory.write(0x1234, 0xAB);
    assert_eq!(memory.read(0x1234), 0xAB);

    memory.write_word(0x1234, 0xCDEF);
    assert_eq!(memory.read_word(0x1234), 0xCDEF);

    // test little-endian behavior
    memory.write_word(0x2000, 0x3412);
    assert_eq!(memory.read(0x2000), 0x12);
    assert_eq!(memory.read(0x2001), 0x34);

    memory.write(0x3000, 0x78);
    memory.write(0x3001, 0x56);
    assert_eq!(memory.read_word(0x3000), 0x5678);
}
