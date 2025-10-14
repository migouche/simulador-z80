pub trait SyncronousComponent {
    fn tick(&mut self);
}

pub trait MemoryMapper {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, data: u8);

    fn read_word(&self, address: u16) -> u16 {
        let low = self.read(address);
        let high = self.read(address + 1);
        ((high as u16) << 8) | (low as u16)
    }

    fn write_word(&mut self, address: u16, data: u16) {
        self.write(address, (data & 0xFF) as u8);
        self.write(address + 1, (data >> 8) as u8);
    }
}
