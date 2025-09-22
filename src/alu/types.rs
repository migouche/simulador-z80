pub struct Byte(u8);

impl Byte {
    pub fn new(value: u8) -> Self {
        Byte(value)
    }

    pub fn get(&self) -> u8 {
        self.0
    }

    pub fn set(&mut self, value: u8) {
        self.0 = value;
    }
}