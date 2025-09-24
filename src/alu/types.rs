#[derive(Debug)]
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

    pub fn twos_complement(&self) -> u8 {
        (!self.0).wrapping_add(1)
    }

    pub fn extend(&self) -> Word {
        Word::new(self.0 as u16)
    }
}

impl PartialEq for Byte {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

#[derive(Debug)]

pub struct Word(u16);

impl Word {
    pub fn new(value: u16) -> Self {
        Word(value)
    }
    pub fn get(&self) -> u16 {
        self.0
    }
    pub fn from_bytes(low: Byte, high: Byte) -> Self { 
        // putting low first because of little-endian so you can do Word::from_bytes(fetch(), fetch())
        Word(((high.get() as u16) << 8) | (low.get() as u16))
    }
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
    
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_byte() {
        let mut byte = Byte::new(5);
        assert_eq!(byte.get(), 5);
        byte.set(10);
        assert_eq!(byte.get(), 10);
    }

    fn test_byte_eq() {
        let byte1 = Byte::new(5);
        let byte2 = Byte::new(5);
        let byte3 = Byte::new(10);
        assert_eq!(byte1, byte2);
        assert_ne!(byte1, byte3);
    }

    #[test]
    fn test_word() {
        let low = Byte::new(5);
        let high = Byte::new(10);
        let word = Word::from_bytes(low, high);
        assert_eq!(word.get(), 0x0A05);
    }

    fn test_twos_complement() {
        let byte = Byte::new(1);
        assert_eq!(byte.twos_complement(), 255);
        let byte = Byte::new(255);
        assert_eq!(byte.twos_complement(), 1);
        let byte = Byte::new(177);
        assert_eq!(byte.twos_complement(), 79);
    }
}