pub mod rot {
    pub fn rlc (value: u8) -> (u8, bool) {
        let carry = (value & 0x80) != 0;
        let result = (value << 1) | if carry { 1 } else { 0 };
        (result, carry)
    }

    pub fn rrc (value: u8) -> (u8, bool) {
        let carry = (value & 0x01) != 0;
        let result = (value >> 1) | if carry { 0x80 } else { 0 };
        (result, carry)
    }

}