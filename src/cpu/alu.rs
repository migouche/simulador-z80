pub mod rot {

    pub enum RotOperation {
        RLC,
        RRC,
        RL,
        RR,
        SLA,
        SRA,
        SLL,
        SRL,
    } 


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

    pub fn rl (value: u8, carry_in: bool) -> (u8, bool) {
        let carry = (value & 0x80) != 0;
        let result = (value << 1) | if carry_in { 1 } else { 0 };
        (result, carry)
    }

    pub fn rr (value: u8, carry_in: bool) -> (u8, bool) {
        let carry = (value & 0x01) != 0;
        let result = (value >> 1) | if carry_in { 0x80 } else { 0 };
        (result, carry)
    }

    pub fn sla (value: u8) -> (u8, bool) {
        let carry = (value & 0x80) != 0;
        let result = value << 1;
        (result, carry)
    }

    pub fn sra (value: u8) -> (u8, bool) {
        let carry = (value & 0x01) != 0;
        let msb = value & 0x80;
        let result = (value >> 1) | msb;
        (result, carry)
    }

    pub fn sll (value: u8) -> (u8, bool) {
        let carry = (value & 0x80) != 0;
        let result = (value << 1) | 0x01;
        (result, carry)
    }

    pub fn srl (value: u8) -> (u8, bool) {
        let carry = (value & 0x01) != 0;
        let result = value >> 1;
        (result, carry)
    }
}