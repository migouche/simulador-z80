use std::mem::transmute;

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

pub fn bit(value: u8, bit_position: u8) -> u8 {
    let bit: bool = (value & (1 << bit_position)) != 0;

    let z = !bit;
    let s = bit_position == 7 && bit;
    let pv = z;
    let h = true;
    let n = false;
    let c = false; // unaffected, will use mask later to ignore
    let f3 = bit_position == 3 && bit;
    let f5 = bit_position == 5 && bit;

    (if c { 0x01 } else { 0x00 })
        | (if n { 0x02 } else { 0x00 })
        | (if pv { 0x04 } else { 0x00 })
        | (if f3 { 0x08 } else { 0x00 })
        | (if h { 0x10 } else { 0x00 })
        | (if f5 { 0x20 } else { 0x00 })
        | (if z { 0x40 } else { 0x00 })
        | (if s { 0x80 } else { 0x00 })
}

pub fn res(value: u8, bit_position: u8) -> u8 {
    value & !(1 << bit_position)
}

pub fn set(value: u8, bit_position: u8) -> u8 {
    value | (1 << bit_position)
}