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

    fn calculate_flags(result: u8, carry: bool) -> u8 {
        let z = result == 0;
        let s = (result & 0x80) != 0;
        let pv = result.count_ones() % 2 == 0;
        let x = (result & 0x08) != 0;
        let y = (result & 0x20) != 0;
        // Carry is passed as parameter

        (if carry { 0x01 } else { 0x00 })
            | (if pv { 0x04 } else { 0x00 })
            | (if x { 0x08 } else { 0x00 })
            | (if y { 0x20 } else { 0x00 })
            | (if z { 0x40 } else { 0x00 })
            | (if s { 0x80 } else { 0x00 })
    }

    pub fn rlc(value: u8) -> (u8, u8) {
        let carry = (value & 0x80) != 0;
        let result = (value << 1) | if carry { 1 } else { 0 };
        let flags = calculate_flags(result, carry);
        (result, flags)
    }

    pub fn rrc(value: u8) -> (u8, u8) {
        let carry = (value & 0x01) != 0;
        let result = (value >> 1) | if carry { 0x80 } else { 0 };
        let flags = calculate_flags(result, carry);
        (result, flags)
    }

    pub fn rl(value: u8, carry_in: bool) -> (u8, u8) {
        let carry = (value & 0x80) != 0;
        let result = (value << 1) | if carry_in { 1 } else { 0 };
        let flags = calculate_flags(result, carry);
        (result, flags)
    }

    pub fn rr(value: u8, carry_in: bool) -> (u8, u8) {
        let carry = (value & 0x01) != 0;
        let result = (value >> 1) | if carry_in { 0x80 } else { 0 };
        let flags = calculate_flags(result, carry);
        (result, flags)
    }

    pub fn sla(value: u8) -> (u8, u8) {
        let carry = (value & 0x80) != 0;
        let result = value << 1;
        let flags = calculate_flags(result, carry);
        (result, flags)
    }

    pub fn sra(value: u8) -> (u8, u8) {
        let carry = (value & 0x01) != 0;
        let msb = value & 0x80;
        let result = (value >> 1) | msb;
        let flags = calculate_flags(result, carry);
        (result, flags)
    }

    pub fn sll(value: u8) -> (u8, u8) {
        let carry = (value & 0x80) != 0;
        let result = (value << 1) | 0x01;
        let flags = calculate_flags(result, carry);
        (result, flags)
    }

    pub fn srl(value: u8) -> (u8, u8) {
        let carry = (value & 0x01) != 0;
        let result = value >> 1;
        let flags = calculate_flags(result, carry);
        (result, flags)
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

pub mod alu_op {
    pub fn add(a: u8, b: u8, carry_in: bool) -> (u8, u8) {
        let (intermediate_sum, carry1) = a.overflowing_add(b);
        let (final_sum, carry2) = intermediate_sum.overflowing_add(if carry_in { 1 } else { 0 });
        let carry_out = carry1 || carry2;

        let h = ((a & 0x0F) + (b & 0x0F) + if carry_in { 1 } else { 0 }) > 0x0F;
        let z = final_sum == 0;
        let s = (final_sum & 0x80) != 0;
        let pv = ((a ^ final_sum) & (b ^ final_sum) & 0x80) != 0;
        let x = (final_sum & 0x08) != 0;
        let y = (final_sum & 0x20) != 0;

        let flags = (if carry_out { 0x01 } else { 0x00 })
            | (if pv { 0x04 } else { 0x00 })
            | (if x { 0x08 } else { 0x00 })
            | (if y { 0x20 } else { 0x00 })
            | (if h { 0x10 } else { 0x00 })
            | (if z { 0x40 } else { 0x00 })
            | (if s { 0x80 } else { 0x00 });

        (final_sum, flags)
    }

    pub fn sub(a: u8, b: u8, carry_in: bool) -> (u8, u8) {
        let (intermediate_diff, borrow1) = a.overflowing_sub(b);
        let (final_diff, borrow2) = intermediate_diff.overflowing_sub(if carry_in { 1 } else { 0 });
        let carry_out = borrow1 || borrow2;

        let h = (a & 0x0F) < ((b & 0x0F) + if carry_in { 1 } else { 0 });
        let z = final_diff == 0;
        let s = (final_diff & 0x80) != 0;
        let pv = ((a ^ b) & (a ^ final_diff) & 0x80) != 0;
        let x = (final_diff & 0x08) != 0;
        let y = (final_diff & 0x20) != 0;
        let n = true;

        let flags = (if carry_out { 0x01 } else { 0x00 })
            | (if n { 0x02 } else { 0x00 })
            | (if pv { 0x04 } else { 0x00 })
            | (if x { 0x08 } else { 0x00 })
            | (if h { 0x10 } else { 0x00 })
            | (if y { 0x20 } else { 0x00 })
            | (if z { 0x40 } else { 0x00 })
            | (if s { 0x80 } else { 0x00 });

        (final_diff, flags)
    }

    pub fn and(a: u8, b: u8) -> (u8, u8) {
        let result = a & b;
        let s = (result & 0x80) != 0;
        let z = result == 0;
        let h = true;
        let pv = result.count_ones() % 2 == 0;
        let n = false;
        let c = false;
        let x = (result & 0x08) != 0;
        let y = (result & 0x20) != 0;

        let flags = (if c { 0x01 } else { 0x00 })
            | (if n { 0x02 } else { 0x00 })
            | (if pv { 0x04 } else { 0x00 })
            | (if x { 0x08 } else { 0x00 })
            | (if h { 0x10 } else { 0x00 })
            | (if y { 0x20 } else { 0x00 })
            | (if z { 0x40 } else { 0x00 })
            | (if s { 0x80 } else { 0x00 });
        (result, flags)
    }

    pub fn or(a: u8, b: u8) -> (u8, u8) {
        let result = a | b;
        let s = (result & 0x80) != 0;
        let z = result == 0;
        let h = false;
        let pv = result.count_ones() % 2 == 0;
        let n = false;
        let c = false;
        let x = (result & 0x08) != 0;
        let y = (result & 0x20) != 0;

        let flags = (if c { 0x01 } else { 0x00 })
            | (if n { 0x02 } else { 0x00 })
            | (if pv { 0x04 } else { 0x00 })
            | (if x { 0x08 } else { 0x00 })
            | (if h { 0x10 } else { 0x00 })
            | (if y { 0x20 } else { 0x00 })
            | (if z { 0x40 } else { 0x00 })
            | (if s { 0x80 } else { 0x00 });
        (result, flags)
    }

    pub fn xor(a: u8, b: u8) -> (u8, u8) {
        let result = a ^ b;
        let s = (result & 0x80) != 0;
        let z = result == 0;
        let h = false;
        let pv = result.count_ones() % 2 == 0;
        let n = false;
        let c = false;
        let x = (result & 0x08) != 0;
        let y = (result & 0x20) != 0;

        let flags = (if c { 0x01 } else { 0x00 })
            | (if n { 0x02 } else { 0x00 })
            | (if pv { 0x04 } else { 0x00 })
            | (if x { 0x08 } else { 0x00 })
            | (if h { 0x10 } else { 0x00 })
            | (if y { 0x20 } else { 0x00 })
            | (if z { 0x40 } else { 0x00 })
            | (if s { 0x80 } else { 0x00 });
        (result, flags)
    }

    pub fn adc(a: u8, b: u8, carry_in: bool) -> (u8, u8) {
        add(a, b, carry_in)
    }

    pub fn sbc(a: u8, b: u8, carry_in: bool) -> (u8, u8) {
        sub(a, b, carry_in)
    }
}

pub fn inc(value: u8) -> (u8, u8) {
    let result = value.wrapping_add(1);
    let z = result == 0;
    let s = (result & 0x80) != 0;
    let h = (value & 0x0F) == 0x0F;
    let pv = value == 0x7F;
    let n = false;
    let x = (result & 0x08) != 0;
    let y = (result & 0x20) != 0;

    // Note: C flag is not affected by INC, caller must preserve it.
    let flags = (if n { 0x02 } else { 0x00 })
        | (if pv { 0x04 } else { 0x00 })
        | (if x { 0x08 } else { 0x00 })
        | (if h { 0x10 } else { 0x00 })
        | (if y { 0x20 } else { 0x00 })
        | (if z { 0x40 } else { 0x00 })
        | (if s { 0x80 } else { 0x00 });

    (result, flags)
}

pub fn add_16(a: u16, b: u16, current_flags: u8) -> (u16, u8) {
    let (result, carry) = a.overflowing_add(b);

    let h = ((a & 0x0FFF) + (b & 0x0FFF)) > 0x0FFF;
    let n = false;
    let c = carry;

    // Undocumented flags X (bit 3) and Y (bit 5) come from the high byte of the result
    let x = (result & 0x0800) != 0;
    let y = (result & 0x2000) != 0;

    // S, Z, P/V are preserved from current_flags
    let s = (current_flags & 0x80) != 0;
    let z = (current_flags & 0x40) != 0;
    let pv = (current_flags & 0x04) != 0;

    let flags = (if c { 0x01 } else { 0x00 })
        | (if n { 0x02 } else { 0x00 })
        | (if pv { 0x04 } else { 0x00 })
        | (if x { 0x08 } else { 0x00 })
        | (if h { 0x10 } else { 0x00 })
        | (if y { 0x20 } else { 0x00 })
        | (if z { 0x40 } else { 0x00 })
        | (if s { 0x80 } else { 0x00 });

    (result, flags)
}

pub fn dec(value: u8) -> (u8, u8) {
    let result = value.wrapping_sub(1);
    let z = result == 0;
    let s = (result & 0x80) != 0;
    let h = (value & 0x0F) == 0x00;
    let pv = value == 0x80;
    let n = true;
    let x = (result & 0x08) != 0;
    let y = (result & 0x20) != 0;

    // Note: C flag is not affected by DEC, caller must preserve it.
    let flags = (if n { 0x02 } else { 0x00 })
        | (if pv { 0x04 } else { 0x00 })
        | (if x { 0x08 } else { 0x00 })
        | (if h { 0x10 } else { 0x00 })
        | (if y { 0x20 } else { 0x00 })
        | (if z { 0x40 } else { 0x00 })
        | (if s { 0x80 } else { 0x00 });

    (result, flags)
}
