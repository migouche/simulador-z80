use rstest::rstest;

use crate::cpu::alu::{rot::*, bit, res, set};

#[rstest]
#[case(0b1000_0001, (0b0000_0011, true))]
#[case(0b0000_0001, (0b0000_0010, false))]
#[case(0b1111_1111, (0b1111_1111, true))]
fn test_rlc(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = rlc(value);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b0000_0011, (0b1000_0001, true))]
#[case(0b0000_0010, (0b0000_0001, false))]
#[case(0b1111_1111, (0b1111_1111, true))]
fn test_rrc(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = rrc(value);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b1000_0001, false, (0b0000_0010, true))]
#[case(0b1000_0001, true, (0b0000_0011, true))]
#[case(0b0000_0001, false, (0b0000_0010, false))]
#[case(0b0000_0001, true, (0b0000_0011, false))]
#[case(0b1111_1111, false, (0b1111_1110, true))]
#[case(0b1111_1111, true, (0b1111_1111, true))]
fn test_rl(#[case] value: u8, #[case] carry_in: bool, #[case] expected: (u8, bool)) {
    let result = rl(value, carry_in);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b0000_0011, false, (0b0000_0001, true))]
#[case(0b0000_0011, true, (0b1000_0001, true))]
#[case(0b0000_0010, false, (0b0000_0001, false))]
#[case(0b0000_0010, true, (0b1000_0001, false))]
#[case(0b1111_1111, false, (0b0111_1111, true))]
#[case(0b1111_1111, true, (0b1111_1111, true))]
fn test_rr(#[case] value: u8, #[case] carry_in: bool, #[case] expected: (u8, bool)) {
    let result = rr(value, carry_in);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b1000_0001, (0b0000_0010, true))]
#[case(0b0000_0001, (0b0000_0010, false))]
#[case(0b1111_1111, (0b1111_1110, true))]
fn test_sla(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = sla(value);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b1000_0001, (0b1100_0000, true))]
#[case(0b0000_0001, (0b0000_0000, true))]
#[case(0b1111_1111, (0b1111_1111, true))]
#[case(0b0111_1110, (0b0011_1111, false))]
#[case(0b0101_0101, (0b0010_1010, true))]
fn test_sra(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = sra(value);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b1000_0001, (0b0000_0011, true))]
#[case(0b0000_0001, (0b0000_0011, false))]
#[case(0b1111_1111, (0b1111_1111, true))]
fn test_sll(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = sll(value);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b1000_0001, (0b0100_0000, true))]
#[case(0b0000_0001, (0b0000_0000, true))]
#[case(0b1111_1111, (0b0111_1111, true))]
#[case(0b0111_1110, (0b0011_1111, false))]
fn test_srl(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = srl(value);
    assert_eq!(result, expected);
}

#[rstest]
// Test bit 0
#[case(0b0000_0001, 0, 0b0001_0000)] // bit 0 SET: only H
#[case(0b0000_0000, 0, 0b0101_0100)] // bit 0 RESET: Z, PV, H

// Test bit 1
#[case(0b0000_0010, 1, 0b0001_0000)] // bit 1 SET: only H
#[case(0b0000_0001, 1, 0b0101_0100)] // bit 1 RESET: Z, PV, H

// Test bit 2
#[case(0b0000_0100, 2, 0b0001_0000)] // bit 2 SET: only H
#[case(0b0000_0011, 2, 0b0101_0100)] // bit 2 RESET: Z, PV, H

// Test bit 3 (XF flag)
#[case(0b0000_1000, 3, 0b0001_1000)] // bit 3 SET: X, H
#[case(0b0000_0111, 3, 0b0101_0100)] // bit 3 RESET: Z, PV, H

// Test bit 4
#[case(0b0001_0000, 4, 0b0001_0000)] // bit 4 SET: only H
#[case(0b0000_1111, 4, 0b0101_0100)] // bit 4 RESET: Z, PV, H

// Test bit 5 (YF flag)
#[case(0b0010_0000, 5, 0b0011_0000)] // bit 5 SET: Y, H
#[case(0b0001_1111, 5, 0b0101_0100)] // bit 5 RESET: Z, PV, H

// Test bit 6
#[case(0b0100_0000, 6, 0b0001_0000)] // bit 6 SET: only H
#[case(0b0011_1111, 6, 0b0101_0100)] // bit 6 RESET: Z, PV, H

// Test bit 7 (SF flag)
#[case(0b1000_0000, 7, 0b1001_0000)] // bit 7 SET: S, H
#[case(0b0111_1111, 7, 0b0101_0100)] // bit 7 RESET: Z, PV, H

// Edge cases: all bits set
#[case(0b1111_1111, 0, 0b0001_0000)] // all SET, test bit 0: only H
#[case(0b1111_1111, 3, 0b0001_1000)] // all SET, test bit 3: X, H
#[case(0b1111_1111, 5, 0b0011_0000)] // all SET, test bit 5: Y, H
#[case(0b1111_1111, 7, 0b1001_0000)] // all SET, test bit 7: S, H

// Edge cases: no bits set
#[case(0b0000_0000, 0, 0b0101_0100)] // all RESET, test bit 0: Z, PV, H
#[case(0b0000_0000, 3, 0b0101_0100)] // all RESET, test bit 3: Z, PV, H
#[case(0b0000_0000, 5, 0b0101_0100)] // all RESET, test bit 5: Z, PV, H
#[case(0b0000_0000, 7, 0b0101_0100)] // all RESET, test bit 7: Z, PV, H

// Combined flag tests
#[case(0b1010_1000, 3, 0b0001_1000)] // bit 3 SET: X, H
#[case(0b1010_1000, 5, 0b0011_0000)] // bit 5 SET: Y, H
#[case(0b1010_1000, 7, 0b1001_0000)] // bit 7 SET: S, H
#[case(0b0101_0111, 3, 0b0101_0100)] // bit 3 RESET: Z, PV, H
#[case(0b0101_0111, 5, 0b0101_0100)] // bit 5 RESET: Z, PV, H
#[case(0b0101_0111, 7, 0b0101_0100)] // bit 7 RESET: Z, PV, H
fn test_bit(#[case] value: u8, #[case] bit_position: u8, #[case] expected: u8) {
    let result = bit(value, bit_position);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b0000_0001, 0, 0b0000_0000)] // Reset bit 0
#[case(0b0000_0010, 1, 0b0000_0000)] // Reset bit 1
#[case(0b0000_0100, 2, 0b0000_0000)] // Reset bit 2
#[case(0b0000_1000, 3, 0b0000_0000)] // Reset bit 3
#[case(0b0001_0000, 4, 0b0000_0000)] // Reset bit 4
#[case(0b0010_0000, 5, 0b0000_0000)] // Reset bit 5
#[case(0b0100_0000, 6, 0b0000_0000)] // Reset bit 6
#[case(0b1000_0000, 7, 0b0000_0000)] // Reset bit 7
#[case(0b1111_1111, 0, 0b1111_1110)] // Reset bit 0
#[case(0b1111_1111, 1, 0b1111_1101)] // Reset bit 1
#[case(0b1111_1111, 2, 0b1111_1011)] // Reset bit 2
#[case(0b1111_1111, 3, 0b1111_0111)] // Reset bit 3
#[case(0b1111_1111, 4, 0b1110_1111)] // Reset bit 4
#[case(0b1111_1111, 5, 0b1101_1111)] // Reset bit 5
#[case(0b1111_1111, 6, 0b1011_1111)] // Reset bit 6
#[case(0b1111_1111, 7, 0b0111_1111)] // Reset bit 7
#[case(0b0000_0000, 0, 0b0000_0000)] // Reset bit 0 on zero
#[case(0b0000_0000, 7, 0b0000_0000)] // Reset bit 7 on zero
fn test_res(#[case] value: u8, #[case] bit_position: u8, #[case] expected: u8) {
    let result = res(value, bit_position);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b0000_0000, 0, 0b0000_0001)] // Set bit 0
#[case(0b0000_0000, 1, 0b0000_0010)] // Set bit 1
#[case(0b0000_0000, 2, 0b0000_0100)] // Set bit 2
#[case(0b0000_0000, 3, 0b0000_1000)] // Set bit 3
#[case(0b0000_0000, 4, 0b0001_0000)] // Set bit 4
#[case(0b0000_0000, 5, 0b0010_0000)] // Set bit 5
#[case(0b0000_0000, 6, 0b0100_0000)] // Set bit 6
#[case(0b0000_0000, 7, 0b1000_0000)] // Set bit 7
#[case(0b1111_1110, 0, 0b1111_1111)] // Set bit 0
#[case(0b1111_1101, 1, 0b1111_1111)] // Set bit 1
#[case(0b1111_1011, 2, 0b1111_1111)] // Set bit 2
#[case(0b1111_0111, 3, 0b1111_1111)] // Set bit 3
#[case(0b1110_1111, 4, 0b1111_1111)] // Set bit 4
#[case(0b1101_1111, 5, 0b1111_1111)] // Set bit 5
#[case(0b1011_1111, 6, 0b1111_1111)] // Set bit 6
#[case(0b0111_1111, 7, 0b1111_1111)] // Set bit 7
#[case(0b1111_1111, 0, 0b1111_1111)] // Set bit 0 on all set
#[case(0b1111_1111, 7, 0b1111_1111)] // Set bit 7 on all set
fn test_set(#[case] value: u8, #[case] bit_position: u8, #[case] expected: u8) {
    let result = set(value, bit_position);
    assert_eq!(result, expected);
}