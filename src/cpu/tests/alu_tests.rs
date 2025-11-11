use rstest::rstest;

use crate::cpu::alu::rot::*;

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