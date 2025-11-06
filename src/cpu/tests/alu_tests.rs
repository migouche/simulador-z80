use rstest::rstest;

use crate::cpu::alu::rot;

#[rstest]
#[case(0b1000_0001, (0b0000_0011, true))]
#[case(0b0000_0001, (0b0000_0010, false))]
#[case(0b1111_1111, (0b1111_1111, true))]
fn test_rlc(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = rot::rlc(value);
    assert_eq!(result, expected);
}

#[rstest]
#[case(0b0000_0011, (0b1000_0001, true))]
#[case(0b0000_0010, (0b0000_0001, false))]
#[case(0b1111_1111, (0b1111_1111, true))]
fn test_rrc(#[case] value: u8, #[case] expected: (u8, bool)) {
    let result = rot::rrc(value);
    assert_eq!(result, expected);
}