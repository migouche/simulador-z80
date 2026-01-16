use rstest::rstest;

use crate::cpu::{
    alu::{add_16, alu_op::*, bit, dec, inc, res, rot::*, set, sub_16},
    flags,
};

#[rstest]
#[case(0b1000_0001, 0b0000_0011, 0b0000_0101)] // result=0x03, flags: C=1, PV=1
#[case(0b0000_0001, 0b0000_0010, 0b0000_0000)] // result=0x02, flags: PV=1
#[case(0b1111_1111, 0b1111_1111, 0b1010_1101)] // result=0xFF, flags: C=1, Y=1, S=1
fn test_rlc(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = rlc(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0b0000_0011, 0b1000_0001, 0b1000_0101)] // result=0x81, flags: C=1, S=1
#[case(0b0000_0010, 0b0000_0001, 0b0000_0000)] // result=0x01, flags: PV=1
#[case(0b1111_1111, 0b1111_1111, 0b1010_1101)] // result=0xFF, flags: C=1, Y=1, S=1
fn test_rrc(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = rrc(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0b1000_0001, false, 0b0000_0010, 0b0000_0001)] // result=0x02, flags: C=1, PV=1
#[case(0b1000_0001, true, 0b0000_0011, 0b0000_0101)] // result=0x03, flags: C=1, PV=1
#[case(0b0000_0001, false, 0b0000_0010, 0b0000_0000)] // result=0x02, flags: PV=1
#[case(0b0000_0001, true, 0b0000_0011, 0b0000_0100)] // result=0x03, flags: PV=1
#[case(0b1111_1111, false, 0b1111_1110, 0b1010_1001)] // result=0xFE, flags: C=1, Y=1, S=1
#[case(0b1111_1111, true, 0b1111_1111, 0b1010_1101)] // result=0xFF, flags: C=1, Y=1, S=1
fn test_rl(
    #[case] value: u8,
    #[case] carry_in: bool,
    #[case] expected_result: u8,
    #[case] expected_flags: u8,
) {
    let (result, flags) = rl(value, carry_in);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0b0000_0011, false, 0b0000_0001, 0b0000_0001)] // result=0x01, flags: C=1, PV=1
#[case(0b0000_0011, true, 0b1000_0001, 0b1000_0101)] // result=0x81, flags: C=1, S=1
#[case(0b0000_0010, false, 0b0000_0001, 0b0000_0000)] // result=0x01, flags: PV=1
#[case(0b0000_0010, true, 0b1000_0001, 0b1000_0100)] // result=0x81, flags: S=1
#[case(0b1111_1111, false, 0b0111_1111, 0b0010_1001)] // result=0x7F, flags: C=1, Y=1
#[case(0b1111_1111, true, 0b1111_1111, 0b1010_1101)] // result=0xFF, flags: C=1, Y=1, S=1
fn test_rr(
    #[case] value: u8,
    #[case] carry_in: bool,
    #[case] expected_result: u8,
    #[case] expected_flags: u8,
) {
    let (result, flags) = rr(value, carry_in);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0b1000_0001, 0b0000_0010, 0b0000_0001)] // result=0x02, flags: C=1, PV=1
#[case(0b0000_0001, 0b0000_0010, 0b0000_0000)] // result=0x02, flags: PV=1
#[case(0b1111_1111, 0b1111_1110, 0b1010_1001)] // result=0xFE, flags: C=1, Y=1, S=1
fn test_sla(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = sla(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0b1000_0001, 0b1100_0000, 0b1000_0101)] // result=0xC0, flags: C=1, S=1
#[case(0b0000_0001, 0b0000_0000, 0b0100_0101)] // result=0x00, flags: C=1, PV=1, Z=1
#[case(0b1111_1111, 0b1111_1111, 0b1010_1101)] // result=0xFF, flags: C=1, Y=1, S=1
#[case(0b0111_1110, 0b0011_1111, 0b0010_1100)] // result=0x3F, flags: Y=1, PV=1
#[case(0b0101_0101, 0b0010_1010, 0b0010_1001)] // result=0x2A, flags: C=1, Y=1, PV=1
fn test_sra(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = sra(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0b1000_0001, 0b0000_0011, 0b0000_0101)] // result=0x03, flags: C=1, PV=1
#[case(0b0000_0001, 0b0000_0011, 0b0000_0100)] // result=0x03, flags: PV=1
#[case(0b1111_1111, 0b1111_1111, 0b1010_1101)] // result=0xFF, flags: C=1, Y=1, S=1
fn test_sll(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = sll(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0b1000_0001, 0b0100_0000, 0b0000_0001)] // result=0x40, flags: C=1
#[case(0b0000_0001, 0b0000_0000, 0b0100_0101)] // result=0x00, flags: C=1, PV=1, Z=1
#[case(0b1111_1111, 0b0111_1111, 0b0010_1001)] // result=0x7F, flags: C=1, Y=1
#[case(0b0111_1110, 0b0011_1111, 0b0010_1100)] // result=0x3F, flags: Y=1, PV=1
fn test_srl(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = srl(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
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

#[rstest]
#[case(1, 1, false, 2, 0x00)] // 1 + 1 = 2
#[case(0xFF, 1, false, 0, 0x51)] // 0xFF + 1 = 0 (Z, H, C)
#[case(0x7F, 1, false, 0x80, 0x94)] // 0x7F + 1 = 0x80 (S, H, PV)
#[case(0x80, 0x80, false, 0, 0x45)] // 0x80 + 0x80 = 0 (Z, PV, C)
#[case(0, 0, false, 0, 0x40)] // 0 + 0 = 0 (Z)
#[case(0x10, 0x10, false, 0x20, 0x20)] // 0x10 + 0x10 = 0x20 (Y)
#[case(0x04, 0x04, false, 0x08, 0x08)] // 0x04 + 0x04 = 0x08 (X)
#[case(1, 1, true, 3, 0x00)] // 1 + 1 + 1 = 3
fn test_add(
    #[case] a: u8,
    #[case] b: u8,
    #[case] carry_in: bool,
    #[case] expected_result: u8,
    #[case] expected_flags: u8,
) {
    let (result, flags) = add(a, b, carry_in);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0, 0, false, 0, 0x40)] // 0 + 0 + 0 = 0 (Z)
#[case(0, 0, true, 1, 0x00)] // 0 + 0 + 1 = 1
#[case(0xFF, 0, true, 0, 0x51)] // 0xFF + 0 + 1 = 0 (Z, H, C)
#[case(0x7F, 0, true, 0x80, 0x94)] // 0x7F + 0 + 1 = 0x80 (S, H, PV)
fn test_adc(
    #[case] a: u8,
    #[case] b: u8,
    #[case] carry_in: bool,
    #[case] expected_result: u8,
    #[case] expected_flags: u8,
) {
    let (result, flags) = adc(a, b, carry_in);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0, 0, false, 0, 0x42)] // 0 - 0 = 0 (Z, N)
#[case(1, 1, false, 0, 0x42)] // 1 - 1 = 0 (Z, N)
#[case(0, 1, false, 0xFF, 0xBB)] // 0 - 1 = -1 (S, Y, H, X, N, C)
#[case(0x80, 1, false, 0x7F, 0x3E)] // -128 - 1 = 127 (PV, N, H, X, Y)
fn test_sub(
    #[case] a: u8,
    #[case] b: u8,
    #[case] carry_in: bool,
    #[case] expected_result: u8,
    #[case] expected_flags: u8,
) {
    let (result, flags) = sub(a, b, carry_in);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0, 0, true, 0xFF, 0xBB)] // 0 - 0 - 1 = -1 (S, Y, H, X, N, C)
#[case(10, 5, true, 4, 0x02)] // 10 - 5 - 1 = 4 (N)
fn test_sbc(
    #[case] a: u8,
    #[case] b: u8,
    #[case] carry_in: bool,
    #[case] expected_result: u8,
    #[case] expected_flags: u8,
) {
    let (result, flags) = sbc(a, b, carry_in);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0xFF, 0xFF, 0xFF, 0xBC)] // 0xFF & 0xFF = 0xFF (S, H, PV, X, Y)
#[case(0xFF, 0x00, 0x00, 0x54)] // 0xFF & 0x00 = 0x00 (Z, H, PV)
fn test_and(#[case] a: u8, #[case] b: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = and(a, b);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0xFF, 0xFF, 0x00, 0x44)] // 0xFF ^ 0xFF = 0x00 (Z, PV)
#[case(0xFF, 0x00, 0xFF, 0xAC)] // 0xFF ^ 0x00 = 0xFF (S, PV, X, Y)
fn test_xor(#[case] a: u8, #[case] b: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = xor(a, b);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0xFF, 0xFF, 0xFF, 0xAC)] // 0xFF | 0xFF = 0xFF (S, PV, X, Y)
#[case(0x00, 0x00, 0x00, 0x44)] // 0x00 | 0x00 = 0x00 (Z, PV)
fn test_or(#[case] a: u8, #[case] b: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = or(a, b);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0, 0, 0x42)] // 0 - 0 = 0 (Z, N)
#[case(1, 1, 0x42)] // 1 - 1 = 0 (Z, N)
#[case(0, 1, 0xBB)] // 0 - 1 = -1 (S, Y, H, X, N, C)
#[case(0x80, 1, 0x3E)] // -128 - 1 = 127 (PV, N, H, X, Y)
fn test_cp(#[case] a: u8, #[case] b: u8, #[case] expected_flags: u8) {
    let (_, flags) = sub(a, b, false);
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0x00, 0x01, 0x00)] // 0 + 1 = 1
#[case(0xFF, 0x00, 0x50)] // 0xFF + 1 = 0 (Z, H)
#[case(0x7F, 0x80, 0x94)] // 0x7F + 1 = 0x80 (S, H, PV)
#[case(0x0F, 0x10, 0x10)] // 0x0F + 1 = 0x10 (H)
fn test_inc(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = inc(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
#[case(0x01, 0x00, 0x42)] // 1 - 1 = 0 (Z, N)
#[case(0x00, 0xFF, 0xBA)] // 0 - 1 = 0xFF (S, H, N, X, Y)
#[case(0x80, 0x7F, 0x3E)] // 0x80 - 1 = 0x7F (H, PV, N, X, Y)
fn test_dec(#[case] value: u8, #[case] expected_result: u8, #[case] expected_flags: u8) {
    let (result, flags) = dec(value);
    assert_eq!(result, expected_result, "Result mismatch");
    assert_eq!(flags, expected_flags, "Flags mismatch");
}

#[rstest]
// ADC Case 6 Fix: 0x7000 + 0x1000 = 0x8000.
// Bit 11 of 0x8000 is 0. So X (0x08) should be OFF.
// Bit 15 is 1 (Sign), Result is not zero, Overflow is ON.
#[case(0x7000, 0x1000, 0x00, true, 0x8000, flags::PARITY_OVERFLOW | flags::SIGN)]

fn test_add_16_corrected(
    #[case] a: u16,
    #[case] b: u16,
    #[case] current_flags: u8,
    #[case] use_carry: bool,
    #[case] expected_res: u16,
    #[case] expected_flags: u8,
) {
    let (res, flags) = add_16(a, b, current_flags, use_carry);
    assert_eq!(res, expected_res);
    assert_eq!(
        flags, expected_flags,
        "Expected {:08b}, got {:08b}",
        expected_flags, flags
    );
}

#[rstest]
// SUB Case 1 & 3 Fix: 0x2000 - 0x1000 = 0x1000.
// Bit 11 of 0x1000 is 0. So X flag should be OFF.
#[case(0x2000, 0x1000, 0x00, false, 0x1000, flags::ADD_SUB)]
// SUB Case 6 Fix: 0x8000 - 0x0001 = 0x7FFF.
// 0x7FFF binary: 0111 1111 1111 1111.
// Bit 11 is 1 (X=on), Bit 13 is 1 (Y=on).
#[case(0x8000, 0x0001, 0x00, true, 0x7FFF, flags::ADD_SUB | flags::PARITY_OVERFLOW | flags::HALF_CARRY | flags::X | flags::Y)]

fn test_sub_16_corrected(
    #[case] a: u16,
    #[case] b: u16,
    #[case] current_flags: u8,
    #[case] use_carry: bool,
    #[case] expected_res: u16,
    #[case] expected_flags: u8,
) {
    let (res, flags) = sub_16(a, b, current_flags, use_carry);
    assert_eq!(res, expected_res);
    assert_eq!(
        flags, expected_flags,
        "Expected {:08b}, got {:08b}",
        expected_flags, flags
    );
}
