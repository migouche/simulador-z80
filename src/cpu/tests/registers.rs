use rstest::rstest;

use crate::cpu::{GPR, RegisterPair, SpecialRegister};

use super::setup_cpu;

#[rstest]
#[case(GPR::A, 0x12)]
#[case(GPR::B, 0x34)]
#[case(GPR::C, 0x56)]
#[case(GPR::D, 0x78)]
#[case(GPR::E, 0x9A)]
#[case(GPR::H, 0xBC)]
#[case(GPR::L, 0xDE)]
fn test_register(#[case] reg: GPR, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.set_register(reg, value);
    assert_eq!(cpu.get_register(reg), value);
}

#[rstest]
#[case(RegisterPair::AF, 0x1234)]
#[case(RegisterPair::BC, 0x3456)]
#[case(RegisterPair::DE, 0x5678)]
#[case(RegisterPair::HL, 0x789A)]
fn test_register_pair(#[case] reg: RegisterPair, #[case] value: u16) {
    let mut cpu = setup_cpu();
    cpu.set_register_pair(reg, value);
    assert_eq!(cpu.get_register_pair(reg), value);
}


#[rstest]
#[case(SpecialRegister::PC, 0x9ABC)]
#[case(SpecialRegister::SP, 0xDEF0)]
#[case(SpecialRegister::IX, 0x1234)]
#[case(SpecialRegister::IY, 0x5678)]
#[case(SpecialRegister::I, 0x12)]
#[case(SpecialRegister::R, 0x34)]
#[case(SpecialRegister::A, 0x56)]
#[case(SpecialRegister::IXH, 0x78)]
#[case(SpecialRegister::IXL, 0x9A)]
#[case(SpecialRegister::IYH, 0xBC)]
#[case(SpecialRegister::IYL, 0xDE)]
fn test_special_register(#[case] reg: SpecialRegister, #[case] value: u16) {
    let mut cpu = setup_cpu();
    cpu.set_special_register(reg, value);
    assert_eq!(cpu.get_special_register(reg), value);
}
