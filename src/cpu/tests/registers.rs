use rstest::rstest;

use crate::cpu::{GPR, IndexRegister, IndexRegisterPart, RegisterPair, SystemRegister};

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
#[case(RegisterPair::SP, 0xDEF0)]
fn test_register_pair(#[case] reg: RegisterPair, #[case] value: u16) {
    let mut cpu = setup_cpu();
    cpu.set_register_pair(reg, value);
    assert_eq!(cpu.get_register_pair(reg), value);
}

#[rstest]
#[case(SystemRegister::PC, 0x9ABC)]
#[case(SystemRegister::I, 0x12)]
#[case(SystemRegister::R, 0x34)]
fn test_system_register(#[case] reg: SystemRegister, #[case] value: u16) {
    let mut cpu = setup_cpu();
    cpu.set_system_register(reg, value);
    assert_eq!(cpu.get_system_register(reg), value);
}

#[rstest]
#[case(IndexRegister::IX, 0x1234)]
#[case(IndexRegister::IY, 0x5678)]
fn test_index_register(#[case] reg: IndexRegister, #[case] value: u16) {
    let mut cpu = setup_cpu();
    cpu.set_index_register(reg, value);
    assert_eq!(cpu.get_index_register(reg), value);
}

#[rstest]
#[case(IndexRegisterPart::IXH, 0x78)]
#[case(IndexRegisterPart::IXL, 0x9A)]
#[case(IndexRegisterPart::IYH, 0xBC)]
#[case(IndexRegisterPart::IYL, 0xDE)]
fn test_index_register_part(#[case] reg: IndexRegisterPart, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.set_index_register_part(reg, value);
    assert_eq!(cpu.get_index_register_part(reg), value);
}
