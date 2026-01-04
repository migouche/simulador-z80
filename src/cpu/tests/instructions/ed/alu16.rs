use crate::{
    cpu::{
        AddressingMode, GPR, RegisterPair, flags,
        tests::{instructions::ed::PREFIX, setup_cpu},
    },
    traits::SyncronousComponent,
};
use rstest::rstest;

const ADD_HL_BC_OPCODE: u8 = 0x4A;
const ADD_HL_DE_OPCODE: u8 = 0x5A;
const ADD_HL_HL_OPCODE: u8 = 0x6A;
const ADD_HL_SP_OPCODE: u8 = 0x7A;

const SBC_HL_BC_OPCODE: u8 = 0x42;
const SBC_HL_DE_OPCODE: u8 = 0x52;
const SBC_HL_HL_OPCODE: u8 = 0x62;
const SBC_HL_SP_OPCODE: u8 = 0x72;

#[rstest]
// --- ADC HL, rp Tests ---
#[case::adc_simple(
    ADD_HL_BC_OPCODE, 0x1000,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x1000,
    AddressingMode::RegisterPair(RegisterPair::BC), 0x2000,
    0x00,               // No Carry in
    0x3000,             // Result
    flags::Y            // Y set (bit 13 of 0x3000)
)]
#[case::adc_with_carry_in(
    ADD_HL_DE_OPCODE, 0xF1AB,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x00FF,
    AddressingMode::RegisterPair(RegisterPair::DE), 0x0001,
    flags::CARRY,       // Carry in set
    0x0101,             // 00FF + 0001 + 1 = 0101
    0x00                // No S, Z, or H set
)]
#[case::adc_zero_result(
    ADD_HL_HL_OPCODE, 0x2000,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x8000,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x8000, // HL + HL
    0x00,
    0x0000,             // 0x8000 + 0x8000 = 0x0000 (carry out)
    flags::ZERO | flags::CARRY | flags::PARITY_OVERFLOW // PV set (overflow from 2 negs to pos)
)]
// --- SBC HL, rp Tests ---
#[case::sbc_simple(
    SBC_HL_BC_OPCODE, 0x3000,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x5000,
    AddressingMode::RegisterPair(RegisterPair::BC), 0x1000,
    0x00,
    0x4000,             // 0x5000 - 0x1000
    flags::ADD_SUB      // N flag always set for SBC
)]
#[case::sbc_with_borrow_in(
    SBC_HL_DE_OPCODE, 0x4000,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x1000,
    AddressingMode::RegisterPair(RegisterPair::DE), 0x0001,
    flags::CARRY,       // Carry (borrow) in
    0x0FFE,             // 0x1000 - 1 - 1 = 0x0FFE
    flags::ADD_SUB | flags::HALF_CARRY | flags::X // Bit 11 of 0x0FFE is set -> X
)]
#[case::sbc_negative_result(
    SBC_HL_SP_OPCODE, 0xBBBB,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x0000,
    AddressingMode::RegisterPair(RegisterPair::SP), 0x0001,
    0x00,
    0xFFFF,             // 0x0000 - 1
    flags::ADD_SUB | flags::SIGN | flags::CARRY | flags::HALF_CARRY | flags::X | flags::Y
)]
#[case::sbc_overflow(
    SBC_HL_DE_OPCODE, 0x8000,
    AddressingMode::RegisterPair(RegisterPair::HL), 0x8000, // -32768
    AddressingMode::RegisterPair(RegisterPair::DE), 0x0001, // 1
    0x00,
    0x7FFF,             // -32768 - 1 = 32767 (Overflow)
    flags::ADD_SUB | flags::PARITY_OVERFLOW | flags::HALF_CARRY | flags::X | flags::Y
)]

fn test_alu16(
    #[case] opcode: u8,
    #[case] initial_pc: u16,
    #[case] dest: AddressingMode,
    #[case] initial_dest_val: u16,
    #[case] src: AddressingMode,
    #[case] initial_src_val: u16,
    #[case] initial_flags: u8,
    #[case] expected_val: u16,
    #[case] expected_flags: u8,
) {
    let mut cpu = setup_cpu();
    cpu.set_register(GPR::F, initial_flags);
    cpu.PC = initial_pc;
    cpu.memory.borrow_mut().write(initial_pc, PREFIX);
    cpu.memory
        .borrow_mut()
        .write(initial_pc.overflowing_add(1).0, opcode);

    // Set initial values
    match dest {
        AddressingMode::RegisterPair(rp) => cpu.set_register_pair(rp, initial_dest_val),
        AddressingMode::IndexRegister(r) => cpu.set_index_register(r, initial_dest_val),
        _ => panic!("Invalid dest setup"),
    };

    match src {
        AddressingMode::RegisterPair(rp) => cpu.set_register_pair(rp, initial_src_val),
        AddressingMode::IndexRegister(r) => cpu.set_index_register(r, initial_src_val),
        _ => panic!("Invalid src setup"),
    };

    // Execute instruction
    cpu.tick();

    // Check results
    let result_val = match dest {
        AddressingMode::RegisterPair(rp) => cpu.get_register_pair(rp),
        AddressingMode::IndexRegister(r) => cpu.get_index_register(r),
        _ => panic!("Invalid dest check"),
    };
    let result_flags = cpu.get_register(GPR::F);

    assert_eq!(
        result_val, expected_val,
        "Value mismatch: expected 0x{:04X}, got 0x{:04X}",
        expected_val, result_val
    );
    assert_eq!(
        result_flags, expected_flags,
        "Flags mismatch: expected {:08b}, got {:08b}",
        expected_flags, result_flags
    );
}
