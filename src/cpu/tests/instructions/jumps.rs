use rstest::rstest;

use crate::cpu::tests::setup_cpu;
use crate::traits::SyncronousComponent;

const DJNZ_OPCODE: u8 = 0x10;
const JR_D_OPCODE: u8 = 0x18;
const JR_NZ_D_OPCODE: u8 = 0x20;
const JR_Z_D_OPCODE: u8 = 0x28;
const JR_NC_D_OPCODE: u8 = 0x30;
const JR_C_D_OPCODE: u8 = 0x38;

#[rstest]
#[case(0x1000, 5, -5i8 as u8, 4, 0x0FFD)]
#[case(0x1000, 10, 10, 9, 0x100C)]
#[case(0x1000, 1, -5i8 as u8, 0, 0x1002)]
#[case(0x1000, 0, -2i8 as u8, 0xFF, 0x1000)]
#[case(0xFFFE, 2, 5, 1, 0x0005)]
fn test_djnz(
    #[case] starting_pc: u16,
    #[case] initial_b: u8,
    #[case] displacement: u8,
    #[case] expected_b: u8,
    #[case] expected_pc: u16,
) {
    let mut cpu = setup_cpu();

    cpu.PC = starting_pc;
    cpu.set_register(crate::cpu::GPR::B, initial_b);
    cpu.memory.borrow_mut().write(cpu.PC, DJNZ_OPCODE);
    cpu.memory
        .borrow_mut()
        .write(cpu.PC.wrapping_add(1), displacement);
    cpu.tick(); // Fetch DJNZ

    assert_eq!(
        cpu.get_register(crate::cpu::GPR::B),
        expected_b,
        "Wrong B register after DJNZ: expected {}, got {}",
        expected_b,
        cpu.get_register(crate::cpu::GPR::B)
    );
    assert_eq!(
        cpu.PC, expected_pc,
        "Wrong PC after DJNZ: expected {:04X}, got {:04X}",
        expected_pc, cpu.PC
    );
}

#[rstest]
#[case(0x2000, 5, 0x2007)]
#[case(0x2000, -3i8 as u8, 0x1FFF)]
#[case(0x2000, 0, 0x2002)]
#[case(0xFFFD, 4, 0x0003)]
fn test_jr_d(#[case] starting_pc: u16, #[case] displacement: u8, #[case] expected_pc: u16) {
    let mut cpu = setup_cpu();

    cpu.PC = starting_pc;
    cpu.memory.borrow_mut().write(cpu.PC, JR_D_OPCODE); // JR d opcode
    cpu.memory
        .borrow_mut()
        .write(cpu.PC.wrapping_add(1), displacement as u8);
    cpu.tick(); // Fetch JR d

    assert_eq!(
        cpu.PC, expected_pc,
        "Wrong PC after JR d: expected {:04X}, got {:04X}",
        expected_pc, cpu.PC
    );
}

const FLAG_Z: u8 = 0x40;
const FLAG_C: u8 = 0x01;
#[rstest]
#[case(0x1000, JR_NZ_D_OPCODE, -2i8 as u8, 0, 0x1000)]
#[case(0x1000, JR_NZ_D_OPCODE, -2i8 as u8, FLAG_Z, 0x1002)]
#[case(0x2000, JR_Z_D_OPCODE, 5, FLAG_Z, 0x2007)]
#[case(0x2000, JR_Z_D_OPCODE, 5, 0, 0x2002)]
#[case(0x3000, JR_NC_D_OPCODE, 10, 0, 0x300C)]
#[case(0x3000, JR_NC_D_OPCODE, 10, FLAG_C, 0x3002)]
#[case(0x4000, JR_C_D_OPCODE, -4i8 as u8, FLAG_C, 0x3FFE)]
#[case(0x4000, JR_C_D_OPCODE, -4i8 as u8, 0, 0x4002)]
fn test_jr_cc_d(
    #[case] starting_pc: u16,
    #[case] opcode: u8,
    #[case] displacement: u8,
    #[case] flags: u8,
    #[case] expected_pc: u16,
) {
    let mut cpu = setup_cpu();

    cpu.PC = starting_pc;
    cpu.main_set.F = flags;
    cpu.memory.borrow_mut().write(cpu.PC, opcode); // JR cc[y-4], d opcode
    cpu.memory
        .borrow_mut()
        .write(cpu.PC.wrapping_add(1), displacement as u8);
    cpu.tick(); // Fetch JR cc[y-4], d

    assert_eq!(
        cpu.PC, expected_pc,
        "Wrong PC after JR cc[y-4], d: expected {:04X}, got {:04X}",
        expected_pc, cpu.PC
    );
}
