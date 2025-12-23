use rstest::rstest;

use crate::cpu::tests::setup_cpu;
    use crate::traits::SyncronousComponent;

const DJNZ_OPCODE: u8 = 0x10;


#[rstest]
#[case(0x1000, 5, -5i8 as u8, 4, 0x0FFD)]
#[case(0x1000, 10, 10, 9, 0x100C)]
#[case(0x1000, 1, -5i8 as u8, 0, 0x1002)]
#[case(0x1000, 0, -2i8 as u8, 0xFF, 0x1000)]
#[case(0xFFFE, 2, 5, 1, 0x0005)]
fn test_djnz_instruction(#[case] starting_pc: u16, #[case] initial_b: u8, #[case] displacement: u8, #[case] expected_b: u8, #[case] expected_pc: u16) {

    
    let mut cpu = setup_cpu();

    cpu.PC = starting_pc;
    cpu.set_register(crate::cpu::GPR::B, initial_b);
    cpu.memory.borrow_mut().write(cpu.PC, DJNZ_OPCODE);
    cpu.memory.borrow_mut().write(cpu.PC.wrapping_add(1), displacement);
    cpu.tick(); // Fetch DJNZ

    assert_eq!(cpu.get_register(crate::cpu::GPR::B), expected_b, "Wrong B register after DJNZ: expected {}, got {}", expected_b, cpu.get_register(crate::cpu::GPR::B));
    assert_eq!(cpu.PC, expected_pc, "Wrong PC after DJNZ: expected {}, got {}", expected_pc, cpu.PC);
}