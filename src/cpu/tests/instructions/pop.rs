use crate::cpu::RegisterPair;
use crate::cpu::tests::setup_cpu;
use crate::traits::SyncronousComponent;
use rstest::rstest;

#[rstest]
// Case 1: Standard POP BC
// We put 0x44 at 0x1000 and 0x33 at 0x1001.
// POP should load Low=0x44, High=0x33 -> Result 0x3344.
#[case(
    0x0000,                         // Starting PC
    0x1000,                         // Starting SP
    &[(0x1000, 0x44), (0x1001, 0x33)], // Memory Setup (Little Endian!)
    RegisterPair::BC,               // Dest Register
    0x3344,                         // Expected Value in Register
    0x1002                          // Expected Final SP (+2)
)]
// Case 2: POP HL with Address Wrapping
// SP starts at 0xFFFF.
// 1. Reads Low byte at 0xFFFF.
// 2. Increments SP to 0x0000.
// 3. Reads High byte at 0x0000.
// 4. Increments SP to 0x0001.
#[case(
    0x0200,
    0xFFFF,                         // SP at the very top
    &[(0xFFFF, 0xAA), (0x0000, 0xBB)], // Wraps around memory end
    RegisterPair::HL,
    0xBBAA,                         // Expected HL (High=BB, Low=AA)
    0x0001                          // Final SP
)]
// Case 3: POP AF (The Flags Register)
// Validating that the F register accepts values from the stack.
// Stack: 0xC0 (F) and 0x12 (A).
// F=0xC0 means Sign(S) and Zero(Z) flags are set (bits 7 and 6).
#[case(
    0x0000,
    0x2000,
    &[(0x2000, 0xC0), (0x2001, 0x12)], // F=C0, A=12
    RegisterPair::AF,
    0x12C0,
    0x2002
)]
// Case 4: POP DE - "Circular" Stack
// If SP is 0x0000, it pops from 0x0000 then 0x0001.
// This ensures your emulator treats 0x0000 as a valid start point for POP.
#[case(
    0x0010,
    0x0000,
    &[(0x0000, 0xEF), (0x0001, 0xBE)],
    RegisterPair::DE,
    0xBEEF,
    0x0002
)]

fn test_pop(
    #[case] starting_pc: u16,
    #[case] starting_sp: u16,
    #[case] memory_contents: &[(u16, u8)],
    #[case] dest_reg: RegisterPair,
    #[case] expected_value: u16,
    #[case] expected_sp: u16,
) {
    let mut cpu = setup_cpu();

    cpu.PC = starting_pc;
    cpu.SP = starting_sp;

    for (addr, value) in memory_contents {
        cpu.memory.borrow_mut().write(*addr, *value);
    }

    // Write the POP instruction at the current PC
    let pop_opcode = match dest_reg {
        RegisterPair::BC => 0xC1,
        RegisterPair::DE => 0xD1,
        RegisterPair::HL => 0xE1,
        RegisterPair::AF => 0xF1,
        _ => panic!("Invalid register for POP instruction"),
    };
    cpu.memory.borrow_mut().write(cpu.PC, pop_opcode);

    cpu.tick();

    let result = cpu.get_register_pair(dest_reg);
    assert_eq!(
        result, expected_value,
        "Wrong value in {:?} after POP: expected {:04X}, got {:04X}",
        dest_reg, expected_value, result
    );
    assert_eq!(
        cpu.SP, expected_sp,
        "Wrong SP after POP: expected {:04X}, got {:04X}",
        expected_sp, cpu.SP
    );
}
