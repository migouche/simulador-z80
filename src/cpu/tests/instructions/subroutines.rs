use crate::{
    cpu::{
        Condition, GPR,
        tests::{setup_cpu, stack},
    },
    traits::SyncronousComponent,
};
use rstest::rstest;

enum RetCC {
    None,
    Condition(Condition),
}

const RET_OPCODE: u8 = 0xC9;
const RET_NZ_OPCODE: u8 = 0xC0;
const RET_Z_OPCODE: u8 = 0xC8;
const RET_NC_OPCODE: u8 = 0xD0;
const RET_C_OPCODE: u8 = 0xD8;
const RET_PO_OPCODE: u8 = 0xE0;
const RET_PE_OPCODE: u8 = 0xE8;
const RET_P_OPCODE: u8 = 0xF0;
const RET_M_OPCODE: u8 = 0xF8;

#[rstest]
#[case::ret(
    0x1000,
    0x2000,
    0x00,
    &[(0x2000, 0x34), (0x2001, 0x12)],
    RetCC::None,
    0x1234,
    0x2002
)]
#[case::ret_nz_taken(
    0x1000,
    0x3000,
    0xFF ^ crate::cpu::flags::ZERO, // Z flag cleared
    &[(0x3000, 0x78), (0x3001, 0x56)],
    RetCC::Condition(Condition::NZ),
    0x5678,
    0x3002
)]
#[case::ret_nz_not_taken(
    0x1000,
    0x4000,
    crate::cpu::flags::ZERO, // Z flag set
    &[(0x4000, 0x9A), (0x4001, 0xBC)],
    RetCC::Condition(Condition::NZ),
    0x1001, // PC should just advance
    0x4000  // SP should remain unchanged
)]
#[case::ret_z_taken(
    0x1000,
    0x5000,
    crate::cpu::flags::ZERO, // Z flag set
    &[(0x5000, 0xDE), (0x5001, 0xF0)],
    RetCC::Condition(Condition::Z),
    0xF0DE,
    0x5002
)]
#[case::ret_z_not_taken(
    0x1000,
    0x6000,
    0xff ^ crate::cpu::flags::ZERO, // Z flag cleared
    &[(0x6000, 0x11), (0x6001, 0x22)],
    RetCC::Condition(Condition::Z),
    0x1001, // PC should just advance
    0x6000  // SP should remain unchanged
)]
#[case::ret_nc_taken(
    0x1000,
    0x7000,
    0xff ^ crate::cpu::flags::CARRY, // C flag cleared
    &[(0x7000, 0x33), (0x7001, 0x44)],
    RetCC::Condition(Condition::NC),
    0x4433,
    0x7002
)]
#[case::ret_nc_not_taken(
    0x1000,
    0x8000,
    crate::cpu::flags::CARRY, // C flag set
    &[(0x8000, 0x55), (0x8001, 0x66)],
    RetCC::Condition(Condition::NC),
    0x1001, // PC should just advance
    0x8000  // SP should remain unchanged
)]
#[case::ret_c_taken(
    0x1000,
    0x9000,
    crate::cpu::flags::CARRY, // C flag set
    &[(0x9000, 0x77), (0x9001, 0x88)],
    RetCC::Condition(Condition::C),
    0x8877,
    0x9002
)]
#[case::ret_c_not_taken(
    0x1000,
    0xA000,
    0xff ^ crate::cpu::flags::CARRY, // C flag cleared
    &[(0xA000, 0x99), (0xA001, 0xAA)],
    RetCC::Condition(Condition::C),
    0x1001, // PC should just advance
    0xA000  // SP should remain unchanged
)]
#[case::ret_po_taken(
    0x1000,
    0xB000,
    0xff ^ crate::cpu::flags::PARITY_OVERFLOW, // P/V flag cleared
    &[(0xB000, 0xBB), (0xB001, 0xCC)],
    RetCC::Condition(Condition::PO),
    0xCCBB,
    0xB002
)]
#[case::ret_po_not_taken(
    0x1000,
    0xC000,
    crate::cpu::flags::PARITY_OVERFLOW, // P/V flag set
    &[(0xC000, 0xDD), (0xC001, 0xEE)],
    RetCC::Condition(Condition::PO),
    0x1001, // PC should just advance
    0xC000  // SP should remain unchanged
)]
#[case::ret_pe_taken(
    0x1000,
    0xD000,
    crate::cpu::flags::PARITY_OVERFLOW, // P/V flag set
    &[(0xD000, 0xFF), (0xD001, 0x00)],
    RetCC::Condition(Condition::PE),
    0x00FF,
    0xD002
)]
#[case::ret_pe_not_taken(
    0x1000,
    0xE000,
    0xff ^ crate::cpu::flags::PARITY_OVERFLOW, // P/V flag cleared
    &[(0xE000, 0x12), (0xE001, 0x34)],
    RetCC::Condition(Condition::PE),
    0x1001, // PC should just advance
    0xE000  // SP should remain unchanged
)]
#[case::ret_p_taken(
    0x1000,
    0xF000,
    0xff ^ crate::cpu::flags::SIGN, // S flag cleared
    &[(0xF000, 0x56), (0xF001, 0x78)],
    RetCC::Condition(Condition::P),
    0x7856,
    0xF002
)]
#[case::ret_p_not_taken(
    0x1000,
    0x1100,
    crate::cpu::flags::SIGN, // S flag set
    &[(0x1100, 0x9A), (0x1101, 0xBC)],
    RetCC::Condition(Condition::P),
    0x1001, // PC should just advance
    0x1100  // SP should remain unchanged
)]
#[case::ret_m_taken(
    0x1000,
    0x1200,
    crate::cpu::flags::SIGN, // S flag set
    &[(0x1200, 0xDE), (0x1201, 0xF0)],
    RetCC::Condition(Condition::M),
    0xF0DE,
    0x1202
)]
#[case::ret_m_not_taken(
    0x1000,
    0x1300,
    0xff ^ crate::cpu::flags::SIGN, // S flag cleared
    &[(0x1300, 0x11), (0x1301, 0x22)],
    RetCC::Condition(Condition::M),
    0x1001, // PC should just advance
    0x1300  // SP should remain unchanged
)]

fn test_ret(
    #[case] initial_pc: u16,
    #[case] initial_sp: u16,
    #[case] initial_flags: u8,
    #[case] stacked_values: &[(u16, u8)],
    #[case] ret_cc: RetCC,
    #[case] expected_final_pc: u16,
    #[case] expected_final_sp: u16,
) {
    let mut cpu = setup_cpu();
    for &(addr, val) in stacked_values {
        cpu.memory.borrow_mut().write(addr, val);
    }
    cpu.PC = initial_pc;
    cpu.SP = initial_sp;
    cpu.set_register(GPR::F, initial_flags);

    let opcode = match ret_cc {
        RetCC::None => RET_OPCODE,
        RetCC::Condition(Condition::NZ) => RET_NZ_OPCODE,
        RetCC::Condition(Condition::Z) => RET_Z_OPCODE,
        RetCC::Condition(Condition::NC) => RET_NC_OPCODE,
        RetCC::Condition(Condition::C) => RET_C_OPCODE,
        RetCC::Condition(Condition::PO) => RET_PO_OPCODE,
        RetCC::Condition(Condition::PE) => RET_PE_OPCODE,
        RetCC::Condition(Condition::P) => RET_P_OPCODE,
        RetCC::Condition(Condition::M) => RET_M_OPCODE,
    };
    cpu.memory.borrow_mut().write(cpu.PC, opcode);
    cpu.tick();

    assert_eq!(
        cpu.PC, expected_final_pc,
        "PC mismatch: expected 0x{:04X}, got 0x{:04X}",
        expected_final_pc, cpu.PC
    );
    assert_eq!(
        cpu.SP, expected_final_sp,
        "SP mismatch: expected 0x{:04X}, got 0x{:04X}",
        expected_final_sp, cpu.SP
    );
}

const CALL_OPCODE: u8 = 0xCD;
const CALL_NZ_OPCODE: u8 = 0xC4;
const CALL_Z_OPCODE: u8 = 0xCC;
const CALL_NC_OPCODE: u8 = 0xD4;
const CALL_C_OPCODE: u8 = 0xDC;
const CALL_PO_OPCODE: u8 = 0xE4;
const CALL_PE_OPCODE: u8 = 0xEC;
const CALL_P_OPCODE: u8 = 0xF4;
const CALL_M_OPCODE: u8 = 0xFC;

enum CallCC {
    None,
    Condition(Condition),
}

#[rstest]
#[case::call(
    0x1000,
    0x2000,
    0x00,
    0x55AA,
    CallCC::None,
    0x55AA, // PC jumps to target
    0x1FFE  // SP decrements by 2
)]
#[case::call_nz_taken(
    0x1000,
    0x3000,
    0xFF ^ crate::cpu::flags::ZERO, // Z flag cleared
    0x66BB,
    CallCC::Condition(Condition::NZ),
    0x66BB,
    0x2FFE
)]
#[case::call_nz_not_taken(
    0x1000,
    0x4000,
    crate::cpu::flags::ZERO, // Z flag set
    0x66BB,
    CallCC::Condition(Condition::NZ),
    0x1003, // PC advances by 3 (opcode + 2 byte addr)
    0x4000  // SP remains unchanged
)]
#[case::call_z_taken(
    0x1000,
    0x5000,
    crate::cpu::flags::ZERO, // Z flag set
    0x77CC,
    CallCC::Condition(Condition::Z),
    0x77CC,
    0x4FFE
)]
#[case::call_z_not_taken(
    0x1000,
    0x6000,
    0xFF ^ crate::cpu::flags::ZERO, // Z flag cleared
    0x77CC,
    CallCC::Condition(Condition::Z),
    0x1003,
    0x6000
)]
#[case::call_nc_taken(
    0x1000,
    0x7000,
    0xFF ^ crate::cpu::flags::CARRY, // C flag cleared
    0x88DD,
    CallCC::Condition(Condition::NC),
    0x88DD,
    0x6FFE
)]
#[case::call_nc_not_taken(
    0x1000,
    0x8000,
    crate::cpu::flags::CARRY, // C flag set
    0x88DD,
    CallCC::Condition(Condition::NC),
    0x1003,
    0x8000
)]
#[case::call_c_taken(
    0x1000,
    0x9000,
    crate::cpu::flags::CARRY, // C flag set
    0x99EE,
    CallCC::Condition(Condition::C),
    0x99EE,
    0x8FFE
)]
#[case::call_c_not_taken(
    0x1000,
    0xA000,
    0xFF ^ crate::cpu::flags::CARRY, // C flag cleared
    0x99EE,
    CallCC::Condition(Condition::C),
    0x1003,
    0xA000
)]
#[case::call_po_taken(
    0x1000,
    0xB000,
    0xFF ^ crate::cpu::flags::PARITY_OVERFLOW,
    0xAA11,
    CallCC::Condition(Condition::PO),
    0xAA11,
    0xAFFE
)]
#[case::call_po_not_taken(
    0x1000,
    0xC000,
    crate::cpu::flags::PARITY_OVERFLOW,
    0xAA11,
    CallCC::Condition(Condition::PO),
    0x1003,
    0xC000
)]
#[case::call_pe_taken(
    0x1000,
    0xD000,
    crate::cpu::flags::PARITY_OVERFLOW,
    0xBB22,
    CallCC::Condition(Condition::PE),
    0xBB22,
    0xCFFE
)]
#[case::call_pe_not_taken(
    0x1000,
    0xE000,
    0xFF ^ crate::cpu::flags::PARITY_OVERFLOW,
    0xBB22,
    CallCC::Condition(Condition::PE),
    0x1003,
    0xE000
)]
#[case::call_p_taken(
    0x1000,
    0xF000,
    0xFF ^ crate::cpu::flags::SIGN,
    0xCC33,
    CallCC::Condition(Condition::P),
    0xCC33,
    0xEFFE
)]
#[case::call_p_not_taken(
    0x1000,
    0x0100,
    crate::cpu::flags::SIGN,
    0xCC33,
    CallCC::Condition(Condition::P),
    0x1003,
    0x0100
)]
#[case::call_m_taken(
    0x1000,
    0x0200,
    crate::cpu::flags::SIGN,
    0xDD44,
    CallCC::Condition(Condition::M),
    0xDD44,
    0x01FE
)]
#[case::call_m_not_taken(
    0x1000,
    0x0300,
    0xFF ^ crate::cpu::flags::SIGN,
    0xDD44,
    CallCC::Condition(Condition::M),
    0x1003,
    0x0300
)]
fn test_call(
    #[case] initial_pc: u16,
    #[case] initial_sp: u16,
    #[case] initial_flags: u8,
    #[case] target_addr: u16,
    #[case] call_cc: CallCC,
    #[case] expected_final_pc: u16,
    #[case] expected_final_sp: u16,
) {
    let mut cpu = setup_cpu();

    cpu.PC = initial_pc;
    cpu.SP = initial_sp;
    cpu.set_register(GPR::F, initial_flags);

    let opcode = match call_cc {
        CallCC::None => CALL_OPCODE,
        CallCC::Condition(Condition::NZ) => CALL_NZ_OPCODE,
        CallCC::Condition(Condition::Z) => CALL_Z_OPCODE,
        CallCC::Condition(Condition::NC) => CALL_NC_OPCODE,
        CallCC::Condition(Condition::C) => CALL_C_OPCODE,
        CallCC::Condition(Condition::PO) => CALL_PO_OPCODE,
        CallCC::Condition(Condition::PE) => CALL_PE_OPCODE,
        CallCC::Condition(Condition::P) => CALL_P_OPCODE,
        CallCC::Condition(Condition::M) => CALL_M_OPCODE,
    };

    // Write Opcode
    cpu.memory.borrow_mut().write(cpu.PC, opcode);

    // Write Target Address (Little Endian)
    let low_byte = (target_addr & 0xFF) as u8;
    let high_byte = ((target_addr >> 8) & 0xFF) as u8;
    cpu.memory
        .borrow_mut()
        .write(cpu.PC.wrapping_add(1), low_byte);
    cpu.memory
        .borrow_mut()
        .write(cpu.PC.wrapping_add(2), high_byte);

    cpu.tick();

    // 1. Check PC and SP
    assert_eq!(
        cpu.PC, expected_final_pc,
        "PC mismatch: expected 0x{:04X}, got 0x{:04X}",
        expected_final_pc, cpu.PC
    );
    assert_eq!(
        cpu.SP, expected_final_sp,
        "SP mismatch: expected 0x{:04X}, got 0x{:04X}",
        expected_final_sp, cpu.SP
    );

    // 2. Check Stack Content (Only if the call was taken)
    if expected_final_sp != initial_sp {
        let ret_addr = initial_pc.wrapping_add(3);
        let ret_low = (ret_addr & 0xFF) as u8;
        let ret_high = ((ret_addr >> 8) & 0xFF) as u8;

        // In Little Endian, the value at SP should be Low, and SP+1 should be High
        // (Because we Pushed High first to SP-1, then Low to SP-2)
        let stack_low = cpu.memory.borrow().read(cpu.SP);
        let stack_high = cpu.memory.borrow().read(cpu.SP.wrapping_add(1));

        assert_eq!(stack_low, ret_low, "Stack Low Byte (Return Addr) mismatch");
        assert_eq!(
            stack_high, ret_high,
            "Stack High Byte (Return Addr) mismatch"
        );
    }
}
