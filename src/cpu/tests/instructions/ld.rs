use crate::cpu::tests::setup_cpu;
use crate::cpu::*;
use rstest::rstest;

#[rstest]
// MAIN TABLE
#[case::ld_b_b(GPR::B, GPR::B, 0x42)] // ld B, B
#[case::ld_b_c(GPR::B, GPR::C, 0x34)] // ld B, C
#[case::ld_b_d(GPR::B, GPR::D, 0x56)] // ld B, D
#[case::ld_b_e(GPR::B, GPR::E, 0x78)] // ld B, E
#[case::ld_b_h(GPR::B, GPR::H, 0x9A)] // ld B, H
#[case::ld_b_l(GPR::B, GPR::L, 0xBC)] // ld B, L
#[case::ld_b_a(GPR::B, GPR::A, 0xFF)] // ld B, A
#[case::ld_c_b(GPR::C, GPR::B, 0x42)] // ld C, B
#[case::ld_c_c(GPR::C, GPR::C, 0x34)] // ld C, C
#[case::ld_c_d(GPR::C, GPR::D, 0x56)] // ld C, D
#[case::ld_c_e(GPR::C, GPR::E, 0x78)] // ld C, E
#[case::ld_c_h(GPR::C, GPR::H, 0x9A)] // ld C, H
#[case::ld_c_l(GPR::C, GPR::L, 0xBC)] // ld C, L
#[case::ld_c_a(GPR::C, GPR::A, 0xFF)] // ld C, A
#[case::ld_d_b(GPR::D, GPR::B, 0x42)] // ld D, B
#[case::ld_d_c(GPR::D, GPR::C, 0x34)] // ld D, C
#[case::ld_d_d(GPR::D, GPR::D, 0x56)] // ld D, D
#[case::ld_d_e(GPR::D, GPR::E, 0x78)] // ld D, E
#[case::ld_d_h(GPR::D, GPR::H, 0x9A)] // ld D, H
#[case::ld_d_l(GPR::D, GPR::L, 0xBC)] // ld D, L
#[case::ld_d_a(GPR::D, GPR::A, 0xFF)] // ld D, A
#[case::ld_e_b(GPR::E, GPR::B, 0x42)] // ld E, B
#[case::ld_e_c(GPR::E, GPR::C, 0x34)] // ld E, C
#[case::ld_e_d(GPR::E, GPR::D, 0x56)] // ld E, D
#[case::ld_e_e(GPR::E, GPR::E, 0x78)] // ld E, E
#[case::ld_e_h(GPR::E, GPR::H, 0x9A)] // ld E, H
#[case::ld_e_l(GPR::E, GPR::L, 0xBC)] // ld E, L
#[case::ld_e_a(GPR::E, GPR::A, 0xFF)] // ld E, A
#[case::ld_h_b(GPR::H, GPR::B, 0x42)] // ld H, B
#[case::ld_h_c(GPR::H, GPR::C, 0x34)] // ld H, C
#[case::ld_h_d(GPR::H, GPR::D, 0x56)] // ld H, D
#[case::ld_h_e(GPR::H, GPR::E, 0x78)] // ld H, E
#[case::ld_h_h(GPR::H, GPR::H, 0x9A)] // ld H, H
#[case::ld_h_l(GPR::H, GPR::L, 0xBC)] // ld H, L
#[case::ld_h_a(GPR::H, GPR::A, 0xFF)] // ld H, A
#[case::ld_l_b(GPR::L, GPR::B, 0x42)] // ld L, B
#[case::ld_l_c(GPR::L, GPR::C, 0x34)] // ld L, C
#[case::ld_l_d(GPR::L, GPR::D, 0x56)] // ld L, D
#[case::ld_l_e(GPR::L, GPR::E, 0x78)] // ld L, E
#[case::ld_l_h(GPR::L, GPR::H, 0x9A)] // ld L, H
#[case::ld_l_l(GPR::L, GPR::L, 0xBC)] // ld L, L
#[case::ld_l_a(GPR::L, GPR::A, 0xFF)] // ld L, A
#[case::ld_a_b(GPR::A, GPR::B, 0x42)] // ld A, B
#[case::ld_a_c(GPR::A, GPR::C, 0x34)] // ld A, C
#[case::ld_a_d(GPR::A, GPR::D, 0x56)] // ld A, D
#[case::ld_a_e(GPR::A, GPR::E, 0x78)] // ld A, E
#[case::ld_a_h(GPR::A, GPR::H, 0x9A)] // ld A, H
#[case::ld_a_l(GPR::A, GPR::L, 0xBC)] // ld A, L
#[case::ld_a_a(GPR::A, GPR::A, 0xFF)] // ld A, A
fn test_register_register(#[case] src: GPR, #[case] dest: GPR, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.set_register(src, value);
    cpu.ld(
        AddressingMode::Register(dest),
        AddressingMode::Register(src),
    );
    assert_eq!(cpu.get_register(dest), value);
}

#[rstest]
// MAIN TABLE
#[case::ld_b_n(GPR::B, 0x34)] // ld B, n
#[case::ld_d_n(GPR::D, 0x7F)] // ld D, n
#[case::ld_h_n(GPR::H, 0x56)] // ld H, n
#[case::ld_c_n(GPR::C, 0x19)] // ld C, n
#[case::ld_e_n(GPR::E, 0x88)] // ld E, n
#[case::ld_l_n(GPR::L, 0xAB)] // ld L, n
#[case::ld_a_n(GPR::A, 0xFF)] // ld A, n
fn test_register_immediate(#[case] dest: GPR, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.ld(
        AddressingMode::Register(dest),
        AddressingMode::Immediate(value),
    );
    assert_eq!(cpu.get_register(dest), value);
}

#[rstest]
// MAIN TABLE
#[case::ld_bc_nn(RegisterPair::BC, 0x12, 0x34)] // ld BC, nn
#[case::ld_de_nn(RegisterPair::DE, 0x56, 0x78)] // ld DE, nn
#[case::ld_hl_nn(RegisterPair::HL, 0x9A, 0xBC)] // ld HL, nn
#[case::ld_sp_nn(RegisterPair::SP, 0xDE, 0xF0)] // ld SP, nn
fn test_register_pair_immediate(#[case] pair: RegisterPair, #[case] low: u8, #[case] high: u8) {
    let mut cpu = setup_cpu();
    let val = ((high as u16) << 8) | (low as u16);
    cpu.ld_16(
        AddressingMode::RegisterPair(pair),
        AddressingMode::ImmediateExtended(val),
    );
    assert_eq!(cpu.get_register_pair(pair), val);
}

#[rstest]
// MAIN TABLE
#[case::ld_bc_nn(RegisterPair::BC, GPR::A, 0x5619, 0x42)] // ld (BC), A
#[case::ld_de_nn(RegisterPair::DE, GPR::A, 0x1234, 0x7F)] // ld (DE), A
#[case::ld_hl_a(RegisterPair::HL, GPR::A, 0x9ABC, 0x56)] // ld (HL), A
#[case::ld_hl_b(RegisterPair::HL, GPR::B, 0xDEF0, 0x78)] // ld (HL), B
#[case::ld_hl_c(RegisterPair::HL, GPR::C, 0x1234, 0x7F)] // ld (HL), C
#[case::ld_hl_d(RegisterPair::HL, GPR::D, 0x5678, 0x9A)] // ld (HL), D
#[case::ld_hl_e(RegisterPair::HL, GPR::E, 0x9ABC, 0xBC)] // ld (HL), E
#[case::ld_hl_h(RegisterPair::HL, GPR::H, 0xDEF0, 0xDE)] // ld (HL), H
#[case::ld_hl_l(RegisterPair::HL, GPR::L, 0x3456, 0x56)] // ld (HL), L //NOTE, for this one L must match the L in HL (otherwise it's impossible scenario)
fn test_register_pair_indirect_register(
    #[case] pair: RegisterPair,
    #[case] src: GPR,
    #[case] addr: u16,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.set_register(src, value);
    cpu.set_register_pair(pair, addr);
    cpu.ld(
        AddressingMode::RegisterIndirect(pair),
        AddressingMode::Register(src),
    );
    assert_eq!(cpu.memory.borrow().read(addr), value);
}

#[rstest]
// MAIN TABLE
#[case::ld_a_bc(GPR::A, RegisterPair::BC, 0x5619, 0x42)] // ld A, (BC)
#[case::ld_a_de(GPR::A, RegisterPair::DE, 0x1234, 0x7F)] // ld A, (DE)
#[case::ld_b_hl(GPR::B, RegisterPair::HL, 0x9ABC, 0x56)] // ld B, (HL)
#[case::ld_c_hl(GPR::C, RegisterPair::HL, 0xDEF0, 0x78)] // ld C, (HL)
#[case::ld_d_hl(GPR::D, RegisterPair::HL, 0x1234, 0x7F)] // ld D, (HL)
#[case::ld_e_hl(GPR::E, RegisterPair::HL, 0x5678, 0x9A)] // ld E, (HL)
#[case::ld_h_hl(GPR::H, RegisterPair::HL, 0x9ABC, 0xBC)] // ld H, (HL)
#[case::ld_l_hl(GPR::L, RegisterPair::HL, 0xDEF0, 0xDE)] // ld L, (HL)
#[case::ld_a_hl(GPR::A, RegisterPair::HL, 0x3456, 0xEF)] // ld A, (HL)
fn test_register_register_pair_indirect(
    #[case] src: GPR,
    #[case] pair: RegisterPair,
    #[case] addr: u16,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.memory.borrow_mut().write(addr, value);
    cpu.set_register_pair(pair, addr);
    cpu.ld(
        AddressingMode::Register(src),
        AddressingMode::RegisterIndirect(pair),
    );
    assert_eq!(cpu.get_register(src), value);
}

#[rstest]
// MAIN TABLE
#[case::ld_bc_n(RegisterPair::HL, 0x5619, 0x42)] // ld (HL), n
fn test_register_pair_indirect_immediate(
    #[case] pair: RegisterPair,
    #[case] addr: u16,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.set_register_pair(pair, addr);
    cpu.ld(
        AddressingMode::RegisterIndirect(pair),
        AddressingMode::Immediate(value),
    );
    assert_eq!(cpu.memory.borrow().read(addr), value);
}

#[rstest]
// MAIN TABLE
#[case::ld_a_nn(GPR::A, 0x1234, 0x7F)] // ld A, (nn)
fn test_register_absolute(#[case] dest: GPR, #[case] addr: u16, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.memory.borrow_mut().write(addr, value);
    cpu.ld(
        AddressingMode::Register(dest),
        AddressingMode::Absolute(addr),
    );
    assert_eq!(cpu.get_register(dest), value);
}

#[rstest]
// MAIN TABLE
#[case::ld_hl_nn(RegisterPair::HL, 0x9ABC, 0x56)] // ld HL, (nn)

// ED TABLE
#[case::ld_bc_nn(RegisterPair::BC, 0x5619, 0x42)] // ld BC, (nn)
#[case::ld_de_nn(RegisterPair::DE, 0x1234, 0x7F)] // ld DE, (nn)
#[case::ld_sp_nn(RegisterPair::SP, 0xDEF0, 0x78)] // ld SP, (nn)
fn test_register_pair_absolute(#[case] pair: RegisterPair, #[case] addr: u16, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.memory.borrow_mut().write(addr, value);
    cpu.ld_16(
        AddressingMode::RegisterPair(pair),
        AddressingMode::Absolute(addr),
    );
    assert_eq!(cpu.get_register_pair(pair), value as u16);
}

#[rstest]
// MAIN TABLE
#[case::ld_nn_a(0x1234, 0x7F)] // ld (nn), A
fn test_absolute_register(#[case] addr: u16, #[case] value: u8) {
    let mut cpu = setup_cpu();
    cpu.set_register(GPR::A, value);
    cpu.ld(
        AddressingMode::Absolute(addr),
        AddressingMode::Register(GPR::A),
    );
    assert_eq!(cpu.memory.borrow().read(addr), value);
}

#[rstest]
// ED TABLE
#[case::ld_nn_bc(0x5619, 0x42)] // ld (nn), BC
#[case::ld_nn_de(0x1234, 0x7F)] // ld (nn), DE
#[case::ld_nn_hl(0x9ABC, 0x56)] // ld (nn), HL
#[case::ld_nn_sp(0xDEF0, 0x78)] // ld (nn), SP
// DD TABLE
#[case::ld_nn_ix(0x3456, 0x9A)] // ld (nn), IX
fn test_absolute_register_pair(#[case] addr: u16, #[case] value: u16) {
    let mut cpu = setup_cpu();
    cpu.set_register_pair(RegisterPair::BC, value);
    cpu.ld_16(
        AddressingMode::Absolute(addr),
        AddressingMode::RegisterPair(RegisterPair::BC),
    );
    assert_eq!(cpu.memory.borrow().read_word(addr), value);
}

#[rstest]
// ED TABLE
#[case::ld_i_a(SpecialRegister::I, SpecialRegister::A)] // ld I, A
#[case::ld_a_i(SpecialRegister::A, SpecialRegister::I)] // ld A, I
#[case::ld_r_a(SpecialRegister::R, SpecialRegister::A)] // ld R, A
#[case::ld_a_r(SpecialRegister::A, SpecialRegister::R)] // ld A, R
fn test_special_register_special_register(
    #[case] dest: SpecialRegister,
    #[case] src: SpecialRegister,
) {
    let mut cpu = setup_cpu();
    cpu.set_special_register(src, 0x42);
    cpu.ld(
        AddressingMode::Special(dest),
        AddressingMode::Special(src),
    );
    assert_eq!(cpu.get_special_register(dest), 0x42);
}
#[rstest]
// DD TABLE
#[case::ld_ixh_n(SpecialRegister::IXH, 0x34)] // ld IXH, n
#[case::ld_ixl_n(SpecialRegister::IXL, 0x12)] // ld IXL, n
fn test_special_register_immediate(
    #[case] dest: SpecialRegister,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.ld(
        AddressingMode::Special(dest),
        AddressingMode::Immediate(value),
    );
    assert_eq!(cpu.get_special_register(dest) as u8 , value);
}

#[rstest]
// DD TABLE
#[case::ld_ix_nn(SpecialRegister::IX, 0x3412)] // ld IX, (nn)
fn test_special_register_immediate_extended(
    #[case] dest: SpecialRegister,
    #[case] value: u16,
) {
    let mut cpu = setup_cpu();
    cpu.ld_16(
        AddressingMode::Special(dest),
        AddressingMode::ImmediateExtended(value),
    );
    assert_eq!(cpu.get_special_register(dest), value);
}

#[rstest]
// DD TABLE
#[case::ld_ixd_n(IndexRegister::IX, 0x1234, 0x42)] // ld (IX+d), n
fn test_indexed_immediate(
    #[case] base: IndexRegister,
    #[case] addr: u16,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.set_index_register(base, addr);
    cpu.ld(
        AddressingMode::Indexed(base, 0),
        AddressingMode::Immediate(value),
    );
    assert_eq!(cpu.memory.borrow().read(addr), value);
}

#[rstest]
// DD TABLE
#[case::ld_b_ixh(GPR::B, SpecialRegister::IXH, 0x12)] // ld B, IXH
#[case::ld_b_ixl(GPR::B, SpecialRegister::IXL, 0x34)] // ld B, IXL
#[case::ld_c_ixh(GPR::C, SpecialRegister::IXH, 0x12)] // ld C, IXH
#[case::ld_c_ixl(GPR::C, SpecialRegister::IXL, 0x34)] // ld C, IXL

fn test_register_special_register(
    #[case] dest: GPR,
    #[case] src: SpecialRegister,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.set_special_register(src, value as u16);
    cpu.ld(
        AddressingMode::Register(dest),
        AddressingMode::Special(src),
    );
    assert_eq!(cpu.get_register(dest) , value);
}

#[rstest]
// DD TABLE
#[case::ld_ixh_b(SpecialRegister::IXH, GPR::B, 0x34)] // ld IXH, B
fn test_special_register_register(
    #[case] dest: SpecialRegister,
    #[case] src: GPR,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.set_register(src, value);
    cpu.ld(
        AddressingMode::Special(dest),
        AddressingMode::Register(src),
    );
    assert_eq!(cpu.get_special_register(dest) as u8 , value);
}

#[rstest]
// DD TABLE
#[case::ld_b_ixd(GPR::B, IndexRegister::IX, 0x1234, 0x42, 0x01)] // ld B, (IX+d)
#[case::ld_c_ixd(GPR::C, IndexRegister::IX, 0x1234, 0x56, 0xFE)] // ld C, (IX+d)
#[case::ld_d_ixd(GPR::D, IndexRegister::IX, 0x1234, 0x78, 0x10)] // ld D, (IX+d)
#[case::ld_e_ixd(GPR::E, IndexRegister::IX, 0x1234, 0x4A, 0xF0)] // ld E, (IX+d)
#[case::ld_h_ixd(GPR::H, IndexRegister::IX, 0x1234, 0x4C, 0x7F)] // ld H, (IX+d)
#[case::ld_l_ixd(GPR::L, IndexRegister::IX, 0x1234, 0x3E, 0x80)] // ld L, (IX+d)
#[case::ld_a_ixd(GPR::A, IndexRegister::IX, 0x1234, 0x4F, 0x00)] // ld A, (IX+d)
fn test_register_indexed(
    #[case] dest: GPR,
    #[case] base: IndexRegister,
    #[case] addr: u16,
    #[case] displacement: i8,
    #[case] value: u8,
) {
    let mut cpu = setup_cpu();
    cpu.memory.borrow_mut().write(addr.wrapping_add_signed(displacement as i16), value);
    cpu.set_index_register(base, addr);
    cpu.ld(
        AddressingMode::Register(dest),
        AddressingMode::Indexed(base, displacement),
    );
    assert_eq!(cpu.get_register(dest), value);
}
