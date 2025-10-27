use rstest::rstest;

use crate::cpu::SyncronousComponent;
use crate::cpu::tests::setup_cpu;

/*
 * Note: these tests are incomplete, for example  "DEC r[y]" is used for multiple opcodes.
 * It's better than nothing for now, but more logging will be added later on the helper functions.
*/

#[rstest]
#[case::nop(0x29f9, &[0x00], &["decode_unprefixed", "NOP"])] // NOP
#[case::ld_bc_n(0x29f9, &[0x01, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn"])] // LD BC, nn
#[case::ld_bci_a(0x29f9, &[0x02], &["decode_unprefixed", "LD (BC), A"])] // LD (BC), A
#[case::inc_bc(0x29f9, &[0x03], &["decode_unprefixed", "INC rp[p]"])] // INC BC
#[case::inc_b(0x29f9, &[0x04], &["decode_unprefixed", "INC r[y]"])] // INC B
#[case::dec_b(0x29f9, &[0x05], &["decode_unprefixed", "DEC r[y]"])] // DEC B
#[case::ld_b_n(0x29f9, &[0x06, 0x56], &["decode_unprefixed", "LD r[y], n"])] // LD B, n
#[case::rlca(0x29f9, &[0x07], &["decode_unprefixed", "RLCA"])] // RLCA
#[case::ex_af_afp(0x29f9, &[0x08], &["decode_unprefixed", "EX AF, AF'"])] // EX AF, AF'
#[case::add_hl_bc(0x29f9, &[0x09], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, BC
#[case::ld_a_bci(0x29f9, &[0x0a], &["decode_unprefixed", "LD A, (BC)"])] // LD A, (BC)
#[case::dec_bc(0x29f9, &[0x0b], &["decode_unprefixed", "DEC rp[p]"])] // DEC BC
#[case::inc_c(0x29f9, &[0x0c], &["decode_unprefixed", "INC r[y]"])] // INC C
#[case::dec_c(0x29f9, &[0x0d], &["decode_unprefixed", "DEC r[y]"])] // DEC C
#[case::ld_c_n(0x29f9, &[0x0e, 0x78], &["decode_unprefixed", "LD r[y], n"])] // LD C, n
#[case::rrca(0x29f9, &[0x0f], &["decode_unprefixed", "RRCA"])] // RRCA

#[case::djnz_d(0x29f9, &[0x10, 0xfe], &["decode_unprefixed", "DJNZ d"])] // DJNZ d
#[case::ld_de_n(0x29f9, &[0x11, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn"])] // LD DE, nn
#[case::ld_dei_a(0x29f9, &[0x12], &["decode_unprefixed", "LD (DE), A"])] // LD (DE), A
#[case::inc_de(0x29f9, &[0x13], &["decode_unprefixed", "INC rp[p]"])] // INC DE
#[case::inc_d(0x29f9, &[0x14], &["decode_unprefixed", "INC r[y]"])] // INC D
#[case::dec_d(0x29f9, &[0x15], &["decode_unprefixed", "DEC r[y]"])] // DEC D
#[case::ld_d_n(0x29f9, &[0x16, 0x9a], &["decode_unprefixed", "LD r[y], n"])] // LD D, n
#[case::rla(0x29f9, &[0x17], &["decode_unprefixed", "RLA"])] // RLA
#[case::jr_d(0x29f9, &[0x18, 0xfe], &["decode_unprefixed", "JR d"])] // JR d
#[case::add_hl_de(0x29f9, &[0x19], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, DE
#[case::ld_a_dei(0x29f9, &[0x1a], &["decode_unprefixed", "LD A, (DE)"])] // LD A, (DE)
#[case::dec_de(0x29f9, &[0x1b], &["decode_unprefixed", "DEC rp[p]"])] // DEC DE
#[case::inc_e(0x29f9, &[0x1c], &["decode_unprefixed", "INC r[y]"])] // INC E
#[case::dec_e(0x29f9, &[0x1d], &["decode_unprefixed", "DEC r[y]"])] // DEC E
#[case::ld_e_n(0x29f9, &[0x1e, 0xbc], &["decode_unprefixed", "LD r[y], n"])] // LD E, n
#[case::rra(0x29f9, &[0x1f], &["decode_unprefixed", "RRA"])] // RRA

#[case::jr_nz_d(0x29f9, &[0x20, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR NZ, d
#[case::ld_hl_nn(0x29f9, &[0x21, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn"])] // LD HL, nn
#[case::ld_nni_hl(0x29f9, &[0x22, 0x00, 0x80], &["decode_unprefixed", "LD (nn), HL/IX/IY"])] // LD (nn), HL
#[case::inc_hl(0x29f9, &[0x23], &["decode_unprefixed", "INC rp[p]"])] // INC HL
#[case::inc_h(0x29f9, &[0x24], &["decode_unprefixed", "INC r[y]"])] // INC H
#[case::dec_h(0x29f9, &[0x25], &["decode_unprefixed", "DEC r[y]"])] // DEC H
#[case::ld_h_n(0x29f9, &[0x26, 0x56], &["decode_unprefixed", "LD r[y], n"])] // LD H, n
#[case::daa(0x29f9, &[0x27], &["decode_unprefixed", "DAA"])] // DAA
#[case::jr_z_d(0x29f9, &[0x28, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR Z, d
#[case::add_hl_hl(0x29f9, &[0x29], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, HL
#[case::ld_hl_nni(0x29f9, &[0x2a, 0x00, 0x80], &["decode_unprefixed", "LD HL/IX/IY, (nn)"])] // LD HL, (nn)
#[case::dec_hl(0x29f9, &[0x2b], &["decode_unprefixed", "DEC rp[p]"])] // DEC HL
#[case::inc_l(0x29f9, &[0x2c], &["decode_unprefixed", "INC r[y]"])] // INC L
#[case::dec_l(0x29f9, &[0x2d], &["decode_unprefixed", "DEC r[y]"])] // DEC L
#[case::ld_l_n(0x29f9, &[0x2e, 0x78], &["decode_unprefixed", "LD r[y], n"])] // LD L, n
#[case::cpl(0x29f9, &[0x2f], &["decode_unprefixed", "CPL"])] // CPL

#[case::jr_nc_d(0x29f9, &[0x30, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR NC, d
#[case::ld_sp_nn(0x29f9, &[0x31, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn"])] // LD SP, nn
#[case::ld_nni_a(0x29f9, &[0x32, 0x00, 0x80], &["decode_unprefixed", "LD (nn), A"])] // LD (nn), A
#[case::inc_sp(0x29f9, &[0x33], &["decode_unprefixed", "INC rp[p]"])] // INC SP
#[case::inc_hli(0x29f9, &[0x34], &["decode_unprefixed", "INC r[y]"])] // INC (HL)
#[case::dec_hli(0x29f9, &[0x35], &["decode_unprefixed", "DEC r[y]"])] // DEC (HL)
#[case::ld_hli_n(0x29f9, &[0x36, 0x56], &["decode_unprefixed", "LD r[y], n"])] // LD (HL), n
#[case::scf(0x29f9, &[0x37], &["decode_unprefixed", "SCF"])] // SCF
#[case::jr_c_d(0x29f9, &[0x38, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR C, d
#[case::add_hl_sp(0x29f9, &[0x39], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, SP
#[case::ld_a_nni(0x29f9, &[0x3a, 0x00, 0x80], &["decode_unprefixed", "LD A, (nn)"])] // LD A, (nn)
#[case::dec_sp(0x29f9, &[0x3b], &["decode_unprefixed", "DEC rp[p]"])] // DEC SP
#[case::inc_lli(0x29f9, &[0x3c], &["decode_unprefixed", "INC r[y]"])] // INC A
#[case::dec_lli(0x29f9, &[0x3d], &["decode_unprefixed", "DEC r[y]"])] // DEC A
#[case::ld_a_n(0x29f9, &[0x3e, 0x78], &["decode_unprefixed", "LD r[y], n"])] // LD A, n
#[case::ccf(0x29f9, &[0x3f], &["decode_unprefixed", "CCF"])] // CCF
fn test_opcode(
    #[case] starting_pc: u16,
    #[case] memory_contents: &[u8],
    #[case] expected_logs: &[&str],
) {
    let mut cpu = setup_cpu();
    for (i, &byte) in memory_contents.iter().enumerate() {
        cpu.memory
            .borrow_mut()
            .write(starting_pc.wrapping_add(i as u16), byte);
    }
    cpu.PC = starting_pc;
    cpu.tick();

    assert_eq!(expected_logs.len(), cpu.test_callback.0.len());

    // zip logs and cpu logs
    let logs: Vec<&str> = cpu
        .test_callback
        .0
        .iter()
        .rev()
        .map(|s| s.as_str())
        .collect();
    for (expected, actual) in expected_logs.iter().zip(logs.iter()) {
        assert_eq!(expected, actual);
    }
}
