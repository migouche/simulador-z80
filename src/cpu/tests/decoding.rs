use rstest::rstest;

use crate::cpu::SyncronousComponent;
use crate::cpu::tests::setup_cpu;

/*
 * Note: these tests are incomplete, for example  "DEC r[y]" is used for multiple opcodes.
 * It's better than nothing for now, but more logging will be added later on the helper functions.
*/

#[rstest]
#[case::nop(0x29f9, &[0x00], &["decode_unprefixed", "NOP"])] // NOP
#[case::ld_bc_nn(0x29f9, &[0x01, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn", "BC"])] // LD BC, nn
#[case::ld_bci_a(0x29f9, &[0x02], &["decode_unprefixed", "LD (BC), A"])] // LD (BC), A
#[case::inc_bc(0x29f9, &[0x03], &["decode_unprefixed", "INC rp[p]"])] // INC BC
#[case::inc_b(0x29f9, &[0x04], &["decode_unprefixed", "INC r[y]"])] // INC B
#[case::dec_b(0x29f9, &[0x05], &["decode_unprefixed", "DEC r[y]"])] // DEC B
#[case::ld_b_n(0x29f9, &[0x06, 0x56], &["decode_unprefixed", "LD r[y], n", "B"])] // LD B, n
#[case::rlca(0x29f9, &[0x07], &["decode_unprefixed", "RLCA"])] // RLCA
#[case::ex_af_afp(0x29f9, &[0x08], &["decode_unprefixed", "EX AF, AF'"])] // EX AF, AF'
#[case::add_hl_bc(0x29f9, &[0x09], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, BC
#[case::ld_a_bci(0x29f9, &[0x0a], &["decode_unprefixed", "LD A, (BC)"])] // LD A, (BC)
#[case::dec_bc(0x29f9, &[0x0b], &["decode_unprefixed", "DEC rp[p]"])] // DEC BC
#[case::inc_c(0x29f9, &[0x0c], &["decode_unprefixed", "INC r[y]"])] // INC C
#[case::dec_c(0x29f9, &[0x0d], &["decode_unprefixed", "DEC r[y]"])] // DEC C
#[case::ld_c_n(0x29f9, &[0x0e, 0x78], &["decode_unprefixed", "LD r[y], n", "C"])] // LD C, n
#[case::rrca(0x29f9, &[0x0f], &["decode_unprefixed", "RRCA"])] // RRCA

#[case::djnz_d(0x29f9, &[0x10, 0xfe], &["decode_unprefixed", "DJNZ d"])] // DJNZ d
#[case::ld_de_n(0x29f9, &[0x11, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn", "DE"])] // LD DE, nn
#[case::ld_dei_a(0x29f9, &[0x12], &["decode_unprefixed", "LD (DE), A"])] // LD (DE), A
#[case::inc_de(0x29f9, &[0x13], &["decode_unprefixed", "INC rp[p]"])] // INC DE
#[case::inc_d(0x29f9, &[0x14], &["decode_unprefixed", "INC r[y]"])] // INC D
#[case::dec_d(0x29f9, &[0x15], &["decode_unprefixed", "DEC r[y]"])] // DEC D
#[case::ld_d_n(0x29f9, &[0x16, 0x9a], &["decode_unprefixed", "LD r[y], n", "D"])] // LD D, n
#[case::rla(0x29f9, &[0x17], &["decode_unprefixed", "RLA"])] // RLA
#[case::jr_d(0x29f9, &[0x18, 0xfe], &["decode_unprefixed", "JR d"])] // JR d
#[case::add_hl_de(0x29f9, &[0x19], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, DE
#[case::ld_a_dei(0x29f9, &[0x1a], &["decode_unprefixed", "LD A, (DE)"])] // LD A, (DE)
#[case::dec_de(0x29f9, &[0x1b], &["decode_unprefixed", "DEC rp[p]"])] // DEC DE
#[case::inc_e(0x29f9, &[0x1c], &["decode_unprefixed", "INC r[y]"])] // INC E
#[case::dec_e(0x29f9, &[0x1d], &["decode_unprefixed", "DEC r[y]"])] // DEC E
#[case::ld_e_n(0x29f9, &[0x1e, 0xbc], &["decode_unprefixed", "LD r[y], n", "E"])] // LD E, n
#[case::rra(0x29f9, &[0x1f], &["decode_unprefixed", "RRA"])] // RRA

#[case::jr_nz_d(0x29f9, &[0x20, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR NZ, d
#[case::ld_hl_nn(0x29f9, &[0x21, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn", "HL"])] // LD HL, nn
#[case::ld_nni_hl(0x29f9, &[0x22, 0x00, 0x80], &["decode_unprefixed", "LD (nn), HL/IX/IY"])] // LD (nn), HL
#[case::inc_hl(0x29f9, &[0x23], &["decode_unprefixed", "INC rp[p]"])] // INC HL
#[case::inc_h(0x29f9, &[0x24], &["decode_unprefixed", "INC r[y]"])] // INC H
#[case::dec_h(0x29f9, &[0x25], &["decode_unprefixed", "DEC r[y]"])] // DEC H
#[case::ld_h_n(0x29f9, &[0x26, 0x56], &["decode_unprefixed", "LD r[y], n", "H"])] // LD H, n
#[case::daa(0x29f9, &[0x27], &["decode_unprefixed", "DAA"])] // DAA
#[case::jr_z_d(0x29f9, &[0x28, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR Z, d
#[case::add_hl_hl(0x29f9, &[0x29], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, HL
#[case::ld_hl_nni(0x29f9, &[0x2a, 0x00, 0x80], &["decode_unprefixed", "LD HL/IX/IY, (nn)"])] // LD HL, (nn)
#[case::dec_hl(0x29f9, &[0x2b], &["decode_unprefixed", "DEC rp[p]"])] // DEC HL
#[case::inc_l(0x29f9, &[0x2c], &["decode_unprefixed", "INC r[y]"])] // INC L
#[case::dec_l(0x29f9, &[0x2d], &["decode_unprefixed", "DEC r[y]"])] // DEC L
#[case::ld_l_n(0x29f9, &[0x2e, 0x78], &["decode_unprefixed", "LD r[y], n", "L"])] // LD L, n
#[case::cpl(0x29f9, &[0x2f], &["decode_unprefixed", "CPL"])] // CPL

#[case::jr_nc_d(0x29f9, &[0x30, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR NC, d
#[case::ld_sp_nn(0x29f9, &[0x31, 0x34, 0x12], &["decode_unprefixed", "LD rp[p], nn", "SP"])] // LD SP, nn
#[case::ld_nni_a(0x29f9, &[0x32, 0x00, 0x80], &["decode_unprefixed", "LD (nn), A"])] // LD (nn), A
#[case::inc_sp(0x29f9, &[0x33], &["decode_unprefixed", "INC rp[p]"])] // INC SP
#[case::inc_hli(0x29f9, &[0x34], &["decode_unprefixed", "INC r[y]"])] // INC (HL)
#[case::dec_hli(0x29f9, &[0x35], &["decode_unprefixed", "DEC r[y]"])] // DEC (HL)
#[case::ld_hli_n(0x29f9, &[0x36, 0x56], &["decode_unprefixed", "LD r[y], n", "(HL)"])] // LD (HL), n
#[case::scf(0x29f9, &[0x37], &["decode_unprefixed", "SCF"])] // SCF
#[case::jr_c_d(0x29f9, &[0x38, 0xfe], &["decode_unprefixed", "JR cc[y-4], d"])] // JR C, d
#[case::add_hl_sp(0x29f9, &[0x39], &["decode_unprefixed", "ADD HL, rp[p]"])] // ADD HL, SP
#[case::ld_a_nni(0x29f9, &[0x3a, 0x00, 0x80], &["decode_unprefixed", "LD A, (nn)"])] // LD A, (nn)
#[case::dec_sp(0x29f9, &[0x3b], &["decode_unprefixed", "DEC rp[p]"])] // DEC SP
#[case::inc_lli(0x29f9, &[0x3c], &["decode_unprefixed", "INC r[y]"])] // INC A
#[case::dec_lli(0x29f9, &[0x3d], &["decode_unprefixed", "DEC r[y]"])] // DEC A
#[case::ld_a_n(0x29f9, &[0x3e, 0x78], &["decode_unprefixed", "LD r[y], n", "A"])] // LD A, n
#[case::ccf(0x29f9, &[0x3f], &["decode_unprefixed", "CCF"])] // CCF

#[case::ld_b_b(0x29f9, &[0x40], &["decode_unprefixed", "LD r[y], r[z]", "B", "B"])] // LD B, B
#[case::ld_b_c(0x29f9, &[0x41], &["decode_unprefixed", "LD r[y], r[z]", "B", "C"])] // LD B, C
#[case::ld_b_d(0x29f9, &[0x42], &["decode_unprefixed", "LD r[y], r[z]", "B", "D"])] // LD B, D
#[case::ld_b_e(0x29f9, &[0x43], &["decode_unprefixed", "LD r[y], r[z]", "B", "E"])] // LD B, E
#[case::ld_b_h(0x29f9, &[0x44], &["decode_unprefixed", "LD r[y], r[z]", "B", "H"])] // LD B, H
#[case::ld_b_l(0x29f9, &[0x45], &["decode_unprefixed", "LD r[y], r[z]", "B", "L"])] // LD B, L
#[case::ld_b_hli(0x29f9, &[0x46], &["decode_unprefixed", "LD r[y], r[z]", "B", "(HL)"])] // LD B, (HL)
#[case::ld_b_a(0x29f9, &[0x47], &["decode_unprefixed", "LD r[y], r[z]", "B", "A"])] // LD B, A
#[case::ld_c_b(0x29f9, &[0x48], &["decode_unprefixed", "LD r[y], r[z]", "C", "B"])] // LD C, B
#[case::ld_c_c(0x29f9, &[0x49], &["decode_unprefixed", "LD r[y], r[z]", "C", "C"])] // LD C, C
#[case::ld_c_d(0x29f9, &[0x4a], &["decode_unprefixed", "LD r[y], r[z]", "C", "D"])] // LD C, D
#[case::ld_c_e(0x29f9, &[0x4b], &["decode_unprefixed", "LD r[y], r[z]", "C", "E"])] // LD C, E
#[case::ld_c_h(0x29f9, &[0x4c], &["decode_unprefixed", "LD r[y], r[z]", "C", "H"])] // LD C, H
#[case::ld_c_l(0x29f9, &[0x4d], &["decode_unprefixed", "LD r[y], r[z]", "C", "L"])] // LD C, L
#[case::ld_c_hli(0x29f9, &[0x4e], &["decode_unprefixed", "LD r[y], r[z]", "C", "(HL)"])] // LD C, (HL)
#[case::ld_c_a(0x29f9, &[0x4f], &["decode_unprefixed", "LD r[y], r[z]", "C", "A"])] // LD C, A

#[case::ld_d_b(0x29f9, &[0x50], &["decode_unprefixed", "LD r[y], r[z]", "D", "B"])] // LD D, B
#[case::ld_d_c(0x29f9, &[0x51], &["decode_unprefixed", "LD r[y], r[z]", "D", "C"])] // LD D, C
#[case::ld_d_d(0x29f9, &[0x52], &["decode_unprefixed", "LD r[y], r[z]", "D", "D"])] // LD D, D
#[case::ld_d_e(0x29f9, &[0x53], &["decode_unprefixed", "LD r[y], r[z]", "D", "E"])] // LD D, E
#[case::ld_d_h(0x29f9, &[0x54], &["decode_unprefixed", "LD r[y], r[z]", "D", "H"])] // LD D, H
#[case::ld_d_l(0x29f9, &[0x55], &["decode_unprefixed", "LD r[y], r[z]", "D", "L"])] // LD D, L
#[case::ld_d_hli(0x29f9, &[0x56], &["decode_unprefixed", "LD r[y], r[z]", "D", "(HL)"])] // LD D, (HL)
#[case::ld_d_a(0x29f9, &[0x57], &["decode_unprefixed", "LD r[y], r[z]", "D", "A"])] // LD D, A
#[case::ld_e_b(0x29f9, &[0x58], &["decode_unprefixed", "LD r[y], r[z]", "E", "B"])] // LD E, B
#[case::ld_e_c(0x29f9, &[0x59], &["decode_unprefixed", "LD r[y], r[z]", "E", "C"])] // LD E, C
#[case::ld_e_d(0x29f9, &[0x5a], &["decode_unprefixed", "LD r[y], r[z]", "E", "D"])] // LD E, D
#[case::ld_e_e(0x29f9, &[0x5b], &["decode_unprefixed", "LD r[y], r[z]", "E", "E"])] // LD E, E
#[case::ld_e_h(0x29f9, &[0x5c], &["decode_unprefixed", "LD r[y], r[z]", "E", "H"])] // LD E, H
#[case::ld_e_l(0x29f9, &[0x5d], &["decode_unprefixed", "LD r[y], r[z]", "E", "L"])] // LD E, L
#[case::ld_e_hli(0x29f9, &[0x5e], &["decode_unprefixed", "LD r[y], r[z]", "E", "(HL)"])] // LD E, (HL)
#[case::ld_e_a(0x29f9, &[0x5f], &["decode_unprefixed", "LD r[y], r[z]", "E", "A"])] // LD E, A

#[case::ld_h_b(0x29f9, &[0x60], &["decode_unprefixed", "LD r[y], r[z]", "H", "B"])] // LD H, B
#[case::ld_h_c(0x29f9, &[0x61], &["decode_unprefixed", "LD r[y], r[z]", "H", "C"])] // LD H, C
#[case::ld_h_d(0x29f9, &[0x62], &["decode_unprefixed", "LD r[y], r[z]", "H", "D"])] // LD H, D
#[case::ld_h_e(0x29f9, &[0x63], &["decode_unprefixed", "LD r[y], r[z]", "H", "E"])] // LD H, E
#[case::ld_h_h(0x29f9, &[0x64], &["decode_unprefixed", "LD r[y], r[z]", "H", "H"])] // LD H, H
#[case::ld_h_l(0x29f9, &[0x65], &["decode_unprefixed", "LD r[y], r[z]", "H", "L"])] // LD H, L
#[case::ld_h_hli(0x29f9, &[0x66], &["decode_unprefixed", "LD r[y], r[z]", "H", "(HL)"])] // LD H, (HL)
#[case::ld_h_a(0x29f9, &[0x67], &["decode_unprefixed", "LD r[y], r[z]", "H", "A"])] // LD H, A
#[case::ld_l_b(0x29f9, &[0x68], &["decode_unprefixed", "LD r[y], r[z]", "L", "B"])] // LD L, B
#[case::ld_l_c(0x29f9, &[0x69], &["decode_unprefixed", "LD r[y], r[z]", "L", "C"])] // LD L, C
#[case::ld_l_d(0x29f9, &[0x6a], &["decode_unprefixed", "LD r[y], r[z]", "L", "D"])] // LD L, D
#[case::ld_l_e(0x29f9, &[0x6b], &["decode_unprefixed", "LD r[y], r[z]", "L", "E"])] // LD L, E
#[case::ld_l_h(0x29f9, &[0x6c], &["decode_unprefixed", "LD r[y], r[z]", "L", "H"])] // LD L, H
#[case::ld_l_l(0x29f9, &[0x6d], &["decode_unprefixed", "LD r[y], r[z]", "L", "L"])] // LD L, L
#[case::ld_l_hli(0x29f9, &[0x6e], &["decode_unprefixed", "LD r[y], r[z]", "L", "(HL)"])] // LD L, (HL)
#[case::ld_l_a(0x29f9, &[0x6f], &["decode_unprefixed", "LD r[y], r[z]", "L", "A"])] // LD L, A

#[case::ld_hli_b(0x29f9, &[0x70], &["decode_unprefixed", "LD r[y], r[z]", "(HL)", "B"])] // LD (HL), B
#[case::ld_hli_c(0x29f9, &[0x71], &["decode_unprefixed", "LD r[y], r[z]", "(HL)", "C"])] // LD (HL), C
#[case::ld_hli_d(0x29f9, &[0x72], &["decode_unprefixed", "LD r[y], r[z]", "(HL)", "D"])] // LD (HL), D
#[case::ld_hli_e(0x29f9, &[0x73], &["decode_unprefixed", "LD r[y], r[z]", "(HL)", "E"])] // LD (HL), E
#[case::ld_hli_h(0x29f9, &[0x74], &["decode_unprefixed", "LD r[y], r[z]", "(HL)", "H"])] // LD (HL), H
#[case::ld_hli_l(0x29f9, &[0x75], &["decode_unprefixed", "LD r[y], r[z]", "(HL)", "L"])] // LD (HL), L
#[case::halt(0x29f9, &[0x76], &["decode_unprefixed", "HALT"])] // HALT
#[case::ld_hli_a(0x29f9, &[0x77], &["decode_unprefixed", "LD r[y], r[z]", "(HL)", "A"])] // LD (HL), A
#[case::ld_a_b(0x29f9, &[0x78], &["decode_unprefixed", "LD r[y], r[z]", "A", "B"])] // LD A, B
#[case::ld_a_c(0x29f9, &[0x79], &["decode_unprefixed", "LD r[y], r[z]", "A", "C"])] // LD A, C
#[case::ld_a_d(0x29f9, &[0x7a], &["decode_unprefixed", "LD r[y], r[z]", "A", "D"])] // LD A, D
#[case::ld_a_e(0x29f9, &[0x7b], &["decode_unprefixed", "LD r[y], r[z]", "A", "E"])] // LD A, E
#[case::ld_a_h(0x29f9, &[0x7c], &["decode_unprefixed", "LD r[y], r[z]", "A", "H"])] // LD A, H
#[case::ld_a_l(0x29f9, &[0x7d], &["decode_unprefixed", "LD r[y], r[z]", "A", "L"])] // LD A, L
#[case::ld_a_hli(0x29f9, &[0x7e], &["decode_unprefixed", "LD r[y], r[z]", "A", "(HL)"])] // LD A, (HL)
#[case::ld_a_a(0x29f9, &[0x7f], &["decode_unprefixed", "LD r[y], r[z]", "A", "A"])] // LD A, A

#[case::add_a_b(0x29f9, &[0x80], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, B
#[case::add_a_c(0x29f9, &[0x81], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, C
#[case::add_a_d(0x29f9, &[0x82], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, D
#[case::add_a_e(0x29f9, &[0x83], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, E
#[case::add_a_h(0x29f9, &[0x84], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, H
#[case::add_a_l(0x29f9, &[0x85], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, L
#[case::add_a_hli(0x29f9, &[0x86], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, (HL)
#[case::add_a_a(0x29f9, &[0x87], &["decode_unprefixed", "ALU[y] r[z]"])] // ADD A, A
#[case::adc_a_b(0x29f9, &[0x88], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, B
#[case::adc_a_c(0x29f9, &[0x89], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, C
#[case::adc_a_d(0x29f9, &[0x8a], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, D
#[case::adc_a_e(0x29f9, &[0x8b], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, E
#[case::adc_a_h(0x29f9, &[0x8c], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, H
#[case::adc_a_l(0x29f9, &[0x8d], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, L
#[case::adc_a_hli(0x29f9, &[0x8e], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, (HL)
#[case::adc_a_a(0x29f9, &[0x8f], &["decode_unprefixed", "ALU[y] r[z]"])] // ADC A, A

#[case::sub_a_b(0x29f9, &[0x90], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, B
#[case::sub_a_c(0x29f9, &[0x91], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, C
#[case::sub_a_d(0x29f9, &[0x92], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, D
#[case::sub_a_e(0x29f9, &[0x93], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, E
#[case::sub_a_h(0x29f9, &[0x94], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, H
#[case::sub_a_l(0x29f9, &[0x95], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, L
#[case::sub_a_hli(0x29f9, &[0x96], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, (HL)
#[case::sub_a_a(0x29f9, &[0x97], &["decode_unprefixed", "ALU[y] r[z]"])] // SUB A, A
#[case::sbc_a_b(0x29f9, &[0x98], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, B
#[case::sbc_a_c(0x29f9, &[0x99], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, C
#[case::sbc_a_d(0x29f9, &[0x9a], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, D
#[case::sbc_a_e(0x29f9, &[0x9b], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, E
#[case::sbc_a_h(0x29f9, &[0x9c], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, H
#[case::sbc_a_l(0x29f9, &[0x9d], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, L
#[case::sbc_a_hli(0x29f9, &[0x9e], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, (HL)
#[case::sbc_a_a(0x29f9, &[0x9f], &["decode_unprefixed", "ALU[y] r[z]"])] // SBC A, A

#[case::and_a_b(0x29f9, &[0xa0], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, B
#[case::and_a_c(0x29f9, &[0xa1], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, C
#[case::and_a_d(0x29f9, &[0xa2], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, D
#[case::and_a_e(0x29f9, &[0xa3], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, E
#[case::and_a_h(0x29f9, &[0xa4], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, H
#[case::and_a_l(0x29f9, &[0xa5], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, L
#[case::and_a_hli(0x29f9, &[0xa6], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, (HL)
#[case::and_a_a(0x29f9, &[0xa7], &["decode_unprefixed", "ALU[y] r[z]"])] // AND A, A
#[case::xor_a_b(0x29f9, &[0xa8], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, B
#[case::xor_a_c(0x29f9, &[0xa9], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, C
#[case::xor_a_d(0x29f9, &[0xaa], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, D
#[case::xor_a_e(0x29f9, &[0xab], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, E
#[case::xor_a_h(0x29f9, &[0xac], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, H
#[case::xor_a_l(0x29f9, &[0xad], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, L
#[case::xor_a_hli(0x29f9, &[0xae], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, (HL)
#[case::xor_a_a(0x29f9, &[0xaf], &["decode_unprefixed", "ALU[y] r[z]"])] // XOR A, A

#[case::or_a_b(0x29f9, &[0xb0], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, B
#[case::or_a_c(0x29f9, &[0xb1], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, C
#[case::or_a_d(0x29f9, &[0xb2], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, D
#[case::or_a_e(0x29f9, &[0xb3], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, E
#[case::or_a_h(0x29f9, &[0xb4], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, H
#[case::or_a_l(0x29f9, &[0xb5], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, L
#[case::or_a_hli(0x29f9, &[0xb6], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, (HL)
#[case::or_a_a(0x29f9, &[0xb7], &["decode_unprefixed", "ALU[y] r[z]"])] // OR A, A
#[case::cp_a_b(0x29f9, &[0xb8], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, B
#[case::cp_a_c(0x29f9, &[0xb9], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, C
#[case::cp_a_d(0x29f9, &[0xba], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, D
#[case::cp_a_e(0x29f9, &[0xbb], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, E
#[case::cp_a_h(0x29f9, &[0xbc], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, H
#[case::cp_a_l(0x29f9, &[0xbd], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, L
#[case::cp_a_hli(0x29f9, &[0xbe], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, (HL)
#[case::cp_a_a(0x29f9, &[0xbf], &["decode_unprefixed", "ALU[y] r[z]"])] // CP A, A

#[case::ret_nz(0x29f9, &[0xc0], &["decode_unprefixed", "RET cc[y]"])] // RET NZ
#[case::pop_bc(0x29f9, &[0xc1], &["decode_unprefixed", "POP rp2[p]"])] // POP BC
#[case::jp_nz_nn(0x29f9, &[0xc2, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP NZ, nn
#[case::jp_nn(0x29f9, &[0xc3, 0x34, 0x12], &["decode_unprefixed", "JP nn"])] // JP nn
#[case::call_nz_nn(0x29f9, &[0xc4, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL NZ, nn
#[case::push_bc(0x29f9, &[0xc5], &["decode_unprefixed", "PUSH rp2[p]"])] // PUSH BC
#[case::add_a_n(0x29f9, &[0xc6, 0x56], &["decode_unprefixed", "ALU[y] n"])] // ADD A, n
#[case::rst_00h(0x29f9, &[0xc7], &["decode_unprefixed", "RST y*8"])] // RST 00h
#[case::ret_z(0x29f9, &[0xc8], &["decode_unprefixed", "RET cc[y]"])] // RET Z
#[case::ret(0x29f9, &[0xc9], &["decode_unprefixed", "RET"])] // RET
#[case::jp_z_nn(0x29f9, &[0xca, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP Z, nn
#[case::cb_prefix(0x29f9, &[0xcb, 0x00], &["decode_cb", "rot[y] r[z]", "RLC", "B"])] // CB Prefix (RLC B)
#[case::call_z_nn(0x29f9, &[0xcc, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL Z, nn
#[case::call_nn(0x29f9, &[0xcd, 0x34, 0x12], &["decode_unprefixed", "CALL nn"])] // CALL nn
#[case::adc_a_n(0x29f9, &[0xce, 0x56], &["decode_unprefixed", "ALU[y] n"])] // ADC A, n
#[case::rst_08h(0x29f9, &[0xcf], &["decode_unprefixed", "RST y*8"])] // RST 08h

#[case::ret_nc(0x29f9, &[0xd0], &["decode_unprefixed", "RET cc[y]"])] // RET NC
#[case::pop_de(0x29f9, &[0xd1], &["decode_unprefixed", "POP rp2[p]"])] // POP DE
#[case::jp_nc_nn(0x29f9, &[0xd2, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP NC, nn
#[case::out_n_a(0x29f9, &[0xd3, 0x56], &["decode_unprefixed", "OUT (n), A"])] // OUT (n), A
#[case::call_nc_nn(0x29f9, &[0xd4, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL NC, nn
#[case::push_de(0x29f9, &[0xd5], &["decode_unprefixed", "PUSH rp2[p]"])] // PUSH DE
#[case::sub_a_n(0x29f9, &[0xd6, 0x56], &["decode_unprefixed", "ALU[y] n"])] // SUB A, n
#[case::rst_10h(0x29f9, &[0xd7], &["decode_unprefixed", "RST y*8"])] // RST 10h
#[case::ret_c(0x29f9, &[0xd8], &["decode_unprefixed", "RET cc[y]"])] // RET C
#[case::exx(0x29f9, &[0xd9], &["decode_unprefixed", "EXX"])] // EXX
#[case::jp_c_nn(0x29f9, &[0xda, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP C, nn
#[case::in_a_n(0x29f9, &[0xdb, 0x56], &["decode_unprefixed", "IN A, (n)"])] // IN A, (n)
#[case::call_c_nn(0x29f9, &[0xdc, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL C, nn
#[case::dd_prefix(0x29f9, &[0xdd, 0x00], &["decode_dd", "decode_unprefixed", "NOP"])] // DD Prefix (NOP)
#[case::sbc_a_n(0x29f9, &[0xde, 0x56], &["decode_unprefixed", "ALU[y] n"])] // SBC A, n
#[case::rst_18h(0x29f9, &[0xdf], &["decode_unprefixed", "RST y*8"])] // RST 18h

#[case::ret_po(0x29f9, &[0xe0], &["decode_unprefixed", "RET cc[y]"])] // RET PO
#[case::pop_hl(0x29f9, &[0xe1], &["decode_unprefixed", "POP rp2[p]"])] // POP HL
#[case::jp_po_nn(0x29f9, &[0xe2, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP PO, nn
#[case::ex_sp_hl(0x29f9, &[0xe3], &["decode_unprefixed", "EX (SP), HL/IX/IY"])] // EX (SP), HL/IX/IY
#[case::call_po_nn(0x29f9, &[0xe4, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL PO, nn
#[case::push_hl(0x29f9, &[0xe5], &["decode_unprefixed", "PUSH rp2[p]"])] // PUSH HL
#[case::and_a_n(0x29f9, &[0xe6, 0x56], &["decode_unprefixed", "ALU[y] n"])] // AND A, n
#[case::rst_20h(0x29f9, &[0xe7], &["decode_unprefixed", "RST y*8"])] // RST 20h
#[case::ret_pe(0x29f9, &[0xe8], &["decode_unprefixed", "RET cc[y]"])] // RET PE
#[case::jp_hl(0x29f9, &[0xe9], &["decode_unprefixed", "JP HL"])] // JP HL
#[case::jp_pe_nn(0x29f9, &[0xea, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP PE, nn
#[case::ex_de_hl(0x29f9, &[0xeb], &["decode_unprefixed", "EX DE, HL"])] // EX DE, HL NOTE: this is the unaffected by prefixes
#[case::call_pe_nn(0x29f9, &[0xec, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL PE, nn
#[case::ed_prefix(0x29f9, &[0xed, 0x00], &["decode_ed", "NONI"])] // ED Prefix (NONI)
#[case::xor_a_n(0x29f9, &[0xee, 0x56], &["decode_unprefixed", "ALU[y] n"])] // XOR A, n
#[case::rst_28h(0x29f9, &[0xef], &["decode_unprefixed", "RST y*8"])] // RST 28h

#[case::ret_p(0x29f9, &[0xf0], &["decode_unprefixed", "RET cc[y]"])] // RET P
#[case::pop_af(0x29f9, &[0xf1], &["decode_unprefixed", "POP rp2[p]"])] // POP AF
#[case::jp_p_nn(0x29f9, &[0xf2, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP P, nn
#[case::di(0x29f9, &[0xf3], &["decode_unprefixed", "DI"])] // DI
#[case::call_p_nn(0x29f9, &[0xf4, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL P, nn
#[case::push_af(0x29f9, &[0xf5], &["decode_unprefixed", "PUSH rp2[p]"])] // PUSH AF
#[case::or_a_n(0x29f9, &[0xf6, 0x56], &["decode_unprefixed", "ALU[y] n"])] // OR A, n
#[case::rst_30h(0x29f9, &[0xf7], &["decode_unprefixed", "RST y*8"])] // RST 30h
#[case::ret_m(0x29f9, &[0xf8], &["decode_unprefixed", "RET cc[y]"])] // RET M
#[case::ld_sp_hl(0x29f9, &[0xf9], &["decode_unprefixed", "LD SP, HL"])] // LD SP, HL
#[case::jp_m_nn(0x29f9, &[0xfa, 0x34, 0x12], &["decode_unprefixed", "JP cc[y], nn"])] // JP M, nn
#[case::ei(0x29f9, &[0xfb], &["decode_unprefixed", "EI"])] // EI
#[case::call_m_nn(0x29f9, &[0xfc, 0x34, 0x12], &["decode_unprefixed", "CALL cc[y], nn"])] // CALL M, nn
#[case::fd_prefix(0x29f9, &[0xfd, 0x00], &["decode_fd", "decode_unprefixed", "NOP"])] // DD Prefix (NOP)
#[case::cp_a_n(0x29f9, &[0xfe, 0x56], &["decode_unprefixed", "ALU[y] n"])] // CP A, n
#[case::rst_38h(0x29f9, &[0xff], &["decode_unprefixed", "RST y*8"])] // RST 38h

// ED TABLE (MISC INSTRUCTIONS)

#[case::in_b_c(0x29f9, &[0xed, 0x40], &["decode_ed", "IN r[y], (C)"])] // IN B, (C)
#[case::out_c_b(0x29f9, &[0xed, 0x41], &["decode_ed", "OUT (C), r[y]"])] // OUT (C), B
#[case::sbc_hl_bc(0x29f9, &[0xed, 0x42], &["decode_ed", "SBC HL, rp[p]"])] // SBC HL, BC
#[case::ld_nni_bc_a(0x29f9, &[0xed, 0x43], &["decode_ed", "LD (nn), rp[p]", "BC"])] // LD (nn), BC
#[case::neg(0x29f9, &[0xed, 0x44], &["decode_ed", "NEG"])] // NEG
#[case::retn(0x29f9, &[0xed, 0x45], &["decode_ed", "RETN"])] // RETN
#[case::im_0(0x29f9, &[0xed, 0x46], &["decode_ed", "IM 0"])] // IM 0
#[case::ld_i_a(0x29f9, &[0xed, 0x47], &["decode_ed", "LD I, A"])] // LD I, A
#[case::in_c_c(0x29f9, &[0xed, 0x48], &["decode_ed", "IN r[y], (C)"])] // IN C, (C)
#[case::out_c_c(0x29f9, &[0xed, 0x49], &["decode_ed", "OUT (C), r[y]"])] // OUT (C), C
#[case::adc_hl_bc(0x29f9, &[0xed, 0x4a], &["decode_ed", "ADC HL, rp[p]"])] // ADC HL, BC
#[case::ld_bc_nni(0x29f9, &[0xed, 0x4b], &["decode_ed", "LD rp[p], (nn)", "BC"])] // LD BC, (nn)
#[case::ed_4c(0x29f9, &[0xed, 0x4c], &["decode_ed", "NONI"])] // NONI 
#[case::reti(0x29f9, &[0xed, 0x4d], &["decode_ed", "RETI"])] // RETI
#[case::ed_4e(0x29f9, &[0xed, 0x4e], &["decode_ed", "NONI"])] // NONI 
#[case::ld_r_a(0x29f9, &[0xed, 0x4f], &["decode_ed", "LD R, A"])] // LD R, A

#[case::in_d_c(0x29f9, &[0xed, 0x50], &["decode_ed", "IN r[y], (C)"])] // IN D, (C)
#[case::out_c_d(0x29f9, &[0xed, 0x51], &["decode_ed", "OUT (C), r[y]"])] // OUT (C), D
#[case::sbc_hl_de(0x29f9, &[0xed, 0x52], &["decode_ed", "SBC HL, rp[p]"])] // SBC HL, DE
#[case::ld_nni_de_a(0x29f9, &[0xed, 0x53], &["decode_ed", "LD (nn), rp[p]", "DE"])] // LD (nn), DE
#[case::ed_54(0x29f9, &[0xed, 0x54], &["decode_ed", "NONI"])] // NONI 
#[case::ed_55(0x29f9, &[0xed, 0x55], &["decode_ed", "NONI"])] // NONI 
#[case::im_1(0x29f9, &[0xed, 0x56], &["decode_ed", "IM 1"])] // IM 1
#[case::ld_a_i(0x29f9, &[0xed, 0x57], &["decode_ed", "LD A, I"])] // LD A, I
#[case::in_e_c(0x29f9, &[0xed, 0x58], &["decode_ed", "IN r[y], (C)"])] // IN E, (C)
#[case::out_c_e(0x29f9, &[0xed, 0x59], &["decode_ed", "OUT (C), r[y]"])] // OUT (C), E
#[case::adc_hl_de(0x29f9, &[0xed, 0x5a], &["decode_ed", "ADC HL, rp[p]"])] // ADC HL, DE
#[case::ld_de_nni(0x29f9, &[0xed, 0x5b], &["decode_ed", "LD rp[p], (nn)", "DE"])] // LD DE, (nn)
#[case::ed_5c(0x29f9, &[0xed, 0x5c], &["decode_ed", "NONI"])] // NONI 
#[case::ed_5d(0x29f9, &[0xed, 0x5d], &["decode_ed", "NONI"])] // NONI 
#[case::im_2(0x29f9, &[0xed, 0x5e], &["decode_ed", "IM 2"])] // IM 2
#[case::ld_a_r(0x29f9, &[0xed, 0x5f], &["decode_ed", "LD A, R"])] // LD A, R

#[case::in_h_c(0x29f9, &[0xed, 0x60], &["decode_ed", "IN r[y], (C)"])] // IN H, (C)
#[case::out_c_h(0x29f9, &[0xed, 0x61], &["decode_ed", "OUT (C), r[y]"])] // OUT (C), H
#[case::sbc_hl_hl(0x29f9, &[0xed, 0x62], &["decode_ed", "SBC HL, rp[p]"])] // SBC HL, HL
#[case::ld_nni_hl_a(0x29f9, &[0xed, 0x63], &["decode_ed", "LD (nn), rp[p]", "HL"])] // LD (nn), HL
#[case::ed_64(0x29f9, &[0xed, 0x64], &["decode_ed", "NONI"])] // NONI 
#[case::ed_65(0x29f9, &[0xed, 0x65], &["decode_ed", "NONI"])] // NONI 
#[case::ed_66(0x29f9, &[0xed, 0x66], &["decode_ed", "NONI"])] // NONI 
#[case::rrd(0x29f9, &[0xed, 0x67], &["decode_ed", "RRD"])] // RRD
#[case::in_l_c(0x29f9, &[0xed, 0x68], &["decode_ed", "IN r[y], (C)"])] // IN L, (C)
#[case::out_c_l(0x29f9, &[0xed, 0x69], &["decode_ed", "OUT (C), r[y]"])] // OUT (C), L
#[case::adc_hl_hl(0x29f9, &[0xed, 0x6a], &["decode_ed", "ADC HL, rp[p]"])] // ADC HL, HL
#[case::ld_hl_nni(0x29f9, &[0xed, 0x6b], &["decode_ed", "LD rp[p], (nn)", "HL"])] // LD HL, (nn)
#[case::ed_6c(0x29f9, &[0xed, 0x6c], &["decode_ed", "NONI"])] // NONI 
#[case::ed_6d(0x29f9, &[0xed, 0x6d], &["decode_ed", "NONI"])] // NONI
#[case::ed_6e(0x29f9, &[0xed, 0x6e], &["decode_ed", "NONI"])] // NONI
#[case::rld(0x29f9, &[0xed, 0x6f], &["decode_ed", "RLD"])] // RLD

#[case::in_c(0x29f9, &[0xed, 0x70], &["decode_ed", "IN (C)"])] // IN (C)
#[case::out_c_0(0x29f9, &[0xed, 0x71], &["decode_ed", "OUT (C), 0"])] // OUT (C), 0
#[case::sbc_hl_sp(0x29f9, &[0xed, 0x72], &["decode_ed", "SBC HL, rp[p]"])] // SBC HL, SP
#[case::ld_nni_sp_a(0x29f9, &[0xed, 0x73], &["decode_ed", "LD (nn), rp[p]", "SP"])] // LD (nn), SP
#[case::ed_74(0x29f9, &[0xed, 0x74], &["decode_ed", "NONI"])] // NONI 
#[case::ed_75(0x29f9, &[0xed, 0x75], &["decode_ed", "NONI"])] // NONI 
#[case::ed_76(0x29f9, &[0xed, 0x76], &["decode_ed", "NONI"])] // NONI 
#[case::ed_77(0x29f9, &[0xed, 0x77], &["decode_ed", "NONI"])] // NONI 
#[case::in_a_c(0x29f9, &[0xed, 0x78], &["decode_ed", "IN r[y], (C)"])] // IN A, (C)
#[case::out_c_a(0x29f9, &[0xed, 0x79], &["decode_ed", "OUT (C), r[y]"])] // OUT (C), A
#[case::adc_hl_sp(0x29f9, &[0xed, 0x7a], &["decode_ed", "ADC HL, rp[p]"])] // ADC HL, SP
#[case::ld_sp_nni(0x29f9, &[0xed, 0x7b], &["decode_ed", "LD rp[p], (nn)", "SP"])] // LD SP, (nn)

#[case::ldi(0x29f9, &[0xed, 0xa0], &["decode_ed", "bli[y, z]"])] // LDI
#[case::cpi(0x29f9, &[0xed, 0xa1], &["decode_ed", "bli[y, z]"])] // CPI
#[case::ini(0x29f9, &[0xed, 0xa2], &["decode_ed", "bli[y, z]"])] // INI
#[case::outi(0x29f9, &[0xed, 0xa3], &["decode_ed", "bli[y, z]"])] // OUTI

#[case::ldd(0x29f9, &[0xed, 0xa8], &["decode_ed", "bli[y, z]"])] // LDD
#[case::cpd(0x29f9, &[0xed, 0xa9], &["decode_ed", "bli[y, z]"])] // CPD
#[case::ind(0x29f9, &[0xed, 0xaa], &["decode_ed", "bli[y, z]"])] // IND
#[case::outd(0x29f9, &[0xed, 0xab], &["decode_ed", "bli[y, z]"])] // OUTD

#[case::ldir(0x29f9, &[0xed, 0xb0], &["decode_ed", "bli[y, z]"])] // LDIR
#[case::cpir(0x29f9, &[0xed, 0xb1], &["decode_ed", "bli[y, z]"])] // CPIR
#[case::inir(0x29f9, &[0xed, 0xb2], &["decode_ed", "bli[y, z]"])] // INIR
#[case::outir(0x29f9, &[0xed, 0xb3], &["decode_ed", "bli[y, z]"])] // OUTIR
// CB TABLE
#[case::rlc_b(0x29f9, &[0xcb, 0x00], &["decode_cb", "rot[y] r[z]", "RLC", "B"])] // RLC B
#[case::rlc_c(0x29f9, &[0xcb, 0x01], &["decode_cb", "rot[y] r[z]", "RLC", "C"])] // RLC C
#[case::rlc_d(0x29f9, &[0xcb, 0x02], &["decode_cb", "rot[y] r[z]", "RLC", "D"])] // RLC D
#[case::rlc_e(0x29f9, &[0xcb, 0x03], &["decode_cb", "rot[y] r[z]", "RLC", "E"])] // RLC E
#[case::rlc_h(0x29f9, &[0xcb, 0x04], &["decode_cb", "rot[y] r[z]", "RLC", "H"])] // RLC H
#[case::rlc_l(0x29f9, &[0xcb, 0x05], &["decode_cb", "rot[y] r[z]", "RLC", "L"])] // RLC L
#[case::rlc_hli(0x29f9, &[0xcb, 0x06], &["decode_cb", "rot[y] r[z]", "RLC", "(HL)"])] // RLC (HL)
#[case::rlc_a(0x29f9, &[0xcb, 0x07], &["decode_cb", "rot[y] r[z]", "RLC", "A"])] // RLC A
#[case::rrc_b(0x29f9, &[0xcb, 0x08], &["decode_cb", "rot[y] r[z]", "RRC", "B"])] // RRC B
#[case::rrc_c(0x29f9, &[0xcb, 0x09], &["decode_cb", "rot[y] r[z]", "RRC", "C"])] // RRC C
#[case::rrc_d(0x29f9, &[0xcb, 0x0A], &["decode_cb", "rot[y] r[z]", "RRC", "D"])] // RRC D
#[case::rrc_e(0x29f9, &[0xcb, 0x0B], &["decode_cb", "rot[y] r[z]", "RRC", "E"])] // RRC E
#[case::rrc_h(0x29f9, &[0xcb, 0x0C], &["decode_cb", "rot[y] r[z]", "RRC", "H"])] // RRC H
#[case::rrc_l(0x29f9, &[0xcb, 0x0D], &["decode_cb", "rot[y] r[z]", "RRC", "L"])] // RRC L
#[case::rrc_hli(0x29f9, &[0xcb, 0x0E], &["decode_cb", "rot[y] r[z]", "RRC", "(HL)"])] // RRC (HL)
#[case::rrc_a(0x29f9, &[0xcb, 0x0F], &["decode_cb", "rot[y] r[z]", "RRC", "A"])] // RRC A

#[case::rl_b(0x29f9, &[0xcb, 0x10], &["decode_cb", "rot[y] r[z]", "RL", "B"])] // RL B
#[case::rl_c(0x29f9, &[0xcb, 0x11], &["decode_cb", "rot[y] r[z]", "RL", "C"])] // RL C
#[case::rl_d(0x29f9, &[0xcb, 0x12], &["decode_cb", "rot[y] r[z]", "RL", "D"])] // RL D
#[case::rl_e(0x29f9, &[0xcb, 0x13], &["decode_cb", "rot[y] r[z]", "RL", "E"])] // RL E
#[case::rl_h(0x29f9, &[0xcb, 0x14], &["decode_cb", "rot[y] r[z]", "RL", "H"])] // RL H
#[case::rl_l(0x29f9, &[0xcb, 0x15], &["decode_cb", "rot[y] r[z]", "RL", "L"])] // RL L
#[case::rl_hli(0x29f9, &[0xcb, 0x16], &["decode_cb", "rot[y] r[z]", "RL", "(HL)"])] // RL (HL)
#[case::rl_a(0x29f9, &[0xcb, 0x17], &["decode_cb", "rot[y] r[z]", "RL", "A"])] // RL A
#[case::rr_b(0x29f9, &[0xcb, 0x18], &["decode_cb", "rot[y] r[z]", "RR", "B"])] // RR B
#[case::rr_c(0x29f9, &[0xcb, 0x19], &["decode_cb", "rot[y] r[z]", "RR", "C"])] // RR C
#[case::rr_d(0x29f9, &[0xcb, 0x1A], &["decode_cb", "rot[y] r[z]", "RR", "D"])] // RR D
#[case::rr_e(0x29f9, &[0xcb, 0x1B], &["decode_cb", "rot[y] r[z]", "RR", "E"])] // RR E
#[case::rr_h(0x29f9, &[0xcb, 0x1C], &["decode_cb", "rot[y] r[z]", "RR", "H"])] // RR H
#[case::rr_l(0x29f9, &[0xcb, 0x1D], &["decode_cb", "rot[y] r[z]", "RR", "L"])] // RR L
#[case::rr_hli(0x29f9, &[0xcb, 0x1E], &["decode_cb", "rot[y] r[z]", "RR", "(HL)"])] // RR (HL)
#[case::rr_a(0x29f9, &[0xcb, 0x1F], &["decode_cb", "rot[y] r[z]", "RR", "A"])] // RR A

#[case::sla_b(0x29f9, &[0xcb, 0x20], &["decode_cb", "rot[y] r[z]", "SLA", "B"])] // SLA B
#[case::sla_c(0x29f9, &[0xcb, 0x21], &["decode_cb", "rot[y] r[z]", "SLA", "C"])] // SLA C
#[case::sla_d(0x29f9, &[0xcb, 0x22], &["decode_cb", "rot[y] r[z]", "SLA", "D"])] // SLA D
#[case::sla_e(0x29f9, &[0xcb, 0x23], &["decode_cb", "rot[y] r[z]", "SLA", "E"])] // SLA E
#[case::sla_h(0x29f9, &[0xcb, 0x24], &["decode_cb", "rot[y] r[z]", "SLA", "H"])] // SLA H
#[case::sla_l(0x29f9, &[0xcb, 0x25], &["decode_cb", "rot[y] r[z]", "SLA", "L"])] // SLA L
#[case::sla_hli(0x29f9, &[0xcb, 0x26], &["decode_cb", "rot[y] r[z]", "SLA", "(HL)"])] // SLA (HL)
#[case::sla_a(0x29f9, &[0xcb, 0x27], &["decode_cb", "rot[y] r[z]", "SLA", "A"])] // SLA A
#[case::sra_b(0x29f9, &[0xcb, 0x28], &["decode_cb", "rot[y] r[z]", "SRA", "B"])] // SRA B
#[case::sra_c(0x29f9, &[0xcb, 0x29], &["decode_cb", "rot[y] r[z]", "SRA", "C"])] // SRA C
#[case::sra_d(0x29f9, &[0xcb, 0x2A], &["decode_cb", "rot[y] r[z]", "SRA", "D"])] // SRA D
#[case::sra_e(0x29f9, &[0xcb, 0x2B], &["decode_cb", "rot[y] r[z]", "SRA", "E"])] // SRA E
#[case::sra_h(0x29f9, &[0xcb, 0x2C], &["decode_cb", "rot[y] r[z]", "SRA", "H"])] // SRA H
#[case::sra_l(0x29f9, &[0xcb, 0x2D], &["decode_cb", "rot[y] r[z]", "SRA", "L"])] // SRA L
#[case::sra_hli(0x29f9, &[0xcb, 0x2E], &["decode_cb", "rot[y] r[z]", "SRA", "(HL)"])] // SRA (HL)
#[case::sra_a(0x29f9, &[0xcb, 0x2F], &["decode_cb", "rot[y] r[z]", "SRA", "A"])] // SRA A

#[case::sll_b(0x29f9, &[0xcb, 0x30], &["decode_cb", "rot[y] r[z]", "SLL", "B"])] // SLL B
#[case::sll_c(0x29f9, &[0xcb, 0x31], &["decode_cb", "rot[y] r[z]", "SLL", "C"])] // SLL C
#[case::sll_d(0x29f9, &[0xcb, 0x32], &["decode_cb", "rot[y] r[z]", "SLL", "D"])] // SLL D
#[case::sll_e(0x29f9, &[0xcb, 0x33], &["decode_cb", "rot[y] r[z]", "SLL", "E"])] // SLL E
#[case::sll_h(0x29f9, &[0xcb, 0x34], &["decode_cb", "rot[y] r[z]", "SLL", "H"])] // SLL H
#[case::sll_l(0x29f9, &[0xcb, 0x35], &["decode_cb", "rot[y] r[z]", "SLL", "L"])] // SLL L
#[case::sll_hli(0x29f9, &[0xcb, 0x36], &["decode_cb", "rot[y] r[z]", "SLL", "(HL)"])] // SLL (HL)
#[case::sll_a(0x29f9, &[0xcb, 0x37], &["decode_cb", "rot[y] r[z]", "SLL", "A"])] // SLL A
#[case::srl_b(0x29f9, &[0xcb, 0x38], &["decode_cb", "rot[y] r[z]", "SRL", "B"])] // SRL B
#[case::srl_c(0x29f9, &[0xcb, 0x39], &["decode_cb", "rot[y] r[z]", "SRL", "C"])] // SRL C
#[case::srl_d(0x29f9, &[0xcb, 0x3A], &["decode_cb", "rot[y] r[z]", "SRL", "D"])] // SRL D
#[case::srl_e(0x29f9, &[0xcb, 0x3B], &["decode_cb", "rot[y] r[z]", "SRL", "E"])] // SRL E
#[case::srl_h(0x29f9, &[0xcb, 0x3C], &["decode_cb", "rot[y] r[z]", "SRL", "H"])] // SRL H
#[case::srl_l(0x29f9, &[0xcb, 0x3D], &["decode_cb", "rot[y] r[z]", "SRL", "L"])] // SRL L
#[case::srl_hli(0x29f9, &[0xcb, 0x3E], &["decode_cb", "rot[y] r[z]", "SRL", "(HL)"])] // SRL (HL)
#[case::srl_a(0x29f9, &[0xcb, 0x3F], &["decode_cb", "rot[y] r[z]", "SRL", "A"])] // SRL A
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

#[rstest]
#[case::ed_noni_x0(0x29f9, &[0xed], 0x00..=0x3f, &[], &["decode_ed", "NONI"])] // ED NONI where X == 0
#[case::ed_noni_x3(0x29f9, &[0xed], 0xc0..=0xff, &[], &["decode_ed", "NONI"])] // ED NONI where X == 3
#[case(0x29f9, &[0xed], 0x7C..=0x9F, &[], &["decode_ed", "NONI"])]
#[case(0x29f9, &[0xed], 0xA4..=0xA7, &[], &["decode_ed", "NONI"])]
#[case(0x29f9, &[0xed], 0xAC..=0xAF, &[], &["decode_ed", "NONI"])]
#[case(0x29f9, &[0xed], 0xB4..=0xB7, &[], &["decode_ed", "NONI"])]
#[case(0x29f9, &[0xed], 0xBC..=0xBF, &[], &["decode_ed", "NONI"])]
fn test_opcode_range(
    #[case] starting_pc: u16,
    #[case] prefixes: &[u8],
    #[case] opcode_range: std::ops::RangeInclusive<u8>,
    #[case] suffixes: &[u8],
    #[case] expected_logs: &[&str],
) {
    for opcode in opcode_range {
        test_opcode(
            starting_pc,
            &[prefixes, &[opcode], suffixes].concat().as_slice(),
            expected_logs,
        );
    }
}