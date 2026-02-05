use super::*;

// Helper to assemble single line
fn asm(code: &str) -> Vec<u8> {
    assemble(code)
        .map(|(bytes, _, _, _)| bytes)
        .unwrap_or_else(|e| panic!("Failed to assemble '{}': {}", code, e))
}

#[test]
fn test_org_simple() {
    let code = "
        ORG 0x0005
        DB 0xAA
    ";
    let res = asm(code);
    assert_eq!(res.len(), 6);
    assert_eq!(res[0..5], [0, 0, 0, 0, 0]);
    assert_eq!(res[5], 0xAA);
}

#[test]
fn test_variables_scenario() {
    let code = "
        ORG 0x0000
        JP START
    N:  DB 10
    RESULT: DW 0
    START:
        LD A, (N)
        HALT
    ";
    let res = asm(code);
    /*
       0000: JP 0006   (C3 06 00)
       0003: N: 10     (0A)
       0004: RESULT: 0 (00 00)
       0006: START:
       0006: LD A, (0003) (3A 03 00)
       0009: HALT      (76)
    */
    assert_eq!(
        res,
        vec![0xC3, 0x06, 0x00, 0x0A, 0x00, 0x00, 0x3A, 0x03, 0x00, 0x76]
    );
}

#[test]
fn test_db_multiple() {
    let code = "DB 1, 2, 3";
    let res = asm(code);
    assert_eq!(res, vec![1, 2, 3]);
}

#[test]
fn test_db_string() {
    let code = "DB \"Hello\"";
    let res = asm(code);
    assert_eq!(res, vec![72, 101, 108, 108, 111]); // ASCII Hello
}

#[test]
fn test_dw_basic() {
    let code = "DW 0x1234, 0x5678";
    let res = asm(code);
    assert_eq!(res, vec![0x34, 0x12, 0x78, 0x56]);
}

#[test]
fn test_label_forward_ref_in_db() {
    // DB can use labels?
    // Pass 1: "Val: DB LABEL" -> Label unknown. parse_instruction is_dry_run=true. Returns 1 byte.
    // Pass 2: "Val: DB LABEL" -> Label known (hopefully).
    // Let's test.
    let code = "
        DB END
        END: NOP
    ";
    // END is at 1.
    // DB 1.
    // NOP is 0x00.
    let res = asm(code);
    assert_eq!(res, vec![0x01, 0x00]);
}

#[test]
fn test_org_gap() {
    let code = "
        NOP
        ORG 0x0004
        NOP
    ";
    // 0: NOP
    // 1..3: Zeros
    // 4: NOP
    let res = asm(code);
    assert_eq!(res, vec![0x00, 0x00, 0x00, 0x00, 0x00]);
    // NOP (00), 00, 00, 00, NOP (00).
    // Oh NOP is 0x00? Yes.
}
