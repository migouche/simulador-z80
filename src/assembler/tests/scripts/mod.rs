use crate::assembler::Symbol;
use crate::assembler::assemble;
use crate::components::memories::mem_64k::Mem64k;
use crate::cpu::GPR;
use crate::cpu::RegisterPair;
use crate::cpu::Z80A;
use crate::traits::MemoryMapper;
use crate::traits::SyncronousComponent;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Helper function to assemble code, setup CPU, and run until halted.
/// Returns the CPU instance, the memory reference, and the symbol table from the assembler.
fn run_until_halt(
    code: &str,
    max_cycles: usize,
) -> (Z80A, Rc<RefCell<dyn MemoryMapper>>, HashMap<String, Symbol>) {
    // Assemble
    let (bytes, symbols, _, _) = assemble(code).expect("Assembly failed");

    // Setup CPU
    let memory: Rc<RefCell<dyn MemoryMapper>> = Rc::new(RefCell::new(Mem64k::new()));
    let mut cpu = Z80A::new(memory.clone());

    // Load memory
    {
        let mut mem = memory.borrow_mut();
        for (i, b) in bytes.iter().enumerate() {
            mem.write(i as u16, *b);
        }
    }

    // Run until HALT
    let mut cycles = 0;
    while !cpu.is_halted() && cycles < max_cycles {
        cpu.tick();
        cycles += 1;
    }

    assert!(cpu.is_halted(), "CPU did not halt within limit");

    (cpu, memory, symbols)
}

#[test]
fn test_fibonacci_execution() {
    let code = r#"
; ============================
; Z80 Fibonacci
; Calculates F(n)
; ============================

                ORG 0000h
                JP START

; ----------------------------
; Data
; ----------------------------
N:      DB 10h         ; n (16 decimal)
RESULT: DW 0           ; 16-bit result

; ----------------------------
; Code
; ----------------------------
START:
                LD A, (N)      ; A = n
                OR A
                JR Z, FIB_ZERO ; if n == 0

                CP 1
                JR Z, FIB_ONE  ; if n == 1

                ; fib0 = 0
                ; fib1 = 1
                LD DE, 0       ; DE = fib0
                LD HL, 1       ; HL = fib1

                DEC A          ; we already handled first two
                DEC A          ; loop count = n - 2
                
                JR Z, DONE     ; If n=2, HL is already 1, so we are done

FIB_LOOP:
                ; Move High and Low bytes separately (LD BC, HL pseudo)
                LD B, H        ; B = H
                LD C, L        ; C = L (Now BC = HL)

                ADD HL, DE     ; HL = fib0 + fib1

                ; Move bytes separately (LD DE, BC pseudo -> DE = Old HL)
                LD D, B        ; D = B
                LD E, C        ; E = C (Now DE = Old HL)

                DEC A
                JR NZ, FIB_LOOP

DONE:
                LD (RESULT), HL
                JP END

FIB_ZERO:
                LD HL, 0
                LD (RESULT), HL
                JP END

FIB_ONE:
                LD HL, 1
                LD (RESULT), HL

END:
                HALT
        "#;

    let (_cpu, memory, symbols) = run_until_halt(code, 100000);

    // Check Result
    let result_sym = symbols.get("RESULT").expect("Label RESULT not found");
    let result_addr = result_sym.address;
    let res_low = memory.borrow().read(result_addr);
    let res_high = memory.borrow().read(result_addr + 1);
    let res = u16::from_le_bytes([res_low, res_high]);

    // Expected F(15) = 610 (0x0262) for N=16
    assert_eq!(
        res, 610,
        "Fibonacci result incorrect: expected 610, got {}",
        res
    );
}

#[test]
fn test_pv_hc() {
    let code = r#"
                LD A, 0x7F    ; Load 127 into A
                LD B, 0x01    ; Load 1 into B
                ADD A, B      ; A becomes 0x80 (-128). Should set S, V, and H flags.
                LD C, A       ; Save result in C to check later
                PUSH AF       ; Push flags to stack
                POP HL        ; Pop flags into HL (L will contain the F register)
                HALT
        "#;

    let (cpu, _, _) = run_until_halt(code, 1000);

    // Check result in C
    assert_eq!(
        cpu.get_register(GPR::C),
        0x80,
        "Expected C to be 0x80 after addition"
    );

    assert_eq!(
        cpu.get_register(GPR::A),
        0x80,
        "Expected A to be 0x80 after addition"
    );

    // Check flags in L (which contains F)
    let f = cpu.get_register(GPR::L);
    assert_eq!(
        f, 0x94,
        "Expected F to be 0x94 (S, H, and V set) but got {:02X}",
        f
    );
}

#[test]
fn test_ldir() {
    let code = r#"
        ; Setup data in memory (assuming your emulator starts at 0x0000)
; We will copy 3 bytes from 0x1000 to 0x2000
LD HL, 0x1000
LD DE, 0x2000
LD BC, 0x0003
LDIR
HALT

ORG 0x1000
	DB 0xAA
	DB 0xBB
	DB 0xCC
; Mock Data (Place this at 0x1000 in your memory map)
; 0x1000: 0xAA, 0xBB, 0xCC
        "#;

    let (_, memory, _) = run_until_halt(code, 1000);

    // Check that bytes were copied to 0x2000, 0x2001, and 0x2002
    let dest_addr = 0x2000;
    let expected_data: [u8; 3] = [0xAA, 0xBB, 0xCC];
    for i in 0..3 {
        let byte = memory.borrow().read(dest_addr + i);
        assert_eq!(
            byte,
            expected_data[i as usize],
            "Expected byte at {:04X} to be {:02X} but got {:02X}",
            dest_addr + i,
            expected_data[i as usize],
            byte
        );
    }
}

#[test]
fn test_stack() {
    let code = r#"
LD SP, 0x2000 ; Set SP to a safe area
    LD BC, 0x1234
    PUSH BC       ; [Stack: 0x1234]
    CALL SUB_A    ; [Stack: Ret_Addr_Main, 0x1234]
    POP HL        ; Should get 0x1234 back
    HALT

SUB_A:
    LD DE, 0x5566
    PUSH DE       ; [Stack: 0x5566, Ret_Addr_Main, 0x1234]
    CALL SUB_B    ; [Stack: Ret_Addr_A, 0x5566, Ret_Addr_Main, 0x1234]
    POP DE        ; Should get 0x5566 back
    RET           ; Returns to 'POP HL' in Main

SUB_B:
    LD A, 0xFF
    RET           ; Returns to 'POP DE' in SUB_A
"#;

    let (cpu, _, _) = run_until_halt(code, 1000);

    assert_eq!(
        cpu.get_register_pair(RegisterPair::HL),
        0x1234,
        "Expected HL to be 0x1234 after stack operations"
    );

    assert_eq!(
        cpu.get_register_pair(RegisterPair::DE),
        0x5566,
        "Expected DE to be 0x5566 after stack operations"
    );

    assert_eq!(
        cpu.get_register(GPR::A),
        0xFF,
        "Expected A to be 0xFF after SUB_B execution"
    );

    assert_eq!(
        cpu.get_sp(),
        0x2000,
        "Expected SP to be back to 0x2000 after all stack operations"
    );
}
