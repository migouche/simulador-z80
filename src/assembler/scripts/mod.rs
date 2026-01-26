use crate::assembler::assemble;
use crate::cpu::Z80A;
use crate::traits::MemoryMapper;
use std::cell::RefCell;
use std::rc::Rc;
use crate::components::memories::mem_64k::Mem64k;
use crate::traits::SyncronousComponent;

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

    // Assemble
    let (bytes, symbols) = assemble(code).expect("Assembly failed");

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
    // Safety break to prevent infinite loops in bad code
    let mut cycles = 0;
    while !cpu.is_halted() && cycles < 100000 {
        cpu.tick();
        cycles += 1;
    }

    assert!(cpu.is_halted(), "CPU did not halt within limit");

    // Check Result
    let result_sym = symbols.get("RESULT").expect("Label RESULT not found");
    let result_addr = result_sym.address;
    let res_low = memory.borrow().read(result_addr);
    let res_high = memory.borrow().read(result_addr + 1);
    let res = u16::from_le_bytes([res_low, res_high]);

    // Expected F(15) = 610 (0x0262) for N=16
    assert_eq!(res, 610, "Fibonacci result incorrect: expected 610, got {}", res);
}
