    ORG 0000H
    LD SP, 0FFFFH

    ; Initialize Vector Table at 8000H for Vector 00H
    LD HL, ISR      ; Address of ISR
    LD (8000H), HL  ; Store at Table Base (8000H) + Vector (00H)
    
    ; Initialize I Register
    LD A, 80H
    LD I, A         ; I = 80H
    
    IM 2            ; Interrupt Mode 2
    EI              ; Enable Interrupts
    
    LD A, 00H       ; Counter

LOOP:
    JP LOOP         ; Wait for interrupt (Device sends 00 for Vector 00)

ISR:
    INC A           ; Increment
    OUT (02H), A    ; Display
    EI
    RETI            ; Return from Interrupt
