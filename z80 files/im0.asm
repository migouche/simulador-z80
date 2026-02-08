; =================================
; code to test interrupt mode 0    
; if loaded with FF it will go to 38h and display 3
; if loaded with EF it will go to 28h and display 2

	ORG 0000H
	JP START

	ORG 0028H       ; RST 28H Address (Opcode EF)
    LD A, 2         ; Load 2 on A
    OUT (02H), A    ; Show on display attached to Port 2
    EI              ; Re-enable interrupts
    RET             ; Return to loop


    ORG 0038H       ; RST 38H Address (Opcode FF)
    LD A, 3         ; Load 3 on A
    OUT (02H), A    ; Show on display attached to Port 2
    EI              ; Re-enable interrupts
    RETI             ; Return to loop

ORG 1000H
START:
    LD SP, 0FFFFH   ; Stack Pointer
    IM 0            ; Interrupt Mode 0
    EI              ; Enable Interrupts
    LD A, 00H       ; Counter

LOOP:
	HALT
    JP LOOP         ; Infinite Loop waiting for interrupt (Device sends FF for RST 38H)
