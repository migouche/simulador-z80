        ORG 0000h
        JP START

        ; Interrupt Service Routine for Mode 1 (RST 38h)
        ORG 0038h
INTERRUPT_HANDLER:
        PUSH AF         ; Save AF
        IN A, (01h)     ; Read Keypad (Port 1)
        OUT (02h), A    ; Write to Display (Port 2)
        POP AF          ; Restore AF
        EI              ; Enable Interrupts
        RETI            ; Return from Interrupt

START:
        IM 1            ; Set Interrupt Mode 1
        EI              ; Enable Interrupts
        LD A, 0         ; Clear A
        OUT (02h), A    ; Initialize Display to 0

LOOP:
        HALT            ; Wait for interrupt
        JP LOOP         ; Infinite Loop
