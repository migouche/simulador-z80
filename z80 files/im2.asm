    ORG 0000H
	JP START

ORG 1000H ; INTERRUPT VECTOR TABLE

DW 3000h ; 00h goes to 3000h
DW 4000h ; 02h goes to 4000h
DW 5000h ; 04h goes to 5000h

    

ORG 2000h

START:    
    ; Initialize I Register
    LD SP, 0FFFFH
    LD A, 10H
    LD I, A         ; I = 80H
    
    IM 2            ; Interrupt Mode 2
    EI              ; Enable Interrupts
    


LOOP:
	HALT
    JP LOOP         ; Wait for interrupt (Device sends 00 for Vector 00)

ORG 3000H
LD A, 1h
JP ISR

ORG 4000H
LD A, 2H
JP ISR

ORG 5000H
LD A, 3H
JP ISR

ORG 6000H

ISR:

    OUT (02H), A    ; Display
    EI
    RETI            ; Return from Interrupt
