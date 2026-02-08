    ORG 0000h
    JP START

    ; NMI Handler at 0066h
    ORG 0066h

    INC A            ; Increment counter
    OUT (01h), A     ; Output to port 1
    RETN

START:
    LD SP, 0xFFFF
    LD A, 0
LOOP:
    HALT
    JR LOOP
