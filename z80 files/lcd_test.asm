    ; Test file for Block I/O Instructions (OTIR and INIR)
    ; Usage:
    ; 1. Load this file.
    ; 2. Add an LCD Display on Port 03h (Menu -> Devices -> Add LCD Display -> OK)
    ; 3. Run the simulation.
    ; 4. Check the LCD window to see "Hello Output!".
    ; 5. Check memory at INPUT_BUF (0x0050) to see "Hello Input".

    ORG 0000h
    
    LD SP, 0FFFFh

    ; --------------------------------------
    ; Test 1: OTIR (Write string to LCD)
    ; --------------------------------------
    LD HL, MSG_OUT      ; Source address
    LD BC, 0D03h        ; B = Length (13), C = Port (03)
    OTIR                ; Output (HL) to Port C, HL++, B-- until B=0

    ; --------------------------------------
    ; Test 2: INIR (Read from LCD to Memory)
    ; --------------------------------------
    ; The LCD device provides "Hello Input" (11 chars)
    LD HL, INPUT_BUF    ; Destination address
    LD BC, 0B03h        ; B = Length (11), C = Port (03)
    INIR                ; Input from Port C to (HL), HL++, B-- until B=0
    
    HALT

MSG_OUT:
    DB "Hello Output!"  ; 13 bytes including space

    ; Padding to align input buffer
    DS 5 

INPUT_BUF:
    DS 20              ; Reserve space for input
