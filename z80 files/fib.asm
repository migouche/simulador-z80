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
		HALT
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
