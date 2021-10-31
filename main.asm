;
; main.asm
;
; Created: 2019. 12. 16. 22:10:04
; Author : Komornik G.
;

; An application for the Attiny85-20 MCU
; Will blink an LED (or with proper wiring maybe a buzzer)

; ===============================

.INCLUDE "TN85DEF.INC"                          ; (ATTINY85 DEFINITIONS)

;---------------;
; HELPER MACROS ;
;---------------;
#define MASK(B) (1 << (B))                      ; CONVERTS BIT INDEX TO BITMASK

#define DIT 0                                   ; SHORT PULSE BIT
#define DAH 1                                   ; LONG PULSE BIT
#define X   0                                   ; DON'T CARE
#define MORSE_CHAR(LEN, S1, S2, S3, S4, S5) \
        ( ((LEN << 5) & 0b1110_0000) | \
        (S1 << 0) | (S2 << 1) | (S3 << 2) | \
        (S4 << 3) | (S5 << 4) )

; TURN THE LED ON OFF
#define LED_ON()  SBI PORTB, PORTB4
#define LED_OFF() CBI PORTB, PORTB4

.MACRO LDIX                                     ; LOADS AN IMMEDIATE CONSTANT TO X
    LDI XL, (@0 >> 0) & 0xFF
    LDI XH, (@0 >> 8) & 0xFF
.ENDMACRO

.MACRO LDIY                                     ; LOADS AN IMMEDIATE CONSTANT TO Y
    LDI YL, LOW(@0)
    LDI YH, HIGH(@0)
.ENDMACRO

.MACRO LDIZ                                     ; LOADS AN IMMEDIATE CONSTANT TO Z
    LDI ZL, LOW(@0)
    LDI ZH, HIGH(@0)
.ENDMACRO

;----------------------------;
; REGISTER ALIASES AND USAGE ;
;----------------------------;
.DEF MH = R1                                    ; RESERVED FOR SOME ALU OP WHICH ...
.DEF ML = R0                                    ; ... CAN ONLY BE CARRIED OUT HERE
.DEF A = R16                                    ; GENERAL PURPOSE ACCUMULATOR
.DEF B = R17                                    ; GENERAL PURPOSE ACCUMULATOR 2
.DEF C = R18                                    ; GENERAL PURPOSE ACCUMULATOR 2
.DEF EV = R19                                   ; EVENT FLAGS
.DEF DITS = R20                                 ; DITS TO WAIT
                                                ; R26 - R31 IS RESERVED FOR X, Y, Z

;----------------------;
; CONSTANT EXPRESSIONS ;
;----------------------;
    ; DELAY (1 UNIT = 1/1024 SECS)
.EQU DIT_LENGTH      = 102                      ; APROX 1/10 SECOND

    ; EVENT FLAG NAMES
.EQU EVENT_MORSE_DIT = 0
.EQU EVENT_DHT_READ  = 1
.EQU EVENT_100MS     = 2
.EQU EVENT_BTN       = 3
.EQU COMM_START      = 4
.EQU COMM_END        = 5

;------------------------;
; INTERRUPT VECTOR TABLE ;
;------------------------;
.ORG 0000
    RJMP    ON_RESET
.ORG OC0Aaddr
    RJMP    TIM0_OCA                            ; TIMER OVERFLOW VECTOR

;---------------------;
; INITIALIZATION CODE ;
;---------------------;
ON_RESET:
    ; POWER MANAGEMENT
    LDI     A, MASK(PRTIM1) | MASK(PRUSI) | MASK(PRADC)
    ;LDI     A, 0
    OUT     PRR, A                              ; SHUTTING DOWN UNNECESSARY CLOCKS

    ; GPIO SETUP
    SBI     DDRB, DDB4                          ; SET PORTB0 FOR OUTPUT

    ; SET UP TIMER0 FOR MORSE CODE GENERATION
    LDI     A, DIT_LENGTH                       ; THIS VALUE WILL GIVE US APROX 1/10 SECOND
    OUT     OCR0A, A                         
    LDI     A, MASK(CS02) | MASK(CS00)          ; SET TIMER PRESCALER TO /1024
    OUT     TCCR0B, A
    LDI     A, MASK(WGM01)                      ; SET CTC MODE
    OUT     TCCR0A, A
    LDI     A, MASK(OCIE0A)                     ; ENABLE OUTPUT COMPARE MATCH A INTERRUPT
    OUT     TIMSK, A
    
    SEI                                         ; ENABLE INTERUPTS GLOBALLY

;-------------------;
; MAIN LOOP ROUTINE ;
;-------------------;
MAIN_LOOP:
    RCALL   MORSE_DEMO
    RCALL   WAIT_DAH
    RCALL   WAIT_DAH
    RCALL   WAIT_DAH
    RCALL   WAIT_DAH                            ; 4 DIT WAITS (AFTER CHAR 3 ALREADY ELAPSED)
;    RCALL   TOGGLE_LED
;    LDI     A, MASK(EVENT_MORSE_DIT)            
;    RCALL   WAIT                                ; WAIT FOR MORSE DIT EVENT
;    RCALL   BUSY_WAIT_1SEC
;    CBR     EV, MASK(EVENT_MORSE_DIT)
    RJMP    MAIN_LOOP                           ; LOOP FOREVER

TOGGLE_LED:
    PUSH    A
    PUSH    B
    IN      A, PORTB                            ; FLIP THE 0 BIT
    LDI     B, MASK(PORTB4)                     ; BIT MASK
    EOR     A, B                                ; TOGGLE PORT-B PIN 4 VALUE
    OUT     PORTB, A                            ; WRITE THE RESULT
    POP     B
    POP     A
    RET

;-----------------------;
; MORSE CODE PROCESSING ;
;-----------------------;
MORSE_DEMO:
    LDIX(STRING_MORSE<<1)                       ; X HOLDS ADDRESS OF STRING
    PUSH    A
    PUSH    B
MORSE_DEMO_LOOP:
    RCALL   WAIT_DIT                            ; WAIT TWO UNITS
    RCALL   WAIT_DIT                            ; ONE ALREADY ELAPSED AFTER LAST SYMBOL
    MOVW    ZL, XL                              ; COPY Z <- X
    LPM     A, Z+                               ; LOAD NEXT CHARACTER
    MOVW    XL, ZL                              ; COPY BACK THE INCREMENTED ADDRESS X <- Z
    TST     A                                   ; TEST ASCII VALUE
    BREQ    MORSE_DEMO_END                      ; IF BYTE IS 0 RETURN FROM SUBROUTINE
    CPI     A, 32                               ; IF BYTE IS 32 (SPACE) THAN JUST DELAY
    BREQ    MORSE_DEMO_SPACE
    SUBI    A, 65                               ; ASCII TO ABC OFFSET
    LDIZ(MORSE_TABLE<<1)                        ; BASE ADDRESS LOADED TO Z
    CLR     B
    ADD     ZL, A                               ; ADD OFFSET TO WORD Z
    ADC     ZH, B                               ; ADD ONLY THE CARRY FLAG
    LPM     A, Z                                ; SYMBOL SEQUENCE IS LOADED TO A
    MOV     B, A                                ; COPY SS TO B
    LSR     B                                   ; B := B >> 5
    LSR     B
    LSR     B
    LSR     B                                   ; B SHIFTED RIGHT 5 TIMES
    LSR     B                                   ; NOW CONTAINS SYMBOL COUNT
MORSE_DEMO_INNER_LOOP:
    TST     B                                   ; IF B == 0
    BREQ    MORSE_DEMO_LOOP                     ; NO MORE SYMBOL, READ NEXT CHAR
    LED_ON()
    SBRC    A, 0                                ; SKIP IF CURRENT SYMBOL (LSB) IS DIT
    RCALL   WAIT_DAH
    SBRS    A, 0                                ; SKIP IF CURRENT SYMBOL (LSB) IS DAH
    RCALL   WAIT_DIT
    LED_OFF()
    RCALL   WAIT_DIT
    LSR     A                                   ; SHIFT NEXT SYMBOL TO BIT 0 POSITION
    DEC     B                                   ; ONE LESS SYMBOL REMAINING
    RJMP    MORSE_DEMO_INNER_LOOP               ; NEXT SYMBOL
MORSE_DEMO_END:
    POP     B
    POP     A
    RET
MORSE_DEMO_SPACE:
    RCALL   WAIT_DAH                            ; WAIT 4 UNITS...
    RCALL   WAIT_DIT                            ; TOGETHER WITH OTHER WAITS IT WILL GIVE A TOTAL OF 7
    RJMP    MORSE_DEMO_LOOP

;-----------------;
; TIMER0 ROUTINES ;
;-----------------;
; TIMER0 COMPARE MATCH A
TIM0_OCA:                                       ; ISR toggles LED
    PUSH    A
    PUSH    B
    IN      A, SREG
    PUSH    A

    ; ==============/===============
    SBR     EV, MASK(EVENT_MORSE_DIT)
    ;LDI     A, MASK(EVENT_MORSE_DIT)
    ;MOV     EV, A

    POP     A
    OUT     SREG, A
    POP     B
    POP     A
    RETI

;----------------;
;PAUSE ROUTINES  ;
;----------------;
; Waits for one or more events to occur
;   <- A : the event flags to wait for ored together
WAIT:
    PUSH B
    IN   B, SREG
    PUSH B
WAIT_LOOP:
    MOV     B, A                                ; CREATE A COPY OF 'A'
    AND     B, EV                               ; B := B & EVENTS
    BRNE    WAIT_RETURN                         ; IF B != 0 RETURN BECAUSE WE HAVE A FLAG
    RCALL   GO_SLEEP
    RJMP    WAIT_LOOP                           ; WOKEN UP BY INTERRUPT, WE SHOULD CHECK AGAIN
WAIT_RETURN:
    POP     B
    OUT     SREG, B
    POP     B
    RET     

; Goes to sleep mode idle, returns when woken up
GO_SLEEP:
    PUSH    A
    IN      A, SREG
    PUSH    A
    IN      A, MCUCR                            ; READ MCU CONTROL REGISTER TO MANIPULATE
    CBR     A, MASK(SM0) | MASK(SM1)            ; SM[0:1] := 00 -> SLEEP MODE := IDLE
    SBR     A, MASK(SE)                         ; SET SLEEP ENABLE
    OUT     MCUCR, A
    SLEEP
    CBR     A, MASK(SE)                         ; CLEAR SLEEP ENABLE AGAIN
    OUT     MCUCR, A
    POP     A
    OUT     SREG, A
    POP     A
    RET

WAIT_DIT:
    PUSH    A
    LDI     A, MASK(EVENT_MORSE_DIT)
    RCALL   WAIT
    CBR     EV, MASK(EVENT_MORSE_DIT)
    POP     A
    RET

WAIT_DAH:
    PUSH    A
    LDI     A, MASK(EVENT_MORSE_DIT)
    RCALL   WAIT
    CBR     EV, MASK(EVENT_MORSE_DIT)
    RCALL   WAIT
    CBR     EV, MASK(EVENT_MORSE_DIT)
    RCALL   WAIT
    CBR     EV, MASK(EVENT_MORSE_DIT)
    POP     A
    RET

BUSY_WAIT_1SEC:
    PUSH    A
    PUSH    R2
    PUSH    R3
    PUSH    R4
    CLR     R2
    CLR     R3
    CLR     R4
BW1_LOOP:
    ;INC     R2
    TST     R2
    IN      A, SREG
    SBRC    A, SREG_Z
    INC     R3
    IN      A, SREG
    SBRC    A, SREG_Z
    INC     R4
    MOV     A, R4
    CPI     A, 32
    BRNE    BW1_LOOP
BW1_END:
    POP     R4
    POP     R3
    POP     R2
    POP     A
    RET

; ===============================================================
HALT:
    CLI
HALT_FOREVER:
    RJMP HALT_FOREVER

; ===============================================================
; DATA AND CONSTANTS IN FLASH
; ===============================================================
MORSE_TABLE: 
; contents: A-Z 0-9
.DB \
    MORSE_CHAR(2, DIT, DAH, X, X, X), \
    MORSE_CHAR(4, DAH, DIT, DIT, DIT, X), \
    MORSE_CHAR(4, DAH, DIT, DAH, DIT, X), \
    MORSE_CHAR(3, DAH, DIT, DIT, X, X), \
    MORSE_CHAR(1, DIT, X, X, X, X), \
    MORSE_CHAR(4, DIT, DIT, DAH, DIT, X), \
    MORSE_CHAR(3, DAH, DAH, DIT, X, X), \
    MORSE_CHAR(4, DIT, DIT, DIT, DIT, X), \
    MORSE_CHAR(2, DIT, DIT, X, X, X), \
    MORSE_CHAR(4, DIT, DAH, DAH, DAH, X), \
    MORSE_CHAR(3, DAH, DIT, DAH, X, X), \
    MORSE_CHAR(4, DIT, DAH, DIT, DIT, X), \
    MORSE_CHAR(2, DAH, DAH, X, X, X), \
    MORSE_CHAR(2, DAH, DIT, X, X, X), \
    MORSE_CHAR(3, DAH, DAH, DAH, X, X), \
    MORSE_CHAR(4, DIT, DAH, DAH, DIT, X), \
    MORSE_CHAR(4, DAH, DAH, DIT, DAH, X), \
    MORSE_CHAR(3, DIT, DAH, DIT, X, X), \
    MORSE_CHAR(3, DIT, DIT, DIT, X, X), \
    MORSE_CHAR(1, DAH, X, X, X, X), \
    MORSE_CHAR(3, DIT, DIT, DAH, X, X), \
    MORSE_CHAR(4, DIT, DIT, DIT, DAH, X), \
    MORSE_CHAR(3, DIT, DAH, DAH, X, X), \
    MORSE_CHAR(4, DAH, DIT, DIT, DAH, X), \
    MORSE_CHAR(4, DAH, DIT, DAH, DAH, X), \
    MORSE_CHAR(4, DAH, DAH, DIT, DIT, X), \
    MORSE_CHAR(5, DAH, DAH, DAH, DAH, DAH), \
    MORSE_CHAR(5, DIT, DAH, DAH, DAH, DAH), \
    MORSE_CHAR(5, DIT, DIT, DAH, DAH, DAH), \
    MORSE_CHAR(5, DIT, DIT, DIT, DAH, DAH), \
    MORSE_CHAR(5, DIT, DIT, DIT, DIT, DAH), \
    MORSE_CHAR(5, DIT, DIT, DIT, DIT, DIT), \
    MORSE_CHAR(5, DAH, DIT, DIT, DIT, DIT), \
    MORSE_CHAR(5, DAH, DAH, DIT, DIT, DIT), \
    MORSE_CHAR(5, DAH, DAH, DAH, DIT, DIT), \
    MORSE_CHAR(5, DAH, DAH, DAH, DAH, DIT), \
    0, 0   ; end of morse table

STRINGS:
STRING_MORSE: .DB "SOME MORSE CODE WITH SPACES", 0
