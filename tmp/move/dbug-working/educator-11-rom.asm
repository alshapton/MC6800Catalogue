       NAM    DBUG
*2/2/77    VER  1.0
*
        OPT     0,S
*
********************************
*   RAM SPACE ALLOCATION
*   
*   FLAG - GO/HALT STATUS
*
*   PANADR - TWO BYTES FOR PANEL ADR
*
*   STACK1 - STORAGE FOR STACK POINTER
*           ON ENTRY TO "HALT"  POINTS
*           TO STACK USED FOR GO RTI
*
*
*   STACK2 - "BOTTOM" OF STACK AREA USED IN
*            GO/HALT OPERATIONS  TARGET 
*            PROGRAM START VECTOR WILL BE
*            AT FA (PCL) AND F9 (PCH)
*
***********************************
*
        ORG     $F0
*
BEGADR  RMB     2
LAST    RMB     1
STACK2  RMB     1
CCR     RMB     1
ACCB    RMB     1
ACCA    RMB     1
IXH     RMB     1
IXL     RMB     1
PRGHI   RMB     1
PRGLO   RMB     1
STACK1  RMB     2
PANADR  RMB     2
FLAG    RMB     1
*
*
*
BYTE    EQU     ACCA
DRA     EQU     $4000
DRB     EQU     DRA+2
CRA     EQU     DRA+1
CRB     EQU     DRB+1
STACK   EQU     $2200
*
*
        ORG     $8200
*
*
*     SUBROUTINE DEPOSIT     *
*
*
DEP     LDA B   0,X      CLR INT FLAG IN PIA
        BIT A   #02      DISPLAY DATA IF ZERO ADDRESS
        BNE     ADR
DATA1   LDX     PANADR
        STA B   0,X      PLACE PANEL DATA IN MEMORY
        CMP B   0,X
        BNE     BLINK
        STA B   DRB      PLACE DATA INTO DISPLAY
        INX
        STX     PANADR   INCREMENT AND STORE PANEL ADD
        LDA A   #$0F     SET INT FOR POS EDGE
STORA   STA A   CRA
        BRA     BACK1
ADR     LDA B   PANADR+1
        STA B   2,X      PUT LSBYTE OF ADR INTO DISP
        LDA A   #$0D     SETS INT FOR NEG EDGE
        BRA     STORA
*
*
****    SUBROUTINE EXAMINE     *****
*
*
*
EXAM    LDX     PANADR   GET PANEL ADR FOR DATA FETCH
        BIT B   #$2     
        BEQ     ADR2     NEG EDGE=ADR IN DISPLAY
DATA2   LDA A   0,X
        STA A   DRB      PUT OLD DATA INTO DISPLAY
        LDA B   #$35    
        BRA     STORB
ADR2    INX              INCREMENT PANEL ADR
        STX     PANADR   STORE NEW PANEL ADR
        LDA A   PANADR+1 GET LSBYTE OF PANEL ADR
        STA A   DRB      LSBYTE OF PANEL ADR INTO DISP
        LDA B   #$37
STORB   STA B   CRB
BACK1   BRA     BACK
*
*
*********   IRQ ROUTINE *********
*
*       CHECK FOR CB2 INTERRUPT *****
*
*
IRQ     EQU     *
        LDA B   CRB
        BIT B   #$40
        BEQ     DELAY
        JMP     $80
*
*
********  DEBOUNCE ROUTINE ******
*
*
DELAY   LDX     #2400    SET DEBOUNCE TYPE
D2      DEX
        BNE     D2
        LDX     #DRA
*
*
*   IRQ SERVICE CONTINUED **
*
*
        LDA B    3,X
        LDA A    1,X
        BMI     D1       CA1 = DEPOSIT
        BIT A   #$40     NOT CA1. TEST CA2
        BNE     D3
        BRA     EXAM
D1      BIT A   #$40
        BNE     MSBYTE
        BRA     DEP      CA1 AND NOT CA2
D3      BIT B   #$80
        BNE     LSBYTE
        BRA     GOHALT
*
*
*
*
*
*
*        ENTRY FOR RAM DUMP.  $00 - $F0
*
*
*
        LDX     #0
        STX     $F0
        LDX     #$F0
        STX     $F2
        BRA     TAPOUT
*
*
*
***** BLINKING DISPLAY ROUTING *******
*
*
*
BLINK   LDX     #25000
        CLI
        COM     DRB
LOOP    DEX
        BNE     LOOP
        BRA     BLINK
*
*
*
*       GO/HALT ROUTINE         *
*
*
GOHALT  BIT A   #$10     MASK FOR EDGE BIT
        BEQ     OK       IF ZERO, NEG EDGE
        LDA A   #$0D
        STA A   1,X
        TST     0,X
        LDA A   FLAG
        BEQ     BACK
        RTI
OK      LDA A   #$1D     SET FOR POS EDGE
        STA A   1,X
        LDA A   FLAG    
        BEQ     GO       ZERO=GO, FF=HALT
HALT    TSX
        LDAA    6,X      FETCH PCL FROM STACK
        DEC A
HALT3   STA A   DRB      PUT PCL-1 INTO DISPLAY
HALT2   STS     PANADR   STORE STACK AT PANEL ADR
        STS     STACK1   STORE USER'S STACK POINTER
        CLR     FLAG     RESET FLAG FOR "GO"
BACK    LDS     #STACK   RESTORE PANEL PHANTOM STACK
        TST     DRB      CLR INT
        TST     DRA
        CLI              READY FOR GO
        WAI
GO      LDS     STACK1   RESTORE USER'S STACK POINTER
        COM     FLAG     SET FLG FOR HALT
        TST     0,X      LOADS "OLD" STACK INTO PROCSR
        RTI
*
*
******   SUBROUTINE M S BYTE ********
*
*
MSBYTE  LDA A   0,X
        STAA    PANADR
        BRA     FETCH
*
*
*     SUBROUTINE LOAD LSBYTE  *
*
*
LSBYTE  LDA A   0,X      LOAD SWITCH INFO INTO ACCA
        STA A   PANADR+1 STORE LSBYTE INTO PANEL ADR
FETCH   LDX     PANADR   GET DATA AT PANEL ADR
        LDA A   0,X
        STA A   DRB      DISPLAY DATA IN PANEL ADR
        BRA     BACK
*
*
*
TAPOUT  LDS     #$FF
        BSR     INITIA
*
*
START   CLR A
        STA A   PRGHI
START1  CLR B
        STA B   FLAG
        STA B   CCR
        LDS     #STACK2
INZ     CLR     CRA
        CLR     CRB
        LDX     #$000D
        STX     $4000
        LDX     #$FF35
        STX     $4002
*
        BRA     HALT3
*
*
* CASSETTE INPUT ENTRY FOR FILE  *
* NUMBER IN SWITCHES  *
*
TAPIN1  LDA A    DRA
        NEG A
        STAA    FLAG
*
* CASSETTE INPUT ENTRY FOR NEXT FILE *
*
        LDS     #$00FE
        BSR     TAPE2
        BRA     START1
*
*
*
*
*
*
*
*
* *** CASSETTE INTERFACE PROGRAM
*
*
*   OUTPUT TO TAPE ROUTINE:     *
*
*   START VECTOR = 82CF (F9,FA)  *
*   PUT START DUMP ADR AT F0,F1  *
*  END DUMP ADR AT F2,F3    *
*  START TAPE; DEPRESS G/H  *
*  01 IN DISPLAY = END OF TRANSMISSION *
*
*
*
*
*
*   INPUT FROM TAPE ROUTINE         *
*
*   START VECTOR = 82F9
*   DEPRESS G/H; START TAPE.    *
*
*   PROGRAM STATUS IS IN DISPLAY:    *
*   $80 = PROGRAM RUNNING   *
*   $81 = TAPE LEADER TONE DETECTED *       
*   $02 = NO MEMORY AT LOAD ADR  *
*   $03 = SYNC ERROR     *
*   $04 = TONE TRAILER DETECTED   *
*
**** OUTPUT TO TAPE ROUTINE   *****
*

