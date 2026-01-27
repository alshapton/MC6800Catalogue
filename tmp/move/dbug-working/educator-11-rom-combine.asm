; Assembled with sbasm3 (https://www.sbprojects.net/sbasm/);        NAM    DBUG
; All directives are specific to sbasm3, and may need to   ; *2/2/77    VER  1.0
; be changed for other assemblers                          ; *
        .CR 6800             ; LOAD MC6800 CROSS OVERLAY   ;         OPT     0,S
        .TF e.exe,BIN        ; OUTPUT FILE IN BINARY FORMAT; *
        .OR $8200            ; START OF ASSEMBLY ADDRESS   ; ********************************
        .LI   ON             ; TOGGLE ASSEMBLY LISTING     ; *   RAM SPACE ALLOCATION
        .LF listing.txt      ; OUTPUT FROM ASSEMBLY        ; *
        .EF errors.err       ; ERROR FILE                  ; *   FLAG - GO/HALT STATUS
        .SF ED2.SYM          ; CREATE SYMBOL FILE          ; *
                                                           ; *   PANADR - TWO BYTES FOR PANEL ADR
                                                           ; *
                                                           ; *   STACK1 - STORAGE FOR STACK POINTER
                                                           ; *           ON ENTRY TO "HALT"  POINTS
                                                           ; *           TO STACK USED FOR GO RTI
                                                           ; *
                                                           ; *
                                                           ; *   STACK2 - "BOTTOM" OF STACK AREA USED IN
                                                           ; *            GO/HALT OPERATIONS  TARGET
                                                           ; *            PROGRAM START VECTOR WILL BE
                                                           ; *            AT FA (PCL) AND F9 (PCH)
                                                           ; *
                                                           ; ***********************************
                                                           ; *
                                                           ;         ORG     $F0
                                                           ; *
BEGADR  .EQU    $F0                                        ; BEGADR  RMB     2
LAST    .EQU    $F2                                        ; LAST    RMB     1
STACK2  .EQU    $F3                                        ; STACK2  RMB     1
CCR     .EQU    $F4                                        ; CCR     RMB     1
ACCB    .EQU    $F5                                        ; ACCB    RMB     1
ACCA    .EQU    $F6                                        ; ACCA    RMB     1
IXH     .EQU    $F7                                        ; IXH     RMB     1
IXL     .EQU    $F8                                        ; IXL     RMB     1
PRGHI   .EQU    $F9                                        ; PRGHI   RMB     1
PRGLO   .EQU    $FA                                        ; PRGLO   RMB     1
STACK1  .EQU    $FB                                        ; STACK1  RMB     2
PANADR  .EQU    $FD                                        ; PANADR  RMB     2
FLAG    .EQU    $FF                                        ; FLAG    RMB     1
                                                           ; *
                                                           ; *
                                                           ; *
BYTE    .EQU    ACCA                                       ; BYTE    EQU     ACCA
DRA     .EQU    $4000                                      ; DRA     EQU     $4000
DRB     .EQU    DRA+2                                      ; DRB     EQU     DRA+2
CRA     .EQU    DRA+1                                      ; CRA     EQU     DRA+1
CRB     .EQU    DRB+1                                      ; CRB     EQU     DRB+1
STACK   .EQU    $2200                                      ; STACK   EQU     $2200
                                                           ; *
                                                           ; *
                                                           ;         ORG     $8200
                                                           ; *
                                                           ; *
                                                           ; *     SUBROUTINE DEPOSIT     *
                                                           ; *
                                                           ; *
DEP     LDAB    0,X      CLR INT FLAG IN PIA               ; DEP     LDA B   0,X      CLR INT FLAG IN PIA
        BITA    #02      DISPLAY DATA IF ZERO ADDRESS      ;         BIT A   #02      DISPLAY DATA IF ZERO ADDRESS
        BNE     ADR                                        ;         BNE     ADR
DATA1   LDX     PANADR                                     ; DATA1   LDX     PANADR
        STAB    0,X      PLACE PANEL DATA IN MEMORY        ;         STA B   0,X      PLACE PANEL DATA IN MEMORY
        CMPB    0,X                                        ;         CMP B   0,X
        BNE     BLINK                                      ;         BNE     BLINK
        STAB    DRB      PLACE DATA INTO DISPLAY           ;         STA B   DRB      PLACE DATA INTO DISPLAY
        INX                                                ;         INX
        STX     PANADR   INCREMENT AND STORE PANEL ADD     ;         STX     PANADR   INCREMENT AND STORE PANEL ADD
        LDAA    #$0F     SET INT FOR POS EDGE              ;         LDA A   #$0F     SET INT FOR POS EDGE
STORA   STAA    CRA                                        ; STORA   STA A   CRA
        BRA     BACK1                                      ;         BRA     BACK1
ADR     LDAB    PANADR+1                                   ; ADR     LDA B   PANADR+1
        STAB    2,X      PUT LSBYTE OF ADR INTO DISP       ;         STA B   2,X      PUT LSBYTE OF ADR INTO DISP
        LDAA    #$0D     SETS INT FOR NEG EDGE             ;         LDA A   #$0D     SETS INT FOR NEG EDGE
        BRA     STORA                                      ;         BRA     STORA
                                                           ; *
                                                           ; *
                                                           ; ****    SUBROUTINE EXAMINE     *****
                                                           ; *
                                                           ; *
                                                           ; *
EXAM    LDX     PANADR   GET PANEL ADR FOR DATA FETCH      ; EXAM    LDX     PANADR   GET PANEL ADR FOR DATA FETCH
        BITB    #$2                                        ;         BIT B   #$2
        BEQ     ADR2     NEG EDGE=ADR IN DISPLAY           ;         BEQ     ADR2     NEG EDGE=ADR IN DISPLAY
DATA2   LDAA    0,X                                        ; DATA2   LDA A   0,X
        STAA    DRB      PUT OLD DATA INTO DISPLAY         ;         STA A   DRB      PUT OLD DATA INTO DISPLAY
        LDAB    #$35                                       ;         LDA B   #$35
        BRA     STORB                                      ;         BRA     STORB
ADR2    INX              INCREMENT PANEL ADR               ; ADR2    INX              INCREMENT PANEL ADR
        STX     PANADR   STORE NEW PANEL ADR               ;         STX     PANADR   STORE NEW PANEL ADR
        LDAA    PANADR+1 GET LSBYTE OF PANEL ADR           ;         LDA A   PANADR+1 GET LSBYTE OF PANEL ADR
        STAA    DRB      LSBYTE OF PANEL ADR INTO DISP     ;         STA A   DRB      LSBYTE OF PANEL ADR INTO DISP
        LDAB    #$37                                       ;         LDA B   #$37
STORB   STAB    CRB                                        ; STORB   STA B   CRB
BACK1   BRA     BACK                                       ; BACK1   BRA     BACK
                                                           ; *
                                                           ; *
                                                           ; *********   IRQ ROUTINE *********
                                                           ; *
                                                           ; *       CHECK FOR CB2 INTERRUPT *****
                                                           ; *
                                                           ; *
IRQ     .EQU    *                                          ; IRQ     EQU     *
        LDAB    CRB                                        ;         LDA B   CRB
        BITB    #$40                                       ;         BIT B   #$40
        BEQ     DELAY                                      ;         BEQ     DELAY
        JMP     $80                                        ;         JMP     $80
                                                           ; *
                                                           ; *
                                                           ; ********  DEBOUNCE ROUTINE ******
                                                           ; *
                                                           ; *
DELAY   LDX     #2400    SET DEBOUNCE TYPE                 ; DELAY   LDX     #2400    SET DEBOUNCE TYPE
D2      DEX                                                ; D2      DEX
        BNE     D2                                         ;         BNE     D2
        LDX     #DRA                                       ;         LDX     #DRA
                                                           ; *
                                                           ; *
                                                           ; *   IRQ SERVICE CONTINUED **
                                                           ; *
                                                           ; *
        LDAB    3,X                                       ;         LDA B    3,X
        LDAA    1,X                                       ;         LDA A    1,X
        BMI     D1       CA1 = DEPOSIT                     ;         BMI     D1       CA1 = DEPOSIT
        BITA    #$40     NOT CA1. TEST CA2                 ;         BIT A   #$40     NOT CA1. TEST CA2
        BNE     D3                                         ;         BNE     D3
        BRA     EXAM                                       ;         BRA     EXAM
D1      BITA    #$40                                       ; D1      BIT A   #$40
        BNE     MSBYTE                                     ;         BNE     MSBYTE
        BRA     DEP      CA1 AND NOT CA2                   ;         BRA     DEP      CA1 AND NOT CA2
D3      BITB    #$80                                       ; D3      BIT B   #$80
        BNE     LSBYTE                                     ;         BNE     LSBYTE
        BRA     GOHALT                                     ;         BRA     GOHALT
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *        ENTRY FOR RAM DUMP.  $00 - $F0
                                                           ; *
                                                           ; *
                                                           ; *
        LDX     #0                                         ;         LDX     #0
        STX     $F0                                        ;         STX     $F0
        LDX     #$F0                                       ;         LDX     #$F0
        STX     $F2                                        ;         STX     $F2
        BRA     TAPOUT                                     ;         BRA     TAPOUT
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; ***** BLINKING DISPLAY ROUTING *******
                                                           ; *
                                                           ; *
                                                           ; *
BLINK   LDX     #25000                                     ; BLINK   LDX     #25000
        CLI                                                ;         CLI
        COM     DRB                                        ;         COM     DRB
LOOP    DEX                                                ; LOOP    DEX
        BNE     LOOP                                       ;         BNE     LOOP
        BRA     BLINK                                      ;         BRA     BLINK
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *       GO/HALT ROUTINE         *
                                                           ; *
                                                           ; *
GOHALT  BITA    #$10     MASK FOR EDGE BIT                 ; GOHALT  BIT A   #$10     MASK FOR EDGE BIT
        BEQ     OK       IF ZERO, NEG EDGE                 ;         BEQ     OK       IF ZERO, NEG EDGE
        LDAA    #$0D                                       ;         LDA A   #$0D
        STAA    1,X                                        ;         STA A   1,X
        TST     0,X                                        ;         TST     0,X
        LDAA    FLAG                                       ;         LDA A   FLAG
        BEQ     BACK                                       ;         BEQ     BACK
        RTI                                                ;         RTI
OK      LDAA    #$1D     SET FOR POS EDGE                  ; OK      LDA A   #$1D     SET FOR POS EDGE
        STAA    1,X                                        ;         STA A   1,X
        LDAA    FLAG                                       ;         LDA A   FLAG
        BEQ     GO       ZERO=GO, FF=HALT                  ;         BEQ     GO       ZERO=GO, FF=HALT
HALT    TSX                                                ; HALT    TSX
        LDAA    6,X      FETCH PCL FROM STACK              ;         LDAA    6,X      FETCH PCL FROM STACK
        DECA                                               ;         DEC A
HALT3   STAA    DRB      PUT PCL-1 INTO DISPLAY            ; HALT3   STA A   DRB      PUT PCL-1 INTO DISPLAY
HALT2   STS     PANADR   STORE STACK AT PANEL ADR          ; HALT2   STS     PANADR   STORE STACK AT PANEL ADR
        STS     STACK1   STORE USER'S STACK POINTER        ;         STS     STACK1   STORE USER'S STACK POINTER
        CLR     FLAG     RESET FLAG FOR "GO"               ;         CLR     FLAG     RESET FLAG FOR "GO"
BACK    LDS     #STACK   RESTORE PANEL PHANTOM STACK       ; BACK    LDS     #STACK   RESTORE PANEL PHANTOM STACK
        TST     DRB      CLR INT                           ;         TST     DRB      CLR INT
        TST     DRA                                        ;         TST     DRA
        CLI              READY FOR GO                      ;         CLI              READY FOR GO
        WAI                                                ;         WAI
GO      LDS     STACK1   RESTORE USER'S STACK POINTER      ; GO      LDS     STACK1   RESTORE USER'S STACK POINTER
        COM     FLAG     SET FLG FOR HALT                  ;         COM     FLAG     SET FLG FOR HALT
        TST     0,X      LOADS "OLD" STACK INTO PROCSR     ;         TST     0,X      LOADS "OLD" STACK INTO PROCSR
        RTI                                                ;         RTI
                                                           ; *
                                                           ; *
                                                           ; ******   SUBROUTINE M S BYTE  ********
                                                           ; *
                                                           ; *
MSBYTE  LDAA    0,X                                        ; MSBYTE  LDA A   0,X
        STAA    PANADR                                     ;         STAA    PANADR
        BRA     FETCH                                      ;         BRA     FETCH
                                                           ; *
                                                           ; *
                                                           ; *     SUBROUTINE LOAD LSBYTE  *
                                                           ; *
                                                           ; *
LSBYTE  LDAA    0,X      LOAD SWITCH INFO INTO ACCA        ; LSBYTE  LDA A   0,X      LOAD SWITCH INFO INTO ACCA
        STAA    PANADR+1 STORE LSBYTE INTO PANEL ADR       ;         STA A   PANADR+1 STORE LSBYTE INTO PANEL ADR
FETCH   LDX     PANADR   GET DATA AT PANEL ADR             ; FETCH   LDX     PANADR   GET DATA AT PANEL ADR
        LDAA    0,X                                        ;         LDA A   0,X
        STAA    DRB      DISPLAY DATA IN PANEL ADR         ;         STA A   DRB      DISPLAY DATA IN PANEL ADR
        BRA     BACK                                       ;         BRA     BACK
                                                           ; *
                                                           ; *
                                                           ; *
TAPOUT  LDS     #$FF                                       ; TAPOUT  LDS     #$FF
        BSR     INITIA                                     ;         BSR     INITIA
                                                           ; *
                                                           ; *
START   CLRA                                               ; START   CLR A
        STAA    PRGHI                                      ;         STA A   PRGHI
START1  CLRB                                               ; START1  CLR B
        STAB    FLAG                                       ;         STA B   FLAG
        STAB    CCR                                        ;         STA B   CCR
        LDS     #STACK2                                    ;         LDS     #STACK2
INZ     CLR     CRA                                        ; INZ     CLR     CRA
        CLR     CRB                                        ;         CLR     CRB
        LDX     #$000D                                     ;         LDX     #$000D
        STX     $4000                                      ;         STX     $4000
        LDX     #$FF35                                     ;         LDX     #$FF35
        STX     $4002                                      ;         STX     $4002
                                                           ; *
        BRA     HALT3                                      ;         BRA     HALT3
                                                           ; *
                                                           ; *
                                                           ; * CASSETTE INPUT ENTRY FOR FILE  *
                                                           ; * NUMBER IN SWITCHES  *
                                                           ; *
TAPIN1  LDAA    DRA                                        ; TAPIN1  LDA A    DRA
        NEGA                                               ;         NEG A
        STAA    FLAG                                       ;         STAA    FLAG
                                                           ; *
                                                           ; * CASSETTE INPUT ENTRY FOR NEXT FILE *
                                                           ; *
        LDS     #$00FE                                     ;         LDS     #$00FE
        BSR     TAPE2                                      ;         BSR     TAPE2
        BRA     START1                                     ;         BRA     START1
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; * *** CASSETTE INTERFACE PROGRAM
                                                           ; *
                                                           ; *
                                                           ; *   OUTPUT TO TAPE ROUTINE:     *
                                                           ; *
                                                           ; *   START VECTOR = 82CF (F9,FA)  *
                                                           ; *   PUT START DUMP ADR AT F0,F1  *
                                                           ; *  END DUMP ADR AT F2,F3    *
                                                           ; *  START TAPE; DEPRESS G/H  *
                                                           ; *  01 IN DISPLAY = END OF TRANSMISSION *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *
                                                           ; *   INPUT FROM TAPE ROUTINE         *
                                                           ; *
                                                           ; *   START VECTOR = 82F9
                                                           ; *   DEPRESS G/H; START TAPE.    *
                                                           ; *
                                                           ; *   PROGRAM STATUS IS IN DISPLAY:    *
                                                           ; *   $80 = PROGRAM RUNNING   *
                                                           ; *   $81 = TAPE LEADER TONE DETECTED *
                                                           ; *   $02 = NO MEMORY AT LOAD ADR  *
                                                           ; *   $03 = SYNC ERROR     *
                                                           ; *   $04 = TONE TRAILER DETECTED   *
                                                           ; *
                                                           ; **** OUTPUT TO TAPE ROUTINE   *****
                                                           ; *
                                                           ;
; Code below here supplied by disassembing second ROM in 
; the EDUCATOR-II, since early editions of the manual only 
; had the first ROM source code shown.                                                           
; Therefore, disassembled code on the right:
INITIA                                                     ; L8300:
        SEI                                                ;         SEI
        BSR     R2LBL8                                     ;         BSR     $8339
        LDAA    $F0                                        ;         LDAA    $F0
        BSR     $831C                                      ;         BSR     $831C
        LDAA    $F1                                        ;         LDAA    $F1
        BSR     $831C                                      ;         BSR     $831C
        LDX     $F0                                        ;         LDX     $F0
INITLP                                                     ; L830D:
        LDAA    0,X                                        ;         LDAA    $00,X
        BSR     $831C                                      ;         BSR     $831C
        CPX     $F2                                        ;         CPX     $F2
        BEQ     $8318                                      ;         BEQ     $8318
        INX                                                ;         INX
        BRA     INITLP                                     ;         BRA     $830D
R2LBL1
L8318:                                                     ; L8318:
        BSR     $8350                                      ;         BSR     $8350
        BRA     R2LBL8                                     ;         BRA     $8339
R2LBL2        
L831C:                                                     ; L831C:
        STAA    $F6                                        ;         STAA    $F6
        BSR     $8350                                      ;         BSR     $8350
        LDAB    #$09                                       ;         LDAB    #$09
        BRA     $832E                                      ;         BRA     $832E
R2LBL3
L8324:                                                     ; L8324:
        DECB                                               ;         DECB
        BEQ     $8338                                      ;         BEQ     $8338
        BSR     $8350                                      ;         BSR     $8350
        ROL     $00F6                                      ;         ROL     $00F6
        BCS     $8334                                      ;         BCS     $8334
R2LBL4
L832E:                                                     ; L832E:
        LDAA    #$14                                       ;         LDAA    #$14
R2LBL5
L8330:                                                     ; L8330:
        BSR     $8343                                      ;         BSR     $8343
        BRA     $8324                                      ;         BRA     $8324
R2LBL6
L8334:                                                     ; L8334:
        LDAA    #$28                                       ;         LDAA    #$28
        BRA     $8330                                      ;         BRA     $8330
R2LBL7
L8338:                                                     ; L8338:
        RTS                                                ;         RTS
R2LBL8                                                     ; L8339:
        LDAB    #$0F                                       ;         LDAB    #$0F
R2LBL9
L833B:                                                     ; L833B:
        LDAA    #$7E                                       ;         LDAA    #$7E
        BSR     $8343                                      ;         BSR     $8343
        DECB                                               ;         DECB
        BNE     $833B                                      ;         BNE     $833B
        RTS                                                ;         RTS
R2LBL10
L8343:                                                     ; L8343:
        STAA    $4002                                      ;         STAA    $4002
R2LBL11
L8346:                                                     ; L8346:
        LDAA    #$19                                       ;         LDAA    #$19
        BSR     $835D                                      ;         BSR     $835D
        DEC     $4002                                      ;         DEC     $4002
        BNE     $8346                                      ;         BNE     $8346
        RTS                                                ;         RTS
R2LBL12
L8350:                                                     ; L8350:
        CLR     $4002                                      ;         CLR     $4002
        BSR     $835B                                      ;         BSR     $835B
        INC     $4002                                      ;         INC     $4002
        BSR     $835B                                      ;         BSR     $835B
        RTS                                                ;         RTS
R2LBL13
L835B:                                                     ; L835B:
        LDAA    #$64                                       ;         LDAA    #$64
R2LBL14
L835D:                                                     ; L835D:
        DECA                                               ;         DECA
        BNE     $835D                                      ;         BNE     $835D
        RTS                                                ;         RTS
TAPE2:                                                     ; L8361:
        LDAA    #$38                                       ;         LDAA    #$38
        LDX     #$4000                                     ;         LDX     #$4000
        TAP                                                ;         TAP
        NOP                                                ;         NOP
        STAA    3,X                                        ;         STAA    $03,X
        LDAB    #$7F                                       ;         LDAB    #$7F
        STAB    2,X                                        ;         STAB    $02,X
        LDAA    #$3C                                       ;         LDAA    #$3C
        STAA    3,X                                        ;         STAA    $03,X
        CLR     2,X                                        ;         CLR     $02,X
        INC     $00FF                                      ;         INC     $00FF
R2LBL15
L8377:                                                     ; L8377:
        BSR     $83C8                                      ;         BSR     $83C8
        TSTA                                               ;         TSTA
        BNE     R2LBL15                                    ;         BNE     $8377
        INC     2,X                                        ;         INC     $02,X
R2LBL16
L837E:                                                     ; L837E:
        BSR     $83C8                                      ;         BSR     $83C8
        TSTA                                               ;         TSTA
        BEQ     $837E                                      ;         BEQ     $837E
        BSR     $83A2                                      ;         BSR     $83A2
        STAA    $F0                                        ;         STAA    $F0
        BSR     $83A2                                      ;         BSR     $83A2
        STAA    $F1                                        ;         STAA    $F1
        LDX     $F0                                        ;         LDX     $F0
R2LBL17
L838D:                                                     ; L838D:
        BSR     $83A2                                      ;         BSR     $83A2
        LDAB    $FF                                        ;         LDAB    $FF
        BNE     $838D                                      ;         BNE     $838D
        STAA    0,X                                        ;         STAA    $00,X
        STAA    $4002                                      ;         STAA    $4002
        CMPA    0,X                                        ;         CMPA    $00,X
        BNE     $839F                                      ;         BNE     $839F
        INX                                                ;         INX
        BRA     $838D                                      ;         BRA     $838D
R2LBL18
L839F:                                                     ; L839F:
        LDAA    #$02                                       ;         LDAA    #$02
R2LBL19
L83A1:                                                     ; L83A1:
        RTS                                                ;         RTS
R2LBL20
L83A2:                                                     ; L83A2:
        LDAB    #$09                                       ;         LDAB    #$09
        BSR     $83C8                                      ;         BSR     $83C8
        TSTA                                               ;         TSTA
        BEQ     $83E0                                      ;         BEQ     $83E0
        CMPA    #$EE                                       ;         CMPA    #$EE
        BCC     $83B5                                      ;         BCC     $83B5
        LDAA    #$03                                       ;         LDAA    #$03
        BRA     $83F5                                      ;         BRA     $83F5
R2LBL21
L83B1:                                                     ; L83B1:
        CLC                                                ;         CLC
R2LBL22
L83B2:                                                     ; L83B2:
        ROL     $00F6                                      ;         ROL     $00F6
R2LBL23
L83B5:                                                     ; L83B5:
        LDAA    $F6                                        ;         LDAA    $F6
        DECB                                               ;         DECB
        BEQ     $83A1                                      ;         BEQ     $83A1
        BSR     $83C8                                      ;         BSR     $83C8
        TSTA                                               ;         TSTA
        BEQ     $83E0                                      ;         BEQ     $83E0
        CMPA    #$F0                                       ;         CMPA    #$F0
        BCC     $83B1                                      ;         BCC     $83B1
        SEC                                                ;         SEC
        BRA     $83B2                                      ;         BRA     $83B2
R2LBL24
L83C6:                                                     ; L83C6:
        BRA     $8361                                      ;         BRA     $8361
R2LBL25
L83C8:                                                     ; L83C8:
        PSHB                                               ;         PSHB
        LDAA    $FF                                        ;         LDAA    $FF
R2LBL26
L83CB:                                                     ; L83CB:
        CLRB                                               ;         CLRB
        TST     $4002                                      ;         TST     $4002
        BPL     $83CB                                      ;         BPL     $83CB
        DECA                                               ;         DECA
        BEQ     $83F6                                      ;         BEQ     $83F6
R2LBL27
L83D4:                                                     ; L83D4:
        TST     $4002                                      ;         TST     $4002
        BPL     $83CB                                      ;         BPL     $83CB
        INCB                                               ;         INCB
        CMPB    #$19                                       ;         CMPB    #$19
        BLS     $83D4                                      ;         BLS     $83D4
        BRA     $83F6                                      ;         BRA     $83F6
R2LBL28
L83E0:                                                     ; L83E0:
        LDX     #$FFFF                                     ;         LDX     #$FFFF
R2LBL29
L83E3:                                                     ; L83E3:
        DEX                                                ;         DEX
        NOP                                                ;         NOP
        BNE     $83E3                                      ;         BNE     $83E3
        LDAA    $FF                                        ;         LDAA    $FF
        BEQ     $83EF                                      ;         BEQ     $83EF
        INS                                                ;         INS
        INS                                                ;         INS
        BRA     $83C6                                      ;         BRA     $83C6
R2LBL30
L83EF:                                                     ; L83EF:
        LDX     $F0                                        ;         LDX     $F0
        STX     $F9                                        ;         STX     $F9
        LDAA    #$04                                       ;         LDAA    #$04
R2LBL31
L83F5:                                                     ; L83F5:
        INS                                                ;         INS
R2LBL32
L83F6:                                                     ; L83F6:
        PULB                                               ;         PULB
        RTS                                                ;         RTS
                                                           ; 
; Interrupt Request Vector                                 ;         ; Interrupt Request Vector
;                                                          ;         ;
        .DB     $82                                        ;         .DB     $82
        .DB     $41                                        ;         .DB     $41
                                                           ; 
; Software Interrupt Vector                                ;         ; Software Interrupt Vector
;                                                          ;         ;
        .DB     $82                                        ;         .DB     $82
        .DB     $9B                                        ;         .DB     $9B
                                                           ; 
; Non-Maskable Interrupt Vector                            ;         ; Non-Maskable Interrupt Vector
;                                                          ;         ;
        .DB     $00                                        ;         .DB     $00
        .DB     $83                                        ;         .DB     $83
                                                           ; 
; Reset Vector                                             ;         ; Reset Vector
;                                                          ;         ;
        .DB     $82                                        ;         .DB     $82
        .DB     $D4                                        ;         .DB     $D4
