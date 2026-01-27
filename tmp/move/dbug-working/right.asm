L8300:
        SEI
        BSR     $8339
        LDAA    $F0
        BSR     $831C
        LDAA    $F1
        BSR     $831C
        LDX     $F0
L830D:
        LDAA    $00,X
        BSR     $831C
        CPX     $F2
        BEQ     $8318
        INX
        BRA     $830D
L8318:
        BSR     $8350
        BRA     $8339
L831C:
        STAA    $F6
        BSR     $8350
        LDAB    #$09
        BRA     $832E
L8324:
        DECB
        BEQ     $8338
        BSR     $8350
        ROL     $00F6
        BCS     $8334
L832E:
        LDAA    #$14
L8330:
        BSR     $8343
        BRA     $8324
L8334:
        LDAA    #$28
        BRA     $8330
L8338:
        RTS
L8339:
        LDAB    #$0F
L833B:
        LDAA    #$7E
        BSR     $8343
        DECB
        BNE     $833B
        RTS
L8343:
        STAA    $4002
L8346:
        LDAA    #$19
        BSR     $835D
        DEC     $4002
        BNE     $8346
        RTS
L8350:
        CLR     $4002
        BSR     $835B
        INC     $4002
        BSR     $835B
        RTS
L835B:
        LDAA    #$64
L835D:
        DECA
        BNE     $835D
        RTS
L8361:
        LDAA    #$38
        LDX     #$4000
        TAP
        NOP
        STAA    $03,X
        LDAB    #$7F
        STAB    $02,X
        LDAA    #$3C
        STAA    $03,X
        CLR     $02,X
        INC     $00FF
L8377:
        BSR     $83C8
        TSTA
        BNE     $8377
        INC     $02,X
L837E:
        BSR     $83C8
        TSTA
        BEQ     $837E
        BSR     $83A2
        STAA    $F0
        BSR     $83A2
        STAA    $F1
        LDX     $F0
L838D:
        BSR     $83A2
        LDAB    $FF
        BNE     $838D
        STAA    $00,X
        STAA    $4002
        CMPA    $00,X
        BNE     $839F
        INX
        BRA     $838D
L839F:
        LDAA    #$02
L83A1:
        RTS
L83A2:
        LDAB    #$09
        BSR     $83C8
        TSTA
        BEQ     $83E0
        CMPA    #$EE
        BCC     $83B5
        LDAA    #$03
        BRA     $83F5
L83B1:
        CLC
L83B2:
        ROL     $00F6
L83B5:
        LDAA    $F6
        DECB
        BEQ     $83A1
        BSR     $83C8
        TSTA
        BEQ     $83E0
        CMPA    #$F0
        BCC     $83B1
        SEC
        BRA     $83B2
L83C6:
        BRA     $8361
L83C8:
        PSHB
        LDAA    $FF
L83CB:
        CLRB
        TST     $4002
        BPL     $83CB
        DECA
        BEQ     $83F6
L83D4:
        TST     $4002
        BPL     $83CB
        INCB
        CMPB    #$19
        BLS     $83D4
        BRA     $83F6
L83E0:
        LDX     #$FFFF
L83E3:
        DEX
        NOP
        BNE     $83E3
        LDAA    $FF
        BEQ     $83EF
        INS
        INS
        BRA     $83C6
L83EF:
        LDX     $F0
        STX     $F9
        LDAA    #$04
L83F5:
        INS
L83F6:
        PULB
        RTS

        ; Interrupt Request Vector
        ;
        .DB     $82
        .DB     $41

        ; Software Interrupt Vector
        ;
        .DB     $82
        .DB     $9B

        ; Non-Maskable Interrupt Vector
        ;
        .DB     $00
        .DB     $83

        ; Reset Vector
        ;
        .DB     $82
        .DB     $D4
