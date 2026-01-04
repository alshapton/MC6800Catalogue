        .OR     $8200

L8200:
        LDAB    $00,X
        BITA    #$02
        BNE     $821B
        LDX     $FD
        STAB    $00,X
        CMPB    $00,X
        BNE     $8278
        STAB    $4002
        INX
        STX     $FD
        LDAA    #$0F
L8216:
        STAA    $4001
        BRA     $823F
L821B:
        LDAB    $FE
        STAB    $02,X
        LDAA    #$0D
        BRA     $8216
L8223:
        LDX     $FD
        BITB    #$02
        BEQ     $8232
        LDAA    $00,X
        STAA    $4002
        LDAB    #$35
        BRA     $823C
L8232:
        INX
        STX     $FD
        LDAA    $FE
        STAA    $4002
        LDAB    #$37
L823C:
        STAB    $4003
L823F:
        BRA     $82A9

        ; Interrupt Request Entry Point
        ;
L8241:
        LDAB    $4003
        BITB    #$40
        BEQ     $824B
        JMP     $0080
L824B:
        LDX     #$0960
L824E:
        DEX
        BNE     $824E
        LDX     #$4000
        LDAB    $03,X
        LDAA    $01,X
        BMI     $8260
        BITA    #$40
        BNE     $8266
        BRA     $8223
L8260:
        BITA    #$40
        BNE     $82BC
        BRA     $8200
L8266:
        BITB    #$80
        BNE     $82C2
        BRA     $8284
L826C:
        LDX     #$0000
        STX     $F0
        LDX     #$00F0
        STX     $F2
        BRA     $82CF
L8278:
        LDX     #$61A8
        CLI
        COM     $4002
L827F:
        DEX
        BNE     $827F
        BRA     $8278
L8284:
        BITA    #$10
        BEQ     $8293
        LDAA    #$0D
        STAA    $01,X
        TST     $00,X
        LDAA    $FF
        BEQ     $82A9
        RTI
L8293:
        LDAA    #$1D
        STAA    $01,X
        LDAA    $FF
        BEQ     $82B4

        ; Software Interrupt Entry Point
        ;
L829B:
        TSX
        LDAA    $06,X
        DECA
L829F:
        STAA    $4002
        STS     $FD
        STS     $FB
        CLR     $00FF
L82A9:
        LDS     #$2200
        TST     $4002
        TST     $4000
        CLI
        WAI
L82B4:
        LDS     $FB
        COM     $00FF
        TST     $00,X
        RTI
L82BC:
        LDAA    $00,X
        STAA    $FD
        BRA     $82C6
L82C2:
        LDAA    $00,X
        STAA    $FE
L82C6:
        LDX     $FD
        LDAA    $00,X
        STAA    $4002
        BRA     $82A9
L82CF:
        LDS     #$00FF
        BSR     $8300

        ; Reset Entry Point
        ;
L82D4:
        CLRA
        STAA    $F9
L82D7:
        CLRB
        STAB    $FF
        STAB    $F4
        LDS     #$00F3
        CLR     $4001
        CLR     $4003
        LDX     #$000D
        STX     $4000
        LDX     #$FF35
        STX     $4002
        BRA     $829F
L82F3:
        LDAA    $4000
        NEGA
        STAA    $FF
        LDS     #$00FE
        BSR     $8361
        BRA     $82D7
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