.include "../charmap.asm"
.include "../macros.asm"

.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0005
heldButtons: .res 1             ; $0006
nmiHappened: .res 1
renderMode: .res 1

pulledNumber: .res 5
pulledDigits: .res 13
found:  .res    1

offset: .res    2

seedStash: .res 2
mapsStart: .res 2

seed:   .res    5
seed_range: .res 5

trans:  .res    5
start:  .res    5
range:  .res    5

stop: .res 5
end_: .res 5
bump:  .res 5 
new_span : .res 5
xlated_start: .res 5
xlated_end: .res 5
xlate_stop:  .res 5
total:  .res    5
total2: .res    5

totalFrames: .res 2

digitHiTmp: .res 2
digitLoTmp: .res 2
wordTemp: .res  2

errorFlag: .res 1

result1DecimalOut: .res 13
result2DecimalOut: .res 13

decBuffer: .res 5
decResult: .res 5

digit0Loc: .res 2
digit1Loc: .res 2
digit2Loc: .res 2
digit3Loc: .res 2
digit4Loc: .res 2

generalCounter: .res 1


.res    $50

.bss
stack:
        .res    $100

unused:
        .res    $100
        .res    $100
        .res    $100
        .res    $100
        .res    $100
        .res    $100

.segment "PRG"

PPUCTRL :=      $2000
PPUMASK :=      $2001
PPUSTATUS :=    $2002
OAMADDR :=      $2003
OAMDATA :=      $2004
PPUSCROLL :=    $2005
PPUADDR :=      $2006
PPUDATA :=      $2007

DMC_FREQ :=     $4010
JOYPAD1 :=      $4016
JOY2_APUFC :=   $4017

BUTTON_A        = $80
BUTTON_B        = $40
BUTTON_SELECT   = $20
BUTTON_START    = $10
BUTTON_UP       = $8
BUTTON_DOWN     = $4
BUTTON_LEFT     = $2
BUTTON_RIGHT    = $1

asciiOffset     = $30

EOF             = $00
newline         = $0a

.macro  INCREMENT_OFFSET
        inc     offset
        bne     :+
        inc     offset+1
:
.endmacro

compareBytes:
bytesFor1t:
        .byte   $00,$10,$a5,$d4,$e8
bytesFor100b:
        .byte   $00,$e8,$76,$48,$17
bytesFor10b:
        .byte   $00,$e4,$0b,$54,$02
bytesFor1b:
        .byte   $00,$ca,$9a,$3b,$00
bytesFor100m:
        .byte   $00,$e1,$f5,$05,$00
bytesFor10m:
        .byte   $80,$96,$98,$00,$00
bytesFor1m:
        .byte   $40,$42,$0f,$00,$00
bytesFor100k:
        .byte   $a0,$86,$01,$00,$00
bytesFor10k:
        .byte   $10,$27,$00,$00,$00
bytesFor1k:
        .byte   $e8,$03,$00,$00,$00
bytesFor100:
        .byte   $64,$00,$00,$00,$00
bytesFor10:
        .byte   $0a,$00,$00,$00,$00
bytesFor1:
        .byte   $01,$00,$00,$00,$00


convert4BytesToDecimal:
; 4 bytes need to be in decBuffer. x is offset to result
        lda     #$00            ; count up, stop at 10
        sta     generalCounter

@getMod:
        lda     generalCounter
        asl
        asl
        clc
        adc     generalCounter
        tay                     ; y = generalCounter * 5
@doSubtraction:
        sec
        lda     decBuffer
        sbc     compareBytes,y
        sta     decResult

        lda     decBuffer+1
        sbc     compareBytes+1,y
        sta     decResult+1

        lda     decBuffer+2
        sbc     compareBytes+2,y
        sta     decResult+2

        lda     decBuffer+3
        sbc     compareBytes+3,y
        sta     decResult+3

        lda     decBuffer+4
        sbc     compareBytes+4,y
        sta     decResult+4

        bcc     @nextDigit

        ; cannot inc (tmpX),y
        inc     tmp1,x

        lda     decResult
        sta     decBuffer
        lda     decResult+1
        sta     decBuffer+1
        lda     decResult+2
        sta     decBuffer+2
        lda     decResult+3
        sta     decBuffer+3
        lda     decResult+4
        sta     decBuffer+4
        jmp     @doSubtraction

@nextDigit:
        inx
        inc     generalCounter
        lda     generalCounter
        cmp     #$0D
        bne     @getMod
        rts

digitTables:
        .addr   oneDigitLo
        .addr   oneDigitHi
        .addr   twoDigitLo
        .addr   twoDigitHi
        .addr   threeDigitLo
        .addr   threeDigitHi
        .addr   fourDigitLo
        .addr   fourDigitHi
        .addr   fiveDigitLo
        .addr   fiveDigitHi
        .addr   sixDigitLo
        .addr   sixDigitHi
        .addr   sevenDigitLo
        .addr   sevenDigitHi
        .addr   eightDigitLo
        .addr   eightDigitHi
        .addr   nineDigitLo
        .addr   nineDigitHi
        .addr   tenDigitLo
        .addr   tenDigitHi
        .addr   elevenDigitLo
        .addr   elevenDigitHi
        .addr   twelveDigitLo
        .addr   twelveDigitHi
        .addr   thirteenDigitLo
        .addr   thirteenDigitHi


thirteenDigitHi:
        .byte   >multBy1000000000000Table0
        .byte   >multBy1000000000000Table1
        .byte   >multBy1000000000000Table2
        .byte   >multBy1000000000000Table3
        .byte   >multBy1000000000000Table4
twelveDigitHi:
        .byte   >multBy100000000000Table0
        .byte   >multBy100000000000Table1
        .byte   >multBy100000000000Table2
        .byte   >multBy100000000000Table3
        .byte   >multBy100000000000Table4
elevenDigitHi:
        .byte   >multBy10000000000Table0
        .byte   >multBy10000000000Table1
        .byte   >multBy10000000000Table2
        .byte   >multBy10000000000Table3
        .byte   >multBy10000000000Table4
tenDigitHi:
        .byte   >multBy1000000000Table0
        .byte   >multBy1000000000Table1
        .byte   >multBy1000000000Table2
        .byte   >multBy1000000000Table3
        .byte   >multBy1000000000Table4
nineDigitHi:
        .byte   >multBy100000000Table0
        .byte   >multBy100000000Table1
        .byte   >multBy100000000Table2
        .byte   >multBy100000000Table3
        .byte   >multBy100000000Table4
eightDigitHi:
        .byte   >multBy10000000Table0
        .byte   >multBy10000000Table1
        .byte   >multBy10000000Table2
        .byte   >multBy10000000Table3
        .byte   >multBy10000000Table4
sevenDigitHi:
        .byte   >multBy1000000Table0
        .byte   >multBy1000000Table1
        .byte   >multBy1000000Table2
        .byte   >multBy1000000Table3
        .byte   >multBy1000000Table4
sixDigitHi:
        .byte   >multBy100000Table0
        .byte   >multBy100000Table1
        .byte   >multBy100000Table2
        .byte   >multBy100000Table3
        .byte   >multBy100000Table4
fiveDigitHi:
        .byte   >multBy10000Table0
        .byte   >multBy10000Table1
        .byte   >multBy10000Table2
        .byte   >multBy10000Table3
        .byte   >multBy10000Table4
fourDigitHi:
        .byte   >multBy1000Table0
        .byte   >multBy1000Table1
        .byte   >multBy1000Table2
        .byte   >multBy1000Table3
        .byte   >multBy1000Table4
threeDigitHi:
        .byte   >multBy100Table0
        .byte   >multBy100Table1
        .byte   >multBy100Table2
        .byte   >multBy100Table3
        .byte   >multBy100Table4
twoDigitHi:
        .byte   >multBy10Table0
        .byte   >multBy10Table1
        .byte   >multBy10Table2
        .byte   >multBy10Table3
        .byte   >multBy10Table4
oneDigitHi:
        .byte   >multBy1Table0
        .byte   >multBy1Table1
        .byte   >multBy1Table2
        .byte   >multBy1Table3
        .byte   >multBy1Table4

thirteenDigitLo:
        .byte   <multBy1000000000000Table0
        .byte   <multBy1000000000000Table1
        .byte   <multBy1000000000000Table2
        .byte   <multBy1000000000000Table3
        .byte   <multBy1000000000000Table4
twelveDigitLo:
        .byte   <multBy100000000000Table0
        .byte   <multBy100000000000Table1
        .byte   <multBy100000000000Table2
        .byte   <multBy100000000000Table3
        .byte   <multBy100000000000Table4
elevenDigitLo:
        .byte   <multBy10000000000Table0
        .byte   <multBy10000000000Table1
        .byte   <multBy10000000000Table2
        .byte   <multBy10000000000Table3
        .byte   <multBy10000000000Table4
tenDigitLo:
        .byte   <multBy1000000000Table0
        .byte   <multBy1000000000Table1
        .byte   <multBy1000000000Table2
        .byte   <multBy1000000000Table3
        .byte   <multBy1000000000Table4
nineDigitLo:
        .byte   <multBy100000000Table0
        .byte   <multBy100000000Table1
        .byte   <multBy100000000Table2
        .byte   <multBy100000000Table3
        .byte   <multBy100000000Table4
eightDigitLo:
        .byte   <multBy10000000Table0
        .byte   <multBy10000000Table1
        .byte   <multBy10000000Table2
        .byte   <multBy10000000Table3
        .byte   <multBy10000000Table4
sevenDigitLo:
        .byte   <multBy1000000Table0
        .byte   <multBy1000000Table1
        .byte   <multBy1000000Table2
        .byte   <multBy1000000Table3
        .byte   <multBy1000000Table4
sixDigitLo:
        .byte   <multBy100000Table0
        .byte   <multBy100000Table1
        .byte   <multBy100000Table2
        .byte   <multBy100000Table3
        .byte   <multBy100000Table4
fiveDigitLo:
        .byte   <multBy10000Table0
        .byte   <multBy10000Table1
        .byte   <multBy10000Table2
        .byte   <multBy10000Table3
        .byte   <multBy10000Table4
fourDigitLo:
        .byte   <multBy1000Table0
        .byte   <multBy1000Table1
        .byte   <multBy1000Table2
        .byte   <multBy1000Table3
        .byte   <multBy1000Table4
threeDigitLo:
        .byte   <multBy100Table0
        .byte   <multBy100Table1
        .byte   <multBy100Table2
        .byte   <multBy100Table3
        .byte   <multBy100Table4
twoDigitLo:
        .byte   <multBy10Table0
        .byte   <multBy10Table1
        .byte   <multBy10Table2
        .byte   <multBy10Table3
        .byte   <multBy10Table4
oneDigitLo:
        .byte   <multBy1Table0
        .byte   <multBy1Table1
        .byte   <multBy1Table2
        .byte   <multBy1Table3
        .byte   <multBy1Table4




        ;900000000000
multBy1000000000000Table4:
        .byte   $00,$e8,$d1,$ba,$a3,$8c,$74,$5d,$46,$2f
multBy1000000000000Table3:
        .byte   $00,$d4,$a9,$7d,$52,$27,$fb,$d0,$a5,$79
multBy1000000000000Table2:
        .byte   $00,$a5,$4a,$ef,$94,$39,$de,$83,$28,$cd
multBy1000000000000Table1:
        .byte   $00,$10,$20,$30,$40,$50,$60,$70,$80,$90

multBy100000000000Table4:
        .byte   $00,$17,$2e,$45,$5d,$74,$8b,$a2,$ba,$d1
multBy100000000000Table3:
        .byte   $00,$48,$90,$d9,$21,$6a,$b2,$fb,$43,$8c
multBy100000000000Table2:
        .byte   $00,$76,$ed,$64,$db,$52,$c9,$40,$b7,$2e
multBy100000000000Table1:
        .byte   $00,$e8,$d0,$b8,$a0,$88,$70,$58,$40,$28

multBy10000000000Table4:
        .byte   $00,$02,$04,$06,$09,$0b,$0d,$10,$12,$14
multBy10000000000Table3:
        .byte   $00,$54,$a8,$fc,$50,$a4,$f8,$4c,$a0,$f4
multBy10000000000Table2:
        .byte   $00,$0b,$17,$23,$2f,$3b,$47,$53,$5f,$6b
multBy10000000000Table1:
        .byte   $00,$e4,$c8,$ac,$90,$74,$58,$3c,$20,$04

multBy1000000000Table4:
        .byte   $00,$00,$00,$00,$00,$01,$01,$01,$01,$02
multBy1000000000Table3:
        .byte   $00,$3b,$77,$b2,$ee,$2a,$65,$a1,$dc,$18
multBy1000000000Table2:
        .byte   $00,$9a,$35,$d0,$6b,$05,$a0,$3b,$d6,$71
multBy1000000000Table1:
        .byte   $00,$ca,$94,$5e,$28,$f2,$bc,$86,$50,$1a

multBy100000000Table3:
        .byte   $00,$05,$0b,$11,$17,$1d,$23,$29,$2f,$35
multBy100000000Table2:
        .byte   $00,$f5,$eb,$e1,$d7,$cd,$c3,$b9,$af,$a4
multBy100000000Table1:
        .byte   $00,$e1,$c2,$a3,$84,$65,$46,$27,$08,$e9

multBy10000000Table3:
        .byte   $00,$00,$01,$01,$02,$02,$03,$04,$04,$05
multBy10000000Table2:
        .byte   $00,$98,$31,$c9,$62,$fa,$93,$2c,$c4,$5d
multBy10000000Table1:
        .byte   $00,$96,$2d,$c3,$5a,$f0,$87,$1d,$b4,$4a
multBy10000000Table0:
        .byte   $00,$80,$00,$80,$00,$80,$00,$80,$00,$80

multBy1000000Table2:
        .byte   $00,$0f,$1e,$2d,$3d,$4c,$5b,$6a,$7a,$89
multBy1000000Table1:
        .byte   $00,$42,$84,$c6,$09,$4b,$8d,$cf,$12,$54
multBy1000000Table0:
        .byte   $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40

multBy100000Table2:
        .byte   $00,$01,$03,$04,$06,$07,$09,$0a,$0c,$0d
multBy100000Table1:
        .byte   $00,$86,$0d,$93,$1a,$a1,$27,$ae,$35,$bb
multBy100000Table0:
        .byte   $00,$a0,$40,$e0,$80,$20,$c0,$60,$00,$a0

multBy10000Table2:
        .byte   $00,$00,$00,$00,$00,$00,$00,$01,$01,$01
multBy10000Table1:
        .byte   $00,$27,$4e,$75,$9c,$c3,$ea,$11,$38,$5f
multBy10000Table0:
        .byte   $00,$10,$20,$30,$40,$50,$60,$70,$80,$90

multBy1000Table1:
        .byte   $00,$03,$07,$0b,$0f,$13,$17,$1b,$1f,$23
multBy1000Table0:
        .byte   $00,$e8,$d0,$b8,$a0,$88,$70,$58,$40,$28

multBy100Table1:
        .byte   $00,$00,$00,$01,$01,$01,$02,$02,$03,$03
multBy100Table0:
        .byte   $00,$64,$c8,$2c,$90,$f4,$58,$bc,$20,$84


multBy10Table0:
        .byte   $00,$0a,$14,$1e,$28,$32,$3c,$46,$50,$5a

multBy1Table0:
        .byte   $00,$01,$02,$03,$04,$05,$06,$07,$08,$09

multBy1Table1:
multBy1Table2:
multBy1Table3:
multBy1Table4:
multBy10Table1:
multBy10Table2:
multBy10Table3:
multBy10Table4:
multBy100Table2:
multBy100Table3:
multBy100Table4:
multBy1000Table2:
multBy1000Table3:
multBy1000Table4:
multBy10000Table3:
multBy10000Table4:
multBy100000Table3:
multBy100000Table4:
multBy1000000Table3:
multBy1000000Table4:
multBy10000000Table4:
multBy100000000Table0:
multBy100000000Table4:
multBy1000000000Table0:
multBy10000000000Table0:
multBy100000000000Table0:
multBy1000000000000Table0:
        .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00

setCarryAndReturn:
        sec
        rts

isItANumber:
        ; carry clear == number
        ldy     #$00
        lda     (offset),y
        sec
        sbc     #asciiOffset
        bcc     setCarryAndReturn
        cmp     #$A
        rts

pullOutNumber:
        ldx     #$00
        stx     pulledNumber
        stx     pulledNumber+1
        stx     pulledNumber+2
        stx     pulledNumber+3
        stx     pulledNumber+4
@pullDigit:
        jsr     isItANumber
        bcs     @NaN
        sta     pulledDigits,x

        INCREMENT_OFFSET
        inx
        bpl     @pullDigit
@NaN:
        dex
        txa
        pha
        asl
        asl
        tax
        lda     digitTables,x
        sta     digitLoTmp
        lda     digitTables+1,x
        sta     digitLoTmp+1

        lda     digitTables+2,x
        sta     digitHiTmp
        lda     digitTables+3,x
        sta     digitHiTmp+1
        pla
        tax
@pullDigitLoop:
        txa
        sta     generalCounter
        asl
        asl
        clc
        adc     generalCounter
        tay
        lda     (digitLoTmp),y
        sta     digit0Loc
        lda     (digitHiTmp),y
        sta     digit0Loc+1

        iny
        lda     (digitLoTmp),y
        sta     digit1Loc
        lda     (digitHiTmp),y
        sta     digit1Loc+1

        iny
        lda     (digitLoTmp),y
        sta     digit2Loc
        lda     (digitHiTmp),y
        sta     digit2Loc+1

        iny
        lda     (digitLoTmp),y
        sta     digit3Loc
        lda     (digitHiTmp),y
        sta     digit3Loc+1

        iny
        lda     (digitLoTmp),y
        sta     digit4Loc
        lda     (digitHiTmp),y
        sta     digit4Loc+1

        ldy     pulledDigits,x
        clc
        lda     (digit0Loc),y
        adc     pulledNumber
        sta     pulledNumber

        lda     (digit1Loc),y
        adc     pulledNumber+1
        sta     pulledNumber+1

        lda     (digit2Loc),y
        adc     pulledNumber+2
        sta     pulledNumber+2

        lda     (digit3Loc),y
        adc     pulledNumber+3
        sta     pulledNumber+3

        lda     (digit4Loc),y
        adc     pulledNumber+4
        sta     pulledNumber+4
        dex
        bmi     @ret
        jmp     @pullDigitLoop
@ret:
        ldy     #$00
        rts


findColonOrEOF:
        ldy     #$00
@loop:
        lda     (offset),y
        beq     clearNReturn
        cmp     #$3a
        beq     setNReturn
        INCREMENT_OFFSET
        jmp     @loop

setNReturn:
        sec
        rts
clearNReturn:
        clc
        rts

findDigitOrNewline:
        ldy     #$00
@loop:
        lda     (offset),y
        cmp     #newline
        beq     clearNReturn
        jsr     isItANumber
        bcc     setNReturn
        INCREMENT_OFFSET
        jmp     @loop

findDigitOrEOF:
        ldy     #$00
@loop:
        lda     (offset),y
        beq     clearNReturn
        jsr     isItANumber
        bcc     setNReturn
        INCREMENT_OFFSET
        jmp     @loop

pushOffsetNewSeedAndStop:
        lda     offset
        pha
        lda     offset+1
        pha

        lda     end_
        pha
        lda     end_+1
        pha
        lda     end_+2
        pha
        lda     end_+3
        pha
        lda     end_+4
        pha

        lda     stop
        pha
        lda     stop+1
        pha
        lda     stop+2
        pha
        lda     stop+3
        pha
        lda     stop+4
        pha

        rts

pullStopSeedAndOffset:
        pla
        sta     stop+4
        pla
        sta     stop+3
        pla
        sta     stop+2
        pla
        sta     stop+1
        pla
        sta     stop

        pla
        sta     seed+4
        pla
        sta     seed+3
        pla
        sta     seed+2
        pla
        sta     seed+1
        pla
        sta     seed

        pla
        sta     offset+1
        pla
        sta     offset
        rts

runThroughMap2:
        jsr     findColonOrEOF
        bcs     @notEnd
        jsr     findDigitOrEOF
        bcs     @notEnd
        ; Add endgame here

        ; skip if seed is zero
        lda    seed+0
        bne    @seedIsntZero
        lda    seed+1
        bne    @seedIsntZero
        lda    seed+2
        bne    @seedIsntZero
        lda    seed+3
        bne    @seedIsntZero
        lda    seed+4
        bne    @seedIsntZero
        rts
@seedIsntZero:
        lda     total2
        bne     @notZero
        lda     total2+1
        bne     @notZero
        lda     total2+2
        bne     @notZero
        lda     total2+3
        bne     @notZero
        lda     total2+4
        bne     @notZero
        beq     @storeSeedAnyway
@notZero:
        sec
        lda     seed
        sbc     total2
        lda     seed+1
        sbc     total2+1
        lda     seed+2
        sbc     total2+2
        lda     seed+3
        sbc     total2+3
        lda     seed+4
        sbc     total2+4
        bcs     @ret  ; return if seed is bigger
@storeSeedAnyway:
; store seed as total otherwise
        lda     seed
        sta     total2
        lda     seed+1
        sta     total2+1
        lda     seed+2
        sta     total2+2
        lda     seed+3
        sta     total2+3
        lda     seed+4
        sta     total2+4
@ret:   rts

@notEnd:
        jsr     pullMapNumbers
        jsr     subtractStartFromSeedAndSaveToBump
        bcs     @startGreaterThanOrEqualToStart
        jmp     @seedLessThanStart
@startGreaterThanOrEqualToStart:
        jsr     addStartToRangeAndSaveToXlateStop
        jsr     subtractXlateStopFromSeed
        bcc     @startPlusRangeGreaterThanSeed
        jmp     @seedGreaterThanOrEqualToStartPlusRange


@startPlusRangeGreaterThanSeed:
; end = stop if stop < xlate_stop else xlate_stop
        sub     5, xlate_stop, stop
        bcc     @setEndToStop

;       set end to xlate_stop
        copy    5, xlate_stop, end_

@setEndToStop:
        copy    5, stop, end_

@endSet:

; set bump
        sub     5, start, seed, bump

; set new_span
        sub     5, seed, end_, new_span

        jsr     pushOffsetNewSeedAndStop
        
        add     5, bump, trans, seed

        add     5, new_span, seed, stop
        jsr     pushOffsetNewSeedAndStop
        jsr     runThroughMap2
        jsr     pullStopSeedAndOffset

        lda     seed+0
        cmp     stop+0
        bne     @seedNotEqualToStop
        lda     seed+1
        cmp     stop+1
        bne     @seedNotEqualToStop
        lda     seed+2
        cmp     stop+2
        bne     @seedNotEqualToStop
        lda     seed+3
        cmp     stop+3
        bne     @seedNotEqualToStop
        lda     seed+4
        cmp     stop+4
        bne     @seedNotEqualToStop
        rts
@seedNotEqualToStop:
@seedGreaterThanOrEqualToStartPlusRange:
@seedLessThanStart:
        jmp     runThroughMap2


        
runThroughMap:
        jsr     findDigitOrEOF
        bcs     @notEnd
; End!  check if total is zero first

        lda     total
        bne     @notZero
        lda     total+1
        bne     @notZero
        lda     total+2
        bne     @notZero
        lda     total+3
        bne     @notZero
        lda     total+4
        bne     @notZero
        beq     @storeSeedAnyway

@notZero:
        sub     5, total, seed
        bcs     @ret  ; return if seed is bigger

@storeSeedAnyway:
; store seed as total otherwise
        copy    5,seed,total
@ret:   rts
@notEnd:
        jsr     pullMapNumbers
        ;     if (seed >= start && seed < start + range){
        jsr     subtractStartFromSeedAndSaveToBump
        bcc     @seedLessThanStart
        jsr     addStartToRangeAndSaveToXlateStop
        jsr     subtractXlateStopFromSeed
        bcs     @seedGreaterThanOrEqualToStartPlusRange
        ;         return trans + (seed-start)
        ;     }
        add     5, bump, trans, seed
        jsr     findColonOrEOF ; Skip the rest of this group
        jmp     runThroughMap

@seedLessThanStart:
@seedGreaterThanOrEqualToStartPlusRange:
        jmp     runThroughMap

processSeed:
        jsr     findDigitOrNewline
        bcc     clearNReturn2
        jsr     pullOutNumber
        copy    5, pulledNumber, seed

        jsr     stashSeed
        jsr     mapRestore

        jsr     runThroughMap

        jmp     setCarryAndReturn

clearNReturn2:
        clc
        rts

processSeedRange:
        ; pull two instead of one
        jsr     findDigitOrNewline
        bcc     clearNReturn2
        jsr     pullOutNumber
        lda     pulledNumber
        sta     seed
        lda     pulledNumber+1
        sta     seed+1
        lda     pulledNumber+2
        sta     seed+2
        lda     pulledNumber+3
        sta     seed+3
        lda     pulledNumber+4
        sta     seed+4

        jsr     findDigitOrNewline
        jsr     pullOutNumber

        clc
        lda     pulledNumber+0
        adc     seed_range+0
        sta     stop+0
        lda     pulledNumber+1
        adc     seed_range+1
        sta     stop+1
        lda     pulledNumber+2
        adc     seed_range+2
        sta     stop+2
        lda     pulledNumber+3
        adc     seed_range+3
        sta     stop+3
        lda     pulledNumber+4
        adc     seed_range+4
        sta     stop+4

        jsr     stashSeed
        jsr     mapRestore

        jsr     runThroughMap2

        jmp     setCarryAndReturn

mapRestore:
        lda     mapsStart
        sta     offset
        lda     mapsStart+1
        sta     offset+1
        rts

stashSeed:
        lda     offset          ; store next position
        sta     seedStash
        lda     offset+1
        sta     seedStash+1
        rts

seedRestore:
        lda     seedStash
        sta     offset
        lda     seedStash+1
        sta     offset+1
        rts

loopInit:
        lda     #<(data+7)
        sta     offset
        sta     seedStash

        lda     #>(data+7)
        sta     offset+1
        sta     seedStash+1

        jsr     findColonOrEOF

        INCREMENT_OFFSET

        lda     offset
        sta     mapsStart
        lda     offset+1
        sta     mapsStart+1
@loop:
        jsr     seedRestore
        jsr     processSeed
        bcc     endOfLoop
        INCREMENT_OFFSET
        jmp     @loop

somethingWrong:
        inc     errorFlag

part2init:
        lda     #<(data+7)
        sta     seedStash

        lda     #>(data+7)
        sta     seedStash+1
        jsr     mapRestore
@loop:
        jsr     seedRestore
        jsr     processSeedRange
        bcc     endOfLoop
        INCREMENT_OFFSET
        jmp     @loop 


endOfLoop:
        inc     found
        copy    5, total, decBuffer

        ldx     #result1DecimalOut
        jsr     convert4BytesToDecimal

        copy    5, total2, decBuffer

        ldx     #result2DecimalOut
        jsr     convert4BytesToDecimal

loop4Ever:
        jmp     loop4Ever



pullMapNumbers:
        jsr     pullOutNumber
        copy    5, pulledNumber, trans

        jsr     findDigitOrEOF
        jsr     pullOutNumber
        copy    5, pulledNumber, start
        jsr     findDigitOrEOF
        jsr     pullOutNumber

        copy    5, pulledNumber, range
        rts

subtractStartFromSeedAndSaveToBump:
        sub     5, start, seed, bump
        rts

addStartToRangeAndSaveToXlateStop:
        add     5, start, range, xlate_stop
        rts


subtractXlateStopFromSeed:
        sub     5, xlate_stop, seed
        rts

; NMI Functions

nmi:    pha
        txa
        pha
        tya
        pha
        inc     frameCounter
        lda     #$01
        sta     nmiHappened
        jsr     renderStuff
        lda     #$00
        sta     PPUSCROLL
        sta     PPUSCROLL
        lda     #%10001000
        sta     PPUCTRL
        lda     #%00001110
        sta     PPUMASK
        jsr     readjoy
        lda     found
        bne     @stopCounting
        inc     totalFrames
        bne     @stopCounting
        inc     totalFrames+1
@stopCounting:
        pla
        tay
        pla
        tax
        pla
irq:    rti



renderStuff:
        lda     errorFlag
        beq     @noErrorCondition
        jmp     @errorCondition
@noErrorCondition:
        lda     #$20
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringHello
        ldy     #>stringHello
        jsr     sendWordToPPU

        lda     #$21
        sta     PPUADDR
        lda     #$61
        sta     PPUADDR
        ldx     #<stringAnswer
        ldy     #>stringAnswer
        jsr     sendWordToPPU
        ; lda     total+4
        ; jsr     twoDigitsToPPU
        lda     total+3
        jsr     twoDigitsToPPU
        lda     total+2
        jsr     twoDigitsToPPU
        lda     total+1
        jsr     twoDigitsToPPU
        lda     total
        jsr     twoDigitsToPPU


        lda     #$21
        sta     PPUADDR
        lda     #$81
        sta     PPUADDR
        ldx     #<stringDecimal
        ldy     #>stringDecimal
        jsr     sendWordToPPU
        lda     result1DecimalOut
        sta     PPUDATA
        lda     result1DecimalOut+1
        sta     PPUDATA
        lda     result1DecimalOut+2
        sta     PPUDATA
        lda     result1DecimalOut+3
        sta     PPUDATA
        lda     result1DecimalOut+4
        sta     PPUDATA
        lda     result1DecimalOut+5
        sta     PPUDATA
        lda     result1DecimalOut+6
        sta     PPUDATA
        lda     result1DecimalOut+7
        sta     PPUDATA
        lda     result1DecimalOut+8
        sta     PPUDATA
        lda     result1DecimalOut+9
        sta     PPUDATA
        lda     result1DecimalOut+10
        sta     PPUDATA
        lda     result1DecimalOut+11
        sta     PPUDATA
        lda     result1DecimalOut+12
        sta     PPUDATA

        lda     #$21
        sta     PPUADDR
        lda     #$C1
        sta     PPUADDR
        ldx     #<stringAnswer
        ldy     #>stringAnswer
        jsr     sendWordToPPU
        ; lda     total2+4
        ; jsr     twoDigitsToPPU
        lda     total2+3
        jsr     twoDigitsToPPU
        lda     total2+2
        jsr     twoDigitsToPPU
        lda     total2+1
        jsr     twoDigitsToPPU
        lda     total2
        jsr     twoDigitsToPPU


        lda     #$21
        sta     PPUADDR
        lda     #$E1
        sta     PPUADDR
        ldx     #<stringDecimal
        ldy     #>stringDecimal
        jsr     sendWordToPPU
        lda     result2DecimalOut
        sta     PPUDATA
        lda     result2DecimalOut+1
        sta     PPUDATA
        lda     result2DecimalOut+2
        sta     PPUDATA
        lda     result2DecimalOut+3
        sta     PPUDATA
        lda     result2DecimalOut+4
        sta     PPUDATA
        lda     result2DecimalOut+5
        sta     PPUDATA
        lda     result2DecimalOut+6
        sta     PPUDATA
        lda     result2DecimalOut+7
        sta     PPUDATA
        lda     result2DecimalOut+8
        sta     PPUDATA
        lda     result2DecimalOut+9
        sta     PPUDATA
        lda     result2DecimalOut+10
        sta     PPUDATA
        lda     result2DecimalOut+11
        sta     PPUDATA
        lda     result2DecimalOut+12
        sta     PPUDATA


        lda     #$22
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringTotalFrames
        ldy     #>stringTotalFrames
        jsr     sendWordToPPU
        lda     totalFrames+1
        jsr     twoDigitsToPPU
        lda     totalFrames
        jmp     twoDigitsToPPU


@errorCondition:
        lda     #$21
        sta     PPUADDR
        lda     #$C1
        sta     PPUADDR
        ldx     #<stringSomethingWrong
        ldy     #>stringSomethingWrong
        jmp     sendWordToPPU

blankOutNameTable:
        ldx     #$C0
        ldy     #$04
        lda     #$20
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR
        lda     #$FF
@blankingLoop:
        sta     PPUDATA
        dex
        bne     @blankingLoop
        dey
        bne     @blankingLoop
waitForVBlank:
        bit     PPUSTATUS
        bpl     waitForVBlank
        rts

sendWordToPPU:
        stx     wordTemp
        sty     wordTemp+1
        ldy     #$00
wordLoop:
        lda     (wordTemp),y
        beq     wordLoopEnd
        sta     PPUDATA
        iny
        bne     wordLoop
wordLoopEnd:
        rts

twoDigitsToPPU:
        pha
        lsr
        lsr
        lsr
        lsr
        sta     PPUDATA
        pla
        and     #$0F
        sta     PPUDATA
        rts


stringHello:
        .byte   " Hello there",$00
stringAnswer:
        .byte   " Result: $",$00
stringTotalFrames:
        .byte   " Frame Count: $",$00
stringSomethingWrong:
        .byte   " Something Wrong",$00
stringDecimal:
        .byte   "    ",$00


; from https://www.nesdev.org/wiki/Controller_reading_code
; At the same time that we strobe bit 0, we initialize the ring counter
; so we're hitting two birds with one stone here
readjoy:
        lda     #$01
        ; While the strobe bit is set, buttons will be continuously reloaded.
        ; This means that reading from JOYPAD1 will only return the state of the
        ; first button: button A.
        sta     JOYPAD1
        sta     newButtons
        lsr     a               ; now A is 0
        ; By storing 0 into JOYPAD1, the strobe bit is cleared and the reloading stops.
        ; This allows all 8 buttons (newly reloaded) to be read from JOYPAD1.
        sta     JOYPAD1
@loop:
        lda     JOYPAD1
        lsr     a               ; bit 0 -> Carry
        rol     newButtons      ; Carry -> bit 0; bit 7 -> Carry
        bcc     @loop
        lda     newButtons
        pha
        eor     heldButtons
        and     newButtons
        sta     newButtons
        pla
        sta     heldButtons
        rts

reset:
        ; from https://www.nesdev.org/wiki/Init_code
        sei                     ; ignore IRQs
        cld                     ; disable decimal mode
        ldx     #$40
        stx     JOY2_APUFC      ; disable APU frame IRQ
        ldx     #$ff
        txs                     ; Set up stack
        inx                     ; now X = 0
        stx     PPUCTRL         ; disable NMI
        stx     PPUMASK         ; disable rendering
        stx     DMC_FREQ        ; disable DMC IRQs

        ; Optional (omitted):
        ; Set up mapper and jmp to further init code here.

        ; The vblank flag is in an unknown state after reset,
        ; so it is cleared here to make sure that @vblankwait1
        ; does not exit immediately.
        bit     PPUSTATUS

        ; First of two waits for vertical blank to make sure that the
        ; PPU has stabilized
        jsr     waitForVBlank

        ; We now have about 30,000 cycles to burn before the PPU stabilizes.
        ; One thing we can do with this time is put RAM in a known state.
        ; Here we fill it with $00, which matches what (say) a C compiler
        ; expects for BSS.  Conveniently, X is still 0.
        txa
@clrmem:
        sta     $0000,x
        sta     $0100,x
        sta     $0200,x
        sta     $0300,x
        sta     $0400,x
        sta     $0500,x
        sta     $0600,x
        sta     $0700,x
        inx
        bne     @clrmem

        ; Other things you can do between vblank waits are set up audio
        ; or set up other mapper registers.

        jsr     waitForVBlank

        lda     #$3F
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR
        ldx     #$0
paletteLoop:
        lda     palette,x
        sta     PPUDATA
        inx
        cpx     #(endpalette-palette)
        bne     paletteLoop

        jsr     blankOutNameTable

        lda     #%10001000
        sta     PPUCTRL
        lda     #%00011110
        sta     PPUMASK
        jmp     loopInit

palette:
        .byte   $0F,$30,$30,$30
        .byte   $0F,$30,$30,$30
        .byte   $0F,$30,$30,$30
        .byte   $0F,$30,$30,$30
        .byte   $0F,$30,$30,$30
        .byte   $0F,$30,$30,$30
        .byte   $0F,$30,$30,$30
        .byte   $0F,$30,$30,$30
endpalette:

data:
        .incbin "day05.input"
endOfData:
        .byte   $00

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code

resetPoint      = endOfData-1
halfway         = (resetPoint-data)/2


