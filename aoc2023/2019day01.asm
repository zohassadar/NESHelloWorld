.include "../charmap.asm"

.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0005
heldButtons: .res 1             ; $0006

pulledNumber: .res 3

pulledDigits: .res 6
.res    4

nmiHappened: .res 1
renderMode: .res 1

found:  .res    1

offset: .res    2

total:  .res    4
total2: .res    4

totalFrames: .res 2

tmpQ:   .res    1
tmpR:   .res    1

tmpX:   .res    1
tmpY:   .res    1

tmpZ:   .res    1

wordTemp: .res  2

errorFlag: .res 1

result1DecimalOut: .res 10
result2DecimalOut: .res 10

decBuffer: .res 4
decResult: .res 4

generalCounter: .res 1

loDigitLoc: .res 2
medDigitLoc: .res 2
hiDigitLoc: .res 2

chunk: .res 3
sum: .res 3
test: .res 3

;  ------------------------------
;  ------------------------------


;  ------------------------------
;  ------------------------------

.res    $A0

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

symOctothorpe   = $23           ; #
symDollar       = $24           ; $
symPercent      = $25           ; %
symAmpersand    = $26           ; &
symAsterisk     = $2a           ; *
symPlus         = $2b           ; +
symHyphen       = $2d           ; -

symDot          = $2e           ; .

symSlash        = $2f           ; /


; $30 ; '0'
; $31 ; '1'
; $32 ; '2'
; $33 ; '3'
; $34 ; '4'
; $35 ; '5'
; $36 ; '6'
; $37 ; '7'
; $38 ; '8'
; $39 ; '9'

symEqual        = $3d           ; =
symAt           = $40           ; @

compareBytes:
bytesFor1b:
        .byte   $00,$ca,$9a,$3b
bytesFor100m:
        .byte   $00,$e1,$f5,$05
bytesFor10m:
        .byte   $80,$96,$98,$00
bytesFor1m:
        .byte   $40,$42,$0f,$00
bytesFor100k:
        .byte   $a0,$86,$01,$00
bytesFor10k:
        .byte   $10,$27,$00,$00
bytesFor1k:
        .byte   $e8,$03,$00,$00
bytesFor100:
        .byte   $64,$00,$00,$00
bytesFor10:
        .byte   $0a,$00,$00,$00
bytesFor1:
        .byte   $01,$00,$00,$00

bytesForCompare:
        .addr   bytesFor1b
        .addr   bytesFor100m
        .addr   bytesFor10m
        .addr   bytesFor1m
        .addr   bytesFor100k
        .addr   bytesFor10k
        .addr   bytesFor1k
        .addr   bytesFor100
        .addr   bytesFor10
        .addr   bytesFor1


convert4BytesToDecimal:
; 4 bytes need to be in decBuffer. x is offset to result
        lda     #$00            ; count up, stop at 10
        sta     generalCounter

@getMod:
        lda     generalCounter
        asl
        asl
        tay                     ; y = generalCounter * 4
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
        jmp     @doSubtraction

@nextDigit:
        inx
        inc     generalCounter
        lda     generalCounter
        cmp     #$0A
        bne     @getMod
        rts

digitTableHi:
        .byte   >oneDigit,>twoDigit,>threeDigit,>fourDigit,>fiveDigit,>sixDigit
digitTableLo:
        .byte   <oneDigit,<twoDigit,<threeDigit,<fourDigit,<fiveDigit,<sixDigit

sixDigit:
        .addr   multBy100000TableLo
        .addr   multBy100000TableMed
        .addr   multBy100000TableHi
fiveDigit:
        .addr   multBy10000TableLo
        .addr   multBy10000TableMed
        .addr   multBy10000TableHi
fourDigit:
        .addr   multBy1000TableLo
        .addr   multBy1000TableMed
        .addr   multBy1000TableHi
threeDigit:
        .addr   multBy100TableLo
        .addr   multBy100TableMed
        .addr   multBy100TableHi
twoDigit:
        .addr   multBy10TableLo
        .addr   multBy10TableMed
        .addr   multBy10TableHi
oneDigit:
        .addr   multBy1TableLo
        .addr   multBy1TableMed
        .addr   multBy1TableHi


multBy100000TableHi:
        .byte $00,$01,$03,$04,$06,$07,$09,$0a,$0c,$0d
multBy100000TableMed:
        .byte $00,$86,$0d,$93,$1a,$a1,$27,$ae,$35,$bb
multBy100000TableLo:
        .byte $00,$a0,$40,$e0,$80,$20,$c0,$60,$00,$a0

multBy10000TableHi:
        .byte $00,$00,$00,$00,$00,$00,$00,$01,$01,$01
multBy10000TableMed:
        .byte $00,$27,$4e,$75,$9c,$c3,$ea,$11,$38,$5f
multBy10000TableLo:
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90

; hi is zeros
multBy1000TableMed:
        .byte $00,$03,$07,$0b,$0f,$13,$17,$1b,$1f,$23
multBy1000TableLo:
        .byte $00,$e8,$d0,$b8,$a0,$88,$70,$58,$40,$28

; hi is zeros
multBy100TableMed:
        .byte $00,$00,$00,$01,$01,$01,$02,$02,$03,$03
multBy100TableLo:
        .byte $00,$64,$c8,$2c,$90,$f4,$58,$bc,$20,$84
; hi is zeros
; med is zeros
multBy10TableLo:
        .byte $00,$0a,$14,$1e,$28,$32,$3c,$46,$50,$5a
; hi is zeros
; med is zeros
multBy1TableLo:
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09
multBy1TableHi:
multBy1TableMed:
multBy10TableMed:
multBy10TableHi:
multBy100TableHi:
multBy1000TableHi:
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00




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

; end Part 2 funcs
incrementOffset:
        inc     offset
        bne     @ret
        inc     offset+1
@ret:   rts

pullOutNumber:
        ldx     #$00
        stx     pulledNumber
        stx     pulledNumber+1
        stx     pulledNumber+2
@pullDigit:
        jsr     isItANumber
        bcs     @NaN
        sta     pulledDigits,x

        jsr     incrementOffset
        inx
        bpl     @pullDigit
@NaN:
        dex
        lda     digitTableLo,x
        sta     tmpX
        lda     digitTableHi,x
        sta     tmpX+1

@pullDigitLoop:
        txa
        asl
        sta     generalCounter
        asl                     
        clc
        adc     generalCounter ; * 6
        tay
        lda     (tmpX),y
        sta     loDigitLoc
        iny
        lda     (tmpX),y
        sta     loDigitLoc+1

        iny
        lda     (tmpX),y
        sta     medDigitLoc
        iny
        lda     (tmpX),y
        sta     medDigitLoc+1

        iny
        lda     (tmpX),y
        sta     hiDigitLoc
        iny
        lda     (tmpX),y
        sta     hiDigitLoc+1

        ldy     pulledDigits,x
        clc
        lda     (loDigitLoc),y
        adc     pulledNumber
        sta     pulledNumber

        lda     (medDigitLoc),y
        adc     pulledNumber+1
        sta     pulledNumber+1

        lda     (hiDigitLoc),y
        adc     pulledNumber+2
        sta     pulledNumber+2
        dex
        bmi     @ret
        jmp     @pullDigitLoop
@ret:
        ldy     #$00
        rts

; def bin_division_by_3(x):
;     original = x
;     sum = 1
;     r = 0
;     while x > 3:
;         q = x >> 2 # divide by 4
;         r = x & 3 # last 2 bits
;         sum+=q # our sum
;         # print(f"{x=} {q=} {r=} {sum=}")
;         x = q + r
;     test = sum * 3
;     return sum - 1 if test > original else sum

doPart1ThingToNumber:
; setSum
        ldx     #$01
        stx     sum
        dex
        stx     sum+1
        stx     sum+2

; set chunk
        lda     pulledNumber
        sta     chunk
        lda     pulledNumber+1
        sta     chunk+1
        lda     pulledNumber+2
        sta     chunk+2

@doItAgain:
; set remainder
        lda     chunk
        and     #$03
        pha

; chunk >> 2
        lsr     chunk+2
        ror     chunk+1
        ror     chunk
        
        lsr     chunk+2
        ror     chunk+1
        ror     chunk

; add chunk to sum
        clc
        lda     chunk
        adc     sum
        sta     sum

        lda     chunk+1
        adc     sum+1
        sta     sum+1

        lda     chunk+2
        adc     sum+2
        sta     sum+2

; add remainder to chunk
        clc
        pla
        adc     chunk
        sta     chunk

        lda     #$00
        adc     chunk+1
        sta     chunk+1

        lda     #$00
        adc     chunk+2
        sta     chunk+2

; subtract 4 to test if loop
        sec
        lda     chunk
        sbc     #$04
        lda     chunk+1
        sbc     #$00
        lda     chunk+2
        sbc     #$00
        bcs     @doItAgain

@testNumber:
        lda     sum
        sta     test
        lda     sum+1
        sta     test+1
        lda     sum+2
        sta     test+2

        ; multiply by 2
        asl     test
        rol     test+1
        rol     test+2

        ; add again to make * 3
        clc
        lda     sum
        adc     test
        sta     test

        lda     sum+1
        adc     test+1
        sta     test+1

        lda     sum+2
        adc     test+2
        sta     test+2

        ; actual test is happening here
        sec
        lda     pulledNumber
        sbc     test
        lda     pulledNumber+1
        sbc     test+1
        lda     pulledNumber+2
        sbc     test+2
        ; carry is clear if test is bigger
        

        ; shortcut to correct rounding and to subtract 2

        lda     sum
        sbc     #$02
        sta     sum

        lda     sum+1
        sbc     #$00
        sta     sum+1

        lda     sum+2
        sbc     #$00
        sta     sum+2

        clc
        lda     sum
        adc     total
        sta     total

        lda     sum+1
        adc     total+1
        sta     total+1

        lda     sum+2
        adc     total+2
        sta     total+2

        lda     #$00
        adc     total+3
        sta     total+3

        rts


loopInit:
        lda     #<data
        sta     offset
        lda     #>data
        sta     offset+1
loop:
        ldy     #$00
        lda     (offset),y
        beq     endOfLoop
        cmp     #newline
        beq     @incrementAndJump
        jsr     pullOutNumber
        jsr     doPart1ThingToNumber

@incrementAndJump:
        jsr     incrementOffset
        jmp     loop

somethingWrong:
        inc     errorFlag

endOfLoop:
        lda     total
        sta     decBuffer
        lda     total+1
        sta     decBuffer+1
        lda     total+2
        sta     decBuffer+2
        lda     #$00
        sta     decBuffer+3
        ldx     #result1DecimalOut
        jsr     convert4BytesToDecimal

        ; test 1234567890
        lda     #$d2
        sta     decBuffer
        lda     #$02
        sta     decBuffer+1
        lda     #$96
        sta     decBuffer+2
        lda     #$49
        sta     decBuffer+3

        inc     found

        ; lda     total2
        ; sta     decBuffer
        ; lda     total2+1
        ; sta     decBuffer+1
        ; lda     total2+2
        ; sta     decBuffer+2
        ; lda     total2+3
        ; sta     decBuffer+3

        ldx     #result2DecimalOut
        jsr     convert4BytesToDecimal

loop4Ever:
        jmp     loop4Ever



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

        lda     #$21
        sta     PPUADDR
        lda     #$C1
        sta     PPUADDR
        ldx     #<stringAnswer
        ldy     #>stringAnswer
        jsr     sendWordToPPU
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

.byte   newline                 ; this keeps from having to do anything funky for the first row
data:
        .incbin "2019day01.input"
endOfData:
        .byte $00

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code

resetPoint = endOfData-1
halfway = (resetPoint-data)/2


