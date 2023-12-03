.include "../charmap.asm"

.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0005
heldButtons: .res 1             ; $0006

pulledNumber: .res 2

pulledDigits: .res 3
.res    4

nmiHappened: .res 1
renderMode: .res 1

found:  .res    1

offset: .res    2

total:  .res    3
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

;  ------------------------------
;  ------------------------------

; part 1
moveOffsetBackup: .res 2
symbolOrAsteriskCheckBackup: .res 2
shapeWidth: .res 1
symbolsFound: .res 1

;  ------------------------------
; part 2

; The number to be multiplied is the “multiplicand”,
; and the number by which it is multiplied is the “multiplier”.

multiplier: .res 2
multiplicand: .res 2

product: .res   3               ; largest expected is 999*999

foundNumberSlots:
foundNumberSlot1: .res 2
foundNumberSlot2: .res 2
adjacentNumberCount: .res 1     ; increment to keep track of how many found.  Use last bit to pick slot


;  ------------------------------
;  ------------------------------

.res    $AA

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
        .byte   >oneDigit,>twoDigit,>threeDigit
digitTableLo:
        .byte   <oneDigit,<twoDigit,<threeDigit

threeDigit:
        .addr   multBy100TableLo
        .addr   multBy100TableHi
twoDigit:
        .addr   multBy10TableLo
        .addr   multBy10TableHi
oneDigit:
        .addr   multBy1TableLo
        .addr   multBy1TableHi

multBy100TableLo:
        .byte   $00,$64,$c8,$2c,$90,$f4,$58,$bc,$20,$84
multBy100TableHi:
        .byte   $00,$00,$00,$01,$01,$01,$02,$02,$03,$03
multBy10TableLo:
        .byte   $00,$0a,$14,$1e,$28,$32,$3c,$46,$50,$5a
multBy1TableLo:
        .byte   $00,$01,$02,$03,$04,$05,$06,$07,$08,$09
multBy10TableHi:
multBy1TableHi:
        .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00

loDigitLoc      = tmp1
hiDigitLoc      = tmpQ

isItANumber:
        ; carry clear == number
        ldy     #$00
        lda     (offset),y
        sec
        sbc     #asciiOffset
        bcc     setCarryAndReturn
        cmp     #$A
        rts

isItASymbol:
        ; carry clear == symbol
        jsr     isItANumber
        bcs     maybeASymbol
setCarryAndReturn:
        sec
        rts
maybeASymbol:
        ldy     #$00
        lda     (offset),y
        cmp     #symDot
        clc
        beq     setCarryAndReturn
        rts

isItAnAsterisk:
        ; carry clear == asterisk
        ldy     #$00
        lda     (offset),y
        cmp     symAsterisk
        bne     setCarryAndReturn
        clc
        rts


restoreOffsetForSymbolOrAsteriskCheck:
        lda     symbolOrAsteriskCheckBackup
        sta     offset
        lda     symbolOrAsteriskCheckBackup+1
        sta     offset+1
        rts

backupOffsetForSymbolOrAsteriskCheck:
        lda     offset
        sta     symbolOrAsteriskCheckBackup
        lda     offset+1
        sta     symbolOrAsteriskCheckBackup+1
        rts


isSymbolAdjacent:
        ; carry set - symbol adjacent
        jsr     backupOffsetForSymbolOrAsteriskCheck

; check left first
        jsr     moveLeft
        bcc     @restoreAndCheckRight
        jsr     isItASymbol
        bcc     @SymbolFound

@restoreAndCheckRight:
        jsr     restoreOffsetForSymbolOrAsteriskCheck
; check left first
        jsr     moveRight
        bcc     @restoreAndCheckUp
        jsr     isItASymbol
        bcc     @SymbolFound

@restoreAndCheckUp:
        jsr     restoreOffsetForSymbolOrAsteriskCheck
        jsr     moveUp
        bcc     @restoreAndCheckDown
        jsr     isItASymbol
        bcc     @SymbolFound

; check Up Left
        jsr     moveLeft
        bcc     @restoreAndCheckUpRight
        jsr     isItASymbol
        bcc     @SymbolFound

@restoreAndCheckUpRight:
; we dunno what the offset is here, so reset and start new
        jsr     restoreOffsetForSymbolOrAsteriskCheck
        jsr     moveUp
        jsr     moveRight
        bcc     @restoreAndCheckDown
        jsr     isItASymbol
        bcc     @SymbolFound

@restoreAndCheckDown:
        jsr     restoreOffsetForSymbolOrAsteriskCheck
        jsr     moveDown
        bcc     @SymbolNotFound
        jsr     isItASymbol
        bcc     @SymbolFound

; check Down Left
        jsr     moveLeft
        bcc     @restoreAndCheckDownRight
        jsr     isItASymbol
        bcc     @SymbolFound

        @restoreAndCheckDownRight:
        ; we dunno what the offset is here, so reset and start new
        jsr     restoreOffsetForSymbolOrAsteriskCheck
        jsr     moveDown
        jsr     moveRight
        bcc     @SymbolNotFound
        jsr     isItASymbol
        bcs     @SymbolNotFound

@SymbolFound:
        jsr     restoreOffsetForSymbolOrAsteriskCheck
        sec
        rts

@SymbolNotFound:
        jsr     restoreOffsetForSymbolOrAsteriskCheck
        clc
        rts

; PART 2!!!

areNumbersAdjacent:
        ; carry set - symbol adjacent
        jsr     backupOffsetForSymbolOrAsteriskCheck
        rts

multiplyTwoNumbers:
        lda     #$00
        sta     product
        sta     product+1
        sta     product+2

        ldy     multiplier+1
        ldx     multiplier
        beq     @decrementY

@addAnotherLayer:
        clc
        lda     multiplicand
        adc     product
        sta     product

        lda     multiplicand+1
        adc     product+1
        sta     product+1

        lda     #$00
        adc     product+2
        sta     product+2
        dex
        bne     @addAnotherLayer
@decrementY:
        dey
        cpy     #$FF
        bne     @addAnotherLayer
        rts





pullOutNumber:
        ldx     #$00
        stx     pulledNumber
        stx     pulledNumber+1
        stx     symbolsFound
@pullDigit:
        jsr     isItANumber
        bcs     @NaN
        sta     pulledDigits,x

        jsr     isSymbolAdjacent; carry set if found.  Keep a sum
        lda     #$00
        adc     symbolsFound
        sta     symbolsFound

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
        asl                     ; * 4
        tay
        lda     (tmpX),y
        sta     loDigitLoc
        iny
        lda     (tmpX),y
        sta     loDigitLoc+1

        iny
        lda     (tmpX),y
        sta     hiDigitLoc
        iny
        lda     (tmpX),y
        sta     hiDigitLoc+1

        ldy     pulledDigits,x
        lda     (loDigitLoc),y
        clc
        adc     pulledNumber
        sta     pulledNumber
        lda     (hiDigitLoc),y
        adc     pulledNumber+1
        sta     pulledNumber+1
        dex
        bmi     @ret
        jmp     @pullDigitLoop
@ret:
        ldy     #$00
        rts


incrementY:
        jsr     incrementOffset
        dey
        bne     incrementY
        rts

incrementOffset:
        inc     offset
        bne     @ret
        inc     offset+1
@ret:   rts

decrementOffset:
        dec     offset
        lda     offset
        cmp     #$FF
        bne     @ret
        dec     offset+1
@ret:   rts



setShapeWidth:
        ldy     #$00
        ldx     #$00
        lda     offset
        pha
        lda     offset+1
        pha
@findNewline:
        inx
        jsr     incrementOffset
        lda     (offset),y
        cmp     #newline
        bne     @findNewline
        stx     shapeWidth
        pla
        sta     offset+1
        pla
        sta     offset
        rts

moveUp:
        ; carryset = valid
        jsr     backupOffset
        clc                     ;  -1 to account for newline
        lda     offset
        sbc     shapeWidth
        sta     offset
        lda     offset+1
        sbc     #$00
        sta     offset+1
        ; set offset
        sec
        lda     offset
        sbc     #<data
        lda     offset+1
        sbc     #>data
        bcc     restoreOffset
        rts

moveDown:
        ; carryset = valid
        jsr     backupOffset
        sec                     ;  +1 to account for newline
        lda     offset
        adc     shapeWidth
        sta     offset

        lda     offset+1
        adc     #$00
        sta     offset+1
        sec
        lda     #<endOfData
        sbc     offset
        lda     #>endOfData
        sbc     offset+1
        bcc     restoreOffset
        rts

backupOffset:
        lda     offset
        sta     moveOffsetBackup
        lda     offset+1
        sta     moveOffsetBackup+1
        rts

restoreOffset:
        lda     moveOffsetBackup
        sta     offset
        lda     moveOffsetBackup+1
        sta     offset+1
        rts

moveLeft:
        ; carryset = valid
        jsr     backupOffset
        jsr     decrementOffset
        ldy     #$00
        lda     (offset),y
        cmp     #newline
        clc
        beq     restoreOffset
        sec
        rts

moveRight:
        ; carryset = valid
        jsr     backupOffset
        jsr     incrementOffset
        ldy     #$00
        lda     (offset),y
        cmp     #newline
        clc
        beq     restoreOffset
        sec
        rts

loopInit:
        lda     #<data
        sta     offset
        lda     #>data
        sta     offset+1
        jsr     setShapeWidth

loop:
        ldy     #$00
        lda     (offset),y
        beq     endOfLoop
        cmp     #newline
        bne     @checkIfNumber
@incrementAndJump:
        jsr     incrementOffset
        jmp     loop

@checkIfNumber:
        jsr     isItANumber
        bcs     @incrementAndJump

        jsr     pullOutNumber
        lda     symbolsFound
        beq     loop
        clc
        lda     pulledNumber
        adc     total
        sta     total

        lda     pulledNumber+1
        adc     total+1
        sta     total+1

        lda     #$00
        adc     total+2
        sta     total+2

        jmp     loop


somethingWrong:
        inc     errorFlag

endOfLoop:
        inc     found

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
        ; lda     #$d2
        ; sta     decBuffer
        ; lda     #$02
        ; sta     decBuffer+1
        ; lda     #$96
        ; sta     decBuffer+2
        ; lda     #$49
        ; sta     decBuffer+3


        ; test 69420
        lda     #$34
        sta     multiplicand
        lda     #$00
        sta     multiplicand+1

        lda     #$37
        sta     multiplier
        lda     #$05
        sta     multiplier+1
        jsr     multiplyTwoNumbers

        lda     product
        sta     decBuffer
        lda     product+1
        sta     decBuffer+1
        lda     product+2
        sta     decBuffer+2
        lda     #$00
        sta     decBuffer+3

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
        .incbin "day03.input"
.byte   $00
endOfData:

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code
