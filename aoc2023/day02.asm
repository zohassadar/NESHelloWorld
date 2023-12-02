.include "../charmap.asm"

.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0005
heldButtons: .res 1             ; $0006

pulledNumber: .res 1
pulledDigits: .res 3
.res 4

nmiHappened: .res 1
renderMode: .res 1 

found: .res 1

offset: .res 2

total: .res 2

total2: .res 3

totalFrames: .res 2

tmpX: .res 1
tmpY: .res 1
tmpZ: .res 1


maxRed: .res 1
maxGreen: .res 1
maxBlue: .res 1


; The number to be multiplied is the “multiplicand”, 
; and the number by which it is multiplied is the “multiplier”.

multiplier: .res 1
multiplicand: .res 2

product: .res 2

wordTemp: .res 2

currentGameId: .res 1

errorFlag: .res 1

.res    $CA

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

BUTTON_A :=     $80
BUTTON_B :=     $40
BUTTON_SELECT := $20
BUTTON_START := $10
BUTTON_UP :=    $8
BUTTON_DOWN :=  $4
BUTTON_LEFT :=  $2
BUTTON_RIGHT := $1

asciiOffset :=  $30

newline :=      $0a
semicolon :=    $3b
colon   :=      $3a
space   :=      $20
comma   :=      $2c

gameId  :=      $47
gameHop :=      5

redId   :=      $72
redHop  :=      3

greenId :=      $67
greepHop :=     5

blueId  :=      $62
blueHop :=      4

redLimit :=     12
greenLimit :=   13
blueLimit :=    14

oneDigit:
        .addr   multBy1Table

twoDigit:
        .addr   multBy10Table
        .addr   multBy1Table

threeDigit:
        .addr   multBy100Table
        .addr   multBy10Table
        .addr   multBy1Table

multBy100Table:
        .byte   $00,$64
multBy10Table:
        .byte   $00,$0a,$14,$1e,$28,$32,$3c,$46,$50,$5a
multBy1Table:
        .byte   $00,$01,$02,$03,$04,$05,$06,$07,$08,$09

digitTableHi:
        .byte   >oneDigit,>twoDigit,>threeDigit
digitTableLo:
        .byte   <oneDigit,<twoDigit,<threeDigit


pullOutNumber:
        ldx     #$00
        stx     pulledNumber
@pullDigit:
        ldy     #$00
        lda     (offset),y
        sec
        sbc     #asciiOffset
        cmp     #$A
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
        tay
        lda     (tmpX),y
        sta     tmp1
        iny
        lda     (tmpX),y
        sta     tmp2
        ldy     pulledDigits,x
        lda     (tmp1),y
        clc
        adc     pulledNumber
        sta     pulledNumber
        dex
        bmi     @ret
        jmp     @pullDigitLoop
@ret:
        ldy     #$00
        rts


multiplyTwoNumbers:
        lda   #$00
        sta   product
        sta   product+1
@loop:
        dec   multiplier
        bmi   @ret
        clc
        lda   multiplicand
        adc   product
        sta   product
        lda   multiplicand+1
        adc   product+1
        sta   product+1
        jmp   @loop
@ret:   rts



findGameIdOrEnd:
        ldy     #$00
        lda     (offset),y
        bne     @notEnd
        inc     found
        jmp     endOfLoop
@notEnd:
        cmp     #gameId
        bne     @notGameId
        ldy     #gameHop
        jsr     incrementY
        jsr     pullOutNumber
        lda     pulledNumber
        sta     currentGameId
        lda     #$00
        sta     maxRed
        sta     maxGreen
        sta     maxBlue
        jsr     incrementOffset
        jsr     incrementOffset
        jmp     pullAndIdentifyNumber
@notGameId:
        jsr     incrementOffset
        jmp     findGameIdOrEnd


loopInit:
        lda     #<data
        sta     offset
        lda     #>data
        sta     offset+1
        jmp     findGameIdOrEnd

incrementY:
        jsr     incrementOffset
        dey
        bne     incrementY
        rts


pullAndIdentifyNumber:
        jsr     pullOutNumber
        jsr     incrementOffset
        ldx     pulledNumber
        ldy     #$00
        lda     (offset),y

;checkRed
        cmp     #redId
        bne     @checkBlue
        ldy     #redHop
        jsr     incrementY
        txa
        cmp     maxRed
        bcc     @checkRedLimit
        sta     maxRed
@checkRedLimit:
        cmp     #redLimit+1
        bcc     whatsNext
        lda     #$00
        sta     currentGameId
        jmp     whatsNext

@checkBlue:
        cmp     #blueId
        bne     @checkGreen
        ldy     #blueHop
        jsr     incrementY
        txa
        cmp     maxBlue
        bcc     @checkBlueLimit
        sta     maxBlue
@checkBlueLimit:
        cmp     #blueLimit+1
        bcc     whatsNext
        lda     #$00
        sta     currentGameId
        jmp     whatsNext

@checkGreen:
        cmp     #greenId
        beq     @green
        jmp     somethingWrong
@green:
        ldy     #greepHop
        jsr     incrementY
        txa
        cmp     maxGreen
        bcc     @checkGreenLimit
        sta     maxGreen
@checkGreenLimit:
        cmp     #greenLimit+1
        bcc     whatsNext
        lda     #$00
        sta     currentGameId

whatsNext:
        ldy     #$00
        lda     (offset),y
        cmp     #comma
        bne     @checkSemiColon
@incrementAndJump:
        jsr     incrementOffset
        jsr     incrementOffset
        jmp     pullAndIdentifyNumber

@checkSemiColon:
        cmp     #semicolon
        beq     @incrementAndJump

; check end of game
        cmp     #newline
        bne     somethingWrong


; part 1 add
        lda     currentGameId
        clc
        adc     total
        sta     total
        lda     #$00
        adc     total+1
        sta     total+1
; part 2 add
        lda     maxRed
        sta     multiplicand
        lda     #$00
        sta     multiplicand+1
        lda     maxGreen
        sta     multiplier
        jsr     multiplyTwoNumbers
        lda     product
        sta     multiplicand
        lda     product+1
        sta     multiplicand+1
        lda     maxBlue
        sta     multiplier
        jsr     multiplyTwoNumbers
        clc
        lda     product
        adc     total2
        sta     total2
        lda     product+1
        adc     total2+1
        sta     total2+1
        lda     #$00
        adc     total2+2
        sta     total2+2
        
        jmp     findGameIdOrEnd



somethingWrong:
        inc     errorFlag
endOfLoop:
        jmp     endOfLoop


incrementOffset:
        inc     offset
        bne     @ret
        inc     offset+1
@ret:   rts

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
        bne     @errorCondition
        lda     #$20
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringHello
        ldy     #>stringHello
        jsr     sendWordToPPU

        lda     #$21
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringAnswer
        ldy     #>stringAnswer
        jsr     sendWordToPPU
        lda     total+1
        jsr     twoDigitsToPPU
        lda     total
        jsr     twoDigitsToPPU

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

cursorHiBytes:
        .byte   $20,$21,$21,$22
cursorLoBytes:
        .byte   $A1,$21,$A1,$21

ramtestOffsets:
        .byte   $00,$00,$00,$00,$02,$02,$04,$04
fifoOffsets:
        .byte   $00,$00,$08,$08,$08,$08



stringHello:
        .byte   " Hello there",$00
stringAnswer:
        .byte   " Result: $",$00
stringTotalFrames:
        .byte   " Frame Count: $",$00
stringSomethingWrong:
        .byte   " Something Wrong",$00


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
        .incbin "day02.input"
.byte   $00

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code
