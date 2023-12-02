.include "../charmap.asm"

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

totalFrames: .res 2

offset: .res 2

total: .res 2
total2: .res 2

firstDigit: .res 1
currentDigit: .res 1

found: .res 1

digitSpelled: .res 1

tmpX: .res 1
tmpY: .res 1
tmpZ: .res 1



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

newline :=      $0a
asciiOffset :=  $30

loop:
        lda     found
        bne     part2Init

        lda     (offset),y
        bne     @notFound
        inc     found
        bne     @addToCurrent
@notFound:
        cmp     #newline
        bne     @notNewline
@addToCurrent:
        ldx     firstDigit
        lda     multBy10Table,x
        clc
        adc     currentDigit
        adc     total
        sta     total
        lda     #$00
        adc     total+1
        sta     total+1
        lda     #$00
        sta     firstDigit
        sta     currentDigit
        jmp     @increment

@notNewline:
        sec
        sbc     #asciiOffset
        bmi     @increment
        cmp     #$0A
        bcs     @increment
        ldx     firstDigit
        bne     @notFirstDigit
        sta     firstDigit
@notFirstDigit:
        sta     currentDigit
@increment:
        inc     offset
        bne     @loop
        inc     offset+1
@loop:
        jmp     loop



part2Init:
        lda     #<data
        sta     offset
        lda     #>data
        sta     offset+1
        ldy     #$00
        sty     total2
        sty     total2+1
        sty     firstDigit
        sty     currentDigit
        sty     found
part2Loop:
        ldy     #$00
        lda     found
        bne     @loop

        lda     (offset),y
        bne     @notFound
        inc     found
        bne     @addToCurrent
@notFound:
        cmp     #newline
        bne     @notNewline
@addToCurrent:
        ldx     firstDigit
        lda     multBy10Table,x
        clc
        adc     currentDigit
        adc     total2
        sta     total2
        lda     #$00
        adc     total2+1
        sta     total2+1
        lda     #$00
        sta     firstDigit
        sta     currentDigit
        jmp     @dontCheckAgain

@notNewline:
        sec
        sbc     #asciiOffset
        bmi     @checkIfSpelled
        cmp     #$0A
        bcs     @checkIfSpelled
@storeFoundDigit:
        ldx     firstDigit
        bne     @notFirstDigit
        sta     firstDigit
@notFirstDigit:
        sta     currentDigit
        jmp     @dontCheckAgain
@checkIfSpelled:
        jsr     checkIfSpelled
        bcc     @storeFoundDigit
@dontCheckAgain:
        inc     offset
        bne     @loop
        inc     offset+1
@loop:
        jmp     part2Loop

digits:
digit1:
        .byte "one",$00,$01
digit2:
        .byte "two",$00,$02
digit3:
        .byte "three",$00,$03
digit4:
        .byte "four",$00,$04
digit5:
        .byte "five",$00,$05
digit6:
        .byte "six",$00,$06
digit7:
        .byte "seven",$00,$07
digit8:
        .byte "eight",$00,$08
digit9:
        .byte "nine",$00,$09
.byte $FF


checkIfSpelled:
        lda  #$00
        sta  digitSpelled

        ldx #$00
        ldy #$00
@checkDigit:
        lda digits,x
        beq @reset
        bmi @notFound
        cmp (offset),y
        beq @equal
        inc digitSpelled
@findZero:
        inx
        lda digits,x
        bne @findZero
        beq @reset
@equal:
        inx
        iny
        bne @checkDigit
@reset:
        lda digitSpelled
        beq @foundNumber
        inx
        inx
        ldy #$00
        sty digitSpelled
        beq @checkDigit
@foundNumber:
        lda digits+1,x
        clc
        rts
@notFound:
        sec
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
        jsr     twoDigitsToPPU
        rts


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
        stx     tmp1
        sty     tmp2
        ldy     #$00
wordLoop:
        lda     (tmp1),y
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


multBy10Table:
        .byte   $00,$0a,$14,$1e,$28,$32,$3c,$46,$50,$5a

stringHello:
        .byte   " Hello there",$00
stringAnswer:
        .byte   " Result: $",$00
stringTotalFrames:
        .byte   " Frame Count: $",$00


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

; initialize offset
        lda     #<data
        sta     offset
        lda     #>data
        sta     offset+1
        ldy     #$00
        sty     total
        sty     total+1
        jmp     loop

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
        .incbin "day01.input"
.byte   $00

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code
