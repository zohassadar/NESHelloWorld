.include "hello-ram.asm"
.include "charmap.asm"

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

FIFO_DATA :=    $40f0
FIFO_STATUS :=  $40f1

; Functions

branches:
        .addr   switchToRamtest
        .addr   switchToFifo
ramtestBranches:
        .addr   noOperation
        .addr   writeBytes
        .addr   readBytes
fifoBranches:
        .addr   sendRepeatedByte
        .addr   readStatusByte
        .addr   readFifoByte
        .addr   countBytesInQueue

branchOffsets:
        .byte   $00,(ramtestBranches-branches)/2,(fifoBranches-branches)/2

branchOnIndex:
        ldy     activeMenu
        lda     menuRow
        clc
        adc     branchOffsets,y
        asl
        tax
        lda     branches,x
        sta     tmp1
        lda     branches+1,x
        sta     tmp2
        jmp     (tmp1)

loop:
        ldy     activeMenu
        lda     menuRow
        bne     @notTopRow

        lda     newButtons
        and     #BUTTON_LEFT
        beq     @leftNotPressed
        dec     menuColumn
        bpl     @leftNotPressed
        lda     upperLimitTopInput,y
        sta     menuColumn
@leftNotPressed:

        lda     newButtons
        and     #BUTTON_RIGHT
        beq     @rightNotPressed
        inc     menuColumn
        lda     menuColumn
        cmp     pastUpperLimitTopInput,y
        bne     @rightNotPressed
        lda     #$00
        sta     menuColumn
@rightNotPressed:
        lda     menuColumn
        beq     @notTopRow
        lda     menuColumn
        clc
        adc     inputBytesOffsets,y
        tax
        lda     newButtons
        and     #BUTTON_UP
        beq     @upNotPressedForDigit
        inc     inputOffset-1,x
        lda     inputOffset-1,x
        and     #$0F
        sta     inputOffset-1,x
@upNotPressedForDigit:
        lda     newButtons
        and     #BUTTON_DOWN
        beq     @downNotPressedForDigit
        dec     inputOffset-1,x
        lda     inputOffset-1,x
        and     #$0F
        sta     inputOffset-1,x
@downNotPressedForDigit:

        jmp     @skipUpDownRead
@notTopRow:
        lda     newButtons
        and     #BUTTON_UP
        beq     @upNotPressed
        dec     menuRow
        bpl     @upNotPressed
        lda     rowLimit,y
        sta     menuRow
@upNotPressed:
        lda     newButtons
        and     #BUTTON_DOWN
        beq     @downNotPressed
        inc     menuRow
        lda     menuRow
        cmp     pastRowLimit,y
        bne     @downNotPressed
        lda     #$00
        sta     menuRow
@downNotPressed:
@skipUpDownRead:
        jsr     moveNybblesToBytes

        lda     newButtons
        and     #BUTTON_A
        beq     @aNotPressed

        jsr     branchOnIndex
        jmp     @finishLoop
@aNotPressed:
        lda     newButtons
        and     #BUTTON_B
        beq     @finishLoop
        lda     activeMenu
        beq     @finishLoop
        jsr     switchToMainMenu
@finishLoop:
        lda     #$00
        sta     nmiHappened
@waitForNmi:
        lda     nmiHappened
        beq     @waitForNmi
        jmp     loop


upperLimitTopInput:
        .byte   $00,$08,$06
pastUpperLimitTopInput:
        .byte   $01,$09,$07
inputBytesOffsets:
        .byte   $00,$00,$08

rowLimit:
        .byte   $01,$02,$03
pastRowLimit:
        .byte   $02,$03,$04


; Switch menus

switchToFifo:
        lda     #$02
        bne     storeAndZero
switchToRamtest:
        lda     #$01
        bne     storeAndZero
switchToMainMenu:
        lda     #$00
storeAndZero:
        sta     activeMenu
        inc     counter
        lda     #$00
        sta     menuColumn
        sta     menuRow
        lda     #$03
        sta     renderMode
        rts

; Ram Test

writeBytes:
        lda     count
        beq     noOperation
        ldy     #$00
@writeLoop:
        lda     startingByte
        sta     (startingAddress),y
        inc     startingByte
        iny
        cpy     count
        bne     @writeLoop
        inc     counter
noOperation:
        rts

readBytes:
        ldy     #$00
@readLoop:
        lda     (startingAddress),y
        sta     readBuffer,y
        iny
        cpy     #$10
        bne     @readLoop
        inc     counter
        rts

; Fifo Test

sendRepeatedByte:
        lda     repeats
        bne     @notZero
        lda     repeats+1
        beq     @ret
@notZero:
        inc     counter
        lda     #$2b            ; "+"
        sta     FIFO_DATA
        eor     #$ff
        sta     FIFO_DATA
        lda     #$22            ; CMD_USB_WR
        sta     FIFO_DATA
        eor     #$ff
        sta     FIFO_DATA
        lda     repeats         ; Length.  16 bit LE
        sta     FIFO_DATA
        lda     repeats+1
        sta     FIFO_DATA
        lda     repeatedByte
        ldy     repeats+1
        ldx     repeats
        beq     @decrementY
@sendByte:
        sta     FIFO_DATA
        dex
        bne     @sendByte
@decrementY:
        dey
        cpy     #$FF
        bne     @sendByte
@ret:   rts


readStatusByte:
        inc     counter
        lda     FIFO_STATUS
        sta     readStatus
        rts

readFifoByte:
        inc     counter
        lda     FIFO_DATA
        sta     readFifo
        rts

countBytesInQueue:
        inc     counter
        lda     #$00
        sta     queueCount
        sta     queueCount+1
@checkStatus:
        lda     FIFO_STATUS
        cmp     #$41
        bne     @idle
        lda     FIFO_DATA
        inc     queueCount
        bne     @checkStatus
        inc     queueCount+1
        jmp     @checkStatus
@idle:
        rts



hiBytes:
        .byte   startingAddressHiHi,startingAddressLoHi,startingByteHi,countHi,repeatsHiHi,repeatsLoHi,repeatedByteHi
loBytes:
        .byte   startingAddressHiLo,startingAddressLoLo,startingByteLo,countLo,repeatsHiLo,repeatsLoLo,repeatedByteLo
targets:
        .byte   startingAddress+1,startingAddress,startingByte,count,repeats+1,repeats,repeatedByte


moveNybblesToBytes:
        ldy     #$00

@moveNybbles:
        ldx     hiBytes,y
        lda     tmp1,x
        asl
        asl
        asl
        asl

        ldx     loBytes,y
        ora     tmp1,x

        ldx     targets,y
        sta     tmp1,x

        iny
        cpy     #(loBytes-hiBytes)
        bne     @moveNybbles

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
        lda     renderMode
        jsr     renderBranchOnIndex
        lda     #$00
        sta     PPUSCROLL
        sta     PPUSCROLL
        lda     #%10001000
        sta     PPUCTRL
        lda     #%00001110
        sta     PPUMASK
        jsr     readjoy
        pla
        tay
        pla
        tax
        pla
irq:    rti

renderBranches:
        .addr   renderMainMenu
        .addr   renderRamTestScreen
        .addr   renderFifoScreen
        .addr   renderBlankScreen

renderBranchOnIndex:
        lda     renderMode
        asl
        tax
        lda     renderBranches,x
        sta     tmp1
        lda     renderBranches+1,x
        sta     tmp2
        jmp     (tmp1)


renderMainMenu:

; ram test
        lda     #$20
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringRamTest
        ldy     #>stringRamTest
        jsr     sendWordToPPU

; fifo test
        lda     #$21
        sta     PPUADDR
        lda     #$21
        sta     PPUADDR
        ldx     #<stringFifoTest
        ldy     #>stringFifoTest
        jsr     sendWordToPPU

; Counter
        lda     #$22
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringSpaceDollar
        ldy     #>stringSpaceDollar
        jsr     sendWordToPPU
        lda     counter
        jsr     twoDigitsToPPU

; plant cursor
        ldx     menuRow
        lda     cursorHiBytes,x
        sta     PPUADDR
        lda     cursorLoBytes,x
        sta     PPUADDR
        lda     #$F0
        sta     PPUDATA

        rts


renderRamTestScreen:
        lda     #$20
        sta     PPUADDR
        lda     #$83
        sta     PPUADDR
        ldx     #$C
        lda     #$FF
@blankCursor:
        sta     PPUDATA
        dex
        bpl     @blankCursor


        lda     #$20
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR

        ldx     #<stringSpaceDollar
        ldy     #>stringSpaceDollar
        jsr     sendWordToPPU
        lda     startingAddressHiHi
        sta     PPUDATA
        lda     startingAddressHiLo
        sta     PPUDATA
        lda     startingAddressLoHi
        sta     PPUDATA
        lda     startingAddressLoLo
        sta     PPUDATA

        ldx     #<stringSpaceDollar
        ldy     #>stringSpaceDollar
        jsr     sendWordToPPU

        lda     startingByteHi
        sta     PPUDATA
        lda     startingByteLo
        sta     PPUDATA

        ldx     #<stringSpaceDollar
        ldy     #>stringSpaceDollar
        jsr     sendWordToPPU

        lda     countHi
        sta     PPUDATA
        lda     countLo
        sta     PPUDATA


; write row
        lda     #$21
        sta     PPUADDR
        lda     #$21
        sta     PPUADDR
        ldx     #<stringWrite
        ldy     #>stringWrite
        jsr     sendWordToPPU


; read row
        lda     #$21
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringRead
        ldy     #>stringRead
        jsr     sendWordToPPU


; read results top row
        lda     #$22
        sta     PPUADDR
        lda     #$21
        sta     PPUADDR
        ldx     #0
writeTopRow:
        lda     readBuffer,x
        jsr     twoDigitsToPPU
        lda     #$FF
        sta     PPUDATA
        inx
        cpx     #$08
        bne     writeTopRow


; read results bottom row
        lda     #$22
        sta     PPUADDR
        lda     #$41
        sta     PPUADDR
        ldx     #0
writeBottomRow:
        lda     readBuffer+8,x
        jsr     twoDigitsToPPU
        lda     #$FF
        sta     PPUDATA
        inx
        cpx     #$08
        bne     writeBottomRow


; Counter
        lda     #$22
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringSpaceDollar
        ldy     #>stringSpaceDollar
        jsr     sendWordToPPU
        lda     counter
        jsr     twoDigitsToPPU


; plant cursor
        lda     menuColumn
        bne     @checkTop
        ldx     menuRow
        lda     cursorHiBytes,x
        sta     PPUADDR
        lda     cursorLoBytes,x
        sta     PPUADDR
        lda     #$F0
        sta     PPUDATA


@checkTop:
        lda     menuColumn
        beq     @noTopCursor

; top cursor
        lda     #$20
        sta     PPUADDR
        ldx     menuColumn
        lda     ramtestOffsets-1,x
        clc
        adc     #$82
        clc
        adc     menuColumn
        sta     PPUADDR
        lda     #$F1
        sta     PPUDATA
@noTopCursor:
        rts

renderFifoScreen:
        lda     #$20
        sta     PPUADDR
        lda     #$88
        sta     PPUADDR
        ldx     #$F
        lda     #$FF
blankCursor:
        sta     PPUDATA
        dex
        bpl     blankCursor

        lda     #$20
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR

        ldx     #<stringSend
        ldy     #>stringSend
        jsr     sendWordToPPU
        lda     repeatedByteHi
        sta     PPUDATA
        lda     repeatedByteLo
        sta     PPUDATA

        ldx     #<stringCount
        ldy     #>stringCount
        jsr     sendWordToPPU

        lda     repeatsHiHi
        sta     PPUDATA
        lda     repeatsHiLo
        sta     PPUDATA
        lda     repeatsLoHi
        sta     PPUDATA
        lda     repeatsLoLo
        sta     PPUDATA

; status row
        lda     #$21
        sta     PPUADDR
        lda     #$21
        sta     PPUADDR
        ldx     #<stringReadStatus
        ldy     #>stringReadStatus
        jsr     sendWordToPPU
        lda     readStatus
        jsr     twoDigitsToPPU

; fifo row
        lda     #$21
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringReadFifo
        ldy     #>stringReadFifo
        jsr     sendWordToPPU
        lda     readFifo
        jsr     twoDigitsToPPU

; count row
        lda     #$22
        sta     PPUADDR
        lda     #$21
        sta     PPUADDR
        ldx     #<stringCountQueue
        ldy     #>stringCountQueue
        jsr     sendWordToPPU
        lda     queueCount+1
        jsr     twoDigitsToPPU
        lda     queueCount
        jsr     twoDigitsToPPU

; counter row
        lda     #$22
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<stringCounter
        ldy     #>stringCounter
        jsr     sendWordToPPU
        lda     counter
        jsr     twoDigitsToPPU

; plant cursor
        ldx     menuRow
        lda     cursorHiBytes,x
        sta     PPUADDR
        lda     cursorLoBytes,x
        sta     PPUADDR
        lda     #$F0
        sta     PPUDATA

        lda     menuColumn
        beq     @noTopCursor

; top cursor
        lda     #$20
        sta     PPUADDR
        ldx     menuColumn
        lda     fifoOffsets-1,x
        clc
        adc     #$87
        clc
        adc     menuColumn
        sta     PPUADDR
        lda     #$F1
        sta     PPUDATA
@noTopCursor:
        rts

renderBlankScreen:
        lda     #$00
        sta     PPUCTRL
        sta     PPUMASK
        lda     activeMenu
        sta     renderMode
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

stringRamTest:
        .byte   " RAM Read/Write",$00
stringFifoTest:
        .byte   " Everdrive FIFO Test",$00
stringSpaceDollar:
        .byte   " $",$00
stringWrite:
        .byte   " Write",$00
stringRead:
        .byte   " Read",$00
stringSend:
        .byte   " Send $",$00
stringCount:
        .byte   " count $",$00
stringReadStatus:
        .byte   " Read $40f1=$",$00
stringReadFifo:
        .byte   " Read $40f0=$",$00
stringCountQueue:
        .byte   " len(queue)=$",$00
stringCounter:
        .byte   " $",$00



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

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code
