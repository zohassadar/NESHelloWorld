
.include "hello-ram.asm"

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

BUTTON_DOWN :=  $4
BUTTON_UP :=    $8
BUTTON_RIGHT := $1
BUTTON_LEFT :=  $2
BUTTON_B :=     $40
BUTTON_A :=     $80
BUTTON_SELECT := $20
BUTTON_START := $10


FIFO_DATA :=    $40f0
FIFO_STATUS :=  $40f1

.include "charmap.asm"

sendRepeatedByte:
        lda     repeats
        beq     @ret
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
        lda     #$00
        sta     FIFO_DATA
        ldx     repeats
        lda     repeatedByte
@sendBytes:
        sta     FIFO_DATA
        dex
        bne     @sendBytes
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


sendMessage:
        lda     #$2b            ; "+"
        sta     FIFO_DATA
        eor     #$ff
        sta     FIFO_DATA
        lda     #$22            ; CMD_USB_WR
        sta     FIFO_DATA
        eor     #$ff
        sta     FIFO_DATA
        lda     #$05            ; Length.  16 bit LE
        sta     FIFO_DATA
        lda     #$00
        sta     FIFO_DATA
        lda     frameCounter
        sta     FIFO_DATA
        lda     frameCounter
        sta     FIFO_DATA
        lda     frameCounter
        sta     FIFO_DATA
        lda     frameCounter
        sta     FIFO_DATA
        lda     frameCounter
        sta     FIFO_DATA
        lda     #$00
        sta     sendMessageFlag
        rts


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


nmi:    pha
        txa
        pha
        tya
        pha
        inc     frameCounter
        lda     #$01
        sta     nmiHappened

        lda     #$20
        sta     PPUADDR
        lda     #$88
        sta     PPUADDR
        ldx     #$D
        lda     #$FF
blankCursor:
        sta     PPUDATA
        dex
        bpl     blankCursor


        lda     #$20
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR

        ldx     #<wordByte
        ldy     #>wordByte
        jsr     sendWordToPPU
        lda     repeatedByteHi
        sta     PPUDATA
        lda     repeatedByteLo
        sta     PPUDATA

        ldx     #<wordRepeats
        ldy     #>wordRepeats
        jsr     sendWordToPPU

        lda     repeatsHi
        sta     PPUDATA
        lda     repeatsLo
        sta     PPUDATA

; status row
        lda     #$21
        sta     PPUADDR
        lda     #$21
        sta     PPUADDR
        ldx     #<wordReadStatus
        ldy     #>wordReadStatus
        jsr     sendWordToPPU
        lda     readStatus
        jsr     twoDigitsToPPU

; fifo row
        lda     #$21
        sta     PPUADDR
        lda     #$A1
        sta     PPUADDR
        ldx     #<wordReadFifo
        ldy     #>wordReadFifo
        jsr     sendWordToPPU
        lda     readFifo
        jsr     twoDigitsToPPU

; count row
        lda     #$22
        sta     PPUADDR
        lda     #$21
        sta     PPUADDR
        ldx     #<wordCountQueue
        ldy     #>wordCountQueue
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
        ldx     #<wordCounter
        ldy     #>wordCounter
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
        lda     topCursorOffsets-1,x
        clc
        adc     #$87
        clc
        adc     menuColumn
        sta     PPUADDR
        lda     #$F1
        sta     PPUDATA
@noTopCursor:
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


cursorHiBytes:
        .byte   $20,$21,$21,$22
cursorLoBytes:
        .byte   $A1,$21,$A1,$21

topCursorOffsets:
        .byte   $00,$00,$08,$08

wordByte:
        .byte   " Send $",$FF
wordRepeats:
        .byte   " count $",$FF
wordReadStatus:
        .byte   " Read $40f1=$",$FF
wordReadFifo:
        .byte   " Read $40f0=$",$FF
wordCountQueue:
        .byte   " len(queue)=$",$FF
wordCounter:
        .byte   " $",$FF


sendWordToPPU:
        stx     tmp1
        sty     tmp2
        ldy     #$00
wordLoop:
        lda     (tmp1),y
        cmp     #$FF
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



loop:
        lda     menuRow
        bne     @notTopRow


        lda     newButtons
        and     #BUTTON_LEFT
        beq     @leftNotPressed
        dec     menuColumn
        bpl     @leftNotPressed
        lda     #$04
        sta     menuColumn
@leftNotPressed:

        lda     newButtons
        and     #BUTTON_RIGHT
        beq     @rightNotPressed
        inc     menuColumn
        lda     menuColumn
        cmp     #$05
        bne     @rightNotPressed
        lda     #$00
        sta     menuColumn
@rightNotPressed:
        lda     menuColumn
        beq     @notTopRow

        ldx     menuColumn
        lda     newButtons
        and     #BUTTON_UP
        beq     @upNotPressedForDigit
        inc     repeatedByteHi-1,x
        lda     repeatedByteHi-1,x
        and     #$0F
        sta     repeatedByteHi-1,x
@upNotPressedForDigit:
        lda     newButtons
        and     #BUTTON_DOWN
        beq     @downNotPressedForDigit
        dec     repeatedByteHi-1,x
        lda     repeatedByteHi-1,x
        and     #$0F
        sta     repeatedByteHi-1,x
@downNotPressedForDigit:

        jmp     @skipUpDownRead
@notTopRow:
        lda     newButtons
        and     #BUTTON_UP
        beq     @upNotPressed
        dec     menuRow
        bpl     @upNotPressed
        lda     #$03
        sta     menuRow
@upNotPressed:
        lda     newButtons
        and     #BUTTON_DOWN
        beq     @downNotPressed
        inc     menuRow
        lda     menuRow
        cmp     #$04
        bne     @downNotPressed
        lda     #$00
        sta     menuRow
@downNotPressed:
@skipUpDownRead:

; move repeats
        lda     repeatsHi
        asl
        asl
        asl
        asl
        ora     repeatsLo
        sta     repeats

; move repeated byte
        lda     repeatedByteHi
        asl
        asl
        asl
        asl
        ora     repeatedByteLo
        sta     repeatedByte

        lda     newButtons
        and     #BUTTON_A
        beq     @aNotPressed
        lda     menuRow
        jsr     branchOnIndex
@aNotPressed:
        lda     #$00
        sta     nmiHappened
@waitForNmi:
        lda     nmiHappened
        beq     @waitForNmi
        jmp     loop

branches:
        .addr   sendRepeatedByte
        .addr   readStatusByte
        .addr   readFifoByte
        .addr   countBytesInQueue

branchOnIndex:
        asl
        tax
        lda     branches,x
        sta     tmp1
        lda     branches+1,x
        sta     tmp2
        jmp     (tmp1)

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
@vblankwait1:
        bit     PPUSTATUS
        bpl     @vblankwait1

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

@vblankwait2:
        bit     PPUSTATUS
        bpl     @vblankwait2


        lda     #$20
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR

        ldx     #$00
        lda     #$FF
nametableGroup1Loop:
        sta     PPUDATA
        inx
        bne     nametableGroup1Loop


        lda     #$21
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR

        ldx     #$00
        lda     #$FF
nametableGroup2Loop:
        sta     PPUDATA
        inx
        bne     nametableGroup2Loop


        lda     #$22
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR

        ldx     #$00
        lda     #$FF
nametableGroup3Loop:
        sta     PPUDATA
        inx
        bne     nametableGroup3Loop

        lda     #$23
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR

        ldx     #$C0
        lda     #$FF
nametableGroup4Loop:
        sta     PPUDATA
        dex
        bne     nametableGroup4Loop

        lda     #$3F
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR
        ldx     #$00

paletteLoop:
        lda     palette,x
        cmp     #$FF
        beq     endPaletteLoop
        sta     PPUDATA
        inx
        jmp     paletteLoop
endPaletteLoop:

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
        .byte   $FF

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code
