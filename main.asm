
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

FIFO_DATA :=    $40f0
FIFO_STATUS :=  $40f1

sendMessage:
        lda     #$2b        ; "+"
        sta     FIFO_DATA
        eor     #$ff
        sta     FIFO_DATA
        lda     #$22        ; CMD_USB_WR
        sta     FIFO_DATA
        eor     #$ff
        sta     FIFO_DATA
        lda     #$05        ; Length.  16 bit LE
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



receiveMessage:
        lda     FIFO_STATUS
        cmp     #$40 ; Mesen/FCEUX unknown register
        beq     @ret
        cmp     #$C1 ; No message waiting
        beq     @ret
        lda     FIFO_DATA
        sta     sendMessageFlag
        jmp     receiveMessage
@ret:
        rts



renderRows:
        ; .word $2021,
        .word   $2041,$2061,$2081,$20a1
        .word   $20c1,$20e1,$2101,$2121,$2141
        .word   $2161,$2181,$21a1,$21c1,$21e1
        .word   $2201,$2221,$2241,$2261,$2281
        .word   $22a1,$22c1,$22e1,$2301,$2321
        .word   $2341,$2361     ;,$2381


setRenderedRow:
        inc     renderedRow
        lda     renderedRow
        cmp     #$1a
        bne     @ret
        lda     #$00
        sta     renderedRow
@ret:
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

nmi:    pha
        txa
        pha
        tya
        pha
        inc     frameCounter
        jsr     receiveMessage

        jsr     setRenderedRow
        lda     renderedRow
        asl
        tax
        lda     renderRows+1,x
        sta     PPUADDR
        lda     renderRows,x
        sta     PPUADDR
        lda     frameCounter
        jsr     twoDigitsToPPU
        lda     #$FF
        sta     PPUDATA
        ; lda     $40F0
        ; jsr     twoDigitsToPPU
        ; lda     #$FF
        ; sta     PPUDATA
        ; lda     $40F1
        ; jsr     twoDigitsToPPU
        ; lda     #$FF
        ; sta     PPUDATA
        lda     newButtons
        jsr     twoDigitsToPPU
        lda     #$FF
        sta     PPUDATA
        lda     heldButtons
        jsr     twoDigitsToPPU
        lda     #$FF
        sta     PPUDATA
        lda     sendMessageFlag
        jsr     twoDigitsToPPU
        lda     #$00
        sta     PPUSCROLL
        sta     PPUSCROLL
        lda     #%10001000
        sta     PPUCTRL
        lda     #%00001110
        sta     PPUMASK
        jsr     readjoy
        lda     sendMessageFlag
        beq     @nonePressed
        jsr     sendMessage
@nonePressed:
        pla
        tay
        pla
        tax
        pla
irq:    rti

loop:
        nop
        jmp     loop

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
        .byte   $0F,$30,$16,$12
        .byte   $0F,$30,$16,$12
        .byte   $0F,$30,$16,$12
        .byte   $0F,$30,$16,$12
        .byte   $0F,$30,$16,$12
        .byte   $0F,$30,$16,$12
        .byte   $0F,$30,$16,$12
        .byte   $0F,$30,$16,$12
        .byte   $FF

.segment "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code
