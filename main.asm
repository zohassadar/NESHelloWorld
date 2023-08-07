
.include "hello-ram.asm"

.segment "PRG"

PPUCTRL         := $2000
PPUMASK         := $2001
PPUSTATUS       := $2002
OAMADDR         := $2003
OAMDATA         := $2004
PPUSCROLL       := $2005
PPUADDR         := $2006
PPUDATA         := $2007

DMC_FREQ        := $4010
JOY2_APUFC      := $4017 


renderRows:
    ; .word $2021,
    .word $2041,$2061,$2081,$20a1
    .word $20c1,$20e1,$2101,$2121,$2141
    .word $2161,$2181,$21a1,$21c1,$21e1
    .word $2201,$2221,$2241,$2261,$2281
    .word $22a1,$22c1,$22e1,$2301,$2321
    .word $2341,$2361 ;,$2381


setRenderedRow:
    inc renderedRow
    lda renderedRow
    cmp #$1a
    bne @ret
    lda #$00
    sta renderedRow
@ret:
    rts

twoDigitsToPPU:
    pha
    lsr
    lsr
    lsr
    lsr
    sta PPUDATA
    pla
    and #$0F
    sta PPUDATA
    rts

nmi:    pha
        txa
        pha
        tya
        pha
        inc frameCounter

        jsr setRenderedRow
        lda renderedRow
        asl
        tax
        lda renderRows+1,x
        sta PPUADDR
        lda renderRows,x
        sta PPUADDR
        lda frameCounter
        jsr twoDigitsToPPU
        lda #$FF
        sta PPUDATA
        lda $40F0
        jsr twoDigitsToPPU
        lda #$FF
        sta PPUDATA
        lda $40F1
        jsr twoDigitsToPPU
        lda #$00
        sta PPUSCROLL
        sta PPUSCROLL
    lda #%10001000
    sta PPUCTRL
    lda #%00001110
    sta PPUMASK
        pla
        tay
        pla
        tax
        pla
irq:    rti

loop:
    nop
    jmp loop

reset:
    ; from https://www.nesdev.org/wiki/Init_code
    sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx JOY2_APUFC  ; disable APU frame IRQ
    ldx #$ff
    txs        ; Set up stack
    inx        ; now X = 0
    stx PPUCTRL ; disable NMI
    stx PPUMASK  ; disable rendering
    stx DMC_FREQ  ; disable DMC IRQs

    ; Optional (omitted):
    ; Set up mapper and jmp to further init code here.

    ; The vblank flag is in an unknown state after reset,
    ; so it is cleared here to make sure that @vblankwait1
    ; does not exit immediately.
    bit PPUSTATUS

    ; First of two waits for vertical blank to make sure that the
    ; PPU has stabilized
@vblankwait1:  
    bit PPUSTATUS
    bpl @vblankwait1

    ; We now have about 30,000 cycles to burn before the PPU stabilizes.
    ; One thing we can do with this time is put RAM in a known state.
    ; Here we fill it with $00, which matches what (say) a C compiler
    ; expects for BSS.  Conveniently, X is still 0.
    txa
@clrmem:
    sta $0000,x
    sta $0100,x
    sta $0200,x
    sta $0300,x
    sta $0400,x
    sta $0500,x
    sta $0600,x
    sta $0700,x
    inx
    bne @clrmem

    ; Other things you can do between vblank waits are set up audio
    ; or set up other mapper registers.
   
@vblankwait2:
    bit PPUSTATUS
    bpl @vblankwait2


    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ldx #$00
    lda #$FF
nametableGroup1Loop:
    sta PPUDATA
    inx
    bne nametableGroup1Loop


    lda #$21
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ldx #$00
    lda #$FF
nametableGroup2Loop:
    sta PPUDATA
    inx
    bne nametableGroup2Loop


    lda #$22
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ldx #$00
    lda #$FF
nametableGroup3Loop:
    sta PPUDATA
    inx
    bne nametableGroup3Loop



    lda #$23
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ldx #$C0
    lda #$FF
nametableGroup4Loop:
    sta PPUDATA
    dex
    bne nametableGroup4Loop





    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    ldx #$00

paletteLoop:
    lda palette,x
    cmp #$FF
    beq endPaletteLoop
    sta PPUDATA
    inx
    jmp paletteLoop
endPaletteLoop:


    lda #%10001000
    sta PPUCTRL
    lda #%00011110
    sta PPUMASK
    jmp loop



palette:
    .byte $0F,$30,$16,$12
    .byte $0F,$30,$16,$12
    .byte $0F,$30,$16,$12
    .byte $0F,$30,$16,$12
    .byte $0F,$30,$16,$12
    .byte $0F,$30,$16,$12
    .byte $0F,$30,$16,$12
    .byte $0F,$30,$16,$12
    .byte $FF

.segment    "VECTORS": absolute

        .addr   nmi
        .addr   reset
        .addr   irq

.code