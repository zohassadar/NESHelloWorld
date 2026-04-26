syncData:
        ; source: ','.join(random.choice("01") for _ in range(32))
        .byte 0,0,0,0,1,0,1,1,1,1,0,0,1,1,1,0,0,0,1,1,0,0,1,1,0,0,0,1,0,1,0,1
syncDataEnd:
syncDataLength = syncDataEnd - syncData

sendPreamble:
        ldx #0
@sendBit:
        lda syncData,x
        sta JOYPAD1
        lda JOY2_APUFC
        ; time is needed for the arduino interrupt routine to finish
        jsr @ret
        jsr @ret
        jsr @ret
        inx
        cpx #syncDataLength
        bne @sendBit
@ret:
        rts



readWriteControllers:
        ldx #1
        stx JOYPAD1
        dex
        stx JOYPAD1
        jsr sendPreamble
        lda frameCounter
        jsr transmitByte
        ; all bits should be transmited, arduino is waiting on last pulse
        ; restore 4016 to normal unused state
        lda #$00
        sta JOYPAD1
        ; signal ready to read controllers
        lda JOY2_APUFC
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @ret
        jsr @read
        rts
@ret:
        rts


@read:
@button := tmp1
        lda sleepCounter
        bne @skipP2
        ldx #1
        jsr @readController
        sta buttonBuffer
        cmp #$EF
        beq @skipP2
        lda #40
        sta sleepCounter
@skipP2:
        ldx #$0
        jsr @readController
        sta buttonBuffer
        ldx #1
        stx JOYPAD1
        dex
        stx JOYPAD1

;         inx
;         jsr @readController
;         cmp #$EF ; wait until arduino is present
;         bne @setNew
; @keepReading:
;         sta buttonBuffer+0
;         jsr @readController
        ; sta buttonBuffer+1
        ; jsr @readController
        ; sta buttonBuffer+2
        ; jsr @readController
        ; sta buttonBuffer+3

@setNew:
; only matters for p1 input
        lda newButtons
        pha
        eor heldButtons
        and newButtons
        sta newButtons
        pla
        sta heldButtons
@wait:
        rts

; https://www.nesdev.org/wiki/Controller_reading_code
@readController:
        lda #$01
        sta JOYPAD1
        sta @button
        lsr
        sta JOYPAD1
@loop:
        lda JOYPAD1,x
        lsr
        rol @button
        bcc @loop
        lda @button
        rts



transmitByte:
        sta     generalCounter
        ldy #8
@nextBit:
        ror     generalCounter
        rol     JOYPAD1 ; bit 0 is held on OUT/LATCH (Pin 9)
        lda     JOY2_APUFC ; causes CLOCK to pulse low
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        dey
        bne @nextBit
        lda #0
        sta JOYPAD1
        rts
