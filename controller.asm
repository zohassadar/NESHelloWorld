readWriteControllers:
        jsr readP1

        lda #$F0
        jsr sendReceiveP2Port
        sta arduinoId+0

        lda JOY2_APUFC

        ; lda #$AA
        ; jsr sendReceiveP2Port
        ; sta arduinoId+1
        ;
        ; lda #$AA
        ; jsr sendReceiveP2Port
        ; sta arduinoId+2
        ;
        ; lda #$AA
        ; jsr sendReceiveP2Port
        ; sta arduinoId+3
        ;
        rts


sendReceiveP2Port:
        sta outByte
        ldy #8
@loop:
        asl outByte           ; next bit in carry
        rol JOYPAD1           ; carry stored in 4016.0
        nop
        nop
        nop
        lda JOY2_APUFC        ; read bit 4017.0 (D0)
                              ; also pulses clock (triggers arduino interrupt)
        lsr
        rol inByte            ; save bit

        .repeat 8             ; 12 cycles per, tune down later
        jsr @ret              ; give arduino time to finish isr
        .endrepeat

        dey
        bne @loop

        ; restore 4016.0
        lda #$0
        sta JOYPAD1

        lda inByte
@ret:
        rts


; https://www.nesdev.org/wiki/Controller_reading_code
readP1:
        lda #$01
        sta JOYPAD1
        sta newButtons
        lsr
        sta JOYPAD1
@loop:
        lda JOYPAD1
        lsr
        rol newButtons
        bcc @loop
        lda newButtons
        pha
        eor heldButtons
        and newButtons
        sta newButtons
        pla
        sta heldButtons
        rts
