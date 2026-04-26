readWriteControllers:
        ldx #1
        stx JOYPAD1
        dex
        stx JOYPAD1
        lda #%01011010
        jsr sendReceiveP2Port
        sta arduinoId+0

        lda #%11000011
        jsr sendReceiveP2Port
        sta arduinoId+1

        lda #$00
        sta JOYPAD1
        jsr readP1
@ret:
        rts


sendReceiveP2Port:
        sta outByte
        lda #0
        sta inByte
        ldy #8
@loop:
        asl outByte           ; next bit in carry
        rol JOYPAD1           ; carry stored in 4016.0
        lda JOY2_APUFC        ; read bit 4017.0 (D0)
                              ; also pulses clock (triggers arduino interrupt)
        lsr
        ror inByte            ; save bit

        jsr @ret              ; give arduino time to finish isr
        jsr @ret              ; give arduino time to finish isr

        dey
        bne @loop

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
