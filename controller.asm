readWriteControllers:
; pulse 4016.0 to help sync
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

; restore 4016.0
        lda #$00
        sta JOYPAD1

        jsr readP1
@ret:
        rts


sendReceiveP2Port:
        sta outByte
        lda #1
        sta inByte
@loop:
        asl outByte           ; 5 next bit in carry
        rol JOYPAD1           ; 6 carry stored in 4016.0
        lda JOY2_APUFC        ; 4 read bit 4017.0 (D0)
        lsr                   ; 2
        rol inByte            ; 5 save bit
        jsr @ret              ; 12 give arduino time to finish isr
        jsr @ret              ; 12 give arduino time to finish isr
        bcc @loop             ; mostly 3
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
