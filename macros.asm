; These are handy

.macro sub length, this, that, store
; subtracts this from that and optionally stores
        sec
.repeat length, pointer
        lda that+pointer
        sbc this+pointer
.ifnblank store
        sta store+pointer
.endif
.endrepeat
.endmacro


.macro add length, this, that, store
; add this to that and optionally stores
        clc
.repeat length, pointer
        lda that+pointer
        adc this+pointer
.ifnblank store
        sta store+pointer
.endif
.endrepeat
.endmacro


.macro copy length, here, there
; copies from here to there
.repeat length, pointer
        lda here+pointer
        sta there+pointer
.endrepeat
.endmacro

.macro isZero length, address
; zero reflects length bytes
.repeat length, pointer
        lda address+pointer
        bne :+
.endrepeat
:
.endmacro

.macro push length, address
; push length bytes to stack, smallest value first
.repeat length, pointer
        lda address+pointer
        pha
.endrepeat
.endmacro

.macro pull length, address
; pull length bytes from stack, biggest value first
.repeat length, pointer
        pla
        sta address+(length-(pointer+1))
.endrepeat
.endmacro