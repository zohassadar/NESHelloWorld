; These are handy


.macro subtract length, this, that, store
; subtracts this from that and optionally stores
        sec
.repeat length,pointer
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
.repeat length,pointer
        lda that+pointer
        adc this+pointer
.ifnblank store
        sta store+pointer
.endif
.endrepeat
.endmacro


.macro copy length, here, there
; copies from here to there
.repeat length,pointer
        lda here+pointer
        sta there+pointer
.endrepeat
.endmacro
