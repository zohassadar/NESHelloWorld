;
; iNES header
;

; This iNES header is from Brad Smith (rainwarrior)
; https://github.com/bbbradsmith/NES-ca65-example

.segment "HEADER"

INES_MAPPER = 0 ; NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring (ignored in MMC1)
INES_SRAM   = 1 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding


.segment "CHR"
.incbin "tileset.chr"
.incbin "tileset.chr"
