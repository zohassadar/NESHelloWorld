.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0007
heldButtons: .res 1             ; $0008
.res    $F9
.bss
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
