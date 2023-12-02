.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0005
heldButtons: .res 1             ; $0006

nmiHappened: .res 1
renderMode: .res 1 

offset: .res 2

total: .res 2

firstDigit: .res 1
currentDigit: .res 1

found: .res 1

tmpX: .res 1
tmpY: .res 1
tmpZ: .res 1







.res    $CA

.bss
stack:
    .res    $100

unused:
    .res    $100
    .res    $100
    .res    $100
    .res    $100
    .res    $100
    .res    $100
