.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0005
heldButtons: .res 1             ; $0006

pulledNumber: .res 1
pulledDigits: .res 3
.res 4

nmiHappened: .res 1
renderMode: .res 1 

found: .res 1

offset: .res 2

total: .res 2

total2: .res 3

totalFrames: .res 2

tmpX: .res 1
tmpY: .res 1
tmpZ: .res 1


maxRed: .res 1
maxGreen: .res 1
maxBlue: .res 1


; The number to be multiplied is the “multiplicand”, 
; and the number by which it is multiplied is the “multiplier”.

multiplier: .res 1
multiplicand: .res 2

product: .res 2


currentGameId: .res 1

errorFlag: .res 1

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
