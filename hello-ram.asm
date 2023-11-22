.zeropage
tmp1:   .res    1               ; $0000
tmp2:   .res    1               ; $0001
frameCounter: .res 1            ; $0002
renderedRow: .res 1             ; $0003
sendMessageFlag: .res 1         ; $0004
newButtons: .res 1              ; $0007
heldButtons: .res 1             ; $0008

nmiHappened: .res 1

menuRow: .res 1
menuColumn: .res 1

readStatus: .res 1
readFifo: .res 1

repeatedByte: .res 1
repeats: .res 2

repeatedByteHi: .res 1
repeatedByteLo: .res 1

; should be called count..
repeatsHiHi: .res 1
repeatsHiLo: .res 1
repeatsLoHi: .res 1
repeatsLoLo: .res 1


queueCount: .res 2

counter: .res 1


.res    $E8
.bss
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
