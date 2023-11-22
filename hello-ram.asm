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
repeats: .res 1

repeatedByteHi: .res 1
repeatedByteLo: .res 1
repeatsHi: .res 1
repeatsLo: .res 1

queueCount: .res 2

counter: .res 1


.res    $EB
.bss
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
.res    $100
