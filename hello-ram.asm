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

activeMenu: .res 1
menuRow: .res 1
menuColumn: .res 1

counter: .res 1

; ramtest bytes
startingAddress: .res 2
startingByte: .res 1
count: .res 1

; fifo bytes
readStatus: .res 1
readFifo: .res 1
repeatedByte: .res 1
repeats: .res 2
queueCount: .res 2

; ramtest inputs
inputOffset:

ramtestInputStart:
startingAddressHiHi: .res 1
startingAddressHiLo: .res 1
startingAddressLoHi: .res 1
startingAddressLoLo: .res 1
startingByteHi: .res 1
startingByteLo: .res 1
countHi: .res 1
countLo: .res 1
ramtestInputEnd:

; fifo inputs
repeatedByteHi: .res 1
repeatedByteLo: .res 1
; should be called count..
repeatsHiHi: .res 1
repeatsHiLo: .res 1
repeatsLoHi: .res 1
repeatsLoLo: .res 1


readBuffer: .res $10

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
