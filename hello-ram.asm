.zeropage
tmp1: .res 1 ; $0000
tmp2: .res 1 ; $0001
frameCounter: .res 1 ; $0002
renderedRow: .res 1 ; $0003
messageSent: .res 1 ; $0004
dataContent: .res 1 ; $0005
statusContent: .res 1 ; $0006
newButtons: .res 1 ; $0007
heldButtons: .res 1 ; $0008
.res $F7
.bss
.res $100
.res $100
.res $100
.res $100
.res $100
.res $100
.res $100