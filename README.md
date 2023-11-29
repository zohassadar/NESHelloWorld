# Hello

This is my "hello world" NES rom for debugging and testing purposes. 

## Install

You'll need

* cc65
* python >= 3.10
* bash
* flips

`bash build.sh`

`hello.nes` is the rom

`fifo_testrom.py` is a b85 encoded bps patch against a file of all zeroes the same length as the rom.   Used for [python-edlinkn8](https://github.com/zohassadar/python-edlinkn8)

## Menus

Main Menu:

```
>RAM Read/Write

 Everdrive FIFO Test





 $00

```

The counter displayed at the bottom is there to provide feedback that an action occurred.


RAM Test Menu:

```
>$0000 $00 $00

 Write

 Read

00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00

 $00

```

Inputs (top row) are `startingAddress`, `startingByte` & `count`.

If `count` is between `$01` and `$FF`, selecting `Write` will write to a stretch of ram at `startingAddress` of length `count`. The first byte will be `startingByte` and every subsequent byte will be `startingByte` incremented.

Selecting `Read` will read 16 bytes at `startingAddress`


Everdrive FIFO Queue Test Menu:

```
>Send $00 count $0000

 Read $40f1=$00

 Read $40f0=$00

 len(queue)=$0000

 $00

```

Inputs are `byteToSend` and `count`.  If `count` is greater than zero, selecting `Send` will send `byteToSend` to the queue until `count` is reached.

The two `Read` options will read and display `FIFO_STATUS` (`$40f1`) or `FIFO_DATA` (`$40f0`)

Selecting `len(queue)` will read `FIFO_STATUS`, if the value read is `FIFO_PENDING` (`$41`), then `FIFO_DATA` will be read and a counter will be incremented.  This process repeats until `FIFO_STATUS` no longer reads `FIFO_PENDING`.  All bytes read from `FIFO_DATA` are discarded.



## To Do

* Fix the graphic glitch on startup
* Use nametable to look more interesting
* Sprites for cursors
* Render queue
* Beeps

