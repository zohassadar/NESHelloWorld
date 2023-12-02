#!/bin/bash

compile_flags=()
# set -x
set -e

python3 tools/nes-util/nes_chr_encode.py tileset.png tileset.chr

ca65 ${compile_flags[*]} -g header.asm -o header.o

find "./aoc2023" -name "*.asm" | while read asm; do
    echo ${asm}
    ca65 ${compile_flags[*]} -l ${asm%.*}.lst -g $asm -o ${asm%.*}.o
    ld65 -m ${asm%.*}.map -Ln ${asm%.*}.lbl --dbgfile ${asm%.*}.dbg -o ${asm%.*}.nes -C hello.cfg ${asm%.*}.o header.o
done




