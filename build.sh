#!/bin/bash

compile_flags=()
set -x
set -e

help () {
    echo "Usage: $0 [-f] [-v] [-h]"
    echo "-f  flag"
    echo "-v  verbose"
    echo "-h  you are here"
}

while getopts "f:vh" flag; do
  case "${flag}" in
    v) echo "This option doesn't do anything" ;;
    f)
        compile_flags+=("-D ${OPTARG}")
        echo "${OPTARG} enabled"  ;;
    h)
        help; exit ;;
    *)
        help; exit 1 ;;
  esac
done

python3 tools/nes-util/nes_chr_encode.py tileset.png tileset.chr

ca65 ${compile_flags[*]} -g header.asm -o header.o
ca65 ${compile_flags[*]} -l hello.lst -g main.asm -o main.o

# link object files

ld65 -m hello.map -Ln hello.lbl --dbgfile hello.dbg -o hello.nes -C hello.cfg main.o header.o

head -c $(wc -c hello.nes | cut -d ' ' -f 1) </dev/zero >zeroes.nes
flips --create -i zeroes.nes hello.nes fifo_testrom.ips
python convert_patch.py > fifo_testrom.py
cat fifo_testrom.py
rm fifo_testrom.ips
rm zeroes.nes


