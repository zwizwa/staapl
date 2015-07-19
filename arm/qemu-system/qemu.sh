#!/bin/bash
HERE=$(dirname $0)
ELF=kernel.elf
PORT=3333

# http://zwizwa.be/-/staapl/20140801-214528
# https://github.com/hackndev/qemu/blob/master/hw/versatilepb.c

GDB="gdb --args"
GDB=

# cpulimit -l 10 -- \

exec \
$GDB qemu-system-arm \
    -M versatilepb \
    -cpu cortex-m3 \
    -nographic \
    -monitor null \
    -serial pty \
    -serial stdio \
    -semihosting \
    -kernel $ELF \
    -gdb tcp::$PORT \
    -m 1 \
    "$@"



