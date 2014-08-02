#!/bin/bash
HERE=$(dirname $0)
ELF=kernel.elf
PORT=3333

# The versatilepb system is arm926.  Why does -cpy cortex-m3 work as well?
# http://zwizwa.be/-/staapl/20140801-214528
# https://github.com/hackndev/qemu/blob/master/hw/versatilepb.c

GDB="gdb --args"
GDB=
exec $GDB qemu-system-arm \
    -M versatilepb \
    -cpu cortex-m3 \
    -nographic \
    -monitor null \
    -serial pty \
    -semihosting \
    -kernel $ELF \
    -gdb tcp::$PORT \
    -m 1 \
    "$@"



