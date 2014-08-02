#!/bin/bash
HERE=$(dirname $0)
ELF=kernel.elf
# PORT=1234
PORT=3333

# HACK: qemu doesn't (yet) supprt cortex-m4, so we run the code in a
# cortex-a8 which supports thumb2 DSP instruction set.
GDB="gdb --args"
GDB=
exec $GDB qemu-system-arm \
    -M versatilepb \
    -cpu cortex-m3 \
    -nographic \
    -monitor null \
    -serial stdio \
    -semihosting \
    -kernel $ELF \
    -gdb tcp::$PORT \
    -m 1 \
    "$@"


