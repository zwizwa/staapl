#!/bin/bash
LINK=/tmp/kernel
echo "LINK=$LINK"
exec socat PTY,link=$LINK EXEC:./kernel.elf
