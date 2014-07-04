all: test.elf

clean:
	rm -rf *~ *.o *.elf

CC = arm-eabi-gcc  -mcpu=arm7tdmi -O0 -g
LD = arm-eabi-gcc  -mno-thumb-interwork -mcpu=arm7tdmi -Wl,-static -g -nostdlib -Ttarget.ld
OBJDUMP = arm-eabi-objdump

%.o:	%.S
	$(CC) -o $@ -c $<

%.elf:	%.o
	$(LD) -o $@ $<

%.dump: %.elf
	$(OBJDUMP) -h $< >$@
	cat $@

