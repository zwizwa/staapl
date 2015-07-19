OBJ = kernel.o infof.o hw_linux.o
.PHONY: all
all: kernel.elf

kernel.o: kernel.c infof.c infof.h

%.c:
	ln -s ../qemu/$@ .
%.h:
	ln -s ../qemu/$@ .

PREFIX:=arm-linux-gnu
GCC:=$(PREFIX)-gcc
LD:=$(PREFIX)-ld
OBJDUMP:=$(PREFIX)-objdump

LIBS:=


# Libc is not in thumb mode.
# CFLAGS := -g -Os -mcpu=cortex-m3 -mthumb -mfloat-abi=hard
CFLAGS := -g -Os -mfloat-abi=hard

kernel.elf: $(OBJ)
	$(GCC) -static -o $@ $(OBJ) $(LIBS)

#	$(OBJDUMP) -d $@

%.o: %.c
	$(GCC) $(CFLAGS)  -o $@ -c $$(readlink -f $<)

clean:
	rm -f core *~ *.elf *.o

