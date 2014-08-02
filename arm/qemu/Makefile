OBJ = kernel.o
all: kernel.elf

PREFIX:=arm-none-eabi
GCC:=$(PREFIX)-gcc
LD:=$(PREFIX)-ld
OBJDUMP:=$(PREFIX)-objdump

LIBM:=$(shell $(GCC) -print-file-name=libm.a)
LIBC:=$(shell $(GCC) -print-file-name=libc.a)
LIBGCC:=$(shell $(GCC) -print-file-name=libgcc.a)
LIBS:=$(LIBM) $(LIBC) $(LIBGCC)

CFLAGS := -g -Os -mcpu=cortex-m3 -mthumb -mfloat-abi=hard

kernel.elf: $(OBJ)
	$(LD) -T qemu.ld --entry reset --gc-sections -o $@ $(OBJ) $(LIBS)
	$(OBJDUMP) -d $@

%.o: %.c
	$(GCC) $(CFLAGS) -o $@ -c $<

%.o: %.S
	$(GCC) $(CFLAGS) -o $@ -c $< 

clean:
	rm -f core *~ *.elf *.o

