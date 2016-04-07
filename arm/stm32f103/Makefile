all: monitor.core.elf

C_LIB := \
	vector.c \
	cdcacm_desc.c \
	kernel.c


LIBOPENCM3 := libopencm3
LIBOPENCM3_F1_A := $(LIBOPENCM3)/lib/libopencm3_stm32f1.a

# Tool prefix
# TPF := /usr/bin/arm-none-eabi
TPF := ./toolchain/gcc-arm-none-eabi-4_8-2014q2/bin/arm-none-eabi

GCC      := $(TPF)-gcc
OBJDUMP  := $(TPF)-objdump
OBJCOPY  := $(TPF)-objcopy
READELF  := $(TPF)-readelf

# Machine flags needed both for compiling and linking.
MFLAGS := \
	-mthumb \
	-mcpu=cortex-m3 \
	-msoft-float \
	-mfix-cortex-m3-ldrd

CPPFLAGS := \
	-I../gdb \
	-I$(LIBOPENCM3)/include \
	-I../gateway/tools \


CFLAGS := \
	-std=c99 \
	-fno-common \
	-ffunction-sections \
	-fdata-sections \
	-MD \
	-DSTM32F1 \
	-Wall \
	-Werror \
	-Wno-format \
	-Wno-attributes \
	-Wno-multichar \
	-g \
	-Os \
	$(CPPFLAGS) \
	$(MFLAGS)

LDFLAGS := \
	-g \
	--static \
	-nostartfiles \
	-L$(LIBOPENCM3)/lib \
	-Wl,--gc-sections \
	$(MFLAGS)

LDLIBS := \
	-lopencm3_stm32f1 \
	-Wl,--start-group \
	-lc \
	-lgcc \
	-Wl,--end-group

C := $(wildcard *.c)
O := $(C:.c=.o)
D := $(C:.c=.d)
O_LIB := $(C_LIB:.c=.o)
-include $(D)



$(LIBOPENCM3)/README.md:
	git clone https://github.com/libopencm3/libopencm3.git 
	cd libopencm3 ; git checkout 4b8f6e01abeefbf226a8a10a9039a15363bc888e ; git tag staapl


$(LIBOPENCM3_F1_A): $(GCC) $(LIBOPENCM3)/README.md
	make -C libopencm3 PREFIX=$(shell readlink -f $(TPF)) TARGETS=stm32/f1

$(GCC):
	make -C toolchain

# External tool and library dependencies.  These trigger install.
TL := $(GCC) $(LIBOPENCM3_F1_A)

%.d: %.c $(TL)
	$(GCC) $(CFLAGS) -M -MG -MT $(@:.d=.o) $<

%.o: %.c $(TL)
	$(GCC) $(CFLAGS) -c $$(readlink -f $<) -o $@

lib.a: $(O_LIB)
	ar -r $@ $(O_LIB)

%.core.elf: %.o lib.a core.ld stm32f1.ld registers_stm32f103.o 
	$(GCC) $(LDFLAGS) -Tcore.ld -Wl,-Map=$*.map -o $@ $*.o registers_stm32f103.o lib.a $(LDLIBS)

clean:
	rm -f *.elf *.o *~ *.d *.map arm-* *.a

arm: arm-objdump arm-readelf arm-as arm-gcc arm-objcopy
arm-%: ${TPF}-%
	ln -s $< $@

