all:
	@echo
	@echo Pick a target from one of the source files in:
	@ls *.fm
	@echo	
	@echo by replacing the .fm extension by one of the following:
	@echo "  .hex     Compile HEX file."
	@echo "  .dict    Create interactive dictionary."
	@echo "  .flash   Build HEX and upload to target."
	@echo "  .live    Build HEX and upload and connect to target."

.SECONDARY:
.DELETE_ON_ERROR:

# INTERP=racket
# MZC=???
# RACKET_BIN=/usr/local/racket-6.1.1/bin
RACKET_BIN:=$(shell dirname $$(which racket))

INTERP=$(RACKET_BIN)/mzscheme
MZC=$(RACKET_BIN)/mzc
RACKET=$(RACKET_BIN)/racket

#INTERP=mzscheme
#MZC=mzc
#RACKET=racket

FMS := $(shell ls *.fm)
APPS := $(FMS:.fm=.app)

apps: $(APPS)

# Console to record in the .dict file
# If 'baud' is defined as a macro in the .fm it uses uart, else pickit2.
# STAAPLC_FLAGS = -c uart -d /dev/ttyUSB0
# STAAPLC_FLAGS = -c uart -d /dev/tty-FTDCIX5Z
# STAAPLC_FLAGS = -c uart -d /dev/tty3V3
# STAAPLC_FLAGS = -c uart -d /dev/tty5V
# STAAPLC_FLAGS = -c uart -d /dev/ttyUPS
# STAAPLC_FLAGS = -c pickit2
# STAAPLC_FLAGS = -c uart -d /dev/tty-um0 --baud 0


## FLUNK hacks
# STAAPLC_FLAGS = -c uart -d /dev/ttyACM0 --baud 0
# STAAPLC_FLAGS = -c uart -d /dev/tty-FT9FP1Z6 --baud 9600

# STAAPLC_FLAGS = --live-module staapl/live-pic18-dtc

# make sure this works without the "staaplc" command line app wrapper installed
STAAPLC = $(INTERP) -l staapl/staaplc --

PROGRAM = pk2cmd -I -M -R 

# Hack to include .f dependencies which are not properly handled by staaplc.
F_APP   := $(shell find .        -name '*.f')
F_PIC18 := $(shell find ../pic18 -name '*.f')
F_LIB := $(F_APP) $(F_PIC18)

# Compile the module before instantiating it in staaplc.
# The rm is a hack to include .f dependencies from F_LIB.
%.app: %.fm $(F_LIB)
	@echo "RACKET_BIN=$(RACKET_BIN)"
	rm -f compiled/$*_fm.*
	$(MZC) -vk $<
	$(STAAPLC) $(STAAPLC_FLAGS) --version-tag $(shell git rev-parse HEAD) --output-code $*.sx --output-asm $*.asm $< 
	chmod +x $*.dict
	ls -l $*.hex $*.dict >$@

# Pseudo rules for .dict and .hex generated as side-effect of .app rule
%.dict: %.app
	touch $*.dict
%.hex:  %.app
	touch $*.hex

# The purpose of the .flash file is to carry the time stamp of
# flashing the PIC chip.  Each .fm file has an associated .program
# script with instructions to upload the binary.
%.flash: %.app
	rm -f *.flash
	sh $*.program $*.hex | tee $@
	grep 'Operation\ Succeeded' $@

# Note: if you're sure the microcontroller's flash memory is
# consistent with the data in the .dict file, it's OK to just run
# "staapl <dictfile>".  However, we don't know that here so we program
# the device before connecting.

%.live: %.dict %.flash
	$(MZC) -vk ../live.rkt
	@while echo -ne '\nrestarting $@\nPress ctrl-C to quit'; do sleep 1; echo -ne '\r                    \n'; $(INTERP) $*.dict; done


clean:
	rm -rf *~ *.hex *.dict test.md5sum *.timestamp *.dasm compiled *.app *.flash *.sx *.asm

# Regression tests for binary code generator.

# Tests depend on compiler: use the .zo file as a timestamp

HEX = synth-1220-8.hex logan.hex 2620-40.hex 1220-8.hex  picstamp.hex 452-40.hex blink-a-led.hex

DASM = $(HEX:.hex=.dasm)
DIFF = $(HEX:.hex=.diff)


test.md5sum: $(HEX)
	md5sum *.hex >$@

dasm: $(DASM)

test: $(DIFF)


%.dasm: %.hex
	gpdasm -p 18F1220 $<  >$@

# Currently being worked on.  Others might be broken due to compiler or library changes.
current: dtc.app synth.app synth-1220-8.app dip40kit.app

# We diff the hex instead of the dasm as gpdasm produces output
# dependent on 32bit or 64bit host.
%.diff: %.hex
	diff $< hex/$<

_test: 
	make clean ; make test

# C file needs to be kept in sync with what's in the device's Flash memory.
%.c: %.gen-c.rkt
	$(RACKET) $< >$@
%.elf: %.c ../live/console.c
	gcc -o $@ $<
