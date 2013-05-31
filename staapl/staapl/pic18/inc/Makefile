
# Make sure that the INC and inc files are available under this directory.
# MPASM=mpasm
MPASM=~/staapl/upstream/mpasm

# /opt/xc/mplab/8.60/MPASM Suite

.SUFFIXES: ss
.PHONY: all clean

# MODULES = $(patsubst %.INC,%.ss,$(notdir $(shell echo mpasm/P18*.INC)))
MODULES = P18F2550.ss P18F4550.ss P18F1320.ss P18F1220.ss P18F2620.ss
all: $(MODULES)

clean: 
	rm -f *.ss *~

$(MPASM):
	@echo Please provide the directory with MPASM INC files (MPASM=$@)

%.ss: $(MPASM)/%.INC 
	mzscheme ../genheader.ss $< >$@
