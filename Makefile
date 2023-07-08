.PHONY: clean install scat macro pic18 doc staapl

PACKAGE_ROOT = staapl
PACKAGE = staapl.plt

all: dev


# multiple tasks doesn't seem to make it faster, and -v doesn't work.
# RACO_MAKE=raco make -j 4 -v   # 20s
# RACO_MAKE=raco make -v

# FIXME: racket's macro implementation changed in a way incompatible
# to some workaround in staapl.  Currently version is kept at the last
# working racket release.  6.2 has the breaking change.
RACKET_VER=6.1.1
#RACKET_VER=6.2

# RACKET_BIN=/usr/local/racket-$(RACKET_VER)/bin
RACKET_BIN=$(shell dirname $$(which racket))

RACO_MAKE=$(RACKET_BIN)/mzc -vk
RACO_PKG=$(RACKET_BIN)/raco pkg
MZSCHEME=$(RACKET_BIN)/mzscheme

# 20191124: trying out racket 6.8, see default.nix
# Has some errors, but in general seems to work with "make link ; make".
# EDIT: No, compiling the binaries doesn't work.  This needs 6.1.1

# RACO_MAKE=mzc -vk
# RACO_PKG=raco pkg
# MZSCHEME=mzscheme

unlink:
	$(RACO_PKG) remove staapl

link:
	cd $$(readlink -f .)/.. ; $(RACO_PKG) install --link staapl

gitbuf:
	$(RACO) pkg install github://github.com/zwizwa/staapl/master

# run after updating plt or on fresh local install of staapl dev tree
dev:
	make pic18

install:
	$(MZSCHEME) -l staapl/install

# Compile all .ss files found in the package tree.  This mimicks what
# setup-plt does, and is a quick way to prevent problems when packaging.

all-modules: planet-version.txt
	cd staapl && $(RACO_MAKE) `find -name '*.rkt'` 
	cd app && $(RACO_MAKE) `find -name '*.fm'` 

pic18:
	$(RACO_MAKE) arm/arm.rkt
	$(RACO_MAKE) pic18/pic18.rkt
	$(RACO_MAKE) pic18/pic18-dtc.rkt
	$(RACO_MAKE) pic18/sim.rkt
	$(RACO_MAKE) live.rkt
	$(RACO_MAKE) staaplc.rkt
	$(RACO_MAKE) pic18/live.rkt
	$(RACO_MAKE) pic18/live-dtc.rkt

# use planet instead
install-collects:
	sh bin/install

clean:
	find -name 'compiled' -exec rm -rf '{}' ';'  || echo -n
	find -name '*~' -exec rm -rf '{}' ';' || echo -n
	rm -rf .*~ *.plt *.tar.gz *version.txt staapl/prj/version.rkt
	make -C doc clean
	make -C app clean

doc: all
	make -C doc

snapshot:
	sh bin/release -d

release: 
	make test
	make clean
	make release-unclean

test:
	make clean
	make unsafe_test

unsafe_test:
	make dev
	make -C app test

# Run this before making the planet package.  It will build all scheme
# files, not just those that are actually used, and is much faster
# than the package compiler.
all_files:
	mzc -vk `find staapl -name '*.rkt'`
all_exp_files:
	mzc -vk `find staapl-exp -name '*.rkt'`


%.html: %.scrbl
	cd $(dir $<) ; raco scribble $(notdir $<)

zwizwa: staapl/scribblings/staapl.html www/index.html
	cp -av www/*                ~/www/zwizwa.be/staapl/
	cp -av staapl/scribblings/* ~/www/zwizwa.be/staapl/

# planet


