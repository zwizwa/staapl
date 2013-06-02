.PHONY: clean install scat macro pic18 doc planet-test planet-local planet-clean planet-package staapl

PACKAGE_ROOT = staapl
PACKAGE = staapl.plt

all: dev

# MZSCHEME=racket
MZSCHEME=mzscheme
PLANET=raco planet
include Makefile.planet

planet-version.txt: bin/version
	$(MZSCHEME) -f bin/version

planet-versioned: planet-package
	ln -f staapl.plt staapl-`cat staapl-version.txt`-pre-`date +%Y%m%d-%H%M%S`.plt



# run after updating plt or on fresh local install of staapl dev tree
dev:
# 	make planet-link
	make pic18

install:
	$(MZSCHEME) -l staapl/install

# Compile all .ss files found in the package tree.  This mimicks what
# setup-plt does, and is a quick way to prevent problems when packaging.

all-modules: planet-version.txt
	cd staapl && mzc -vk `find -name '*.ss'` 
	cd app && mzc -vk `find -name '*.fm'` 

pic18:
	mzc -vk staapl/pic18.ss
	mzc -vk staapl/staaplc.ss
	mzc -vk staapl/live.ss

# use planet instead
install-collects:
	sh bin/install

clean:
	find -name 'compiled' -exec rm -rf '{}' ';'  || echo -n
	find -name '*~' -exec rm -rf '{}' ';' || echo -n
	rm -rf .*~ *.plt *.tar.gz *version.txt staapl/prj/version.ss
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

release-unclean:
	make planet-fileinject
	mv staapl.plt staapl-`cat staapl-version.txt`.plt
	@echo
	@echo "REMEMBER: 1. tag the darcs archive"
	@echo "          2. bump the version in bin/version."
	@echo "          3. upload to planet + giebrok"
	@echo
	@echo '#' darcs tag staapl-`cat staapl-version.txt`
	@echo '#' scp staapl-`cat staapl-version.txt`.plt staapl-`cat staapl-version.txt`.tar.gz giebrok:www/zwizwa.be/archive/
	@echo '# http://planet.plt-scheme.org/display.ss?package=staapl.plt&owner=zwizwa'

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
	mzc -vk `find staapl -name '*.ss'`


%.html: %.scrbl
	cd $(dir $<) ; raco scribble $(notdir $<)

zwizwa: staapl/scribblings/staapl.html www/index.html
	cp -av www/*                ~/www/zwizwa.be/staapl/
	cp -av staapl/scribblings/* ~/www/zwizwa.be/staapl/

# planet


