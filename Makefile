.PHONY: clean install scat macro pic18 doc staapl

PACKAGE_ROOT = staapl
PACKAGE = staapl.plt

all: dev

# MZSCHEME=racket
MZSCHEME=mzscheme
RACO=raco

unlink:
	$(RACO) pkg remove staapl

link:
	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link staapl

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
	cd staapl && mzc -vk `find -name '*.rkt'` 
	cd app && mzc -vk `find -name '*.fm'` 

pic18:
	mzc -vk pic18.rkt
	mzc -vk live.rkt
	mzc -vk staaplc.rkt

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


