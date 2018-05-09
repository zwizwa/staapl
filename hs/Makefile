# Wrapper Makefile.
# See https://github.com/Gabriel439/haskell-nix
all: compile

clean:
	rm -f staapl result *~

# Release build
compile: default.nix release.nix
	nix-build release.nix
	ls -l result/bin/billing
default.nix: staapl.cabal
	cabal2nix . >$@

# Shell with build deps
NIX_SHELL := nix-shell --attr env release.nix
shell: default.nix
	$(NIX_SHELL)
repl: default.nix
	$(NIX_SHELL) --run "cabal repl"

# Cabal is smarter about recompiling only part of the project for
# incremental development.
cabal-configure: default.nix
	$(NIX_SHELL) --run "cabal configure"
cabal-test: default.nix
	$(NIX_SHELL) --run "cabal test --log=/dev/stdout"
cabal-run: default.nix
	$(NIX_SHELL) --run "cabal run staapl"








