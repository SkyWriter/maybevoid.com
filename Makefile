clean-env:
	rm -f .ghc.environment.*

watch: clean-env
	nix-shell shell.nix --run \
		"cabal new-run site -- watch --host 0.0.0.0"

build-src: clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-build"

build-site: build-src clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-run site build"

build: build-src build-site

rebuild: build-src clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-run site rebuild"

repl: clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-repl"

shell: clean-env
	nix-shell shell.nix

release:
	nix-build release.nix

sync:
	nix-shell --pure -p cabal2nix --run \
		"cabal2nix ." > default.nix

.PHONY: watch build-src build-site build shell repl