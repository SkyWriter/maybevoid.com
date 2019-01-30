clean-env:
	rm -f .ghc.environment.*

clean: clean-env
	rm -f result
	rm -rf dist/ dist-newstyle/

watch: clean-env
	nix-shell shell.nix --run \
		"cabal new-run --write-ghc-environment-files never generate-site -- watch --host 0.0.0.0"

build-src: clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-build --write-ghc-environment-files never"

build-site: clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-run --write-ghc-environment-files never generate-site build"

build: build-src build-site

rebuild: clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-run --write-ghc-environment-files never generate-site rebuild"

repl: clean-env
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never"

shell: clean-env
	nix-shell shell.nix

release: clean
	nix-build release.nix

sync:
	nix-shell --pure -p cabal2nix --run \
		"cabal2nix ." > default.nix

.PHONY: watch build-src build-site build shell repl