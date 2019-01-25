watch:
	nix-shell --attr env release.nix --run \
		"cabal new-run site -- watch --host 0.0.0.0"

build-src:
	nix-shell --attr env release.nix --run "cabal new-build"

build-site: build-src
	nix-shell --attr env release.nix --run \
		"cabal new-run site build"

build: build-src build-site

rebuild: build-src
	nix-shell --attr env release.nix --run \
		"cabal new-run site rebuild"

repl:
	nix-shell --attr env release.nix --run "cabal new-repl"

shell:
	nix-shell --attr env release.nix

release:
	nix-build release.nix

.PHONY: watch build-src build-site build shell repl